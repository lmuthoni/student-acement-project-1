## ---- load_libraries ----
library(readxl)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(Metrics)
library(corrplot)
library(mlr)

###----Loading data---------
exceldata <- read_excel("C:/Users/Admin/Desktop/EDICET/STUDENTS PERFORMANCE/class.xlsx")
View(exceldata)
#####-----Data Wrangling---------

## Making imported excel file as dataframe(table)

placeddata = data.frame(exceldata)

#  Checking for missing Values
placeddata_raw<- na.omit(placeddata)   #raw data

# Remove columns with non-numeric structure(unwanted colums)

placeddata<-placeddata[,!grepl("sl_no|gender|ssc_b|hsc_b|hsc_s|degree_t|workex|specialisation",colnames(placeddata))]


str(placeddata)


dplyr::glimpse(placeddata)
#####---Spilliting train (80) and test (20)--------
set.seed(123)

index<- sample(2, nrow(placeddata), prob = c(0.8, 0.2), replace = TRUE)

placeddata_train<-placeddata[index==1, ]  #train data
placeddata_test<-placeddata[index==2, ]    #test data

# Names and dimensions of the split data
names(placeddata_train)   
names(placeddata_test)
print(dim(placeddata_train))
print(dim(placeddata_test))

str(placeddata_train)
####-----Training a tree model using rpart library--------

#Training a decision tree model
placeddata_model<-rpart(status ~ .,
                    data = placeddata_train, method = "class")

##Making prediction
predict_placedunseen <-predict(placeddata_model, placeddata_test, type = 'class')
table_placed <- table(placeddata_test$status, predict_placedunseen)
table_placed


NewPredictions <- function(model, newdata){
  new.predictions <- predict(object = placeddata_model, newdata = newdata)
  return(new.predictions)
}

####------Performance metrics and Visualization-----

##measure performance
accuracy_Test <- sum(diag(table_placed)) / sum(table_placed)
print(paste('Accuracy for test', accuracy_Test*100))

#plotting the trained model#####


# yes/no =2 : Add yes/no at each level
# type=0    : Draw Split label at each split and a node label at each lea
# extra =   : No extra information

rpart.plot(x = placeddata_model, yesno =0, type = 0, extra = 0)

###----Checking variable importance of the trained model based on the plotted model---

df <- data.frame(imp = placeddata_model$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()

ggplot2::ggplot(df2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()


###---- Model performance evaluation on test data set-----

# Placement prediction
placed_predicted <- predict(object = placeddata_model,
                          newdata = placeddata_test,
                          type = "class")


# Mode accuracy on the test data

placed <- accuracy(actual = placeddata_test$status,
                 predicted = placed_predicted)*100

print(paste('Accuracy for test', placed))

###---- Test splitting criteria based comparison------

# Model training based on gini based splitting criteria
placed_model1 <- rpart(formula = status~.,
                     data = placeddata_train,
                     method = "class",
                     parms = list(split= "gini"))

# Model training based on information gain based splitting criterion

placed_model2<- rpart(formula = status~.,
                    data = placeddata_train,
                    method = "class",
                    parms = list(split= "information"))

# Generate tip predictions on the test data using gini- based spllitting method
pred1<- predict(object = placed_model1,
                newdata = placeddata_test,
                type = "class")
# Generating tip prediction on test date using information based splitting criterion
pred2<- predict(object = placed_model2,
                newdata = placeddata_test,
                type = "class")

# Compare classification accuracy on test data
accuracy(actual = placeddata_test$status,
         predicted = pred1)
accuracy(actual = placeddata_test$status,
         predicted = pred2)

rpart.plot(x = placed_model1, yesno = 2, type = 0, extra = 0)

####-----Tree Pruning on placed_model1-----

# Plotting cost parameter (cp) Table
plotcp(placed_model1)

# Printing the cost parameter (cp) T able
print(placed_model1$cptable)

# Retrieve of optimal cp value based on cross validated error
index<- which.max(placed_model1$cptable[, "xerror"])

cp_optimal<- placed_model1$cptable[index, "CP"]

# Pruning tree based on optimal CP value

placed_model1_opt <-prune(tree = placed_model1, cp = cp_optimal)

# Predicting based on the pruning

pred3 <- predict(object = placed_model1_opt,
                 newdata = placeddata_test,
                 type = "class")

###--------Hyper Parameter Grid Search---------
# Hyperparameter Tuning training with mlr
getParamSet("classif.rpart")

d.tree.mlr <- makeClassifTask(data=placeddata_train, target="status")


# Search Parameter for Max Depth
param_grid <- makeParamSet( 
  makeDiscreteParam("maxdepth", values=1:30))

# Define Grid
control_grid = makeTuneControlGrid()

# Define Cross Validation
resample = makeResampleDesc("CV", iters = 3L)

# Define Measure
measure = acc


set.seed(123) 
dt_tuneparam <- tuneParams(learner='classif.rpart', 
                           task=d.tree.mlr, 
                           resampling = resample,
                           measures = measure,
                           par.set=param_grid, 
                           control=control_grid, 
                           show.info = TRUE)

# Extracting results
result_hyperparam <- generateHyperParsEffectData(dt_tuneparam, partial.dep = TRUE)

# Plotting Accuracy Score across different maxdepth
ggplot(
  data = result_hyperparam$data,
  aes(x = maxdepth, y=acc.test.mean)
) + geom_line(color = 'darkblue')

dt_tuneparam

# Pick Up Best Params and train them
best_parameters = setHyperPars(
  makeLearner("classif.rpart", predict.type = "prob"), 
  par.vals = dt_tuneparam$x
)

# fit a decision tree with the saved hyperparameters in the best_parameters object

c = train(best_parameters, d.tree.mlr)

d.tree.mlr.test <- makeClassifTask(
  data=placeddata_test, 
  target="status"
)
bestmodeltest <- rpart(status ~., 
                       data = placeddata_train,
                       method = "class",
                       maxdepth = 30)
# Predicting the best Model
results <- predict(best_model, task = d.tree.mlr.test)$data

accuracy(results$truth, results$response)

# Tweaking multiple hyperparameters
param_grid_multi <- makeParamSet( 
  makeDiscreteParam("maxdepth", values=1:30),
  makeNumericParam("cp", lower = 0.001, upper = 0.01),
  makeDiscreteParam("minsplit", values=1:10)
)

dt_tuneparam_multi <- tuneParams(learner='classif.rpart', 
                                 task=d.tree.mlr, 
                                 resampling = resample,
                                 measures = measure,
                                 par.set=param_grid_multi, 
                                 control=control_grid, 
                                 show.info = TRUE)


# Extracting best Parameters from Multi Search
best_parameters_multi = setHyperPars(
  makeLearner("classif.rpart", predict.type = "prob"), 
  par.vals = dt_tuneparam_multi$x
)

# fit a decision tree with the saved hyperparameters in the best_parameters object

best_model_multi = train(best_parameters_multi, d.tree.mlr)

# Predicting the best model

resultsbest <- predict(best_model_multi, task = d.tree.mlr.test)$data

accuracy(resultsbest$truth, resultsbest$response)

# Extracting resultsfrom multigrid
result_hyperparam.multi <- generateHyperParsEffectData(dt_tuneparam_multi, partial.dep =  TRUE)




#------------------------------------------------------------
#            Save Model
#------------------------------------------------------------
#Save R Model file
saveRDS(placeddata_model, file = "placed_model.rds")
saveRDS(best_model, file = "best_model.rds")
saveRDS(best_model_multi, file = "best_model_multi.rds")

# Serialize model
trained_model <- as.raw(serialize(placeddata_model, connection = NULL))


#Save R Model file
modellist <- vector(mode = 'list')

# save fitted model
modellist$modelobject<- placeddata_model
modellist$NewPredictions<- NewPredictions
saveRDS(placeddata_model, file = "placeddata2.rds")
saveRDS(NewPredictions, file = "placeddatatest.rds")

