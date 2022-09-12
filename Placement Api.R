library(plumber)
##Model
library(rpart)
library(readxl)    #read excels files
library(rpart.plot)# tree plotting
library(caret)     #confusion matrix
library(Metrics)   # Accuracy metrics
library(jsonlite)
library(e1071)
library(yaml)

#* Test connection
#* @get /check
function(messg = "") {
  list(messg = paste0("Hi I am listening ' ", messg, "'"))
}
## load model 

model1 <- readr::read_rds("best_model.rds")
#* @post /predict
function(req, res)
{
  data.frame(predict(model1``, new_data =as.data.frame(req$body), type = "prob")
  )  
}

#* @plumber 
function(pr){
  pr %>%
    pr_set_api_spec(yaml::read_yaml("new.yaml"))
}

