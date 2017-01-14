---
title: "Practical Machine Learning Course Project (Personal Activity Predictions)"
author: "james c walmsley"
date: "1/10/2017"
output:
        html_document:
        keep_md: yes
                toc: yes
        pdf_document:
                toc: yes
        word_document:
                toc: yes
---
## EXECUTIVE SUMMARY:
        Background
        Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large
        amount of data about personal activity relatively inexpensively. These type of devices are part of
        the quantified self movement – a group of enthusiasts who take measurements about themselves regularly
        to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing
        that people regularly do is quantify how much of a particular activity they do, but they rarely
        quantify how well they do it. In this project, our goal will be to use data from accelerometers on the
        belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly
        and incorrectly in 5 different ways. More information is available from the website here:
        http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).
        #
        Data
        The training data for this project are available here:
        https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
        The test data are available here:
        https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
        The data for this project come from this source:
        http://groupware.les.inf.puc-rio.br/har.
        #
        A) How the Model was built:
        In order to build a predictive model from the two data sets provided we first removed all irrelevant
        columns of variables having no impact on the predictability of the model being built (ie., variables
        which were not measurements) and variables with near zero variance (ie., having no significant
        predictable value becuase of the lack of influence on the outcome). Next, the given training data
        set was partitioned into two smaller data sets 1) new training data set and 2) a validation data set
        to be used to check the accruacy of the selected model (this is what is called cross validation) before
        applying it to the given test data set for the final predictions. We first tested a classification
        tree model and found its accuracy to be below the desireded 80% accuracy or better it was at best less
        than 72% accrurate. Therefore we moved to the randomForest classification method where we ended up
        with a model capable of predicting with an accuracy of over 99% and a 95% CI with a lower limit of
        99.43% and an upper limit of 99.77% and with an estimated out of sample error rate of .000373 using
        7 variables in each of 100 trees.
        #
        B) The results of applying the randomForest model fit for prediction on the original test data set
        provided was 100% based on the results of the solutions for the prediction test questions.
        #
        C) My conclusion is that while the randomForest model prediction method is very accruate it is a bit
        complex and takes a little more time to process and because of this high complexity is it not easy to
        develop and interpretation of how the prediction model actually works due to the large numbers of
        permutations involved in the calculation.


# rm(list=ls())
---
## CLEAN & SET UP THE ENVIRONMENT:
```{r setWD,echo=TRUE,eval=T}
#rm(list=ls())
local({r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org"
options(repos=r)
})
setwd("~/Desktop/Coursera_R/8_Practicle Machine Learning/PML_PA")
```

## RAW DATA IMPORT:
```{r, ImportData, echo=T, eval=T}
# Download the training data set
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
csvTrainingFile <- "pml-training.csv"
if (!file.exists(csvTrainingFile)){
        message(paste("Downloding", csvTrainingFile))
        download.file(trainUrl, destfile="pml-training.csv", method = "curl")
}else{
        message(paste("File exists;", csvTrainingFile))
}
dateDownLoaded <- date()
dateDownLoaded
# Download the test data set
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
csvTestingFile <- "pml-testing.csv"
if (!file.exists(csvTestingFile)) {
        message(paste("Downloding", csvTestingFile))
        download.file(testUrl, destfile="pml-testing.csv", method = "curl")
}else{
        message(paste("File exists;", csvTestingFile))
}
dateDownLoaded <- date()
dateDownLoaded
```
## READ AND SAVE THE CSV DATA:
```{r, readData, echo=T, eval=T}
# Read and save the training and test data csv files
# training
csvTrainingFile <- read.csv("pml-training.csv", header = TRUE, sep = ",", quote = "", na.strings = c("NA","#DIV/0!",""))
training <- csvTrainingFile
# testing
csvTestingFile <- read.csv("pml-testing.csv", header = TRUE, sep = ",", quote = "", na.strings = c("NA","#DIV/0!",""))
testing <- csvTestingFile
```
## PROCESS DATA BY REMOVING IRRELEVANT DATA:
```{r, processData, echo=T, eval=T, message=FALSE, warning=FALSE}
library(caret)
nopredval <- c(1:6) # remove columns with no predictive value
lowvar <- nearZeroVar(testing) # remove vales with near zero variance
training2 <- training[, -c(nopredval,lowvar)]
testing2 <- testing[, -c(nopredval,lowvar)]
```
## MODELING, WE PARTITION WITH A 70:30 SPLIT THE TRAINING DATA TO GET A TRAINING & VALIDATION DATA SET:
```{r, modelDataA, echo=c(1:3,5), eval=T, message=FALSE, warning=FALSE, results='hide'}
# Here we have split the original training data set into two samller data sets consisting
# of a new training data set 70% of the size of the original training set and the
# remainig 30% of the data will be a validation data set to test the prediction models built
# with the new smaller training data set to determine the prediction accuracy before using as a prediction
# model on the original test data set
set.seed(556)
intrain <- createDataPartition(y=training2$X.classe., p=0.70, list = F)
trainingSplit <- training2[intrain,]
trainingSplit$classe <- trainingSplit$X.classe.
trainingSplit <- trainingSplit[,-54]
validationSplit <- training[-intrain,]
validationSplit$classe <- validationSplit$X.classe.
validationSplit <- validationSplit[,-54]
```
## USING THE CLASSIFICATION TREE MODEL TO PREDICT ON THE TRAINING DATA SET:
```{r, modelDataB, echo=T, eval=T, message=FALSE, warning=FALSE}
# MODELING for prediction:
# PART 1 - Using the Classification - Decision Tree method on the training data set
set.seed(96723)
install.packages("rattle");install.packages("rpart")
library(rpart.plot);library(rpart);library(rattle)
f1a <- rpart::rpart(classe ~ ., data = trainingSplit, method = "class")
rattle::fancyRpartPlot(f1a)
predictf1a <- predict(f1a, trainingSplit, type = "class")
confMatrf1a <- confusionMatrix(predictf1a, trainingSplit$classe)
confMatrf1a
plot(confMatrf1a$table, col = confMatrf1a$byClass, main = paste("Confusion Matrix: Accuracy = ", round(confMatrf1a$overall["Accuracy"], 4)))
```
---


        ## USING THE CLASSIFICATION TREE MODEL TO PREDICT ON THE VALIDATION DATA SET:
        ```{r,echo=T, eval=T, message=FALSE, warning=FALSE, classificationTree}
# PART 2 Using the Classification - Decision Tree method on the validation data set
set.seed(36528)
f1b <- rpart::rpart(classe ~ ., data = trainingSplit, method = "class")
rattle::fancyRpartPlot(f1b)
predictf2b <- predict(f1b, validationSplit, type = "class")
confMatrf2b <- confusionMatrix(validationSplit$classe, predictf2b)
confMatrf2b
plot(confMatrf2b$table, col = confMatrf2b$byClass, main = paste("Confusion Matrix: Accuracy = ", round(confMatrf2b$overall["Accuracy"], 4)))
```
---


        ## GENERATING THE RANDOM FOREST MODEL ON THE TRAINING DATA SET:
        ```{r, randomForest, echo=T,eval=T}
# PART 3 Using the Random Forest Method generate the prediction model
set.seed(675)
install.packages("randomForest");library(randomForest)
rfmod1 <- randomForest(classe ~ ., data = trainingSplit, importance=T, ntree=100)
rfmod1
```



## USING THE RANDOM FOREST MODEL FOR PREDICTION ON THE VALIDATION DATA SET:
```{r,validationTesting,echo=T,eval=T}
# PART 4 Validate the RandonForest model predictions using the validation data set
set.seed(342)
predictfrmod1 <- predict(rfmod1, validationSplit, type = "class")
confMatrrfmod1 <- confusionMatrix(validationSplit$classe, predictfrmod1)
confMatrrfmod1
```

---


        ```{r, importanceOfVars, echo=T,eval=T}
# Plot variable importance to see which variables have the most impact on the prediction model
library(randomForest)
impvars <- varImpPlot(rfmod1)
```


```{r,outOfSampleErrorRate, echo=T,eval=T}
# Calculation of the expected out of sample error rate
ooer <- 1-confMatrrfmod1$overall[1]
names(ooer) <- "Out of Sample Error Rate"
ooer
```



## USING THE VALIDATED RANDOM FOREST PREDICTION MODEL ON THE TEST DATA SET:
```{r, predictRandomForestModel1, echo=c(1:3),eval=T}
# PART 5 Using the randomForest method on the test data set
set.seed(846)
predictionstesting2 <- predict(rfmod1, newdata=testing2) #type = "class"
testing2$classe <-  predictionstesting2
testing2$classe
```

## PRODUCING A RESULTS DATA FRAME OF THE RANDOM FOREST PREDICTIONS ON THE TEST DATA SET:
```{r, PredictionReaults, echo=T,eval=T}
# Capturing the results of the randomForest prediction on the test data set as results
results <- data.frame(problem_id= testing2$X.problem_id., classe = predictionstesting2)
results
```







