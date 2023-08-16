# predict
## Practical Machine Learning: Course Project
This project aims to predict if a sequence of exercise movements is performed correctly according to their intentions. The data are collected from six participants fitted with accelerometers on their forearms, belts, arms, and dumbells. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways while their movements were recorded. The data, which are taken from http://groupware.les.inf.puc-rio.br/har, are subsetted into training and test sets. The training set data will be used to train a predictive model and then the test set data will be used to predict if the movements were correct (class A) or incorrect exercise movements.

#1-) Packages required
```{r pack}

IscaretInstalled <- require("caret")
if(!IscaretInstalled){
  install.packages("caret")
  library("caret")
}
IsrandomForestInstalled <- require("randomForest")
if(!IsrandomForestInstalled){
  install.packages("randomForest")
  library("randomForest")
}
IsRpartInstalled <- require("rpart")        
if(!IsRpartInstalled){
  install.packages("rpart")
  library("rpart")
}
IsRpartPlotInstalled <- require("rpart.plot")
if(!IsRpartPlotInstalled){
  install.packages("rpart.plot")
  library("rpart.plot")
}

#Set seed for reproducability
set.seed(1000)
```

#2-) Data Processing

#Load the data and Data variables
```{r process}
 trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"  
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
```
# Download data
```{r Download}
training <- read.csv(url(trainUrl))
testing <- read.csv(url(testUrl))
```

#3-)Clean data
```{r clean}
#Delete missing values and Remove variables(columns) with zero number
training1 <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))  
testing1 <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
training2<-training1[,colSums(is.na(training1)) == 0]
testing2 <-testing1[,colSums(is.na(testing1)) == 0]

#Delete seven first columns that are not predictors
training3   <-training2[,-c(1:7)]
testing3 <-testing2[,-c(1:7)]
```

#4-) Cross-validation
In this section split the training data in training (75%) and testing (25%) data) subsets.

```{r cross ,echo=TRUE, results='hide'}
SplitTrain <- createDataPartition(y=training3$classe, p=0.75, list=FALSE)    
STraining <- training3[SplitTrain, ]
STesting <- training3[-SplitTrain, ]
dim(STraining)
dim(STesting)
```

# Exploratory analysis
 The variable `classe` contains 5 levels. The plot of the outcome variable shows the frequency of each levels in the subTraining data.
 
```{r Analysis, echo=TRUE}

plot(as.factor(STraining$classe), col="green", xlab="classe_levels", ylab="Frequency",main="Levels_classe")
```


#5-) Prediction Models
In order to process and predict the data, in this section, a decision tree and random forest are applied to the data.

```{r decisiontree, echo=TRUE}

### Decision tree
#Fit model
fit_DT <- rpart(as.factor(classe) ~ ., data=STraining, method="class")
# Perform prediction(model to predict class)
prediction_DT <- predict(fit_DT, STesting, type = "class")
# Plot result
rpart.plot(fit_DT, main="Classification Tree", extra=0, under=TRUE, faclen=0)
```

```{r dt, echo=TRUE}
#Shows the errors of the prediction algorithm with confusionMatrix in testing .

confusionMatrix(prediction_DT, as.factor(STesting$classe))
```

```{r randomforestcm, echo=TRUE}
### Random forest
#Fit model 
fit_RF <- randomForest(as.factor(classe) ~ ., data=STraining, method="class")

# Perform prediction(model to predict class)
prediction_RF <- predict(fit_RF, STesting, type = "class")
```


```{r rf, echo=TRUE}
#Shows the errors of the prediction algorithm with Random Forest in testing .
confusionMatrix(as.factor(STesting$classe), prediction_RF)
```

#6-) Conclusion

## Result
The confusion matrices show, that the Random Forest algorithm performens better than decision trees. The accuracy for the Random Forest model was 0.9927 (95% CI: (0.9899, 0.9949)) compared to  0.7543  (95% CI: (0.742, 0.7663)) for Decision Tree model. The random Forest model is choosen.

#submission
Finally, in order to complete the project, in this section, the submitted files of the project are generated using the random forest algorithm on the test data.
```{r submission, echo=TRUE}
# Perform prediction
predictSubmission <- predict(fit_RF, testing3, type="class")
predictSubmission
```
