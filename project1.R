options(warn=-1)
################ Functions #############################
### Normalization of decimal results to classe values(1:5)
normPredict <- function(predicted){
  minP <- min(predicted)
  maxP <- max(predicted)
  meanP <- mean(predicted)
  diff <- (maxP - meanP)/4
  normF <- function(x) {
    delta <- x-meanP
    if(abs(delta)<diff) {
      return (3)
    } else if(delta>0){ 
      if(abs(delta)<diff*2.5)
        return (4)
      else
        return (5)       
    }else{
      if(abs(delta)<diff*2.5)
        return (2)
      else
        return (1)     
    }
    
  }
  return (sapply(predicted,normF))
}
############# PCA prediction ######################
pcaFunc <- function(trainingF,testingF){
  filtered <- trainingF[,-53]
  filteredTest <- testingF[,-53]
  
  #filtered <- sapply(filtered,function(x) (x-mean(x))/(sd(x)*10))
  #filteredTest <- sapply(filteredTest,function(x) (x-mean(x))/(sd(x)*10))
  
  preProc <- preProcess(filtered,method="pca",pcaComp=5)
  
  trainPC <- predict(preProc,filtered)
  modelFit <- train(trainingF$classe ~ .,method="glm",data=trainPC)
  testPC <- predict(preProc,filteredTest)
  
  predicted <- predict(modelFit,testPC)
  
  predictedN <- normPredict(predicted)
  
  confusionMatrix(testingF$classe,predictedN)
  
}

######## Replace correlated columns 
diagFunc <- function(trainingF){
  M <- abs(cor(trainingF[,-53]))
  diag(M) <- 0
  d <- which(M > 0.55,arr.ind=T)
  filteredNames <- row.names(d);
  filteredNames <- unique(filteredNames)
  
  filtered <- trainingF[,filteredNames]
  filtered <- na.aggregate(filtered)
  
  
  filtered <- cbind(filtered,trainingF$classe)
  filteredNames <- c(filteredNames,"classe")
  names(filtered)<-filteredNames
  
  return (filtered)
}

######### Rpart (caret)
rpartFunc <- function(filtered,testingF){
  modFitTr <- train(classe ~ .,method="rpart", tuneLength=120, data=filtered)
  print(modFitTr$finalModel)
  predicted <- predict(modFitTr,newdata=testingF)
  #predictedN <- normPredict(predicted)
  predictedN <- round(predicted)
  cm <- confusionMatrix(testingF$classe,predictedN)
  list(cm,modFitTr)
}

############# RPart(rpart)
rpart2Func <- function(filtered,testingF){
 tree <- rpart(classe ~ .,method="class",data=filtered)
 ptree<- prune(tree, cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
  print(ptree)
  predicted <- predict(ptree,newdata=testingF)
  #predictedN <- normPredict(predicted)
  predictedN <- round(predicted)
  confusionMatrix(testingF$classe,predictedN)
  
}

############# Random forests
rfFunc <- function(filtered,testingF){
  modFitTr <- train(classe ~ .,method="rf", prox=TRUE, data=filtered)
  print(modFitTr$finalModel)
  predicted <- predict(modFitTr,newdata=testingF)
  #predictedN <- normPredict(predicted)
  predictedN <- round(predicted)
  confusionMatrix(testingF$classe,predictedN)
  
}

########## lm 
lmFunc <- function(filtered,testingF){
  modFitLm <- train(classe ~ .,method="lm",data=filtered)
  print(modFitLm$finalModel)
  predicted <- predict(modFitLm,newdata=testingF)
  predictedN <- normPredict(predicted)
  confusionMatrix(testingF$classe,predictedN)
  
}

###### Check correlation with classe
covarFunc <- function(trainingF){
  
  
  filteredNames = c();
  filtered <- trainingF[,1]
  for(i in 1:53){
    corVal <- cor(trainingF$classe,trainingF[,i])
    if(corVal>.08){
      print(i)
      filtered <- cbind(filtered,trainingF[,i])
      filteredNames <- c(filteredNames,names[i+3])
    }
  }
  filtered <- filtered[,-1]
  filtered <- data.frame(filtered)
  colnames(filtered) <- filteredNames
  
}


###### Data preparation and cleaning
onePrep <- function(trainingSet, index){
  seedIndex <- 12*index;
  set.seed(seedIndex)
  inTrain <- createDataPartition(y=trainingSet$classe,
                                 p=0.75, list=FALSE)
  training <- trainingSet[inTrain,]
  testing <- trainingSet[-inTrain,]
  
  
  trainingF <- training[,names]
  testingF <- testing[,names]
  
  trainingF$classe <- as.numeric(trainingF$classe)
  trainingF$userCode <- as.numeric(trainingF$user_name)
  trainingF <- trainingF[4:57]
  
  testingF$classe <- as.numeric(testingF$classe)
  testingF$userCode <- as.numeric(testingF$user_name)
  testingF <- testingF[4:57]
  list(trainingF,testingF)
}

############# Convert classe values back to letters
convertBack <- function(res){
  letters <- c("A","B","C","D","E")
  sapply(res,function(x){letters[x]})
}

############ Generate answer files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

################################################################




library(caret)
library(zoo)
library(rpart)

setwd("D:/develop/Machine Learning")
names <- c("Number","user_name","cvtd_timestamp","raw_timestamp_part_1", "roll_belt","pitch_belt","yaw_belt","total_accel_belt","gyros_belt_x","gyros_belt_y","gyros_belt_z","accel_belt_x","accel_belt_y","accel_belt_z","magnet_belt_x","magnet_belt_y","magnet_belt_z","roll_arm","pitch_arm","yaw_arm","total_accel_arm","gyros_arm_x","gyros_arm_y","gyros_arm_z","accel_arm_x","accel_arm_y","accel_arm_z","magnet_arm_x","magnet_arm_y","magnet_arm_z","roll_dumbbell","pitch_dumbbell","yaw_dumbbell","total_accel_dumbbell","gyros_dumbbell_x","gyros_dumbbell_z","accel_dumbbell_x","accel_dumbbell_y","accel_dumbbell_z","magnet_dumbbell_x","magnet_dumbbell_y","magnet_dumbbell_z","roll_forearm","pitch_forearm","yaw_forearm","total_accel_forearm","gyros_forearm_x","gyros_forearm_y","gyros_forearm_z","accel_forearm_x","accel_forearm_y","accel_forearm_z","magnet_forearm_x","magnet_forearm_y","magnet_forearm_z","classe")
trainingSet<-read.csv("pml-training.csv")
testingSet<-read.csv("pml-testing.csv")

maxAcc <- 0
bestFit <- 0
for(i in 1:20){
  res <- onePrep(trainingSet,i)
  trainingF <- data.frame(res[1])
  testingF <- data.frame(res[2])
  filtered <- diagFunc(trainingF)
  res<- rpartFunc(filtered,testingF)
  cm <- res[1]
  modFit <- res[2]
  acc<-  unname(cm[[1]]$overall[1])[1]
  print(sprintf("%f: %f", i , acc))
  if(acc>maxAcc){
    maxAcc <- acc
    bestFit <- modFit
  }
}
namesT <- names[-length(names)]
namesT[1] = 'X'
dataF <- testingSet[,namesT]
dataF$userCode <- as.numeric(dataF$user_name)
cleanedTest <- dataF[4:56]


predicted <- predict(bestFit,newdata=cleanedTest)
predictedN <- round(predicted[[1]])

print(predictedN)

predictedLetters <- convertBack(predictedN)
print(predictedLetters)

pml_write_files(predictedLetters)

#filteredTest <- testingF[,filteredNames]
#filteredTest <- na.aggregate(filteredTest)

#pcaFunc(trainingF,testingF)


###################################



#lmFunc(filtered,testingF)


#rpart2Func(filtered,testingF)

#rfFunc(filtered,testingF)



plot(trainingSet$classe,trainingSet$total_accel_belt)
plot(trainingF$total_accel_dumbbell,as.numeric(trainingF$classe))
featurePlot(trainingF[,4:12],y=trainingSet$classe, plot="pairs")
