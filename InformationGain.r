
library(ggplot2)

#This method is used to compute entropy for a variable.
    #classes - Target Variable (Parent Node)
    #splitvar - Children Node 
entropy <-function(classes,splitvar = NULL){
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(-sum(base_prob*log(base_prob,2)))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(splitvar,classes)
  crossprob <- prop.table(crosstab,1)
  entro = matrix(NA,1,nrow(crossprob))
  for (i in 1:nrow(crossprob)){
    t = crossprob[i,1:ncol(crossprob)]
    k = length(unique(splitvar))
    t1 = t[t > 0]
    enp <- -sum(t1*log(t1,k))
    entro[i] = enp
  }
  if (dim(entro)[2] == 1){
    return (sum(base_prob * entro[1]))
  }
  else{
    return (sum(base_prob * data.frame(entro)))
  }
}

#This method is used to compute information gain for all the variables. 
    #data - data
    #target - Target Variable (Parent Node) 
calculateInfoGain <-function(data,target){
  targetEntropy <- entropy(data[[target]])
  columnNames <- colnames(data)
  combined_df <- data.frame(columnName=as.character(),information.gain = as.numeric(), entropy=as.numeric(), entropyInv= as.numeric())
  for (i in 1:length(columnNames)){
    colname = columnNames[i]
    if(colname != target){
      temp = entropy(data[[target]],data[[colname]])
      gain = round(targetEntropy - temp, 5)
      entropyInv = round(1-temp, 5)
      #print(colname)
      df <- data.frame(columnName=colname,information.gain = gain, entropy = round(temp,5), entropyInv = entropyInv)
      combined_df <- rbind(combined_df, df)
    }
  }
  return (combined_df)
}

#This method is used to compute gini index for a variable.
    #classes - Target Variable (Parent Node)
    #splitvar - Children Node 
gini_index <-function(classes,splitvar = NULL){
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(1-sum(base_prob**2))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(splitvar,classes)
  crossprob <- prop.table(crosstab,1)
  gini = matrix(NA,1,nrow(crossprob))
  for (i in 1:nrow(crossprob)){
    t = crossprob[i,1:ncol(crossprob)]
    impurity <- 1-sum(t**2)
    gini[i] = impurity
  }
  #print(gini)
  if (dim(gini)[2] == 1){
    return (sum(base_prob * gini[1]))
  }
  else{
    return (sum(base_prob * data.frame(gini)))
  }
}

#This method is used to compute gini gain for all the variables.
    #data - data
    #target - Target Variable (Parent Node)
calculateGiniGain <-function(data,target){
  targetGiniIndex <- gini_index(data[[target]])
  columnNames <- colnames(data)
  combined_df <- data.frame(columnName=as.character(),gini.gain = as.numeric(),giniIndex = as.numeric())
  for (i in 1:length(columnNames)){
    colname = columnNames[i]
    if(colname != target){
      temp = gini_index(data[[target]],data[[colname]])
      gain = round(targetGiniIndex - temp, 5)
      giniIndex = round(temp, 5)
      #print(colname)
      df <- data.frame(columnName=colname,gini.gain = gain,giniIndex=giniIndex)
      combined_df <- rbind(combined_df, df)
    }
  }
  return (combined_df)
}

#This method is used to compute classification error for a variable.
    #classes - Target Variable (Parent Node)
    #splitvar - Children Node 
classificationError <-function(classes,splitvar = NULL){
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(1-max(base_prob))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(splitvar,classes)
  crossprob <- prop.table(crosstab,1)
  error = matrix(NA,1,nrow(crossprob))
  for (i in 1:nrow(crossprob)){
    t = crossprob[i,1:ncol(crossprob)]
    impurity <- 1-max(t)
    error[i] = impurity
  }
  if (dim(error)[2] == 1){
    return (sum((base_prob*error[1])))
  }
  else{
    return (sum(base_prob*data.frame(error)))
  }
}

#This method is used to compute accuracy gain for all the variables.
    #data - data
    #target - Target Variable (Parent Node)
calculateAcuracy <-function(data,target){
  targetError <- classificationError(data[[target]])
  columnNames <- colnames(data)
  combined_df <- data.frame(columnName=as.character(),accuracy.gain = as.numeric(), accuracy = as.numeric())
  for (i in 1:length(columnNames)){
    colname = columnNames[i]
    if(colname != target){
      temp = classificationError(data[[target]],data[[colname]])
      gain = round(targetError - temp, 5)
      accuracy = round(1-temp, 5)
      df <- data.frame(columnName=colname,accuracy.gain = gain, accuracy = accuracy)
      combined_df <- rbind(combined_df, df)
    }
  }
  return (combined_df)
}

data <- read.csv("/Users/a5sharma/Documents/ISB/DMG2/Assignment/data/Mushroom/train.csv", stringsAsFactors = TRUE)
data = data[,2:ncol(data)]

targetVariable = 'V1'

res <- calculateInfoGain(data, targetVariable)
res1 <- calculateGiniGain(data, targetVariable)
res2 <- calculateAcuracy(data, targetVariable)

final <- merge(res,res1,by.x  = 'columnName',by.y  = 'columnName')
final <- merge(final,res2,by.x = 'columnName',by.y  = 'columnName')

head(final[order(final$information.gain, decreasing = T),])

print(final[order(final$information.gain, decreasing = T),])

colnames(final)[which(colnames(final) == 'columnName')] <- 'Feature_name'
colnames(final)[which(colnames(final) == 'entropyInv')] <- '1- Entropy'
colnames(final)[which(colnames(final) == 'giniIndex')] <- 'GINI index'
print(final[order(final$information.gain, decreasing = T),c(1,8,6,4,2,5,7)])

print(final[order(final$accuracy.gain, decreasing = T),c(1,7,8,6,4)])

print(final[order(final$accuracy, decreasing = T),c(1,8,6,4)])

print(final[order(final$entropy, decreasing = F),c(1,3,8,6,4)])

print(final[order(final['GINI index'], decreasing = T),c(1,3,8,5,6,4)])

print(final[order(final['gini.gain'], decreasing = T),c(1,3,8,5,6,4)])

options(repr.plot.width=5, repr.plot.height=5)
ggplot() + geom_point(data = final, aes(accuracy, 1-entropy))
