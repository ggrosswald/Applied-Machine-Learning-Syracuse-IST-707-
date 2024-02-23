#
# Author: Gavin Grosswald
# Purpose: HW 6
#


library(tm)
library(stringr)
library(wordcloud)
library(stringi)
library(Matrix)
library(tidytext) 
library(dplyr)
library(ggplot2)
library(factoextra)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(cluster)
library(NbClust)
library(wordspace)
library(tidyverse)
library(dendextend)
library(randomcoloR)
library(randomForest)
library(randomForestExplainer)



# Read in the pre cleaned data set
FedPapers <- read.csv("C:\\Users\\gsgro\\OneDrive\\Desktop\\Syr_MSBA\\Term 3\\Machine Learning\\Week 6\\fedPapers85.csv")

# What is the breakdown of authorship
table(FedPapers[,1])

# Create a dataframe for each author
papersHam <- FedPapers[FedPapers$author == "Hamilton",]
papersH_and_M <- FedPapers[FedPapers$author == "HM",]
papersJay <- FedPapers[FedPapers$author == "Jay",]
papersMadison <- FedPapers[FedPapers$author == "Madison",]

# Function finds the average tfidf of a word for a specific author
createWordMean <- function(x) {
  y <- ncol(x)
  x <- colMeans(x[,3:y])
  newVec_1 <- c()
  for (i in 1:length(x)) {
    newVec_1[i] <- x[[i]]
  }
  print(newVec_1)
}

# Run the function on each subsetted dataframe
hamiltonVector <- createWordMean(papersHam)
ham_n_mad_Vector <- createWordMean(papersH_and_M)
jay_Vector <- createWordMean(papersJay)
mad_Vector <- createWordMean(papersMadison)
columns <- colnames(papersHam)

# Put the results into a dataframe
newFrame <- papersHam
newFrame <- data.frame(rbind(hamiltonVector,ham_n_mad_Vector,jay_Vector,mad_Vector))

# Name the columns of the new df their corresponding words
colnames(newFrame) <- columns[3:length(columns)]

# The dataframe is the average tfidf of a word for each author
newFrame[,1:3]

# Find the variance of each column (word)
wordVariance <- sapply(newFrame, var)

# Order and plot the results
ordered <- sort(wordVariance, decreasing = TRUE)
plot(ordered)

# Set the good words as the top 40% of words
goodWords <- data.frame(ordered[1:(length(ordered)*2/5)])

# Number of words chosen
length(rownames(goodWords))
rownames(goodWords)

# Create a new data frame of only the "good words"
smallFrame <- FedPapers[,c("author", "filename",rownames(goodWords))]

# Rename the rows with their corresponding filename
rownames(smallFrame) <- smallFrame$filename
rownames(FedPapers) <- FedPapers$filename
write.csv(smallFrame,"C:\\Users\\gsgro\\OneDrive\\Desktop\\Syr_MSBA\\Term 3\\Machine Learning\\Week 6\\fedPapers_varianceframe.csv")
write.csv(FedPapers,"C:\\Users\\gsgro\\OneDrive\\Desktop\\Syr_MSBA\\Term 3\\Machine Learning\\Week 6\\fedPapers_FullPapersframe.csv")

# Drop Jay and HM
set.seed(1234)

treeData_small <- read.csv("C:\\Users\\gsgro\\OneDrive\\Desktop\\Syr_MSBA\\Term 3\\Machine Learning\\Week 6\\fedPapers_varianceframe.csv")
treeData_small <- treeData_small[,-1]

treeData_small <- subset(treeData_small, !(author %in% c("Jay", "HM")))
treeData_small <- droplevels(treeData_small)
treeData_small <- treeData_small[,-2]
treeData_small <- treeData_small[,c(2:ncol(treeData_small),1)]

treeData_small_disputed <- treeData_small[1:11,]
treeData_small_full_noD <- treeData_small[-c(1:11),]

hamilton_indices <- sample(which(treeData_small_full_noD$author == "Hamilton"), 0.65 * sum(treeData_small_full_noD$author == "Hamilton"))
madison_indices <- sample(which(treeData_small_full_noD$author == "Madison"), 0.65 * sum(treeData_small_full_noD$author == "Madison"))

indexes <- c(hamilton_indices, madison_indices)


tree_model1 <- rpart(author ~ . , data = treeData_small_full_noD[indexes,]
                     , method = 'class'
                     , control = rpart.control(minbucket = 1, minsplit=1, cp=-1)
                     , model = TRUE
)

rsq.rpart(tree_model1)
rpart.plot(tree_model1)

preds1 <- predict(tree_model1, treeData_small_full_noD[-indexes,], type = "class")
table(treeData_small_full_noD$author[-indexes],preds1)

# Fully pruned
tree_model3 <- rpart(author ~ . , data = treeData_small_full_noD[indexes,]
                     , method = "class"
                     , model = TRUE
)
                     
rsq.rpart(tree_model3)
rpart.plot(tree_model3)

preds3 <- predict(tree_model3, treeData_small_full_noD[-indexes,], type = "class")
table(treeData_small_full_noD$author[-indexes],preds3)

# Tree predictions
tree_model1 <- rpart(author ~ . , data = treeData_small_full_noD
                     , method = "Class"
                     , control = rpart.control(minbucket = 1, minsplit=1, cp=-1)
                     , model = TRUE
)

tree_model3 <- rpart(author ~ . , data = treeData_small_full_noD, method = "class", model = TRUE)

predict(tree_model1, treeData_small_disputed, type = "class")
predict(tree_model3, treeData_small_disputed, type = "class")




