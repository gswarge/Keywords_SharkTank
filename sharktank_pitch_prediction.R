library(rpart)
library(rpart.plot)
library(randomForest)
library(tm)
library(SnowballC)
library(wordcloud)
library(caret)

sharktank <- read.csv("~/MEGA/Personal/My_Projects/DS_Projects/SharkTank/Shark_Tank_Companies.csv",stringsAsFactors = FALSE)

dim(sharktank)
head(sharktank)
View(sharktank)

str(sharktank)

#Create Corpus
varCorpus = Corpus(VectorSource(sharktank$description))
# Convert to lower-case
varCorpus = tm_map(varCorpus, tolower)
varCorpus = tm_map(varCorpus,removePunctuation)
stopwords("english")[1:10]
varCorpus = tm_map(varCorpus,removeWords,c(stopwords("english")))
varCorpus = tm_map(varCorpus,stripWhitespace)
varCorpus = tm_map(varCorpus,stemDocument)
wordcloud(varCorpus,colors=rainbow(15),max.words = 150)
head(varCorpus)
varfreq = DocumentTermMatrix(varCorpus)
dim(varfreq)
varfreq
#Look at the matrix
inspect(varfreq[490:495,505:515])

#Check for Sparsity
findFreqTerms(varfreq,lowfreq = 20)

#Remove sparse Terms
varsparse = removeSparseTerms(varfreq,0.995)

#convert to a Data Frame
descSparsedf = as.data.frame(as.matrix(varsparse))
# Make all variable names R-friendly
# R does not likes words starting with number

colnames(descSparsedf) = make.names(colnames(descSparsedf))

#Adding deal as Dependent Variable
descSparsedf$deal = sharktank$deal
#Creating Dependent variable | deal as factor variable
descSparsedf$deal <- as.factor(descSparsedf$deal)

#Get no of deals
table(descSparsedf$deal)

#Using Cart algorithm
descCart = rpart(deal~.,data = descSparsedf,method = "class")

#plot the cart 
prp(descCart,extra = 2)
# Extra=2 gives you the tree with numbers
#predictions
pred_cart = predict(descCart,type = "class")
table(pred_cart)

confusionMatrix(data = pred_cart,reference = descSparsedf$deal)

### Logisting Regression ###
sharktankLR = glm(formula = deal~.,data = descSparsedf,family = binomial(link = "logit"))
summary(sharktankLR)

# ., means take all the columns in the dataset

#Step 3 - Finding Accuracy

sharktankPred = predict(sharktankLR,data = sharktank,type = "response")

# type = response gives you probabilities

# Confusion Matrix
table(sharktank$deal,sharktankPred > 0.5)

#Accuracy of the model
#False False + True True / all four
#(244 + 251) / (0 + 0 + 244 + 251)

### Building Random Forest
# Interpreting Results
set.seed(123)
sharktankRF <- randomForest(deal~.,data = descSparsedf)
varImpPlot(sharktankRF)


#Adding Ratio Variable into dataframe descSparsedf

descSparsedf$ratio = (sharktank$askedFor/sharktank$valuation)

#Rerun the models to see if there are any changes

#CART
descCart2 = rpart(deal~.,data=descSparsedf,method="class")
#plot the cart
prp(descCart2,extra=2)
# Extra=2 gives you the tree with numbers
#Evaluate Performance of CART 
predictCARTRatio = predict(descCart2, data=descSparsedf, type="class")
CART_ratio <- table(descSparsedf$deal, predictCARTRatio)
 # Baseline accuracy
BaseAccuracyRatio = sum(diag(CART_ratio))/sum(CART_ratio)
CART_ratio
BaseAccuracyRatio
### Logisting Regression
sharktankLR = glm(deal~.,data=sharktank,family="binomial")
set.seed(123)
SaRatio = glm(deal~., data = descSparsedf)
predictLogistic = predict(SaRatio, data = descSparsedf)
LogRatio <- table(descSparsedf$deal, predictLogistic > 0.5)
predictLogistic
LogRatio
#Accuracy of the model
#False False + True True / all four
(244+250) / (0+1+244+250)
> LogRatio 

