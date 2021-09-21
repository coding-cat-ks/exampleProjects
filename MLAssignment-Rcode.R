
#################################################### ASSIGNMENT ##########################################################

#1. Load and explore the data set. Present some statistics/figures about the data set. 
# What can you say about this radio station in terms of music taste?

data=read.delim(file="hw2_data.txt", header = TRUE, sep = "", dec = ".")
dim(data) # 1483   16
summary(data, digits = 4)
par("mfrow"=c(4,3))


attach(data)

summary(artist_popularity)
sd(artist_popularity) #15.94
hist(artist_popularity, breaks=15, xlab="Artist popularity", main="Histogram of artist popularity", col="grey", ylim = c(0, 250))

summary(song_popularity)
hist(song_popularity, breaks=15, xlab="Song popularity", main="Histogram of song popularity", col="grey", ylim = c(0, 250))
plot(artist_popularity, song_popularity, xlab = "Artist popularity", ylab = "Song popularity", main = "Artist popularity VS song popularity", cex=0.6, pch=19)

summary(instrumentalness)
hist(instrumentalness, breaks=15, xlab="Instrumentalness", main="Histogram of instrumentalness", col="grey", ylim = c(0, 1400))

summary(liveness)
hist(liveness, breaks=15, xlab="Liveness", main="Histogram of liveness", col="grey", ylim = c(0, 500))
sum(liveness>0.8)cks.

summary(acousticness)
sd(acousticness)
hist(acousticness, breaks=15, xlab="Acousticness", main="Histogram of acousticness", col="grey")

summary(loudness)
hist(loudness, breaks=15, xlab="Loudness", main="Histogram of loudness", col="grey", ylim = c(0, 500))

summary(tempo)
sd(tempo)
hist(tempo, breaks=15, xlab="Tempo", main="Histogram of tempo", col="grey", xlim=c(50, 250), ylim=c(0,250))

summary(danceability)
hist(danceability, breaks=15, xlab="Danceability", main="Histogram of danceability", col="grey", xlim=c(0,1))

summary(energy)
hist(energy, breaks=15, xlab="Energy", main="Histogram of energy", col="grey", ylim = c(0, 200))

summary(speechiness)
hist(speechiness, breaks=15, xlab="Speechiness", main="Histogram of speechiness", col="grey", xlim=c(0,0.5))

summary(valence)
hist(valence, breaks=15, xlab="Valence", main="Histogram of valence", col="grey", ylim = c(0, 120))
sd(valence)
sum(valence>0.5)


#####2. Perform K-NN to predict the number of listeners using numerical features of each track with K=5.
# Examine the performance of the model using at least two different methods (e.g. holdout and repeated holdout).  

#Idea: split data into training and test set. In test set pretend you don't know the outcomes (number of listeners). So training observations have predictor with outcomes, and test observation have only predictors
# to which k training observations are the test observations most similar in terms of eucl. distance.
# Pick for each test observation K nearest neighbors and compute average of their outcomes, yielding predicted outcome for this test observation
# compute MSE by summing squared difference (predicted and observed) of all test observations.


library(caret)
library(pROC)
library(mlbench)
str(data)
sapply(data,class)
allData=data[,-c(2,3,4)]

# KNN Model with k=5 and performance evalutation using k-fold cross validation
trControl=trainControl(method='cv', number=10)
set.seed(333)
fit=train(listener~., # listener represents outcome, dot represents all present predictors
          data=allData, #data used for training set
          tuneGrid=expand.grid(k=5), 
          metric='RMSE', # can specify the metric used for selecting the optimal model. default is RMSE. changing this metric, may improve the eventual classification accuracy.
          method='knn', 
          trControl=trControl, # use the trControl that we created above
          preProc=c('center','scale'))

# Model performance
fit

# KNN Model with k=5 and performance evaluation using repeated cross validation
trControl=trainControl(method='repeatedcv', number=10, repeats = 5)
set.seed(333)
fit=train(listener~., # listener represents outcome, dot represents all present predictors
          data=allData, #data used for training set
          tuneGrid=expand.grid(k=5), 
          metric='RMSE', # can specify the metric used for selecting the optimal model. default is RMSE. changing this metric, may improve the eventual classification accuracy.
          method='knn', 
          trControl=trControl, # use the trControl that we created above
          preProc=c('center','scale'))

#Model performance
fit


#####3. Perform K-NN to predict the number of listeners using numerical features of each track with different K values (you are free to choose the maximum number of K). 
# You can use Euclidean distance as a distance measure and you are expected to use two different error measures (e.g. RMSE and MAE). 
# Examine the performance of the model using at least two different methods (e.g. k-fold, LOOCV and repeated k-fold). 

# to keep in mind for choosing K 
# The choice of K has a drastic effect on the KNN classifier obtained. Figure 2.16 displays two KNN's to the simulated data from Figure 2.13, using K = 1 and K = 100.
#When K = 1, the decision boundary is overly flexible and ???nds patterns in the data that don't correspond to the Bayes decision boundary. 
#This corresponds to a classifier that has low bias but very high variance. As K grows, the method becomes less flexible and produces a decision boundary that is close to linear. 
# This corresponds to a low-variance but high-bias classifier. 
#On this simulated data set, neither K = 1 nor K = 100 give good predictions: they have test error rates of 0.1695 and 0.1925, respectively. 
#  In Figure 2.17, we have plotted the KNN test and training errors as a function of 1/K. As 1/K increases, the method becomes more flexible. 
#As in the regression setting, the training error rate consistently declines as the flexibility increases. 
#However, the test error exhibits a characteristic U-shape, declining at ???rst (with a minimum at approximately K = 10) before increasing again when the method becomes excessively flexible and over???ts.

#create train and test set
ind=sample(2,nrow(data), replace=TRUE, prob=c(0.7,0.3)) # training data is about 70%, test data about 30%
training=data[ind==1,-c(2,3,4)]
test=data[ind==2,-c(2,3,4)]

# KNN Model for all K = 1:200 and performance evaluation using k-fold cross validation and error measure RMSE
trControl=trainControl(method='cv', number=10)

set.seed(333)
metricList = vector("list", 200)
for (i in 1:200)
{
  fit=train(listener~., # listener represents outcome, dot represents all present predictors
            data=training, #data used for training set
            tuneGrid=expand.grid(k=i), 
            metric='RMSE', # can specify the metric used for selecting the optimal model. default is RMSE. changing this metric, may improve the eventual classification accuracy.
            method='knn', 
            trControl=trControl, # use the trControl that we created above
            preProc=c('center','scale'))
  # store RMSE
  value = fit$results$RMSE
  metricList[[i]] = value
}

# choose the best k and evaluate model by predicting test set
bestK = which.min(metricList)

fit=train(listener~., # listener represents outcome, dot represents all present predictors
          data=training, #data used for training set
          tuneGrid=expand.grid(k=bestK), 
          metric='RMSE', # can specify the metric used for selecting the optimal model. default is RMSE. changing this metric, may improve the eventual classification accuracy.
          method='knn', 
          trControl=trControl, # use the trControl that we created above
          preProc=c('center','scale'))
#Model performance
fit
plot(fit)
# varImp(fit)
pred= predict(fit,newdata=test)
RMSE(pred,test$listener)
plot(pred~test$listener)

# Do same as above but with MAE
# KNN Model for all K = 1:200 and performance evaluation using k-fold cross validation and error measure MAE
trControl=trainControl(method='cv', number=10)

set.seed(333)
metricList = vector("list", 200)
for (i in 1:200)
{
  fit=train(listener~., # listener represents outcome, dot represents all present predictors
            data=training, #data used for training set
            tuneGrid=expand.grid(k=i), 
            metric='MAE', # can specify the metric used for selecting the optimal model. default is RMSE. changing this metric, may improve the eventual classification accuracy.
            method='knn', 
            trControl=trControl, # use the trControl that we created above
            preProc=c('center','scale'))
  # store MAE
  value = fit$results$MAE
  metricList[[i]] = value
}

# choose the best k and evaluate model by predicting test set
bestK = which.min(metricList)

fit=train(listener~., # listener represents outcome, dot represents all present predictors
          data=training, #data used for training set
          tuneGrid=expand.grid(k=bestK), 
          metric='MAE', # can specify the metric used for selecting the optimal model. default is RMSE. changing this metric, may improve the eventual classification accuracy.
          method='knn', 
          trControl=trControl, # use the trControl that we created above
          preProc=c('center','scale'))
#Model performance
fit
plot(fit)
# varImp(fit)
pred= predict(fit,newdata=test)
MAE(pred,test$listener)
plot(pred~test$listener)

# KNN Model for all K = 1:200 and performance evaluation using repeated cross validation and error measure RMSE
trControl=trainControl(method='repeatedcv', number=10, repeats = 5)

set.seed(333)
metricList = vector("list", 200)
for (i in 1:200)
{
  fit=train(listener~., # listener represents outcome, dot represents all present predictors
            data=training, #data used for training set
            tuneGrid=expand.grid(k=i), 
            metric='RMSE', # can specify the metric used for selecting the optimal model. default is RMSE. changing this metric, may improve the eventual classification accuracy.
            method='knn', 
            trControl=trControl, # use the trControl that we created above
            preProc=c('center','scale'))
  # store RMSE
  value = fit$results$RMSE
  metricList[[i]] = value
}

# choose the best k and evaluate model by predicting test set
bestK = which.min(metricList)

fit=train(listener~., # listener represents outcome, dot represents all present predictors
          data=training, #data used for training set
          tuneGrid=expand.grid(k=bestK), 
          metric='RMSE', # can specify the metric used for selecting the optimal model. default is RMSE. changing this metric, may improve the eventual classification accuracy.
          method='knn', 
          trControl=trControl, # use the trControl that we created above
          preProc=c('center','scale'))
#Model performance
fit
plot(fit)
# varImp(fit)
pred= predict(fit,newdata=test)
RMSE(pred,test$listener)
plot(pred~test$listener)


# Do same as above but with MAE
trControl=trainControl(method='repeatedcv', number=10, repeats = 5)

set.seed(333)
metricList = vector("list", 200)
for (i in 1:200)
{
  fit=train(listener~., # listener represents outcome, dot represents all present predictors
            data=training, #data used for training set
            tuneGrid=expand.grid(k=i), 
            metric='MAE', # can specify the metric used for selecting the optimal model. default is RMSE. changing this metric, may improve the eventual classification accuracy.
            method='knn', 
            trControl=trControl, # use the trControl that we created above
            preProc=c('center','scale'))
  # store RMSE
  value = fit$results$MAE
  metricList[[i]] = value
}

# choose the best k and evaluate model by predicting test set
bestK = which.min(metricList)

fit=train(listener~., # listener represents outcome, dot represents all present predictors
          data=training, #data used for training set
          tuneGrid=expand.grid(k=bestK), 
          metric='MAE', # can specify the metric used for selecting the optimal model. default is RMSE. changing this metric, may improve the eventual classification accuracy.
          method='knn', 
          trControl=trControl, # use the trControl that we created above
          preProc=c('center','scale'))

#Model performance
fit
plot(fit)
pred= predict(fit,newdata=test)
MAE(pred,test$listener)
plot(pred~test$listener)