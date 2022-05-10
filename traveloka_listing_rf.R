# RANDOM FOREST CLASSIFICATION 
# DARI HASIL SENTIMENT DI GABUNG DENGAN DATASET PROPERTY
install.packages("fBasics")
library(randomForest)
library(caret)
library(fBasics)

# LOAD DATA
travelokabdg <- read.csv(file.choose(), header = TRUE )

# CHECK DATA & SET AS FACTOR
str(travelokabdg)
travelokabdg$landmarknear <- as.factor(travelokabdg$landmarknear)
table(travelokabdg$landmarknear)
summary(travelokabdg)

# DATA PARTITION
set.seed(123)
indie <- sample(2, nrow(travelokabdg), replace = TRUE, prob = c(0.55, 0.45))
train <- travelokabdg[indie == 1, ] 
test <- travelokabdg[indie == 2, ]

# MODELS 
set.seed(234)
rf<- randomForest(landmarknear~.,data= train,
                 na.action = na.roughfix, ntree = 300, mtry = 8
                 , importance = TRUE, 
                 proximity = TRUE)
fit_rf<-randomForest(landmarknear~.,
                     data=train,
                     importance=TRUE,
                     prOximity=TRUE,
                     na.action=na.roughfix)

print(rf)
attributes(rf)
plot(fit_rf)
plot(rf)

# PREDICTION AND CONF MATRIX - train
pre1 <- predict(rf, train)
confusionMatrix(pre1, train$landmarknear)

# PREDICTION AND CONF MATRIX - test
pre2 <- predict(rf, test)
confusionMatrix(pre2, test$landmarknear)

# TUNE THE MRTY
tune <- tuneRF(train[,-22], train[,22],
       stepFactor = 0.5, plot = TRUE, ntreeTry = 300, trace = TRUE, improve = 0.5)

# NUMBER OF NODES FOR THE TREES
hist(treesize(rf), main = "Number of Nodes Trees", col = "grey")

# ERROR RATE RF
plot(pre1)
plot(pre2)

# K FOLD CV
train_control <- trainControl(method = "cv", number = 10)
rfKfold <- train(landmarknear~., data = travelokabdg, trControl = train_control, method = "rf")
print(rfKfold)
plot(rfKfold, main = "Cross Validation RF")

# VARIABLE IMPORTANCE
varImpPlot(rf, sort = TRUE, n.var = 10, main = "Top 10 Variable Importance")
importance(rf)
varUsed(rf)

# PARTIAL DEPEDENCE PLOT
partialPlot(rf, train, ratingprop, main =  "Sate Building")
partialPlot(rf, train, ratingprop, main = "Geological Museum")
partialPlot(rf, train, ratingprop, main = "Kawah Putih")
partialPlot(rf, train, ratingprop, main = "Braga, Asia Afrika")
partialPlot(rf, train, ratingprop, main = "Taman Hutan Raya Juanda")

# EXTRACT SINGLE TREE
getTree(rf, 1, labelVar = TRUE)
plot(rf)

# PERFORM MULTIDIMENSIONAL SCALING PLOT FOR PROXIMITY MATRIX
MDSplot(rf, train$landmarknear)
# MDSplot(rf, test$landmarknear)

# basic statss
# LOAD DATA
travelokabasic <- read.csv(file.choose(), header = TRUE )

# CHECK DATA & SET AS FACTOR
str(travelokabasic)
travelokbasic$landmarknear <- as.factor(travelokabasic$landmarknear)
table(travelokabasic$landmarknear)
summary(travelokabasic)

basicStats(data.frame(travelokabasic$ratinguser, travelokabasic$ratingprop,
                      travelokabasic$randomrate, travelokabasic$reviewamount, travelokabasic$score))

