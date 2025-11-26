library(e1071)
library(rpart)
library(cluster)
#Most similar to our practice work

setwd("C:/Users/max.estrade/Downloads")

#datos mixtos
dd <- read.table("hotel_sample_5000.csv",header=T, sep=",")
test<-sample(1:nrow(dd),size = nrow(dd)/3)
dataTrain<-dd[-test,]
dataTest<-dd[test,]
# Convert the target variable to a factor
dataTrain$canceled <- as.factor(dataTrain$canceled)

# In order to mantain a balanced dataset we are going to goin countries into contintens
# install.packages("countrycode") 
library(countrycode)

get_continent <- function(country_column) {
  continents <- countrycode(sourcevar = country_column,
                            origin = "iso3c",
                            destination = "continent")
  
  continents[is.na(continents)] <- "Other"
  
  return(as.factor(continents))
}

  #Create the new 'continent' variable
dataTrain$continent <- get_continent(dataTrain$country)
dataTest$continent  <- get_continent(dataTest$country)

#REMOVE the original 'country' column.
dataTrain$country <- NULL
dataTest$country  <- NULL

# Check the new distribution
print(table(dataTrain$continent))

dataTrain$rs_date <- NULL
dataTest$rs_date  <- NULL

# --- CRITICAL STEP ---
# You modified the data, so you MUST re-train the model now!
svm.model <- svm(canceled ~ ., data = dataTrain, cost = 10, kernel="radial", gamma = 0.1)

# Now predict will work perfectly
svm.pred <- predict(svm.model, newdata = dataTest)
t2<-table(svm.pred, dataTest$canceled)
t2
n <- nrow(dataTest)
errorRate <- 1-(sum(diag(t2))/n)
errorRate


svm.model <- svm(Antiguedad.Trabajo ~ ., data = dataTrain, cost = 10, kernel="radial", gamma = 0.1)
svm.pred <- predict(svm.model, dataTest[,-2])
t2<-table(svm.pred, dataTest$Antiguedad.Trabajo)
t2
n<-nrow(testset)

y<-dataTest[,2]
yp<-svm.pred

mse<-sum((y-yp)^2)/(length(y))
rmse<-sqrt(mse)
aux2<-sum((y - mean(y))^2)/(length(y)) 
r.square<-1- mse/aux2

1-(sum((yp-y)^2)/sum((y-mean(y))^2))


plot(cmdscale(dist(dd[,-1])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])


svm.model <- svm(Antiguedad.Trabajo ~ ., data = dataTrain, cost = 10, kernel="radial", gamma = 0.1)
svm.pred <- predict(svm.model, dataTest[,-2])
t2<-table(svm.pred, dataTest$Antiguedad.Trabajo)
t2
n<-nrow(testset)

y<-dataTest[,2]
yp<-svm.pred

mse<-sum((y-yp)^2)/(length(y))
rmse<-sqrt(mse)
aux2<-sum((y - mean(y))^2)/(length(y)) 
r.square<-1- mse/aux2

1-(sum((yp-y)^2)/sum((y-mean(y))^2))







svm.model <- svm(AGE ~ ., data = basetotal_def, cost = 10, kernel="radial", gamma = 0.1)
svm.pred <- predict(svm.model, dataTest[,-2])
t2<-table(svm.pred, dataTest$Antiguedad.Trabajo)
t2


plot(cmdscale(daisy(dd[,-1])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])
