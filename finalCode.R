library(dplyr)
library(ggplot2)
library(stargazer)
setwd("E:/New folder (2)/KnnFinal")
loan <- read.csv("LoanStats3a_securev1.csv", skip=1)
table(loan$loan_status)
loan <- filter(loan, loan_status!="")
loan$good <- ifelse(loan$loan_status == "Current" | 
                                               loan$loan_status == "Fully Paid" |
                                               loan$loan_status == "Does not meet the credit policy.  Status:Fully Paid",
                                            "good","bad")
table(loan$good)
loan$fico <- (loan$fico_range_high+loan$fico_range_low)/2
library(stargazer)
stargazer(select(filter(loan, good == "good"),dti, fico), median = TRUE, type = "text")
stargazer(select(filter(loan, good == "good"),dti, fico), median = TRUE, type = "text")
#install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)
ggplot(aes(x = dti, color = factor(good)) ,data = loan) + geom_density()
ggplot(aes(x = fico, color = factor(good)) ,data = loan) + geom_density()
loan <- loan %>% select(good, fico, dti)
normalize <- function(x) {
       return ((x - min(x)) / (max(x) - min(x)))
}
loan$fico_n <- normalize(loan$fico)
loan$dti_n <- normalize(loan$dti)
summary(loan[,c("fico", "fico_n")])
set.seed(364)
sample <- sample(nrow(loan),floor(nrow(loan)*0.8))
train <- loan[sample,]
test <- loan[-sample,]
prop.table(table(train$good))
prop.table(table(test$good))
train_knn <- select(train, fico_n, dti_n)
test_knn <- select(test, fico_n, dti_n)
library(class)
pred <- knn(train_knn, test_knn, train$good, k = 5)
head(pred)
library(gmodels) #contains CrossTable function
#install.packages("gmodels")
library(gmodels) #contains CrossTable function
CrossTable(x = test$good, y = pred, prop.chisq = FALSE)
