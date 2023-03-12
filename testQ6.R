
## auto data with mpg and name columns
library(ISLR)
attach(Auto)

Auto$mpgmed <- factor(as.numeric(Auto$mpg > median(Auto$mpg)))

library(e1071)
set.seed(5)
tune.out = tune(svm, mpgmed ~ . - mpg - name, data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

tune.out = tune(svm, mpgmed ~ . - mpg - name, data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree= c(2,3,4)))
summary(tune.out)

tune.out = tune(svm, mpgmed ~ . - mpg - name, data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)


svm.linear = svm(mpgmed ~ . - mpg - name, data = Auto, kernel = "linear", cost = 1)
svm.poly = svm(mpgmed ~ . - mpg - name, data = Auto, kernel = "polynomial", cost = 10, 
               degree = 3)
svm.radial = svm(mpgmed ~ . - mpg - name, data = Auto, kernel = "radial", cost = 1, gamma = 1)
plotpairs = function(fit) {
  for (variable in names(Auto)[!(names(Auto) %in% c("mpg", "mpgmed", "name"))]) {
    plot(fit, Auto, as.formula(paste("weight~", variable, sep = "")))
  }
}
plotpairs(svm.linear)
plot(svm.radial, Auto, weight ~ horsepower)




## auto data without mpg and name columns

library(magrittr)
library(dplyr)
Auto <- Auto %>%
  select(-c(mpg, name)) %>%
  select(mpgmed, everything())

library(e1071)
set.seed(5)
tune.out = tune(svm, mpgmed ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

tune.out = tune(svm, mpgmed ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree= c(2,3,4)))
summary(tune.out)

tune.out = tune(svm, mpgmed ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

svm_linear = svm(mpgmed ~ ., data = Auto, kernel = "linear", cost = 1)
svm_poly = svm(mpgmed ~ ., data = Auto, kernel = "polynomial", cost = 10, degree = 3)
svm_radial = svm(mpgmed ~ ., data = Auto, kernel = "radial", scale = T, cost = 1, gamma = 1)
plotpairs = function(fit) {
  for (variable in names(Auto)) {
    plot(fit, Auto, as.formula(paste("weight~", variable, sep = "")))
  }
}
plotpairs(svm_linear)
plot(svm_radial, Auto, weight ~ horsepower)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

plot(svm_radial, 
     Auto, 
     weight ~ horsepower, 
     slice = list(cylinders = median(Auto$cylinders), 
                  displacement = median(Auto$displacement),
                  acceleration = median(Auto$acceleration),
                  year = median(Auto$year), 
                  origin = Mode(Auto$origin)))
