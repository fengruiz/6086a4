library(ISLR)

Auto$mpgmed <- factor(as.numeric(Auto$mpg > median(Auto$mpg)))

library(e1071)
set.seed(5)
tune.out = tune(svm, mpgmed ~ . - mpg - name, data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

tune.out = tune(svm, mpgmed ~ . - mpg - name, data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree= c(2,3,4)))
summary(tune.out)

tune.out = tune(svm, mpgmed ~ . - mpg - name, data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

library(magrittr) # pipe support
library(dplyr) # for dataframe operations(such as select, filter...)
library(scales) # comma_format()

svm_linear = svm(mpgmed ~ . - mpg - name, data = Auto, kernel = "linear", cost = 1)
svm_poly = svm(mpgmed ~ . - mpg - name, data = Auto, kernel = "polynomial", cost = 10, degree = 3)
svm_radial = svm(mpgmed ~ . - mpg - name, data = Auto, kernel = "radial", cost = 1, gamma = 1)

svm_radial_plot_df <- Auto %>%
  select(mpgmed, weight, horsepower) %>%
  mutate(support = case_when(row_number() %in% svm_radial$index ~ "Yes", T ~ "No"))

library(ggplot2)
ggplot(svm_radial_plot_df, aes(x = horsepower, y = weight, col = mpgmed, shape = support)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("red", "blue")) + 
  scale_shape_manual(values = c('Yes' = 16, 'No' = 1)) + 
  scale_y_continuous(labels = comma_format()) +
  labs(shape = "Support Vector:", 
       col = "Actual mpg0med:")

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
