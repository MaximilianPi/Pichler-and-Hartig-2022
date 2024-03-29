library(torch)
library(luz)
data = iris
head(data)

#### Classification ####
# Response: Species -> three classes
sv = svm(Species~., data=data, probability=TRUE)
summary(sv)

# Make predictions:
pred = predict(sv,  data, probability=TRUE)
# Probabilities for each species:
pred


#### Regression ####
# Response: Sepal.Length -> continuous
sv = svm(Sepal.Length~., data=data)

# Make predictions:
pred = predict(sv,  data)
# Probabilities for each species:
pred
