library(kknn)
data = iris
head(data)

#### Classification ####
# Response: Species -> three classes
knn = kknn(Species~., train = data, test = data)

# Probabilities for each species:
knn$prob


#### Regression ####
# Response: Sepal.Length -> continuous
knn = kknn(Sepal.Length~., train = data, test = data)

# Predicted responses:
knn$fitted.values



