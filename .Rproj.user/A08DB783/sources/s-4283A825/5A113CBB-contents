library(ranger)
data = iris
head(data)

#### Classification ####
# Response: Species -> three classes
rf = ranger(Species~., data = data, probability = TRUE, importance = "impurity")

# Make predictions:
pred = predict(rf, data = data)
# Probabilities for each species:
pred$predictions
# Variable importance:
importance(rf)



#### Regression ####
# Response: Sepal.Length -> continuous
rf = ranger(Sepal.Length~., data = data, classification = FALSE ,importance = "impurity")
# Make predictions:
pred = predict(rf, data = data)
print(pred$predictions)
# Variable importance:
importance(rf)


