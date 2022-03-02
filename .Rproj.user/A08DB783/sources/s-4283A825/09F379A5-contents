library(xgboost)
data = iris
head(data)

#### Classification ####
# Response: Species -> three classes
xg_data = xgb.DMatrix(data = as.matrix(data[,1:4]), label = as.integer(data$Species)-1L) # labels must be integer vector starting with 0
brt = xgboost(data = xg_data, nrounds = 100L, objective="multi:softprob", num_class=3) # nrounds = number of trees

# Make predictions:
pred = predict(brt, newdata = xgb.DMatrix(data = as.matrix(data[,1:4])))
# Probabilities for each species:
matrix(pred, ncol = 3)
# Variable importance:
xgb.importance(model = brt)


#### Regression ####
# Response: Sepal.Length -> continuous
xg_data = xgb.DMatrix(data = as.matrix(data[,2:4]), label = data$Sepal.Length) # labels must be integer vector starting with 0
brt = xgboost(data = xg_data, nrounds = 100L, objective="reg:squarederror") # nrounds = number of trees

# Make predictions:
pred = predict(brt, newdata = xgb.DMatrix(data = as.matrix(data[,2:4])))
# Probabilities for each species:
pred
# Variable importance:
xgb.importance(model = brt)

