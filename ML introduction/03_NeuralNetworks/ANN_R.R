library(torch)
library(luz)
data = iris
head(data)
X = scale(iris[,1:4])
Y = iris[,5]
Y = as.integer(Y)

#### Classification ####
# Response: Species -> three classes

NN = 
  nn_module(
    "Net",
    initialize = function() {
      self$net = nn_sequential(
        nn_linear(ncol(X), 10L),
        nn_relu(),
        nn_linear(10L, 10L),
        nn_relu(),
        nn_linear(10L, 3L)
      )
    },
    forward = function(x) {
      self$net(x)
    }
  )

DT = dataloader(tensor_dataset(torch_tensor(X), torch_tensor(Y,dtype=torch_long())), batch_size = 20L)

NN = setup(NN, loss = nn_cross_entropy_loss(), optimizer = optim_adam)
fitted = fit(NN, DT, epochs = 100)

# Make predictions
preds =  as_array( nnf_softmax( predict(fitted, DT), dim = 2 ) )

# Probabilities for each species:
pred


#### Regression ####
# Response: Sepal.Length -> continuous

NN = 
  nn_module(
    "Net",
    initialize = function() {
      self$net = nn_sequential(
        nn_linear(ncol(X)-1, 10L),
        nn_relu(),
        nn_linear(10L, 10L),
        nn_relu(),
        nn_linear(10L, 1L)
      )
    },
    forward = function(x) {
      self$net(x)
    }
  )

DT = dataloader(tensor_dataset(torch_tensor(X[,-1]), torch_tensor(X[,1])), batch_size = 20L)

NN = setup(NN, loss = nn_mse_loss(), optimizer = optim_adam)
fitted = fit(NN, DT, epochs = 100)

# Make predictions
preds =  as_array( predict(fitted, DT) )

# Predictions:
pred
