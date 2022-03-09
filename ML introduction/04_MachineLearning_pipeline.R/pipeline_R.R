library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3measures)
library(paradox)

data = iris


# Create task
task = TaskClassif$new(id = "iris", backend = data, target = "Species")
task$data()

# We can create a preprocessing pipeline for imputation, scaling, and encoding of categorical features (many ML algorithms don't support factors)
preprocess = po("imputeoor") %>>% po("scale") %>>% po("encode")

# Preprocess the task:
scaled_task = preprocess$train(task)[[1]]
scaled_task$data()

# 10-folded cross validation
cv10 = mlr3::rsmp("cv", folds = 10L)
randomForest = lrn("classif.ranger", predict_type="prob")
metric = msr("classif.acc")

results = resample(scaled_task, randomForest, resampling = cv10, store_models = TRUE)
results$aggregate(metric)

## Train and predict:
randomForest$train(scaled_task)
randomForest$predict(scaled_task)


# Hyper-parameter tuning
## Let's check which parameters can be tuned:
randomForest$param_set
## Define hyper-parameter search space:
rf_pars = 
  paradox::ParamSet$new(
    list(paradox::ParamInt$new("min.node.size", lower = 1, upper = 30L),
         paradox::ParamInt$new("mtry", lower = 1, upper = 4L),
         paradox::ParamLgl$new("regularization.usedepth", default = TRUE)))
print(rf_pars)

## To set up the tuning pipeline we need:
## - Inner cross-validation resampling object.
## - Tuning criterion (e.g. AUC).
## - Tuning method (e.g. random or block search).
## - Tuning terminator (When should we stop tuning? E.g. after n iterations).

inner3 = mlr3::rsmp("cv", folds = 3L)
measurement =  msr("classif.acc")
tuner =  mlr3tuning::tnr("random_search") 
terminator = mlr3tuning::trm("evals", n_evals = 5L)
rf = lrn("classif.ranger", predict_type = "prob")

learner_tuner = AutoTuner$new(learner = rf, 
                              measure = measurement, 
                              tuner = tuner, 
                              terminator = terminator,
                              search_space = rf_pars,
                              resampling = inner3)
print(learner_tuner)

# Now we can wrap it normally into the 10-fold cross-validated setup as done previously:
outer3 = mlr3::rsmp("cv", folds = 3L)
result = mlr3::resample(scaled_task, learner_tuner,
                        resampling = outer3, store_models = TRUE)

# Calculate the average ACC of the holdouts.
result$aggregate(measurement)
