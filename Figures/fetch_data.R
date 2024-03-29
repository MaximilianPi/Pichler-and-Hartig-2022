###### Script to fetch the trend data ###### 

library(europepmc)
library(tidyverse)
library(ggplot2)
library(parallel)

eco_query =   ' AND ("ecology" OR "ecolog*" OR "evolution*")'
add_second = function(first, second) paste0(first, second)
search_terms = c(
  '("artificial neural network" OR "deep neural network" OR "multi-layer perceptron" OR "fully connected neural network")',
  '("deep learning")',
  '("convolutional neural network" OR "object detection")',
  '("recurrent neural network")',
  '("graph neural network" OR "graph convolutional")',
  '("random forest")',
  '("boosted regression tree" OR "boosted reg" OR "gradient boosting" OR "adaboost")',
  '("k-nearest-neighbor")',
  '("ridge regression" OR "lasso regression" OR "elastic-net" OR "elastic net")',
  '("support vector machine" OR "support vector")',
  '("p value" OR "p-value" OR "statistically significant")',
  '("machine learning" OR "machine-learning")',
  '("predictions" OR "predicting" OR "forecasting" OR "forecasts")',
  '("explainable AI")'
)
search_names = c("NN", "DL", "CNN", "RNN","GNN", "RF", "BRT", "KNN", "Ridge", "SVM", "stats", "ML", "Preds", "xAI")
names(search_terms) = search_names

cl = makeCluster(14L)

if(TRUE) {
  results = parLapply(cl, add_second(search_terms, ""), fun = function(q) europepmc::epmc_hits_trend(query=q, period = 1920:2021) )
  saveRDS(results, "Figures/data/all.RDS")
}
if(TRUE) {
  results_eco = parLapply(cl, add_second(c("", search_terms), eco_query), fun = function(q) europepmc::epmc_hits_trend(query=q, period = 1920:2021) )
  saveRDS(results_eco, "Figures/data/all_eco.RDS")
}



#### Word clouds ####
cl = parallel::makeCluster(10L)
parallel::clusterExport(cl, list("add_second", "search_terms", "eco_query"))

if(TRUE) {
  text_lists = parLapply(cl, 1:10, function(term) return(europepmc::epmc_search( add_second(search_terms[term], eco_query), output = "raw", limit = 999099999, verbose = FALSE)))
  saveRDS(text_lists, "Figures/data/text_lists.RDS")
}
parallel::stopCluster(cl)
