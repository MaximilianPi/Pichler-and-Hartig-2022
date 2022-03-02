library(europepmc)
library(tidyverse)
library(ggplot)
library(parallel)

eco_query =   ' AND ("ecology" OR "ecolog*" OR "evolution*")'
add_second = function(first, second) paste0(first, second)
search_terms = c(
  '("deep neural networks" OR "convolutional neural networks")',
  '("deep learning")',
  '("random forest")',
  '("boosted regression tree" OR "boosted reg" OR "boosting" OR "gradient boosting")',
  '("k-nearest-neighbor")',
  '("support vector machine" OR "support vector")',
  '("p value" OR "p-value" OR "statistically significant")',
  '("machine learning" OR "machine-learning")',
  '("predictions" OR "predicting" OR "forecasting" OR "forecasts")',
  '("explainable AI")'
)
search_names = c("NN", "DL", "RF", "BRT", "KNN", "SVM", "stats", "ML", "Preds", "xAI")
names(search_names) = search_names

cl = makeCluster(9L)

results = parLapply(cl, add_second(search_terms, ""), fun = function(q) europepmc::epmc_hits_trend(query=q, period = 1920:2021) )

names(results) = search_names

data = 
  data.frame(Year =  results$NN$year,
             NN =    results$NN$query_hits/    results$NN$all_hits, 
             RF =    results$RF$query_hits/    results$RF$all_hits, 
             BRT =   results$BRT$query_hits/   results$BRT$all_hits, 
             KNN =   results$KNN$query_hits/   results$KNN$all_hits, 
             SVM =   results$SVM$query_hits/   results$SVM$all_hits,
             DL =    results$DL$query_hits/    results$DL$all_hits,
             STATS = results$stats$query_hits/ results$stats$all_hits,
             ML =    results$ML$query_hits/    results$ML$all_hits)


#### Figure 1 ####
cols = c(   "#F4B183", "#4FB998", "#377EB8")
ML_stats = data %>% select(STATS, DL,ML, -Year) %>% map_df(.,.f=function(x) log(x+0.01))
pdf("stats_ml.pdf", width = 10, height = 4)
plot(NULL, NULL, xlim = c(1920,2020), ylim = c(-4.7, -1.5))
splines = lapply(ML_stats, function(p) smooth.spline(1920:2020, y = p[-length(p)], spar = 0.35) )
lapply(1:3, function(i) lines(x = 1920:2020, y = splines[[i]]$y, col = cols[i], lwd = 3.0))
dev.off()

oldpar = par()
data_ML = data %>% filter(Year > 2000) %>% select(-STATS, -ML, - Year, - DL)
matplot(t((apply(data_ML, 1, function(row) row/sum(row)))), type = "o", pch = paste0(1:5))

data_ML_flow = t((apply(data_ML, 1, function(row) row/sum(row))))

data_ML_flow = data_ML_flow[,c(4, 2, 1, 5, 3)]
data_ML_flow = cbind(matrix(0.0, nrow(data_ML_flow)), data_ML_flow)
cols = RColorBrewer::brewer.pal(5, "Pastel1")

pdf("flow_chart.pdf", width = 14.0, height = 5)
par(mfrow = c(1, 2))

par( mar=c(6, 5, 2, 4), fig = c(0.0, 0.4, 0.0, 1.0))
plot(NULL, NULL, xlim = c(1920, 2020), ylim = c(0.0, 0.07), las = 1, lwd = 2.2, xlab = "", ylab = "Proportion of publications with predictors/forecasts")
lines(x = 1920:2020, 
      y = smooth.spline(x = 1920:2020, Preds$query_hits/Preds$all_hits, spar = 0.5)$y, lwd = 2.2)

par( mar=c(5, 1, 1, 10), fig = c(0.4, 1.0, 0.0, 1.0), new = TRUE)
plot(NULL, NULL, xlim = c(1, 20), ylim = c(0, 1), axes = FALSE, ylab = "Relative useage of ML models", xlab = "")
for(i in 2:6) {
  p1 = apply(data_ML_flow[, 1:i], 1, sum)
  p2 = apply(data_ML_flow[, 1:(i-1), drop=FALSE], 1, sum)
  polygon(c(1:20, 20:1), y = c(p1, rev(p2)), col =cols[i-1] )
}

names(data_ML)[c(4, 4, 1, 5, 3)]
y_coords = matrix(c(cumsum( data_ML_flow[20, ] ), cumsum( data_ML_flow[20, ] )[-1]), ncol = 2)
text(x = 20,pos = 4, y = apply(y_coords, 1, mean)[-6], 
     labels = c("kNN", "Random Forest", "Neural Networks", "Support Vector Machines", "Boosting"),
     xpd = NA)

text(1, x = (seq(1.3, 19.7, length.out = 20)-0.2)[seq(2, 20, by = 2)],y=-0.03, labels = (2001:2020)[seq(2, 20, by = 2)],pos = 1, las = 1, srt = 45, xpd = NA)

dev.off()

