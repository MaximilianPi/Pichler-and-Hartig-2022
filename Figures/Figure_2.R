library(europepmc)
library(tidyverse)
library(ggplot2)
library(parallel)

eco_query =   ' AND ("ecology" OR "ecolog*" OR "evolution*")'
add_second = function(first, second) paste0(first, second)
search_terms = c(
  '("deep neural network" OR "convolutional neural network" OR "image recognition")',
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

cl = makeCluster(12L)

results = parLapply(cl, add_second(search_terms, ""), fun = function(q) europepmc::epmc_hits_trend(query=q, period = 1920:2021) )

results_eco = parLapply(cl, add_second(c("", search_terms), eco_query), fun = function(q) europepmc::epmc_hits_trend(query=q, period = 1920:2021) )

names(results) =  search_names
names(results_eco) =  c("ZERO", search_names)

data = 
  data.frame(Year =  results$NN$year,
             NN =    results$NN$query_hits/    results$NN$all_hits,
             DL =    results$DL$query_hits/    results$DL$all_hits,
             CNN =   results$CNN$query_hits/   results$CNN$all_hits,
             RNN =   results$RNN$query_hits/   results$RNN$all_hits,
             GNN =   results$GNN$query_hits/   results$GNN$all_hits,
             RF =    results$RF$query_hits/    results$RF$all_hits, 
             BRT =   results$BRT$query_hits/   results$BRT$all_hits, 
             KNN =   results$KNN$query_hits/   results$KNN$all_hits, 
             RIDGE = results$Ridge$query_hits/ results$Ridge$all_hits, 
             SVM =   results$SVM$query_hits/   results$SVM$all_hits,
             STATS = results$stats$query_hits/ results$stats$all_hits,
             ML =    results$ML$query_hits/    results$ML$all_hits)

data_eco = 
  data.frame(ZERO =  results_eco$ZERO$query_hits,
             Year =  results_eco$NN$year,
             NN =    results_eco$NN$query_hits/    results_eco$NN$all_hits,
             DL =    results_eco$DL$query_hits/    results_eco$DL$all_hits,
             CNN =   results_eco$CNN$query_hits/   results_eco$CNN$all_hits,
             RNN =   results_eco$RNN$query_hits/   results_eco$RNN$all_hits,
             GNN =   results_eco$GNN$query_hits/   results_eco$GNN$all_hits,
             RF =    results_eco$RF$query_hits/    results_eco$RF$all_hits, 
             BRT =   results_eco$BRT$query_hits/   results_eco$BRT$all_hits, 
             KNN =   results_eco$KNN$query_hits/   results_eco$KNN$all_hits, 
             SVM =   results_eco$SVM$query_hits/   results_eco$SVM$all_hits,
             RIDGE = results_eco$Ridge$query_hits/ results_eco$Ridge$all_hits, 
             STATS = results_eco$stats$query_hits/ results_eco$stats$all_hits,
             ML =    results_eco$ML$query_hits/    results_eco$ML$all_hits)


#### Figure 1 ####
cols = c(   "#F4B183", "#4FB998", "#377EB8")
ML_stats = data %>% select(STATS, DL,ML, -Year) %>% map_df(.,.f=function(x) log(x+0.01))
pdf("stats_ml.pdf", width = 10, height = 4)
plot(NULL, NULL, xlim = c(1920,2020), ylim = c(-4.7, -1.5))
splines = lapply(ML_stats, function(p) smooth.spline(1920:2020, y = p[-length(p)], spar = 0.35) )
lapply(1:3, function(i) lines(x = 1920:2020, y = splines[[i]]$y, col = cols[i], lwd = 3.0))
dev.off()


ML_stats = data_eco %>% select(STATS, DL,ML, -Year) %>% map_df(.,.f=function(x) log(x+0.01))
plot(NULL, NULL, xlim = c(1920,2020), ylim = c(-4.7, -1.5))
splines = lapply(ML_stats, function(p) smooth.spline(1920:2020, y = p[-length(p)], spar = 0.35) )
lapply(1:3, function(i) lines(x = 1920:2020, y = splines[[i]]$y, col = cols[i], lwd = 3.0))



#### Word clouds ####

library(tm)
cl = parallel::makeCluster(10L)
parallel::clusterExport(cl, list("add_second", "search_terms", "eco_query"))


text_lists = parLapply(cl, 1:10, function(term) return(europepmc::epmc_search( add_second(search_terms[term], eco_query), output = "raw", limit = 999099999, verbose = FALSE)))
parallel::stopCluster(cl)
k = 4
pdf("wordclouds.pdf")
for(k in 1:10) {
  text <- unlist(sapply(1:length(text_lists[[k]]), function(i) c(text_lists[[k]][[i]]$abstractText, text_lists[[k]][[i]]$keywordList, text_lists[[k]][[i]]$title)))
  
  #text <- unlist(sapply(1:length(test), function(i) test[[i]]$keywordList))
  
  docs <- Corpus(VectorSource(text))
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeNumbers)
  words = unlist(as.list(docs))
  subjects = c("species distribution",
               "species interaction",
               #"community",
               "mortality", 
               #"climate change", 
               "remote sensing", 
               "invasive",
               "decision making", 
               "ecosystem",
               "species identification",
               "species detection",
               "extinction", 
               "functional trait",
               "ecological network",
               "biodiversity",
               "camera trap")
  freq = sapply(1:length(subjects), function(i) sum(grepl(subjects[i], words)))
  wordcloud(words = subjects, freq = freq, min.freq = 2,           
            max.words=max(freq), random.order=FALSE, rot.per=0.00,            
            colors=brewer.pal(8, "Dark2"))
  title(search_terms[k])
}
dev.off()



pdf("streamchart.pdf", width = 10, height = 4.5)
cols = rev(viridis::turbo(7))
par( mar=c(5, 5, 1, 7), new = FALSE, mfrow = c(1,1))
#### Stacked area/ stream flow char ####
stats = data_eco %>% filter(Year>1989)
stats = data.frame((stats)/(apply(stats[,c(13, 14)],1, sum ) + 1e-7)) %>% select(KNN, NN, RIDGE, BRT, RF, SVM)
#stats = stats[,order(stats[nrow(stats),], decreasing = FALSE)]
stats = cbind(0, stats)
plot(NULL, NULL, xlim = c(1, nrow(stats)), ylim = c(0, .2), axes = FALSE, ylab = "Relative useage of ML models in E&E", xlab = "", xaxs="i", yaxs="i")
axis(1, col = "grey", col.ticks="black", tick = FALSE,at = (1:nrow(stats))[seq(1, nrow(stats), length.out = 5)], labels = (1990:2021)[seq(1, 21, length.out = 5)], line = 0.0)
sapply(1:5, function(i) segments( x0 = (1:nrow(stats))[seq(1, nrow(stats), length.out = 5)], 
                                  x1 = (1:nrow(stats))[seq(1, nrow(stats), length.out = 5)],
                                  y0 = 0, y1 = 0.2,
                                  col = "grey", xpd = NA))
axis(2, las = 1)
sapply(1:5, function(i) segments(y0 = seq(0, 0.2, length.out=5),
                                 y1 = seq(0, 0.2, length.out=5), x0=1, x1=nrow(stats), col = "grey", xpd = NA))
for(i in 2:7) {
  p1 = apply(stats[, 1:i,drop=FALSE], 1, sum)
  p2 = apply(stats[, 1:(i-1), drop=FALSE], 1, sum)
  polygon(c(1:nrow(stats), nrow(stats):1), y = c(p1, rev(p2)), col =cols[i-1], border = "white")
}
coords= apply(cbind(cumsum(unlist(stats[nrow(stats),-7])), cumsum(unlist(stats[nrow(stats),-1])) ), 1, mean)*0.91
text(y = coords, pos = 4, x = nrow(stats), labels=c("kNN", "NN", "Ridge/Lasso", "BRT", "RF", "SVM"), xpd = NA)
dev.off()

stats$year = 1990:2021
stats = stats[,-1]

data = pivot_longer(stats, cols = c("KNN", "NN", "RIDGE", "RF", "BRT", "SVM"))

library(ggstream)
ggplot(data, aes(x = year, y = value, fill = name)) +
  geom_stream(bw=0.5) +
  geom_stream_label(aes(label = name)) +
  theme_minimal()




#### Test prop Plot ####
#cols = c(   "#F4B183", "#4FB998", "#377EB8")

Ridge_stats = data_eco %>% filter(Year>1989) %>% select(-STATS)
Ridge_stats = data.frame((Ridge_stats)/(apply(Ridge_stats[,3:ncol(Ridge_stats)],1, sum ) + 1e-7)) %>% select(KNN, NN, RIDGE, BRT, RF, SVM)

#pdf("stats_ml.pdf", width = 10, height = 4)
plot(NULL, NULL, xlim = c(1990,2020), ylim = c(0, 0.4))
splines = lapply(Ridge_stats, function(p) smooth.spline(1990:2021, y = p, spar = 0.3) )
lapply(1:6, function(i) lines(x = 1990:2021, y = splines[[i]]$y, col = cols[i], lwd = 3.0))
legend("topright", col = cols, legend = colnames(Ridge_stats), lty=1)




library(ggstream)
library(patchwork)

names = c("KNN", "NN", "RIDGE", "RF", "BRT", "SVM")
cols = rev((viridis::turbo(7))[-1])
names(cols) = names
############ G1 #############
stats = data_eco %>% filter(Year>1989)
stats = data.frame((stats)/(apply(stats[,c(13, 14)],1, sum ) + 1e-7)) %>% select(KNN, NN, RIDGE, BRT, RF, SVM)
stats$year = 1990:2021
data = pivot_longer(stats, cols = c("KNN", "NN", "RIDGE", "RF", "BRT", "SVM"))
labels=c("kNN", "NN", "Ridge/Lasso", "BRT", "RF", "SVM")
g1 = 
  ggplot(data, aes(x = year, y = value, fill = name)) +
  geom_stream(color="white", lwd = 0.3, bw = 0.70, type = "ridge", sorting = "none") +
  scale_fill_manual(values = cols) +
  labs(y = "Relative useage of ML", x = "Year") +
  theme_minimal() + 
  annotate("text",x = rep(2021, 6), 
           y = c(0.025, 0.059, 0.09, 0.15, 0.19, 0.2), 
           label=labels[rev(order(names))],
           hjust=0) + 
  coord_cartesian(clip = "off") +
  labs(tag = expression(bold("a"))) + 
  theme(legend.position = "none",
        plot.margin = margin(3, 55, 20, 3),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

############ G2 #############
Ridge_stats = data_eco %>% filter(Year>1989) %>% select(-STATS)
Ridge_stats = data.frame((Ridge_stats)/(apply(Ridge_stats[,3:ncol(Ridge_stats)],1, sum ) + 1e-7)) %>% select(KNN, NN, RIDGE, BRT, RF, SVM)
Ridge_stats$year = 1990:2021
data = pivot_longer(Ridge_stats, cols = c("KNN", "NN", "RIDGE", "RF", "BRT", "SVM"))


labels=c("kNN", "NN", "Ridge/Lasso", "BRT", "RF", "SVM")
g2 = 
    ggplot(data, aes(x = year, y = value, fill = name)) +
    geom_stream(color="white", lwd = 0.3, bw = 0.75) +
    scale_fill_manual(values = cols) +
    labs(y = "Relative proportion of ML", x = "Year") +
    theme_minimal() + 
    annotate("text",x = rep(2021, 6), 
             y = c(-0.15, -0.07, 0.0, 0.12, 0.17, 0.2), 
             label=labels[rev(order(names))],
             hjust=0) + 
    coord_cartesian(clip = "off") +
    labs(tag = expression(bold("b"))) + 
    theme(legend.position = "none",
          plot.margin = margin(3, 55, 3, 3),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())

pdf("streamplot_2.pdf", width = 9, height = 6)
grid.arrange(g1, g2, ncol=1, nrow = 2)
dev.off()
library(gridExtra)
grid.arrange(g1, g2, ncol=1, nrow = 2)



doldpar = par()
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


