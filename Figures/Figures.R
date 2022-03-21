library(europepmc)
library(tidyverse)
library(ggplot2)
library(parallel)
library(tm)
library(wordcloud)
library(wordcloud2)

eco_query =   ' AND ("ecology" OR "ecolog*" OR "evolution*")'
add_second = function(first, second) paste0(first, second)
search_terms = c(
  #'("deep neural network" OR "convolutional neural network" OR "image recognition")',
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


results = readRDS("Figures/data/all.RDS")
results_eco = readRDS("Figures/data/all_eco.RDS")

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
pdf("Figures/Fig_1.pdf", width = 10, height = 4)
plot(NULL, NULL, xlim = c(1920,2021), ylim = c(-4.7, -1.5))
splines = lapply(ML_stats, function(p) smooth.spline(1920:2021, y = p, spar = 0.35) )
lapply(1:3, function(i) lines(x = 1920:2021, y = splines[[i]]$y, col = cols[i], lwd = 3.0))
dev.off()



#### Word clouds ####

text_lists = readRDS("Figures/data/text_lists.RDS")

set.seed(42)
k = 4
pdf("Figures/wordclouds.pdf", width = 8, height = 7)
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
            colors=(ggthemes::economist_pal()(9))[1:8])#brewer.pal(8, "Dark2"))
  title(search_terms[k])
}
dev.off()



library(ggstream)
library(patchwork)
library(gridExtra)

names = c("KNN", "DL", "RIDGE", "RF", "BRT", "SVM")
cols = rev((viridis::turbo(7))[-1])
names(cols) = names
############ G1 #############
stats = data_eco %>% filter(Year>1999)
stats = data.frame((stats)) %>% select(KNN, DL, RIDGE, BRT, RF, SVM)
#stats = stats/apply(stats, 1, sum)
stats$year = 2000:2021
data = pivot_longer(stats, cols = c("KNN", "DL", "RIDGE", "RF", "BRT", "SVM"))
labels=c("kNN", "DL", "Ridge/Lasso", "BRT", "RF", "SVM")
g1 = 
  ggplot(data, aes(x = year, y = value*100, fill = name)) +
  geom_stream(color="white", lwd = 0.3, type = "ridge",bw = 0.75, sorting = "none") +
  scale_fill_manual(values = cols) +
  labs(y = "Absolute useage of ML in %", x = "Year") +
  theme_minimal() + 
  ggplot2::annotate("text",x = rep(2021, 6), 
           y = c(0.08, 0.18, 0.3, 0.4, 0.5, 0.7), 
           label=labels[rev(order(names))],
           hjust=0) + 
  coord_cartesian(clip = "off") +
  labs(tag = expression(bold("a"))) + 
  theme(legend.position = "none",
        plot.margin = margin(3, 55, 20, 3),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
############ G2 #############
stats = data_eco %>% filter(Year>1999)
stats = data.frame((stats)) %>% select(KNN, DL, RIDGE, BRT, RF, SVM)
stats = stats/apply(stats, 1, sum)
stats$year = 2000:2021
data = pivot_longer(stats, cols = c("KNN", "DL", "RIDGE", "RF", "BRT", "SVM"))


labels=c("kNN", "DL", "Ridge/Lasso", "BRT", "RF", "SVM")
g2 = 
    ggplot(data, aes(x = year, y = value*100, fill = name)) +
    geom_stream(color="white",type = "proportional", lwd = 0.3, bw = 0.85, extra_span = 0.023) +
    scale_fill_manual(values = cols) +
    labs(y = "Relative usage of ML algorithms in %", x = "Year") +
    theme_minimal() + 
    ggplot2::annotate("text",x = rep(2021, 6), 
             y = c(0.125, 0.27, 0.4, 0.58, 0.77, 0.96), 
             label=labels[rev(order(names))],
             hjust=0) + 
    coord_cartesian(clip = "off") +
    labs(tag = expression(bold("b"))) + 
    theme(legend.position = "none",
          plot.margin = margin(3, 55, 3, 3),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())

pdf("Figures/Fig_2.pdf", width = 9, height = 7)
grid.arrange(g1, g2, ncol=1, nrow = 2)
dev.off()


