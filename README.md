## Machine Learning and Deep Learning -- A review for Ecologists

This repository contains the code to reproduce the results in Pichler and Hartig, Machine Learning and Deep Learning -- A review for Ecologists.

You can find classification and regression examples for common ML (+DL) algorithms in the `docs` subfolder or under the following link: <https://maximilianpi.github.io/Pichler-and-Hartig-2022/>

ML algorithms:

| ML algorithm                  | Task                       | Language         | Link                                                                            |
|--------------------|------------------|-----------------|-----------------|
| Elastic-net/LASSO/Ridge       | Classification, Regression | R, Python, Julia | [link](https://maximilianpi.github.io/Pichler-and-Hartig-2022/elastic_net.html) |
| Support Vector Machine        | Classification, Regression | R, Python, Julia | [link](https://maximilianpi.github.io/Pichler-and-Hartig-2022/svm.html)         |
| k-nearest-neighbor            | Classification, Regression | R, Python, Julia | [link](https://maximilianpi.github.io/Pichler-and-Hartig-2022/knn.html)         |
| Random Forest                 | Classification, Regression | R, Python, Julia | [link](https://maximilianpi.github.io/Pichler-and-Hartig-2022/rf.html)          |
| Boosted Regression Trees      | Classification, Regression | R, Python, Julia | [link](https://maximilianpi.github.io/Pichler-and-Hartig-2022/brt.html)         |
| Deep Neural Networks          | Classification, Regression | R, Python, Julia | [link](https://maximilianpi.github.io/Pichler-and-Hartig-2022/dnn.html)         |
| Convolutional Neural Networks | Multi-class/label          | R, Python, Julia | [link](https://maximilianpi.github.io/Pichler-and-Hartig-2022/cnn.html)         |
| Recurrent Neural Network      | Time-series forecasting    | R, Python, Julia | [link](https://maximilianpi.github.io/Pichler-and-Hartig-2022/rnn.html)         |
| Graph Neural Network          | Node classicifaction       | R, Python        | [link](https://maximilianpi.github.io/Pichler-and-Hartig-2022/gnn.html)         |

Link to the pre-print:

<https://arxiv.org/abs/2204.05023>

### Trend analysis

Scripts to fetch/create the trend data and make the figures (trend figures and wordclouds) can be found in the 'Figures' folder:

```{r}
source("Figures/fetch_data.R") # get trend data 
source('Figures/Figures.R') # create figures
```

### Simulation (Box 4)

Script to create the simulation and run the models can be found in the 'Simulation' folder.

```{r}
source("Simulation/simulation.R")
```
