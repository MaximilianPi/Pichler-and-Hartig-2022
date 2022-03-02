set.seed(4)
RMSE = function(p, true) sqrt(mean((true-p)**2))
results = 
  sapply(1:10000, function(kk) {
    N = 500
    Con = runif(N, -0.2, 0.2)
    X = 1*Con + rnorm(N, sd = 0.01)
    X2 = runif(N, -0.2, 0.2)
    W = matrix(runif(50*N, -0.2, 0.2), N, 50)
    Y = 0.5*X - 1.5*Con +  rnorm(N, sd = 0.1)
    df = data.frame(Y= Y, X = X, Con=Con)
    
    
    H = 20
    m1 = lm(Y~X+Con, data = df[1:(H-1),])
    m2 = lm(Y~X, data = df[1:(H-1),])
    m3 = lm(Y~Con, data = df[1:(H-1),])
    m4 = glmnet(as.matrix(df[1:(H-1),2:3]), df[1:(H-1),1], lambda = 0.01)
    
    return(c(sapply(list( predict(m1, newdata=df[H:N, ]),
                          predict(m2, newdata=df[H:N, ]), 
                          predict(m3, newdata=df[H:N, ]), 
                          predict(m4, newx=as.matrix(df[H:N,2:3 ]))[,1]  ), function(p) RMSE(p, Y[H:N] )),
             mean(apply(predict(m1, newdata=df[H:N, ], interval="confidence")[,2:3], 1, diff)),
             mean(apply(predict(m2, newdata=df[H:N, ], interval="confidence")[,2:3], 1, diff)),
             mean(apply(predict(m3, newdata=df[H:N, ], interval="confidence")[,2:3], 1, diff)),
             coef(m1),
             coef(m2),
             coef(m3),
             as.matrix(coef(m4))[,1]
           ))
  })

results = data.frame(t(results))
colnames(results) = c("RMSE_full", "RMSE_X", "RMSE_Con", "RMSE_ridge", 
                      "Un_full", "Un_X", "Un_Con",
                      "Full_inter", "Full_X", "Full_Con",
                      "X_inter", "X_X",
                      "Con_inter", "Con_Con",
                      "Ridge_inter", "Ridge_X", "Ridge_Con"
                      )
apply(results, 2, mean)
