### simulate to see whether my stan file is correct or not
library(MASS)
library(dplyr)
library(rstan)

x = data.frame(
x1 = rnorm(100),
x2 = rnorm(100),
x3 = rnorm(100))
beta_1 = 0.8
beta_2 = 0.5
 beta_3 = 0.7

 Sigma <- matrix(c(1,0.2,0.8,0.2,1,0.3,0.8, 0.3, 1),3,3)
 Sigma
 err = mvrnorm(n = 100, mu = rep(0,3),Sigma = Sigma) %>%
   as.data.frame(.) %>% setNames(c('err1','err2','err3'))

df_simulation = cbind(x, err) %>%
  mutate (y1 = ifelse(x1*beta_1 + err1>0,1,0),
          y2 = ifelse(x2*beta_2 + err2>0,1,0),
          y3 = x3*beta_3 + err3)

mod = stan_model ('model.stan')
model_res = NULL
model_value = -Inf
i = 1
while (i<30){
  temp = optimizing (mod, data = list(y = df_simulation[,c('y1','y2','y3')],
                                      x_1 = matrix(df_simulation$x1),
                                      x_2 = matrix(df_simulation$x2),
                                      x_3 = matrix(df_simulation$x3),
                                      k_1 = 1,
                                      k_2 = 1,
                                      k_3 = 1,
                                      N = nrow(df_simulation)
  ),
  #  verbose = TRUE,
  hessian = TRUE, as_vector = FALSE)
  if (temp$value>model_value){
    model_res = temp
    model_value = temp$value
  }
  i = i+1
}
