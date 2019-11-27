## 3. Multivariate Metropolis Random Variance

library(progress)

log_target <- function(v, x) { # x é um vetor de 9 posições
  dnorm(x = v, mean = 0, sd = 3, log = TRUE) + 
    dnorm(x = x[1], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x[2], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x[3], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x[4], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x[5], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x[6], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x[7], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x[8], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x[9], mean = 0, sd = sqrt(exp(v)), log = TRUE)
}

n_it = 2000 + 1
n_updates = 10000
v = 0
v_atual = 0
x = matrix(NA, ncol = 9, nrow = n_it)
x[1,] = 1
x_atual = x[1,]
log_target_atual = log_target(v_atual, x_atual)

set.seed(11)
inicio = Sys.time()
pb = progress_bar$new(format = "[:bar] :percent in :elapsed",
                      total = 100, clear = FALSE, width = 80)
pb$tick(1/n_it)
for(it in 2:n_it) {
  for(up in 1:n_updates) {
    u = runif(1, min = -3, max = 3)
    sd_prop = 10^u
    v_prop = v_atual + rnorm(1, sd = sd_prop)
    x_prop = x_atual + rnorm(9, sd = sd_prop)
    log_target_prop = log_target(v_prop, x_prop)
    if(log(runif(1)) < (log_target_prop - log_target_atual)) {
      v_atual = v_prop
      x_atual = x_prop
      log_target_atual = log_target_prop
    }
  }
  v[it] = v_atual
  x[it,] = x_atual
  pb$tick(100/n_it)
}
duracao = Sys.time() - inicio

v3 = v
save(v3, file = "data/v3.RData")
save.image(file = "data/sim3.RData")






