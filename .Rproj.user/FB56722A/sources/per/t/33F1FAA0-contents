## 2. Single-variable Metropolis

library(progress)

log_target_v <- function(v) { 
  dnorm(x = v, mean = 0, sd = 3, log = TRUE)
}

log_target_x <- function(x, v) {
  dnorm(x = x, mean = 0, sd = sqrt(exp(v)), log = TRUE)
}

n_it = 2000 + 1
n_updates = 1300
v = 0
v_atual = 0
x = matrix(NA, ncol = 9, nrow = n_it)
x[1,] = 1
x_atual = x[1,]
log_target_v_atual = log_target_v(v_atual)
log_target_x_atual = rep(log_target_x(x_atual, v_atual), 9)
x_prop = NULL
log_target_x_prop = NULL

set.seed(9)
inicio = Sys.time()
pb = progress_bar$new(format = "[:bar] :percent in :elapsed",
                      total = 100, clear = FALSE, width = 80)
pb$tick(1/n_it)
for(it in 2:n_it) {
  for(up in 1:n_updates) {
    v_prop = v_atual + rnorm(1)
    log_target_v_prop = log_target_v(v_prop)
    if(log(runif(1)) < (log_target_v_prop - log_target_v_atual)) {
      v_atual = v_prop
    }
  }
  v[it] = v_atual
  for(i in 1:9) {
    for(up in 1:n_updates) {
      x_prop[i] = x_atual[i] + rnorm(1)
      log_target_x_prop[i] = log_target_x(x_prop[i], v_atual)
      if(log(runif(1)) < (log_target_x_prop[i] - log_target_x_atual[i])) {
        x_atual[i] = x_prop[i]
      }
    }
    x[it,] = x_atual
  }
  pb$tick(100/n_it)  
}
duracao = Sys.time() - inicio

v2 = v
save(v2, file = "data/v2.RData")
save.image(file = "data/sim2.RData")






