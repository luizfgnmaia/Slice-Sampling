## 2. Single-variable Metropolis

log_target_v <- function(v, x) {
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

log_target_x <- function(v, x) {
  dnorm(x = x, mean = 0, sd = sqrt(exp(v)), log = TRUE)
}

n_it = 2000 + 1
n_updates = 1300
v = 0
v_atual = 0
x = matrix(NA, ncol = 9, nrow = n_it)
x[1,] = 1
x_atual = x[1,]
x_prop = rep(NA, 9)
log_target_x_prop = rep(NA, 9)
log_target_x_atual = rep(NA, 9)
cont_acc_v = 0
cont_acc_x = rep(0, 9)

set.seed(9)
inicio = Sys.time()

for(it in 2:n_it) {
  for(up in 1:n_updates) {
    v_prop = v_atual + rnorm(1)
    log_target_v_atual = log_target_v(v_atual, x_atual) # não consegui fazer salvando isso
    log_target_v_prop = log_target_v(v_prop, x_atual)
    if(log(runif(1)) < (log_target_v_prop - log_target_v_atual)) {
      v_atual = v_prop
      cont_acc_v = cont_acc_v + 1
    }
    for(i in 1:9) {
      x_prop[i] = x_atual[i] + rnorm(1)
      log_target_x_atual[i] = log_target_x(v_atual, x_atual[i])
      log_target_x_prop[i] = log_target_x(v_atual, x_prop[i])
      if(log(runif(1)) < (log_target_x_prop[i] - log_target_x_atual[i])) {
        x_atual[i] = x_prop[i]
        cont_acc_x[i] = cont_acc_x[i] + 1
      }
    }
  }
  v[it] = v_atual
  x[it,] = x_atual
}

duracao = Sys.time() - inicio

v2 = v
save(v2, file = "data/v2.RData")
save.image(file = "data/sim2.RData")
