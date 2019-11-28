## 4. Single-variable Slice Sampling

log_target_v <- function(v) {
  dnorm(x = v, mean = 0, sd = 3, log = TRUE) + 
    dnorm(x = x_atual[1], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x_atual[2], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x_atual[3], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x_atual[4], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x_atual[5], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x_atual[6], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x_atual[7], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x_atual[8], mean = 0, sd = sqrt(exp(v)), log = TRUE) + 
    dnorm(x = x_atual[9], mean = 0, sd = sqrt(exp(v)), log = TRUE)
}

log_target_x <- function(x) {
  dnorm(x = x, mean = 0, sd = sqrt(exp(v_atual)), log = TRUE)
}

stepping_out <- function(g, x0, y, w, m = 10^10) {
  U = runif(1)
  L = x0 - w * U
  R = L + w
  V = runif(1)
  J = floor(m * V)
  K = (m - 1) - J
  while(J > 0 & y < g(L)) {
    L = L - w
    J = J - 1
  }
  while(K > 0 & y < g(R)) {
    R = R + w
    K = K - 1
  }
  c(L, R)
}

shrinkage <- function(g, x0, y, L, R) {
  Lbar = L
  Rbar = R
  while(TRUE) {
    U = runif(1)
    x1 = Lbar + U * (Rbar - Lbar)
    if(y < g(x1)) {
      break
    }
    if(x1 < x0) {
      Lbar = x1
    } else {
      Rbar = x1
    }
  }
  x1
}

n_it = 2000 + 1
n_updates = 120
w = 1
v = 0
v_atual = 0
x = matrix(NA, ncol = 9, nrow = n_it)
x[1,] = 1
x_atual = x[1,]

set.seed(1511)
inicio = Sys.time()

for(it in 2:n_it) {
  for(up in 1:n_updates) {
    y = log_target_v(v_atual) - rexp(1)
    I = stepping_out(g = log_target_v, x0 = v_atual, y = y, w = w)
    v_atual = shrinkage(g = log_target_v, x0 = v_atual, y = y, L = I[1], R = I[2])
    for(i in 1:9) {
      y = log_target_x(x_atual[i]) - rexp(1)
      I = stepping_out(g = log_target_x, x0 = x_atual[i], y = y, w = w)
      x_atual[i] = shrinkage(g = log_target_x, x0 = x_atual[i], y = y, L = I[1], R = I[2])
    }
  }
  v[it] = v_atual
  x[it,] = x_atual
}

duracao = Sys.time() - inicio

v4 = v
save(v4, file = "data/v4.RData")
save.image(file = "data/sim4.RData")
