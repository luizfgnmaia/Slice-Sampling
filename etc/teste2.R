# testar a ordem dos for

## 2. Single-variable Metropolis

library(progress)

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
log_target_v_atual = log_target_v(v_atual, x_atual)
x_prop = x_atual

set.seed(9999)
inicio = Sys.time()
pb = progress_bar$new(format = "[:bar] :percent in :elapsed",
                      total = 100, clear = FALSE, width = 80)
pb$tick(1/n_it)
for(it in 2:n_it) {
  for(up in 1:n_updates) {
    v_prop = v_atual + rnorm(1)
    log_target_v_prop = log_target_v(v_prop, x_atual)
    if(log(runif(1)) < (log_target_v_prop - log_target_v_atual)) {
      v_atual = v_prop
      log_target_v_atual = log_target_v_prop
    }
  }
  v[it] = v_atual
  for(i in 1:9) {
    log_target_x_atual = log_target_x(v_atual, x_atual[i])
    for(up in 1:n_updates) {
      x_prop[i] = x_atual[i] + rnorm(1)
      log_target_x_prop = log_target_x(v_atual, x_prop[i])
      if(log(runif(1)) < (log_target_x_prop - log_target_x_atual)) {
        x_atual[i] = x_prop[i]
        log_target_x_atual = log_target_x_prop
      }
    }
    x[it,] = x_atual
  }
  pb$tick(100/n_it)  
}
duracao = Sys.time() - inicio

#v2 = v
#save(v2, file = "data/v2.RData")
#save.image(file = "data/sim2.RData")

library(dplyr)
library(ggplot2)

tib = tibble(it = 0:2000, v)

tib %>%
  ggplot(aes(x = it, y = v)) +
  geom_point() +
  xlab("") +
  ylab("") +
  ylim(-10, 10) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = margin(0, 0.5, 0, 0, "cm")) +
  geom_hline(yintercept = 7.5, linetype = "dashed") +
  geom_hline(yintercept = -7.5, linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0))

