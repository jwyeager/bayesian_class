###################################################
#                                                 #
#                 Project Script                  #
#                                                 #
###################################################
library(tidyverse)
library(magrittr)
library(R2OpenBUGS)
library(rjags)
library(jagsUI)
library(coda)
##### Data Prep ####
# det <- read_csv('SI_Detections_StationName_24-25.csv')
# 
# det.summary <- det %>% group_by(Station) %>% 
#   summarise(n.dets = n(), n.ind = length(unique(Transmitter)))
# 
# bio <- read_csv('MSCIP_2024_biomass.csv')
# bio$sample.id <- gsub("-", "_", bio$sample.id)
# bio <- bio %>% mutate(sta = str_sub(sample.id, 1, -3)) %>% 
#   mutate(rep = rep(c("A", "B", "C"), length.out = n()))
# bio.summary <- bio %>% group_by(sta) %>% 
#   summarise(mean.ann = mean(ann), mean.mol = mean(mol), mean.art = mean(art),
#             mean.ech = mean(ech), mean.misc = mean(misc), mean.tot = mean(tot.sample.biomass))
# 
# d <- inner_join(det.summary, bio.summary, by = c('Station' = 'sta'))
# 
# sed <- read_csv('MSCIP_2024_sediment.csv')
# sed$sta <- gsub("-","_", sed$sta)
# 
# d <- inner_join(d, sed, by = c('Station' = 'sta'))
# 
# wq <- read_csv('MSCIP_2024_WQ.csv')
# wq$sta.scratch <- gsub("-","_", wq$sta.scratch)
# wq <- wq %>% mutate(sta.left = str_extract(sta.scratch, "^[^ ]+"))
# sta.left.list <- wq %>% filter(!sta.left %in% c('M','S'))
# sta.left.list <- sta.left.list$sta.left
# wq <- wq %>% mutate(strata = rep(c("B", "M", "S"), length.out = n())) %>% 
#   mutate(sta = rep(sta.left.list, each = 3))
# wq.bot <- wq %>% select(sta, strata, 3:9) %>% filter(strata == 'B') %>% 
#   rename_with(~ paste0(.x, ".bot"), 3:9) %>% select(!strata)
# wq.mid <- wq %>% select(sta, strata, 3:9) %>% filter(strata == 'M') %>% 
#   rename_with(~ paste0(.x, ".mid"), 3:9) %>% select(!strata)
# wq.sur <- wq %>% select(sta, strata, 3:9) %>% filter(strata == 'S') %>% 
#   rename_with(~ paste0(.x, ".sur"), 3:9) %>% select(!strata)
# wq.pivot <- full_join(wq.bot, wq.mid, by = 'sta')
# wq.pivot <- full_join(wq.pivot, wq.sur, by = 'sta')
# 
# d <- inner_join(d, wq.pivot, by = c('Station' = 'sta'))
# 
# station.coords <- read_csv('SI_Station_Coords_Match.csv')
# 
# d <- inner_join(d, station.coords, by = c('Station' = 'sta.name'))
# write_csv(d, file = 'SI_BI_GS_AllCovariates.csv')

start.date <- mdy('09/01/2024')
end.date <- mdy('03/31/2025')
d.all <- read.csv('SI_BI_GS_AllCovariates.csv')
d.all$p.comb.sand <- (d.all$per.sand.c + d.all$per.sand.f + d.all$per.sand.m)/100

N <- length(d.all$n.dets)
n.dets  <-  as.numeric(d.all$n.dets)
mean.tot  <-  as.numeric(d.all$mean.tot)
p.comb.sand  <-  as.numeric(d.all$p.comb.sand)

E <- 1
coords <- cbind(d.all$lon, d.all$lat)
D <- as.matrix(dist(coords))

##### Build Model ####
jags_data <- list(
  N = N,
  n.dets = n.dets,
  mean.tot = mean.tot,
  p.comb.sand = p.comb.sand
)

model_string <- "
model {

  for (i in 1:N) {

    n.dets[i] ~ dnegbin(p[i], r)

    mu[i] <- exp(eta[i])
    p[i] <- r / (r + mu[i])

    eta[i] <- alpha
              + beta_biomass * log(mean.tot[i] + 1)
              + beta_sand * p.comb.sand[i]
              + eps[i]

    eps[i] ~ dnorm(0, tau_eps)

    # posterior predictive replicate
    n.rep[i] ~ dnegbin(p[i], r)

    # squares for variance calculation
    n.dets_sq[i] <- pow(n.dets[i], 2)
    n.rep_sq[i]  <- pow(n.rep[i], 2)
  }

  tau_eps <- pow(sigma_eps, -2)

  # priors
  alpha ~ dnorm(0, 0.04)
  beta_biomass ~ dnorm(0, 0.25)
  beta_sand ~ dnorm(0, 0.25)

  sigma_eps ~ dunif(0, 5)
  r ~ dgamma(0.1, 0.1)

  ########################################
  # Discrepancy statistics
  ########################################

  # sums
  sum_obs <- sum(n.dets[])
  sum_rep <- sum(n.rep[])

  # means
  mean_obs <- sum_obs / N
  mean_rep <- sum_rep / N

  # second moments
  mean_sq_obs <- sum(n.dets_sq[]) / N
  mean_sq_rep <- sum(n.rep_sq[]) / N

  # variances
  var_obs <- mean_sq_obs - pow(mean_obs, 2)
  var_rep <- mean_sq_rep - pow(mean_rep, 2)

}
"

writeLines(model_string, "model_ppc.jags")

inits <- function() {
  list(
    alpha = 0,
    beta_biomass = 0,
    beta_sand = 0,
    sigma_eps = 1,
    r = 1,
    eps = rnorm(N, 0, 0.1)
  )
}

params <- c(
  "alpha",
  "beta_biomass",
  "beta_sand",
  "sigma_eps",
  "r"
)
params_ppc <- c(
  "sum_obs", "sum_rep",
  "var_obs", "var_rep"
)

jags_model <- jags.model(
  file = "model_ppc.jags",
  data = jags_data,
  inits = inits,
  n.chains = 3,
  n.adapt = 2000
)

update(jags_model, 10000)

samples <- coda.samples(
  jags_model,
  variable.names = params,
  n.iter = 20000,
  thin = 10
)

samples_ppc <- coda.samples(
  jags_model,
  variable.names = params_ppc,
  n.iter = 10000
)

# traceplots
plot(samples)

# convergence
gelman.diag(samples)

# effective sample sizes
effectiveSize(samples)

m <- as.matrix(samples_ppc)
ppp_sum <- mean(m[, "sum_rep"] > m[, "sum_obs"])
ppp_sum
ppp_var <- mean(m[, "var_rep"] > m[, "var_obs"])
ppp_var

