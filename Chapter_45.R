# Chapter 4
rm(list=ls())

## 4.1.1
# Figure 4.2
library(tidyverse)

# We set the seed to make the results of `runif()` reproducible.
#data simulation
set.seed(1000)
pos <- 
  replicate(100, runif(16, -1, 1)) %>%        # Here's the simulation
  as_tibble() %>%                             # For data manipulation, we'll make this a tibble
  rbind(0, .) %>%                             # Here we add a row of zeros above the simulation results
  mutate(step = 0:16) %>%                     # This adds a step index
  gather(key, value, -step) %>%               # Here we convert the data to the long format
  mutate(person = rep(1:100, each = 17)) %>%  # This adds a person id index
  # The next two lines allow us to make culmulative sums within each person
  group_by(person) %>%
  mutate(position = cumsum(value)) %>%
  ungroup()  # Ungrouping allows for further data manipulation%>%                             # Here we add a row of zeros above the simulation results

  # plot
  ggplot(data = pos, 
         aes(x = step, y = position, group = person)) +
    geom_vline(xintercept = c(4, 8, 16), linetype = 2) +
    geom_line(aes(color = person < 2, alpha  = person < 2)) +
    scale_color_manual(values = c("skyblue4", "black")) +
    scale_alpha_manual(values = c(1/5, 1)) +
    scale_x_continuous("step number", breaks = c(0, 4, 8, 12, 16)) +
    theme(legend.position = "none")

  ## 4.1.2
  set.seed(1)
  prod(1 + runif(12, 0, 0.1))
  
  set.seed(.1)
  growth <- 
    replicate(10000, prod(1 + runif(12, 0, 0.1))) %>%
    as_tibble()
  
  ggplot(data = growth, aes(x = value)) +
    geom_density()

  big <- replicate( 10000 , prod( 1 + runif(12,0,0.5) ) )
  small <- replicate( 10000 , prod( 1 + runif(12,0,0.01) ) )

  # combine them
  tibble(samples      = c(big, small),
         distribution = rep(c("big", "small"), each = 10000)) %>% 
    
    # plot
    ggplot(aes(x = samples)) +
    geom_density(fill = "black", color = "transparent") +
    facet_wrap(~ distribution, scales = "free")  
  
  ## 4.1.3
  # 原本的big(@4.1.2)不是常態分佈，取完log之後也變成是
  set.seed(12)
  replicate(10000, log(prod(1 + runif(12, 0, 0.5)))) %>%
    as_tibble() %>% 
    
    ggplot(aes(x = value)) +
    geom_density(color = "transparent", 
                 fill = "gray33")

  
# 4.2
## 4.2.1

# how many `p_grid` points would you like?
n_points <- 100

d <-
  tibble(w          = 6, 
         n          = 9,
         p_grid     = seq(from = 0, to = 1, length.out = n_points)) %>% 
  mutate(prior      = dunif(p_grid, 0, 1),
         likelihood = dbinom(w, n, p_grid)) %>% 
  mutate(posterior  = likelihood * prior) %>% 
  # this last bit converts the posterior to the probability scale
  mutate(posterior  = posterior / sum(posterior))
  

# plot prior, likelihood, and posterior 
d %>% 
  select(-w, -n) %>% 
  gather(key, value, -p_grid) %>% 
  # this line allows us to dictate the order the panels will appear in
  mutate(key = factor(key, levels = c("prior", "likelihood", "posterior"))) %>% 
  
  ggplot(aes(x = p_grid, ymin = 0, ymax = value, fill = key)) +
  geom_ribbon() +
  scale_fill_manual(values = c("blue", "red", "purple")) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(legend.position = "none") +
  facet_wrap(~key, scales = "free")  

# 4.3
## 4.3.1
library(rethinking)  
data(Howell1)
d <- Howell1  

str(d)  

d2 <- 
  d %>%
  filter(age >= 18)


## 4.3.2
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )


## 4.3.3 Grid approximation of the posterior distribution

n <- 200

d_grid <-
  tibble(mu    = seq(from = 140, to = 160, length.out = n),
         sigma = seq(from = 4,   to = 9,   length.out = n)) %>% 
  # we'll accomplish with `tidyr::expand()` what McElreath did with base R `expand.grid()`
  expand(mu, sigma)


grid_function <- function(mu, sigma){
  dnorm(d2$height, mean = mu, sd = sigma, log = T) %>% 
    sum()
}


d_grid <-
  d_grid %>% 
  mutate(log_likelihood = map2(mu, sigma, grid_function)) %>% 
  unnest() %>% 
  mutate(prior_mu       = dnorm(mu,    mean = 178, sd  = 20, log = T),
         prior_sigma    = dunif(sigma, min  = 0,   max = 50, log = T)) %>% 
  mutate(product        = log_likelihood + prior_mu + prior_sigma) %>% 
  mutate(probability    = exp(product - max(product)))

head(d_grid)

d_grid %>% 
  ggplot(aes(x = mu, y = sigma, z = probability)) + 
  geom_contour() +
  labs(x = expression(mu),
       y = expression(sigma)) +
  coord_cartesian(xlim = range(d_grid$mu),
                  ylim = range(d_grid$sigma)) +
  theme(panel.grid = element_blank())

# heat map
d_grid %>% 
  ggplot(aes(x = mu, y = sigma)) + 
  geom_raster(aes(fill = probability)) +
  scale_fill_viridis_c() +
  labs(x = expression(mu),
       y = expression(sigma)) +
  theme(panel.grid = element_blank())

# 4.3.3 textbook code
mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

contour_xyz( post$mu , post$sigma , post$prob )

image_xyz( post$mu , post$sigma , post$prob )


## 4.3.4 (textbook)
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

plot(sample.mu, sample.sigma, cex=0.5, pch=16, col=col.alpha(rangi2,0.1))

dens( sample.mu )
dens( sample.sigma )

HPDI( sample.mu )
HPDI( sample.sigma )

d3 <- sample( d2$height , size=20 )

mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
  sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
              log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
  dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                        prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="mu" , ylab="sigma" , pch=16 )

dens( sample2.sigma , norm.comp=TRUE )


## 4.3.5
library(brms)
b4.1 <- 
  brm(data = d2, family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 20), class = Intercept),
                prior(uniform(0, 50), class = sigma)),
      iter = 31000, warmup = 30000, chains = 4, cores = 4)

print(b4.1)
plot(b4.1)

# try rstan
stanmodel = "
data { 
int length;
real height[length];
}
parameters { // defines the unknowns to be estimate
real mu;
real sigma;
}
model {
// Define the priors
sigma ~ uniform(0, 50); 
mu ~ normal(178, 20);

// The likelihood
height ~ normal(mu, sigma);
}"

data_4.1 = list(height = d2$height, length = length(d2$height))
model_4.1 = stan(model_code = stanmodel, # stan program
                 data = data_4.1, # named list of data
                 cores = 1, # number of cores (could use one per chain)
                 chains = 4, # number of Markov chains
                 warmup = 30000, # number of warmup iterations per chain
                 iter = 31000) # total number of iterations per chain
print(model_4.1)
plot(model_4.1)



# brm, cauchy distribution
b4.1_half_cauchy <- 
  brm(data = d2, family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 20), class = Intercept),
                prior(cauchy(0, 1), class = sigma)), # only change the prior distribution
      iter = 2000, warmup = 1000, chains = 4, cores = 4) # and iterations

print(b4.1_half_cauchy)
plot(b4.1_half_cauchy)

## 4.3.6
# see variance and co-variance
vcov(b4.1_half_cauchy)
diag( vcov( b4.1_half_cauchy) )
cov2cor( vcov( b4.1_half_cauchy ) )

# extract samples
post <- extract(model_4.1, permuted = F)
post_mu <- extract(model_4.1, pars = "mu", permuted = FALSE)
post_sigma <- extract(model_4.1, pars = "sigma", permuted = F)
cov(post_mu, post_sigma)

d_cov <- data.frame(mu = as.vector(post_mu), sigma = as.vector(post_sigma))
precis(d_cov)



# 4.4 Adding a predictor
plot(height ~ weight, data = d2)

# rstan
stanmodel_4.2 = "
data { 
int length;
real height[length];
real weight[length];
}
parameters { // defines the unknowns to be estimate
real a;
real b;
real sigma;
}
model {
// Define the priors
sigma ~ uniform(0, 50); 
a ~ normal(156, 100);
b ~ normal(0, 10);

vector[length] mu = a + b * weight;

// The likelihood
height ~ normal(mu, sigma);
}"

data_4.2 = list(height = d2$height, length = length(d2$height), weight = length(d2$weight))
model_4.2 = stan(model_code = stanmodel_4.2, # stan program
                 data = data_4.2, # named list of data
                 cores = 1, # number of cores (could use one per chain)
                 chains = 4, # number of Markov chains
                 warmup = 2000, # number of warmup iterations per chain
                 iter = 1000) # total number of iterations per chain
print(model_4.2)


#5 Multivariate Linear Models 
library(dplyr); library(tidyr); library(rstan); library(skimr); library(ggplot2); library(ggthemes)
theme_set(theme_tufte(base_family = 'sans'))




# Chapter 5


data('WaffleDivorce', package = 'rethinking')
d  <- WaffleDivorce; rm(WaffleDivorce)


#Figure 5.1


ggplot(d) +
  stat_smooth(aes(WaffleHouses/Population, Divorce), method = 'lm', level = .89, 
              fullrange = T, color = 'black', alpha = .1, lwd = .3) +
  geom_point(aes(WaffleHouses/Population, Divorce), 
             shape = 21, color = 'dodgerblue') +
  geom_text(data = filter(d, Loc %in% c('AL', 'AR', 'GA', 'OK', 'ME', 'NJ', 'SC')),
            aes(x = WaffleHouses/Population + 1.2, y = Divorce, label = Loc), size = 3) +
  scale_x_continuous(limits = c(0, 50)) +
  labs(x = 'Waffle Houses per million', y = 'Divorce Rate')


## 5.1 Spurious association

d <- d %>% 
  mutate(MedianAgeMarriage_z = (MedianAgeMarriage - mean(MedianAgeMarriage)) / 
           sd(MedianAgeMarriage),
         Marriage_z = (Marriage - mean(Marriage)) / sd(Marriage))

m05_1 <- "
data {
int<lower=0> N;
vector[N] divorce;
vector[N] median_age_z;
}
parameters {
real a;
real bA;
real<lower=0, upper=10> sigma;
}
model {
vector[N] mu = a + median_age_z * bA;

divorce ~ normal(mu, sigma);
a ~ normal(10, 10);
bA ~ normal(0, 10);
}"




dat <- list(
  N = NROW(d),
  divorce = d$Divorce,
  median_age_z = d$MedianAgeMarriage_z
)

fit05_1 <- stan(model_code = m05_1, data = dat, cores = 2, chains = 2, iter = 1000)
print(fit05_1, probs = c(0.10, 0.5, 0.9))
plot(fit05_1, plotfun = "trace", inc_warmup = TRUE)

m05_2 <- "
data {
int N;
vector[N] divorce;
vector[N] marriage_z;
}
parameters {
real a;
real bM;
real<lower=0> sigma;
}
model {
vector[N] mu = a + marriage_z * bM;

divorce ~ normal(mu, sigma);
a ~ normal(10, 10);
bM ~ normal(0, 10);
}
"

dat <- list(
  N = NROW(d),
  divorce = d$Divorce,
  marriage_z = d$Marriage_z
)

fit05_2 <- stan(model_code = m05_2, data = dat, cores = 2, chains = 2, iter = 1000)

print(fit05_2, probs = c(.1, .5, .9))


# draw from posterior samples
post <- as.data.frame(fit05_1)

# recreate mu and simulate it with new data
f_mu <- function(x) post$a + post$bA * x
A_z_new <- seq(-3, 3)

mu <- 
  sapply(A_z_new, f_mu) %>%
  as_tibble() %>%
  rename_all(function(x) A_z_new) %>%
  mutate(Iter = row_number()) %>%
  gather(A_z, divorce, -Iter) %>%
  group_by(A_z) %>%
  mutate(hpdi_l = HDInterval::hdi(divorce, credMass = 0.8)[1],
         hpdi_h = HDInterval::hdi(divorce, credMass = 0.8)[2]) %>%
  mutate(mu = mean(divorce)) %>%
  ungroup() %>%
  mutate(A_z = as.numeric(A_z))

# plot raw data and model estimate of mu
p <- ggplot() 
p1 <- p + 
  geom_point(data = d,
             aes(MedianAgeMarriage_z, Divorce), 
             shape = 1, color = 'dodgerblue') +
  geom_ribbon(data = mu,
              aes(x = A_z, ymin = hpdi_l, ymax = hpdi_h), alpha = .1) +
  geom_line(data = mu,
            aes(x = A_z, y = mu))





# draw from posterior samples
post <- as.data.frame(fit05_2)

# recreate mu and simulate it with new data
f_mu <- function(x) post$a + post$bM * x
M_z_new <- seq(-3, 3)

mu <- 
  sapply(M_z_new, f_mu) %>%
  as_tibble() %>%
  rename_all(function(x) M_z_new) %>%
  mutate(Iter = row_number()) %>%
  gather(M_z, divorce, -Iter) %>%
  group_by(M_z) %>%
  mutate(hpdi_l = HDInterval::hdi(divorce, credMass = 0.8)[1],
         hpdi_h = HDInterval::hdi(divorce, credMass = 0.8)[2]) %>%
  mutate(mu = mean(divorce)) %>%
  ungroup() %>%
  mutate(M_z = as.numeric(M_z))

# plot raw data and model estimate of mu
p2 <- p + 
  geom_point(data = d,
             aes(Marriage_z, Divorce), 
             shape = 1, color = 'dodgerblue') +
  geom_ribbon(data = mu,
              aes(x = M_z, ymin = hpdi_l, ymax = hpdi_h), alpha = .1) +
  geom_line(data = mu,
            aes(x = M_z, y = mu))





library(gridExtra)
grid.arrange(p2, p1, nrow = 1)


### 5.1.2. Fitting the model

m05_3 <- "
data {
int N;
vector[N] divorce;
vector[N] marriage_z;
vector[N] median_age_z;
}
parameters {
real a;
real bA;
real bM;
real<lower=0> sigma;
}
model {
vector[N] mu = a + median_age_z * bA + marriage_z * bM;

divorce ~ normal(mu, sigma);
a ~ normal(10, 10);
bA ~ normal(0, 10);
bM ~ normal(0, 10);
sigma ~ exponential(1);
}
"
dat = list(
  N = NROW(d),
  divorce = d$Divorce,
  marriage_z = d$Marriage_z,
  median_age_z = d$MedianAgeMarriage_z
)

fit05_3 <- stan(model_code = m05_3, data = dat, cores = 2, chains = 2, iter = 1000)
print(fit05_3, probs = c(0.1, 0.5, 0.9))


### 5.1.3. Plotting multivariate posteriors


m05_4 <- "
data {
int<lower=1> N;
vector[N] A_z;
vector[N] M_z;
} 
parameters {
real a;
real b;
real<lower=0> sigma;
}
model {
vector[N] mu = a + A_z * b;

M_z ~ normal(mu, sigma);
a ~ normal(0, 10);
b ~ normal(0, 10);
sigma ~ exponential(1);
}
"

dat <- list(
  N = NROW(d),
  A_z = d$MedianAgeMarriage_z,
  M_z = d$Marriage_z
)

fit05_4 <- stan(model_code = m05_4, data = dat, cores = 2, chains = 2, iter = 1000)
print(fit05_4, probs = c(0.1, 0.5, 0.9))


#### 5.1.3.1 Predictor residual plots


post <- as.matrix(fit05_4)

mu <- post[,"a"] + d$MedianAgeMarriage_z %*% t(post[,"b"])
mu <- rowMeans(mu)
resid <- d$Marriage_z - mu

ggplot() + 
  geom_segment(aes(x = d$MedianAgeMarriage_z, 
                   xend = d$MedianAgeMarriage_z,
                   y = mu, yend = d$Marriage_z)) +
  geom_point(data = d,
             aes(MedianAgeMarriage_z, Marriage_z), 
             shape = 21, color = 'dodgerblue', fill = 'white') +
  geom_abline(aes(slope = mean(post[,"b"]), intercept = mean(post[,"a"])))



#### 5.1.3.2 counterfactual plots


# get draws for parameters
post <- as.matrix(fit05_3)

# setup new data
nd <- 
  expand.grid(median_age_z = seq(-3, 3), 
              marriage_z = seq(-3, 3)) %>% 
  as.matrix

# estimate mu
mu <- post[,1] + post[,2:3] %*% t(nd)

# get stats on mu
avg <- colMeans(mu)
hdi <- apply(mu, 2, HDInterval::hdi)

# simulate divorce rate
iter <- 1e4
y_hat <- matrix(nrow = iter, ncol = NROW(nd))
for(i in 1:NROW(nd)) y_hat[,i] <- rnorm(iter, post[,1] + post[,2:3] %*% as.matrix(nd[i,]), post[,4])

# get stats on sim
y_hat_avg <- colMeans(y_hat)
y_hat_hdi <- apply(y_hat, 2, HDInterval::hdi)

nd <- 
  as_tibble(nd) %>%
  bind_cols(avg = avg, 
            mu_hdi_l = hdi[1,], 
            mu_hdi_h = hdi[2,],
            y_hdi_l = y_hat_hdi[1,],
            y_hdi_h = y_hat_hdi[2,])


# Setup Figure 5.5a
p1 <- ggplot(nd, aes(x = median_age_z, y = avg, group = marriage_z)) + 
  geom_line(data = nd,
            color = 'gray90') +
  geom_ribbon(data = nd %>% filter(marriage_z == 0),
              aes(x = median_age_z, ymin = mu_hdi_l, ymax = mu_hdi_h), alpha = .1) +
  geom_ribbon(data = nd %>% filter(marriage_z == 0),
              aes(x = median_age_z, ymin = y_hdi_l, ymax = y_hdi_h), alpha = .1) +
  geom_line(data = nd %>% filter(marriage_z == 0),
            aes(x = median_age_z, y = avg)) + 
  geom_text(data = nd %>% filter(median_age_z == min(median_age_z)), 
            aes(label = marriage_z, x = median_age_z - 0.1, y = avg), size = 2) +
  geom_text(data = nd %>% filter(median_age_z == max(median_age_z)), 
            aes(label = marriage_z, x = median_age_z + 0.1, y = avg), size = 2) +
  labs(x = 'Standardized Median Age of Marriage', y = 'Divorce rate') 


# Setup Figure 5.5b


p2 <- ggplot(nd, aes(x = marriage_z, y = avg, group = median_age_z)) + 
  geom_line(data = nd,
            color = 'gray90') +
  geom_ribbon(data = nd %>% filter(median_age_z == 0),
              aes(x = marriage_z, ymin = mu_hdi_l, ymax = mu_hdi_h), alpha = .1) +
  geom_ribbon(data = nd %>% filter(median_age_z == 0),
              aes(x = marriage_z, ymin = y_hdi_l, ymax = y_hdi_h), alpha = .1) +
  geom_line(data = nd %>% filter(median_age_z == 0),
            aes(x = marriage_z, y = avg)) + 
  geom_text(data = nd %>% filter(marriage_z == min(marriage_z)), 
            aes(label = median_age_z, x = marriage_z - 0.1, y = avg), size = 2) +
  geom_text(data = nd %>% filter(marriage_z == max(marriage_z)), 
            aes(label = median_age_z, x = marriage_z + 0.1, y = avg), size = 2) +
  labs(x = 'Standardized Rate of Marriage', y = 'Divorce rate')

#plot
grid.arrange(p2, p1, nrow = 1)



#### 5.1.3.3 posterior prediction plots


# estimate mu
mu <- post[,1] + post[,2:3] %*% t(d[,14:15])

# get stats on mu
avg <- colMeans(mu)
hdi <- apply(mu, 2, HDInterval::hdi)

# simulate divorce rate
iter <- 1e4
y_hat <- matrix(nrow = iter, ncol = NROW(d[,14:15]))
for(i in 1:NROW(d[,14:15])) y_hat[,i] <- rnorm(iter, post[,1] + post[,2:3] %*% t(d[i,14:15]), post[,4])

# get stats on sim
y_hat_avg <- colMeans(y_hat)
y_hat_hdi <- apply(y_hat, 2, HDInterval::hdi)
d <- d %>% mutate(mu = avg,
                  mu_hdi_l = hdi[1,],
                  mu_hdi_h = hdi[2,],
                  y_hdi_l = y_hat_hdi[1,],
                  y_hdi_h = y_hat_hdi[2,])


ggplot() + 
  geom_abline(intercept = 0, slope = 1, 
              linetype = 'dashed', color = 'gray70') +
  geom_segment(data = d,
               aes(x = Divorce, xend = Divorce, 
                   y = mu_hdi_l, yend = mu_hdi_h),
               color = 'dodgerblue') +
  geom_point(data = d,
             aes(Divorce, mu), 
             shape = 1, color = 'dodgerblue', fill = 'white') +
  labs(x = "Observed divorce rate", y = 'Estimated average divorce rate',
       subtitle = 'Observed versus\nestimated for each state')

#plot 5.6b
d1 <- d %>% 
  mutate(pred_err = Divorce - mu) %>%
  mutate(Loc = factor(Loc, levels = Loc[order(Divorce - mu)] ))

ggplot(d1) +
  geom_vline(xintercept = 0, color = 'gray80') +
  geom_segment(aes(x = -6, xend = 6, y = Loc, yend = Loc), 
               linetype = 'dotted', color = 'gray90') +
  geom_segment(aes(x = Divorce - mu_hdi_h, xend = Divorce - mu_hdi_l,
                   y = Loc, yend = Loc), 
               shape = 3, color = 'black') +
  geom_point(aes(Divorce - mu, Loc), shape = 21, fill = 'white') +
  geom_point(aes(Divorce - y_hdi_l, Loc), shape = 3, color = 'gray80') +
  geom_point(aes(Divorce - y_hdi_h, Loc), shape = 3, color = 'gray80') +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(x = '', y = '')


#plot 5.6c

ggplot() +
  stat_smooth(data = d,
              aes(WaffleHouses/Population, Divorce - mu), method = 'lm', fullrange = T, color = 'black', alpha = .2, lwd = .4) +
  geom_point(data = d,
             aes(WaffleHouses/Population, Divorce - mu), shape = 21, color = 'dodgerblue') +
  geom_text(data = filter(d, Loc %in% c('AL', 'AR', 'GA', 'OK', 'ME', 'NJ', 'SC')),
            aes(x = WaffleHouses/Population + 1.2, y = Divorce - mu, label = Loc), size = 3) +
  labs(x = 'Waffles per capita', y = 'Divorce error')


##5.2 Masked relationship

library(rethinking)
data(milk)
d <- milk
rm(milk)
detach(package:rethinking, unload = T)

#plot
d %>% 
  select(kcal.per.g, mass, neocortex.perc) %>% 
  pairs(col = "firebrick4")

dcc <- 
  d %>%
  filter(complete.cases(.))  

b5.5 <- "
data {
int<lower=1> N;
vector[N] kcal_per_g;
vector[N] neocortex_perc;
}
parameters {
real alpha;
real beta;
real sigma;
}
model {
vector[N] mu = alpha + beta * neocortex_perc;

kcal_per_g ~ normal(mu, sigma);
alpha ~ normal(0,100);
beta ~ normal(0,1);
sigma ~ cauchy(0, 1);
}
"
dat <- list(N = NROW(dcc),
            neocortex_perc = dcc$neocortex.perc,
            kcal_per_g = dcc$kcal.per.g)

fit.b5.5 <- stan(model_code = b5.5, data = dat, cores = 2, chains = 2, iter = 1000)
print(fit.b5.5, digit = 3)

#plot
fit.b5.5 <- as.data.frame(fit.b5.5)
f_mu <- function(x) fit.b5.5$alpha + fit.b5.5$beta * x
A_z_new <- seq(50, 80)

mu <- 
  sapply(A_z_new, f_mu) %>%
  as_tibble() %>%
  rename_all(function(x) A_z_new) %>%
  mutate(Iter = row_number()) %>%
  gather(A_z, kcal.per.g, -Iter) %>%
  group_by(A_z) %>%
  mutate(hpdi_l = HDInterval::hdi(kcal.per.g, credMass = 0.8)[1],
         hpdi_h = HDInterval::hdi(kcal.per.g, credMass = 0.8)[2]) %>%
  mutate(mu = mean(kcal.per.g)) %>%
  ungroup() %>%
  mutate(A_z = as.numeric(A_z))

# plot raw data and model estimate of mu
p <- ggplot() 
p1 <- p + 
  geom_point(data = dcc,
             aes(neocortex.perc, kcal.per.g), 
             shape = 1, color = 'dodgerblue') +
  geom_ribbon(data = mu,
              aes(x = A_z, ymin = hpdi_l, ymax = hpdi_h), alpha = .1) +
  geom_line(data = mu,
            aes(x = A_z, y = mu))

library(gridExtra)
grid.arrange(p1)

dcc <-
  dcc %>%
  mutate(log.mass = log(mass))

b5.6 <- "
data {
int<lower=1> N;
vector[N] kcal_per_g;
vector[N] log_mass;
}
parameters {
real alpha;
real beta;
real sigma;
}
model {
vector[N] mu = alpha + beta * log_mass;

kcal_per_g ~ normal(mu, sigma);
alpha ~ normal(0,100);
beta ~ normal(0,1);
sigma ~ uniform(0, 1);

}
"
dat <- list(N = NROW(dcc),
            log_mass = dcc$log.mass,
            kcal_per_g = dcc$kcal.per.g)

fit.b5.6 <- stan(model_code = b5.6, data = dat, cores = 2, chains = 2, iter = 1000)
plot(fit.b5.6, plotfun = "trace", inc_warmup = TRUE)



print(fit.b5.6, digits = 3)


#plot
fit.b5.6 <- as.data.frame(fit.b5.6)
f_mu <- function(x) fit.b5.6$alpha + fit.b5.6$beta * x
A_z_new <- seq(-3, 5)

mu <- 
  sapply(A_z_new, f_mu) %>%
  as_tibble() %>%
  rename_all(function(x) A_z_new) %>%
  mutate(Iter = row_number()) %>%
  gather(A_z, kcal.per.g, -Iter) %>%
  group_by(A_z) %>%
  mutate(hpdi_l = HDInterval::hdi(kcal.per.g, credMass = 0.8)[1],
         hpdi_h = HDInterval::hdi(kcal.per.g, credMass = 0.8)[2]) %>%
  mutate(mu = mean(kcal.per.g)) %>%
  ungroup() %>%
  mutate(A_z = as.numeric(A_z))

# plot raw data and model estimate of mu
p <- ggplot() 
p1 <- p + 
  geom_point(data = dcc,
             aes(log.mass, kcal.per.g), 
             shape = 1, color = 'dodgerblue') +
  geom_ribbon(data = mu,
              aes(x = A_z, ymin = hpdi_l, ymax = hpdi_h), alpha = .1) +
  geom_line(data = mu,
            aes(x = A_z, y = mu))

library(gridExtra)
grid.arrange(p1)

b5.7 <- "
data {
int<lower=1> N;
vector[N] kcal_per_g;
vector[N] neocortex_perc;
vector[N] log_mass;
}
parameters {
real alpha;
real bn;
real bm;
real sigma;
}
model {
vector[N] mu = alpha + bn * neocortex_perc + bm * log_mass;

kcal_per_g ~ normal(mu, sigma);
alpha ~ normal(0,100);
bn ~ normal(0,1);
bm ~ normal(0,1);
sigma ~ uniform(0, 1);

}
"
dat <- list(N = NROW(dcc),
            log_mass = dcc$log.mass,
            kcal_per_g = dcc$kcal.per.g,
            neocortex_perc = dcc$neocortex.perc)

fit.b5.7 <- stan(model_code = b5.7, data = dat, chains = 2, iter = 1e5)
plot(fit.b5.7, plotfun = "trace", inc_warmup = TRUE)
print(fit.b5.7, digits = 3)


#plot
fit.b5.7 <- as.data.frame(fit.b5.7)
f_mu <- function(x) fit.b5.7$alpha + fit.b5.7$bn * x
A_z_new <- seq(50, 80)

mu <- 
  sapply(A_z_new, f_mu) %>%
  as_tibble() %>%
  rename_all(function(x) A_z_new) %>%
  mutate(Iter = row_number()) %>%
  gather(A_z, kcal.per.g, -Iter) %>%
  group_by(A_z) %>%
  mutate(hpdi_l = HDInterval::hdi(kcal.per.g, credMass = 0.8)[1],
         hpdi_h = HDInterval::hdi(kcal.per.g, credMass = 0.8)[2]) %>%
  mutate(mu = mean(kcal.per.g)) %>%
  ungroup() %>%
  mutate(A_z = as.numeric(A_z))

# plot raw data and model estimate of mu
p <- ggplot() 
p1 <- p + 
  geom_point(data = dcc,
             aes(neocortex.perc, kcal.per.g), 
             shape = 1, color = 'dodgerblue') +
  geom_ribbon(data = mu,
              aes(x = A_z, ymin = hpdi_l, ymax = hpdi_h), alpha = .1) +
  geom_line(data = mu,
            aes(x = A_z, y = mu))

library(gridExtra)
grid.arrange(p1)

####5.2.0.1 Overthinking: Simulating a masking relationship.


N   <- 100     # number of cases
rho <- .7      # correlation between x_pos and x_neg

set.seed(141)  # setting the seed makes the results reproducible
d <- 
  tibble(x_pos = rnorm(N),                              # x_pos as a standard Gaussian
         x_neg = rnorm(N, rho*x_pos, sqrt(1 - rho^2)),  # x_neg correlated with x_pos
         y     = rnorm(N, x_pos - x_neg))               # y equally associated with x_pos and x_neg


#plot
pairs(d, col = "firebrick4")

b5.O.both <- "
data {
int<lower=1> N;
vector[N] y;
vector[N] x_pos;
vector[N] x_neg;
}
parameters {
real alpha;
real beta_x_pos;
real beta_x_neg;
real sigma;
}
model {
vector[N] mu = alpha + beta_x_pos * x_pos + beta_x_neg*x_neg;

y ~ normal(mu, sigma);
alpha ~ normal(0,100);
beta_x_pos ~ normal(0,1);
beta_x_neg ~ normal(0,1);
sigma ~ cauchy(0, 1);
}
"
dat <- list(N = NROW(d),
            y = d$y,
            x_pos = d$x_pos,
            x_neg = d$x_neg)

fit.b5.O.both <- stan(model_code = b5.O.both, data = dat, cores = 2, chains = 2, iter = 1000)
plot(fit.b5.O.both, plotfun = "trace", inc_warmup = TRUE)


b5.O.pos <- "
data {
int<lower=1> N;
vector[N] y;
vector[N] x_pos;
}
parameters {
real alpha;
real beta_x_pos;
real sigma;
}
model {
vector[N] mu = alpha + beta_x_pos * x_pos;

y ~ normal(mu, sigma);
alpha ~ normal(0,100);
beta_x_pos ~ normal(0,1);
sigma ~ cauchy(0, 1);
}
"

dat <- list(N = NROW(d),
            y = d$y,
            x_pos = d$x_pos
)

fit.b5.O.pos <- stan(model_code = b5.O.pos, data = dat, cores = 2, chains = 2, iter = 1000)

b5.O.neg <- "
data {
int<lower=1> N;
vector[N] y;
vector[N] x_neg;
}
parameters {
real alpha;
real beta_x_neg;
real sigma;
}
model {
vector[N] mu = alpha + beta_x_neg * x_neg;

y ~ normal(mu, sigma);
alpha ~ normal(0,100);
beta_x_neg ~ normal(0,1);
sigma ~ cauchy(0, 1);
}
"
dat <- list(N = NROW(d),
            y = d$y,
            x_neg = d$x_neg
)

fit.b5.O.neg <- stan(model_code = b5.O.neg, data = dat, cores = 2, chains = 2, iter = 1000)


print(fit.b5.O.both, digits = 2,par=c("alpha","beta_x_pos","beta_x_neg")) 
print(fit.b5.O.pos, digits = 2,par=c("alpha","beta_x_pos")) 
print(fit.b5.O.neg, digits = 2,par=c("alpha","beta_x_neg")) 


##5.3 When adding variables hurts
###5.3.1 Multicollinear legs

N <- 100
set.seed(531)

d <- 
  tibble(height    = rnorm(N, mean = 10, sd = 2),
         leg_prop  = runif(N, min = 0.4, max = 0.5)) %>% 
  mutate(leg_left  = leg_prop*height + rnorm(N, mean = 0, sd = 0.02),
         leg_right = leg_prop*height + rnorm(N, mean = 0, sd = 0.02))

d %>%
  select(leg_left:leg_right) %>%
  cor() %>%
  round(digits = 4)

# plot
d %>%
  ggplot(aes(x = leg_left, y = leg_right)) +
  geom_point(alpha = 1/2, color = "firebrick4") +
  theme_bw() +
  theme(panel.grid = element_blank())

b5.8 <- "
data {
int<lower=1> N;
vector[N] leg_left;
vector[N] leg_right;
vector[N] height;
}
parameters {
real alpha;
real beta_leg_left;
real beta_leg_right;
real sigma;
}
model {
vector[N] mu = alpha + beta_leg_left * leg_left + beta_leg_right*leg_right;

height ~ normal(mu, sigma);
alpha ~ normal(10,100);
beta_leg_left ~ normal(2,10);
beta_leg_right ~ normal(2,10);
sigma ~ uniform(0, 10);

}
"
dat <- list(N = NROW(d),
            leg_left = d$leg_left,
            leg_right = d$leg_right,
            height = d$height)
fit.b5.8 <- stan(model_code = b5.8, data = dat, cores = 2, chains = 2, iter = 1000)

print(fit.b5.8)


# plot
plot(fit.b5.8, show_density = TRUE, ci_level = 0.5, fill_color = "red")

# plot
fit.b5.8.f <- as.data.frame(fit.b5.8)
library(BioStatR)
pairs(fit.b5.8.f[2:3], cex.labels = 2, font.labels = 2, diag.panel = panel.hist)


b5.9 <- "
data {
int<lower=1> N;
vector[N] leg_left;
vector[N] height;
}
parameters {
real alpha;
real beta_leg_left;
real sigma;
}
model {
vector[N] mu = alpha + beta_leg_left * leg_left;

height ~ normal(mu, sigma);
alpha ~ normal(10,100);
beta_leg_left ~ normal(2,10);
sigma ~ uniform(0, 10);

}
"
dat <- list(N = NROW(d),
            leg_left = d$leg_left,
            height = d$height)
fit.b5.9 <- stan(model_code = b5.9, data = dat, cores = 2, chains = 2, iter = 1000)



print(fit.b5.9)

#plot
plot(fit.b5.9, plotfun = "hist", pars = "beta_leg_left", include = FALSE,main = "Just one coefficient needed")

###5.3.2 Multicollinear milk.

library(rethinking)
data(milk)
d <- milk
rm(milk)
detach(package:rethinking, unload = TRUE)

# kcal.per.g regressed on perc.fat
b5.10 <- "
data {
int<lower=1> N;
vector[N] kcal_per_g;
vector[N] perc_fat;
}
parameters {
real a;
real b_perc_fat;
real sigma;
}
model {
vector[N] mu = a + b_perc_fat * perc_fat;

kcal_per_g ~ normal(mu, sigma);
a ~ normal(0.6,10);
b_perc_fat ~ normal(0,1);
sigma ~ uniform(0,10);

}
"
dat <- list(N = NROW(d),
            perc_fat = d$perc.fat,
            kcal_per_g = d$kcal.per.g)

fit.b5.10 <- stan(model_code = b5.10, data = dat, cores = 2, chains = 2, iter = 1000)


# kcal.per.g regressed on perc.lactose
b5.11 <- "
data {
int<lower=1> N;
vector[N] kcal_per_g;
vector[N] perc_lactose;
}
parameters {
real a;
real b_perc_lactose;
real sigma;
}
model {
vector[N] mu = a + b_perc_lactose * perc_lactose;

kcal_per_g ~ normal(mu, sigma);
a ~ normal(0.6,10);
b_perc_lactose ~ normal(0,1);
sigma ~ uniform(0,10);

}
"
dat <- list(N = NROW(d),
            perc_lactose = d$perc.lactose,
            kcal_per_g = d$kcal.per.g)

fit.b5.11 <- stan(model_code = b5.11, data = dat, cores = 2, chains = 2, iter = 1000)

print(fit.b5.10, digits = 3,par=c("a","b_perc_fat")) 
print(fit.b5.11, digits = 3,par=c("a","b_perc_lactose"))


library( rstanarm )
posterior_interval(as.matrix(fit.b5.10), prob = 0.9, type = "central",
                   pars = NULL, regex_pars = NULL)



posterior_interval(as.matrix(fit.b5.11), prob = 0.9, type = "central",
                   pars = NULL, regex_pars = NULL)


b5.12 <- "
data {
int<lower=1> N;
vector[N] kcal_per_g;
vector[N] perc_fat;
vector[N] perc_lactose;
}
parameters {
real a;
real b_perc_fat;
real b_perc_lactose;
real sigma;
}
model {
vector[N] mu = a + b_perc_lactose * perc_lactose + b_perc_fat * perc_fat;

kcal_per_g ~ normal(mu, sigma);
a ~ normal(0.6,10);
b_perc_lactose ~ normal(0,1);
b_perc_fat ~ normal(0,1);
sigma ~ uniform(0,10);

}
"

dat <- list(N = NROW(d),
            perc_lactose = d$perc.lactose,
            kcal_per_g = d$kcal.per.g,
            perc_fat = d$perc.fat)

fit.b5.12 <- stan(model_code = b5.12, data = dat, cores = 2, chains = 2, iter = 1000)

print(fit.b5.12, digits = 3)


#plot
#install.packages("GGally", dependencies = T)
library(GGally)

ggpairs(data = d, columns = c(3:4, 6)) + 
  theme_bw()


#plot
my_diag <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) + 
    geom_density(fill = "firebrick4", size = 0)
}

my_lower <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) + 
    geom_smooth(method = "lm", color = "firebrick4", size = 1/3, 
                fill = "firebrick", alpha = 1/5) +
    geom_point(color = "firebrick", alpha = .8, size = 1/4)
}

# Then plug those custom functions into `ggpairs()`
ggpairs(data  = d, columns = c(3:4, 6),
        diag  = list(continuous = my_diag),
        lower = list(continuous = my_lower)) + 
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank())

####5.3.2.1 Overthinking: Simulating collinearity


sim_coll <- function(seed, rho){
  set.seed(seed)
  d <-
    d %>% 
    mutate(x = rnorm(n(), 
                     mean = perc.fat * rho,
                     sd   = sqrt((1 - rho^2) * var(perc.fat))))
  
  m <- lm(kcal.per.g ~ perc.fat + x, data = d)
  
  sqrt(diag(vcov(m)))[2]  # parameter SD
}

# how many simulations per `rho`-value would you like?
n_seed <- 100
# how many `rho`-values from 0 to .99 would you like to evaluate the process over?
n_rho  <- 30

d <-
  tibble(seed = 1:n_seed) %>% 
  expand(seed, rho = seq(from = 0, to = .99, length.out = n_rho)) %>% 
  mutate(parameter_sd = purrr::map2(seed, rho, sim_coll)) %>% 
  unnest() %>% 
  group_by(rho) %>% 
  # we'll `summarise()` our output by the mean and 95% intervals
  summarise(mean = mean(parameter_sd),
            ll   = quantile(parameter_sd, prob = .025),
            ul   = quantile(parameter_sd, prob = .975))

#plot
d %>% 
  ggplot(aes(x = rho, y = mean)) +
  geom_line(color = "firebrick4") +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              fill = "firebrick", alpha = 1/4) +
  labs(x = expression(rho),
       y = "parameter SD") +
  coord_cartesian(ylim = c(0, .0072)) +
  theme_bw() +
  theme(panel.grid = element_blank())

### 5.3.3 Post-treatment bias.

N <- 100

set.seed(17)
d <- 
  tibble(h0        = rnorm(N, mean = 10, sd = 2),  # initial height
         treatment = rep(0:1, each = N / 2),
         fungus    = rbinom(N, size = 1, prob = .5 - treatment * 0.4),
         h1        = h0 + rnorm(N, mean = 5 - 3 * fungus, sd = 1))


d %>%
  head()

b5.13 <- "
data {
int<lower=1> N;
vector[N] h1;
vector[N] h0;
vector[N] treatment;
vector[N] fungus;
}
parameters {
real a;
real b_h0;
real b_treatment;
real b_fungus;
real sigma;
}
model {
vector[N] mu = a + b_h0 * h0 + b_treatment * treatment + b_fungus * fungus;

h1 ~ normal(mu, sigma);
a ~ normal(0,100);
b_h0 ~ normal(0,10);
b_treatment ~ normal(0,10);
b_fungus ~ normal(0,10);
sigma ~ uniform(0, 10);

}
"
dat <- list(N = NROW(d),
            h1 = d$h1,
            h0 = d$h0,
            treatment = d$treatment,
            fungus = d$fungus)

fit.b5.13 <- stan(model_code = b5.13, data = dat, cores = 2, chains = 2, iter = 1000)

print(fit.b5.13)

b5.14 <- "
data {
int<lower=1> N;
vector[N] h1;
vector[N] h0;
vector[N] treatment;
}
parameters {
real a;
real b_h0;
real b_treatment;
real sigma;
}
model {
vector[N] mu = a + b_h0 * h0 + b_treatment*treatment;

h1 ~ normal(mu, sigma);
a ~ normal(0,100);
b_h0 ~ normal(0,10);
b_treatment ~ normal(0,10);
sigma ~ uniform(0,10);

}
"
dat <- list(N = NROW(d),
            h1 = d$h1,
            h0 = d$h0,
            treatment = d$treatment)

fit.b5.14 <- stan(model_code = b5.14, data = dat, cores = 2, chains = 2, iter = 1000)

print(fit.b5.14)

##5.4 Categorical varaibles
###5.4.1 Binary categories.

library(rethinking)
data(Howell1)
d <- Howell1

rm(Howell1)
detach(package:rethinking, unload = T)
library(brms)

d %>%
  glimpse()

b5.15 <- "
data {
int<lower=1> N;
vector[N] height;
vector[N] male;
}
parameters {
real a;
real b_male;
real sigma;
}
model {
vector[N] mu = a + b_male * male ;

height ~ normal(mu, sigma);
a ~ normal(178, 100);
b_male ~ normal(0,10);
sigma ~ cauchy(0, 2);

}
"
dat <- list(N = NROW(d),
            height = d$height,
            male = d$male
            
)

fit.b5.15 <- stan(model_code = b5.15, data = dat, cores = 2, chains = 2, iter = 1000)



print(fit.b5.15)

#plot
plot(fit.b5.15, show_density = TRUE, ci_level = 0.9, fill_color = "red")


####5.4.1.1 Overthinking: Re-parameterizing the model.

d <-
  d %>%
  mutate(female = 1 - male)

b5.15b <- "
data {
int<lower=1> N;
vector[N] height;
vector[N] male;
vector[N] female;
}
parameters {
real b_male;
real b_female;
real sigma;
}
model {
vector[N] mu = b_male * male + b_female*female;

height ~ normal(mu, sigma);
b_male ~ normal(178, 100);
b_female ~ normal(178, 100);
sigma ~ cauchy(0, 2);

}
"
dat <- list(N = NROW(d),
            female = d$female,
            male = d$male,
            height = d$height
            
)

fit.b5.15b <- stan(model_code = b5.15b, data = dat, cores = 2, chains = 2, iter = 1000)



print(fit.b5.15b)


# plot
plot(fit.b5.15b, show_density = TRUE, ci_level = 0.9, fill_color = "red")


###5.4.2 Many categories.
library(rethinking)
data(milk)
d <- milk


rm(milk)
detach(package:rethinking, unload = T)
library(brms)

d %>%
  distinct(clade)

d <- 
  d %>%
  mutate(clade_nwm = ifelse(clade == "New World Monkey", 1, 0),
         clade_owm = ifelse(clade == "Old World Monkey", 1, 0),
         clade_s   = ifelse(clade == "Strepsirrhine", 1, 0),
         clade_ape = ifelse(clade == "Ape", 1, 0))


b5.16 <- "
data {
int<lower=1> N;
vector[N] kcal_per_g;
vector[N] clade_nwm;
vector[N] clade_owm;
vector[N] clade_s;
}
parameters {
real a;
real b_clade_nwm;
real b_clade_owm;
real b_clade_s;
real sigma;
}
model {
vector[N] mu = a + b_clade_nwm * clade_nwm + b_clade_owm*clade_owm + b_clade_s*clade_s;

kcal_per_g ~ normal(mu, sigma);
a ~ normal(0.6, 10);
b_clade_nwm ~ normal(0, 1);
b_clade_owm ~ normal(0, 1);
b_clade_s ~ normal(0, 1);
sigma ~ uniform(0, 10);

}
"
dat <- list(N = NROW(d),
            kcal_per_g = d$kcal.per.g,
            clade_nwm = d$clade_nwm,
            clade_owm = d$clade_owm,
            clade_s = d$clade_s )

fit.b5.16 <- stan(model_code = b5.16, data = dat, cores = 2, chains = 2, iter = 1000)
print(fit.b5.16)
plot(fit.b5.16, show_density = TRUE, ci_level = 0.9, fill_color = "red")


###5.4.3 Adding regular predictor variables.

#

###5.4.4 Another approach: Unique intercepts.
b5.16_alt <- "
data {
int<lower=1> N;
vector[N] kcal_per_g;
vector[N] clade;
}
parameters {
real a;
real b;
real sigma;
}
model {
vector[N] mu = a + b * clade;

kcal_per_g ~ normal(mu, sigma);
b ~ normal(.6, 10);
sigma ~ cauchy(0, 10);

}
"

library(rethinking)


dat <- list(N = NROW(d),
            clade = d$clade_id,
            kcal_per_g = d$kcal.per.g)

b5.16_alt <- stan(model_code = b5.16_alt, data = dat, cores = 2, chains = 2, iter = 1000)



print(b5.16_alt)
