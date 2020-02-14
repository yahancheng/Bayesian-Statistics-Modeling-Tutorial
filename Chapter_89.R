#8 Markov Chain Monte Carlo
##8.1 Good King Markov and His island kingdom
set.seed(103)

num_weeks <- 1e5
positions <- rep(0, num_weeks)
current   <- 10
for (i in 1:num_weeks) {
  # record current position
  positions[i] <- current
  # flip coin to generate proposal
  proposal <- current + sample(c(-1, 1), size = 1)
  # now make sure he loops around the archipelago
  if (proposal < 1) proposal <- 10
  if (proposal > 10) proposal <- 1
  # move?
  prob_move <- proposal / current
  current   <- ifelse(runif(1) < prob_move, proposal, current)
}


#Figure 8.2.a
library(tidyverse)

tibble(week   = 1:1e5,
       island = positions) %>%
  
  ggplot(aes(x = week, y = island)) +
  geom_point(shape = 1) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 20)) +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2)) +
  coord_cartesian(xlim = 0:100,
                  ylim = 1:10) +
  labs(title = "Behold: The Metropolis algorithm in action!",
       subtitle = "The dots show the king's path over the first 100 weeks.") 


#Figure 8.2.b.
tibble(week   = 1:1e5,
       island = positions) %>%
  mutate(island = factor(island)) %>%
  
  ggplot(aes(x = island)) +
  geom_bar() +
  labs(title = "Old Metropolis shines in the long run.",
       subtitle = "Sure enough, the time the king spent on each island was\nproportional to its population size.") 


##8.2 Markov chain Monte Carlo
##8.3 Easy HMC: map2stan brm()
library(rethinking)
data(rugged)
d <- rugged

detach(package:rethinking)
library(brms)
rm(rugged)

data('rugged', package = 'rethinking')
d  <- rugged; rm(rugged)
d$loggdp <- log(d$rgdppc_2000)
d <- d[complete.cases(d$loggdp),]
d.A1 <- d[d$cont_africa==1,]
d.A0 <- d[d$cont_africa==0,]

###8.3.1 Preparation.

d.trim <-
  d %>%
  select(loggdp, rugged, cont_africa)

str(d.trim)


###8.3.2 Estimation.

model.b8.1 <- "
data {
int N;
vector[N] loggdp;
vector[N] rugged;
vector<lower=0,upper=1>[N] cont_africa;
}
parameters {
real a;
real bR;
real bA;
real bAR;
real<lower=0, upper=2> sigma;
}
model {
vector[N] mu;

for (i in 1:N) {
mu[i] = a + bR * rugged[i] + bAR * rugged[i] * cont_africa[i] + bA * cont_africa[i];
}

loggdp ~ normal( mu, sigma);
a ~ normal(8, 100);
bR ~ normal(0, 1);
bA ~ normal(0, 1);
bAR ~ normal(0, 1);
}

generated quantities {
vector[N] log_lik;
{
  vector[N] mu; vector[N] gamma;
  for(n in 1:N) {
  gamma[n] = bR + bAR * cont_africa[n];
  mu[n] = a + gamma[n] * rugged[n] + bA * cont_africa[n];
  log_lik[n] = normal_lpdf(loggdp[n] | mu[n], sigma);
  }
}
}"


dat <- list(
  N = NROW(d),
  loggdp = d$loggdp,
  rugged = d$rugged,
  cont_africa = d$cont_africa
)


b8.1 <- stan(model_code = model.b8.1, data = dat, cores = 2, chains = 2, iter = 1000)

library(tidybayes);library(brms)

post <- posterior_samples(b8.1)

post %>% 
  gather() %>% 
  group_by(key) %>% 
  mean_hdi(value, .width = .89)  # note our rare use of 89% intervals


###8.3.3 Sampling again, in parallel.
b8.1_4chains_4cores <- stan(model_code = model.b8.1, data = dat, cores = 4, chains = 2, iter = 1000)


###8.3.4 Visualization.
post <- posterior_samples(b8.1)
str(post)

library(GGally)
#plot.1
post %>%
  select(a:sigma) %>%
  ggpairs()

#plot.2
post %>%
  select(a:sigma) %>%
  
  ggpairs() +
  labs(subtitle = "My custom pairs plot")

###8.3.5 Checking the chain.

stan_dens(b8.1, pars = c("a", "bR", "bA", "bAR", "sigma"), include = TRUE, unconstrain = FALSE,
          inc_warmup = FALSE, nrow = NULL, ncol = NULL,
          separate_chains = FALSE)
plot(b8.1, plotfun = "trace", pars = c("a", "bR", "bA", "bAR", "sigma"), inc_warmup = TRUE)

library(bayesplot)

post <- posterior_samples(b8.1, add_chain = T)

mcmc_trace(post[, c(1:5)],
           facet_args = list(ncol = 3), 
           size = .15) +
  labs(title = "My custom trace plots") +
  
  theme(legend.position = c(.95, .2))

mcmc_acf(post, 
         pars = c("a", "bR", "bA", "bAR", "sigma"),
         lags = 5) 

##8.4 Care and feeding of your Markov chain.
###8.4.3 Taming a wild chain.

b8.2 <- "
data {
int N; 
vector[N] x;
} 

parameters {
real mu;
real sigma;
} 

model {
// The likelihood
x ~ normal(mu,sigma);

// Define the priors
mu ~ uniform(-1e10, 1e10);
sigma ~ uniform(0, 1e10);
}"

f <- function() list(N = 1, x = array(-1,1))

fit.b8.2 <- stan(model_code = b8.2, data = f())

plot(fit.b8.2, plotfun = "trace", pars = c("mu","sigma"), inc_warmup = TRUE)

summary(fit.b8.2)



b8.3 <- "
data {
int N; 
vector[N] x;
} 

parameters {
real b_Intercept;
real sigma;
} 

model {
// The likelihood
x ~ normal(b_Intercept,sigma);

// Define the priors
b_Intercept ~ normal(0, 10);
sigma ~ cauchy(0, 1);
}"

f <- function() list(N = 1, x = array(-1,1))

fit.b8.3 <- stan(model_code = b8.3, data = f())

print(fit.b8.3)

plot(fit.b8.3, plotfun = "trace", pars = c("b_Intercept","sigma"), inc_warmup = TRUE)


#Now behold our version of Figure 8.6.a.
post <- posterior_samples(fit.b8.3, add_chain = T)
post %>%
  select(b_Intercept) %>%
  
  ggplot(aes(x = b_Intercept)) +
  stat_density(geom = "line") +
  geom_line(data = data.frame(x = seq(from = min(post$b_Intercept),
                                      to = max(post$b_Intercept),
                                      length.out = 50)),
            aes(x = x, y = dnorm(x = x, mean = 0, sd = 10)),
            linetype = 2) 


#Here’s our version of Figure 8.6.b.
post %>%
  select(sigma) %>%
  
  ggplot(aes(x = sigma)) +
  stat_density(geom = "line") +
  geom_line(data = data.frame(x = seq(from = 0,
                                      to = max(post$sigma),
                                      length.out = 50)),
            aes(x = x, y = dcauchy(x = x, location = 0, scale = 1)*2),
            linetype = 2) +
  coord_cartesian(xlim = c(0, 10)) 


####8.4.3.1 Overthinking: Cauchy distribution.

N <- 1e4

set.seed(1e4)
tibble(y     = rcauchy(N, 0, 5),
       mu    = cummean(y),
       index = 1:N) %>% 
  
  ggplot(aes(x = index, y = mu)) +
  geom_line() 

library(brms);library(ggplot2);library(tidyr);library(scales)
N <- 1e4

set.seed(1)
tibble(a = rcauchy(N, 0, 5),
       b = rcauchy(N, 0, 5),
       c = rcauchy(N, 0, 5),
       d = rcauchy(N, 0, 5),
       e = rcauchy(N, 0, 5),
       f = rcauchy(N, 0, 5),
       g = rcauchy(N, 0, 5),
       h = rcauchy(N, 0, 5)) %>% 
  gather() %>% 
  group_by(key) %>% 
  mutate(mu = cummean(value)) %>% 
  ungroup() %>% 
  mutate(index = rep(1:N, times = 8)) %>% 
  
  ggplot(aes(x = index, y = mu)) +
  geom_line(aes(color = key)) +
  scale_x_continuous(breaks = c(0, 5000, 10000)) +
  theme(legend.position = "none") +
  facet_wrap(~key, ncol = 4, scales = "free")


###8.4.4 Non-identifiable parameters.

b8.4 <- "
data {
int N; 
vector[N] x;
vector[N] intercept_1;
vector[N] intercept_2;
} 

parameters {
real beta_1;
real beta_2;
real sigma;
} 

model {
vector[N] mu = beta_1 * intercept_1 + beta_2 * intercept_2;

// The likelihood
x ~ normal(mu,sigma);

// Define the priors
beta_1 ~ uniform(-1e10, 1e10);
beta_2 ~ uniform(-1e10, 1e10);
sigma ~ cauchy(0, 1);
}"
f <- function() list(N = 100, x = rnorm(100, mean = 0, sd = 1), intercept_1 = rep(1,100), intercept_2 = rep(1,100))

fit.b8.4 <- stan(model_code = b8.4, data = f(),chains=2)

print(fit.b8.4)



b8.5 <- "
data {
int N; 
vector[N] x;
vector[N] intercept_1;
vector[N] intercept_2;
} 

parameters {
real beta_1;
real beta_2;
real sigma;
} 

model {
vector[N] mu = beta_1 * intercept_1 + beta_2 * intercept_2;

// The likelihood
x ~ normal(mu,sigma);

// Define the priors
beta_1 ~ uniform(0,10);
beta_2 ~ uniform(0,10);
sigma ~ cauchy(0, 1);
}"
f <- function() list(N = 100, x = rnorm(100, mean = 0, sd = 1), intercept_1 = rep(1,100), intercept_2 = rep(1,100))
fit.b8.5 <- stan(model_code = b8.5, data = f(),chains=2)

print(fit.b8.5)

plot(fit.b8.4, plotfun = "trace", pars = c("beta_1","beta_2","sigma"), inc_warmup = TRUE)

plot(fit.b8.5, plotfun = "trace", pars = c("beta_1","beta_2","sigma"), inc_warmup = TRUE) 

#9 Big Entropy and the Generalized Linear Model

##9.1 Maximum entropy
library(tidyverse)

d <-
  tibble(a = c(0, 0, 10, 0, 0),
         b = c(0, 1, 8, 1, 0),
         c = c(0, 2, 6, 2, 0),
         d = c(1, 2, 4, 2, 1),
         e = 2) 

# this is our analogue to McElreath's `lapply()` code
d %>% 
  mutate_all(funs(. / sum(.))) %>% 
  # the next few lines constitute our analogue to his `sapply()` code
  gather() %>% 
  group_by(key) %>% 
  summarise(h = -sum(ifelse(value == 0, 0, value * log(value))))

library(ghibli)
ghibli_palette("MarnieMedium1")
ghibli_palette("MarnieMedium1")[1:7]

d %>% 
  mutate(bucket = 1:5) %>% 
  gather(letter, pebbles, - bucket) %>% 
  
  ggplot(aes(x = bucket, y = pebbles)) +
  geom_col(width = 1/5, fill = ghibli_palette("MarnieMedium1")[2]) +
  geom_text(aes(y = pebbles + 1, label = pebbles)) +
  geom_text(data = tibble(
    letter  = letters[1:5],
    bucket  = 5.5,
    pebbles = 10,
    label   = str_c(c(1, 90, 1260, 37800, 113400), 
                    rep(c(" way", " ways"), times = c(1, 4)))),
    aes(label = label), hjust = 1) +
  scale_y_continuous(breaks = c(0, 5, 10)) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("MarnieMedium1")[6]),
        strip.background = element_rect(fill = ghibli_palette("MarnieMedium1")[1])) +
  facet_wrap(~letter, ncol = 2)

d %>% 
  # the next four lines are the same from above
  mutate_all(funs(. / sum(.))) %>% 
  gather() %>% # key = colnames, value = observations
  group_by(key) %>% 
  summarise(h = -sum(ifelse(value == 0, 0, value * log(value)))) %>% 
  # here's the R code 9.4 stuff
  mutate(n_ways   = c(1, 90, 1260, 37800, 113400)) %>% 
  group_by(key) %>% 
  mutate(log_ways = log(n_ways) / 10,
         text_y   = ifelse(key < "c", h + .15, h - .15)) %>% # text position
  
  # plot
  ggplot(aes(x = log_ways, y = h)) +
  geom_abline(intercept = 0, slope = 1.37, 
              color = "white") +
  geom_point(size = 2.5, color = ghibli_palette("MarnieMedium1")[7]) +
  geom_text(aes(y = text_y, label = key)) +
  labs(x = "log(ways) per pebble",
       y = "entropy") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("MarnieMedium1")[6]))

###9.1.1 Gaussian.
alpha_per_beta <- function(variance, beta){
  sqrt((variance * gamma(1 / beta)) / gamma(3 / beta))
}

tibble(mu       = 0,
       variance = 1,
       # I arrived at these values by trial and error
       beta     = c(1, 1.5, 2, 4)) %>% 
  mutate(alpha  = map2(variance, beta, alpha_per_beta)) %>% 
  unnest() %>% 
  expand(nesting(mu, beta, alpha), 
         value = seq(from = -5, to = 5, by = .1)) %>% 
  # behold the formula for the generalized normal distribution in code
  mutate(density = (beta / (2 * alpha * gamma(1 / beta))) * exp(1) ^ (-1 * (abs(value - mu) / alpha) ^ beta)) %>% 
  
  ggplot(aes(x = value, y = density,
             group = beta)) +
  geom_line(aes(color = beta == 2,
                size  = beta == 2)) +
  scale_color_manual(values = c(ghibli_palette("MarnieMedium2")[2],
                                ghibli_palette("MarnieMedium2")[4])) +
  scale_size_manual(values = c(1/4, 1.25)) +
  ggtitle(NULL, subtitle = "Guess which color denotes the Gaussian.") +
  coord_cartesian(xlim = -4:4) +
  theme(panel.grid       = element_blank(),
        legend.position  = "none",
        panel.background = element_rect(fill = ghibli_palette("MarnieMedium2")[7]))


#Here’s Figure 9.2’s right panel.
tibble(mu       = 0,
       variance = 1,
       # this time we need a more densely-packed sequence of `beta` values
       beta     = seq(from = 1, to = 4, length.out = 100)) %>% 
  mutate(alpha  = map2(variance, beta, alpha_per_beta)) %>%
  unnest() %>%
  expand(nesting(mu, beta, alpha), 
         value = -8:8) %>% 
  mutate(density = (beta / (2 * alpha * gamma(1 / beta))) * exp(1) ^ (-1 * (abs(value - mu) / alpha) ^ beta)) %>% 
  group_by(beta) %>% 
  # this is just an abbreviated version of the formula we used in our first code block
  summarise(entropy = -sum(density * log(density))) %>% 
  
  ggplot(aes(x = beta, y = entropy)) +
  geom_vline(xintercept = 2, color = "white") +
  geom_line(size = 2, color = ghibli_palette("MarnieMedium2")[6]) +
  coord_cartesian(ylim = c(1.34, 1.42)) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("MarnieMedium2")[7]))

###9.1.2 Binomial.
count_ways <- function(n, y){
  # n = the total number of trials (i.e., the number of rows in your vector)
  # y = the total number of 1s (i.e., successes) in your vector
  (factorial(n) / (factorial(y) * factorial(n - y)))
} 


tibble(sequence = 1:3,
       n        = 4,
       y        = c(0, 1, 2)) %>% 
  mutate(n_ways = map2(n, y, count_ways)) %>% 
  unnest()

(
  d <-
    tibble(distribution = letters[1:4],
           ww = c(1/4, 2/6, 1/6, 1/8),
           bw = c(1/4, 1/6, 2/6, 4/8),
           wb = c(1/4, 1/6, 2/6, 2/8),
           bb = c(1/4, 2/6, 1/6, 1/8))
)

d %>% 
  gather(key, value, -distribution) %>% 
  mutate(key = factor(key, levels = c("ww", "bw", "wb", "bb"))) %>% 
  
  ggplot(aes(x = key, y = value, group = 1)) +
  geom_point(size = 2, color = ghibli_palette("PonyoMedium")[4]) +
  geom_line(color = ghibli_palette("PonyoMedium")[5]) +
  coord_cartesian(ylim = 0:1) +
  labs(x = NULL,
       y = NULL) +
  theme(panel.grid   = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("PonyoMedium")[2]),
        strip.background = element_rect(fill = ghibli_palette("PonyoMedium")[6])) +
  facet_wrap(~distribution)

d %>% 
  gather(sequence, probability, -distribution) %>% 
  # `str_count()` will count the number of times "b" occurs within a given row of `sequence`
  mutate(n_b     = str_count(sequence, "b")) %>% 
  mutate(product = probability * n_b) %>% 
  group_by(distribution) %>% 
  summarise(expected_value = sum(product))

# calculate entropies
d %>% 
  gather(sequence, probability, -distribution) %>% 
  group_by(distribution) %>% 
  summarise(entropy = -sum(probability * log(probability)))

p <- 0.7

(
  a <- 
    c((1 - p)^2, 
      p * (1 - p), 
      (1 - p) * p, 
      p^2)
)

# entropy
-sum(a * log(a))

sim_p <- function(seed, g = 1.4) {
  
  set.seed(seed)
  
  x_123 <- runif(3)
  x_4   <- ((g) * sum(x_123) - x_123[2] - x_123[3]) / (2 - g)
  z     <- sum(c(x_123, x_4))
  p     <- c(x_123, x_4) / z
  tibble(h   = -sum(p * log(p)), 
         p   = p,
         key = factor(c("ww", "bw", "wb", "bb"), levels = c("ww", "bw", "wb", "bb")))
}
sim_p(seed = 9.9, g = 1.4)

# how many replications would you like?
n_rep <- 1e5

d <-
  tibble(seed = 1:1e5) %>% 
  mutate(sim = map2(seed, 1.4, sim_p)) %>% 
  unnest()

head(d)

ranked_d <-
  d %>% 
  group_by(seed) %>% 
  arrange(desc(h)) %>% 
  ungroup() %>%
  # here's the rank order step
  mutate(rank = rep(1:n_rep, each = 4))

head(ranked_d)

subset_d <-
  ranked_d %>%
  # I arrived at these `rank` values by trial and error
  filter(rank %in% c(1, 87373, n_rep - 1500, n_rep - 10)) %>% 
  # I arrived at the `height` values by trial and error, too
  mutate(height       = rep(c(8, 2.25, .75, .5), each = 4),
         distribution = rep(letters[1:4], each = 4))

head(subset_d)

d %>% 
  ggplot(aes(x = h)) +
  geom_density(size = 0, fill = ghibli_palette("LaputaMedium")[3],
               adjust = 1/4) +
  # note the data statements for the next two geoms
  geom_linerange(data = subset_d %>% group_by(seed) %>% slice(1),
                 aes(ymin = 0, ymax = height),
                 color = ghibli_palette("LaputaMedium")[5]) +
  geom_text(data = subset_d %>% group_by(seed) %>% slice(1),
            aes(y = height + .5, label = distribution)) +
  scale_x_continuous("Entropy",
                     breaks = seq(from = .7, to = 1.2, by = .1)) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("LaputaMedium")[7]))

ranked_d %>%
  filter(rank %in% c(1, 87373, n_rep - 1500, n_rep - 10)) %>% 
  mutate(distribution = rep(letters[1:4], each = 4)) %>% 
  
  ggplot(aes(x = key, y = p, group = 1)) +
  geom_line(color = ghibli_palette("LaputaMedium")[5]) +
  geom_point(size = 2, color = ghibli_palette("LaputaMedium")[4]) +
  coord_cartesian(ylim = 0:1) +
  labs(x = NULL,
       y = NULL) +
  theme(panel.grid   = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("LaputaMedium")[7]),
        strip.background = element_rect(fill = ghibli_palette("LaputaMedium")[6])) +
  facet_wrap(~distribution)

ranked_d %>% 
  group_by(key) %>% 
  slice(max(h))
a


##9.2 Generalized linear models
#I winged the values for our Figure 9.5.

tibble(x = seq(from = -1, to = 3, by = .01)) %>%
  mutate(probability = .35 + x * .5) %>% 
  
  ggplot(aes(x = x, y = probability)) +
  geom_rect(aes(xmin = -1, xmax = 3,
                ymin = 0,  ymax = 1),
            fill = ghibli_palette("MononokeMedium")[5]) +
  geom_hline(yintercept = 0:1, linetype = 2, color = ghibli_palette("MononokeMedium")[7]) +
  geom_line(aes(linetype = probability > 1, color = probability > 1),
            size = 1) +
  geom_segment(x = 1.3, xend = 3,
               y = 1, yend = 1,
               size = 2/3, color = ghibli_palette("MononokeMedium")[3]) +
  scale_color_manual(values = c(ghibli_palette("MononokeMedium")[3],
                                ghibli_palette("MononokeMedium")[7])) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  coord_cartesian(xlim = 0:2,
                  ylim = c(0, 1.2)) +
  theme(panel.grid       = element_blank(),
        legend.position  = "none",
        panel.background = element_rect(fill = ghibli_palette("MononokeMedium")[1]))

###9.2.1 Meet the family.

#Here are the Gamma and Exponential panels for Figure 9.6.


length_out <- 100

tibble(x = seq(from = 0, to = 5, length.out = length_out)) %>% 
  mutate(Gamma       = dgamma(x, 2, 2),
         Exponential = dexp(x)) %>% 
  gather(key, density, -x) %>% 
  mutate(label = rep(c("y %~% Gamma(lambda, kappa)", "y %~% Exponential(lambda)"), each = n()/2)) %>% 
  
  ggplot(aes(x = x, ymin = 0, ymax = density)) +
  geom_ribbon(fill = ghibli_palette("SpiritedMedium")[3]) +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:4) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("SpiritedMedium")[5]),
        strip.background = element_rect(fill = ghibli_palette("SpiritedMedium")[7])) +
  facet_wrap(~label, scales = "free_y", labeller = label_parsed)


#The Gaussian:


length_out <- 100

tibble(x = seq(from = -5, to = 5, length.out = length_out)) %>% 
  mutate(density = dnorm(x),
         strip   = "y %~% Normal(mu, sigma)") %>% 
  
  ggplot(aes(x = x, ymin = 0, ymax = density)) +
  geom_ribbon(fill = ghibli_palette("SpiritedMedium")[3]) +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = -4:4) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("SpiritedMedium")[5]),
        strip.background = element_rect(fill = ghibli_palette("SpiritedMedium")[7])) +
  facet_wrap(~strip, labeller = label_parsed)


#Here is the Poisson.


length_out <- 100

tibble(x = 0:20) %>% 
  mutate(density = dpois(x, lambda = 2.5),
         strip   = "y %~% Poisson(lambda)") %>% 
  
  ggplot(aes(x = x, y = density)) +
  geom_col(fill = ghibli_palette("SpiritedMedium")[2], width = 1/2) +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:10) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("SpiritedMedium")[5]),
        strip.background = element_rect(fill = ghibli_palette("SpiritedMedium")[7])) +
  facet_wrap(~strip, labeller = label_parsed)

length_out <- 100

tibble(x = 0:10) %>% 
  mutate(density = dbinom(x, size = 10, prob = .85),
         strip   = "y %~% Binomial(n, p)") %>% 
  
  ggplot(aes(x = x, y = density)) +
  geom_col(fill = ghibli_palette("SpiritedMedium")[2], width = 1/2) +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:10) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("SpiritedMedium")[5]),
        strip.background = element_rect(fill = ghibli_palette("SpiritedMedium")[7])) +
  facet_wrap(~strip, labeller = label_parsed)


####9.2.1.1 Rethinking: A likelihood is a prior.

###9.2.2 Linking linear models to distributions.

# first, we'll make data for the horizontal lines
alpha <- 0
beta  <- 4

lines <-
  tibble(x           = seq(from = -1, to = 1, by = .25)) %>% 
  mutate(`log-odds`  = alpha + x * beta,
         probability = exp(alpha + x * beta) / (1 + exp(alpha + x * beta)))

# now we're ready to make the primary data
beta  <- 2

d <-
  tibble(x           = seq(from = -1.5, to = 1.5, length.out = 50)) %>% 
  mutate(`log-odds`  = alpha + x * beta,
         probability = exp(alpha + x * beta) / (1 + exp(alpha + x * beta))) 

# now we make the individual plots
p1 <-
  d %>% 
  ggplot(aes(x = x, y = `log-odds`)) +
  geom_hline(data = lines,
             aes(yintercept = `log-odds`),
             color = ghibli_palette("YesterdayMedium")[6]) +
  geom_line(size = 1.5, color = ghibli_palette("YesterdayMedium")[3]) +
  coord_cartesian(xlim = -1:1) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("YesterdayMedium")[5]))

p2 <-
  d %>% 
  ggplot(aes(x = x, y = probability)) +
  geom_hline(data = lines,
             aes(yintercept = probability),
             color = ghibli_palette("YesterdayMedium")[6]) +
  geom_line(size = 1.5, color = ghibli_palette("YesterdayMedium")[3]) +
  coord_cartesian(xlim = -1:1) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("YesterdayMedium")[7]))

# finally, we're ready to mash the plots together and behold their nerdy glory
library(gridExtra)

grid.arrange(p1, p2, ncol = 2)

set.seed(100)
(
  d <-
    tibble(x = rep(0:1, each = 100)) %>% 
    mutate(y = rnorm(n = n(), mean = 100, sd = 10 + x * 10))
)

library(tidybayes)

d %>% 
  mutate(x = x %>% as.character()) %>% 
  
  ggplot(aes(x = y, y = x, fill = x)) +
  geom_halfeyeh(color = ghibli_palette("KikiMedium")[2],
                point_interval = mean_qi, .width = .68) +
  scale_fill_manual(values = c(ghibli_palette("KikiMedium")[4],
                               ghibli_palette("KikiMedium")[6])) +
  theme(panel.grid       = element_blank(),
        axis.ticks.y     = element_blank(),
        legend.position  = "none",
        panel.background = element_rect(fill = ghibli_palette("KikiMedium")[7]))

library(brms)
b9.1 <- "
data {
int N;
vector[N] y;
vector[N] x;
}
parameters {
real alpha;
real beta;
real mu;
}
model {
vector[N] sigma = alpha + beta * x ;

y ~ normal(mu, sigma);
mu ~ normal(100, 10);
alpha ~ normal(0, 10);
beta ~ normal(0, 10);
}
"
library(brms)
library(rstan)
dat <- list(N = NROW(d),
            x = d$x,
            y = d$y)
fit.b9.1 <- stan(model_code = b9.1, data = dat)

post <- posterior_samples(fit.b9.1)

head(post)

post %>% 
  transmute(`x == 0` = exp(alpha + beta * 0),
            `x == 1` = exp(alpha + beta * 1)) %>% 
  gather(key, sd) %>% 
  
  ggplot(aes(x = sd, y = key, fill = key)) +
  geom_halfeyeh(color = ghibli_palette("KikiMedium")[2],
                point_interval = median_qi, .width = .95) +
  scale_fill_manual(values = c(ghibli_palette("KikiMedium")[4],
                               ghibli_palette("KikiMedium")[6])) +
  labs(x = NULL, y = NULL,
       subtitle = expression(paste("Model-implied ", italic(SD), "s by group x"))) +
  theme(panel.grid       = element_blank(),
        axis.ticks.y     = element_blank(),
        legend.position  = "none",
        panel.background = element_rect(fill = ghibli_palette("KikiMedium")[7]))

d %>% 
  group_by(x) %>% 
  summarise(sd = sd(y) %>% round(digits = 1)) 

# first, we'll make data that'll be make the horizontal lines
alpha <- 0
beta  <- 2

lines <-
  tibble(`log-measurement`      = -3:3) %>% 
  mutate(`original measurement` = exp(`log-measurement`))

# now we're ready to make the primary data
d <-
  tibble(x                      = seq(from = -1.5, to = 1.5, length.out = 50)) %>% 
  mutate(`log-measurement`      = alpha + x * beta,
         `original measurement` = exp(alpha + x * beta)) 

# now we make the individual plots
p1 <-
  d %>% 
  ggplot(aes(x = x, y = `log-measurement`)) +
  geom_hline(data = lines,
             aes(yintercept = `log-measurement`),
             color = ghibli_palette("YesterdayMedium")[6]) +
  geom_line(size = 1.5, color = ghibli_palette("YesterdayMedium")[3]) +
  coord_cartesian(xlim = -1:1) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("YesterdayMedium")[5]))

p2 <-
  d %>% 
  ggplot(aes(x = x, y = `original measurement`)) +
  geom_hline(data = lines,
             aes(yintercept = `original measurement`),
             color = ghibli_palette("YesterdayMedium")[6]) +
  geom_line(size = 1.5, color = ghibli_palette("YesterdayMedium")[3]) +
  coord_cartesian(xlim = -1:1,
                  ylim = 0:10) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = ghibli_palette("YesterdayMedium")[7]))

# finally, we're ready to mash the plots together and behold their nerdy glory
grid.arrange(p1, p2, ncol = 2)

