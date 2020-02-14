#Tutorial 3 

library(dplyr)
library(tidyr)
library(rstan)
library(ggplot2)
library(ggthemes)
library(ggthemes)
library(tidyverse)


#Chapter 6 : Overfitting, Regularization, and Information Criteria

##6.1 The problem with parameters

###6.1.1. More parameters always improve fit
  
# open tiduverse and rcartocolor
library(tidyverse)
library(rcartocolor)



d <- 
  tibble(species = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"), 
         brain   = c(438, 452, 612, 521, 752, 871, 1350), 
         mass    = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5))


library(ggrepel)
d %>%
  ggplot(aes(x =  mass, y = brain, label = species)) +
  geom_point(color = carto_pal(7, "BurgYl")[5]) +
  geom_text_repel(size = 3, color = carto_pal(7, "BurgYl")[7], family = "Courier", seed = 438) +
  coord_cartesian(xlim = 30:65) +
  labs(x = "body mass (kg)",
       y = "brain volume (cc)",
       subtitle = "Average brain volume by body\nmass for six hominin species") +
  theme_classic() +
  theme(text = element_text(family = "Courier"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))


library(broom)

fit_lm <- function(model, formula){
  model <- lm(data = d, formula = formula)
}


fits <-
  tibble(model   = str_c("b6.", 1:6),
         formula = c("brain ~ mass", 
                     "brain ~ mass + I(mass^2)", 
                     "brain ~ mass + I(mass^2) + I(mass^3)", 
                     "brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4)", 
                     "brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5)", 
                     "brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6)")) %>% 
  mutate(fit     = map2(model, formula, fit_lm)) %>% 
  mutate(tidy    = map(fit, tidy),
         glance  = map(fit, glance))

print(fits)


p <- # original scatter plot
  d %>% 
  ggplot(aes(x = mass, y = brain)) +
  geom_point(color = carto_pal(7, "BurgYl")[7]) +
  scale_x_continuous(limits = c(33, 62), expand = c(0, 0)) +
  coord_cartesian(ylim = c(300, 1500)) +
  labs(x = "body mass (kg)",
       y = "brain volume (cc)") +
  theme_classic() +
  theme(text = element_text(family = "Courier"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))

p1 <- 
  p +
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,  # Note our rare use of 89% confidence intervals
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,
              formula = y ~ x) + # first-degree polynomial formula
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = .49")))

# quadratic
p2 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 2)) + # we only change formula here
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = .54")))

# cubic
p3 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 3)) +
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = .68")))

# fourth-order polynomial
p4 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 4)) +
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = .81")))

# fifth-order polynomial
p5 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 5)) +
  coord_cartesian(ylim = c(150, 1900)) +  
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = .99")))

# sixth-order polynomial
p6 <-
  p + 
  geom_hline(yintercept = 0, color = carto_pal(7, "BurgYl")[2], linetype = 2) +  
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 6)) +
  coord_cartesian(ylim = c(-300, 1500)) +
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = 1")))

library(gridExtra)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)


# plot to examine fitness
fits <-
  fits %>% 
  mutate(r2      = glance %>% map_dbl("r.squared")) %>% 
  # glance: convert to other R objec single-row data frame
  # map_dbl: return numeric type of data
  mutate(r2_text = round(r2, digits = 2) %>% as.character() %>% str_replace(., "0.", "."))

# plot to check R square
fits %>% 
  ggplot(aes(x = r2, y = formula, label = r2_text)) +
  geom_text(color = carto_pal(7, "BurgYl")[7], size = 3.5)  +
  labs(x = expression(italic(R)^2),
       y = NULL) +
  scale_x_continuous(limits = 0:1, breaks = 0:1) +
  theme_classic() +
  theme(axis.text.y  = element_text(hjust = 0),
        axis.ticks.y = element_blank(),
        text         = element_text(family = "Courier"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))


###6.1.2. Too few parameters hurts, too.

b6.7 <- lm(data = d,brain ~ 1)
glance(b6.7)
p +
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ 1) +
  ggtitle(NULL, subtitle = expression(paste(italic(R)^2, " = 0")))

####6.1.2.1 Overthinking: Dropping rows.

d %>%
  filter(row_number() != 2)
nd <- tibble(mass = c(30, 70))

make_lines <- function(row){ # one-degree
  my_fit <-
    d %>%
    filter(row_number() != row) %>% 
    lm(formula = brain ~ mass)
  
  predict(my_fit, nd) %>% 
    as_tibble() %>% 
    rename(brain = value) %>% 
    bind_cols(nd)
}

lines <-
  tibble(row = 1:7) %>% 
  mutate(p = map(row, make_lines)) %>% 
  unnest(p)

p1 <-
  p + 
  scale_x_continuous(expand = c(0, 0)) +
  geom_line(data = lines, 
            aes(x = mass, y = brain, group = row),
            color = carto_pal(7, "BurgYl")[6], alpha = 1/2, size = 1/2)
# because these lines will be very curvy, we'll need new data over many points of `mass`
nd <- tibble(mass = seq(from = 30, to = 65, length.out = 200))

# redifine the function
make_lines <- function(row){
  my_fit <-
    d %>%
    filter(row_number() != row) %>% 
    lm(formula = brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6))
  
  predict(my_fit, nd) %>% 
    as_tibble() %>% 
    rename(brain = value) %>% 
    bind_cols(nd)
}

# make our new tibble
lines <-
  tibble(row = 1:7) %>% 
  mutate(p = map(row, make_lines)) %>% 
  unnest(p)

# plot!
p2 <-
  p +
  geom_line(data = lines, 
            aes(group = row),
            color = carto_pal(7, "BurgYl")[6], alpha = 1/2, size = 1/2) +
  coord_cartesian(ylim = -300:2000)


grid.arrange(p1, p2, ncol = 2)

#6.2 Information theory and model performance

###6.2.1 Firing the weatherperson.

weatherperson <-
  tibble(day        = 1:10,
         prediction = rep(c(1, 0.6), times = c(3, 7)),
         observed   = rep(c(1, 0), times = c(3, 7))) 

weatherperson %>% 
  gather(key, value, -day) %>% 
  
  ggplot(aes(x = day, y = key, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value, color = value == 0)) +
  scale_x_continuous(breaks = 1:10, expand = c(0, 0)) +
  scale_y_discrete(NULL, expand = c(0, 0)) +
  scale_fill_viridis_c(direction = -1) +
  scale_color_manual(values = c("white", "black")) +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        text = element_text(family = "Courier"))


newcomer <-
  tibble(day        = 1:10,
         prediction = 0,
         observed   = rep(c(1, 0), times = c(3, 7)))

weatherperson %>% 
  bind_rows(newcomer) %>% 
  mutate(person = rep(c("weatherperson", "newcomer"), each = n()/2),
         hit    = ifelse(prediction == observed, 1, 1 - prediction - observed)) %>% 
  group_by(person) %>% 
  summarise(hit_rate = mean(hit))

####6.2.1.1 Costs and benefits.

weatherperson %>% 
  bind_rows(newcomer) %>% 
  mutate(person = rep(c("weatherperson", "newcomer"), each = n()/2),
         points = ifelse(observed == 1 & prediction != 1, -5,
                         ifelse(observed == 1 & prediction == 1, -1,
                                -1 * prediction))) %>% 
  group_by(person) %>% 
  summarise(happiness = sum(points))


####6.2.1.2 Measuring accuracy.

weatherperson %>% 
  bind_rows(newcomer) %>% 
  mutate(person = rep(c("weatherperson", "newcomer"), each = n()/2),
         hit    = ifelse(prediction == observed, 1, 1 - prediction - observed)) %>% 
  group_by(person, hit) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(power = hit ^ n,
         term  = rep(letters[1:2], times = 2)) %>% 
  select(person, term, power) %>% 
  spread(key = term, value = power) %>% 
  mutate(probability_correct_sequence = a * b)


###6.2.2 Information and uncertainty.
tibble(place  = c("McElreath's house", "Abu Dhabi"),
       p_rain = c(.3, .01)) %>% 
  mutate(p_shine = 1 - p_rain) %>% 
  group_by(place) %>% 
  mutate(H_p = (p_rain * log(p_rain) + p_shine * log(p_shine)) %>% mean() * -1)


###6.2.3 From entropy to accuracy.
t <- 
  tibble(p_1  = .3,
         p_2  = .7,
         q_1  = seq(from = .01, to = .99, by = .01)) %>% 
  mutate(q_2  = 1 - q_1) %>%
  mutate(d_kl = (p_1 * log(p_1 / q_1)) + (p_2 * log(p_2 / q_2)))

head(t)

t %>% 
  ggplot(aes(x = q_1, y = d_kl)) +
  geom_vline(xintercept = .3, color = carto_pal(7, "BurgYl")[5], linetype = 2) +
  geom_line(color = carto_pal(7, "BurgYl")[7], size = 1.5) +
  annotate(geom = "text", x = .4, y = 1.5, label = "q = p",
           color = carto_pal(7, "BurgYl")[5], family = "Courier", size = 3.5) +
  labs(x = "q[1]",
       y = "Divergence of q from p") +
  theme_classic() +
  theme(text = element_text(family = "Courier"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))

#### 6.2.3.1 Rethinking: Divergence depends upon direction

tibble(direction = c("Earth to Mars", "Mars to Earth"),
       p_1    = c(.01, .7),
       q_1    = c(.7, .01)) %>% 
  mutate(p_2  = 1 - p_1,
         q_2  = 1 - q_1) %>%
  mutate(d_kl = (p_1 * log(p_1 / q_1)) + (p_2 * log(p_2 / q_2)))

###6.2.4 From divergence to deviance.
lm(data = d,
   brain ~ mass) %>% 
  logLik() * -2

   ####6.2.4.1 Overthinking: Computing deviance.
d <-
  d %>%
  mutate(mass_s = (mass - mean(mass)) / sd(mass))
#build the stan model.
library(rstan)
m06_8 = "
data {
int N;
vector[N] brain;
vector[N] mass_s;
}
parameters {
real a;
real b;
real sigma;
}
model {
vector[N] mu = a + b * mass_s;
a ~ normal(0,10000);
b ~ normal(0,10000);
sigma ~ cauchy(0, 10);

brain ~ normal(mu, sigma);
}
generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + b * mass_s[n];
  log_lik[n] = normal_lpdf(brain[n] | mu[n], sigma);
  }
}
}
"
#set initial points
inits <- list(a = mean(d$brain),
              mass_s    = 0,
              sigma     = sd(d$brain))
inits_list <-list(inits, inits, inits, inits)

#set data and run stan programing
dat = list(N = 7, brain = d$brain, mass_s=d$mass_s)
fit06_8 <- stan(model_code = m06_8, 
                data = dat,
                cores = 4,
                iter = 2000, 
                chains = 4,
                init = inits_list)
print(fit06_8)


# use extract_log_lik() in loo package.
library(loo)
log_lik_06_8 <- extract_log_lik(fit06_8, merge_chains = FALSE)

dfLL <-
  fit06_8 %>%
  extract_log_lik() %>%
  as_tibble()

dfLL <-
  dfLL %>%
  mutate(sums     = rowSums(.),
         deviance = -2*sums)

mean(dfLL$deviance)


###6.2.5 From deviance to out-of-sample.
library(rethinking)

N       <- 20
kseq    <- 1:5
# I've reduced this number by one order of magnitude to reduce computation time
n_sim   <- 1e3
n_cores <- 4

# here's our dev object based on `N <- 20`
dev_20 <-
  sapply(kseq, function(k) {
    print(k);
    r <- mcreplicate(n_sim, sim.train.test(N = N, k = k),
                     mc.cores = n_cores);
    c(mean(r[1, ]), mean(r[2, ]), sd(r[1, ]), sd(r[2, ]))
  })


# here's our dev object based on N <- 100
N       <- 100
dev_100 <- 
  sapply(kseq, function(k) {
    print(k);
    r <- mcreplicate( n_sim , sim.train.test( N=N, k=k ),
                      mc.cores = n_cores);
    c(mean(r[1, ]), mean(r[2, ]), sd(r[1, ]), sd(r[2, ]))
  })

dev_tibble <-
  dev_20 %>% 
  as_tibble() %>% 
  bind_rows(
    dev_100 %>%
      as_tibble()
  ) %>% 
  mutate(N = rep(c("N = 20", "N = 100"), each = 4),
         statistic = rep(c("mean", "sd"), each = 2) %>% rep(., times = 2),
         sample    = rep(c("in", "out"), times = 2) %>% rep(., times = 2)) %>% 
  gather(n_par, value, -N, -statistic, -sample) %>% 
  spread(key = statistic, value = value) %>% 
  mutate(N     = factor(N, levels = c("N = 20", "N = 100")),
         n_par = str_remove(n_par, "V") %>% as.double()) %>% 
  mutate(n_par = ifelse(sample == "in", n_par - .075, n_par + .075))

head(dev_tibble)

# this intermediary tibble will make `geom_text()` easier
dev_text <-
  dev_tibble %>% 
  filter(n_par > 1.5, 
         n_par < 2.5) %>% 
  mutate(n_par = ifelse(sample == "in", n_par - .2, n_par + .28))

# the plot
dev_tibble %>% 
  ggplot(aes(x     = n_par, y = mean,
             ymin  = mean - sd, ymax = mean + sd,
             group = sample,
             color = sample, 
             fill  = sample)) +
  geom_pointrange(shape = 21) +
  geom_text(data = dev_text,
            aes(label = sample)) +
  scale_color_manual(values = c(carto_pal(7, "BurgYl")[7], carto_pal(7, "BurgYl")[5])) +
  scale_fill_manual(values  = c(carto_pal(7, "BurgYl")[5], carto_pal(7, "BurgYl")[7])) +
  labs(x = "number of parameters",
       y = "deviance") +
  theme_classic() +
  theme(text             = element_text(family = "Courier"),
        legend.position  = "none",
        strip.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[1], 1/4), color = "white"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4))) +
  facet_wrap(~N, scale = "free_y")

####6.2.5.1 Overthinking: Simulated training and testing.
#6.3 Regularization
tibble(x = seq(from = - 3.5, 
               to   = 3.5, 
               by   = .01)) %>%
  
  ggplot(aes(x = x)) +
  geom_ribbon(aes(ymin = 0, ymax = dnorm(x, mean = 0, sd = 0.2)), 
              fill = carto_pal(7, "BurgYl")[7], alpha = 1/2) +
  geom_ribbon(aes(ymin = 0, ymax = dnorm(x, mean = 0, sd = 0.5)), 
              fill = carto_pal(7, "BurgYl")[6], alpha = 1/2) +
  geom_ribbon(aes(ymin = 0, ymax = dnorm(x, mean = 0, sd = 1)), 
              fill = carto_pal(7, "BurgYl")[5], alpha = 1/2) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = "parameter value") +
  coord_cartesian(xlim = c(-3, 3)) +
  theme_classic() +
  theme(text = element_text(family = "Courier"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))

library(rethinking)

n_sim <- 1e3

make_sim <- function(N, b_sigma){
  sapply(kseq, function(k) {
    print(k);
    r <- replicate(n_sim, sim.train.test(N = N, k = k, b_sigma = b_sigma) );
    #for Mac:
    #mcreplicate(n_sim, sim.train.test(N = N, k = k, b_sigma = b_sigma),mc.cores = n_cores);
    c(mean(r[1, ]), mean(r[2, ]), sd(r[1, ]), sd(r[2, ])) }) %>% 
    
    # this is a new line of code
    as_tibble()
}
s <-
  tibble(N       = rep(c(20, 100), each = 3),
         b_sigma = rep(c(1, .5, .2), times = 2)) %>% 
  mutate(sim     = map2(N, b_sigma, make_sim)) %>% 
  unnest()
# wrangle the simulation data
s %>% 
  mutate(statistic = rep(c("mean", "sd"), each = 2) %>% rep(., times = 3 * 2),
         sample    = rep(c("in", "out"), times = 2) %>% rep(., times = 3 * 2)) %>% 
  gather(n_par, value, -N, -b_sigma, -statistic, -sample) %>% 
  spread(key = statistic, value = value) %>% 
  mutate(N     = str_c("N = ", N) %>% factor(., levels = c("N = 20", "N = 100")),
         n_par = str_remove(n_par, "V") %>% as.double())  %>% 
  
  # now plot
  ggplot(aes(x = n_par, y = mean,
             group = interaction(sample, b_sigma))) +
  geom_line(aes(color = sample, size = b_sigma %>% as.character())) +
  # this function contains the data from the previous simulation
  geom_point(data = dev_tibble, 
             aes(x = n_par, y = mean, group = sample, fill = sample),
             color = "black", shape = 21, size = 2.5, stroke = .1) +
  scale_fill_manual(values = c(carto_pal(7, "BurgYl")[7], carto_pal(7, "BurgYl")[5])) +
  scale_color_manual(values = c(carto_pal(7, "BurgYl")[7], carto_pal(7, "BurgYl")[5])) +
  scale_size_manual(values = c(1, .5, .2)) +
  labs(x = "number of parameters",
       y = "deviance") +
  theme_classic() +
  theme(text             = element_text(family = "Courier"),
        legend.position  = "none",
        strip.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[1], 1/4), color = "white"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4))) +
  facet_wrap(~N, scale = "free_y")

##6.4 Information criteria
dev_tibble <-
  dev_tibble %>% 
  select(-sd) %>% 
  mutate(n_par  = ifelse(sample == "in", n_par + .075, n_par - .075)) %>% 
  spread(key = sample, value = mean) %>% 
  mutate(height = (out - `in`) %>% round(digits = 1) %>% as.character(),
         dash   = `in` + 2 * n_par)

dev_tibble  %>% 
  ggplot(aes(x = n_par)) +
  geom_line(aes(y = dash),
            linetype = 2, color = carto_pal(7, "BurgYl")[5]) +
  geom_point(aes(y = `in`),
             color = carto_pal(7, "BurgYl")[7], size = 2) +
  geom_point(aes(y = out),
             color = carto_pal(7, "BurgYl")[5], size = 2) +
  geom_errorbar(aes(x = n_par + .15,
                    ymin = `in`, ymax = out),
                width = .1, color = carto_pal(7, "BurgYl")[6]) +
  geom_text(aes(x = n_par + .4,
                y = (out + `in`) / 2,
                label = height),
            family = "Courier", size = 3, color = carto_pal(7, "BurgYl")[6]) +
  labs(x = "number of parameters",
       y = "deviance") +
  theme_classic() +
  theme(text             = element_text(family = "Courier"),
        strip.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[1], 1/4), color = "white"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4))) +
  facet_wrap(~N, scale = "free_y")

###6.4.1 DIC.

###6.4.2 WAIC.

####6.4.2.1 Overthinking: WAIC calculation

library(rstan)
d <-cars
m = "
data {
int N;
vector[N] speed;
vector[N] dist;
}
parameters {
real a;
real b;
real <lower=0, upper=10>sigma;
}
model {
vector[N] mu = a + b * speed;
a ~ normal(0,100);
b ~ normal(0,10);

dist ~ normal(mu, sigma);
}
generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + b * speed[n];
  log_lik[n] = normal_lpdf(dist[n] | mu[n], sigma);
  }
}
}
"
dat = list(N = 50,speed = d$speed,dist=d$dist)
fit <- stan(model_code = m, 
            data = dat,
            cores = 4,
            iter = 2000, 
            chains = 4)
print(fit)

library(loo)
dfLL <-
  fit %>%
  extract_log_lik() %>%
  as_tibble()

dfmean <-
  dfLL %>%
  exp() %>%
  summarise_all(mean) %>%
  gather(key, means) %>%
  select(means) %>%
  log()

lppd <-
    dfmean %>%
    sum()

dfvar <-
  dfLL %>%
  summarise_all(var) %>%
  gather(key, vars) %>%
  select(vars) 

pwaic <-
  dfvar %>%
  sum()

-2 * (lppd - pwaic)

fit %>%
  extract_log_lik() %>%
  waic()

dfmean %>%
  mutate(waic_vec   = -2 * (means - dfvar$vars)) %>%
  summarise(waic_se = (var(waic_vec) * nrow(dfmean)) %>% sqrt())





###6.4.3. DIC and WAIC as estimates of deviance.

n_sim <- 1e3

make_sim <- function(b_sigma){
  sapply(kseq, function(k) {
    print(k);
    r <- mcreplicate(n_sim,
              sim.train.test(N         = 20,
                             k         = k,
                             b_sigma   = b_sigma,
                             DIC       = T,
                             WAIC      = T,
                             devbar    = T,
                             devbarout = T),
               mc.cores = n_cores);
    
    c(dev_in    = mean(r[1, ]),
      dev_out   = mean(r[2, ]),
      DIC       = mean(r[3, ]), 
      WAIC      = mean(r[4, ]), 
      devbar    = mean(r[5, ]), 
      devbarout = mean(r[6, ])) 
  }
  ) %>% 
    data.frame() %>% 
    rownames_to_column() %>% 
    rename(statistic = rowname)
}

s <-
  tibble(b_sigma = c(100, .5)) %>% 
  mutate(sim = purrr::map(b_sigma, make_sim)) %>% 
  unnest()

#plot
s %>% 
  gather(n_par, value, -b_sigma, -statistic) %>% 
  mutate(n_par = str_remove(n_par, "X") %>% as.double()) %>% 
  filter(statistic != "devbar" & statistic != "devbarout") %>% 
  spread(key = statistic, value = value) %>% 
  gather(ic, value, -b_sigma, -n_par, -dev_in, -dev_out) %>% 
  gather(sample, deviance, -b_sigma, -n_par, -ic, -value) %>% 
  filter(sample == "dev_out") %>% 
  mutate(b_sigma = b_sigma %>% as.character()) %>% 
  
  ggplot(aes(x = n_par)) +
  geom_point(aes(y = deviance, color = b_sigma),
             size = 2.5) +
  geom_line(aes(y = value, group = b_sigma, color = b_sigma)) +
  scale_color_manual(values = c(carto_pal(7, "BurgYl")[7], carto_pal(7, "BurgYl")[5])) +
  # scale_color_manual(values = c("steelblue", "black")) +
  labs(subtitle = "N = 20",
       x = "number of parameters",
       y = "deviance") +
  theme_classic() +
  theme(text             = element_text(family = "Courier"),
        strip.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[1], 1/4), color = "white"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)),
        legend.position  = "none") +
  facet_wrap(~ic, ncol = 1)

##6.5. Using information criteria

###6.5.1. Model comparison
library(rethinking)
data(milk)

d <- 
  milk %>%
  filter(complete.cases(.))
rm(milk)

d <-
  d %>%
  mutate(neocortex = neocortex.perc / 100)

####6.5.1.1 Modeling and comparing WAIC values.

m06_11 = "
data {
int N;
vector[N] kcal_per_g;
}
parameters {
real <lower=-1000, upper=1000>a;
real <lower=0, upper=100>sigma;
}
model {

kcal_per_g ~ normal(a, sigma);
}
generated quantities {
vector[N] log_lik;
{
  for(n in 1:N) {
  log_lik[n] = normal_lpdf(kcal_per_g[n] | a, sigma);
  }
}
}
"
inits <- list(Intercept = mean(d$kcal.per.g),
              sigma     = sd(d$kcal.per.g))
inits_list <-list(inits, inits, inits, inits)

dat = list(N = 17,kcal_per_g = d$kcal.per.g)
fit06_11 <- stan(model_code = m06_11, 
                 data = dat,
                 cores = 4,
                 iter = 2000,
                 chains = 4,
                 init = inits_list)
log_lik_06_11 <- extract_log_lik(fit06_11, merge_chains = FALSE)
waic06_11 <- waic(log_lik_06_11)

m06_12 = "
data {
int N;
vector[N] kcal_per_g;
vector[N] neocortex;
}
parameters {
real <lower=-1000, upper=1000>a;
real <lower=-1000, upper=1000>b;
real <lower=0, upper=100>sigma;
}
model {
vector[N] mu = a + b * neocortex ;
kcal_per_g ~ normal(mu, sigma);
}
generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + b * neocortex[n];
  log_lik[n] = normal_lpdf(kcal_per_g[n] | mu[n], sigma);
  }
}
}
"
inits <- list(Intercept = mean(d$kcal.per.g),
              neocortex = 0,
              sigma     = sd(d$kcal.per.g))
inits_list <-list(inits, inits, inits, inits)

dat = list(N = 17,kcal_per_g = d$kcal.per.g,neocortex=d$neocortex)
fit06_12 <- stan(model_code = m06_12,
                 data = dat,
                 cores = 4,
                 iter = 2000,
                 warmup = 1000,
                 chains = 4)
log_lik_06_12 <- extract_log_lik(fit06_12, merge_chains = FALSE)
(waic06_12 <- waic(log_lik_06_12))


m06_13 = "
data {
int N;
vector[N] kcal_per_g;
vector[N] log_mass;
}
parameters {
real <lower=-1000, upper=1000>a;
real <lower=-1000, upper=1000>b;
real <lower=0, upper=100>sigma;
}

model {

vector[N] mu = a + b * log_mass ;
kcal_per_g ~ normal(mu, sigma);
}
generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + b * log_mass[n];
  log_lik[n] = normal_lpdf(kcal_per_g[n] | mu[n], sigma);
  }
}
}
"

inits <- list(Intercept   = mean(d$kcal.per.g),
              `log(mass)` = 0,
              sigma       = sd(d$kcal.per.g))
inits_list <-list(inits, inits, inits, inits)

dat = list(N = 17,kcal_per_g = d$kcal.per.g,log_mass=log(d$mass))
fit06_13 <- stan(model_code = m06_13, 
                 data = dat,
                 cores = 4,
                 iter = 2000,
                 chains = 4)
log_lik_06_13 <- extract_log_lik(fit06_13, merge_chains = FALSE)
(waic06_13 <- waic(log_lik_06_13))

m06_14 = "
data {
int N;
vector[N] kcal_per_g;
vector[N] neocortex;
vector[N] log_mass;
}
parameters {
real <lower=-1000, upper=1000>a;
real <lower=-1000, upper=1000>bn;
real <lower=-1000, upper=1000>bm;
real <lower=0, upper=100>sigma;
}

model {

vector[N] mu = a + bn * neocortex + bm * log_mass ;
kcal_per_g ~ normal(mu, sigma);
}
generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + bn * neocortex[n] + bm * log_mass[n];
  log_lik[n] = normal_lpdf(kcal_per_g[n] | mu[n], sigma);
  }
}
}
"
inits <- list(Intercept   = mean(d$kcal.per.g),
              neocortex   = 0,
              `log(mass)` = 0,
              sigma       = sd(d$kcal.per.g))
inits_list <-list(inits, inits, inits, inits)

dat = list(N = 17,
           kcal_per_g = d$kcal.per.g,
           neocortex = d$neocortex,
           log_mass=log(d$mass))
fit06_14 <- stan(model_code = m06_14,
                 data = dat,
                 cores = 4,
                 iter = 2000, 
                 chains = 4)
log_lik_06_14 <- extract_log_lik(fit06_14, merge_chains = FALSE)
(waic06_14 <- waic(log_lik_06_14))

library(loo)
loo::compare(waic06_11, waic06_12, waic06_13, waic06_14)

waic_tibble <- function(chr,...){
  tibble(model_name = rep(chr, times = 3)) %>% 
    bind_cols(
      waic(...)$estimates %>% 
        data.frame() %>% 
        rownames_to_column() %>% 
        rename(statistic = rowname,
               estimate  = Estimate,
               se        = SE)
    )
}
waic_tibble("fit06_11",log_lik_06_11)

#plot
waic_tibble("fit06_11",log_lik_06_11) %>% 
  bind_rows(
    waic_tibble("fit06_12",log_lik_06_12),
    waic_tibble("fit06_13",log_lik_06_13),
    waic_tibble("fit06_14",log_lik_06_14)
  ) %>% 
  filter(statistic == "waic") %>% 
  
  ggplot(aes(x    = model_name, 
             y    = estimate, 
             ymin = estimate - se, 
             ymax = estimate + se)) +
  geom_pointrange(shape = 21, color = carto_pal(7, "BurgYl")[7], fill = carto_pal(7, "BurgYl")[5]) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "My custom WAIC plot") +
  theme_classic() +
  theme(text = element_text(family = "Courier"),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))

####6.5.1.2 Comparing estimates.

tidy(fit06_14)

my_coef_tab <-
  rbind(tidy(fit06_11), tidy(fit06_12), tidy(fit06_13), tidy(fit06_14)) %>%
  mutate(model = c(rep("fit06_11", times = nrow(tidy(fit06_11))),
                   rep("fit06_12", times = nrow(tidy(fit06_12))),
                   rep("fit06_13", times = nrow(tidy(fit06_13))),
                   rep("fit06_14", times = nrow(tidy(fit06_14))))
  ) %>%
  filter(term != "lp__") %>%
  select(model, everything())

head(my_coef_tab)

my_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  select(model, term, estimate) %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate)

p11 <- brms::posterior_samples(fit06_11)
p12 <- brms::posterior_samples(fit06_12)
p13 <- brms::posterior_samples(fit06_13)
p14 <- brms::posterior_samples(fit06_14)

# Here we put it all together and plot!
tibble(mdn = c(NA, median(p11[, 1]), median(p12[, 1]), median(p13[, 1]), median(p14[, 1]),
               NA, NA, median(p12[, 2]), NA, median(p14[, 2]),
               NA, median(p11[, 2]), NA, median(p13[, 2]), NA,
               NA, median(p11[, 2]), median(p12[, 3]), median(p13[, 3]), median(p14[, 4])),
       sd  = c(NA, sd(p11[, 1]), sd(p12[, 1]), sd(p13[, 1]), sd(p14[, 1]),
               NA, NA, sd(p12[, 2]), NA, sd(p14[, 2]),
               NA, sd(p11[, 2]), NA, sd(p13[, 2]), NA,
               NA, sd(p11[, 2]), sd(p12[, 3]), sd(p13[, 3]), sd(p14[, 4])),
       order = c(20:1)) %>%
  
  ggplot(aes(x = mdn, y = order)) +
  geom_hline(yintercept = 0, color = carto_pal(7, "BurgYl")[2]) +
  geom_pointrange(aes(x = order, y = mdn, 
                      ymin = mdn - sd, 
                      ymax = mdn + sd),
                  shape = 21, color = carto_pal(7, "BurgYl")[7], fill = carto_pal(7, "BurgYl")[5]) +
  scale_x_continuous(breaks = 20:1,
                     labels = c("intercept", c("fit06_11", "fit06_12", "fit06_13", "fit06_14"),
                                "neocortex", c("fit06_11", "fit06_12", "fit06_13", "fit06_14"),
                                "logmass",   c("fit06_11", "fit06_12", "fit06_13", "fit06_14"),
                                "sigma",     c("fit06_11", "fit06_12", "fit06_13", "fit06_14"))) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "My custom coeftab() plot") +
  theme_classic() +
  theme(text             = element_text(family = "Courier"),
        axis.ticks.y     = element_blank(),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))



#Chapter 7 : Interactions

##7.1 Building an interaction.

data('rugged', package = 'rethinking')
d  <- rugged; rm(rugged)

#Transform data.
d <- 
  d %>%
  mutate(loggdp = log(rgdppc_2000))

# extract countries with GDP data
d <-
  d %>%
  filter(complete.cases(rgdppc_2000))

# split countries into Africa and not-Africa
d.A1 <-
  d %>%
  filter(cont_africa == 1)

d.A0 <-
  d %>%
  filter(cont_africa == 0)

m07_1="
data {
int N;
vector[N] loggdp;
vector[N] rugged;
}
parameters {
real a;
real bR;
real<lower=0, upper=10> sigma;
}
model {
vector[N] mu = a + bR * rugged;
//prior
a ~ normal(8,100);
bR ~ normal(0,1);
//likelihood
loggdp ~ normal(mu,sigma);  
}
"

dat <- list(
  N = NROW(d.A1),
  loggdp = d.A1$loggdp,
  rugged = d.A1$rugged
  )
fit07_1 = stan(model_code = m07_1, 
               data = dat, 
               cores = 4, 
               chains = 4, 
               iter = 3000)

post <- as.data.frame(fit07_1)
mu <- post$a + d.A1$rugged %*% t(post$bR)
mu_mean <- rowMeans(mu)
mu_hpdi <- apply(mu, 1, rethinking::HPDI)
# simulate loggdp
iter <- 1e5
y_hat <- matrix(nrow = iter, ncol = NROW(d.A1))
for(i in 1:NROW(d.A1)) y_hat[,i] <- rnorm(iter, post[,"a"] + d.A1[i,"rugged"] %*% t(post[,"bR"]), post[,"sigma"])
# get stats on sim
y_hat_avg <- colMeans(y_hat)
y_hat_pi <- apply(y_hat, 2, rethinking::PI)

##Figure 7.2a

d.A1 %>%
  ggplot(aes(x = rugged)) +
  geom_ribbon(aes(x = d.A1$rugged, 
                  ymin = mu_hpdi[1,], 
                  ymax = mu_hpdi[2,]), 
              alpha = 1/4,) +
  geom_point(aes(rugged, loggdp), 
             shape = 16, 
             color = 'dodgerblue',size = 2) + 
  geom_abline(slope = mean(post$bR),
              intercept = mean(post$a),
              size = 1 ) +
  theme(text = element_text(family = "Times"),
        legend.position = "none") +
  theme_pander() + 
  scale_x_continuous("Terrain Ruggedness Index", expand = c(0, 0)) +
  ylab("log GDP from year 2000") 

m07_2 <- m07_1
dat <- list(
  N = NROW(d.A0),
  loggdp = d.A0$loggdp,
  rugged = d.A0$rugged
)
fit07_2 = stan(model_code = m07_2, 
               data = dat, 
               cores = 4, 
               chains = 4, 
               iter = 3000)

post <- as.data.frame(fit07_2)
mu <- post$a + d.A0$rugged %*% t(post$bR)
mu_mean <- rowMeans(mu)
mu_hpdi <- apply(mu, 1, rethinking::HPDI)
# simulate loggdp
iter <- 1e5
y_hat <- matrix(nrow = iter, ncol = NROW(d.A0))
for(i in 1:NROW(d.A0)) y_hat[,i] <- rnorm(iter, post[,"a"] + d.A0[i,"rugged"] %*% t(post[,"bR"]), post[,"sigma"])
# get stats on sim
y_hat_avg <- colMeans(y_hat)
y_hat_pi <- apply(y_hat, 2, rethinking::PI)

##Figure 7.2b

d.A0 %>%
  ggplot(aes(x = rugged)) +
  geom_ribbon(aes(x = d.A0$rugged, 
                  ymin = mu_hpdi[1,], 
                  ymax = mu_hpdi[2,]), 
              alpha = 1/4,) +
  geom_point(aes(rugged, loggdp), 
             shape = 16, 
             color = 'green',size = 2) + 
  geom_abline(slope = mean(post$bR),
              intercept = mean(post$a),
              size = 1 ) +
  theme(text = element_text(family = "Times"),
        legend.position = "none") +
  theme_pander() + 
  scale_x_continuous("Terrain Ruggedness Index", expand = c(0, 0)) +
  ylab("log GDP from year 2000") 

### 7.1.1 Adding a dummy variable doesn't work
m07_3 ="
data {
int N;
vector[N] loggdp;
vector[N] rugged;
}
parameters {
real a;
real bR;
real<lower=0, upper=10> sigma;
}
model {
vector[N] mu = a + bR * rugged;
//prior
a ~ normal(8,100);
bR ~ normal(0,1);
//likelihood
loggdp ~ normal(mu,sigma);  
}
generated quantities {
// For model comparison, we'll want to keep the likelihood contribution of each point
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + bR * rugged[n];
  log_lik[n] = normal_lpdf(loggdp[n] | mu[n], sigma);
  }
}
}
"

dat <- list(
  N = NROW(d),
  loggdp = d$loggdp,
  rugged = d$rugged
)
fit07_3 = stan(model_code = m07_3, 
               data = dat, 
               cores = 4, 
               chains = 4, 
               iter = 3000)

m07_4 ="
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
real<lower=0, upper=10> sigma;
}
model {
vector[N] mu = a + bR * rugged + bA * cont_africa;
//prior
a  ~ normal(8,100);
bR ~ normal(8,100);
bA ~ normal(8,100);

//likelihood
loggdp ~ normal(mu,sigma);  
}
generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + bR * rugged[n] + bA * cont_africa[n];
  log_lik[n] = normal_lpdf(loggdp[n] | mu[n], sigma);
  }
}
}
"

dat <- list(
  N = NROW(d),
  loggdp = d$loggdp,
  rugged = d$rugged,
  cont_africa = d$cont_africa
)
fit07_4 = stan(model_code = m07_4, 
               data = dat,
               cores = 4, 
               chains = 4, 
               iter = 3000)

# compare models
library(loo)
log_lik_07_3 <- extract_log_lik(fit07_3, merge_chains = FALSE)
waic07_3 <- waic(log_lik_07_3)
log_lik_07_4 <- extract_log_lik(fit07_4, merge_chains = FALSE)
waic07_4 <- waic(log_lik_07_4)
c(waic07_3$waic,waic07_4$waic)
mod_comp <- loo::compare(waic07_3, waic07_4)
mod_comp


post <- as.data.frame(fit07_4)
mu <- post$a + d$rugged %*% t(post$bR) + d$cont_africa %*% t(post$bA)
mu_mean <- rowMeans(mu)
mu_hpdi <- apply(mu, 1, rethinking::HPDI)
# simulate loggdp
iter <- 1e5
y_hat <- matrix(nrow = iter, ncol = NROW(d))
for(i in 1:NROW(d)) y_hat[,i] <- rnorm(iter, post[,"a"] + d[i,"rugged"] %*% t(post[,"bR"]) + d[i,"cont_africa"] %*% t(post[,"bA"]), post[,"sigma"])
# get stats on sim
y_hat_avg <- colMeans(y_hat)
y_hat_pi <- apply(y_hat, 2, rethinking::PI)

##Figure 7.3
d_04 <- d %>% 
  mutate(mu_mean = mu_mean,
         mu_hpdi_l = mu_hpdi[1,],
         mu_hpdi_h = mu_hpdi[2,])
d_04%>%
  ggplot(aes(x = rugged)) +
  geom_point(aes(rugged, loggdp, color = cont_africa), shape = 16) +
  theme(legend.position = '') +
  geom_line(data = d_04 %>% filter(cont_africa == 0),
            aes(rugged, mu_mean)) +
  geom_ribbon(data = d_04 %>% filter(cont_africa == 0),
              aes(x=rugged, ymin=mu_hpdi_l, ymax=mu_hpdi_h),
              alpha = .1) +
  geom_line(data = d_04 %>% filter(cont_africa == 1),
            aes(rugged, mu_mean),
            color = 'dodgerblue') +
  geom_ribbon(data = d_04 %>% filter(cont_africa == 1),
              aes(x=rugged, ymin=mu_hpdi_l, ymax=mu_hpdi_h),
              alpha = .1,
              fill = 'dodgerblue') +
  scale_x_continuous("Terrain Ruggedness Index", expand = c(0, 0)) +
  ylab("log GDP from year 2000") +
  theme_pander()  

### 7.1.2 Adding a linear interaction does work

m07_5="
data {
int N;
vector[N] loggdp;
vector[N] rugged;
vector[N] cont_africa;
}
parameters {
real a;
real bR;
real bA;
real bAR;
real<lower=0, upper=10> sigma;
}
model {
// transfer variable
vector[N] gamma;
vector[N] mu;
for (i in 1:N) {
gamma[i] = bR + bAR * cont_africa[i];
mu[i] = a + gamma[i] * rugged[i] + bA * cont_africa[i];
}
//prior
a   ~ normal(8,100);
bR  ~ normal(0,1);
bA  ~ normal(0,1);
bAR ~ normal(0,1);

//likelihood
loggdp ~ normal(mu,sigma);  
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
}
"

dat <- list(
  N = NROW(d),
  loggdp = d$loggdp,
  rugged = d$rugged,
  cont_africa = d$cont_africa
)
fit07_5 = stan(model_code = m07_5, 
               data = dat, 
               cores = 4, 
               chains = 4, 
               iter = 3000)

log_lik_07_5 <- extract_log_lik(fit07_5, merge_chains = FALSE)
waic07_5 <- waic(log_lik_07_5)
mod_comp <- loo::compare(waic07_3, waic07_4, waic07_5)
mod_comp

####7.1.2 Overthinking

m07_5b = "
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
real<lower=0, upper=10> sigma;
}
model {
//transfer variable
vector[N] mu;
for (i in 1:N) {
mu[i] = a + bR * rugged[i] + bAR * rugged[i] * cont_africa[i] +      bA * cont_africa[i];
}
//prior
a   ~ normal(8,100);
bR  ~ normal(0,1);
bA  ~ normal(0,1);
bAR ~ normal(0,1);

//likelihood
loggdp ~ normal(mu,sigma); 
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
}
"

### 7.1.3 Plotting the interaction

post07_5 <- as.data.frame(fit07_5)
f_mu_07_5 <- function(rugged, cont_africa) with(post07_5, 
                                                a + bR * rugged + bAR * rugged * cont_africa + bA * cont_africa )
mu_07_5 <- mapply(f_mu_07_5, rugged = d$rugged, cont_africa = d$cont_africa)
mu_07_5_mean <- apply(mu_07_5, 2, mean)
mu_07_5_pi <- apply(mu_07_5, 2, rethinking::PI, prob = .97)
d_07_5 <- d %>%
  mutate(mu_mean = mu_07_5_mean,
         mu_pi_l = mu_07_5_pi[1,],
         mu_pi_h = mu_07_5_pi[2,],
         inAfrica = ifelse(cont_africa, 'African nations', 'Non-African nations'))

##Figure 7.4

d_07_5 %>%
  ggplot(aes(x = rugged)) +
  geom_point(aes(rugged, loggdp, color = cont_africa), shape = 16) +
  theme(legend.position = '') +
  geom_line(aes(rugged, mu_mean)) +
  geom_ribbon(aes(x=rugged, 
                  ymin=mu_pi_l, 
                  ymax=mu_pi_h, 
                  fill = cont_africa), 
              alpha = .1) +
  facet_wrap(~inAfrica) +
  theme_pander()  +
  labs(x = 'Terrain Ruggedness Index', y = 'log GDP year 2000')

###7.1.4. Interpreting an interaction estimate.

#### 7.1.4.1 parameters change meaning

#Model summary
print(fit07_5, probs = c(0.1, 0.5, 0.9), pars = c('a', 'bA', 'bR', 'bAR', 'sigma'))

#### 7.1.4.2 Incorporating uncertainty
post <- as.data.frame(fit07_5, pars = c('a', 'bA', 'bR', 'bAR'))
gamma_africa <- post$bR + post$bAR * 1
gamma_notafrica <- post$bR + post$bAR * 0

c(mean(gamma_africa), mean(gamma_notafrica))

#Figure 7.5
ggplot() +
  geom_density(aes(gamma_africa), color = 'dodgerblue') +
  geom_density(aes(gamma_notafrica), color = 'black') +
  theme_pander() + 
  labs(x = 'gamma')

diff <- gamma_africa - gamma_notafrica
sum( diff < 0 ) / length( diff )

## 7.2 Symmetric of the liner interaction

### 7.2.2 Africa depends on ruggedness
d_07_5 <- d_07_5 %>%
  mutate(rugged_hi = rugged > median(rugged),
         inAfrica_num = as.integer(inAfrica=='African nations')) %>%
  mutate(inAfrica_num = ifelse(rugged_hi, inAfrica_num + .02, inAfrica_num))
d_imaginary <- d_07_5 %>% select(rugged) %>% range
# imaginary low values
mu_rugged_lo <- mapply(f_mu_07_5, rugged = d_imaginary[1], cont_africa = 0:1)
mu_rugged_lo_mean <- apply(mu_rugged_lo, 2, mean)
mu_rugged_lo_pi <- apply(mu_rugged_lo, 2, rethinking::PI, prob = .97)
# imaginary high values
mu_rugged_hi <- mapply(f_mu_07_5, rugged = d_imaginary[2], cont_africa = 0:1)
mu_rugged_hi_mean <- apply(mu_rugged_hi, 2, mean)
mu_rugged_hi_pi <- apply(mu_rugged_hi, 2, rethinking::PI, prob = .97)
# organize data
d_imag = 
  data.frame(inAfrica_num = 0:1, 
             country = rep(c('lo', 'hi'), each = 2), 
             loggdp_mean = c(mu_rugged_lo_mean, mu_rugged_hi_mean), 
             loggdp_pi_l = c(mu_rugged_lo_pi[1,], mu_rugged_hi_pi[1,]), 
             loggdp_pi_h = c(mu_rugged_lo_pi[2,], mu_rugged_hi_pi[2,]))

#Figure 7.6
d_07_5%>%
  ggplot(aes(x = cont_africa)) +
  # graph raw actual values
  geom_point(aes(inAfrica_num, loggdp, color = rugged_hi), 
             shape = 21) +
  
  # graph imaginary hi values
  geom_ribbon(data = d_imag %>% filter(country == 'hi'),
              aes(x = inAfrica_num, y = loggdp_mean, ymin = loggdp_pi_l, ymax = loggdp_pi_h),
              alpha = .1, fill = 'dodgerblue') +
  geom_line(data = d_imag %>% filter(country == 'hi'),
            aes(x = inAfrica_num, y = loggdp_mean),
            color = 'dodgerblue') +
  
  # graph imaginary lo values
  geom_ribbon(data = d_imag %>% filter(country == 'lo'),
              aes(x = inAfrica_num, y = loggdp_mean, ymin = loggdp_pi_l, ymax = loggdp_pi_h),
              alpha = .1, fill = 'black') +
  geom_line(data = d_imag %>% filter(country == 'lo'),
            aes(x = inAfrica_num, y = loggdp_mean),
            color = 'black', linetype = 'dashed') +
  
  # other stuff
  scale_color_manual(values = c('black', 'dodgerblue')) +
  theme(legend.position = '') +
  labs(x = 'continent', y = 'log GDP year 2000') +
  scale_x_continuous(breaks = c(0.01, 1.01), labels = c('other', 'Africa'))

## 7.3 Continuous interactions
### 7.3.1 The data

data('tulips', package = 'rethinking')
d  <- tulips; rm(tulips)

### 7.3.2 The uncentered model
m07_6 ="
data {
int N;
vector[N] water;
vector[N] shade;
vector[N] blooms;
}
parameters {
real a;
real bW;
real bS;
real<lower=0> sigma;
}
model {
//transfer variable
vector[N] mu;
mu = a + bW * water + bS * shade;

//prior
a  ~ normal(0,100);
bW ~ normal(0,100);
bS ~ normal(0,100);

//likelihood
blooms ~ normal(mu,sigma);
}
generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(i in 1:N) {
  mu[i] = a + bW * water[i] + bS * shade[i];
  log_lik[i] = normal_lpdf(blooms[i] | mu[i], sigma);
  }
}
}
"

m07_7 = "
data {
int N;
vector[N] water;
vector[N] shade;
vector[N] blooms;
}
parameters {
real a;
real bW;
real bS;
real bWS;
real<lower=0> sigma;
}
model {
//transfer variable
vector[N] mu;
mu = a + bW * water + bS * shade + bWS * water .* shade;

//prior
a   ~ normal(0,100);
bW  ~ normal(0,100);
bS  ~ normal(0,100);
bWS ~ normal(0,100);
//likelihood
blooms ~ normal(mu,sigma);
}
generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(i in 1:N) {
  mu[i] = a + bW * water[i] + bS * shade[i] + bWS * water[i] * shade[i];
  log_lik[i] = normal_lpdf(blooms[i] | mu[i], sigma);
  }
}
}
"

dat <- list(N = NROW(d), water = d$water, shade = d$shade, blooms = d$blooms)
fit07_6 = stan(model_code = m07_6, data = dat, cores = 2, chains = 2, iter = 1000)
fit07_7 = stan(model_code = m07_7, data = dat, cores = 2, chains = 2, iter = 1000)

print(fit07_6, include = F, pars = 'log_lik', probs = c(0.1, 0.5, 0.9))
print(fit07_7, include = F, pars = 'log_lik', probs = c(0.1, 0.5, 0.9))

log_lik_07_6 <- extract_log_lik(fit07_6, merge_chains = FALSE)
waic07_6 <- waic(log_lik_07_6)
log_lik_07_7 <- extract_log_lik(fit07_7, merge_chains = FALSE)
waic07_7 <- waic(log_lik_07_7)
c(waic07_6$waic,waic07_7$waic)
mod_comp <- loo::compare(waic07_6, waic07_7)
mod_comp

###7.3.3 Center and re-estimate.
dat <- list(N = NROW(d), water = d$water - mean(d$water), shade = d$shade - mean(d$shade), blooms = d$blooms)
fit07_8 = stan(model_code = m07_6, data = dat, cores = 2, chains = 2, iter = 10000)
fit07_9 = stan(model_code = m07_7, data = dat, cores = 2, chains = 2, iter = 10000)

print(fit07_8, include = F, pars = 'log_lik', probs = c(0.1, 0.5, 0.9))
print(fit07_9, include = F, pars = 'log_lik', probs = c(0.1, 0.5, 0.9))

### 7.3.4 Plotting implied predictions

# setup new data
nd <- expand.grid(water = seq(-1, 1), shade = seq(-1, 1))
# get draws
post07_8 <- as.data.frame(fit07_8, pars = c('a', 'bW', 'bS'))
post07_9 <- as.data.frame(fit07_9, pars = c('a', 'bW', 'bS', 'bWS'))
# setup posterior calcs
f_mu_07_8 <- function(water, shade) with(post07_8, 
                                         a + bW * water + bS * shade  )
mu_07_8 <- mapply(f_mu_07_8, water = nd$water, shade = nd$shade)
f_mu_07_9 <- function(water, shade) with(post07_9, 
                                         a + bW * water + bS * shade + bWS * water * shade )
mu_07_9 <- mapply(f_mu_07_9, water = nd$water, shade = nd$shade)
# summarise posterior
mu_07_8_mean <- apply(mu_07_8, 2, mean)
mu_07_8_pi <- apply(mu_07_8, 2, rethinking::PI, prob = .97)
mu_07_9_mean <- apply(mu_07_9, 2, mean)
mu_07_9_pi <- apply(mu_07_9, 2, rethinking::PI, prob = .97)
# organize data
nd <- cbind(bWS = 0, nd) %>% rbind(cbind(bWS = 1, nd))
nd <- nd %>%
  mutate(bWS = ifelse(bWS, 'With Interaction', 'Without interaction'),
         water = paste0('water.c = ', water),
         blooms = c(mu_07_8_mean, mu_07_9_mean),
         blooms_pi_l = c(mu_07_8_pi[1,], mu_07_9_pi[1,]),
         blooms_pi_h = c(mu_07_8_pi[2,], mu_07_9_pi[2,]))

nd %>%
  ggplot(aes(x=shade)) +
  geom_ribbon(aes(x=shade, ymin=blooms_pi_l, ymax=blooms_pi_h,
                  fill = "#CC79A"),
              alpha = .1) +
  geom_line(aes(shade, blooms),
            color = "#CC79A7") +
  coord_cartesian(xlim = range(nd$shade), 
                  ylim = range(nd$blooms)) +
  scale_x_continuous("Shade (centered)", breaks = c(-1, 0, 1)) +
  theme(panel.border = element_rect(fill = NA),
        panel.spacing = unit(0, 'mm')) +
  ylab("Blooms") +
  theme_pander() + 
  facet_grid(bWS~water)
