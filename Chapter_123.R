# Chapter 1 Tutorial
Sys.info() # check your system information


# Chapter 2 Tutorial

# 2.1 The garden of forking data
## 2.1.1 Counting possibilities

# install and open tidyverse 
if (!require("tidyverse")) {
  install.packages("tidyverse") 
  library(tidyverse)}

# create a tibbles dataframe
d <- tibble(p_1 = 0, # every different combination of marbles selected randomly for a bag
         p_2 = rep(1:0, times = c(1, 3)), # there are 4 different combination
         p_3 = rep(1:0, times = c(2, 2)), # 0 = “white” 1 = “blue”
         p_4 = rep(1:0, times = c(3, 1)), 
         p_5 = 1)

head(d) # check the format of tibble


# %>% pipe
head(d)
# equals to
d %>% head()


# plot the possibility data
install.packages("ggplot2")
library(ggplot2)

d %>% 
  gather() %>% # turn multiple columns and collapses into key-value pairs
  mutate(x = rep(1:4, times = 5), # adds new variables and preserves existing
         possibility = rep(1:5, each = 4)) %>% 
  
  ggplot(aes(x = x, y = possibility, # specify x- and y-axis
             fill = value %>% as.character())) + # fill dots with color (value)
  geom_point(shape = 21, size = 5) + # set the shape and size of dots
  scale_fill_manual(values = c("white", "navy")) + # fill colors (0 = "white"; 1 = "navy")
  scale_x_continuous(NULL, breaks = NULL) + # delete x-axis label
  coord_cartesian(xlim = c(.75, 4.25),
                  ylim = c(.75, 5.25)) + # zooming out the plot
  theme(legend.position = "none") # delete legend


## 2.1.2 Using prior information

n_blue <- function(x){
  rowSums(x == "b")
} # self-defined function to calculate how many blue marbles

n_white <- function(x){
  rowSums(x == "w")
} # self-defined function to calculate how many white marbles


t <-
  # different combinations of 4 marbles 
  tibble(p_1 = rep(c("w", "b"), times = c(1, 4)),
         p_2 = rep(c("w", "b"), times = c(2, 3)),
         p_3 = rep(c("w", "b"), times = c(3, 2)),
         p_4 = rep(c("w", "b"), times = c(4, 1))) %>% 
  mutate(`draw 1: blue`  = n_blue(.), # ways to produce "first blue marble"
         `draw 2: white` = n_white(.), # ways to produce "second white marble"
         `draw 3: blue`  = n_blue(.)) %>%  # ways to produce "third blue marble"
  mutate(`ways to produce` = `draw 1: blue` * `draw 2: white` * `draw 3: blue`) 
         # multiply 3 ways above = ways to produce "blue, white, blue"


# we want the forth one to be blue
t <-
  t %>% 
  rename(`previous counts` = `ways to produce`,
         `ways to produce` = `draw 1: blue`) %>% 
  select(p_1:p_4, `ways to produce`, `previous counts`) %>% 
  mutate(`new count` = `ways to produce` * `previous counts`)

## 2.1.3 From counts to probability
t %>% 
  select(p_1:p_4) %>% 
  mutate(p                      = seq(from = 0, to = 1, by = .25), # the proportion of marbles that are blue
         `ways to produce data` = c(0, 3, 8, 9, 0)) %>% 
  mutate(plausibility           = `ways to produce data` / sum(`ways to produce data`))


# 2.2 Building a model

(d <- tibble(toss = c("w", "l", "w", "w", "w", "l", "w", "l", "w")))

d <-
  d %>% 
  mutate(n_trials  = 1:9,
         n_success = cumsum(toss == "w")) # cumulative sum


## 2.4.1 Grid approximation

d <- # create a data frame with 20 points
  tibble(p_grid            = seq(from = 0, to = 1, length.out = 20),  # define grid
         prior             = 1) %>%                                   # define prior
  mutate(likelihood      = dbinom(6, size = 9, prob = p_grid)) %>%  # compute likelihood at each value in grid
  mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
  mutate(posterior       = unstd_posterior / sum(unstd_posterior))

d %>%  # plot
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() + # add line to connect the dots
  labs(subtitle = "20 points",
       x = "probability of water",
       y = "posterior probability") +
  theme(panel.grid = element_blank())


# try plot with 5 points and compare these two plot
tibble(p_grid            = seq(from = 0, to = 1, length.out = 5), # we only change the quantity of observations
       prior             = 1) %>%
  mutate(likelihood      = dbinom(6, size = 9, prob = p_grid)) %>%
  mutate(unstd_posterior = likelihood * prior) %>%
  mutate(posterior       = unstd_posterior / sum(unstd_posterior)) %>% 
  
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "5 points",
       x = "probability of water",
       y = "posterior probability") +
  theme(panel.grid = element_blank())


## 2.4.2 Markov chain Monte Carlo
# install "rstan" package
# English ver: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
# Chinese ver: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started-(%E7%B9%81%E9%AB%94%E4%B8%AD%E6%96%87)

library(rstan)
rstan_options(auto_write = TRUE) # controls whether a compiled instance is written to the same directory
options(mc.cores = parallel::detectCores()) # if you are using rstan locally on a multicore machine and have plenty of RAM to estimate your model in parallel, at this point execute


# write a basic stan sample
stanmodel =  "
data { // declares the data to be used for the model
int n; // observations
int w; // success
}
parameters { // defines the unknowns to be estimate
real p;   
}
model {
// Define the priors
p ~ uniform(0, 1); 

// The likelihood
w ~ binomial(n, p);
}
"

n9_model = list(w = 6, n = 9)  # declare data
globe_qa_9 = stan(model_code = stanmodel, # stan program
                data = n9_model, # named list of data
                cores = 1, # number of cores (could use one per chain)
                chains = 2, # number of Markov chains
                warmup = 500, # number of warmup iterations per chain
                iter = 1000) # total number of iterations per chain
print(globe_qa_9)

# try 18 and 36 observations
n18_model = list(w = 6*2, n = 18)
globe_qa_18 = stan(model_code = stanmodel, data = n18_model, chains = 2, warmup = 500, iter = 1000)
print(globe_qa_18)

n36_model = list(w = 6*4, n = 36)
globe_qa_36 = stan(model_code = stanmodel, data = n36_model, chains = 2, warmup = 500, iter = 1000)
print(globe_qa_36)

# plot to check the difference
n_grid <- 100

tibble(p_grid                  = seq(from = 0, to = 1, length.out = n_grid) %>% rep(., times = 3),
       prior                   = 1,
       w                       = rep(c(6, 12, 24), each = n_grid),
       n                       = rep(c(9, 18, 36), each = n_grid),
       m                       = .65, # mean (stan model)
       s                       = rep(c(.14, .10, .07), each = n_grid)) %>% # sd of stan models (9, 18, 36)
  mutate(likelihood            = dbinom(w, size = n, prob = p_grid)) %>%
  mutate(unstd_grid_posterior  = likelihood * prior,
         unstd_quad_posterior  = dnorm(p_grid, m, s)) %>% 
  
  mutate(grid_posterior        = unstd_grid_posterior / sum(unstd_grid_posterior),
         quad_posterior        = unstd_quad_posterior / sum(unstd_quad_posterior),
         n = str_c("n = ", n)) %>% 
  mutate(n = factor(n, levels = c("n = 9", "n = 18", "n = 36"))) %>% 
  
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = grid_posterior)) +
  geom_line(aes(y = quad_posterior),
            color = "grey50") +
  labs(x = "proportion water",
       y = "density") +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ n, scales = "free")



# Chapter 3 tutorial

# 3.1 Sampling from a grid-like approximate posterior
rm(list=ls()) # clean environment
library(tidyverse) # open "tidyverse" package
library(tidybayes) # open "tidybayes" package

# Here we use grid approximation, again, to generate samples.
n <- 1000 # we want 1000 grid points
n_success <- 6
n_tirals  <- 9

d <-
  tibble(p_grid     = seq(from = 0, to = 1, length.out = n),
         prior      = 1) %>% # note we're still using a flat uniform prior
  mutate(likelihood = dbinom(n_success, size = n_tirals, prob = p_grid)) %>% 
  mutate(posterior  = likelihood * prior) %>% 
  mutate(posterior  = posterior / sum(posterior))


samples <- sample(d$p_grid, # randomly select from p_grid
                  prob = d$posterior, # probility of each p_grid
                  size = 1e4, # select 10,000 sample
                  replace = T) # sample with replacement

glimpse(samples) # get a glimpse of your data (class: vector)

# transform into tibble to plot
samples <-
  tibble(samples = sample(d$p_grid, prob = d$posterior, size = 1e4, replace = T)) %>% 
  mutate(sample_number = 1:n()) # n=n() means that a variable named n will be assigned the number of rows

# plot to see your sample
samples %>% 
  ggplot(aes(x = sample_number, y = samples)) +
  geom_line(size = 1/10) + 
  labs(x = "sample number",
       y = "proportion of water (p)")

samples %>% 
  ggplot(aes(x = samples)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) + # set the limit of coordinate system
  xlab("proportion of water (p)")


## 3.2.1 Intervals of defined boundaries
d %>% 
  filter(p_grid < .5) %>% 
  summarise(sum = sum(posterior)) # sum posterior of p_grid < .5

samples %>% 
  filter(samples < .5) %>% 
  summarise(sum = n() / 1e4)

samples %>% 
  filter(samples > .5 & samples < .75) %>%  
  summarise(sum = n() / 1e4) # about 61% of the posterior probability lies between 0.5 and 0.75.


## 3.2.2 Intervals of defined mass

d %>% # plot posterior density and fill the interval < 0.5
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = posterior)) +
  geom_ribbon(data = d %>% filter(p_grid < .5), 
              aes(ymin = 0, ymax = posterior)) + # fill the plot between ymin and ymax
  labs(x = "proportion of water (p)",
       y = "density")


d %>% # plot posterior density and fill the interval between .5 & .75
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = posterior)) +
  # note this next line is the only difference in code from the last plot
  geom_ribbon(data = d %>% filter(p_grid < .75 & p_grid > .5),
              aes(ymin = 0, ymax = posterior)) +
  labs(x = "proportion of water (p)",
       y = "density")

# three ways to check 0.8 quantile
# pull()
samples %>% 
  select(samples) %>% # select the column
  pull() %>% # transform the tibble into vector
  quantile(prob = .8)

# with quantile()
(q_80 <- quantile(samples$samples, prob = .8))

# with summarise()
samples %>% 
  summarise(`80th percentile` = quantile(samples, p = .8))


# Assigns 25% of the probability mass above and below the median point
median_qi(samples$samples, .width = .5) 

# find 50% highest posterior density interval (HPDI)
mode_hdi(samples$samples, .width = .5)

# update d data frame
n_success <- 3
n_tirals  <- 3
d <-
  d %>% 
  mutate(likelihood = dbinom(n_success, size = n_tirals, prob = p_grid)) %>% 
  mutate(posterior  = likelihood * prior) %>% 
  mutate(posterior  = posterior / sum(posterior))

# here's our new samples tibble
samples <- tibble(samples = sample(d$p_grid, prob = d$posterior, size = 1e4, replace = T))

# Plot 50% percentile interval
d %>% 
  ggplot(aes(x = p_grid)) +
  geom_ribbon(data = d %>% filter(p_grid > median_qi(samples$samples, .width = .5)[, "ymin"] & 
                                    p_grid < median_qi(samples$samples, .width = .5)[, "ymax"]),
              aes(ymin = 0, ymax = posterior),
              fill = "grey75") +
  geom_line(aes(y = posterior)) +
  labs(subtitle = "50% Percentile Interval",
       x = "proportion of water (p)",
       y = "density")

# Plot 50% HPDI
d %>% 
  ggplot(aes(x = p_grid)) +
  geom_ribbon(data = d %>% filter(p_grid > median_hdi(samples$samples, .width = .5)[, "ymin"] & 
                                    p_grid < median_hdi(samples$samples, .width = .5)[, "ymax"]),
              aes(ymin = 0, ymax = posterior),
              fill = "grey75") +
  geom_line(aes(y = posterior)) +
  labs(subtitle = "50% HPDI",
       x = "proportion of water (p)",
       y = "density")


# In tidybayes package, 
# you specify the measure of central tendency in the prefix (i.e., mean, median, or mode) 
# and then the type of interval you’d like (i.e., qi or hdi).
# There are mean_qi(), median_qi(), mode_hdi(), etc. commands.


## 3.2.3 Point estimate
# arrange() our d tibble in descending order by posterior
d %>% 
  arrange(desc(posterior))
# we can use slice() to select the top row
d %>% 
  arrange(desc(posterior)) %>% 
  slice(1)

# you can also do with with mode_hdi() or mode_qi()
samples %>% mode_hdi(samples)
samples %>% mode_qi(samples)

# medians and means are typical, too
samples %>% 
  summarise(mean   = mean(samples),
            median = median(samples))

# Loss Function
d %>% 
  mutate(loss = posterior * abs(0.5 - p_grid)) %>% # our guess is .5
  summarise(`expected loss` = sum(loss)) # expected loss 

# make a self-defined function to calculate our loss
make_loss <- function(our_d){ # input: our_d
  d %>% 
    mutate(loss = posterior * abs(our_d - p_grid)) %>% 
    summarise(weighted_average_loss = sum(loss))
}

# make a data frame (prepare to plot later)
l <-
  d %>% 
  select(p_grid) %>% 
  rename(decision = p_grid) %>%  # rename column
  mutate(weighted_average_loss = sapply(decision, make_loss)) %>% # calculate loss
  unnest() # change data type (list -> value)

# find minimun loss
min_loss <-
  l %>% 
  filter(weighted_average_loss == min(weighted_average_loss)) %>% 
  as.numeric() 
# when our guess = median, we'll have min expected loss 


# Plot to see expected loss
l %>%   
  ggplot(aes(x = decision)) +
  geom_ribbon(aes(ymin = 0, ymax = weighted_average_loss),
              fill = "grey75") + # fill plot
  geom_vline(xintercept = min_loss[1], color = "white", linetype = 3) + # add vertical line
  geom_hline(yintercept = min_loss[2], color = "white", linetype = 3) + # add horizontal line
  ylab("expected proportional loss") + # add ylad name
  theme(panel.grid = element_blank()) # eliminate grid line


# generate dummy data
n_draws <- 1e5 # 100,000 observations

set.seed(331)
d <- tibble(draws = rbinom(n_draws, size = 9, prob = .7))

d %>% 
  group_by(draws) %>% # group observations according to their draws
  count() %>% # count the number of occurences in each group
  mutate(proportion = n / nrow(d)) # calculate the proportion of draws

# plot the histogram
d %>% 
  ggplot(aes(x = draws)) + # set x-coordinate
  geom_histogram(binwidth = 1, # width of the bins
                 center = 0, # center of the bins
                 color = "grey92", # color of the bins
                 size = 1/10) + # size of the interval
  scale_x_continuous("dummy water count", # x-coordinate name
                     breaks = seq(from = 0, to = 9, by = 2)) + # mark
  ylab("frequency") + # y-coordinate name
  coord_cartesian(xlim = 0:9) + # set xlim form 0 to 9
  theme(panel.grid = element_blank()) # eliminate grid line


## 3.3.2 Model checking
# data simulation
n <- 1000
n_success <- 6
n_tirals  <- 9

d <-
  tibble(p_grid     = seq(from = 0, to = 1, length.out = n),
         prior      = 1) %>% 
  mutate(likelihood = dbinom(n_success, size = n_tirals, prob = p_grid)) %>% 
  mutate(posterior  = likelihood * prior) %>% 
  mutate(posterior  = posterior / sum(posterior))

# samples!
set.seed(33.22)
samples <-
  tibble(samples = sample(d$p_grid, prob = d$posterior, size = 1e4, replace = T)) 

head(samples)


# Plot to see sampling distribution
# the simulation
set.seed(3322)
samples <-
  samples %>% 
  mutate(w   = rbinom(1e4, size = n_tirals,  prob = samples),
         key = str_c("p = ", round(samples, digits = 1)))

# the plot
samples  %>% 
  filter(key != "p = 1") %>%
  ggplot(aes(x = w)) +
  geom_histogram(binwidth = 1, center = 0,
                 color = "grey92", size = 1/10) +
  scale_x_continuous("dummy water count",
                     breaks = seq(from = 0, to = 9, by = 3)) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:9) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ key, ncol = 9, scales = "free_y") # wrap 9 hist together


# 3.4 Let’s practice with stan

library(rstan)

# set up simple stan model
stanmodel =  "
data { // declares the data to be used for the model
int n; // observations
int w; // success
}
parameters { // defines the unknowns to be estimate
real p;   
}
model {
// Define the priors
p ~ uniform(0, 1); 

// The likelihood
w ~ binomial(n, p);
}
"

data_3.1 = list(w = 6, n = 9)  # declare data
model_3.1 = stan(model_code = stanmodel, # stan program
                 data = data_3.1, # named list of data
                 cores = 1, # number of cores (could use one per chain)
                 chains = 4, # number of Markov chains
                 warmup = 1000, # number of warmup iterations per chain
                 iter = 2000) # total number of iterations per chain
# print result
print(model_3.1)
fit_3.1 <- extract(model_3.1, pars="p", permuted = TRUE) %>% # extract the estimated probability of "w"
  as.tibble() %>% # make the data a tibble
  glimpse() # take a glimpse of the tibble

# plot the distribution
fit_3.1 %>% 
  ggplot(aes(x = p)) + # set the canvas 
  geom_density(fill = "grey50", color = "transparent") +
  scale_x_continuous("probability of water",
                     breaks = c(0, .5, 1)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "Posterior probability") +
  theme(panel.grid = element_blank())


# Use this distribution of probabilities to predict histograms of “w” counts
# the simulation
set.seed(33.22)

fit_3.1 <-
  fit_3.1 %>% 
  mutate(w   = rbinom(n(), size =  9,  prob = p)) %>% # compute "w"
  mutate(key = str_c("p = ", round(p, digits = 1))) # plot name

# the plot
fit_3.1  %>% 
  filter(key != "p = 1") %>% # filter out p = 1 
  ggplot(aes(x = w)) + # set canvas
  geom_histogram(binwidth = 1, center = 0, # plot hist.
                 color = "grey92", size = 1/10) +
  scale_x_continuous("dummy water count", # set x-coordinate
                     breaks = seq(from = 0, to = 9, by = 3)) +
  scale_y_continuous(NULL, breaks = NULL) + # set y-coordinate
  coord_cartesian(xlim = 0:9) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ key, ncol = 9, scales = "free_y") # wrap plots in one plot


# Plot posterior predictive distribution
fit_3.1 %>% 
  ggplot(aes(x = w)) +
  geom_histogram(binwidth = 1, center = 0,
                 color = "grey92", size = 1/10) +
  scale_x_continuous("number of water samples", # set x-coordinate
                     breaks = seq(from = 0, to = 9, by = 3)) +
  scale_y_continuous(NULL, breaks = NULL) + # set y-coordinate
  ggtitle("Posterior predictive distribution") +
  coord_cartesian(xlim = 0:9) +
  theme(panel.grid = element_blank())






