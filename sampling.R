require(tidyverse)
require(brms)

n <- 1001
n_success <- 6
n_trails <- 9

dat <- tibble(
        p_grid = seq(from = 0,  to = 1, length.out = n),
        prior = 1
    ) %>%
    mutate(
        likelihood = dbinom(n_success, size = n_trails, p = p_grid)
    ) %>%
    mutate(
        posterior = (likelihood * prior) / sum(likelihood * prior)
    )
    

n_samples <- 1e4
set.seed(3)

samples <- dat %>%
    sample_n(
        size = n_samples,
        weight = posterior, 
        replace = TRUE
    ) 

# Plot the proportion of water in the samples
samples %>%
    mutate(sample_number = 1:n()) %>%
    ggplot(aes(sample_number, p_grid)) +
    geom_line(size = .1) +
    labs(
        x = 'Sample Number',
        y = 'Proportion of Water'
    )

# Create a density plot
samples %>%
    ggplot(aes(p_grid)) + 
    geom_density(fill = 'black') + 
    coord_cartesian(xlim = 0:1) +
    labs(
        x = 'Proportion of Water',
        y = 'Density'
    )

####  SAMPLING TO SUMMARIZE

# Get the proportion of water less than some value from model
dat %>%
    filter(p_grid < 0.5) %>%
    summarise(sum = sum(posterior))

# From samples
samples %>%
    filter(p_grid < .5) %>%
    summarise(sum = n() / n_samples)

# Get range from sample
samples %>%
    filter(p_grid > .5 & p_grid < .75) %>%
    summarise(sum = n() / n_samples)

# Visualize
dat %>%
    ggplot(aes(x = p_grid, y = posterior)) + 
    geom_line() +
    geom_ribbon(
        data = dat %>% filter(p_grid > .5 & p_grid < .75),
        aes(ymin = 0, ymax = posterior)
    ) + 
    labs(
        x = 'Proportion of Water',
        y = 'Density'
    )



# Get 80th percentile 

q80 <- quantile(samples$p_grid, prob = .8)
q10_q90 <- quantile(samples$p_grid, prob = c(.1, .9))

# OR

q80 <- samples %>%
    select(p_grid) %>%
    pull() %>%
    quantile(prob = .8)


dat %>%
    ggplot(aes(p_grid, posterior)) +
    geom_line() +
    geom_ribbon(
        data = dat %>% filter(p_grid > q10_q90[1] & p_grid < q10_q90[2]),
        aes(ymin = 0, ymax = posterior)
    ) +
    labs(
        x = 'Proportion of Water',
        y = 'Density'
    )
