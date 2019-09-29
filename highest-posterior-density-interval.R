require(tidyverse)
require(brms)
require(tidybayes)

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

# Central tendency summarization of distribution
median_qi(samples$p_grid, .width = c(.5, .8, .99))

# Highest posterior density interval
mode_hdi(samples$p_grid, .width = .5)
hdi <- hdi(samples$p_grid, .width = .5)

# Plot HPDI
dat %>%
    ggplot(aes(p_grid, posterior)) +
    geom_line() +
    geom_ribbon(
        data = dat %>% filter(p_grid > hdi[1] & p_grid < hdi[2]),
        aes(ymin = 0, ymax = posterior)
    ) +
    coord_cartesian(xlim = c(0, 1)) +
    labs(
        x = 'Proportion of Water',
        y = 'Density', 
        subtitle = '50% HPDI'
    )
