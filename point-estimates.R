require(tidyverse)
require(brms)
require(tidybayes)

n <- 1001
n_success <- 3
n_trails <- 3

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

samples <- dat %>%
    sample_n(
        size = n_samples,
        weight = posterior, 
        replace = TRUE
    ) 

# Get point estimate
dat %>%
    arrange(desc(posterior)) %>%
    slice(1)

samples %>% mode_hdi(p_grid)
samples %>% mode_qi(p_grid)
samples$p_grid %>% Mode()

samples %>%
    summarise(
        mean = mean(p_grid),
        median = median(p_grid)
    )

# Clean point estimates
point_estimates <- bind_rows(
    samples %>% mean_qi(p_grid),
    samples %>% median_qi(p_grid),
    samples %>% mode_qi(p_grid)
) %>%
select(p_grid, .point) %>%
mutate( 
    x = p_grid + c(-.03, .03, -0.03),
    y = c(.1, .24, .4)
)


# Plot HPDI with point estimates
dat %>%
    ggplot(aes(p_grid, likelihood)) +
    geom_line() +
    geom_ribbon(
        aes(ymin = 0, ymax = likelihood),
        fill = 'grey75'
    ) +
    geom_vline(xintercept = point_estimates$p_grid) +
    geom_text(
        data = point_estimates, 
        aes(x = x, y = y, label = .point),
        angle = 90
    ) +
    coord_cartesian(xlim = c(0, 1)) +
    labs(
        x = 'Proportion of Water',
        y = 'Density', 
        subtitle = '50% HPDI'
    )