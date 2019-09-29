require(rethinking)
data(Howell1)
set.seed(4)

# Load data
dat <- Howell1

# Swap brms package for rethinking
rm(Howell1)
detach(package:rethinking, unload = TRUE)
require(brms)
require(tidyverse)
require(tidybayes)

# Filter for adults
dat2 <- dat %>% filter(age >= 18)

# Get prior predictive distribution for Hi
n <- 1e4
tibble(
    sample_mu = rnorm(n, mean = 278, sd = 20),
    sample_sigma = runif(n, min = 0, max = 50)
) %>% 
    mutate(x = rnorm(n, mean = sample_mu, sd = sample_sigma)) %>%
    ggplot(aes(x)) + 
    geom_density(fill = 'black', size = 0) 

# Grid approximation of the posterior distribution
n <- 200

d_grid <- tibble(
        mu = seq(140, 160, length.out = n),
        sigma = seq(4, 9, length.out = n)
    ) %>%
    expand(mu, sigma)

grid_function <- function(mu, sigma) {
    dnorm(dat2$height, mean = mu, sd = sigma, log = TRUE) %>% sum()
}

d_grid2 <- d_grid %>%
    mutate(log_likelihood = map2(mu, sigma, grid_function)) %>%
    unnest() %>%
    mutate(
        prior_mu = dnorm(mu, mean = 278, sd = 20, log = TRUE),
        prior_sigma = dunif(sigma, min = 0, max = 50, log = TRUE)
    ) %>%
    mutate(
        product = log_likelihood + prior_mu + prior_sigma,
        probability = exp(product - max(product))
    )

# Contour plot
d_grid2 %>%
    ggplot(aes(x = mu, y = sigma)) + 
    geom_raster(aes(fill = probability)) +
    scale_fill_viridis_c(option = 'B' ) +
    theme_tufte(base_family = 'Gill Sans')

# Sampling the posterior
samples <- d_grid2 %>%
    sample_n(
        size = 1e4, 
        replace = TRUE,
        weight = probability
    )

# Scatter plot of mu and sigma 
samples %>%
    ggplot(aes(mu, sigma)) + 
    geom_jitter(alpha = 1/15) +
    scale_fill_viridis_c() 

# Plot the densities of mu and sigma
samples %>%
    select(mu, sigma) %>%
    gather() %>%
    ggplot(aes(value)) +
    geom_density(fill = '#333333') + 
    facet_wrap(~key, scales = 'free') 

# Compute posterior modes and 95% HPDIs
samples %>%
    select(mu, sigma) %>%
    gather() %>%
    group_by(key) %>%
    mode_hdi(value)

# Fitting the model with BRM()
b4_1 <-
    brm(
        data = dat2,
        family = gaussian,
        height ~ 1,
        prior = c(
                prior(normal(178, 20), class = Intercept),
                prior(cauchy(0, 1), class = sigma)
            ),
        iter = 31000,
        warmup = 30000,
        chains = 4,
        cores = 4,
        seed = 4
    )

plot(b4_1)
b4_1$fit

## ADDING A PREDICTOR
dat2 %>%
    ggplot(aes(weight, height)) + 
    geom_point(alpha = 1/2)

b4_2 <- brm(
        data = dat2, 
        family = gaussian,
        height ~ 1 + weight,
        prior = c(
            prior(normal(156, 100), class = Intercept),
            prior(normal(0, 10), class = b),
            prior(uniform(0, 50), class = sigma)
        ),
        iter = 41000,
        warmup = 40000,
        chains = 4,
        cores = 4,
        seed = 4
    )

# Interpretation
plot(b4_2)
posterior_summary(b4_2)


b4_3 <- brm(
        data = dat2, 
        family = gaussian,
        height ~ 1 + weight,
        prior = c(
            prior(normal(156, 100), class = Intercept),
            prior(normal(0, 10), class = b),
            prior(cauchy(0, 1), class = sigma)
        ),
        iter = 41000,
        warmup = 40000,
        chains = 4,
        cores = 4,
        seed = 4
    )

# Interpretation
plot(b4_3)
posterior_summary(b4_3)

# Check correlations
posterior_samples(b4_3) %>%
    select(-lp__) %>%
    cor() %>%
    round(digits = 2)


# Center weight
b4_4 <- brm(
        data = dat2 %>% mutate(weight_c = weight - mean(weight)), 
        family = gaussian,
        height ~ 1 + weight_c,
        prior = c(
            prior(normal(156, 100), class = Intercept),
            prior(normal(0, 10), class = b),
            prior(cauchy(0, 1), class = sigma)
        ),
        iter = 41000,
        warmup = 40000,
        chains = 4,
        cores = 4,
        seed = 4
    )

# Interpretation
plot(b4_4)
posterior_summary(b4_4)

# Check correlations
posterior_samples(b4_4) %>%
    select(-lp__) %>%
    cor() %>%
    round(digits = 2)

# Plot posterior against the data
dat2 %>%
    ggplot(aes(weight, height)) + 
    geom_abline(
        intercept = fixef(b4_3)[1], 
        slope = fixef(b4_3)[2]
    ) + 
    geom_point(alpha = 1/2) 

# Adding uncertainty around the mean
post <- posterior_samples(b4_3)

# Run model with different samples sizes
run_model <- function(df, n = 10) {
        brm(
            data = df %>% slice(1:n),
            family = gaussian,
            height ~ 1 + weight,
            prior = c(
                prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)
            ),
            iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 4
        )
}

b_10 <- run_model(dat2, n = 10)
b_50 <- run_model(dat2, n = 50)
b_150 <- run_model(dat2, n = 150)
b_352 <- run_model(dat2, n = 352)