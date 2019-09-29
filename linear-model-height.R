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
        iter = 41000, warmup = 40000, chains = 4, cores = 4, seed = 4
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

# Put chains into data frames
post10 <- posterior_samples(b_10)
post50 <- posterior_samples(b_50)
post150 <- posterior_samples(b_150)
post352 <- posterior_samples(b_352)

# Function to plot data
plot_b <- function(df, samples, n) {
    df %>%
        slice(1:n) %>%
        ggplot(aes(weight, height)) +
            geom_abline(
                intercept = samples[1:20, 1],
                slope = samples[1:20, 2],
                size = 1/3,
                alpha = 1/3
            ) +
            geom_point() +
            coord_cartesian(
                xlim = range(df$weight),
                ylim = range(df$height)
            ) +
            theme(panel.grid = element_blank())
}

p10 <- plot_b(dat2, post10, 10)
p50 <- plot_b(dat2, post50, 50)
p150 <- plot_b(dat2, post150, 150)
p352 <- plot_b(dat2, post352, 352)

# Multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
        print(plots[[i]], vp = viewport(
            layout.pos.row = matchidx$row, 
            layout.pos.col = matchidx$col
        ))
    }
  }
}

multiplot(p10, p50, p150, p352, cols = 2)

# Plotting regression intervals and contours
mu_at_50 <- post %>%
    transmute(mu_at_50 = b_Intercept + b_weight * 50)

# Plot it
mu_at_50 %>%
    ggplot(aes(mu_at_50)) +
        geom_density(fill = '#333333') +
        labs(
            x = 'Mean Height | weight = 50'
        )

# Get HPDI 
mean_hdi(mu_at_50[, 1], .width = .95)

# Plot it
mu_at_50 %>%
    ggplot(aes(mu_at_50)) +
        geom_density(fill = '#999999') +
        stat_pointintervalh(
            aes(y = 0),
            point_interval = mode_hdi, 
            .width = .95
        ) +
        labs(
            x = 'Mean Height | weight = 50'
        )

# Extract model's fitted values
mu <- fitted(b4_3, summary= FALSE)

weight_seq <- tibble(
    weight = seq(25, 70, by = 1)
)

mu <- fitted(
        b4_3,
        summary = FALSE,
        newdata = weight_seq
    ) %>%
    as_tibble() %>%
    set_names(25:70) %>%
    mutate(iter = 1:n()) %>%
    gather(weight, height, -iter) %>%
    mutate(weight = as.numeric(weight))

dat2 %>% 
    ggplot(aes(weight, height)) +
    geom_point(
        data = mu %>% filter(iter < 101),
        alpha = 1/10
    )

# Plot a regression line with interval
mu_summary <- b4_3 %>%
    fitted(
        newdata = weight_seq
    ) %>%
    as_tibble() %>%
    bind_cols(weight_seq)

dat2 %>%
    ggplot(aes(weight, height)) +
        geom_smooth(
            data = mu_summary,
            aes(
                y = Estimate, 
                ymin = Q2.5,
                ymax = Q97.5
            ),
            stat = 'identity',
            fill = 'grey70',
            color = 'black', 
            alpha = 1, 
            size = 1/2
        ) +
        geom_point(alpha = 1/2) 

# Predictions intervals (incorporates standard deviation)
# The summary information in our data frame is for “simulated heights, 
# not distributions of plausible average height." These simulations 
# are the joint consequence of both μ and σ, unlike the results of 
# fitted(), which only reflect μ.
pred_height <- b4_3 %>%
    predict(newdata = weight_seq) %>%
    as_tibble() %>%
    bind_cols(weight_seq)

dat2 %>%
    ggplot(aes(weight)) +
        geom_ribbon(
            data = pred_height,
            aes(
                ymin = Q2.5,
                ymax = Q97.5
            ),
            fill = 'grey83',
            alpha = 1/2
        ) +
        geom_smooth(
            data = mu_summary,
            aes(
                y = Estimate,
                ymin = Q2.5,
                ymax = Q97.5
            ),
            stat = 'identity',
            fill = 'grey70',
            color = 'black', 
            size = 1/2
        ) + 
        geom_point(
            aes(y = height)
        )