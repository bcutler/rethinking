require(tidyverse)
require(tidybayes)
require(brms)
require(ggthemes)
set.seed(4)

#### LOAD DATA
dat <- read.csv(file = '~/dev/rethinking/data/height-vs-weight.csv')
dat2 <- dat %>% filter(age >= 18) # filter for adults

#### MODEL

model <- brm(
        data = dat2, 
        family = gaussian,
        height ~ 1 + weight,
        prior = c(
            prior(normal(156, 100), class = Intercept),
            prior(normal(0, 10), class = b),
            prior(cauchy(0, 1), class = sigma)
        ),
        iter = 41000, warmup = 40000, chains = 4, cores = 4, seed = 4
    )

# Interpretation
posterior_summary(model)
plot(model)

# Create a data frame for predictions
weight_seq <- tibble(
    weight = seq(25, 70, by = 1)
)

# Get a distribution of plausable average heights
mu_summary <- model %>%
    fitted(
        newdata = weight_seq
    ) %>%
    as_tibble() %>%
    bind_cols(weight_seq)


# Get prediction intervals
# The summary information in our data frame is for “simulated heights, 
# not distributions of plausible average height." These simulations 
# are the joint consequence of both μ and σ, unlike the results of 
# fitted(), which only reflect μ.
pred_height <- model %>%
    predict(
        newdata = weight_seq
    ) %>%
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
            alpha = 1/4
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
            size = 1/4
        ) + 
        geom_point(
            aes(y = height),
            alpha = 1/2
        ) + 
        coord_cartesian(
            xlim = range(dat2$weight),
            ylim = range(dat2$height)
        ) +
        theme_tufte(
            base_family = 'Gill Sans',
            base_size = 16
        ) +
        labs(
            x = 'Weight',
            y = 'Height'
        )

ggsave(
    file = '~/dev/rethinking/plots/linear-model.png',
    height = 6,
    width = 6 * 1.618
)