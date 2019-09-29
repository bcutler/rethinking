require(tidyverse)
require(brms)
require(tidybayes)

set.seed(4)

# Generate data for random walk
pos <- 
    replicate(100, runif(16, -1, 1)) %>%
    as_tibble() %>%
    rbind(0, .) %>% 
    mutate(step = 0:16) %>%
    gather(key, value, -step) %>%
    mutate(
        person = rep(1:100, each = 17)
    ) %>%
    group_by(person) %>%
    mutate(position = cumsum(value)) %>%
    ungroup()

# Plot it
pos %>%
    ggplot(aes(step, position, group = person)) +
    geom_line(aes(color = person < 2, alpha = person < 2)) +
    scale_color_manual(values = c('skyblue4', 'black')) +
    scale_alpha_manual(values = c(1/5, 1)) +
    theme(legend.position = 'none')
    