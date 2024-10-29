require(ggplot2)
require(readr)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)

df_sim <- read_tsv(
  "../results/random_simulations-hd4.tsv",
  col_names = c("k", "h", "rho", "len", "bix", "d_true", "dist_llh")
) %>% filter(dist_llh < 0.5) %>% filter(dist_llh < 0.5 & dist_llh > 0.0001)
df_sim$dist_true <- df_sim$d_true / df_sim$len

df_sim %>% filter(d_true == 1) %>% 
  ggplot() + 
  facet_wrap(c("dist_true"), scale="free") +
  stat_bin(aes(x=dist_llh), bins = 30) +
  geom_vline(xintercept = mean((df_sim %>% filter(d_true == 1))$dist_llh)) + 
  theme_cowplot() +
  scale_x_continuous(n.breaks = 15)

df_sim %>%
  ggplot() +
  aes(y=dist_llh, x=dist_true) +
  geom_point(size = 0.2, alpha = 0.25) +
  stat_summary_bin(fun = mean, orientation = "x") +
  stat_function(fun = function(x) x) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + 0) +
  theme_cowplot() +
  labs(x = "True distance", y = "LLH estimate") # +
  # stat_smooth(method = "glm", formula = y ~ x) +
  # coord_cartesian(xlim = c(0, 0.25), ylim = c(0.9, 1.25))
  coord_cartesian(xlim = c(0.0, 0.05), ylim = c(0.0, 0.05))

df <- read_tsv(
  "../results/all_random_skims.out",
  col_names = c("dist_true", "read_id", "genome_id", "dist_llh")# "form", "match_count", "len", "rho", "genome_id", "dist_llh", "gamma")
)

df %>% filter(dist_true == 0.001) %>%
  ggplot() + 
  facet_wrap(c("dist_true"), scale="free") +
  stat_bin(aes(x=dist_llh), bins = 30) +
  theme_cowplot() +
  scale_x_continuous(n.breaks = 10) # +
  # scale_y_continuous(n.breaks = 10, trans="log2")

model <- lm((dist_true) ~ (dist_llh + I(dist_llh^2) + I(dist_llh^3) + 0), df_sim)
df$fit <- (predict(model, df))

df %>%  # filter(dist_true == 0.001) %>% 
  ggplot() + 
  # facet_wrap(c("dist_true"), scale="free") +
  stat_summary(fun = mean, aes(x=dist_true, y=dist_llh/dist_true)) +
  # stat_summary(fun = mean, aes(x=dist_true, y=fit/dist_true, color="Corrected")) +
  # stat_summary(fun = mean, aes(x=dist_true, y=(dist_llh)-dist_true)/dist_true)) +
  geom_hline(yintercept = 1) +
  # stat_bin(aes(dist_avg/dist_true)) +
  theme_cowplot() +
  # scale_x_continuous(trans="log2") +
  # scale_y_continuous(n.breaks = 10, trans="log2") +
  # coord_cartesian(xlim = c(0, 0.05)) +
  labs(y= "LLH estimation / True distance", x="True distance")

df %>%
  ggplot() +
  aes(y=dist_true, x=dist_llh) +
  geom_point(size = 0.2, alpha = 0.25) +
  stat_summary(fun = mean, orientation = "y")
