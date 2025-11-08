require(ggplot2)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)
require(vroom)
require(data.table)
require(ggnewscale)
require(scales)

df <- vroom("./../results/algorithmic_evaluation/all_simulations-results.tsv")
df <- df %>% mutate(ahigh = grepl("a22", QID, fixed = TRUE))

df %>% filter(!is.na(DIST_EST) & !ahigh) %>%
  group_by(QID, GND, ahigh) %>%
  summarise(
    DIST_EST_mean = mean(DIST_EST, na.rm = TRUE),
    DIST_TRUE_mean = mean(DIST_TRUE, na.rm = TRUE),
    DIST_TRUE_sd = sd(DIST_TRUE, na.rm = TRUE),
    DIST_EST_sd = sd(DIST_EST, na.rm = TRUE)
  ) %>%
  ggplot() +
  aes(x=DIST_TRUE_mean, y=DIST_EST_mean) +
  geom_abline(color="gray") +
  geom_point(size = 0.75) +
  theme_half_open() +
  background_grid() +
  labs(x="True mean HD", y="Estimated mean HD")
ggsave2("../figures/simulation-mean_HD-per_genome.pdf", width = 3, height = 3)

df %>% filter(!is.na(DIST_EST) & !ahigh) %>%
  group_by(QID, GND, ahigh) %>%
  summarise(
    DIST_EST_mean = mean(DIST_EST, na.rm = TRUE),
    DIST_TRUE_mean = mean(DIST_TRUE, na.rm = TRUE),
    DIST_TRUE_sd = sd(DIST_TRUE, na.rm = TRUE),
    DIST_EST_sd = sd(DIST_EST, na.rm = TRUE)
  ) %>%
  ggplot() +
  aes(x=DIST_TRUE_sd, y=DIST_EST_sd) +
  geom_abline(color="gray") +
  geom_point(size = 0.75) +
  coord_cartesian(xlim=c(0, 0.065), ylim=c(0, 0.065)) +
  theme_half_open() +
  background_grid() +
  labs(x="True std dev of HD", y="Estimated std dev of HD")
ggsave2("../figures/simulation-stddev_HD-per_genome.pdf", width = 3, height = 3)

sample_n(df, 1000000) %>% filter(!is.na(DIST_EST) & !ahigh) %>%
  ggplot() +
  aes(x=DIST_TRUE, y=DIST_EST) +
  geom_point(alpha = 0.005) +
  # stat_summary(geom="point") +
  stat_smooth(method = "lm", se = TRUE, color="red") +
  geom_abline() +
  # stat_smooth(aes(ymin = after_stat(y - 2 * se), ymax = after_stat(y + 2 * se)), geom = "ribbon", method = lm, fill = "grey60", alpha = .4) +
  coord_cartesian(xlim = c(0, 0.2), ylim = c(0, 0.2)) +
  theme_classic() +
  background_grid() +
  labs(x="True HD per read", y="Estimated HD per read")
ggsave2("../figures/simulation-HD-per_read.pdf", width = 3, height = 3)

sample_n(df, 100000) %>% filter(!is.na(DIST_EST) & !ahigh) %>%
  mutate(DIST_TRUE_BIN = cut(DIST_TRUE, c(0, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, Inf), include.lowest = TRUE)) %>%
  ggplot() +
  # facet_wrap(c("DIST_TRUE_BIN"), scales="free") +
  aes(x=DIST_TRUE_BIN, y=(DIST_EST-DIST_TRUE)/DIST_TRUE) +
  geom_boxplot(na.rm = TRUE, outliers = FALSE) +
  theme_half_open() +
  background_grid() + labs(x="True HD bin", y="Error per read") +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.99))
ggsave2("../figures/simulation-error_wrt_bin-per_read.pdf", width = 4, height = 3)

sample_n(df, 1000000) %>% filter(!is.na(DIST_EST) & !ahigh) %>%
  mutate(DIST_TRUE_BIN = round(DIST_TRUE*150)/150) %>%
  ggplot(aes(group=as.factor(DIST_TRUE_BIN))) +
  # facet_wrap(c("DIST_TRUE_BIN"), scales="free") +
  aes(x=DIST_TRUE_BIN, y=DIST_EST) +
  geom_boxplot(na.rm = TRUE, outliers = FALSE) +
  stat_summary()+
  theme_half_open(font_size = 18) +
  background_grid() +
  coord_cartesian(xlim = c(0, 30/150)) +
  geom_abline(color="red") +
  labs(x="True distance", y="Estimated distance")
ggsave2("../figures/simulation-mean_HD_wrt_bin-per_read.pdf", width = 4, height = 3)

sample_n(df, 1000000) %>% filter(!ahigh) %>%
  mutate(DIST_TRUE_BIN = cut(DIST_TRUE, c(0, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, Inf), include.lowest = TRUE)) %>%
  group_by(DIST_TRUE_BIN) %>%
  summarise(PERCENT_MAPPED = 1-sum(is.na(DIST_EST))/n()) %>%
  ggplot() +
  # facet_wrap(c("DIST_TRUE_BIN"), scales="free") +
  aes(x=DIST_TRUE_BIN, y=PERCENT_MAPPED) +
  stat_summary(geom = "col") +
  theme_half_open(font_size = 18) +
  background_grid() +
  labs(x="True distance", y="Mapping rate") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.99))
ggsave2("../figures/simulation-portion_mapped_per_bin-per_read.pdf", width = 4, height = 3)
