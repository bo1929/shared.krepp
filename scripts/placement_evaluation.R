require(ggplot2)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)
require(vroom)
require(data.table)
require(ggnewscale)
require(scales)
dfe1 <- vroom(
  "../results/ppmetrics-krepp-wrt_closest.tsv",
  col_names = c("query_id", "read_id", "placement", "dist_llh", "error", "mlogllh")
  )
dfe1$method <- "closest"
dfe2 <- vroom(
  "../results/ppmetrics-krepp-final.tsv",
  col_names = c("query_id", "read_id", "placement", "dist_llh", "error", "mlogllh")
)
dfe2$method <- "final"
dfe <- rbind(dfe1, dfe2)
dfl <- vroom("../results/selected_ppqueries_levels.tsv", col_names = c("query_id", "level"))
dfe <- merge(dfe, dfl, by = c("query_id"))
dfe$error[dfe$error == "None"] <- NaN
dfe$error[dfe$error == "NaN"] <- NaN
dfe$error <- as.numeric(dfe$error)
dfe %>% 
  # filter(error != "NaN") %>%
  ggplot() +
  aes(x = as.numeric(error), color = as.factor(method)) +
  facet_wrap(~ as.factor(level), scale = "free") +
  stat_ecdf() +
  coord_cartesian(xlim = c(0, 25)) +
  geom_hline(yintercept = 0.85, alpha = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.3, alpha = 0.5, linetype = 5) +
  geom_vline(xintercept = 5, alpha = 0.5, linetype = 3) +
  geom_vline(xintercept = 8, alpha = 0.5, linetype = 4) +
  # scale_x_continuous(transform = "log2") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal_grid()

dfe %>%
  # filter(dist_llh < 0.25) %>%
  ggplot() +
  facet_wrap(~as.factor(level), scale="free_y") +
  aes(x = method, y = error, color = method) +
  geom_boxplot(outliers = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(transform = "log1p", breaks = c(0, 5, 10, 25, 50)) +
  theme_minimal_grid() +
  theme(axis.text.x = element_blank())

dfe %>%
  filter(dist_llh < 0.25) %>%
  ggplot() +
  aes(x = as.factor(level), y = error, color = method) +
  stat_summary(aes(shape = "mean"), fun = mean) +
  stat_summary(aes(group = method), fun = mean, geom = "line") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  theme_minimal_grid()

dfe %>%
  # filter(dist_llh < 0.25) %>%
  ggplot() +
  aes(x = dist_llh, y = error, color = method) +
  stat_summary_bin(aes(shape = "mean"), fun = mean, bins = 20) +
  stat_summary_bin(aes(group = method), fun = mean, geom = "line", bins = 20) +
  theme_minimal_grid() +
  scale_color_brewer(palette = "Dark2") # +
  scale_y_continuous(transform = "log1p")

dfe %>%
  filter(dist_llh < 0.2) %>%
  group_by(query_id, level, method) %>%
  summarise(error_mean = mean(error, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(query_id, level), values_from = error_mean, names_from = method) %>%
  ggplot() +
  aes(x = as.factor(level), y = closest-final) +
  stat_summary(fun = mean) +
  stat_summary(fun = mean, geom = "line") +
  geom_point(alpha = 0.25) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  # scale_y_continuous(transform = "log1p") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal_grid()

dfe %>%
  filter(dist_llh < 0.2) %>%
  group_by(query_id, level, method) %>%
  summarise(error_mean = mean(error, na.rm = TRUE)) %>%
  # pivot_wider(id_cols = c(query_id, level), values_from = error_mean, names_from = method) %>%
  ggplot() +
  aes(x = as.factor(level), y = error_mean, color = method) +
  stat_summary(fun = mean) +
  stat_summary(fun = mean, geom = "line") +
  geom_point(alpha = 0.25) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  # scale_y_continuous(transform = "log1p") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal_grid()
# Percolation theory?
dfe %>%
  group_by(query_id, level, method) %>%
  summarise(portion_placed = 1 - sum(is.na(error) | dist_llh > 0.2) / n()) %>%
  ggplot() +
  aes(x = as.factor(level),
      y = portion_placed,
      color = method) +
  stat_summary(fun = mean) +
  stat_summary(aes(group = method), fun = mean, geom = "line") +
  scale_color_brewer(palette = "Dark2") +
  geom_point(alpha = 0.5) + theme_minimal_grid()

dfe %>%
  group_by(query_id, level, method) %>%
  summarise(portion_placed = sum(!is.na(error) & error < 2) / n()) %>%
  ggplot() +
  aes(x = as.factor(level),
      y = portion_placed,
      color = method) +
  stat_summary(fun = mean) +
  stat_summary(aes(group = method), fun = mean, geom = "line") +
  scale_color_brewer(palette = "Dark2") +
  geom_point(alpha = 0.5) + theme_minimal_grid()

dfe <- vroom(
  "../results/ppmetrics-krepp16S-wrt_final.tsv",
  col_names = c("query_id", "read_id", "placement", "dist_llh", "error", "mlogllh")
)
dfe %>% 
  # group_by(query_id) %>%
  summarise(portion_placed = sum(!is.na(error) & dist_llh < 0.1) / n(),
            mean_error = mean(error[dist_llh < 0.1], na.rm = TRUE)) %>%
  ggplot() +
  aes(x =1) + # reorder(query_id, mean_error)) +
  # geom_point(aes(y=mean_error), color="blue") +
  geom_point(aes(y=portion_placed), color="red") +
  theme_minimal_grid() + theme(axis.text.x = element_text(angle=90))
dfe %>%
  group_by(query_id) %>%
  summarise(portion_placed = sum(!is.na(error) & dist_llh < 0.1) / n()) %>%
  ggplot() +
  aes(x = reorder(query_id, portion_placed),
      y = portion_placed) +
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  geom_point(alpha = 0.5) + theme_minimal_grid() + theme(axis.text.x = element_text(angle=90))
