require(ggplot2)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)
require(vroom)
require(data.table)
require(ggnewscale)
require(scales)

# WoL-v2: Node error
dfl <- vroom("../misc_data/data-WoLv2_placement/query_selection/selected_queries-mindist.tsv", col_names = c("qid", "novelty"))
dfe <- vroom(
  "../results/placement_comparison/ppmetrics-heuristic_comparison.tsv",
  col_names = c("method", "qid", "rid", "placement", "dist", "filter", "error")
)
dfe <- merge(dfe, dfl, by = c("qid"))
dfe <- dfe %>% mutate(error = ifelse(is.na(error) | is.infinite((error)), NaN, error))
dfe <- dfe %>% mutate(method = ifelse(method == "krepp-mavgpp", "krepp", method))

dist_th <- 0.2
dfe$filter[(dfe$dist < dist_th) & (dfe$method == "bowtie-closest")] <- "True"

dfe %>%
  filter(!method %in% c("krepp-LCA") & novelty < 12) %>%
  mutate(
    error = ifelse((placement != "N1" & (filter == "True" & !is.na(placement))), error, NaN),
    novelty_bin = cut(novelty, c(0, 0.5, 1.0, 2.0, 4, 8, 12, Inf))
  ) %>%
  group_by(novelty_bin, method) %>%
  summarise(
    # root_placement_ratio = sum(placement == "N1")/n(),
    median_error = median(error, na.rm = TRUE),
    mean_error = mean(error, na.rm = TRUE),
    portion_placed = 1 - sum(is.na(error)) / n(),
    exactly_placed = sum(!is.na(error) & error == 0, rm.na = TRUE) / n()
  ) %>%
  ggplot() +
  aes(x = portion_placed, color = method, y = mean_error) +
  geom_line(aes(group = novelty_bin), linetype = 1, size = 0.5, color = "grey20") +
  geom_point(aes(), alpha = 0.85, size = 4) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#ff7f00")) +
  labs(y = "Mean node error", x = "Placed reads", color = "Method") +
  scale_x_continuous(labels = percent) +
  theme_half_open() +
  background_grid() +
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave("../figures/placement_WoLv2-x_percent_placed-y_mean_node_error.pdf", width = 5, height = 4)


dfe %>%
  filter(!method %in% c("krepp-LCA") & novelty < 12) %>%
  mutate(
    error = ifelse((placement != "N1" & (filter == "True" & !is.na(placement))), error, NaN),
    novelty_bin = cut_number(novelty, 10)
  ) %>%
  group_by(novelty_bin, method) %>%
  summarise(
    # root_placement_ratio = sum(placement == "N1")/n(),
    median_error = median(error, na.rm = TRUE),
    mean_error = mean(error, na.rm = TRUE),
    portion_placed = 1 - sum(is.na(error)) / n(),
    exactly_placed = sum(!is.na(error) & error == 0, rm.na = TRUE) / n()
  ) %>% #portion_placed
  ggplot() +
  aes(x = novelty_bin, color = method, y = mean_error, size=portion_placed) +
  geom_line(aes(group = method), linetype = 1, size = 0.5, color = "grey20") +
  geom_point(aes(), alpha = 0.85) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#ff7f00")) +
  labs(y = "Mean node error", x = "Placed reads", color = "Method") +
  scale_x_discrete() +
  theme_half_open() +
  background_grid() +
  scale_size_area(name="placed",labels=percent)+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle=30))

dfe %>%
  filter(!method %in% c("krepp-LCA") & novelty < 12) %>%
  mutate(
    error = ifelse((placement != "N1" & (filter == "True" & !is.na(placement))), error, NaN),
    novelty_bin = cut(novelty, c(0, 0.5, 1.0, 2.0, 4, 8, 12, Inf))
  ) %>%
  group_by(novelty_bin, method) %>%
  summarise(
    median_error = median(error, na.rm = TRUE),
    mean_error = mean(error, na.rm = TRUE),
    portion_placed = 1 - sum(is.na(error)) / n()
  ) %>%
  ggplot() +
  aes(x = portion_placed, color = method, y = mean_error, shape = novelty_bin) +
  geom_line(aes(group = novelty_bin), linetype = 1, size = 0.5, color = "grey20") +
  geom_point(aes(), alpha = 0.85, size = 5) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#ff7f00")) +
  labs(y = "Mean node error", x = "% placed reads", color = "Method", shape = "Novelty") +
  scale_x_continuous(labels = percent) +
  scale_shape_manual(values = c(7, 8, 10, 15, 16, 17)) +
  theme_half_open() +
  background_grid() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.66))

dfe %>%
  filter(!method %in% c("krepp-LCA") & novelty < 12) %>%
  mutate(
    error = ifelse((placement != "N1" & (filter == "True" & !is.na(placement))), error, NaN),
    novelty_bin = cut(novelty, c(0, 0.5, 1.0, 2.0, 4, 8, 12, Inf))
  ) %>%
  ggplot() +
  aes(x = novelty_bin, fill = method, , y = error) +
  # geom_line(aes(group=novelty_bin), linetype=1,size=0.5,color="grey20")+
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 10)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_fill_manual(values = c("#377eb8", "#e41a1c", "#ff7f00")) +
  labs(y = "Node error", x = "Novelty bin", fill = "Method") +
  theme_half_open() +
  background_grid()
ggsave("../figures/placement_WoLv2-boxplot.pdf", width = 5.5, height = 4)

dfe %>%
  filter(method %in% c("krepp", "krepp-LCA") & novelty < 12) %>%
  mutate(
    error = ifelse((placement != "N1" & (filter == "True" & !is.na(placement))), error, NaN),
    novelty_bin = cut(novelty, c(0, 0.5, 1.0, 2.0, 4, 8, 12, Inf))
  ) %>%
  ggplot() +
  aes(x = novelty_bin, fill = method, , y = error) +
  # geom_line(aes(group=novelty_bin), linetype=1,size=0.5,color="grey20")+
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 10)) +
  scale_fill_manual(values = c("#e41a1c", "#fb9a99")) +
  labs(y = "Median node error", x = "Novelty bin", fill = "Method") +
  theme_half_open() +
  background_grid()
ggsave("../figures/placement_WoLv2-boxplot-LCA_comparison.pdf", width = 5.5, height = 4)

dfe %>%
  filter(method %in% c("krepp", "krepp-LCA") & novelty < 12) %>%
  mutate(
    error = ifelse((placement != "N1" & (filter == "True" & !is.na(placement))), error, NaN),
    novelty_bin = cut(novelty, c(0, 0.5, 1.0, 2.0, 4, 8, 12, Inf))
  ) %>%
  group_by(novelty_bin, method) %>%
  summarise(
    median_error = median(error, na.rm = TRUE),
    mean_error = mean(error, na.rm = TRUE),
    portion_placed = 1 - sum(is.na(error)) / n()
  ) %>%
  ggplot() +
  aes(x = portion_placed, color = method, y = mean_error, shape = novelty_bin) +
  geom_line(aes(group = novelty_bin), linetype = 1, size = 0.5, color = "grey20") +
  geom_point(aes(), alpha = 0.85, size = 5) +
  scale_color_manual(values = c("#e41a1c", "#fb9a99")) +
  labs(y = "Mean node error", x = "% placed reads", color = "Method", shape = "Novelty") +
  scale_x_continuous(labels = percent) +
  scale_shape_manual(values = c(7, 8, 10, 15, 16, 17)) +
  theme_half_open() +
  background_grid() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.66))

dfe %>%
  filter(method %in% c("krepp", "krepp-LCA") & novelty < 12) %>%
  mutate(
    error = ifelse((placement != "N1" & (filter == "True" & !is.na(placement))), error, NaN),
    novelty_bin = cut(novelty, c(0, 0.5, 1.0, 2.0, 4, 8, 12, Inf))
  ) %>%
  group_by(novelty_bin, method) %>%
  summarise(median_error = median(error, na.rm = TRUE), mean_error = mean(error, na.rm = TRUE), portion_placed = 1 - sum(is.na(error)) / n()) %>%
  ggplot() +
  aes(x = portion_placed, color = method, y = mean_error) +
  geom_line(aes(group = novelty_bin), linetype = 1, size = 0.5, color = "grey20") +
  geom_point(aes(), alpha = 0.85, size = 5) +
  scale_color_manual(values = c("#e41a1c", "#fb9a99")) +
  labs(y = "Mean node error", x = "Placed reads", color = "Method") +
  scale_x_continuous(labels = percent) +
  scale_shape_manual(values = c(7, 8, 10, 15, 16, 17)) +
  theme_half_open() +
  background_grid()
ggsave("../figures/placement_WoLv2-x_percent_placed-y_mean_node_error-LCA_comparison.pdf", width = 5.5, height = 4.5)

dfe %>%
  filter(!method %in% c("krepp-LCA") & novelty < 12) %>%
  mutate(
    error = ifelse((is.na(placement)) | is.na(error), max(error, na.rm = TRUE), error),
    novelty_bin = cut(novelty, c(0, 2, 4, 8, 12))
  ) %>%
  ggplot() +
  aes(x = error, color = method, fill = method) + # , linetype = method) +
  facet_wrap(~novelty_bin, nrow = 2) +
  labs(x = "Error", y = "ECDF", color = "") +
  # scale_linetype_manual(values = c(4,5,6)) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#ff7f00", "#fb9a99")) +
  stat_ecdf(alpha = 0.75, size = 0.85) +
  coord_cartesian(xlim = c(0, 8)) +
  theme_half_open() +
  background_grid() +
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave("../figures/placement_WoLv2-ecdf.pdf", width = 4.5, height = 4)


# WoL-v1 16S: comparison with EPA-ng
dfs_gw <- vroom(
  "../results/placement_comparison/ppmetrics_queries16S-all.tsv",
  col_names = c("method", "qid", "rid", "placement", "dist", "filter", "error")
)
dfs_reads <- vroom(
  "../results/placement_comparison/ppmetrics_reads16S-all.tsv",
  col_names = c("method", "qid", "rid", "placement", "dist", "filter", "error")
)
dfg <- vroom(
  "../misc_data/data-WoLv1_placement/errors16S-EPAng.txt"
)
dfg <- separate_wider_delim(dfg, cols = id, delim = "_", names = c("qid", "i1", "i2"))
dfd <- vroom("../misc_data/data-WoLv1_placement/all_mindist.tsv", col_names = c("qid", "novelty"))
dfs <- merge(dfs_gw, dfd, by = c("qid"))
# dfs <- merge(dfs_reads, dfd, by = c("qid"))
dfg <- merge(dfg, dfd, by = c("qid"))

dfg %>%filter(novelty <= 11.0030184) %>% 
  ggplot() + aes(x=reorder(qid, novelty), y=novelty) +
  geom_point(size=0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, size = 4.5)) +
  labs(y="Distance to the closest reference (path length)", x="Query ID")
ggsave2("../figures/distance_to_closest-WoLv1.pdf", width = 6, height = 4)

dfm <- rbind(
  dfs_gw %>% mutate(
    error = ifelse((placement != "N1" & (filter == "True" & (!is.na(placement)))), error, NaN),
  ) %>% select(qid, error) %>% mutate(method = "krepp"),
  dfg %>% select(qid, error) %>% mutate(method = "EPA-ng")
)
dfm <- merge(dfm, dfd, by = c("qid"))

dfg %>% ggplot() +
  aes(x = novelty, y = error) +
  geom_point() +
  theme_cowplot() +
  labs(x = "Novelty", "Error")

dfs %>%
  mutate(
    error = ifelse((is.na(placement)) | is.na(error), max(error, na.rm = TRUE), error),
  ) %>%
  ggplot() +
  aes(x = error) +
  labs(x = "Error", y = "ECDF") +
  stat_ecdf() +
  coord_cartesian(xlim = c(0, 8)) +
  theme_cowplot()

dfs %>%
  mutate(
    error = ifelse((placement != "N1" & (filter == "True" & !is.na(placement))), error, NaN),
    novelty_bin = cut(novelty, c(0, 0.5, 1, 2, 4, 6, 8, 10, Inf))
  ) %>%
  group_by(novelty_bin) %>%
  summarise(median_error = median(error, na.rm = TRUE), mean_error = mean(error, na.rm = TRUE), portion_placed = 1 - sum(is.na(error)) / n()) %>%
  ggplot() +
  aes(x = novelty_bin) +
  geom_line(aes(y = median_error, group = 1), size = 0.5, color = "darkgray") +
  geom_line(aes(y = mean_error, group = 2), size = 0.5, color = "darkgray") +
  geom_point(aes(color = "median", y = median_error, group = 1), size = 3) +
  geom_point(aes(color = "mean", y = mean_error, group = 2), size = 3) +
  scale_color_manual(values = c("darkred", "darkblue")) +
  theme_half_open() +
  theme(axis.text.x = element_text(angle = 90)) +
  background_grid() +
  labs(y = "Node error", x = "Novelty bin", color = "")
ggsave("../figures/query_16S-krepp_mean_median_error.pdf", width = 4.5, height = 3.5)

dfs %>%
  mutate(
    error = ifelse((placement != "N1" & (filter == "True" & !is.na(placement))), error, NaN),
    novelty_bin = cut(novelty, c(0, 0.5, 1, 2, 4, 6, 8, 10, Inf))
  ) %>%
  group_by(novelty_bin) %>%
  summarise(median_error = median(error, na.rm = TRUE), mean_error = mean(error, na.rm = TRUE), portion_placed = 1 - sum(is.na(error)) / n()) %>%
  ggplot(aes(y = portion_placed)) +
  aes(x = novelty_bin) +
  geom_point(size = 3) +
  geom_line(group = 1) +
  scale_y_continuous(labels = percent) +
  theme_half_open() +
  background_grid() +
  labs(y = "Placed reads", x = "Novelty bin") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.66))
ggsave("../figures/query_16S-krepp_percent_placed.pdf", width = 4.5, height = 3.5)

dfm %>%
  filter(novelty <= 11.0030184) %>%
  ggplot() +
  aes(x = novelty) +
  stat_summary_bin(aes(width=0.5, y = error, fill = method), color="black", alpha = 0.925, binwidth = 1.25, geom="bar",  width = 0.5, position = position_dodge()) +
  scale_fill_manual(values = c("#4daf4a", "#e41a1c")) +
  theme_half_open() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_half_open() +
  background_grid() +
  labs(y = "Mean node error", x = "Novelty", color = "Method") +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.8))
ggsave("../figures/query_16S-x_novelty-y_mean_node_error.pdf", width = 4.5, height = 3.5)

dfm %>% filter(novelty <= 11.0030184) %>%
  mutate(
    novelty_bin = cut(novelty, c(0, 1, 2, 3, 4, 6, 8, 10, 12, Inf))
  ) %>%
  group_by(method, novelty_bin) %>%
  summarise(median_error = median(error, na.rm = TRUE), mean_error = mean(error, na.rm = TRUE), portion_placed = 1 - sum(is.na(error)) / n()) %>%
  ggplot() + aes(x = novelty_bin, color=method) +
    # geom_line(aes(y = median_error), size = 0.5) +
    # geom_line(aes(y = mean_error), size = 0.5) +
    # geom_point(aes(y = median_error), size = 3) +
    geom_point(aes(y = mean_error), size = 3) +
    # scale_color_manual(values = c("darkred", "darkblue")) +
    theme_half_open() +
    theme(axis.text.x = element_text(angle = 90)) +
    background_grid() +
    labs(y = "Node error", x = "Novelty bin", color = "")

dfm %>%
  filter(novelty <= 11.0030184) %>%
  mutate(error = ifelse(!is.na(error), error, max(error, na.rm = TRUE)), novelty_bin=cut(novelty, c(0, 2, 4, 8, Inf))) %>%
  ggplot() +
  facet_wrap(~novelty_bin) +
  stat_ecdf(aes(x = error, color = method, linetype = cut(novelty, c(0, 2, 4, 8, 12))), alpha = 0.925, linewidth=1) +
  scale_color_manual(values = c("#4daf4a", "#e41a1c")) +
  coord_cartesian(xlim = c(0, 15)) +
  theme_half_open() +
  background_grid() +
  labs(y = "ECDF", x = "Node error", color = "Method", linetype = "Novelty")
ggsave("../figures/query_16S-error_ecdf-with_unplaced_without_filtering.pdf", width = 4.5, height = 3.5)

dfm %>%
  filter(novelty <= 11.0030184) %>%
  ggplot() +
  stat_density(aes(x = error, fill = method), position=position_nudge(), alpha = 0.25, bw = 2) +
  scale_fill_manual(values = c("#4daf4a", "#e41a1c")) +
  coord_cartesian(xlim = c(0, 15)) +
  theme_half_open() +
  background_grid()

dfm %>%
  filter(novelty <= 11.0030184) %>%
  ggplot() +
  facet_wrap(~method) +
  stat_density(aes(x = error, fill = method), bw=2) +
  scale_fill_manual(values = c("#4daf4a", "#e41a1c")) +
  theme_half_open() +
  background_grid()
dfm %>%
  filter(novelty <= 11.0030184) %>%
  ggplot() +
  geom_boxplot(aes(x = method, y=error, fill = method), outliers = F) +
  stat_summary(aes(x = method, y=error)) +
  scale_fill_manual(values = c("#4daf4a", "#e41a1c")) +
  theme_half_open() +
  background_grid()

dfm %>%
  filter(novelty <= 11.0030184) %>%
  ggplot() + aes(x=1, y=error, fill = method) +
  stat_summary(geom="col", position = position_dodge2()) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge2()) + 
  scale_fill_manual(values = c("#4daf4a", "#e41a1c")) +
  theme_half_open() +
  background_grid()

dfm %>%
  filter(novelty <= 11.0030184) %>%
  mutate(is_placed = !is.na(error)) %>%
  group_by(method) %>%
  summarize(
    portion_placed = sum(is_placed)/n(), 
    # count_placed=sum(is_placed),
    portion_unplaced = 1-sum(is_placed)/n()
    # count_unplaced=sum(!is_placed)
  ) %>% pivot_longer(!c(method), names_to = c("type", "name"), names_sep = "_") %>%
  ggplot() +
  aes(x=method, y=value, fill=reorder(interaction(method, name), value)) +
  # facet_wrap(~type, scale="free") +
  geom_col(color="black", position = position_stack()) +
  scale_fill_manual(values = c("#4daf4a", "#401d1d", "#e41a1c",  "#4daf4a")) +
  theme_half_open() +
  background_grid() +
  scale_y_continuous(labels = percent) +
  labs(y = "Placement rate") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  guides(fill="none")

dfm %>%
  filter(novelty <= 11.0030184) %>%
  mutate(novelty_bin=cut(novelty, c(0, 2, 4, 8, Inf))) %>%
  ggplot() +
  facet_wrap(~novelty_bin) +
  # stat_ecdf(aes(x = error, color = method, linetype = cut(novelty, c(0, 5, Inf))), alpha = 0.925) +
  stat_ecdf(aes(x = error, color = method, alpha = 0.925) +
  scale_color_manual(values = c("#4daf4a", "#e41a1c")) +
  coord_cartesian(xlim = c(0, 25)) +
  theme_half_open() +
  background_grid() +
  labs(y = "ECDF", x = "Node error", color = "Method", linetype = "Novelty")
ggsave("../figures/query_16S-error_ecdf-only_placed_with_filtering.pdf", width = 4.5, height = 3.5)


df_appspam <- vroom("./../results/expt-appspam_comparison/appspam_comparison.tsv")

df_appspam %>%
  ggplot() +
  aes(y=reorder(reference,percent_of_correct) , x=percent_of_correct, fill=method)+
  geom_col(position = position_dodge2(), color="black") +
  theme_half_open(font_size = 16) +
  background_grid() +
  scale_x_continuous(name="% of correct placement") +
  labs(y="Taxon") +
  scale_fill_manual(values=c("#66c2a5", "#e41a1c")) +
  theme(legend.title = element_blank())
ggsave2("../figures/appspam_comparison-percent_correct.pdf", width = 5.5, height = 3)


df_appspam %>%
  ggplot() +
  aes(y=reorder(reference,percent_of_correct) , x=avg_node_error, fill=method)+
  geom_col(position = position_dodge2(), color="black") +
    theme_half_open(font_size = 16) +
      background_grid() +
  scale_x_continuous(name="Edge error (average)") +
  labs(y="Taxon") +
  scale_fill_manual(values=c("#66c2a5", "#e41a1c")) +
  theme(legend.title = element_blank())
ggsave2("../figures/appspam_comparison-avg_node_error.pdf", width = 5.5, height = 3)


df_appspam %>%
  ggplot() +
  aes(y=reorder(reference,percent_of_correct) , x=running_time_s, fill=method)+
  geom_col(position = position_dodge2(), color="black") +
    theme_half_open(font_size = 16) +
  background_grid() +
  scale_x_continuous(name="Running time (seconds)") +
  labs(y="Taxon") +
  scale_fill_manual(values=c("#66c2a5", "#e41a1c")) +
  theme(legend.title = element_blank())
ggsave2("../figures/appspam_comparison-running_time_s.pdf", width = 5.5, height = 3)


df_appspam %>%
  ggplot() +
  aes(y=reorder(reference,percent_of_correct) , x=memory_gb, fill=method)+
  geom_col(position = position_dodge2(), color="black") +
    theme_half_open(font_size = 16) +
  background_grid() +
  scale_x_continuous(name="Peak memory (GB)") +
  labs(y="Taxon") +
  scale_fill_manual(values=c("#66c2a5", "#e41a1c")) +
  theme(legend.title = element_blank())
ggsave2("../figures/appspam_comparison-memory_gb.pdf", width = 5.5, height = 3)