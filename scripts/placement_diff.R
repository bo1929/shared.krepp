require(ggplot2)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)
require(vroom)
require(data.table)
require(ggnewscale)
require(scales)
require(qiime2R)
require(plotly)
require(reticulate)
require(Cairo)


df <- vroom("../results/placement_comparison/placement_diff.tsv")

df %>%
  filter(max_dist <= 0.05) %>%
  ggplot() +
  aes(x=cut(max_dist, c(0, 0.005, 0.01, 0.015, 0.02, 0.025, 0.03, 0.035, 0.04, 0.45, 0.05), include.lowest = TRUE), y=max_blen) +
  stat_summary(aes(color="5-th/95-th percentiles"), fun.data = function(y)
    list(y=median(y), ymin = quantile(y,0.05),ymax = quantile(y,0.95))
    , size=0.5,
    geom="errorbar", width=0.2) +
  stat_summary(aes(color="Mean"), size=0.5) + 
  stat_summary(aes(color="Median"), fun=median, geom="point", bins = 10, size=2.5) + 
  theme_minimal_grid() +
  scale_color_manual(values =  c("black", "#55af4a", "#664ea3")) +
  labs(x="Placement Hamming distance", y="Path length", color="", title="Most distant placement among multiplacements") +
  # coord_cartesian(xlim = c(0, 0.051)) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle=30, hjust = 1))
ggsave2("../figures/most_distant_placement.pdf", width = 6, height = 4)

df %>%
  ggplot() +
  aes(x=max_blen/min_blen) +
  stat_density() +
  scale_x_continuous(transform = "log", breaks = c(1, 10, 100, 1000, 10000)) +
  labs(x="Ratio of distance to the most distant and the closest placement", y="Density") +
  theme_cowplot()

 df %>%
  mutate(c=ifelse(max_dist <= 0.05, "<=5%", ">5%"), v=(max_blen)-(min_blen)) %>%
  group_by(qid) %>%
  mutate(q = quantile(v, 0.5)) %>%
  ungroup() %>%
  ggplot() +
  aes(x=v, color=c) +
  stat_ecdf(n=10000, linewidth = 1) +
  # scale_x_continuous(transform = "log", breaks = c(1, 10, 100, 1000, 10000)) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  scale_color_manual(values =  c("#80b1d3", "#fb8072")) +
  theme_minimal_grid() +
  coord_cartesian(ylim=c(0.80, 1), xlim = c(0.0, 1)) +
  theme(legend.position = "top", legend.title.position = "top") +
  labs(color="Placement Hamming distance of the most distant placement", x="Maximum path length difference among multiplacements", y="ECDF")
ggsave2("../figures/placement_diff_ecdf.pdf", width = 6, height = 4)

df %>%
  group_by(qid) %>%
  mutate(ratio = (max_blen-min_blen), ecdf_score = ecdf(ratio)(ratio)) %>%
  mutate(ecdf_bin = cut(ecdf_score, breaks = c(0.5, 0.9, 0.95, 0.96, 0.97, 0.98, 0.99, 1), include.lowest = TRUE)) %>%
  ungroup() %>%
  group_by(qid, ecdf_bin) %>%
  summarize(maxratio = min(ratio)) %>%
  filter(!is.na(ecdf_bin)) %>%
  mutate(ratio_bin = cut(maxratio, breaks = c(0, 0.1, 0.5, 1.0, 10, Inf), include.lowest = TRUE)) %>%
  group_by(ecdf_bin, ratio_bin) %>% 
  summarize(portion = n()/length(unique(df$qid))) %>%
  ggplot() +
  aes(x=ecdf_bin, y = portion, fill=ratio_bin) +
  geom_col(position = position_stack()) +
  scale_fill_manual(values =  c("#e5d8bd", "#bebada", "#fb8072", "#80b1d3")) +
  # scale_fill_brewer(palette =  "Paired") +
  scale_x_discrete(labels = c("50%", "90%", "95%", "96%", "97%", "98%", "99%")) +
  labs(y="Portion of the query genomes", x="Percentile of reads", fill="Closest vs. most distant placement difference") +
  theme_cowplot() +
  theme(legend.position = "top", legend.title.position = "top")
