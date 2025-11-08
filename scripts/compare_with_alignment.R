require(ggplot2)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)
require(vroom)
require(data.table)
require(ggnewscale)
require(scales)
require(latex2exp)

dfd <- vroom(
  "../misc_data/data-bowtie_comparison/query_reference_pairs-WoLv2-ge20p/all_distances-mash.tsv",
  col_names = c("ref_id", "query_id", "mash_dist")
  )
# dfd$mash_dist <- 1 - dfd$mash_dist/100
dfd  = dfd[!duplicated(dfd),]
dfc <- vroom(
  "../results/alignment_comparison/count_summary-all-WoLv2-alignment_comparison.csv",
  skip = 1,
  col_names = c("read_id", "counts_bowtie", "counts_krepp", "novelty", "query_id")
)
dfr <- vroom(
  "../results/alignment_comparison/reference_summary-all-WoLv2-alignment_comparison.csv",
  skip = 1,
  col_names = c("ref_id", "num_reads", "mean_dist", "method", "portion_reads", "novelty", "query_id")
) %>% filter(portion_reads >= 0.20)
dfr <- merge(dfr, dfd, by = c("ref_id", "query_id"))

dfc_s <- dfc %>% group_by(query_id, novelty) %>% 
  summarize(
    median_counts_bowtie = median(counts_bowtie),
    median_counts_krepp = median(counts_krepp),
    ratio_mapped_bowtie = sum(counts_bowtie > 0)/n(),
    ratio_mapped_krepp = sum(counts_krepp > 0)/n(),
    ratio_g1mapped_bowtie = sum(counts_bowtie > 1)/n(),
    ratio_g1mapped_krepp = sum(counts_krepp > 1)/n()
    ) %>% ungroup()
dfc_s <- melt(
  data = setDT(dfc_s),
  id.vars = c("query_id", "novelty"),
  measure.vars = list(
    c('median_counts_bowtie', 'median_counts_krepp'),
    c('ratio_mapped_bowtie', 'ratio_mapped_krepp'),
    c('ratio_g1mapped_bowtie', 'ratio_g1mapped_krepp')
    ),
  value.name = c('median_counts', 'ratio_mapped', "ratio_g1mapped"),
  variable.name = "method"
)[, method := factor(method, labels = c("bowtie", "krepp"))]

dfc_s %>% mutate(novelty_bin = cut(novelty, c(0, 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.33))) %>%
  group_by(novelty_bin, method) %>%
  summarise(
    ratio_mapped_mean=mean(ratio_mapped),
    ratio_g1mapped_mean=mean(ratio_g1mapped)
    ) %>% filter(!is.na(novelty_bin)) %>%
  ggplot() +
  facet_wrap(~method, scale="free") +
  # geom_point(aes(x=novelty, y=ratio_mapped, color=interaction(">0",method))) +
  # geom_point(aes(x=novelty, y=ratio_g1mapped, color=interaction(">1",method))) +
  geom_segment(
     aes(x=novelty_bin,xend=novelty_bin, y=ratio_mapped_mean,yend=ratio_g1mapped_mean,color=method),
     # arrow = arrow(length = unit(7, "pt"), type="closed"),
     lineend="round",
     linejoin="bevel",
     linewidth = 1
    ) +
  # geom_ribbon(aes(alpha = 0.15, group = method, x=novelty_bin, ymin=ratio_mapped_mean, ymax=ratio_g1mapped_mean, color=method)) +
  geom_label(aes(x=novelty_bin, y=ratio_mapped_mean, label=">0"), label.size = 0) +
  geom_label(aes(x=novelty_bin, y=ratio_g1mapped_mean, label=">1"), label.size = 0) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme_minimal_grid() +
  labs(
    x = "Distance to the closest reference",
    y = "% of mapped reads",
    color = "Method"
    ) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.66), legend.position = "none")
ggsave2("../figures/portion_mapped-facet.pdf", width = 6.5, height = 3.5)
dfc_s[dfc_s$method=="bowtie"]$method <- "bowtie2"
dfc_s %>% mutate(novelty_bin = cut(novelty, c(0, 0.01, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.2, 0.25, 0.34))) %>%
  group_by(novelty_bin, method) %>%
  summarise(
    ratio_mapped_mean=mean(ratio_mapped),
    ratio_g1mapped_mean=mean(ratio_g1mapped)
  ) %>% filter(!is.na(novelty_bin)) %>%
  ggplot() +
  # geom_point(aes(x=novelty, y=ratio_mapped, color=interaction(">0",method))) +
  # geom_point(aes(x=novelty, y=ratio_g1mapped, color=interaction(">1",method))) +
  # geom_segment(
  #   aes(x=novelty_bin,xend=novelty_bin, y=ratio_mapped_mean,yend=ratio_g1mapped_mean,color=method),
  #   arrow = arrow(length = unit(5, "pt"))
  # ) +
  geom_ribbon(linewidth = 0.75, alpha = 0.25, aes(fill=method, group = method, x=novelty_bin, ymin=ratio_mapped_mean, ymax=ratio_g1mapped_mean, color=method)) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal_grid() +
  labs(
    x = "1-ANI to the closest reference",
    y = "% of mapped reads",
    color = "Method",
    fill = "Method"
  ) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.66))
ggsave2("../figures/portion_mapped-combined.pdf", width = 4.5, height = 3.5)

dfr$method[dfr$method=="bowtie"] <- "bowtie2"
mean((dfr %>% filter(mash_dist > 0.05 & mash_dist <0.12 & method == "krepp") %>% mutate(bias=mean_dist/mash_dist))$bias)
dfr %>% 
  ggplot(aes(x=mash_dist, y=mean_dist, group=interaction(ref_id,query_id,method))) +
  stat_summary(aes(color=portion_reads), alpha = 0.35, size = 0.15) +
  scale_color_viridis_c(labels = percent) +
  facet_wrap(~method) +
  labs(color = "Mapped") + 
  scale_y_continuous()  +
   new_scale_colour() +
   stat_smooth(
     se = F, method = "lm",
     formula = y ~ splines::bs(x, 3),
     alpha=1, linewidth = 1.25
     ) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_linetype_manual(values = c(2, 3)) +
  geom_abline() +
  theme_minimal_grid() +
  coord_cartesian(xlim = c(0.0,0.25),ylim = c(0.0,0.25)) +
  guides(colour = guide_legend(override.aes = list(size=1))) +
  labs(
    x = "1 - ANI between query/reference pair",
    y = "Estimated distances (mean)",
    color = "Method", shape = "Method" # , linetype = "Method"
  ) +
  theme(
    legend.key.width = unit(2, "line"),
    legend.direction = "vertical",
    legend.position = c(0.285, 0.285),
    legend.title.position = "top",
    panel.spacing.x = unit(5, "mm")
    )
ggsave2("~/Documents/krepp_talk_figures/alignment-ani_versus_mean.pdf", width = 6.5, height = 4.5)
ggsave2("../figures/meanHD_versus_MashD-wportion_mapped-1.pdf", width = 6.5, height = 4.5)

dfr$method[dfr$method=="bowtie2"] <- "bowtie"
dfr %>% 
  ggplot(aes(mash_dist,mean_dist,shape=method,group=interaction(ref_id,query_id,method)))+
  geom_abline(linetype=2, linewidth=0.75) +
  theme_cowplot() +
  stat_summary(aes(color=interaction(method,cut(portion_reads*100, 4))), alpha = 0.45, size = 0.25) +
  scale_color_manual(
    values = c("#253494", "#b30000", "#2c7fb8", "#d34a33", "#41b6c4", "#ee8c59", "#72dab4", "#ffac9a"),
    ) +
  guides(color = guide_legend(override.aes = list(nrow = 4, alpha = 1))) +
  labs(color = "% of mapped reads") +
  new_scale_colour() + 
  stat_smooth(
    aes(group=method),
    se = F, method = "lm",
    formula = y ~ splines::bs(x, 3),
    alpha=1, linewidth = 0.75, color = "black"
  ) + 
  scale_linetype_manual(values = c(2, 3)) +
  coord_cartesian(xlim = c(0,0.25), ylim = c(0,0.25)) +
  labs(
    x = "Mash distance between query and reference",
    y = "Mean HD of all mapped reads",
    color = "Method", shape = "Method", linetype = "Method"
  ) +
  guides(color = guide_legend(override.aes = list(alpha=1, size = 2))) +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.key.width = unit(2.25, "line")
    )
ggsave2("../figures/meanHD_versus_MashD-wportion_mapped-2.pdf", width = 7, height = 5)

dfr %>%
  select(method,mean_dist, ref_id, query_id, portion_reads, mash_dist) %>%
  pivot_wider(names_from=c(method), values_from=c(mean_dist, portion_reads)) %>%
  filter(!is.na(mean_dist_bowtie) & !is.na(mean_dist_krepp)) %>%
  ggplot(aes(x=mash_dist, xend=mash_dist, y=mean_dist_krepp, yend=mean_dist_bowtie)) +
  geom_segment(alpha=0.3, linewidth = 0.15) +
  geom_point(aes(color="bowtie", y=mean_dist_bowtie), alpha=0.8, size=2, shape=16) +
  geom_point(aes(color="krepp", y=mean_dist_krepp), alpha=0.8, size=2, shape=17) +
  geom_abline() + theme_minimal_grid(font_size = 16) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  coord_cartesian(xlim = c(0,0.15), ylim = c(0,0.15)) +
  labs(
    x = "1-ANI between query/reference pair (Mash)",
    y = "Estimated distance (mean)",
    color = "Method", shape = "Method"
  )
ggsave2("../figures/meanHD_versus_MashD-pairs_in_both.pdf", width = 6, height = 4.5)

dfr %>% select(method, mean_dist, ref_id, query_id, portion_reads, mash_dist)%>%
  pivot_wider(names_from=c(method), values_from=c(mean_dist, portion_reads), names_sep = "__") %>%
  filter(!is.na(mean_dist__bowtie) & !is.na(mean_dist__krepp)) %>% 
  pivot_longer(
    cols = c(mean_dist__bowtie,mean_dist__krepp, portion_reads__krepp, portion_reads__bowtie),
    names_to=c("metric","method"),
    names_sep = "__"
    )%>%
  pivot_wider(names_from=metric) %>%
  ggplot(aes(x=1-portion_reads, y=(mean_dist-mash_dist)/mash_dist)) +
  facet_wrap(~method) +
  geom_point(aes(color=method), alpha=0.5) +
  theme_minimal_grid() +
  scale_color_brewer(palette="Set1", direction = -1) +
  theme(panel.spacing.x = unit(15, "pt")) +
  labs(y = "Mean ratio difference (1-HD/Mash)", x = "% of reads unmapped") +
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
  coord_cartesian(ylim=c(-0.75, 0.75))
ggsave2("../figures/meanHD_versus_MashD-x_portion_mapped.pdf", width = 7, height = 5)

dfr$method[dfr$method=="bowtie"] <- "bowtie2"
dfr %>% select(method, mean_dist, ref_id, query_id, portion_reads, mash_dist) %>%
  filter(mash_dist < 0.25) %>%
  ggplot(
    aes(x=cut(mash_dist, c(0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.125, 0.15, 0.2, 0.25)),
        y=abs(mean_dist-mash_dist)/mash_dist,
        color=method
    )
  ) +
  stat_summary()+
  theme_bw()+
  scale_y_continuous(n.breaks = 6, labels = percent) +
  scale_color_brewer(palette="Set1", direction = -1) +
  labs(color="Method", y = "Absolute percent error", x = "1-ANI between query/reference pair (Mash)") +
  theme(axis.text.x = element_text(angle=0.90))
ggsave2("../figures/errorpercent-HD_versus_MashD-x_MashD.pdf", width = 8, height = 5)

dfr %>% select(method, mean_dist, ref_id, query_id, portion_reads, mash_dist) %>%
  pivot_wider(names_from=c(method), values_from=c(mean_dist, portion_reads), names_sep = "__") %>%
  filter(!is.na(mean_dist__bowtie) & !is.na(mean_dist__krepp)) %>%
  mutate(mash_dist_g=cut(mash_dist, breaks = c(0:15)/100)) %>%
  group_by(mash_dist_g) %>%
  summarise(
    portion_reads__bowtie=median(portion_reads__bowtie),
    portion_reads__krepp=median(portion_reads__krepp),
    mean_dist__bowtie=mean(abs(mean_dist__bowtie) / mash_dist),
    mean_dist__krepp=mean(abs(mean_dist__krepp) / mash_dist),
  ) %>%
  ggplot(
    aes(x=1-portion_reads__bowtie,
        xend=1-portion_reads__krepp,
        y=abs(mean_dist__bowtie),
        yend=abs(mean_dist__krepp),
        color=mash_dist_g
        )
    ) +
  geom_segment(alpha = 0.85, linewidth = 1, arrow = arrow(length =unit(8, "pt")))+
  scale_y_log10()+
  # scale_y_continuous(transform = "log1p")+
  scale_x_continuous(labels=percent)+
  theme_minimal_grid()+
  coord_cartesian(xlim=c(0, 0.80))+
  scale_colour_viridis_d(name="Mash distance", direction = -1, end = 0.9) +
  labs(y = "Mean ratio difference (HD/Mash)", x = "% of unmapped reads (median)") +
  scale_x_continuous(labels = percent)
ggsave2("../figures/meanHD_versus_MashD-xarrow_portion_mapped.pdf", width = 5, height = 5)

dfs <- vroom(
  "../results/alignment_comparison/read_summary-WoLv2-alignment_comparison-1M.csv",
  col_names = c("query_id", "read_id", "ref_id", "dist_estimated", "dist_aligned")
  )
dfs %>% 
  ggplot(aes(x=dist_aligned, y=dist_estimated)) +
  # geom_point(alpha=0.0025) + 
  # stat_summary(
  #  fun.y = median,
  #  fun.ymin = function(x) quantile(x, 0.25), 
  #  fun.ymax = function(x) quantile(x, 0.75),
  #  geom = "pointrange",
  #  color = "darkred"
  #  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x), 
    fun.ymax = function(x) mean(x) + sd(x), 
    geom = "pointrange",
    color = "darkred",
  ) + 
  coord_cartesian(xlim=c(0.0, 0.125), ylim=c(0.0, 0.125)) +
  geom_abline() +
  labs(x = "Alignment Hamming distance", y = "Maximum likelihood estimation") +
  theme_minimal_grid()
ggsave2("../figures/alignmentD_versus_maxllhD-mean_1std.pdf", width = 5, height = 5)

dfs %>%
  ggplot() +
  stat_ecdf(aes(x=abs(dist_aligned-dist_estimated)/dist_aligned), n=100000) +
  # scale_x_continuous() +
  coord_cartesian(xlim=c(0, 1)) +
  theme_minimal_grid() +
  scale_x_continuous(labels = percent) +
  labs(x = "Error (%)", y = "ECDF")
ggsave2("../figures/alignmentD_versus_maxllhD-percenterr_ecdf.pdf", width = 5, height = 5)


dfdc_mash <- vroom(
  "../misc_data/data-bowtie_comparison/query_reference_pairs-WoLv2-ge20p/common_distances-mash.tsv",
  col_names = c("ref_id", "query_id", "mash_dist")
) 
dfdc_skani <- vroom(
  "../misc_data/data-bowtie_comparison/query_reference_pairs-WoLv2-ge20p/common_distances-skani.tsv",
  col_names = c("ref_id", "query_id", "skani_dist")
) 
dfdc_orthoani <- vroom(
  "../misc_data/data-bowtie_comparison/query_reference_pairs-WoLv2-ge20p/common_distances-orthoANI.tsv",
  col_names = c("ref_id", "query_id", "orthoANI_dist")
) 
dfdc <- merge(merge(dfdc_mash, dfdc_skani, by = c("ref_id", "query_id")), dfdc_orthoani, by = c("ref_id", "query_id"))
dfdc$skani_dist <- 1 - dfdc$skani_dist/100
dfdc$orthoANI_dist <- 1 - dfdc$orthoANI_dist/100

dfdc %>% ggplot() +
  aes(x=mash_dist, y=skani_dist) +
  geom_point(size=0.75, alpha=0.8) + theme_minimal_grid() +
  coord_cartesian(xlim = c(0, 0.25), ylim = c(0, 0.25)) +
  labs(x = "Mash distance", y = "skani distance")
ggsave2("../figures/distance-Mash_vs_skani.pdf", width = 3, height = 3)


dfdc %>% ggplot() +
  aes(x=mash_dist, y=orthoANI_dist) +
  geom_point(size=0.75, alpha=0.8) + theme_minimal_grid() +
  coord_cartesian(xlim = c(0, 0.25), ylim = c(0, 0.25)) +
  labs(x = "Mash distance", y = "orhoANI distance")
ggsave2("../figures/distance-Mash_vs_orthoANI.pdf", width = 3, height = 3)


dfdist <- vroom("../results/alignment_comparison/dist_to_closest-final.tsv", col_names = c("Q", "R", "d"))
dfdist %>% ggplot() +
  facet_wrap(~cut(d, c(0, 0.16, 0.33)), nrow = 2, scale="free") +
  aes(x=reorder(Q, d), y=d) +
  geom_point(size=0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, size = 3)) +
  labs(y="Distance to the closest reference (Mash)", x="Query ID")
ggsave2("../figures/distance_to_closest-WoLv2.pdf", width = 7.5, height = 5)
