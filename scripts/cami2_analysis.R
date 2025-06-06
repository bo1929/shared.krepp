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
  
df_mrmg = vroom("../results/cami-ii/amber_marine_contigs-min_dist/results.tsv")
df_mrmg$Dataset="Marine"
df_strmg = vroom("../results/cami-ii/amber_strain_madness_contigs-min_dist/results.tsv")
df_strmg$Dataset="Strain-madness"
df <- rbind(df_mrmg, df_strmg)
df <- df %>% filter(Tool %in% c("PhyloPythiaS+ 1.4", "DIAMOND 0.9.28", "krepp-algpp-dup v0.4.2", "krepp-minD-dup v0.4.2", "LSHVec cami2", "Kraken 2.0.8-beta", "MEGAN"))
df <- rbind(
  pivot_longer(df, cols = -c(Tool, Sample, rank, Dataset, `binning type`), names_to = c(".value", "filtered"),  names_pattern = '(.*) (\\(.*)') %>% filter(!is.na(filtered)) %>% mutate(filtered=FALSE),
  df %>% select(!ends_with("(unfiltered)")) %>% mutate(filtered=TRUE)
)
df$Tool[df$Tool == "krepp-algpp-dup v0.4.2"] <- "krepp v0.4.5"
df$Tool[df$Tool == "krepp-minD-dup v0.4.2"] <- "krepp-closest v0.4.5"

df %>% filter(filtered==FALSE) %>%
  ggplot() +
  aes(x=reorder(rank, accuracy_seq), color=Tool, y=accuracy_seq) +
  facet_wrap(~Dataset, nrow = 2) +
  geom_point(aes(shape=Tool), size = 3, alpha=0.7) +
  geom_line(aes(group=Tool)) +
  scale_shape_manual(values = c(3, 15, 16, 17, 18, 8, 4),  guide = guide_legend(nrow = 2)) +
  scale_color_manual(values = c( "#66a2b5", "#55c00c", "#e41a1c", "#ff7f00", "#b15928", "#e7298a", "#6a3d9a"), guide = guide_legend(nrow = 2)) +
  theme_half_open() +
  background_grid() +
  theme(
    axis.text.x = element_text(angle=30, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
    ) +
  labs(y="Accuracy")
ggsave2("../figures/cami2-accuracy.pdf", width = 4, height = 6)

df %>% filter(filtered==FALSE & rank != "superkingdom") %>%
  ggplot() +
  aes(y=recall_avg_seq, color=Tool, x=precision_avg_seq) +
  facet_grid(cols = vars(Dataset), rows= vars(reorder(rank, -accuracy_seq)), scale="fixed") +
  # geom_point(aes(shape=rank), size = 9, alpha=0.75) +
  geom_point(size = 3, shape=16, alpha=0.65) +
  scale_color_manual(values = c( "#66a2b5", "#55c00c", "#e41a1c", "#ff7f00", "#b15928", "#e7298a", "#6a3d9a")) +
  theme_half_open(font_size = 13) +
  background_grid() +
  theme(panel.spacing.x = unit(5, "mm")) +
  labs(x = "Purity", y="Completeness") +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.0)) +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  # coord_cartesian(xlim=c(0,0.8), ylim=c(0, 1)) +
  guides(color="none")
ggsave2("../figures/cami2-completeness_purity.pdf", width = 3.9, height = 6)

df %>% filter(filtered==FALSE) %>% filter(!grepl("krepp ", Tool)) %>%
  ggplot() +
  aes(x=reorder(rank, accuracy_seq), color=Tool, y=accuracy_seq) +
  facet_wrap(~Dataset) +
  geom_point(aes(shape=Tool), size = 3, alpha=0.7) +
  geom_line(aes(group=Tool)) +
  scale_shape_manual(values = c(3, 15, 16, 18, 8, 4)) +
  scale_color_manual(values = c( "#1b9e77", "#33a02c","#e41a1c", "#b15928", "#e7298a", "#6a3d9a")) +
  theme_half_open() +
  background_grid() +
  theme(
    axis.text.x = element_text(angle=30, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  labs(y="Accuracy") + guides(color=guide_legend(nrow=2,byrow=TRUE), shape=guide_legend(nrow=2,byrow=TRUE))

df %>% filter(filtered==FALSE) %>%
  ggplot() +
  # aes(x=reorder(rank, accuracy_seq), color=Tool, y=recall_weighted_seq) +
  aes(x=reorder(rank, accuracy_seq), color=Tool, y=recall_weighted_seq) +
  facet_wrap(~Dataset) +
  geom_point(aes(shape=Tool), size = 2, alpha=0.75) +
  geom_line(aes(group=interaction(Tool, filtered), linetype = filtered)) +
  scale_color_brewer(palette = "Dark2") +
  theme_half_open() +
  background_grid() +
  theme(
    axis.text.x = element_text(angle=30, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(y="Recall")

df %>% filter(filtered==FALSE) %>%
  ggplot() +
  # aes(x=reorder(rank, accuracy_seq), color=Tool, y=recall_weighted_seq) +
  aes(x=reorder(rank, accuracy_seq), color=Tool, y=precision_weighted_seq) +
  facet_wrap(~Dataset) +
  geom_point(aes(shape=Tool), size = 2, alpha=0.75) +
  geom_line(aes(group=interaction(Tool, filtered), linetype = filtered)) +
  scale_color_brewer(palette = "Dark2") +
  theme_half_open() +
  background_grid() +
  theme(
    axis.text.x = element_text(angle=30, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(y="Precision")

df %>% filter(filtered==FALSE & rank != "superkingdom") %>%
  ggplot() +
  aes(y=percentage_of_assigned_seqs, fill=Tool, x=reorder(rank, recall_weighted_seq)) +
  facet_wrap(vars(Dataset), nrow=2, scale="fixed") +
  geom_col(position = position_dodge2(), alpha=0.9) +
  scale_fill_manual(values = c( "#66a2b5", "#55c00c", "#e41a1c", "#ff7f00", "#b15928", "#e7298a", "#6a3d9a"), guide = guide_legend(nrow = 2)) +
  theme_half_open() +
  background_grid() +
  theme(
    axis.text.x = element_text(angle=30, hjust = 1),
    axis.title.x = element_blank()
  ) + labs(y = "% sequences assigned") +
  scale_y_continuous(labels=percent) +
  guides(fill="none")
ggsave2("../figures/cami2-percent_assigned.pdf", width = 3.5, height = 6)

df %>% filter(filtered==FALSE & rank != "superkingdom" & Dataset == "Strain-madness") %>%
  ggplot() +
  aes(x=precision_weighted_seq, color=Tool, y=recall_weighted_seq) +
  facet_wrap(~rank, scale="free") +
  geom_point(size = 2, alpha=0.7) +
  scale_color_brewer(palette = "Dark2") +
  theme_half_open() +
  background_grid() +
  labs(x = "Precision", y="Recall")