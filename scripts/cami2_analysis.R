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
library(Cairo)

df_mrmg = vroom("../results/cami-ii/amber_marine_contigs-min_dist/results.tsv")
df_mrmg$Dataset="Marine"
df_strmg = vroom("../results/cami-ii/amber_strain_madness_contigs-min_dist/results.tsv")
df_strmg$Dataset="Strain-madness"
dff <- rbind(df_mrmg, df_strmg)
dff <- rbind(
  pivot_longer(dff, cols = -c(Tool, Sample, rank, Dataset, `binning type`), names_to = c(".value", "filtered"),  names_pattern = '(.*) (\\(.*)') %>% filter(!is.na(filtered)) %>% mutate(filtered=FALSE),
  dff %>% select(!ends_with("(unfiltered)")) %>% mutate(filtered=TRUE)
)
df <- dff %>% filter(Tool %in% c("PhyloPythiaS+ 1.4", "DIAMOND 0.9.28", "krepp-algpp-dup v0.4.2", "krepp-minD-dup v0.4.2", "LSHVec cami2", "Kraken 2.0.8-beta", "MEGAN"))
df$Tool[df$Tool == "krepp-algpp-dup v0.4.2"] <- "krepp v0.4.5"
df$Tool[df$Tool == "krepp-minD-dup v0.4.2"] <- "krepp-closest v0.4.5"

df %>% filter(filtered==FALSE) %>%
  ggplot() +
  aes(x=reorder(rank, accuracy_bp), color=Tool, y=accuracy_bp) +
  facet_wrap(~Dataset, nrow = 1) +
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
ggsave2("../figures/cami2-accuracy-bp.pdf", width = 4, height = 6)

df %>% filter(filtered==FALSE & rank == "species") %>%
  ggplot() +
  aes(y=reorder(Tool, unifrac_seq), fill=Tool, x=unifrac_seq) +
  facet_wrap(~Dataset, nrow = 1, scales = "free_y") +
  geom_col(aes(shape=Tool)) +
  scale_fill_manual(values = c( "#66a2b5", "#55c00c", "#e41a1c", "#ff7f00", "#b15928", "#e7298a", "#6a3d9a"), guide = guide_legend(nrow = 2)) +
  theme_half_open() +
  background_grid() +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  ) +
  labs(x="wUniFrac error", y="Tools") +
  guides(fill = "none")
ggsave2("../figures/cami2-unifrac-seq.pdf", width = 8, height = 3)


df %>% filter(filtered==FALSE & rank != "superkingdom") %>%
  ggplot() +
  aes(y=recall_avg_bp, color=Tool, x=precision_avg_bp) +
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
ggsave2("../figures/cami2-completeness_purity-bp.pdf", width = 3.9, height = 6)

df %>% filter(filtered==FALSE) %>% filter(!grepl("krepp ", Tool)) %>%
  ggplot() +
  aes(x=reorder(rank, accuracy_bp), color=Tool, y=accuracy_bp) +
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
  aes(y=percentage_of_assigned_seqs, fill=reorder(Tool, -percentage_of_assigned_seqs), x=reorder(rank, recall_weighted_seq)) +
  facet_wrap(vars(Dataset), nrow=2, scale="fixed") +
  geom_col(position = position_dodge2(), alpha=0.9) +
  scale_fill_manual(values = c( "#ff7f00", "#e41a1c", "#55c00c", "#b15928", "#e7298a", "#66a2b5", "#6a3d9a"), guide = guide_legend(nrow = 2)) +
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

### CL-Analysis

dff %>% filter(filtered==FALSE) %>% 
  filter(rank != "superkingdom") %>%
  filter(Dataset == "Marine") %>%
  filter(grepl("krepp ", Tool)) %>%
  mutate(Tool = ifelse(Tool == "krepp v0.4.5", "krepp v0.4.5 (90%)", Tool)) %>%
  mutate(cl = sub("krepp v0.4.5 \\(", "", Tool)) %>%
  mutate(cl = sub("%\\)", "", cl)) %>%
  ggplot() +
  aes(y=recall_avg_bp, color=(cl), x=precision_avg_seq) +
  facet_wrap(vars(reorder(rank, -precision_avg_seq)), scale="free_x") +
  # facet_grid(cols = vars(Dataset), rows= vars(reorder(rank, -accuracy_seq)), scale="fixed") +
  geom_point(size = 4, alpha=0.9) +
  scale_color_brewer(palette = "Paired") +
  theme_half_open(font_size = 13) +
  background_grid() +
  theme(panel.spacing.x = unit(5, "mm")) +
  labs(x = "Purity", y="Completeness")

dff %>% filter(filtered==FALSE) %>% 
  filter(rank != "superkingdom") %>%
  filter(Dataset == "Marine") %>%
  filter(grepl("krepp ", Tool)) %>%
  mutate(Tool = ifelse(Tool == "krepp v0.4.5", "krepp v0.4.5 (90%)", Tool)) %>%
  mutate(cl = sub("krepp v0.4.5 \\(", "", Tool)) %>%
  mutate(cl = sub("%\\)", "", cl)) %>%
  pivot_wider(id_cols = c("rank", "Dataset"), names_from = c("cl"), values_from = -c("rank", "Dataset", "Tool", "cl")) %>%
  ggplot() +
  facet_wrap(~Dataset) +
  # facet_grid(cols = vars(Dataset), rows= vars(reorder(rank, -accuracy_seq)), scale="fixed") +
  geom_segment(aes(x = `precision_avg_bp_HD=4)`, xend = `precision_avg_bp_HD=2)`,
                   y = `recall_avg_bp_HD=4)`, yend = `recall_avg_bp_HD=2)`, color=factor(rank, levels=c("species", "genus", "family", "order", "class", "phylum"))
                   ), arrow = arrow(type = "closed", length = unit(3, "pt")), lineend = "butt", linewidth = 1) +
  scale_color_brewer(palette = "Paired") +
  theme_half_open(font_size = 13) +
  background_grid() +
  theme(panel.spacing.x = unit(5, "mm"), plot.title = element_text(hjust = 0.675)) +
  labs(x = "Purity", y="Completeness", color="", title="Decreasing HD threshold δ=4 → δ=2")
ggsave2("../figures/cami2-delta-params.pdf", width = 4, height = 3.5)

dff %>% filter(filtered==FALSE) %>% 
  filter(rank != "superkingdom") %>%
  filter(Dataset == "Marine") %>%
  filter(grepl("krepp ", Tool)) %>%
  mutate(Tool = ifelse(Tool == "krepp v0.4.5", "krepp v0.4.5 (90%)", Tool)) %>%
  mutate(cl = sub("krepp v0.4.5 \\(", "", Tool)) %>%
  mutate(cl = sub("%\\)", "", cl)) %>%
  pivot_wider(id_cols = c("rank", "Dataset"), names_from = c("cl"), values_from = -c("rank", "Dataset", "Tool", "cl")) %>%
  ggplot() +
  facet_wrap(~Dataset, scale="free") +
  # facet_grid(cols = vars(Dataset), rows= vars(reorder(rank, -accuracy_seq)), scale="fixed") +
  geom_segment(aes(x = `precision_avg_seq_HD=4)`, xend = precision_avg_seq_99,
                   y = `recall_avg_seq_HD=4)`, yend = recall_avg_seq_99, color=factor(rank, levels=c("species", "genus", "family", "order", "class", "phylum"))
  ), arrow = arrow(type = "closed", length = unit(3, "pt")), lineend = "butt", linewidth = 1) +
  scale_color_brewer(palette = "Paired") +
  theme_half_open(font_size = 13) +
  background_grid() +
  theme(panel.spacing.x = unit(5, "mm"), plot.title = element_text(hjust = 0.675)) +
  labs(x = "Purity", y="Completeness", color="", title="Decreasing significance α=10% → α=1%")
ggsave2("../figures/cami2-alpha-params.pdf", width = 4, height = 3.5)

dff %>% filter(filtered==FALSE) %>% 
  filter(Dataset == "Marine") %>%
  filter(rank == "superkingdom") %>%
  filter(grepl("krepp ", Tool)) %>%
  mutate(Tool = ifelse(Tool == "krepp v0.4.5 (HD=4)", "default: α=90%, δ=4", Tool)) %>%
  mutate(Tool = ifelse(Tool == "krepp v0.4.5 (HD=2)", "α=90%, δ=2", Tool)) %>%
  mutate(Tool = ifelse(Tool == "krepp v0.4.5 (99%)", "α=99%, δ=4", Tool)) %>%
  filter(grepl("α=", Tool)) %>%
  ggplot() +
  aes(y=reorder(Tool, unifrac_seq), fill=Tool, x=unifrac_seq) +
  facet_wrap(~Dataset, nrow = 1, scales = "free_y") +
  geom_col(aes(shape=Tool), color="black") +
  theme_half_open() +
  background_grid() +
  theme(
    legend.position = "none",
    legend.title = element_blank()
  ) +
  scale_fill_brewer(palette= "Set2") +
  labs(x="wUniFrac error", y="") +
  guides(fill=guide_legend(nrow=1,byrow=TRUE), shape=guide_legend(nrow=3,byrow=TRUE))
ggsave2("../figures/cami2-wunifrac-params.pdf", width = 4, height = 3)

dff$rank <- factor(dff$rank,  levels = c("species", "genus", "family", "order", "class", "phylum", "superkingdom"))

sort_by(dff, dff$rank) %>% filter(filtered==FALSE) %>% 
  filter(Dataset == "Marine") %>%
  filter(grepl("krepp ", Tool)) %>%
  mutate(Tool = ifelse(Tool == "krepp v0.4.5 (HD=4)", "default: α=90%, δ=4", Tool)) %>%
  mutate(Tool = ifelse(Tool == "krepp v0.4.5 (HD=2)", "α=90%, δ=2", Tool)) %>%
  mutate(Tool = ifelse(Tool == "krepp v0.4.5 (99%)", "α=99%, δ=4", Tool)) %>%
  filter(grepl("α=", Tool)) %>%
  ggplot() +
  aes(x=rank, fill=Tool, y=accuracy_seq) +
  geom_col(position = position_dodge2(), color="black", width = 0.8) +
  theme_half_open() +
  background_grid() +
  theme(
    axis.text.x = element_text(angle=30, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(y="Accuracy") +
  scale_x_discrete() +
  scale_fill_brewer(palette= "Set2") +
  coord_cartesian(ylim=c(0.75, 1)) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE), shape=guide_legend(nrow=3,byrow=TRUE))
ggsave2("../figures/cami2-accuracy-params.pdf", width = 5, height = 4)
