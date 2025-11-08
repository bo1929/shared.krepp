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
library(ggtext)
library(scatterplot3d)

df_krepp <- read_qza("../results/expt-hmi/hmi-ogu-WoLv2/metrics-fp01/weighted_unifrac_pcoa_results.qza")
df_woltka <- read_qza("../results/expt-hmi/hmi-woltka-WoLv2/metrics-fp01/weighted_unifrac_pcoa_results.qza")

variance_explained <- rbind(
  pivot_longer(df_krepp$data$ProportionExplained, cols = everything()) %>% mutate(method="krepp"),
  pivot_longer(df_woltka$data$ProportionExplained, cols = everything()) %>% mutate(method="woltka-OGU")
)
variance_explained %>% filter(name %in% c("PC1", "PC2", "PC3")) %>%
  ggplot() +
  aes(x=method, fill=reorder(name, value), y=value) +
  geom_col() +
  scale_fill_manual(values=c("#c7e9c0", "#74c476", "#238b45")) +
  theme_half_open(font_size = 20) +
  background_grid() + # guides(fill="none") +
  labs(x="Method", y="PCoA-explained variance", fill="")
ggsave("../figures/hmi-pcoa_explained_variance.pdf", height=8, width=4.5)

metadata <- read_q2metadata("../misc_data/qiime2_hmi_metadata.tsv")
wunifrac <- read_qza("../results/expt-hmi/hmi-pp-WoLv2/metrics-fp01/weighted_unifrac_pcoa_results.qza")

df <- wunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2, PC3) %>%
  left_join(metadata)

scene = list(camera = list(eye = list(x = 2, y = -2, z = 1)))
p <- plot_ly(df, x= ~PC1, y= ~PC2, z= ~PC3, 
        type="scatter3d",
        mode="markers",
        marker = list(size=4, opacity=.8),
        color= ~`Body site`,
        # color= ~`empo_3`,
        # symbol=~`empo_4`,
        # symbols = c("circle","diamond"),
        colors='Paired'
) %>% layout(scene = scene)
p
save_image(p, "../figures/pcoa_hmi_sites.pdf", width = 1000, height = 600, scale=3)

df_dsite <- vroom("../results/expt-hmi/hmi-ogu-WoLv2/pairwise_distances-body_site.tsv")
df_dsex <- vroom("../results/expt-hmi/hmi-ogu-WoLv2/pairwise_distances-host_sex.tsv")

df_dsite %>% group_by(Group1, Group2) %>%
  summarise(Distance=mean(Distance)) %>%
  ggplot() +
  aes(x=Group1, y=Group2, fill=Distance) +
  geom_tile() +
  scale_fill_viridis_c(begin = 0.0, name = "UniFrac Distance") +
  theme_half_open() +
  background_grid() +
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) +
  labs(x="", y="")
ggsave("../figures/hmi_body_sites-mean_dist.pdf",height = 4, width = 5)

df_sep <- vroom("../results/expt-hmi/hmi_separation_summary.csv")
df_sep$reference[df_sep$reference == "RefSeq (CAMI-II)"] <- "RefSeq (123,853)"
df_sep$reference[df_sep$reference == "RefSeq (CAMI-II) [dedup]"] <- "RefSeq [dedup] (50,752)"
df_sep$reference[df_sep$reference == "WoL-v2"] <- "WoL-v2 (15,953)"

df_sep %>%
  mutate(p_val=paste('p', p_val, sep="=")) %>%
  ggplot() +
  aes(x=reorder(interaction(interaction(method, reference), approach), pseudo_F) , fill=interaction(approach, reference), y=pseudo_F) +
  facet_wrap(~type) +
  scale_y_continuous(trans="log10") +
  scale_fill_brewer(palette = "Paired") +
  geom_col() +
  geom_text(aes(label=round(pseudo_F, 2)), nudge_y = 0.05)  +
  # geom_text(aes(label=p_val), nudge_y = 0.15)  +
  theme_cowplot() + theme(legend.title = element_blank(), legend.position = "inside", legend.justification = c(0.95, 0.6)) +
  labs(x="Method", y="pseudo F") + theme(axis.text.x = element_blank())

df_sep %>% filter(type == "Body site") %>% # filter(reference != "51K") %>%
  mutate(p_val=paste('p', p_val, sep="=")) %>%
  ggplot() +
  aes(x=reorder(interaction(interaction(method, reference), approach), as.numeric(size)+pseudo_F/100), color=method, y=pseudo_F, shape=approach) +
  # facet_wrap(~type, scales = "free_y") +
  # scale_y_continuous(trans="log10") +
  scale_color_manual(values=c("#009955", "#e41a1c", "#377eb8")) +
  scale_shape_manual(values=c(16, 17, 15)) +
  scale_linetype_manual(values=c(2, 3, 1)) +
  geom_line(aes(group=reference, linetype = reference), color="darkgray", linewidth = 1) +
  geom_point(size=4, alpha=0.95) +
  geom_text(aes(label=round(pseudo_F, 2)), nudge_x = -0.25, nudge_y = 3.35, color="black")  +
  # geom_text(aes(label=p_val), nudge_y = -0.15, color="black")  +
  theme_half_open() +
  # background_grid() +
  coord_cartesian(ylim = c(0, 85)) +
  theme(
    legend.background = element_rect(fill = "white"),
    legend.box = "vertical",
    legend.position = "inside",
    legend.key.width = unit(2.4, "line"),
    legend.justification = c(0.95, 0.05),
    legend.title = element_blank()
    ) +
  labs(x="Method & reference dataset", y="pseudo *F*") +
  # theme(axis.text.x = element_text(angle=45, vjust=0.6)) +
  theme(axis.text.x = element_blank(), axis.title.y = element_markdown())
ggsave("../figures/hmi-pseudo_F.pdf",height = 4, width = 5)

dfpw_dup <- vroom("../results/expt-hmi/hmi-pp-RefSeqCIIdup/pairwise_distances-body_sites.tsv")
dfpw_dedup <- vroom("../results/expt-hmi/hmi-pp-RefSeqCII/pairwise_distances-body_sites.tsv")
dfpw_dup$Method <- "krepp_dup"
dfpw_dedup$Method <- "krepp_dedup"
dfpw <- rbind(dfpw_dup, dfpw_dedup)
dfpw <- dfpw %>% mutate(DistanceType=ifelse(Group1==Group2, "Within group", "Between group"))
dfpw <- dfpw %>% group_by(SubjectID1, SubjectID2) %>% mutate(N=n()) %>% ungroup()

dfpw %>% # filter(N==max(N)) %>% 
  ggplot() +
  # facet_wrap(~DistanceType, scale="free_y", ncol=4) +
  facet_wrap(~Group1, scale="free_y", ncol=4) +
  aes(color=Group2, x=Method, y = Distance) +
  stat_summary() +
  stat_summary(aes(group=Group2), geom = "line") +
  scale_color_brewer(palette = "Paired") +
  theme_bw()

dfdiff <- dfpw %>% pivot_wider(id_cols = c(SubjectID1, SubjectID2, Group1, Group2, DistanceType, N), names_from = Method, values_from = Distance)
dfdiff$DistanceDiff <- (dfdiff$krepp_dedup - dfdiff$krepp_dup )/dfdiff$krepp_dup
dfdiff %>% group_by(Group1, Group2) %>% 
  summarise(meanDistanceDiff = mean(DistanceDiff)) %>%
  ggplot() +
  aes(x=Group1, y=Group2, fill=meanDistanceDiff) +
  stat_summary(geom = "tile") + # geom_tile(color="black") +
  scale_fill_gradient2(labels=percent) +
  coord_fixed() + 
  theme_cowplot() +
  labs(fill="Deduplication effect") + 
  theme(axis.text.x  = element_text(angle=90, hjust=0.95, vjust=0.2), axis.title.x = element_blank(), axis.title.y = element_blank())
ggsave("../figures/deduplication_effect_hmidist.pdf",height = 6, width = 7.5)
