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

df_sep <- vroom("../results/expt-emp/emp_separation_summary-v2.tsv",
                col_names = c("method", "filtering", "sampling_depth", "level", "approach", "reference", "pseudo_F", "p_val", "nsamples", "size")
          )

df_sep %>% filter(method == "Woltka" | (reference == "WoLv1" & filtering == "fp01")) %>%
  ggplot() +
  aes(x=level, shape=approach,  color=method, y=pseudo_F, shape=method) +
  # facet_wrap(~type, scales = "free_y") +
  # scale_y_continuous(trans="log10") +
  scale_color_brewer(palette = "Set1") +
  # scale_shape_manual(values=c(16, 16), guide = 'none') +
  # geom_line(aes(group=level), color="darkgray", linewidth = 1) +
  geom_point(size=2.5, alpha=0.95) +
  # geom_text(aes(label=round(pseudo_F, 2)), nudge_x = -0.2, nudge_y = 2.5, color="black")  +
  # geom_text(aes(label=p_val), nudge_y = -0.15, color="black")  +
  theme_half_open() +
  background_grid() +
  coord_cartesian(ylim = c(0, 330)) +
  theme(axis.title.y = element_markdown(), legend.background = element_rect(fill = "white"), legend.box = "vertical", legend.title = element_blank(), legend.position = c(0.7, 0.8)) +
  labs(x="Level", y="pseudo *F*")  +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))
# theme(axis.text.x = element_blank())
ggsave("../figures/emp-pseudo_F.pdf",height = 4, width = 2.5)

metadata <- read_q2metadata("../results/expt-emp/metadata_emp-v2.tsv")
wunifrac <- read_qza("../results/expt-emp/emp-pp-WoLv1/metrics-fp01-sd6550/weighted_unifrac_pcoa_results.qza")

df <- wunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2, PC3) %>%
  left_join(metadata)

scene = list(camera = list(eye = list(x = -1, y = 3.25, z = 1.5)))
p <- plot_ly(df, x= ~PC1, y= ~PC2, z= ~PC3, 
        type="scatter3d",
        mode="markers",
        marker = list(size=4, opacity=.8),
        color= ~`empo_4`,
        # symbol=~`empo_4`,
        # symbols = c("circle","diamond"),
        colors='Paired'
) %>% layout(scene = scene)
p
save_image(p, "../figures/pcoa_emp_empo4.pdf", width = 1000, height = 600, scale=3)

df <- vroom("../results/expt-emp/emp-ogu-WoLv1/feature_tables_merged.tsv")
df_ogu <- transpose(df %>%  summarise(across(!`#FeatureID`, sum), method="OGU"), keep.names = "SampleID", make.names = "method")
df <- vroom("../results/expt-emp/emp-pp-WoLv1/placement_table.tsv")
df_dist <- vroom("../results/expt-emp/emp-pp-WoLv1/placement_distances.tsv", col_names = c("#FeatureID", "dist"))
df<-merge(df_dist, df)
dfm_pp <- df %>% pivot_longer(cols = -c(dist, `#FeatureID`), values_to = "Count", names_to = "SampleID")
df <- merge(dfm_pp, df_ogu, by = "SampleID")
df$ndist <- df$dist
dfs_emp <- df %>% 
  mutate(qth=quantile(OGU, 0.05)) %>%
  # mutate(distBin = cut(ndist, c(0:10)/10)) %>% 
  # group_by(SampleID, distBin) %>% 
  # summarise(ncount = sum(Count)/OGU)
  group_by(SampleID) %>%  
  summarise(ncount = sum(Count)/max(OGU), mOGU=max(OGU), qth=max(qth), dist=sum(dist*(Count/sum(Count))))
dfs_emp$data <- "EMP"

df <- vroom("../results/expt-hmi/hmi-ogu-WoLv2/table_merged.tsv")
df_ogu <- transpose(df %>%  summarise(across(!`#FeatureID`, sum), method="OGU"), keep.names = "SampleID", make.names = "method")
df <- vroom("../results/expt-hmi/hmi-pp-WoLv2/placement_table.tsv")
df_dist <- vroom("../results/expt-hmi/hmi-pp-WoLv2/placement_distances.tsv", col_names = c("#FeatureID", "dist"))
df<-merge(df_dist, df)
dfm_pp <- df %>% pivot_longer(cols = -c(dist, `#FeatureID`), values_to = "Count", names_to = "SampleID")
df <- merge(dfm_pp, df_ogu, by = "SampleID")
df$ndist <- df$dist
dfs_hmi <- df %>% 
  mutate(qth=quantile(OGU, 0.05)) %>%
  # mutate(distBin = cut(ndist, c(0:10)/10)) %>% 
  # group_by(SampleID, distBin) %>%  
  group_by(SampleID) %>%  
  summarise(ncount = sum(Count)/max(OGU), mOGU=max(OGU), qth=max(qth), dist=sum(dist*(Count/sum(Count))))
dfs_hmi$data <- "HMP"

rbind(dfs_hmi, dfs_emp) %>% # filter(mOGU > qth) %>%
  ggplot() +
  aes(x=data, y=ncount) +
  geom_boxplot(outliers = FALSE, color="black") +
  coord_cartesian(ylim=c(0, 1)) + theme_cowplot() +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=90, vjust = 0.5)) +
  labs(y="Ratio of total counts (PP/OGU)")


df <- vroom("../results/expt-emp/emp-ogu-WoLv1/raw_data_distances.tsv")
df$Group1s <- sub(" (non-saline)", "", df$Group1, fixed = TRUE)
df$Group2s <- sub(" (non-saline)", "", df$Group2, fixed = TRUE)
df$Group1s <- sub(" (saline)", "", df$Group1s, fixed = TRUE)
df$Group2s <- sub(" (saline)", "", df$Group2s, fixed = TRUE)
df$saline1 <- grepl(" (saline)", df$Group1, fixed = TRUE)
df$saline2<- grepl(" (saline)", df$Group2, fixed = TRUE)
df$sameGroup <- df$Group1s == df$Group2s

df %>% group_by(Group1, Group2) %>%
  summarize(dist=mean(Distance)) %>%
  ggplot() +
  # aes(x=reorder(Group1, grepl("non-saline", Group1)), y=reorder(Group2, grepl("non-saline", Group2)), fill=dist) +
  aes(x=Group1, y=Group2, fill=dist) +
  stat_summary(geom = "tile") +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_fill_viridis_c() + labs(x="", y="", fill="Mean distance")
ggsave("../figures/emp_mean_distances.pdf", width = 7, height = 6)

x<-data.frame(
  "Group 1"=union(dfp$`Group 1`, dfp$`Group 2`),
  "Group 2"=union(dfp$`Group 1`, dfp$`Group 2`),
  "Sample size"=NA,
  `Permutations`=NA,
  `pseudo-F`=NA,
  `p-value`=NA,
  `q-value`=NA
  )
names(x) <- names(dfp)
dfp <- vroom("../results/expt-emp/emp-ogu-WoLv1/permanova-pairwise.csv") 
rbind(dfp, x) %>%
  mutate(pval=cut(`p-value`,c(0,0.0001,0.001,0.01,0.05,1), labels=c("****","***","**","*",""))) %>%
  ggplot() +
  aes(x=`Group 1`,
      y=`Group 2`,
      fill=`pseudo-F`,
      label=pval) +
  geom_tile(na.rm = FALSE)+
  scale_fill_viridis_c(trans="log10") +
  theme_classic() +
  geom_text(color="red")+
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  labs(x="", y="")
ggsave("../figures/permanova_pairwise.pdf", width =8, height = 7)
