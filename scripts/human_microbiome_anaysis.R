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

df_krepp <- read_qza("../results/metrics_krepp_hmi-fp50_depth100k/weighted_unifrac_pcoa_results.qza")
df_woltka <- read_qza("../results/metrics_woltka_hmi-depth100k/weighted_unifrac_pcoa_results.qza")

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
wunifrac <- read_qza("../results/metrics_krepp_hmi-fp50_depth100k/weighted_unifrac_pcoa_results.qza")

df <- wunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2, PC3) %>%
  left_join(metadata)

scene = list(camera = list(eye = list(x = 1.5, y = 1.5, z = -0.25)))
p <- plot_ly(df, x= ~PC1, y= ~PC2, z= ~PC3, 
        type="scatter3d",
        mode="markers",
        marker = list(size=5, opacity=.9),
        color= ~`Body site`,
        # symbol=~`Host sex`,
        # symbols = c("circle","diamond"),
        colors='Paired'
) %>% layout( scene = scene)
p
save_image(p, "../figures/pcoa_hmi_sites.pdf", width = 750, height = 450)

df_dsite <- vroom("../results/krepp_hmi-fp50-body_site.tsv")
df_dsex <- vroom("../results/krepp_hmi-fp50-host_sex.tsv")

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

df_sep = data.frame(method=c("krepp", "woltka-OGU", "Bracken", "krepp", "woltka-OGU", "Bracken"),
                    approach=c("OGU+UniFrac", "OGU+UniFrac", "Taxonomy+Bray-Curtis", "OGU+UniFrac", "OGU+UniFrac", "Taxonomy+Bray-Curtis"),
                    type=c("Body site", "Body site", "Body site", "Host sex", "Host sex", "Host sex"),
                    pseudo_F=c(74.803363, 72.612034, 42.357998, 2.628345, 2.614361, 1.691569),
                    p_val=c(0.001, 0.001, 0.001, 0.032, 0.033, 0.071)
)

df_sep %>%
  mutate(p_val=paste('p', p_val, sep="=")) %>%
  ggplot() +
  aes(x=reorder(method,-pseudo_F) , fill=approach, y=pseudo_F) +
  facet_wrap(~type) +
  scale_y_continuous(trans="log10") +
  scale_fill_manual(values=c("#02818a", "#e41a1c")) +
  geom_col() +
  geom_text(aes(label=round(pseudo_F, 2)), nudge_y = 0.05)  +
  geom_text(aes(label=p_val), nudge_y = 0.15)  +
  theme_cowplot() + theme(legend.title = element_blank(), legend.position = "inside", legend.justification = c(0.95, 0.6)) +
  labs(x="Method", y="pseudo F") + theme(axis.text.x = element_text(angle=45, vjust=0.6))
ggsave("../figures/seperation_statistics.pdf",height = 5, width = 5)