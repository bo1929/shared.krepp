require(ggplot2)
require(readr)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)

df <- read_tsv(
  "../results/dest-maxhd_ro.tsv",
  #col_names = c("read_id", "form", "genome_id", "dist", "hd_avg", "covpos", "covmer")
  col_names = c("cfg", "maxhd", "read_id", "form", "match_count", "nuniqkmers", "ro", "reference_genome", "dist_avg", "dist_llh", "covpos", "covmer")
)
df$dist_mash[df$reference_genome == "G000026325"] <- 0.027141
df$dist_mash[df$reference_genome == "G000299455"] <- 0.0183684

df %>% filter %>% filter(nuniqkmers > 10^6) %>% ggplot() + 
  aes(x=maxhd, y=dist_llh/dist_mash, color=reference_genome) +
  geom_point() + theme_minimal_grid()

df %>% filter %>% filter(nuniqkmers > 10^6) %>% ggplot() + 
  facet_wrap(c("ro"), scale="free") + 
  aes(x=maxhd, y=dist_llh/dist_mash, color=as.factor(dist_mash)) +
  geom_point() + theme_minimal_grid()

df %>% filter %>% filter(nuniqkmers > 10^6) %>% ggplot() + 
  facet_wrap(c("cfg"), scale="fixed") + 
  aes(x=maxhd, y=dist_llh/dist_mash, color=as.factor(dist_mash)) +
  geom_point() + theme_minimal_grid() + 
  scale_x_continuous(n.breaks = 10) + 
  scale_y_continuous(n.breaks = 8) +
  labs(x="Maximum HD", y="LLH estimate / Mash estimate", color="Mash distance")
df %>% filter %>% filter(nuniqkmers > 10^6) %>% ggplot() + 
  facet_wrap(c("cfg"), scale="fixed") + 
  aes(x=maxhd, y=dist_llh-dist_mash, color=as.factor(dist_mash)) +
  geom_point() + theme_minimal_grid() + 
  scale_x_continuous(n.breaks = 10) + 
  scale_y_continuous(n.breaks = 8) +
  labs(x="Maximum HD", y="LLH estimate - Mash estimate", color="Mash distance")


df %>% filter %>% filter(nuniqkmers > 10^6) %>% ggplot() + 
  facet_wrap(c("dist_mash")) + 
  aes(x=maxhd, y=dist_llh/dist_mash, color=ro) + 
  geom_point(alpha=0.85) + theme_minimal_grid() + 
  scale_x_continuous(n.breaks = 10) + 
  scale_y_continuous(n.breaks = 8) +
  labs(x="Maximum HD", y="LLH estimate / Mash estimate", color="rho")

df %>% filter %>% filter(nuniqkmers > 10^6) %>% ggplot() + 
  facet_wrap(c("dist_mash")) + 
  aes(x=maxhd, y=dist_llh-dist_mash, color=ro) + 
  geom_point(alpha=0.85) + theme_minimal_grid() + 
  scale_x_continuous(n.breaks = 10) + 
  scale_y_continuous(n.breaks = 8) +
  labs(x="Maximum HD", y="LLH estimate - Mash estimate", color="rho")
