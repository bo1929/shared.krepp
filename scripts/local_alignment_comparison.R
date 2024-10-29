require(ggplot2)
require(readr)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)

ddf <- read_tsv(
  "../results/G000006925-summary-llh-werror_wrepeats-hd4.txt",
  #col_names = c("read_id", "form", "genome_id", "dist", "hd_avg", "covpos", "covmer")
  col_names = c("read_id", "form", "match_count", "len", "ro", "reference_genome", "dist_avg", "avg_hdist", "covpos", "covmer")
) # %>% filter(form == "or")

mdf <- read_tsv(
  "../results/G000006925-mapped-all_cigar.txt", 
  col_names = c("reference_genome", "read_id", "dist_mapping")
)

mashdf <- read_tsv(
  "../results/G000006925-mash_dist.txt", 
  col_names = c("query_genome", "reference_genome", "mash_dist")
)

df <- merge(
  ddf, mdf, 
  by = c("read_id", "reference_genome")
  )
df <- merge(
  ddf, mashdf,
  by = c("reference_genome")
)
df %>%  mutate(mash_dist_bin = cut(mash_dist, breaks = c(0, 0.025, 0.05, 0.1, 0.2, 1))) %>%
  ggplot() + 
  facet_wrap(c("mash_dist_bin"), scales = "free") +
  stat_ecdf() +
  aes(x=abs(avg_hdist/mash_dist)) + theme_cowplot() +
  scale_x_continuous(trans = "log1p", breaks = c(0,1,2,3,4)) +
  labs()

ddf %>% filter(genome_id == gid) %>% ggplot() + 
  aes(x=dist, y=hd_avg/29) +
  stat_density2d_filled() +
  stat_function(fun = function(x) x, color = "white") +
  scale_x_continuous(limits = c(0, 0.1)) + scale_y_continuous(limits = c(0.0, 0.1)) +
  theme_cowplot()

ddf %>% filter(genome_id == gid) %>%filter(covpos > 0.66) %>% ggplot() + 
  aes(x=dist) +
  stat_density() +
  theme_cowplot()
mdf %>% filter(genome_id == gid) %>% filter(dist < 0.5) %>% ggplot() + 
  aes(x=dist) +
  stat_density() +
  theme_cowplot()

df %>% filter(covpos > 0.66) %>% ggplot() + 
  aes(x=dist_krmt, y=dist_minimap) +
  stat_density_2d_filled() +
  stat_function(fun = function(x) x, color = "white") +
  scale_x_continuous(limits = c(0, 0.1)) + scale_y_continuous(limits = c(0, 0.1)) +
  theme_cowplot()

df %>%
  filter(covpos > 0.33) %>% 
  ggplot() + 
  aes(x=((dist_mapping-avg_hdist)/dist_mapping)) +
  stat_bin() +
  scale_x_continuous(limits = c(-2, 1)) +
  theme_cowplot()

df %>%
  filter(covpos > 0.33) %>% 
  ggplot() + 
  aes(x=((dist_mapping-dist_avg)/dist_mapping)) +
  stat_ecdf() +
  geom_vline(xintercept = 0.0, linetype = 3) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_cowplot()

df %>%
  filter(covpos > 0.66) %>%
  ggplot() + 
  aes(x=dist_mapping, y=dist_avg) +
  geom_point(alpha = 0.05) +
  stat_smooth(method = "loess") +
  stat_function(fun = function(x) x) +
  scale_x_continuous(limits = c(0, 0.1)) + scale_y_continuous(limits = c(0, 0.1)) +
  theme_cowplot()

## New
df %>% filter(genome_id == gid) %>% ggplot() + 
  aes(x=dist_avg, y=dist_mapping) +
  stat_density2d_filled() +
  # stat_function(fun = function(x) x, color = "white") +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  theme_cowplot()


df %>% filter(covpos > 0.0) %>% 
  # filter(X4 == 150) %>%
  filter(dist_mapping < 0.5) %>%
  # filter(dist_mash > 0.1) %>%
  ggplot() + 
  facet_wrap(c("dist_mash"), scale = "free") +
  # aes(x=dist_mapping, y=(dist_avg*covpos+(1-covpos)*avg_hdist)) +
  # aes(x=dist_mapping, y=avg_hdist) +
  aes(x=covpos, y=avg_hdist/dist_mash) +
  stat_density2d_filled(contour_var = "ndensity") +
  # stat_function(fun = function(x) x, color = "white") +
  # scale_y_continuous(limits = c(0, 0.2)) + scale_y_continuous(limits = c(0.0, 0.2)) +
  theme_cowplot()
df$x <- df$avg_hdist/df$dist_mapping
df %>% filter(covpos > 0.33) %>% 
  # filter(dist_mapping < 0.5) %>%
  # filter(X4 == 150) %>%
  # filter(dist_mash  < 0.02) %>%
  ggplot() + 
  # facet_wrap(c("dist_mash"), scale = "free") +
   # aes(x=((dist_avg*covpos+(1-covpos)*avg_hdist)/dist_mapping)) +
   # stat_bin(bins = 50) +
  stat_summary(aes(y=1.0, x=((dist_avg*covpos+(1-covpos)*avg_hdist)/dist_mash)), fun = 'mean', geom = "point", color = "red") + 
  # stat_summary(aes(y=1.0, x=((dist_avg*covpos+(1-covpos)*avg_hdist)/dist_mapping)), fun = 'mean_cl_normal', geom = "errorbar", color = "gray") + 
  # stat_summary(aes(x=1, y=((dist_avg/dist_mapping))), fun = 'mean', geom = "point", color = "blue") + 
  # stat_summary(aes(x=1, y=((avg_hdist/dist_mapping))), fun = 'mean', geom = "point", color = "green") + 
  scale_x_continuous(transform = "log1p") +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_cowplot()

df %>% # mutate(covpos = cut(covpos, breaks = c(0,0.33, 0.66, 1))) %>%
  # mutate(mash_dist_bin = cut(mash_dist,  breaks = c(0,0.05,0.1,0.25))) %>%
  # filter(X11+X12+X13+X14+X15+X16+X17+X18+X19 > 1) %>% 
  # filter(dist_mapping < 0.5) %>%
  # filter(covpos > 0.3) %>%
  ggplot() + 
  # facet_grid(cols = vars(mash_dist_bin), rows = vars(covpos) ,scale="free") +
  # aes(x=dist_mash, y=((dist_avg*covpos+(1-covpos)*avg_hdist))/dist_mash) +
  # geom_point(alpha = 0.5) +
  # geom_errorbar(stat = "summary", fun.data = mean_se) +
   # stat_summary_bin(aes(x=mash_dist, y=((dist_avg*covpos+(1-covpos)*avg_hdist)/mash_dist)),
   #                   fun = 'mean', color = "red") + 
   #stat_summary_bin(aes(x=mash_dist, y= dist_avg/mash_dist),
   #                  fun = 'mean', color = "blue") + 
  # stat_summary(aes(x=mash_dist, y= avg_hdist/mash_dist),
  #                 fun = 'mean', color = "purple") + 
  stat_summary(fun = mean, aes(x=mash_dist, y=avg_hdist/mash_dist)) +
  geom_hline(yintercept = 1) +
  # stat_summary(fun = 'mean', color = "red") + 
  # scale_y_continuous(transform = "log1p") +
  # coord_cartesian(ylim = c(0, 2)) +
  theme_cowplot() +
  labs(title="At least one match HD<5", x="Mash distance estimate", y = "LLH estimate / Mash estimate")


df %>% # filter(covpomean_sedf %>% # filter(covpos > 0.33) %>% 
  # filter(dist_mapping < 0.5) %>%
  # filter(X4 == 150) %>%
  ggplot() + 
  # geom_point(alpha = 0.5) +
  #stat_summary_bin(aes(x=covpos, y=((dist_avg*covpos+(1-covpos)*avg_hdist)/dist_mapping)),
  #                 fun = 'mean', color = "red") + 
  #stat_summary_bin(aes(x=covpos, y= dist_avg/dist_mapping),
  #fun = 'mean', color = "blue") + 
  stat_summary_bin(aes(x=covpos, y= avg_hdist/mash_dist, color = mash_dist, group = mash_dist),
                   fun = 'mean', alpha = 0.85) + 
  scale_color_continuous(type = "viridis") +
  geom_hline(yintercept = 1) +
  # stat_summary(fun = 'mean', color = "red") + 
  # scale_y_continuous(transform = "log1p") +
  coord_cartesian(ylim = c(0.0, 5)) +
  labs(x="Percentage of positions covered by at least one k-mer", y = "LLH estimate / Mash estimate", color="Mash distance") +
  theme_cowplot()

df %>% mutate(mash_dist = cut(mash_dist, breaks = c(0,0.01,0.025,0.05,0.1,0.25))) %>%
  ggplot(aes(x=covpos)) + 
  facet_wrap(c("mash_dist")) +
  stat_density() +
  labs(x="Percentage of positions covered by at least one k-mer", y = "Density") +
  # scale_y_continuous(trans = "log") +
  theme_cowplot()


df %>% filter(covpos > 0.5) %>% ggplot() + 
  aes(x=dist_avg) +
  stat_density() +
  theme_cowplot()

df  %>% ggplot() + 
  facet_wrap(c("dist_mash"), scale = "free") +
  aes(x=dist_avg, y=covpos) +
  stat_density2d_filled(contour_var = "ndensity") +
  stat_function(fun = function(x) x, color = "white") +
  # scale_x_continuous(limits = c(0, 0.125)) + scale_y_continuous(limits = c(0.0, 0.125)) +
  theme_cowplot()
