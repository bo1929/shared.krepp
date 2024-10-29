require(ggplot2)
require(readr)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)

mdf <- read_tsv("../results/mixture_queries-matching_stats-k29w35-gt_HDdiv2.tsv")
mdf$HD <- as.factor(mdf$HD)

p1 <- mdf %>% ggplot() + # facet_wrap(facets = c("HD"), ncol = 2) + 
  aes(x = PCOV, color = HD) + 
  stat_ecdf(linewidth = 1.5, alpha = 0.75) +
  theme_cowplot() + 
  scale_color_brewer(palette = "Paired", direction = -1) +
  labs(y = "ECDF", x = "Portion of loci covered by at least one k-mer match ≤ HD", color = "HD")
p2 <- mdf %>% ggplot() + # facet_wrap(facets = c("HD"), ncol = 2) + 
  aes(x = MCOV, color = HD) + 
  stat_ecdf(linewidth = 1.5, alpha = 0.75) +
  theme_cowplot() + 
  scale_color_brewer(palette = "Paired", direction = -1) + 
  labs(y = "ECDF", x = "Portion of k-mers with at least one match ≤ HD", color = "HD")
plot_grid(p1, p2, ncol=2)

mdf %>% ggplot() + # facet_wrap(facets = c("HD"), ncol = 2) + 
  aes(x = DIST, color = HD, y = MCOV) + 
  stat_smooth(se = F, span = 0.7, method = "glm", method.args = list(family = binomial), size = 1) +  theme_cowplot() + 
  scale_color_brewer(palette = "Paired", direction = -1)

mdf %>% mutate(DIST_BIN = cut(
      DIST,
      include.lowest = TRUE,
      breaks = c(0.35, 0.2, 0.1, 0.05, 0.025, 0.001, 0.0)
    )
  ) %>% ggplot() + facet_wrap(facets = c("DIST_BIN"), scale = "free", ncol = 3) + 
  aes(x = HD, color = HD, y = PCOV) + 
  geom_violin(scale = "width") +
  stat_summary(fun.data = "mean_se", color = "black") +
  theme_cowplot() +
  scale_color_brewer(palette = "Paired", direction = -1) +
  labs(y = "Mean portion of loci covered by at least one k-mer match ≤ HD", color = "HD")
mdf %>% mutate(DIST_BIN = cut(
    DIST,
    include.lowest = TRUE,
    breaks = c(0.35, 0.2, 0.1, 0.05, 0.025, 0.001, 0.0)
    )
  ) %>% ggplot() + facet_wrap(facets = c("DIST_BIN"), scale = "free", ncol = 3) + 
  aes(x = HD, color = HD, y = MCOV) + 
  geom_violin(scale = "width") +
  stat_summary(fun.data = "mean_se", color = "black") +
  theme_cowplot() +
  scale_color_brewer(palette = "Paired", direction = -1) +
  labs(y = "Portion of k-mers with at least one match ≤ HD", color = "HD")

mdf %>% ggplot() + # facet_wrap(facets = c("HD"), scale = "free", ncol = 3) + 
  aes(x = NNODES, color = HD) + 
  stat_ecdf(linewidth = 1.25) +
  theme_cowplot() +
  scale_color_brewer(palette = "Paired", direction = -1) +
  scale_x_continuous(transform = "log2") +
  labs(y = "ECDF", color = "HD", x = "Total number of tree nodes with at least one match = HD")
psb <- mdf %>% ggplot() + facet_wrap(facets = c("HD"), scale = "free", ncol = 3) + 
  aes(x = DIST, y = NNODES, color = HD) + 
  stat_smooth(method = "glm", color = "darkgray") +
  stat_summary_bin(bins = 5, fun.data = "mean_cl_boot") +
  theme_cowplot() +
  scale_color_brewer(palette = "Paired", direction = -1) +
  labs(x = "Distante to the closest", color = "HD", y = "Total number of tree nodes with at least one match = HD")
psb

cdf <- read_tsv("../results/mixture_queries-coverage_info-cond_hdist3.tsv")
sdf <- cdf %>% 
  group_by(READ_ID, MIN_GND, NGENOMES) %>% 
  summarise(NUM_ABOVE_THOLD = sum(GENOME_COV > 0.5), 
            MAX_COV = max(GENOME_COV), 
            MAX2_COV = max(GENOME_COV[GENOME_COV!=max(GENOME_COV)]),
            DIST_BIN = cut(MIN_GND, include.lowest = TRUE, breaks = c(0.35, 0.2, 0.1, 0.05, 0.025, 0.001, 0.0)))

p1 <- ggplot(sdf) + aes(x = NGENOMES) + 
  facet_wrap(c("DIST_BIN"), scales = "free") + 
  geom_histogram(aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), fill="gray", color="darkblue") +
  theme_cowplot() + 
  labs(title = "All matching genomes", x = "Number of genomes", y = "Portion of the reads")
p1
p2 <- ggplot(sdf) + aes(x = NUM_ABOVE_THOLD) + 
  facet_wrap(c("DIST_BIN"), scales = "free") + 
  geom_histogram(aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), fill="gray", color="darkblue") +
  theme_cowplot() + 
  scale_x_continuous(transform = "log1p", breaks = c(0, 1, 10, 100, 1000)) + 
  labs(title = "Genomes with coverage more than the threshold 0.5", x = "Number of genomes", y = "Portion of the reads")
p2
p3 <- ggplot(sdf) + aes(x = MAX_COV)  + 
  facet_wrap(c("DIST_BIN"), scales = "free") + 
  geom_histogram(aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), fill="gray", color="darkblue") +
  labs(title = "Maximum reference coverage", x = "Coverage", y = "Portion of the reads") +
  theme_cowplot()
p3
p4 <- ggplot(sdf) + aes(x = MAX2_COV)  + 
  facet_wrap(c("DIST_BIN"), scales = "free") + 
  geom_histogram(aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), fill="gray", color="darkblue") +
  labs(title = "Secnod largest reference coverage", x = "Coverage", y = "Portion of the reads") +
  theme_cowplot()
p4
