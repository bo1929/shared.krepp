require(ggplot2)
require(readr)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)

df_err_c <- read_tsv(
  "../results/all-placement_wrt_closest-error.tsv",
  col_names = c("genome_id", "read_id", "placement", "error")
)

df_err_t <- read_tsv(
  "../results/all-placement_wrt_tau-error.tsv",
  col_names = c("genome_id", "read_id", "placement", "error", "min_d", "tau")
)

df_err = merge(df_err_t, df_err_c, by = c("genome_id", "read_id"), suffixes = c("_wrt_tau", "_wrt_closest"))

df_err %>% filter(error_wrt_tau != "UP") %>% ggplot() +
  facet_wrap(c("genome_id"), scale="free") + 
  stat_ecdf(aes(x=error_wrt_tau)) +
  # stat_bin(aes(x=error_wrt_tau)) +
  labs(x = "Placement error [dist(LCA, query) + dist(placement, LCA)]", y = "# of reads") +
  scale_x_continuous(trans="log1p") + 
  theme_cowplot()

df_err %>% filter(error_wrt_tau != "UP") %>% ggplot() +
  # facet_wrap(c("genome_id"), scale="free") + 
  stat_ecdf(aes(x=error_wrt_tau, color=genome_id)) +
  # stat_bin(aes(x=error_wrt_tau)) +
  labs(x = "Placement error [dist(LCA, query) + dist(placement, LCA)]", y = "# of reads") +
  scale_x_continuous(trans="log1p", breaks=c(0, 1,2,5,10,20)) + 
  theme_cowplot()

df_err %>% filter(error_wrt_tau != "UP") %>% ggplot() +
  facet_wrap(c("genome_id"), scale="free") + 
  stat_density_2d_filled(contour = TRUE, contour_var = "ndensity", aes(y=error_wrt_tau, x=tau)) +
  labs(x = "Placement error [dist(LCA, query) + dist(placement, LCA)]", y = "# of reads") +
  scale_y_continuous(trans="log1p", breaks=c(0, 1,2,5,10,20)) + 
  xlim(c(-1, 1)) +
  theme_cowplot()

df_err %>% ggplot() +
  geom_bar(aes(x=placement_wrt_closest != "UP", y = (after_stat(count))/sum(after_stat(count)))) +
  labs(x = "Sufficiently covered and placed", y = "Portion of reads") +
  theme_cowplot()

df_err %>% ggplot() +
  stat_bin(aes(x=abs(tau))) +
  scale_y_continuous(trans="log2") +
  scale_x_continuous(trans="log1p") + 
  theme_cowplot()
