require(ggplot2)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)
require(vroom)
require(data.table)
require(ggnewscale)
require(scales)

df <- vroom("../results/resource_benchmarking.tsv")
df$reference_size <- NaN
df$reference_size[df$data == "WoLv2"] <- 15953
df$reference_size[df$data == "WoLv1"] <- 10575
df$reference_size[df$data == "WoLv1_sampled2000"] <- 2000
df$reference_size[df$data == "WoLv1_sampled5000"] <- 5000
  
df %>% filter(process == "query") %>%
  filter(reference_size > 2000) %>%
  ggplot() +
  aes(x = size, y = time_sec, color = method, linetype = as.factor(reference_size)) +
  geom_line() +
  geom_point(aes(shape = method)) +
  labs(x = "Number of reads", y = "Running time (minutes)", linetype = "Reference size", shape = "Method", color = "Method") +
  scale_x_continuous(breaks = c(100000, 1000000, 10000000), trans="log10", labels=c("100K", "1M", "10M")) +
  # scale_y_continuous(breaks = c(60, 300, 900, 1800), labels=c("1", "5", "15", "30"), trans="log2") +
  scale_y_continuous(breaks = c(60, 600, 1200, 1800), labels=c("1", "10", "20", "30")) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme_minimal_grid()
# ggsave2("../figures/running_time-x_nreads-logy.pdf", height = 3, width = 3.75)
ggsave2("../figures/running_time-x_nreads.pdf", height = 3, width = 3.75)

df %>% filter(process == "query") %>%
  # filter(reference_size > 2000) %>%
  ggplot() +
  aes(x = reference_size, y = time_sec, color = method, linetype = as.factor(size)) +
  geom_line() +
  geom_point(aes(shape = method), size = 3, alpha = 0.75) +
  labs(x = "# of references", y = "Running time (minutes)", linetype = "# of reads", shape = "Method", color = "Method") +
  scale_linetype_manual(values = c(3, 2, 1), labels=c("100K", "1M", "10M")) +
  scale_y_continuous(breaks = c(60, 300, 900, 1800), labels=c("1", "5", "15", "30"), trans="log2") +
  scale_x_continuous(breaks = c(2000, 5000, 10000, 16000), labels=c("2K", "5K", "10K", "16K"), trans="log2") +
  # scale_y_continuous(breaks = c(60, 600, 1200, 1800), labels=c("1", "10", "20", "30")) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme_minimal_grid()
ggsave2("../figures/running_time-x_nreferences-logxy.pdf", height = 4, width = 5)
# ggsave2("../figures/running_time-x_nreferences.pdf", height = 3, width = 3.75)