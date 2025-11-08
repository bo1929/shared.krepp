require(ggplot2)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)
require(vroom)
require(data.table)
require(ggnewscale)
require(scales)

df <- vroom(
  "../results/color_graph_stats.tsv",
  col_names = c("ID", "cardinality", "clade_colors", "nonclade_colors", "unobserved_colors", "cladexp_card")
  )

df %>% 
  filter(cardinality >= 4) %>%
  ggplot() +
  aes(x=cardinality, y = cladexp_card/cardinality) +
  stat_summary_bin(geom = "crossbar") +
  # geom_point(alpha = 0.1) + 
  scale_x_continuous(transform = "log2") +
  scale_y_continuous(labels = percent) +
  theme_minimal_grid() +
  labs(x = "Color cardinality", y = "% in clade colors")

df %>% 
  filter(cardinality >= 4) %>%
  mutate(card_bin = cut(cardinality, breaks = c(4, 16, 64, 256, max(cardinality)+1), right = FALSE, dig.lab = 10)) %>% 
  ggplot() +
  aes(x = cladexp_card/cardinality, color = card_bin) +
  stat_ecdf(linewidth = 1) +
  scale_x_continuous(labels = percent) +
  scale_color_brewer(palette = "Paired") +
  theme_minimal_grid() +
  labs(y = "ECDF", x = "% in clade colors", color="Cardinality")

df %>% 
  filter(cardinality >= 4) %>%
  mutate(card_bin = cut(cardinality, breaks = c(4, 8, 16, 32, 64, 128, 256, 512, 1024, max(cardinality)+1), right = FALSE, dig.lab = 10)) %>% 
  ggplot() +
  aes(x=card_bin, y = unobserved_colors/cardinality) +
  stat_summary_bin() +
  # scale_color_brewer(palette = "Paired") +
  theme_minimal_grid() +
  labs(y = "# of unobserved colors / cardinality", x = "Color cardinality") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5))

df %>% 
  filter(cardinality >= 4) %>%
  mutate(card_bin = cut(cardinality, breaks = c(4, 8, 16, 32, 64, 128, 256, 512, 1024, max(cardinality)+1), right = FALSE, dig.lab = 10)) %>% 
  ggplot() +
  aes(x=card_bin, y = nonclade_colors/cardinality) +
  stat_summary(fun.y = mean,
               fun.ymin = function(x) mean(x) - sd(x), 
               fun.ymax = function(x) mean(x) + sd(x), 
  ) +
  # scale_color_brewer(palette = "Paired") +
  scale_y_continuous(trans = "log2") +
  theme_minimal_grid() +
  labs(y = "# of non-clade colors", x = "Color cardinality") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5))


df <- vroom(
  "../results/out_degrees.tsv",
  col_names = c("ID", "out_degree", "count")
)

df %>% 
  filter(count == 0) %>%
  ggplot() +
  aes(x = out_degree > 1) +
  geom_bar( ) +
  theme_minimal_grid() +
  labs(y = "ECDF", x = "Out degree > 1") 

df %>% 
  ggplot() +
  aes(x = out_degree) +
  stat_ecdf( ) +
  theme_minimal_grid() +
  scale_x_continuous(trans = "log2") +
  labs(y = "ECDF", x = "Out degree")