require(ggplot2)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)
require(vroom)
require(data.table)
require(ggnewscale)
require(scales)

dfm <- vroom(
  "../results/algorithmic_evaluation/match_stats-krepp_dth4-all.tsv",
  col_names = c("qid", "rid", "hdmin")
)
nreads <- nrow(dfm)
dfd <- vroom(
  "../misc_data/data-WoLv1_placement/dist_to_closest.txt",
  col_names = c("qid", "rid", "dist")
)
dfm <- merge(dfm, dfd, by="qid")
nrow(dfm)
dfm %>% group_by(dist=cut(dist, c(0, 0.025, 0.05, 0.75, 0.1, 0.125, 0.15, 0.175, 0.2, 0.25, 0.33))) %>% 
  summarise(
    portion_le0=sum(hdmin==0, na.rm = TRUE)/n(),
    portion_le1=sum(hdmin<=1, na.rm = TRUE)/n(),
    portion_le2=sum(hdmin<=2, na.rm = TRUE)/n(),
    portion_le3=sum(hdmin<=3, na.rm = TRUE)/n(),
    portion_le4=sum(hdmin<=4, na.rm = TRUE)/n(),
  ) %>%
  ggplot() +
  aes(x=dist) +
  geom_line(aes(y=portion_le0, group=1), color="gray") +
  geom_line(aes(y=portion_le1, group=2), color="gray") +
  geom_line(aes(y=portion_le2, group=3), color="gray") +
  geom_line(aes(y=portion_le3, group=4), color="gray") +
  geom_line(aes(y=portion_le4, group=5), color="gray") +
  geom_point(aes(y=portion_le0, color="HD=0", group=1), size=4, alpha=0.85) +
  geom_point(aes(y=portion_le1, color="HD≤1", group=2), size=4, alpha=0.85) +
  geom_point(aes(y=portion_le2, color="HD≤2", group=3), size=4, alpha=0.85) +
  geom_point(aes(y=portion_le3, color="HD≤3", group=4), size=4, alpha=0.85) +
  geom_point(aes(y=portion_le4, color="HD≤4", group=5), size=4, alpha=0.85) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c("#fb9a99", "#fdbf6f", "#b2df8a", "#a6cee3", "darkgray")) +
  theme_half_open() +
  background_grid() +
  theme(axis.text.x = element_text(angle=35, vjust = 0.6)) +
  labs(x="Distance to the closest", y="Reads with ≥1 match", color="29-mer")
ggsave("../figures/match_counts_hamming_dist.pdf")

dfg <- vroom("../results/algorithmic_evaluation/multitree_heights_info-WoLv2.tsv",
             col_names = c("Tree", "r", "ix", "h", "nkmers"))

all_nkmers <- sum((dfg %>% filter(Tree == "WoLv2 tree"))$nkmers)
dfs <- dfg %>% group_by(Tree, h) %>% summarise(nkmers = sum(nkmers))

dfg %>% filter(Tree == "WoLv2 tree") %>% group_by(h) %>% summarise(nkmers = sum(nkmers)) %>% 
  ggplot() +
  aes(x=h, y=nkmers) +
  geom_col() + 
  scale_y_continuous(labels = function(x)percent(x/all_nkmers), trans = "log10", breaks = c(0, all_nkmers/100000000, all_nkmers/10000000, all_nkmers/1000000, all_nkmers/100000, all_nkmers/10000, all_nkmers/1000, all_nkmers/100, all_nkmers/10)) +
  labs(x="Multitree height", y="% of k-mers") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50)) +
  theme_half_open() +
  background_grid()
ggsave("../figures/multitree_height-percent.pdf")

dfg %>% filter(Tree == "WoLv2 tree") %>% group_by(h) %>% summarise(nkmers = sum(nkmers)) %>% 
  ggplot() +
  aes(x=h, y=nkmers) +
  geom_col() + 
  labs(x="Multitree height", y="# of k-mers") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50)) +
  theme_half_open() +
  background_grid()
ggsave("../figures/multitree_height-count.pdf", width = 4.5, height = 2.5)

dfg %>% group_by(h, Tree) %>% summarise(nkmers = sum(nkmers), count = n()) %>% 
  ggplot() +
  aes(x=h, y=nkmers, fill=Tree) +
  geom_col(position = position_dodge2()) + 
  # scale_y_continuous(labels = function(x)percent(x/all_nkmers), trans = "pseudo_log", breaks = c(-all_nkmers/100, -all_nkmers/1000, -all_nkmers/10000, -all_nkmers/100000, 0, all_nkmers/100000, all_nkmers/10000, all_nkmers/1000, all_nkmers/100, all_nkmers/10)) +
  labs(x="Multitree height", y="# of k-mers") +
  scale_y_continuous(trans = "pseudo_log") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50)) +
  coord_cartesian(xlim = c(0, 30)) +
  theme_half_open() +
  background_grid()

dfg <- dfg %>% group_by(h, Tree) %>% summarise(nkmers = sum(nkmers), count = n()) %>% 
  pivot_wider(id_cols = h, names_from = Tree, values_from = c("nkmers", "count"))

dfg %>%
  ggplot() +
  aes(x=h, y=`nkmers_Random tree` - `nkmers_WoLv2 tree`) +
  geom_col() + 
  # scale_y_continuous(labels = function(x)percent(x/all_nkmers), trans = "pseudo_log", breaks = c(-all_nkmers/100, -all_nkmers/1000, -all_nkmers/10000, -all_nkmers/100000, 0, all_nkmers/100000, all_nkmers/10000, all_nkmers/1000, all_nkmers/100, all_nkmers/10)) +
  # scale_y_continuous(trans = "pseudo_log") +
  # scale_x_continuous(breaks = c(0,5,10,15,20)) +
  coord_cartesian(xlim = c(0, 15), ylim=c(-2.5e8, 2e8)) +
  labs(x="Multitree height", y="# of k-mers (Random tree - WoLv2 tree)") +
  theme_half_open() +
  background_grid()
ggsave("../figures/multitree_height_diff-count-random_vs_wolv2.pdf", width = 4, height = 4)

dfg %>%
  ggplot() +
  aes(x=h, y=`nkmers_Ladder tree` - `nkmers_WoLv2 tree`) +
  geom_col() + 
  # scale_y_continuous(labels = function(x)percent(x/all_nkmers), trans = "pseudo_log", breaks = c(-all_nkmers/100, -all_nkmers/1000, -all_nkmers/10000, -all_nkmers/100000, 0, all_nkmers/100000, all_nkmers/10000, all_nkmers/1000, all_nkmers/100, all_nkmers/10)) +
  # scale_y_continuous(trans = "pseudo_log") +
  # scale_x_continuous(breaks = c(0,5,10,15,20)) +
  coord_cartesian(xlim = c(0, 15), ylim=c(-2.5e8, 2e8)) +
  labs(x="Multitree height", y="# of k-mers (Ladder tree - WoLv2 tree)") +
  theme_half_open() +
  background_grid()
ggsave("../figures/multitree_height_diff-count-ladder_vs_wolv2.pdf", width = 4, height = 4)

dfg %>% 
  ggplot() +
  aes(x=h, y=`count_Random tree` - `count_WoLv2 tree`) +
  geom_col() + 
  # scale_y_continuous(labels = function(x)percent(x/all_nkmers), trans = "pseudo_log", breaks = c(-all_nkmers/100, -all_nkmers/1000, -all_nkmers/10000, -all_nkmers/100000, 0, all_nkmers/100000, all_nkmers/10000, all_nkmers/1000, all_nkmers/100, all_nkmers/10)) +
  # scale_y_continuous(trans = "pseudo_log") +
  # scale_x_continuous(breaks = c(0,5,10,15,20)) +
  # coord_cartesian(xlim = c(0, 5)) +
  labs(x="Multitree height", y="# of colors (Random tree - WoLv2 tree)") +
  theme_half_open() +
  background_grid()

dfg %>% 
  ggplot() +
  aes(x=h, y=`count_Ladder tree` - `count_WoLv2 tree`) +
  geom_col() + 
  # scale_y_continuous(labels = function(x)percent(x/all_nkmers), trans = "pseudo_log", breaks = c(-all_nkmers/100, -all_nkmers/1000, -all_nkmers/10000, -all_nkmers/100000, 0, all_nkmers/100000, all_nkmers/10000, all_nkmers/1000, all_nkmers/100, all_nkmers/10)) +
  # scale_y_continuous(trans = "pseudo_log") +
  # scale_x_continuous(breaks = c(0,5,10,15,20)) +
  # coord_cartesian(xlim = c(0, 5)) +
  labs(x="Multitree height", y="# of colors (Random tree - WoLv2 tree)") +
  theme_half_open() +
  background_grid()

dfg %>%
  ggplot() +
  # aes(x=h, y=`Random tree` - `WoL-v2 tree`) +
  aes(x=h, y=`count_Random tree` - `count_WoLv2 tree`) +
  geom_col() + 
  labs(x="Multitree height", y="# of colors (Random tree - WoLv2 tree)") +
  theme_half_open() +
  background_grid()
