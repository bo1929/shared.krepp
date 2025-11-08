require(ggplot2)
require(cowplot)
require(dplyr)
require(reshape2)
require(tidyr)
require(vroom)
require(data.table)
require(ggnewscale)
require(scales)

df <- vroom("../results/alignment_comparison/resource_benchmarking.tsv")
df$reference_size <- NaN
df$reference_size[df$data == "WoLv2"] <- 15953
df$reference_size[df$data == "WoLv1"] <- 10575
df$reference_size[df$data == "WoLv1_sampled2000"] <- 2000
df$reference_size[df$data == "WoLv1_sampled5000"] <- 5000
df$reference_size[df$data == "RefSeq (CAMI-II) [dedup]"] <- 50752
df$reference_size[df$data == "RefSeq (CAMI-II)"] <- 123853
df$reference <- NaN
df$reference[df$data == "WoLv2"] <- "WoLv2 (16K)"
df$reference[df$data == "WoLv1"] <- "WoLv1 (11K)"
df$reference[df$data == "WoLv1_sampled2000"] <- "WoLv2 (2K)"
df$reference[df$data == "WoLv1_sampled5000"] <- "WoLv2 (5K)"
df$reference[df$data == "RefSeq (CAMI-II) [dedup]"] <- "RefSeq (51K)"
df$reference[df$data == "RefSeq (CAMI-II)"] <- "RefSeq (124K)"
df <- df %>% filter(!(process == "query" & method == "krepp-v000"))
df$method[df$method != "bowtie2"] <- "krepp"

df %>% filter(process == "query") %>%
  filter(reference_size > 2000 & reference_size < 50000) %>%
  ggplot() +
  aes(x = size, y = time_sec, color = method, linetype = as.factor(reference_size)) +
  geom_line() +
  geom_point(aes(shape = method)) +
  labs(x = "Number of reads", y = "Running time (minutes)", linetype = "Reference size", shape = "Method", color = "Method") +
  scale_x_continuous(breaks = c(100000, 1000000, 10000000), trans="log10", labels=c("100K", "1M", "10M")) +
  scale_y_continuous(breaks = c(60, 120, 300, 900), labels=c("1", "5", "10", "15"), transform = "log2") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme_minimal_grid()
ggsave2("../figures/running_time-x_nreads.pdf", height = 3, width = 3.75)

df %>% filter(process == "query") %>%
  # filter(reference_size > 2000 & reference_size < 50000) %>%
  filter(size > 1000000) %>%
  ggplot() +
  aes(x = reference_size, y = time_sec, color = method) +
  geom_line() +
  # stat_smooth(method="lm",se=F)+
  geom_point(aes(shape = method), size = 3, alpha = 0.75) +
  # labs(x = "Number of references", y = "Running time (minutes)", linetype = "Reads", shape = "Method", color = "Method") +
  labs(x = "Number of reference genomes", y = "Running time (minutes)", linetype = "Reads", shape = "Method", color = "Method") +
  # scale_linetype_manual(values = c(2, 1), labels=c("1M", "10M")) +
  scale_linetype_manual(values = c(1), labels=c("10M")) +
  scale_y_continuous(breaks = c(1*60, 10*60, 20*60, 30*60), labels=c("1", "10", "20", "30")) +
  scale_x_continuous(transform = "log2", breaks = c(2000, 8000, 32000, 128000)) +
  # scale_color_manual(values = c("#377eb8", "#ff7f00", "#e41a1c")) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme_minimal_grid()
# ggsave2("../figures/running_time-x_nreferences-logxy.pdf", height = 4, width = 5)
# ggsave2("../figures/running_time-x_nreferences.pdf", height = 3, width = 3.75)
ggsave2("../figures/running_time-x_nreferences-logxy.pdf", height = 3, width = 3.75)
sublinear_model <- formula(time_sec ~ a * reference_size^b)
start_values <- list(a = 1, b = 0.5)
nls(sublinear_model, data = df %>% filter(process == "query" & method == "krepp"), start = start_values)
nls(sublinear_model, data = df %>% filter(process == "query" & method == "bowtie2"), start = start_values)
lm(time_sec ~ reference_size, data = df %>% filter(process == "query" & method == "krepp"))
df %>% filter(process == "query") %>%
  # filter(reference_size > 2000 & reference_size < 50000) %>%
  filter(size > 1000000) %>%
  ggplot() +
  aes(x = reference_size, y = time_sec, color = method) +
  #geom_line() +
  stat_smooth(method="lm",se=F)+
  geom_point(aes(shape = method), size = 3, alpha = 0.75) +
  # labs(x = "Number of references", y = "Running time (minutes)", linetype = "Reads", shape = "Method", color = "Method") +
  labs(x = "Number of reference genomes", y = "Running time (minutes)", linetype = "Reads", shape = "Method", color = "Method") +
  # scale_linetype_manual(values = c(2, 1), labels=c("1M", "10M")) +
  scale_linetype_manual(values = c(1), labels=c("10M")) +
  scale_y_continuous(transform = "log2", breaks = c(1*60, 2*60, 5*60, 15*60, 30*60), labels=c("1", "2", "5", "15", "30")) +
  scale_x_continuous(transform = "log2", breaks = c(2000, 8000, 32000, 128000)) +
  # scale_color_manual(values = c("#377eb8", "#ff7f00", "#e41a1c")) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme_minimal_grid()
ggsave2("../figures/running_time-x_nreferences-logxlogy.pdf", height = 3, width = 3.75)

df %>% filter(!is.na(time_sec)) %>% filter(grepl("index", process)) %>% 
  # filter(reference_size < 16000) %>% 
  # filter((reference_size < 15000) | grepl("krepp", method)) %>%
  group_by(method, reference_size, size) %>% mutate(time_sec = cumsum(time_sec)/60/60) %>%
  ggplot() +
  aes(x = reference_size, y = time_sec, color = method) +
  geom_point(size = 0.75, alpha = 0.9) +
  stat_summary(fun = "max", geom = "point", size=2) +
  stat_summary(fun = "max", geom = "line") +
  stat_summary(aes(group=method), fun = "max", geom = "point", color="darkgray", size=1.25) +
  labs(x = "Number of references", y = "Running time (hours)", shape = "Method", color = "Method") +
  # scale_y_continuous(breaks = c(0, 100*60, 200*60, 300*60, 400*60), labels=c("0", "100", "200", "300", "400")) +
  scale_color_brewer(palette = "Set1", direction = -1, guide="none") +
  scale_x_continuous(transform = "log2", breaks = c(4000, 8000, 16000, 32000, 64000, 128000)) +
  # scale_y_continuous(transform = "log2") +
  # coord_cartesian(ylim = c(0, 450*60)) +
  theme_minimal_grid()
  ggsave2("../figures/indexing_time-x_nreferences-logx.pdf", height = 3.5, width = 5)

  df %>% filter(!is.na(time_sec)) %>% filter(grepl("index", process)) %>% filter(grepl("krepp", method)) %>%
    group_by(method, reference_size, size) %>% mutate(time_sec = cumsum(time_sec)/60/60) %>%
    ggplot() +
    aes(x = reference_size, y = time_sec, color = method) +
    geom_point(size = 1.5, alpha = 0.9) +
    stat_summary(fun = "max", geom = "point", size=3.5) +
    # stat_summary(fun = "max", geom = "line") +
    stat_summary(aes(group=method), fun = "max", geom = "point", color="darkgray", size=1.25) +
    labs(x = "Number of references", y = "Running time (hours)", shape = "Method", color = "Method") +
    # scale_y_continuous(breaks = c(0, 100*60, 200*60, 300*60, 400*60), labels=c("0", "100", "200", "300", "400")) +
    scale_color_brewer(palette = "Set1", direction = -1, guide="none") +
    scale_x_continuous(transform = "log2") +
    # scale_y_continuous(transform = "log2") +
    # coord_cartesian(ylim = c(0, 450*60)) +
    theme_minimal_grid()

df %>% filter(!is.na(time_sec)) %>% filter(grepl("index", process)) %>% filter(reference_size < 15000) %>%
    group_by(method, reference_size, size) %>% mutate(time_sec = (time_sec)/60/60) %>%
    ggplot() +
    facet_wrap(~reorder(reference, reference_size), scale="free_y") +
    aes(x = method, y = time_sec, fill = method) +
    geom_col(size = 0.25, alpha = 0.9, color="black") +
    labs(x = "Number of references", y = "Running time (hours)", shape = "Method", color = "Method") +
    # scale_y_continuous(breaks = c(0, 100*60, 200*60, 300*60, 400*60), labels=c("0", "100", "200", "300", "400")) +
    scale_fill_brewer(palette = "Set1", direction = -1, guide="none") +
    # scale_y_continuous(transform = "log2") +
    # coord_cartesian(ylim = c(0, 450*60)) +
    theme_minimal_grid()

df %>% filter(process == "query") %>%
  group_by(data, reference_size, method) %>% summarize(peak_memory=max(peak_memory)) %>%
  ggplot() +
  aes(x = as.factor(reference_size), y = as.numeric(peak_memory), fill = method) +
  geom_col(position = position_dodge2(preserve = "single"), color="black", width = ) +
  labs(x = "Reference", y = "Peak memory (GB)", fill = "Method", color = "Method") +
  scale_x_discrete(labels = c("WoLv1\n(2000)", "WoLv1\n(5000)", "WoLv1\n(10575)", "WoLv2\n(15953)", "RefSeq\n(50752)", "RefSeq\n(123853)")) +
  scale_y_continuous(breaks = c(5*10**6, 50*10**6, 100*10**6, 150*10**6, 200*10**6), labels=c("5", "50", "100", "150", "200")) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  theme_minimal_grid()
ggsave2("~/Documents/krepp_talk_figures/querying-memory.pdf", width = 5.8, height=3)

df %>% filter(process == "query") %>%
  group_by(data, reference_size, method) %>% summarize(peak_memory=max(peak_memory)) %>%
  ggplot() +
  aes(x = reference_size, y = as.numeric(peak_memory)) +
  geom_line(aes(group=method, color = method), linetype=2, linewidth=1) +
  geom_point(aes(color = method), size=3.5) +
  geom_point(color = "darkgray", size=1.25) +
  labs(x = "Reference size", y = "Peak memory (GB)", fill = "Method", color = "Method") +
  scale_x_continuous(transform = "log2", breaks = c(2000, 8000, 32000, 128000)) +
  scale_y_continuous(breaks = c(5*10**6, 50*10**6, 100*10**6, 150*10**6, 200*10**6), labels=c("5", "50", "100", "150", "200")) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  theme_minimal_grid()

df %>% filter(grepl("index", process)) %>%
  ggplot() +
  aes(x = as.factor(reference_size), y = peak_memory, fill = method) +
  geom_col(position = position_dodge2(preserve = "total"), size=0.0) +
  labs(x = "Reference", y = "Peak memory (GB)", fill = "Method", color = "Method") +
  scale_x_discrete(labels = c("WoLv1\n(2000)", "WoLv1\n(5000)", "WoLv1\n(10575)", "WoLv2\n(15953)", "RefSeq\n(50752)", "RefSeq\n(123853)")) +
  scale_y_continuous(breaks = c(0, 50*10**6, 100*10**6, 150*10**6, 200*10**6, 250*10**6), labels=c("0", "50", "100", "150", "200", "250")) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  theme_minimal_grid()
ggsave2("~/Documents/krepp_talk_figures/indexing-memory.pdf", width = 5.8, height=3)


df %>% filter(grepl("index", process)) %>%
  ggplot() +
  aes(x = as.factor(reference_size), y = peak_memory, fill = method) +
  # geom_col(position = position_dodge2(), color="black", linewidth = 0.1) +
  stat_summary(fun = "max", geom = "col", size=0.1, position = position_dodge2(preserve = 'single'), na.rm = FALSE) +
  labs(x = "Reference", y = "Peak memory (GB)", fill = "Method", color = "Method") +
  scale_x_discrete(labels = c("WoLv1\n(2000)", "WoLv1\n(5000)", "WoLv1\n(10575)", "WoLv2\n(15953)", "RefSeq\n(50752)", "RefSeq\n(123853)")) +
  scale_y_continuous(breaks = c(0, 50*10**6, 100*10**6, 150*10**6, 200*10**6, 250*10**6), labels=c("0", "50", "100", "150", "200", "250")) +
  scale_fill_brewer(palette = "Set1", direction = -1, guide="none") +
  theme_minimal_grid()
