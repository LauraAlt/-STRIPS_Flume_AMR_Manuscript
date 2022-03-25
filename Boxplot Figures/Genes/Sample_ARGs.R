library("readxl")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("scales")

#16S rRNA#

high_flow_rRNA <- read_excel("../Boxplot Data.xlsx", sheet = "High Flow_Clean") %>%
  subset(Contaminant == "16S_rRNA") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 9",
                           Sample == "Downstream" ~ "Runoff \n n = 9",
                           Sample == "Infiltration" ~ "Infiltration \n n = 9"))

levels_h = c("Run-on \n n = 9", "Runoff \n n = 9", "Infiltration \n n = 9")

medium_flow_rRNA <- read_excel("../Boxplot Data.xlsx", sheet = "Medium Flow_Clean") %>%
  subset(Contaminant == "16S_rRNA") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 5",
                           Sample == "Downstream" ~ "Runoff \n n = 5",
                           Sample == "Infiltration" ~ "Infiltration \n n = 7"))

levels_m = c("Run-on \n n = 5", "Runoff \n n = 5", "Infiltration \n n = 7")

low_flow_rRNA <- read_excel("../Boxplot Data.xlsx", sheet = "Low Flow_Clean") %>%
  subset(Contaminant == "16S_rRNA") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 6",
                           Sample == "Downstream" ~ "Runoff \n n = 8",
                           Sample == "Infiltration" ~ "Infiltration \n n = 8"))

levels_l = c("Run-on \n n = 6", "Runoff \n n = 8", "Infiltration \n n = 8")

high_flow_rRNA_plot <- ggplot(high_flow_rRNA, aes(ordered(x = Label, levels = levels_h), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 9", size = 6) +
  scale_y_log10(limits = c(299000000, 24000000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("High Flow") +
  ylab(bquote("16S rRNA (copies 100"~mL^-1*")")) +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_text(family = "Times New Roman", size = 24),
        legend.position = "none")

high_flow_rRNA_plot

medium_flow_rRNA_plot <- ggplot(medium_flow_rRNA, aes(ordered(x = Label, levels = levels_m), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 5", size = 6) +
  scale_y_log10(limits = c(299000000, 24000000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Medium Flow") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_blank(),
        legend.position = "none")

medium_flow_rRNA_plot

low_flow_rRNA_plot <- ggplot(low_flow_rRNA, aes(ordered(x = Label, levels = levels_l), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 6", hide.ns = TRUE, size = 6) +
  scale_y_log10(limits = c(299000000, 24000000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Low Flow") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_blank(),
        legend.position = "none")

low_flow_rRNA_plot

ggsave("High_Flow_rRNA.tiff", plot = high_flow_rRNA_plot,
       width = 6, height = 6, dpi = 300, units = c("in"))

ggsave("Medium_Flow_rRNA.tiff", plot = medium_flow_rRNA_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))

ggsave("Low_Flow_rRNA.tiff", plot = low_flow_rRNA_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))

#tetM#

high_flow_tetM <- read_excel("../Boxplot Data.xlsx", sheet = "High Flow_Clean") %>%
  subset(Contaminant == "tetM") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 9",
                           Sample == "Downstream" ~ "Runoff \n n = 9",
                           Sample == "Infiltration" ~ "Infiltration \n n = 9"))

levels_h = c("Run-on \n n = 9", "Runoff \n n = 9", "Infiltration \n n = 9")

medium_flow_tetM <- read_excel("../Boxplot Data.xlsx", sheet = "Medium Flow_Clean") %>%
  subset(Contaminant == "tetM") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 5",
                           Sample == "Downstream" ~ "Runoff \n n = 5",
                           Sample == "Infiltration" ~ "Infiltration \n n = 7"))

levels_m = c("Run-on \n n = 5", "Runoff \n n = 5", "Infiltration \n n = 7")

low_flow_tetM <- read_excel("../Boxplot Data.xlsx", sheet = "Low Flow_Clean") %>%
  subset(Contaminant == "tetM") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 6",
                           Sample == "Downstream" ~ "Runoff \n n = 8",
                           Sample == "Infiltration" ~ "Infiltration \n n = 8"))

levels_l = c("Run-on \n n = 6", "Runoff \n n = 8", "Infiltration \n n = 8")

high_flow_tetM_plot <- ggplot(high_flow_tetM, aes(ordered(x = Label, levels = levels_h), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 9", size = 6) +
  scale_y_log10(limits = c(4590000, 775000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("High Flow") +
  theme_bw() +
  ylab(bquote(italic(.("tetM"))~"(copies 100"~mL^-1*")")) +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_text(family = "Times New Roman", size = 24),
        legend.position = "none")

high_flow_tetM_plot

medium_flow_tetM_plot <- ggplot(medium_flow_tetM, aes(ordered(x = Label, levels = levels_m), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 5", size = 6) +
  scale_y_log10(limits = c(4590000, 775000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Medium Flow") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_blank(),
        legend.position = "none")

medium_flow_tetM_plot

low_flow_tetM_plot <- ggplot(low_flow_tetM, aes(ordered(x = Label, levels = levels_l), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 6", hide.ns = TRUE, size = 6) +
  scale_y_log10(limits = c(4590000, 775000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Low Flow") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_blank(),
        legend.position = "none")

low_flow_tetM_plot

ggsave("High_Flow_tetM.tiff", plot = high_flow_tetM_plot,
       width = 6, height = 6, dpi = 300, units = c("in"))

ggsave("Medium_Flow_tetM.tiff", plot = medium_flow_tetM_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))

ggsave("Low_Flow_tetM.tiff", plot = low_flow_tetM_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))

#ermB#

high_flow_ermB <- read_excel("../Boxplot Data.xlsx", sheet = "High Flow_Clean") %>%
  subset(Contaminant == "ermB") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 9",
                           Sample == "Downstream" ~ "Runoff \n n = 9",
                           Sample == "Infiltration" ~ "Infiltration \n n = 9"))

levels_h = c("Run-on \n n = 9", "Runoff \n n = 9", "Infiltration \n n = 9")

medium_flow_ermB <- read_excel("../Boxplot Data.xlsx", sheet = "Medium Flow_Clean") %>%
  subset(Contaminant == "ermB") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 5",
                           Sample == "Downstream" ~ "Runoff \n n = 5",
                           Sample == "Infiltration" ~ "Infiltration \n n = 7"))

levels_m = c("Run-on \n n = 5", "Runoff \n n = 5", "Infiltration \n n = 7")

low_flow_ermB <- read_excel("../Boxplot Data.xlsx", sheet = "Low Flow_Clean") %>%
  subset(Contaminant == "ermB") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 6",
                           Sample == "Downstream" ~ "Runoff \n n = 8",
                           Sample == "Infiltration" ~ "Infiltration \n n = 8"))

levels_l = c("Run-on \n n = 6", "Runoff \n n = 8", "Infiltration \n n = 8")

high_flow_ermB_plot <- ggplot(high_flow_ermB, aes(ordered(x = Label, levels = levels_h), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 9", size = 6) +
  scale_y_log10(limits = c(4590000, 775000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("High Flow") +
  ylab(bquote(italic(.("ermB"))~"(copies 100"~mL^-1*")")) +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_text(family = "Times New Roman", size = 24),
        legend.position = "none")

high_flow_ermB_plot

medium_flow_ermB_plot <- ggplot(medium_flow_ermB, aes(ordered(x = Label, levels = levels_m), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 5", size = 6) +
  scale_y_log10(limits = c(4590000, 775000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Medium Flow") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_blank(),
        legend.position = "none")

medium_flow_ermB_plot

low_flow_ermB_plot <- ggplot(low_flow_ermB, aes(ordered(x = Label, levels = levels_l), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 6", hide.ns = TRUE, size = 6) +
  scale_y_log10(limits = c(4590000, 775000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Low Flow") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_blank(),
        legend.position = "none")

low_flow_ermB_plot

ggsave("High_Flow_ermB.tiff", plot = high_flow_ermB_plot,
       width = 6, height = 6, dpi = 300, units = c("in"))

ggsave("Medium_Flow_ermB.tiff", plot = medium_flow_ermB_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))

ggsave("Low_Flow_ermB.tiff", plot = low_flow_ermB_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))


#ermF#

high_flow_ermF <- read_excel("../Boxplot Data.xlsx", sheet = "High Flow_Clean") %>%
  subset(Contaminant == "ermF") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 9",
                           Sample == "Downstream" ~ "Runoff \n n = 9",
                           Sample == "Infiltration" ~ "Infiltration \n n = 9"))

levels_h = c("Run-on \n n = 9", "Runoff \n n = 9", "Infiltration \n n = 9")

medium_flow_ermF <- read_excel("../Boxplot Data.xlsx", sheet = "Medium Flow_Clean") %>%
  subset(Contaminant == "ermF") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 5",
                           Sample == "Downstream" ~ "Runoff \n n = 5",
                           Sample == "Infiltration" ~ "Infiltration \n n = 7"))

levels_m = c("Run-on \n n = 5", "Runoff \n n = 5", "Infiltration \n n = 7")

low_flow_ermF <- read_excel("../Boxplot Data.xlsx", sheet = "Low Flow_Clean") %>%
  subset(Contaminant == "ermF") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 6",
                           Sample == "Downstream" ~ "Runoff \n n = 8",
                           Sample == "Infiltration" ~ "Infiltration \n n = 8"))

levels_l = c("Run-on \n n = 6", "Runoff \n n = 8", "Infiltration \n n = 8")

high_flow_ermF_plot <- ggplot(high_flow_ermF, aes(ordered(x = Label, levels = levels_h), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 9", size = 6) +
  scale_y_log10(limits = c(4590000, 775000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("High Flow") +
  ylab(bquote(italic(.("ermF"))~"(copies 100"~mL^-1*")")) +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_text(family = "Times New Roman", size = 24),
        legend.position = "none")

high_flow_ermF_plot

medium_flow_ermF_plot <- ggplot(medium_flow_ermF, aes(ordered(x = Label, levels = levels_m), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 5", size = 6) +
  scale_y_log10(limits = c(4590000, 775000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Medium Flow") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_blank(),
        legend.position = "none")

medium_flow_ermF_plot

low_flow_ermF_plot <- ggplot(low_flow_ermF, aes(ordered(x = Label, levels = levels_l), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 6", hide.ns = TRUE, size = 6) +
  scale_y_log10(limits = c(4590000, 775000000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Low Flow") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_blank(),
        legend.position = "none")

low_flow_ermF_plot

ggsave("High_Flow_ermF.tiff", plot = high_flow_ermF_plot,
       width = 6, height = 6, dpi = 300, units = c("in"))

ggsave("Medium_Flow_ermF.tiff", plot = medium_flow_ermF_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))

ggsave("Low_Flow_ermF.tiff", plot = low_flow_ermF_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))


