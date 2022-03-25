library("readxl")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("scales")

#ENTEROCOCCUS#

high_flow_Ent <- read_excel("../Boxplot Data.xlsx", sheet = "High Flow_Clean") %>%
  subset(Contaminant == "ENT") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 9",
                           Sample == "Downstream" ~ "Runoff \n n = 9",
                           Sample == "Infiltration" ~ "Infiltration \n n = 10"))

levels_h = c("Run-on \n n = 9", "Runoff \n n = 9", "Infiltration \n n = 10")

medium_flow_Ent <- read_excel("../Boxplot Data.xlsx", sheet = "Medium Flow_Clean") %>%
  subset(Contaminant == "ENT") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 5",
                           Sample == "Downstream" ~ "Runoff \n n = 5",
                           Sample == "Infiltration" ~ "Infiltration \n n = 7"))

levels_m = c("Run-on \n n = 5", "Runoff \n n = 5", "Infiltration \n n = 7")

low_flow_Ent <- read_excel("../Boxplot Data.xlsx", sheet = "Low Flow_Clean") %>%
  subset(Contaminant == "ENT") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 9",
                           Sample == "Downstream" ~ "Runoff \n n = 9",
                           Sample == "Infiltration" ~ "Infiltration \n n = 9"))

levels_l = c("Run-on \n n = 9", "Runoff \n n = 9", "Infiltration \n n = 9")

high_flow_Ent_plot <- ggplot(high_flow_Ent, aes(ordered(x = Label, levels = levels_h), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 9", size = 6) +
  scale_y_log10(limits = c(1000, 18000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("High Flow") +
  ylab(bquote("ENT (cfu 100"~mL^-1*")")) +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_text(family = "Times New Roman", size = 24),
        legend.position = "none")

high_flow_Ent_plot

medium_flow_Ent_plot <- ggplot(medium_flow_Ent, aes(ordered(x = Label, levels = levels_m), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 5", size = 6) +
  scale_y_log10(limits = c(1000, 18000),
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

medium_flow_Ent_plot

low_flow_Ent_plot <- ggplot(low_flow_Ent, aes(ordered(x = Label, levels = levels_l), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 9", hide.ns = TRUE, size = 6) +
  scale_y_log10(limits = c(1000, 18000),
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

low_flow_Ent_plot

ggsave("High_Flow_Ent.tiff", plot = high_flow_Ent_plot,
       width = 6, height = 6, dpi = 300, units = c("in"))

ggsave("Medium_Flow_Ent.tiff", plot = medium_flow_Ent_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))

ggsave("Low_Flow_Ent.tiff", plot = low_flow_Ent_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))

#TETRACYCLINE RESISTANT ENTEROCOCCUS#

high_flow_Tet <- read_excel("../Boxplot Data.xlsx", sheet = "High Flow_Clean") %>%
  subset(Contaminant == "TET") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 9",
                           Sample == "Downstream" ~ "Runoff \n n = 9",
                           Sample == "Infiltration" ~ "Infiltration \n n = 10"))

levels_h = c("Run-on \n n = 9", "Runoff \n n = 9", "Infiltration \n n = 10")

medium_flow_Tet <- read_excel("../Boxplot Data.xlsx", sheet = "Medium Flow_Clean") %>%
  subset(Contaminant == "TET") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 5",
                           Sample == "Downstream" ~ "Runoff \n n = 5",
                           Sample == "Infiltration" ~ "Infiltration \n n = 7"))

levels_m = c("Run-on \n n = 5", "Runoff \n n = 5", "Infiltration \n n = 7")

low_flow_Tet <- read_excel("../Boxplot Data.xlsx", sheet = "Low Flow_Clean") %>%
  subset(Contaminant == "TET") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 9",
                           Sample == "Downstream" ~ "Runoff \n n = 9",
                           Sample == "Infiltration" ~ "Infiltration \n n = 9"))

levels_l = c("Run-on \n n = 9", "Runoff \n n = 9", "Infiltration \n n = 9")

high_flow_Tet_plot <- ggplot(high_flow_Tet, aes(ordered(x = Label, levels = levels_h), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 9", size = 6) +
  scale_y_log10(limits = c(1000, 18000),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("High Flow") +
  ylab(bquote("TET (cfu 100"~mL^-1*")")) +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_text(family = "Times New Roman", size = 24),
        legend.position = "none")

high_flow_Tet_plot

medium_flow_Tet_plot <- ggplot(medium_flow_Tet, aes(ordered(x = Label, levels = levels_m), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 5", size = 6) +
  scale_y_log10(limits = c(1000, 18000),
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

medium_flow_Tet_plot

low_flow_Tet_plot <- ggplot(low_flow_Tet, aes(ordered(x = Label, levels = levels_l), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 9", hide.ns = TRUE, size = 6) +
  scale_y_log10(limits = c(1000, 18000),
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

low_flow_Tet_plot

ggsave("High_Flow_Tet.tiff", plot = high_flow_Tet_plot,
       width = 6, height = 6, dpi = 300, units = c("in"))

ggsave("Medium_Flow_Tet.tiff", plot = medium_flow_Tet_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))

ggsave("Low_Flow_Tet.tiff", plot = low_flow_Tet_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))
