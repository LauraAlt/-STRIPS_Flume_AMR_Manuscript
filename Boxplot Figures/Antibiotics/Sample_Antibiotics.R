library("readxl")
library("ggplot2")
library("dplyr")
library("ggpubr")


high_flow <- read_excel("../Boxplot Data.xlsx", sheet = "High Flow_Clean") %>%
  subset(Contaminant_Type == "Antibiotic") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 8",
                           Sample == "Downstream" ~ "Runoff \n n = 9",
                           Sample == "Infiltration" ~ "Infiltration \n n = 10"))

levels_h = c("Run-on \n n = 8", "Runoff \n n = 9", "Infiltration \n n = 10")

medium_flow <- read_excel("../Boxplot Data.xlsx", sheet = "Medium Flow_Clean") %>%
  subset(Contaminant_Type == "Antibiotic") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 5",
                           Sample == "Downstream" ~ "Runoff \n n = 5",
                           Sample == "Infiltration" ~ "Infiltration \n n = 6"))

levels_m = c("Run-on \n n = 5", "Runoff \n n = 5", "Infiltration \n n = 6")

low_flow <- read_excel("../Boxplot Data.xlsx", sheet = "Low Flow_Clean") %>%
  subset(Contaminant_Type == "Antibiotic") %>%
  mutate(Label = case_when(Sample == "Upstream" ~ "Run-on \n n = 9",
                           Sample == "Downstream" ~ "Runoff \n n = 9",
                           Sample == "Infiltration" ~ "Infiltration \n n = 9"))

levels_l = c("Run-on \n n = 9", "Runoff \n n = 9", "Infiltration \n n = 9")

high_flow_plot <- ggplot(high_flow, aes(ordered(x = Label, levels = levels_h), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 8", label.y = 1.06, size = 6) +
  ylim(0, 1.09) +
  geom_hline(yintercept = 0.02, linetype = "dashed", 
             color = "black", size = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("High Flow") +
  ylab("Tetracycline (ppb)") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_text(family = "Times New Roman", size = 24),
        legend.position = "none")

high_flow_plot

medium_flow_plot <- ggplot(medium_flow, aes(ordered(x = Label, levels = levels_m), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 5", label.y = 1.06, size = 6) +
  ylim(0, 1.09) +
  geom_hline(yintercept = 0.32, linetype = "dashed", 
             color = "black", size = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Medium Flow") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_blank(),
        legend.position = "none")

medium_flow_plot

low_flow_plot <- ggplot(low_flow, aes(ordered(x = Label, levels = levels_l), y = Conc, fill = Sample)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 8, color = "black", fill = "black") +
  stat_compare_means(label = "p.format", ref.group = "Run-on \n n = 9", label.y = 14.5, size = 6) +
  ylim(0, 15) +
  geom_hline(yintercept = 0.32, linetype = "dashed", 
             color = "black", size = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Low Flow") +
  theme_bw() +
  theme(plot.title = element_text(family = "Times New Roman", face = "bold", size = 26),
        axis.text.x = element_text(family = "Times New Roman", size = 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 20),
        axis.title.y = element_blank(),
        legend.position = "none")

low_flow_plot

ggsave("High_Flow_Antibiotic.tiff", plot = high_flow_plot,
       width = 6, height = 6, dpi = 300, units = c("in"))

ggsave("Medium_Flow_Antibiotic.tiff", plot = medium_flow_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))

ggsave("Low_Flow_Antibiotic.tiff", plot = low_flow_plot,
       width = 5.66, height = 6, dpi = 300, units = c("in"))
