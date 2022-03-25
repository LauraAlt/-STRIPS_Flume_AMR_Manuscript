library("readxl")
library("ggplot2")
library("scales")
theme_set(theme_bw())

source("summarySE.R")

data <- read_excel("Manure_Antibiotics.xlsx", sheet = "Clean_Data")

SE <- summarySE(data, measurevar="Conc_ed", groupvars=c("Antibiotic_Class", "Antibiotic"))

levels = ordered(c("Azithromycin", "Clarithromycin", "Erythromycin", "Roxithromycin", "Spiramycin II",
                   "Spiramycin III", "Tilmicosin", "Tylosin", "Tiamulin", "Sulfachloropyridazine",
                   "Sulfadiazine", "Sulfadimethoxine", "Sulfamerazine", "Sulfameter", 
                   "Sulfamethazine", "Sulfamethizole", "Sulfamethoxazole", "Sulfathiazole",
                   "Anhydrochlortetracycline", "Anhydrotetracycline", "Chlortetracycline",
                   "4-Epichlortetracycline", "4-Epitetracycline", "Oxytetracycline", "Tetracycline"))

figure <- ggplot(SE, aes(x = ordered(Antibiotic, levels = levels), y = Conc_ed, 
                                 fill = Antibiotic_Class)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.75), width = 0.6) +
  geom_errorbar(aes(ymin = Conc_ed - sd, ymax = Conc_ed + sd), width = 0.2,
                position = position_dodge(width = 0.75)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_manual(values=c('#1B9E77', '#D95F02', '#7570B3', '#E7298A')) +
  labs(y = "Concentration (ppb)") +
  theme(axis.text.x = element_text(family = "Times New Roman", size = 14,
                                   angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", size = 14),
        axis.title.y = element_text(family = "Times New Roman", size = 16),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman", size = 14),
        legend.position = "top")

figure

ggsave("Manure_Antibiotics.tiff", plot = figure,
       width = 10, height = 6, units = c("in"))
