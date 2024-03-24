library(tidyverse)
library(scales)
setwd("~/Desktop/UV/")

####UV####

UV <- read.csv("UV_full.csv", header=TRUE)
names(UV) <- c("kingdom", 'phyla',	'class',	'order',	'family',	'genera',	'gram',	"species",	"number",	"method",	"type",	"host",	"lamp",	"wavelength",	"1",	"2",	"3",	"4",	"5",	"6",	"protocol",	"tailing",	"antibiotic_resistant",	"reference",	"action_spectrum", "pathogen")
UV <-  pivot_longer(UV, c("1",	"2",	"3",	"4",	"5",	"6"), names_to = "log10_reduction", values_to = "fluence") %>% 
  transform(UV, log10_reduction = as.numeric(log10_reduction))
UV <- filter(UV, !is.na(fluence))

cbp1 <- c("#999999", "#E69F00", "#CC79A7", "#56B4E9",
          "#F0E442", "#0072B2", "#D55E00", "#009E73")

remove <- c("Animalia", "Plantae","Fungi","Protozoa","Archaea","Virus")

UV %>% 
  select("kingdom", 'phyla',	'class',	'order',	'family',	'genera',	'gram',	"species",	"number",	"method",	"type",	"host",	"lamp",	"wavelength",	"protocol",	"tailing",	"antibiotic_resistant",	"reference",	"action_spectrum", "pathogen", "log10_reduction", "fluence") %>% 
  filter(!kingdom %in% remove) %>%
  #filter(!percent_reduction ==	"99.9999%") %>%
  #filter(!percent_reduction == "99.999%") %>%
  filter(pathogen == "Y") %>% 
  filter(fluence >= 0 & fluence <= 500) %>%
  ggplot(aes(x=fluence, y=log10_reduction)) +
  theme_bw(base_size=18) +  
  geom_vline(xintercept=40, linetype="dashed", color = "red") +
  geom_jitter(aes(col=order)) 
  #geom_smooth(aes(col=kingdom),method = "lm") +
  #scale_color_manual(values=cbp1) 
  #labs(x="Log10 Reduction", y="Fluence (mJ/cm2)", title="% reduction, pathogenic species") +
  #scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))


# UV %>% 
#   select("kingdom", 'phyla',	'class',	'order',	'family',	'genera',	'gram',	"species",	"number",	"method",	"type",	"host",	"lamp",	"wavelength",	"protocol",	"tailing",	"antibiotic_resistant",	"reference",	"action_spectrum", "pathogen", "percent_reduction", "fluence") %>% 
#   filter(!kingdom %in% remove) %>%
#   #filter(!percent_reduction ==	"99.9999%") %>%
#   #filter(!percent_reduction == "99.999%") %>%
#   filter(pathogen == "Y") %>%
#   filter(kingdom == "Bacteria") %>% 
#   ggplot(aes(x=percent_reduction, y=fluence)) +
#   theme_bw(base_size=18) +  
#   geom_hline(yintercept=40, linetype="dashed", color = "red") +
#   geom_boxplot(position = position_dodge(preserve = "single")) +
#   labs(x="Percent Reduction", y="Fluence (mJ/cm2)", title="% reduction, pathogenic bacterial species") 
#   #scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
# UV %>% 
#   filter(kingdom== "Bacteria") %>% 
#   filter(pathogen == "Y") %>% 
#   #filter(fluence > 40) %>% 
#   write.csv("bacteria3.csv")
# 
# UV_bacteria <- filter(UV, !is.na(gram))
# UV_bacteria <- filter(UV_bacteria, log_reduction =="log_2")
# 
# UV_bacteria %>% 
#   ggplot(aes(x=percent_reduction, y=fluence)) + 
#   geom_boxplot() +
#   theme_bw(base_size=18) +
#   #coord_cartesian(ylim=c(0,20)) +
#   labs(x="Percent Reduction in Bacteria", y="Fluence (mJ/cm2)")
#   #scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
# 
# UV %>%
#   filter(species == "Listeria monocytogenes") %>%
#   ggplot(aes(x=percent_reduction, y=fluence)) +
#   geom_point(size=3) +
#   theme_bw(base_size=18) +
#   #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#   geom_hline(yintercept=40, linetype="dashed", color = "red") +
#   #scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
#   labs(x="Reduction in Listeria monocytogenes", y="UV Fluence (mJ/cm2)")
# 
# UV %>%
#   filter(str_detect(species, "Cryptosporidium*")) %>%
#   ggplot(aes(x=percent_reduction, y=fluence)) +
#   geom_boxplot() +
#   theme_bw(base_size=18) +
#   #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#   geom_hline(yintercept=40, linetype="dashed", color = "red") +
#   #scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
#   labs(x="Reduction in Cryptosporidium", y="UV Fluence (mJ/cm2)")
# 
# UV %>%
#   filter(str_detect(species, "Salmonella*")) %>%
#   ggplot(aes(x=percent_reduction, y=fluence)) +
#   geom_boxplot() +
#   theme_bw(base_size=18) +
#   #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#   geom_hline(yintercept=40, linetype="dashed", color = "red") +
#   #scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
#   labs(x="Reduction in Salmonella", y="UV Fluence (mJ/cm2)")
# 
# 
# UV %>%
#   filter(str_detect(species, "Legionella*")) %>%
#   ggplot(aes(x=percent_reduction, y=fluence)) +
#   geom_boxplot() +
#   theme_bw(base_size=18) +
#   #scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#   geom_hline(yintercept=40, linetype="dashed", color = "red") +
#   #scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
#   labs(x="Reduction in Legionella", y="UV Fluence (mJ/cm2)")
# 
# 
# UV %>%
#   filter(str_detect(number, "O157.*")) %>% 
#   ggplot(aes(y=fluence, x=log10_reduction)) +
#   geom_boxplot() +
#   theme_bw(base_size=18) +
#   geom_hline(yintercept=40, linetype="dashed", color = "red") +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
#   #scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
#   labs(x="Log 10 Reduction in E. coli 0157:H7", y="UV Fluence (mJ/cm2)")
# 
# UV_all <- filter(UV, log_reduction == "log_1")
# ggplot(UV_all, aes(x=kingdom, y=fluence)) + 
#   geom_jitter(aes(col=phyla)) +
#   geom_hline(yintercept=40, linetype="dashed", color = "red") +
#   labs(x="Kingdom", y="Fluence (mJ/cm2)", title="dose required for 1-log reduction") +
#   theme(legend.position = "none") +
#   scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))
# 
# ####FSA####
# FSA <- read.csv("FSA.csv", header=TRUE)
# 
# FSA %>%
#   arrange(desc(Cases_Median)) %>% 
#   mutate(Kingdom=factor(Kingdom, levels=Kingdom)) %>% 
#   ggplot(aes(x=Kingdom, y=Cases_Median)) +
#   geom_col() +
#   labs(y="Median cases in UK", x=NULL) +
#   theme_bw()
# 
# FSA %>%
#   arrange(desc(Hospital_median)) %>% 
#   mutate(Species=factor(Species, levels=Species)) %>% 
#   ggplot(aes(x=Species, y=Hospital_median)) +
#   geom_col(aes(fill=Kingdom)) +
#   labs(y="Median hospitalisations in UK", x=NULL) +
#   theme_bw() +
#   scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# FSA %>% 
#   mutate(percent=Hospital_median/Cases_Median*100) %>%
#   arrange(desc(percent)) %>% 
#   mutate(Species=factor(Species, levels=Species)) %>% 
#   ggplot(aes(x=Species, y=percent)) +
#   geom_col(aes(fill=Kingdom)) +
#   labs(y="% Hospitalisation", x=NULL) +
#   theme_bw() +
#   coord_cartesian(ylim=c(0,100)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#   