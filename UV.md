---
title: "UV fluence vs Microbial Load, meta-study"
author: "Elliot Erskine"
date: "2024-03-24"
output:
  html_document:
    keep_md: true
    theme: cosmo
    fontsize: 1.6rem
---



## Dataset

The following dataset was collected from the Canadian Industrial \[link-here\]


```r
# making list to filter out kingdoms we are not interested in
remove <- c("Animalia", "Plantae","Fungi","Protozoa","Archaea","Virus")

# read in UV data
UV <- read.csv("UV_full.csv", header=TRUE)

# rename columns
names(UV) <- c("kingdom", 'phyla',	'class',	'order',	'family',	'genera',	'gram',	"species",	"number",	"method",	"type",	"host",	"lamp",	"wavelength",	"1",	"2",	"3",	"4",	"5",	"6",	"protocol",	"tailing",	"antibiotic_resistant",	"reference",	"action_spectrum", "pathogen")

# pivot table from wider to longer on fluence value required for log10 reduction of microbe
UV <-  pivot_longer(UV, c("1",	"2",	"3",	"4",	"5",	"6"), names_to = "log10_reduction", values_to = "fluence") %>% 
  # use log10 reduction as number
  #transform(log10_reduction = as.numeric(log10_reduction)) %>% 
  # remove empty rows with no UV data
  filter(!is.na(fluence)) %>% 
  # select specific columns to take forward
  select("kingdom", 'order',	'genera',	'gram',	"species",	"lamp",	"wavelength",	"antibiotic_resistant",	"pathogen", "log10_reduction", "fluence") %>%
  # filter out unwanted kingdoms
  filter(!kingdom %in% remove) %>%
  # filter out non-pathogens
  filter(pathogen == "Y") %>%
  # thresholding fluence
  filter(fluence >= 0 & fluence <= 500)

# add conditional column of % survival based on log10_reduction
UV <- mutate(UV,
             percent_survival = case_when(log10_reduction == 1 ~ "10%",
                                       log10_reduction == 2 ~ "1%",
                                       log10_reduction == 3 ~ "0.1%",
                                       log10_reduction == 4 ~ "0.01%",
                                       log10_reduction == 5 ~ "0.001%",
                                       log10_reduction == 6 ~ "0.0001%"))
```

## Visualising

#### Log10 reduction in pathogens

The following @fig-scatterplot shows that the majority of pathogenic bacteria are reduced by x-log at a fluence of 40 mJ/cm


```r
cbp1 <- c("#999999", "#E69F00", "#CC79A7", "#56B4E9",
          "#F0E442", "#0072B2", "#D55E00", "#009E73")

ggplot(UV, aes(x=fluence, y=log10_reduction)) +
  theme_bw(base_size=18) +  
  geom_vline(xintercept=40, linetype="dashed", color = "red") +
  geom_jitter(aes(col=order))
```

![Log10 reduction in bacterial load at increasing UV fluence](UV_files/figure-html/fig-scatterplot-1.png)

#### Survival of pathogens using 40 mJ/cm lamp or weaker

Looking at expected pathogen survival using 40 mJ/cm lamp @fig-histogram1


```r
UV_2 <- filter(UV, fluence<=40)
ggplot(UV_2, aes(x=percent_survival,fill=order)) +
  theme_bw(base_size=18) + 
  geom_bar() +
  labs(x="% survival")
```

![Using a 40 mJ/cm lamp (or weaker), expected survival of bacteria](UV_files/figure-html/fig-histogram1-1.png)

#### Survival of pathogens using 40 mJ/cm lamp or stronger

Expected pathogen survival of greater than 0.01% of population when using greater than 40 mJ/cm @fig-histogram2


```r
UV_2 <- filter(UV, log10_reduction <3)
ggplot(UV_2, aes(x=percent_survival,fill=order)) +
  theme_bw(base_size=18) + 
  geom_bar() +
  labs(x="% survival")
```

![Expected survival of >0.01% of pathogen pop.](UV_files/figure-html/fig-histogram2-1.png)


