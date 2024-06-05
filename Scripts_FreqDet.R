#####################################################################################################
#### Frequencies of scars at the source and experiment localities (Aguncheiro and Cabo Silleiro) ####
#####################################################################################################

###### Packages #######
library(tidyverse) # Basic plots and statistical analyses

####### Dataset #######
setwd("/Users/juangefaellborras/Desktop/Juancho/Work/Research/Littorina/Projects/TetheringPredation/TP_4ManuScript/TP_MS_OpenData&Scripts/Data") # Set the directory of the dataset file
read.csv("Data_FreqDet.csv") # Read the FrequencyDetermination dataset

####### Data handling and overview #######
FreqDet <- read.csv("Data_FreqDet.csv") %>% 
  mutate(ColorMorph = factor(ColorMorph, levels = c("Lineata", "Nigra", "Lutea", "Albida"))) # Set a shortcut name for the dataset and convert ColorMorph to factor
str(FreqDet) # Check the structure of the data

####### Preparatory analyses #######
### Are the samplings homogeneous for color morphs counts?
FreqDet %>%
  filter(Locality == "Aguncheiro") %>%
  xtabs(formula = Count ~ ColorMorph + Sampling) %>%
  { list(
    ContingencyTable = .,
    FisherTest = fisher.test(., simulate.p.value = TRUE) # Fisher's exact test (to avoid problems with Chi-square)
  )
  } %>%
  print()
  # OUTPUT: The color morph counts are homogeneous between samplings and therefore can be grouped

### Are the samplings homogeneous for Scars counts?
FreqDet %>% 
  filter(Locality == "Aguncheiro") %>% # Filter cases from Aguncheiro
  xtabs(formula = CountScars ~ ColorMorph + Sampling) %>% # Create a contingency table
  { list(
    ContingencyTable = .,
    FisherTest = fisher.test(., simulate.p.value = TRUE) # Fisher's Exact test with Monte Carlo simulations
  )
  } %>%
  print()
  # OUTPUT: The scars counts are homogeneous between samplings and therefore can be grouped

### Chi-square for intensity of predation (Scar Counts) in Aguncheiro and Cabo Silleiro
FreqDet %>%
  group_by(Locality) %>% 
  summarize(TotalMorphCounts = sum(Count),
            TotalScarCounts = sum(CountScars)) %>% 
  with({
    ContingencyTable <- matrix(c(TotalScarCounts, TotalMorphCounts - TotalScarCounts), ncol = 2)
    print( ContingencyTable)
    ChisqTest <- chisq.test( ContingencyTable)
    print(ChisqTest)
  })
  # OUTPUT: No differences in predation intensity between both localities

####### Table 1 /// Percentages of ColorMorphs and Scars, plus Relative incidence of Scars per morph #######
FreqDet %>%
  group_by(Locality, ColorMorph) %>%
  summarize(TotalMorphCounts = sum(Count),
            TotalScarCounts = sum(CountScars)) %>% 
  mutate(PercentageMorphCounts = (TotalMorphCounts / sum(TotalMorphCounts)) * 100,
         PercentageScarCounts = (TotalScarCounts / TotalMorphCounts) * 100,
         RelativeIncidenceOfScars = (TotalScarCounts / sum(TotalScarCounts)) * 100,
         ScarsToMorphRatio = (RelativeIncidenceOfScars / PercentageMorphCounts)) %>%
  print()

####### Comparison of scars between color morphs in Aguncheiro #######
### Fisher's test for Scar Counts between color morphs in Aguncheiro (without excluding any ColorMorph)
FreqDet %>%
  group_by(Locality, ColorMorph) %>%
  summarize(TotalMorphCounts = sum(Count),
            TotalScarCounts = sum(CountScars)) %>%
  filter(Locality == "Aguncheiro") %>%
  with({
    ContingencyTable <- matrix(c(TotalScarCounts, TotalMorphCounts - TotalScarCounts), ncol = 2)
    print(ContingencyTable) 
    fisher.test(ContingencyTable, simulate.p.value = TRUE) %>% 
      print()
  })
  # OUTPUT: Significant differences in Scars between color morphs

### Fisher's exact test for Scar Counts between color morphs in Aguncheiro (excluding Albida, which has low n)
FreqDet %>%
  group_by(Locality, ColorMorph) %>%
  summarize(TotalMorphCounts = sum(Count),
            TotalScarCounts = sum(CountScars)) %>%
  filter(Locality == "Aguncheiro") %>%
  subset(ColorMorph != "Albida") %>%
  with({
    ContingencyTable <- matrix(c(TotalScarCounts, TotalMorphCounts - TotalScarCounts), ncol = 2)
    print(ContingencyTable) 
    fisher.test(ContingencyTable, simulate.p.value = TRUE) %>% # Fisher's Exact test with Monte Carlo simulations
      print()
  })
  # OUTPUT: When Albida cases (low n) are excluded, significant differences in Scars between color morphs disappear

####### Figure 2 /// Scars to morph frequency ratio for each color morph #######
### Select the data
FreqDet_Figure2 <- FreqDet %>%
  group_by(Locality, ColorMorph) %>%
  summarize(TotalMorphCounts = sum(Count),
            TotalScarCounts = sum(CountScars)) %>% 
  mutate(PercentageMorphCounts = (TotalMorphCounts / sum(TotalMorphCounts)) * 100,
         PercentageScarCounts = (TotalScarCounts / TotalMorphCounts) * 100,
         RelativeIncidenceOfScars = (TotalScarCounts / sum(TotalScarCounts)) * 100,
         ScarsToMorphRatio = (RelativeIncidenceOfScars / PercentageMorphCounts)) %>%
  filter(Locality == "Aguncheiro")

### Graph
FreqDet_Figure2 %>% 
  ggplot(aes(x = ColorMorph, y = ScarsToMorphRatio)) +
  geom_point(size = 3, stroke = 1, aes(colour = ColorMorph), show.legend = FALSE) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = "Color Morph", y = "Scars to Morph Frequency Ratio") +
  theme_bw() +
  ylim(0, 5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(color = "black", margin = margin(t=5), size=11),
        axis.text.y = element_text(color = "black", margin = margin(r=4),size=11),
        axis.title.x = element_text(margin = margin(t = 12), size = 12), 
        axis.title.y = element_text(margin = margin(r = 12), size = 11),
        plot.margin = margin(t = 10, b =10, l = 10, r = 30)) +  
  scale_x_discrete(labels = expression(italic("Lineata"), italic("Nigra"), italic("Lutea"), italic("Albida"))) +
  scale_color_manual(values = c("#E7E3DC", "#473F2D", "#F2E66E", "#F6F5ED"))
