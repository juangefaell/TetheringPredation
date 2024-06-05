#######################################################################################
#### Reflectance spectrometry of the snails and rocks from Aguncheiro and Silleiro ####
#######################################################################################

###### Packages #######
library(tidyverse) # Basic statistical analyses
library(pavo) # Basic spectra data processing
library(stringr) # Plot spectra by grouping factors with shaded area representing confidence interval

####### Dataset #######
setwd("../Data") # Set the directory of the dataset file
read.csv("Data_RefSpec.csv") # Read the ReflectanceSpectrometry dataset

####### Data handling and overview #######
RefSpec <- read.csv("Data_RefSpec.csv") %>% 
  drop_na() %>% # Remove NAs (incompatible with 'pavo')
  as.rspec() # Convert to 'pavo' format

####### Preparatory corrections #######
### Identification of the position of negative values
RefSpec %>% 
  plot(type = "o") %>% 
  abline(h = 0, lty = 3) 
  # OUTPUT: Negative values are rare and mostly gathered around 300-400 nm values

### Smoothing and correcting negative values with 'procspec'
# Apply LOESS smoothing and correct negative values by converting them to '0' 
RefSpec_Zero <- RefSpec %>% 
  procspec(., opt = c("smooth"), # Applies LOESS smoothing
           span = 0.7, # Neighboring size with span = 0.7 in the LOESS smoothing
           fixneg = "zero") # Correct negative values by converting them to zero

# Check the effects of different span sizes used in LOESS smoothing method with 'plotsmooth'
RefSpec_Zero %>%
  subset("Lutea") %>% 
  plotsmooth(minsmooth = 0.05, maxsmooth=1, curves=4, ask = FALSE) 
  # OUTPUT: Span = 0.7 looks okay, as it smooths the curve yet does not distort the pattern.

####### Figure S6 /// Spectra of Aguncheiro objects ####### 
### Select the data
RefSpec_Aguncheiro <- RefSpec_Zero %>%
  select(-contains("RockSilleiro"), -contains("LineataSilleiro")) %>% # Remove objects from Cabo Silleiro
  select(-RockAguncheiro_6) # Remove RockAguncheiro_6 (possibly an artifact)

RefSpec_Aguncheiro_Col <- colnames(RefSpec_Aguncheiro) %>% # Extract the column names
  strsplit("_") %>% # Split the column names based on '_'
  sapply(function(x) x[1]) %>% # Select the first element of the column names (which contains the names of color morphs and rocks)
  .[-1] # Remove the first column, which corresponds to 'wl'

### Graph
RefSpec_Aguncheiro %>% 
  aggplot(by = RefSpec_Aguncheiro_Col,
          FUN.center = mean,
          alpha = 0.4,
          FUN.error = function(x) 1.96*(sd(x)/sqrt(length(x))),
          legend = TRUE,
          lcol = c("#F2E66E", "#E7E3DC", "#473F2D", "blue"),
          shadecol = c("#F2E66E", "#E7E3DC", "#473F2D", "blue"), 
          lty = c("solid", "solid", "solid", "dashed"), 
          lwd = 2)

####### Figure 3 /// Spectra of Experiment objects ####### 
### Select the data
RefSpec_Experiment <- RefSpec_Zero %>%
  select(-contains("RockAguncheiro")) # Remove rocks from Aguncheiro, as they were not used in the experiment

RefSpec_Experiment_Col <- colnames(RefSpec_Experiment) %>%
  strsplit("_") %>%
  sapply(function(x) x[1]) %>% 
  .[-1]

### Graph
RefSpec_Experiment %>% 
  aggplot(by = RefSpec_Experiment_Col,
          FUN.center = mean,
          alpha = 0.4,
          FUN.error = function(x) 1.96*(sd(x)/sqrt(length(x))),
          legend = TRUE,
          ylim = c(0, 40),
          lcol = c("#A59C9A", "#F2E66E", "#A59C9A", "#473F2D", "red"),
          shadecol = c("grey", "#F2E66E", "#E7E3DC", "#473F2D", "red"), 
          lty = c("dashed", "solid", "solid", "solid", "dotted"), 
          lwd = 2)
