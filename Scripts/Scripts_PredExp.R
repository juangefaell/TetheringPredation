###############################################
#### Tethering experiment in Cabo Silleiro ####
###############################################

###### Packages #######
install.packages("lme4", type = "source") # Installation of lm4 from source
library(tidyverse) # Basic plots and statistical analyses
library(car) # ANOVAs
library(lmerTest) # Logistic regressions
library(lme4) # Logistic regressions
library(Matrix) # For dealing with matrices
library(FSA) # Dunn test (Kruskal-Wallis post hoc)
library(simr)  # Power Analysis

####### Dataset #######
setwd("") # Set the directory of the dataset file
read.csv("Data_PredExp.csv") # Read the PredationExperiment dataset

####### Data handling and overview #######
PredExp <- read.csv("Data_PredExp.csv") %>%
  mutate(
    ColorMorph_Locality = factor(ColorMorph_Locality, levels = c("Lineata_Silleiro", "Lineata_Aguncheiro", "Nigra_Aguncheiro", "Lutea_Aguncheiro")),
    TransectPosition = factor(TransectPosition, levels = c("Lower", "Upper")),
    Session = factor(Session, levels = c("1", "2")),
    Alive = as.numeric(Alive),
    Preyed = as.numeric(Preyed), 
    Chipped = as.numeric(Chipped),
    PreyedChipped = as.numeric(PreyedChipped),
    EmptyShell = as.numeric(EmptyShell),
    EpoxyOnlyLost = as.numeric(EpoxyOnlyLost)
  ) %>%
  mutate(
    OutcomeNames_Grouped = case_when(
      OutcomeNames %in% c("AliveIntact", "RecoveredAliveIntact") ~ "Alive",
      is.na(OutcomeNames) | OutcomeNames %in% c("EpoxyOnly", "Lost") ~ "EpoxyOnlyLost_NA",
      OutcomeNames %in% c("PreyedAlive", "PreyedDead") ~ "Preyed",
      TRUE ~ as.character(OutcomeNames)  
    )
  )  # Set a shortcut name for the dataset, convert variables to adequate formats, and create a new OutcomeNames_Grouped variable
str(PredExp) # Check the structure of the data

####### Table 2 /// Number of morphs, shell lengths, and outcomes in Session 1 and 2 ####### 
### n (sample)
PredExp %>% 
  summarise(n_individuals = n_distinct(SnailIndividual)) %>%
  print() # n of snails (morphs pooled) in the total sample (sessions 1, 2 pooled)

PredExp %>% 
  group_by(Session) %>% 
  summarise(n_individuals = n_distinct(SnailIndividual)) %>%
  print() # n of snails (morphs pooled) in sessions 1, 2

PredExp %>% 
  group_by(ColorMorph_Locality) %>% 
  count(ColorMorph_Locality) %>% 
  print() # n of morphs in the total sample (sessions 1, 2 pooled)

PredExp %>% 
  group_by(Session) %>% 
  count(ColorMorph_Locality) %>% 
  print() # n of morphs in sessions 1, 2

### Shell length
PredExp %>% 
  summarise(
    Mean_ShellLength = mean(ShellLength),
    SD_ShellLength = sd(ShellLength)) %>%
  print() # Mean and sd of shell length of snails (morphs pooled) in the total sample (sessions 1, 2 pooled)

PredExp %>% 
  group_by(Session) %>% 
  summarise(
    Mean_ShellLength = mean(ShellLength),
    SD_ShellLength = sd(ShellLength)) %>%
  print() # Mean and sd of shell length of snails (morphs pooled) in sessions 1, 2

PredExp %>% 
  group_by(ColorMorph_Locality) %>% 
  summarise(
    Mean_ShellLength = mean(ShellLength),
    SD_ShellLength = sd(ShellLength)) %>%
  print() # Mean and sd of shell length of morphs in the total sample (sessions 1, 2 pooled)

PredExp %>% 
  group_by(Session, ColorMorph_Locality) %>% 
  summarise(
    Mean_ShellLength = mean(ShellLength),
    SD_ShellLength = sd(ShellLength)) %>%
  print() # Mean and sd of shell length of morphs in sessions 1, 2

### Experiment outcomes
## {NOTE: Some experiment outcomes were grouped in three broad
# -> categories (new variable: OutcomeNames_Grouped) to match the dummy variables: 
# -> AliveIntact+RecoveredAliveIntact (Alive), EpoxyOnly+Lost+NA (EpoxyOnlyLost_NA), 
# -> and PreyedAlive+PreyedDead (Preyed) (See "Data handling and overview")}
PredExp %>% 
  drop_na() %>% 
  count(OutcomeNames_Grouped, sort = TRUE) %>% 
  print() # Experiment outcomes of snails (morphs pooled) in the total sample (sessions 1, 2 pooled)

PredExp %>%
  group_by(Session) %>% 
  count(OutcomeNames_Grouped) %>% 
  print() # Experiment outcomes of snails (morphs pooled) in sessions 1, 2

PredExp %>%
  group_by(ColorMorph_Locality) %>% 
  count(OutcomeNames_Grouped) %>%
  print() # Experiment outcomes of morphs in the total sample (sessions 1, 2 pooled)

PredExp %>%
  group_by(Session, ColorMorph_Locality) %>% 
  filter(Session == "1") %>% # Change this parameter to flip between sessions
  count(OutcomeNames_Grouped) %>%
  print() # Experiment outcomes of morphs in sessions 1, 2 

####### Figure 4 /// Histogram of outcomes (grouped) per session #######
### Simplified dataset for the histogram
PredExp_FigurePE1 <- PredExp %>% 
  group_by(Session, ColorMorph_Locality) %>% 
  count(OutcomeNames_Grouped) %>% 
  print()

PredExp_FiguresPE1PE4_CustomOrder <- c("Alive", "Preyed", "Chipped", "EmptyShell", "EpoxyOnlyLost_NA") # Set the desired order of variables
PredExp_FiguresPE1PE4_CustomLabels <- c("Alive", "Preyed", "Chipped", "Empty shell", "Lost (all)") # Set the corresponding names for variables

### Histogram
PredExp_FigurePE1 %>%
  ggplot(aes(x = OutcomeNames_Grouped, y = n, fill = ColorMorph_Locality)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(x = "Outcomes", 
       y = "Frequency", 
       fill = "Color Morph") +
  scale_fill_manual(values = c("grey", "#E7E3DC","#473F2D", "#F2E66E"),
                    labels = c("Lineata (autochthonous)", "Lineata (allochthonous)", "Nigra", "Lutea")) +
  theme_test() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black", margin = margin(t = 5), size = 12),
        axis.text.y = element_text(color = "black", margin = margin(r=4), size = 12),
        axis.title.x = element_text(margin = margin(t = 12), size = 15), 
        axis.title.y = element_text(margin = margin(r = 12), size = 15),
        plot.margin = margin(t = 10, b =10, l = 10, r = 10),
        strip.text = element_text(colour = "black", size = 13, face = "plain")) +
  scale_x_discrete(expand = c(0.07, 0),
                   limits = PredExp_FiguresPE1PE4_CustomOrder,
                   labels = PredExp_FiguresPE1PE4_CustomLabels) +
  scale_y_continuous(expand = c(0.005, 0.07), limits = c(0, 36)) +
  guides(fill = FALSE) +
  facet_wrap(~ Session, labeller = labeller(Session = c("1" = "Session 1", "2" = "Session 2"))) ## Plot the histogram

####### Comparison tests: comparison of outcomes of the experiment ####### 
### Chi-squared or Fisher test of outcomes between sessions
## All outcomes
Outcome_Session_ContingencyTable <- PredExp %>%
  group_by(Session, OutcomeNames_Grouped) %>%
  filter(OutcomeNames_Grouped != "EpoxyOnlyLost_NA") %>%
  summarise(count = n()) %>%
  spread(Session, count, fill = 0) %>% 
  select(-OutcomeNames_Grouped) %>%
  as.matrix() %>%  # Create a matrix out of the contingency table
  print()

Outcome_Session_ContingencyTable %>% 
  fisher.test(simulate.p.value = TRUE) %>% # Fisher's exact test with Monte Carlo simulations
  print()
  # OUTPUT: Significant differences

## Preyed
PredExp %>%
  {table(.$Session, .$Preyed)} %>%
  print() %>%
  chisq.test() %>%
  print()
  # OUTPUT: No significant differences

## Chipped 
PredExp %>%
  {table(.$Session, .$Chipped)} %>%
  print() %>%
  chisq.test() %>%
  print()
  # OUTPUT: Significant differences

## Empty shell
PredExp %>%
  {table(.$Session, .$EmptyShell)} %>%
  print() %>%
  fisher.test(simulate.p.value = TRUE) %>%
  print()
  # OUTPUT: No significant differences

## PreyedChipped
PredExp %>%
  {table(.$Session, .$PreyedChipped)} %>%
  print() %>%
  chisq.test() %>%
  print()
  # OUTPUT: Significant differences

####### Comparison of shell length between sessions ####### 
### Data
PredExp %>% 
  group_by(Session) %>% 
  summarise(
    Mean_ShellLength = mean(ShellLength),
    SD_ShellLength = sd(ShellLength)) %>%
  print() # Mean and sd of shell length of snails (morphs pooled) in sessions 1, 2

### Wilkoxon rank-sum test
## Assumptions of test
hist(PredExp$ShellLength) # Histogram of shell lengths
plot(density(PredExp$ShellLength)) # Density Plot
boxplot(PredExp$ShellLength) # Box Plot
qqPlot(PredExp$ShellLength) # Q-Q Plot
shapiro.test(PredExp$ShellLength) # Shapiro-Wilk test
leveneTest(ShellLength ~ Session, data = PredExp) # Levene's test
  # OUTPUT: Shapiro-Wilk rejects normality; the variances are homogeneous (Therefore non-parametric test)

## Test (Wilkoxon rank-sum test) 
wilcox.test(ShellLength ~ Session, data = PredExp)
  # OUTPUT: Significant differences between sessions in shell length (morphs pooled)

####### Figure S7 /// Comparison tests: Shell length between color morphs for each session #######
### Boxplots
PredExp %>% 
  ggplot(aes(x = ColorMorph_Locality, y = ShellLength, fill = ColorMorph_Locality)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(shape = 16, position = position_jitter(0.1), alpha = 0.5, color = "black") +
  labs(x = "Colour morph",
       y = "Shell length (mm)",
       fill = "Color morph") +
  scale_fill_manual(values = c("grey", "#E7E3DC", "#473F2D", "#F2E66E")) +
  theme_test() +
  theme(axis.text.x = element_text(color = "black", margin = margin(t=5), size=11),
        axis.text.y = element_text(color = "black", margin = margin(r=4),size=12),
        axis.title.x = element_text(margin = margin(t = 12), size = 15), 
        axis.title.y = element_text(margin = margin(r = 12), size = 15),
        plot.margin = margin(t = 10, b =10, l = 10, r = 30),
        strip.text = element_text(colour = "black", size = 13, face = "plain")) +  
  scale_x_discrete(expand = c(0.08, 0.3),
                   labels = expression(italic("Lineata")~"(autoch.)", italic("Lineata")~"(alloch.)", italic("Nigra"), italic("Lutea"))) +  
  coord_cartesian(ylim = c(5.5, 9.5)) + 
  guides(fill = "none") +
  facet_wrap(~ Session, nrow = 1, labeller = labeller(Session = c("1" = "Session 1", "2" = "Session 2")))

## Tests (Kruskall-Wallis, ShellLength not normally distributed)
PredExp %>%
  filter(Session == 1) %>%
  kruskal.test(ShellLength ~ ColorMorph_Locality, data = .) # Session 1
  # OUTPUT: Significant differences between color morphs in shell length (Session 1)

PredExp %>%
  filter(Session == 2) %>%
  kruskal.test(ShellLength ~ ColorMorph_Locality, data = .) # Session 2
  # OUTPUT: Significant differences between color morphs in shell length (Session 2)

####### Table 3 /// Logistic Regressions on the Preyed + Chipped outcome #######
PreyedChippedModel <- glm(PreyedChipped ~ ColorMorph_Locality + ShellLength + Session + TransectPosition, 
                          data = PredExp %>% drop_na(), family = binomial) # Logistic regression
summary(PreyedChippedModel) # Summary of the model
exp(cbind(OR = coef(PreyedChippedModel), confint(PreyedChippedModel))) # Calculate odds ratios and 95% CI
  # OUTPUT: Session significant predictor, ColorMorph_Locality (Lutea case) almost significant

####### Power Analysis based on results of Table 3 #######
powerSim(PreyedChippedModel, nsim = 1000) # Power analysis of the original sample
# OUTPUT: Power for predictor 'ColorMorph_Locality', (95% confidence interval): 60.20% (57.09, 63.25) [Likelihood ratio test, 1000 simulations, alpha = 0.05]

### Power Analysis for a larger sample (n = 350, not counting simulated NAs)
set.seed(112) # For reproducibility
PredExp_350 <- PredExp %>%
  sample_n(size = 350, replace = TRUE)  # Sample with replacement to get 350 rows (sample size = 350) based on our actual results
table(PredExp_350$ColorMorph_Locality) # The number of individuals per color is not equal but reasonably similar
PredExp_350$ColorMorph_Locality <- factor(PredExp_350$ColorMorph_Locality, 
                                          levels = c("Lineata_Silleiro", 
                                                     "Lineata_Aguncheiro", 
                                                     "Nigra_Aguncheiro", 
                                                     "Lutea_Aguncheiro")) # Ensure the factor levels are maintained in the ColorMorph_Locality variable
PreyedChippedModel_350 <- glm(PreyedChipped ~ ColorMorph_Locality + ShellLength + 
                                Session + TransectPosition, 
                              data = PredExp_350 %>% drop_na(), family = binomial) # Run the logistic regression with the increased sample
summary(PreyedChippedModel_350) # Summary of the model with enlarged sample
powerSim(PreyedChippedModel_350, nsim = 1000) # Power analysis of the enlarged sample (n = 350)
# OUTPUT: Power for predictor 'ColorMorph_Locality', (95% confidence interval): 98.00% (96.93, 98.77) [Likelihood ratio test, 1000 simulations, alpha = 0.05]

### Power Analysis for a even larger sample (n = 400, not counting simulated NAs)
set.seed(111) # For reproducibility
PredExp_400 <- PredExp %>%
  sample_n(size = 400, replace = TRUE)  # Sample with replacement to get 400 rows (sample size = 400) based on our actual results
table(PredExp_400$ColorMorph_Locality) # The number of individuals per color is not equal but reasonably similar
PredExp_400$ColorMorph_Locality <- factor(PredExp_400$ColorMorph_Locality, 
                                          levels = c("Lineata_Silleiro", 
                                                     "Lineata_Aguncheiro", 
                                                     "Nigra_Aguncheiro", 
                                                     "Lutea_Aguncheiro")) # Ensure the factor levels are maintained in the ColorMorph_Locality variable
PreyedChippedModel_400 <- glm(PreyedChipped ~ ColorMorph_Locality + ShellLength + 
                                Session + TransectPosition, 
                              data = PredExp_400 %>% drop_na(), family = binomial) # Run the logistic regression with the increased sample
summary(PreyedChippedModel_400) # Summary of the model with enlarged sample
powerSim(PreyedChippedModel_400, nsim = 1000) # Power analysis of the enlarged sample (n = 400)
# OUTPUT: Power for predictor 'ColorMorph_Locality', (95% confidence interval): 96.20% (94.82, 97.30) [Likelihood ratio test, 1000 simulations, alpha = 0.05]

####### Table S2 /// Logistic Regressions on Preyed outcome #######
PreyedModel <- glm(Preyed ~ ColorMorph_Locality + ShellLength + Session + TransectPosition, 
                   data = PredExp %>% drop_na(), family = binomial) # Logistic regression
summary(PreyedModel) # Summary of the model
exp(cbind(OR = coef(PreyedModel), confint(PreyedModel))) # Calculate odds ratios and 95% CI
  # OUTPUT: No significant predictors

####### Table S3 /// Logistic Regressions on Chipped outcome ####### 
ChippedModel <- glm(Chipped ~ ColorMorph_Locality + ShellLength + Session + TransectPosition, 
                   data = PredExp %>% drop_na(), family = binomial) # Logistic regression
summary(ChippedModel) # Summary of the model
exp(cbind(OR = coef(ChippedModel), confint(ChippedModel))) # Calculate odds ratios and 95% CI
  # OUTPUT: Intercept, ShellLength, and Session significant

####### Table S4 /// Logistic Regressions on EmptyShell outcome #######     
EmptyShellModel <- glm(EmptyShell ~ ColorMorph_Locality + ShellLength + Session + TransectPosition, 
                    data = PredExp %>% drop_na(), family = binomial) # Logistic regression
summary(EmptyShellModel) # Summary of the model
exp(cbind(OR = coef(EmptyShellModel), confint(EmptyShellModel))) # Calculate odds ratios and 95% CI
  # OUTPUT: No significant predictors

####### Comparison tests of outcomes per color morph for Session 1 #######
### Preyed
PredExp %>%
  filter(Session == "1") %>%
  {table(.$ColorMorph_Locality, .$Preyed)} %>%
  print() %>%
  fisher.test(simulate.p.value = TRUE) %>%
  print()
  # OUTPUT: No significant differences

### Chipped
PredExp %>%
  filter(Session == "1") %>%
  {table(.$ColorMorph_Locality, .$Chipped)} %>%
  print() %>%
  fisher.test(simulate.p.value = TRUE) %>%
  print()
  # OUTPUT: No significant differences

### PreyedChipped
PredExp %>%
  filter(Session == "1") %>%
  {table(.$ColorMorph_Locality, .$PreyedChipped)} %>%
  print() %>%
  fisher.test(simulate.p.value = TRUE) %>%
  print()
  # OUTPUT: Significant differences 

####### Table S5 /// Logistic Regressions on Preyed + Chipped per session #######
# Session 1
PreyedChippedModel1 <- glm(PreyedChipped ~ ColorMorph_Locality + ShellLength + TransectPosition, 
                          data = PredExp %>% filter(Session == 1) %>% drop_na(), family = binomial) # Logistic regression
summary(PreyedChippedModel1) # Summary of the model
exp(cbind(OR = coef(PreyedChippedModel1), confint(PreyedChippedModel1))) # Calculate odds ratios and 95% CI
  # OUTPUT: Intercept and ColorMorph_Locality (Lutea case) significant predictor

# Session 2
PreyedChippedModel2 <- glm(PreyedChipped ~ ColorMorph_Locality + ShellLength + TransectPosition, 
                           data = PredExp %>% filter(Session == 2) %>% drop_na(), family = binomial) # Logistic regression
summary(PreyedChippedModel2) # Summary of the model
exp(cbind(OR = coef(PreyedChippedModel2), confint(PreyedChippedModel2))) # Calculate odds ratios and 95% CI
  # OUTPUT: No significant predictors

####### Chi-squared or Fisher test of outcomes between color morphs (sessions 1, 2 pooled) ####### 
## Preyed
PredExp %>%
  {table(.$ColorMorph_Locality, .$Preyed)} %>%
  print() %>%
  fisher.test(simulate.p.value = TRUE) %>%
  print()
  # OUTPUT: No significant differences

## Chipped
PredExp %>%
  {table(.$ColorMorph_Locality, .$Chipped)} %>%
  print() %>%
  fisher.test(simulate.p.value = TRUE) %>%
  print()
  # OUTPUT: No significant differences

## EmptyShell
PredExp %>%
  {table(.$ColorMorph_Locality, .$EmptyShell)} %>%
  print() %>%
  fisher.test(simulate.p.value = TRUE) %>%
  print()
  # OUTPUT: No significant differences

## PreyedChipped
PredExp %>%
  {table(.$ColorMorph_Locality, .$PreyedChipped)} %>%
  print() %>%
  chisq.test() %>%
  print()
  # OUTPUT: Near significant differences (p = 0.05382)

####### Chi-squared test of outcomes between transect positions #######
## Preyed
PredExp %>%
  {table(.$TransectPosition, .$Preyed)} %>%
  print() %>%
  chisq.test() %>%
  print()
  # OUTPUT: No significant differences

## Chipped 
PredExp %>%
  {table(.$TransectPosition, .$Chipped)} %>%
  print() %>%
  chisq.test() %>%
  print()
  # OUTPUT: No significant differences

## EpoxyOnly 
PredExp %>%
  {table(.$TransectPosition, .$EpoxyOnly)} %>%
  print() %>%
  chisq.test() %>%
  print()
  # OUTPUT: No significant differences

## PreyedChipped 
PredExp %>%
  {table(.$TransectPosition, .$PreyedChipped)} %>%
  print() %>%
  chisq.test() %>%
  print()
  # OUTPUT: No significant differences
