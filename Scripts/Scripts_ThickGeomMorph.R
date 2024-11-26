########################################################################################
#### Shell measurements of the color morphs (thickness and geometric morphometrics) ####
########################################################################################

###### Packages #######
library(tidyverse) # Basic plots and statistical analyses
library(car) # ANOVAs
library(effectsize) # Effect size of ANOVA of RW1 ~ ColorMorph_Locality

####### Dataset #######
setwd("") # Set the directory of the dataset file
read.csv("Data_ThickGeomMorph.csv") # Read the ThicknessGeometricMorphometrics dataset

####### Data handling and overview #######
ThickGeomMorph <- read_csv("Data_ThickGeomMorph.csv") %>%
  mutate(ColorMorph_Locality = factor(ColorMorph_Locality, 
                                      levels = c("Lineata_Silleiro", "Lineata_Aguncheiro", 
                                                 "Nigra_Aguncheiro", "Lutea_Aguncheiro"))) # Set a shortcut name for the dataset and convert the ColorMorph_Locality to a factor
str(ThickGeomMorph) # Check the structure of the data

####### Figure S3 /// Scatterplot of Relative Warp 1 and Centroid Size for each color morph #######
### Scatterplot
ThickGeomMorph %>% 
  ggplot(aes(x = RW1, y = CS, fill = ColorMorph_Locality, shape = Locality)) +
  geom_point(size = 3) +
  labs(x = "Relative Warp 1", y = "Centroid Size", fill = "Color Morph") +
  scale_fill_manual(values = c("grey", "#E7E3DC", "#473F2D", "#F2E66E")) +
  scale_shape_manual(values = c(Aguncheiro = 21, Silleiro = 24)) +
  theme_bw()+
  theme(axis.text.x = element_text(color = "black", margin = margin(t=5), size=11),
        axis.text.y = element_text(color = "black", margin = margin(r=4),size=12),
        axis.title.x = element_text(margin = margin(t = 12), size = 15), 
        axis.title.y = element_text(margin = margin(r = 12), size = 15),
        plot.margin = margin(t = 10, b =10, l = 10, r = 30),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())+
  stat_ellipse(geom="polygon", alpha=0.25) +
  guides(color = FALSE, shape = FALSE)

####### ANOVA of Centroid Size between color morphs (in caption of Figure S3) #######
### ANOVA 
## Assumptions
hist(ThickGeomMorph$CS, main = "Histogram of Centroid Size") # Histogram
plot(density(ThickGeomMorph$CS), main = "Density Plot of Centroid Size") # Density Plot
boxplot(ThickGeomMorph$CS, main = "Box Plot of Centroid Size") # Box Plot
qqPlot(ThickGeomMorph$CS) # Q-Q Plot
shapiro.test(ThickGeomMorph$CS) # Shapiro-Wilk test
leveneTest(CS ~ ColorMorph_Locality, data = ThickGeomMorph) # Levene's test
  # OUTPUT: Data follows a normal distribution, and the variances are homogeneous

## Test
ThickGeomMorph %>% 
  aov(CS ~ ColorMorph_Locality, data = .) %>% 
  anova()

ThickGeomMorph %>%
  aov(CS ~ ColorMorph_Locality, data = .) %>%
  TukeyHSD() # Post hoc for ANOVA: Tukey's HSD test
  # OUTPUT: Differences in Centroid Size between various color morphs

####### Figure S4 /// Boxplots of Relative Warp 1 by color morph and corresponding ANOVA #######
### Boxplots
ThickGeomMorph %>% 
  ggplot(aes(x = ColorMorph_Locality, y = RW1, fill = ColorMorph_Locality)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(shape = 16, position = position_jitter(0.1), alpha = 0.5, color = "black") +
  labs(x = "Color morph",
       y = "Relative warp 1",
       fill = "Color morph") +
  scale_fill_manual(values = c("grey", "#E7E3DC", "#473F2D", "#F2E66E")) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", margin = margin(t=5), size=11),
        axis.text.y = element_text(color = "black", margin = margin(r=4),size=12),
        axis.title.x = element_text(margin = margin(t = 12), size = 15), 
        axis.title.y = element_text(margin = margin(r = 12), size = 15),
        plot.margin = margin(t = 10, b =10, l = 10, r = 30)) +  
  scale_x_discrete(expand = c(0.08, 0.3),
                   labels = expression(italic("Lineata")~"(autoch.)", italic("Lineata")~"(alloch.)", italic("Nigra"), italic("Lutea"))) +  
  coord_cartesian(ylim = c(-0.2, 0.2)) + 
  guides(fill = "none")

### ANOVA (Figure S4)
## Assumptions
hist(ThickGeomMorph$RW1, main = "Histogram of Relative Warp 1") # Histogram
plot(density(ThickGeomMorph$RW1), main = "Density Plot of Relative Warp 1") # Density Plot
boxplot(ThickGeomMorph$RW1, main = "Box Plot of Relative Warp 1") # Box Plot
qqPlot(ThickGeomMorph$RW1) # Q-Q Plot
shapiro.test(ThickGeomMorph$RW1) # Shapiro-Wilk test
leveneTest(RW1 ~ ColorMorph_Locality, data = ThickGeomMorph) # Levene's test
  # OUTPUT: Data follows a normal distribution and the variances are homogeneous

## Test
ANOVA_TGM_RW1 <- ThickGeomMorph %>%
  aov(RW1 ~ ColorMorph_Locality, data = .) %>%
  anova() 
print(ANOVA_TGM_RW1)
  # OUTPUT: No differences in Relative Warp 1 between color morphs

## Effect size 
eta_squared (ANOVA_TGM_RW1) %>% 
  print()
  # OUTPUT: Low-mid effect, but wide CI (precaution when interpreting the result)

####### Figure S5 /// Boxplots of Shell Thickness by color morph and corresponding Kruskall-Wallis #######
### Boxplot
ThickGeomMorph %>% 
  ggplot(aes(x = ColorMorph_Locality, y = ShellThickness, fill = ColorMorph_Locality)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(shape = 16, position = position_jitter(0.1), alpha = 0.5, color = "black") +
  labs(x = "Color morph",
       y = "Shell thickness (mm)",
       fill = "Color morph") +
  scale_fill_manual(values = c("grey", "#E7E3DC", "#473F2D", "#F2E66E")) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", margin = margin(t=5), size=11),
        axis.text.y = element_text(color = "black", margin = margin(r=4),size=12),
        axis.title.x = element_text(margin = margin(t = 12), size = 15), 
        axis.title.y = element_text(margin = margin(r = 12), size = 15),
        plot.margin = margin(t = 10, b =10, l = 10, r = 30)) +  
  scale_x_discrete(expand = c(0.08, 0.3),
                   labels = expression(italic("Lineata")~"(autoch.)", italic("Lineata")~"(alloch.)", italic("Nigra"), italic("Lutea"))) +  
  coord_cartesian(ylim = c(0, 1)) + 
  guides(fill = "none")

### Test
## Assumptions
hist(ThickGeomMorph$ShellThickness, main = "Histogram of Shell Thickness") # Histogram
plot(density(ThickGeomMorph$ShellThickness), main = "Density Plot of Shell Thickness") # Density Plot
boxplot(ThickGeomMorph$ShellThickness, main = "Box Plot of Shell Thickness") # Box Plot
qqPlot(ThickGeomMorph$ShellThickness) # Q-Q Plot
shapiro.test(ThickGeomMorph$ShellThickness) # Shapiro-Wilk test
leveneTest(ShellThickness ~ ColorMorph_Locality, data = ThickGeomMorph) # Levene's test
  # OUTPUT: The Shapiro-Wilk test is significant (i.e., non-normal distribution). The variances are homogeneous

## Test (Kruskal-Wallis; non-parametric)
ThickGeomMorph %>% 
  kruskal.test(ShellThickness ~ ColorMorph_Locality, data = .)
  # OUTPUT: No differences between color morphs in shell thickness (but near significance, p = 0.06269)

## Effect size (Eta-squared for non-parametric data)
H <- 7.3086 # Kruskal-Wallis value
k <- 4 # Number of morphs compared
n <- 40 # Total sample size
EtaSquared_ShellThickness <- (H - k + 1) / (n - k) # Formula of Eta-squared (non-parametric)
print(EtaSquared_ShellThickness)
  # OUTPUT: Moderate effect (=0.1196)
