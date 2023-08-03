### BLOOD LEAD AND CADMIUM LEVELS AMONG FIREWOOD USERS, SMOKERS AND PASSIVE SMOKERS(CONTROL)
## AIMS AND OBJECTIVES
# 1. To determine the levels of blood cadmium and lead in active firewood users, smokers and passive smokers
# 2. To assess renal function indices (Urea and creatine) in actvie firewood users, smokers and passive smokers
# 3. To compare the renal function indices and blood cadmium and lead levels

install.packages("nortest")
install.packages("ggpubr")
install.packages("learnr")
#  loading the libraries that will be used)
library(tidyverse)  #data manipulation
library(learnr)  # exports data from r to excel
library(dplyr)   #compute summary statistics
library(readxl)  # read excel files
library(readr)  # read CSV files
library(plotrix)  # plot 3D graphs
library(epiDisplay)  # present graph and table in ho
library(ggplot2)  # data visualisation
library(ggpubr) # used to create graphs
library(patchwork) # used to create multple plot on a page
library(Hmisc) # used to run correlation
library(nortest) # used for normality test


#setting my working directory

setwd("C:/Users/User/Documents/R/TAYE")
library(readxl)
PASSIVE <- read_excel("TAYE'S RAW DATA 2 (1).xlsx", sheet = "PASSIVE") # importing the dataset for passive smokers
FIREWOOD <- read_excel("TAYE'S RAW DATA 2 (1).xlsx", sheet = "FIREWOOD") # importing the dataset for firewood users
CIGAR <- read_excel("TAYE'S RAW DATA 2 (1).xlsx", sheet = "CIGARETTE") # importing the dataset for cigarette
ALL <- read_excel("TAYE'S RAW DATA 2 (1).xlsx", sheet = "OVERALL") # importing the dataset for all participants

str(ALL) #What is the structure of our dataset
summary(ALL) # quick descriptive statistics

# 1. To determine the levels of blood cadmium and lead in active firewood users, smokers and passive smokers

# Ans: What is the difference in the blood cadmium and lead level across the groups of participants, i am goint to use the t-test
# subsetting the dataset for cadmium in the three groups; passive, firewood user, cigarette smoker
cadmium1 <- subset(ALL$`Cd ug/dl`, ALL$GROUP == "Passive")
cadmium2 <- subset(ALL$`Cd ug/dl`, ALL$GROUP == "Firewood User")
cadmium3 <- subset(ALL$`Cd ug/dl`, ALL$GROUP == "Cigarette smoker")

# subsetting the dataset for lead in the three groups; passive, firewood user, cigarette smoker
lead1 <- subset(ALL$`Pb ug/dl`, ALL$GROUP == "Passive")
lead2 <- subset(ALL$`Pb ug/dl`, ALL$GROUP == "Firewood User")
lead3 <- subset(ALL$`Pb ug/dl`, ALL$GROUP == "Cigarette smoker")
# lets see what the summary looks like
summary(cadmium1)
summary(cadmium2)
summary(cadmium3)
summary(lead1)
summary(lead2)
summary(lead3)
# Now, we fire down our t-test

# Between passive and firewood users
t.test(cadmium1, cadmium2)

# Between passive and cigarette smokers
t.test(cadmium1, cadmium3)

# Between cigarette smokers and firewood users
t.test(cadmium3, cadmium2)


# Between passive and firewood users
t.test(lead1, lead2)

# Between passive and cigarette smokers
t.test(lead1, lead3)

# Between cigarette smokers and firewood users
t.test(lead3, lead2)

# Difference in cadmium level Across all the groups
# ANOVA to the rescue!!!
anova.model <- aov(ALL$`Cd ug/dl`~ ALL$GROUP, data= ALL)
summary(anova.model)

# Difference in lead level Across all the groups
# ANOVA to the rescue!!!
anova.model2 <- aov(ALL$`Pb ug/dl`~ ALL$GROUP, data= ALL)
summary(anova.model2)


### correlation
# what is the correlation between cadmium and lead in the passive???
cor(PASSIVE$`Cd ug/dl`,PASSIVE$`Pb ug/dl`, method = "pearson") #just the correlation coefficient 
Passive_cor <- rcorr(PASSIVE$`Cd ug/dl`,PASSIVE$`Pb ug/dl`, type = c("pearson")) # both correlation coefficient and P-value
Passive_cor$r 
Passive_cor$n
Passive_cor$P

# what is the correlation between cadmium and lead in the firewood users???
cor(FIREWOOD$`Cd ug/dl`,FIREWOOD$`Pb ug/dl`, method = "pearson") #just the correlation coefficient 
Firewood_cor <- rcorr(FIREWOOD$`Cd ug/dl`,FIREWOOD$`Pb ug/dl`, type = c("pearson")) # both correlation coefficient and P-value
Firewood_cor$r 
Firewood_cor$n
Firewood_cor$P


# what is the correlation between cadmium and lead in the cigarette smokers???
cor(CIGAR$`Cd ug/dl`,CIGAR$`Pb ug/dl`, method = "pearson") #just the correlation coefficient 
Cigar_cor <- rcorr(CIGAR$`Cd ug/dl`,CIGAR$`Pb ug/dl`, type = c("pearson")) # both correlation coefficient and P-value
Cigar_cor$r 
Cigar_cor$n
Cigar_cor$P


# what is the correlation between cadmium and lead in the overall group???
cor(ALL$`Cd ug/dl`,ALL$`Pb ug/dl`, method = "pearson") #just the correlation coefficient 
All_cor <- rcorr(ALL$`Cd ug/dl`,ALL$`Pb ug/dl`, type = c("pearson")) # both correlation coefficient and P-value

### lets visualise our correlation scatterplot of the lead and cadmium of all the participants

CADMIUM <- as.numeric(ALL$`Cd ug/dl`)
LEAD <- as.numeric(ALL$`Pb ug/dl`)
Overall <- ALL
ggscatter(Overall, x = "LEAD", y = "CADMIUM",
          cor.method = "pearson",
          add = "reg.line", conf.int = TRUE,
          xlab = "Lead ug/dl", ylab = "Cadmium ug/dl")

# boxplot using the palette function for cadmium
GROUP <- as.factor(ALL$GROUP)
statx <- boxplot.stats(CADMIUM)
statxx <- boxplot.stats(LEAD)
ggboxplot (ALL, x = "GROUP", y = "CADMIUM", 
           color = "GROUP",
           palette = c("red", "blue", "006400"))

# boxplot using the palette function for lead
GROUP <- as.factor(ALL$GROUP)
ggboxplot (ALL, x = "GROUP", y = "LEAD",
           color = "GROUP",
           palette = c("red", "blue", "006400"))

ggplot(data = ALL, aes(LEAD, CADMIUM)) +
  geom_point() +  # geom_point is used because we are doing a scatterplot
 geom_smooth() + # to make the plot more clear by adding the line of origin and area of concentration, so to say
  labs(title = "Scatterplot correlation between lead and cadmium among all the participants",
       y = 'lead ug/dl',
       x = 'cadmium ug/dl') + # this is where i write the label
  theme(text = element_text(size = 12)) # this is where i specify the text size
abline(lm(CADMIUM~LEAD), col= "blue")
### I need to perform Post-Hoc analysis, first i will load the library(stats)

library(stats)
TukeyHSD(anova.model, ordered = TRUE, conf.level = 0.95)
TukeyHSD(anova.model2, ordered = TRUE, conf.level = 0.95)

#Descriptive statistics by groups for cadmium

ALL %>%
  group_by(GROUP) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(CADMIUM, na.rm = TRUE), # for mean across the group, where na.rm means remove missing values
    sd = sd(CADMIUM, na.rm = TRUE)) # for standard deviation across the group, where na.rm means remove missing values


#Descriptive statistics by groups for lead

ALL %>%
  group_by(GROUP) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(LEAD, na.rm = TRUE), # for mean across the group, where na.rm means remove missing values
    sd = sd(LEAD, na.rm = TRUE)) # for standard deviation across the group, where na.rm means remove missing values

plot(LEAD, CADMIUM, main = "Correlation between cadmium and lead among all the participants", xlab = "Lead (ug/dl)", ylab = "Cadmium (ug/dl", pch = 16)
abline(lm(CADMIUM~LEAD), col= "blue")

ggboxplot (ALL, aes( X = "GROUP", y = "CADMIUM"))+ 
           geom_boxplot()+
          stat_boxplot(geom = "text", aes(label = "..CADMIUM..", y = "..CADMIUM.."),
                       vjust = -1.5,
                      color = "black", size = 3)



#### 2. To assess renal function indices (Urea and creatine) in actvie firewood users, smokers and passive smokers
# Ans: What is the difference in the blood urea and creatine level across the groups of participants, i am goint to use the t-test
# subsetting the dataset for cadmium in the three groups; passive, firewood user, cigarette smoker
urea1 <- subset(ALL$urea, ALL$GROUP == "Passive")
urea2 <- subset(ALL$urea, ALL$GROUP == "Firewood User")
urea3 <- subset(ALL$urea, ALL$GROUP == "Cigarette smoker")

# subsetting the dataset for lead in the three groups; passive, firewood user, cigarette smoker
creatinine1 <- subset(ALL$creatinine, ALL$GROUP == "Passive")
creatinine2 <- subset(ALL$creatinine, ALL$GROUP == "Firewood User")
creatinine3 <- subset(ALL$creatinine, ALL$GROUP == "Cigarette smoker")
# lets see what the summary looks like
summary(urea1)
summary(urea2)
summary(urea3)
summary(creatinine1)
summary(creatinine2)
summary(creatinine3)

sd(urea1)
sd(urea2)
sd(urea3)
sd(creatinine1)
sd(creatinine2)
sd(creatinine3)
# Now, we fire down our t-test

# Between passive and firewood users
t.test(urea1, urea2)

# Between passive and cigarette smokers
t.test(urea1, urea3)

# Between cigarette smokers and firewood users
t.test(urea3, urea2)


# Between passive and firewood users
t.test(creatinine1, creatinine2)

# Between passive and cigarette smokers
t.test(creatinine1, creatinine3)

# Between cigarette smokers and firewood users
t.test(creatinine3, creatinine2)

# Difference in urea level Across all the groups
# ANOVA to the rescue!!!
anova.model3 <- aov(ALL$urea~ ALL$GROUP, data= ALL)
summary(anova.model3)

# Difference in creatinine level Across all the groups
# ANOVA to the rescue!!!
anova.model4 <- aov(ALL$creatinine~ ALL$GROUP, data= ALL)
summary(anova.model4)


library(stats)
TukeyHSD(anova.model3, ordered = TRUE, conf.level = 0.95)
TukeyHSD(anova.model4, ordered = TRUE, conf.level = 0.95)

UREA <- ALL$urea
CREATININE<- ALL$creatinine

#Descriptive statistics by groups for urea
ALL %>%
  group_by(GROUP) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(UREA, na.rm = TRUE), # for mean across the group, where na.rm means remove missing values
    sd = sd(UREA, na.rm = TRUE)) # for standard deviation across the group, where na.rm means remove missing values


#Descriptive statistics by groups for creatinine

ALL %>%
  group_by(GROUP) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(CREATININE, na.rm = TRUE), # for mean across the group, where na.rm means remove missing values
    sd = sd(CREATININE, na.rm = TRUE)) # for standard deviation across the group, where na.rm means remove missing values

# boxplot using the palette function for urea
ggboxplot (ALL, x = "GROUP", y = "UREA", 
           color = "GROUP",
           palette = c("red", "blue", "006400"))

# boxplot using the palette function for creatinine
ggboxplot (ALL, x = "GROUP", y = "CREATININE",
           color = "GROUP",
           palette = c("red", "blue", "006400"))



### 3. To compare the renal function indices and blood cadmium and lead levels
### Ans = the renal function indices i.e the urea and creatinine will be correlated with cadmium and lead within each of the three groups
### I am using the Hmisc library, then, i will create an object for a new dataset which will exclude unnecessary column of the dataset for each group
### I will then create a correlation matrix of each
### lastly, i will create a correlation matrix plot

summary(PASSIVE)
PASSIVE2 <- subset(PASSIVE, select = c('Urea (mmol/l)', 'creatininine (umol/l)', 'Cd ug/dl', 'Pb ug/dl'))
FIREWOOD2 <- subset(FIREWOOD, select = c('urea', 'creatinine', 'Cd ug/dl', 'Pb ug/dl'))
CIGAR2 <- subset(CIGAR, select = c('urea', 'creatinine', 'Cd ug/dl', 'Pb ug/dl'))

passive_cor_matrix <- rcorr(as.matrix(PASSIVE2))
firewood_cor_matrix <- rcorr(as.matrix(FIREWOOD2))
cigar_cor_matrix <- rcorr(as.matrix(CIGAR2))

passive_cor_matrix
firewood_cor_matrix
cigar_cor_matrix


########## Costructing multiple graph on a page for passive smokers
par(mfrow = c(4,4))
plot(PASSIVE2, ) +
  title(main = "inter-correlation matrix plot for passive smokers", outer = TRUE) ### Adding a title where the title is placed outside the margin of the plot

# Costructing multiple graph on a page for firewood users
par(mfrow = c(4,4))
plot(FIREWOOD2, ) +
  title(main = "inter-correlation matrix plot for firewood users", outer = TRUE)

# Costructing multiple graph on a page for cigar smokers
par(mfrow = c(4,4))
plot(CIGAR2, ) +
  title(main = "inter-correlation matrix plot for cigar smoker", outer = TRUE)

#### time to patch all our graphs
### creating objects for all the plots

# boxplot using the palette function for cadmium

PlotA <- ggboxplot (ALL, x = "GROUP", y = "CADMIUM", 
           color = "GROUP",
           palette = c("red", "blue", "006400"))

# boxplot using the palette function for lead

PlotB <- ggboxplot (ALL, x = "GROUP", y = "LEAD",
           color = "GROUP",
           palette = c("red", "blue", "006400"))

PlotC <- ggplot(data = ALL, aes(LEAD, CADMIUM)) +
  geom_point() +  # geom_point is used because we are doing a scatterplot
  geom_smooth() + # to make the plot more clear by adding the line of origin and area of concentration, so to say
  labs(title = "Scatterplot correlation between lead and cadmium among all the participants",
       y = 'lead ug/dl',
       x = 'cadmium ug/dl') + # this is where i write the label
  theme(text = element_text(size = 12)) # this is where i specify the text size
abline(lm(CADMIUM~LEAD), col= "blue")

# boxplot using the palette function for urea
PlotD <- ggboxplot (ALL, x = "GROUP", y = "UREA", 
           color = "GROUP",
           palette = c("red", "blue", "006400"))

# boxplot using the palette function for creatinine
PlotE <- ggboxplot (ALL, x = "GROUP", y = "CREATININE",
           color = "GROUP",
           palette = c("red", "blue", "006400"))
PlotF <- par(mfrow = c(4,4))
plot(PASSIVE2, ) +
  title(main = "inter-correlation matrix plot for passive smokers", outer = TRUE) ### Adding a title where the title is placed outside the margin of the plot

# Costructing multiple graph on a page for firewood users
PlotG <- par(mfrow = c(4,4))
plot(FIREWOOD2, ) +
  title(main = "inter-correlation matrix plot for firewood users", outer = TRUE)

# Costructing multiple graph on a page for cigar smokers
PlotH <- par(mfrow = c(4,4))
plot(CIGAR2, ) +
  title(main = "inter-correlation matrix plot for cigar smoker", outer = TRUE)

### Time to patch the plots
AllPatch <- (PlotA + PlotB + PlotC + PlotD)/(PlotE + PlotF + PlotG + PlotH)
AllPatch + plot_annotation(tag_levels = 'A') # tag level can be alphabetical use A, numerical use 1, roman numeral use I

ggsave(filename = 'output/three_plots.png', width = 10, height = 5)

