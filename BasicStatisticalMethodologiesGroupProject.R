#Loading Packages
library(readr)
library(dplyr)
library(outliers)
library(tidyverse)
library(ggplot2)

#Importing the Dataset
insurance <- read_csv('insurance.csv')

#Checking the Dimensions of the Dataset
dim(insurance)

#Checking Variable Types of Each Column
str(insurance)

#Converting Incorrectly assigned Character Variables to Factor Data Type
insurance$sex <- factor(insurance$sex)
insurance$smoker <- factor(insurance$smoker)
insurance$region <- factor(insurance$region)

#Checking the categories of all factor variables
levels(insurance$sex)
levels(insurance$smoker)
levels(insurance$region)

#Rechecking Variable Types of Each Column
str(insurance)

#Scanning for NA and NaN Values
colSums(is.na(insurance))

#Scanning for -Inf and Inf Values
sapply(insurance, function(x) sum(is.infinite(x)))

#Scanning for Obvious Errors / Inconsistencies in age
summary(insurance$age)

#Scanning for Obvious Errors / Inconsistencies in bmi
summary(insurance$bmi)

#Scanning for Obvious Errors / Inconsistencies in charges
summary(insurance$charges)

#Scanning for Obvious Errors / Inconsistencies in children
summary(insurance$children)

#Boxplots of all numeric columns to check the presence of Outliers
insurance$age %>%  boxplot(main="Boxplot of age", ylab="age", col = "grey")
insurance$bmi %>%  boxplot(main="Boxplot of bmi", ylab="bmi", col = "grey")
insurance$charges %>%  boxplot(main="Boxplot of charges", ylab="charges", col = "grey")
insurance$children %>%  boxplot(main="Boxplot of children", ylab="children", col = "grey")

#Locating the outliers in bmi column using Z-score method
z.scores_bmi <- insurance$bmi %>%  scores(type = "z")
z.scores_bmi %>% summary()
length(which(abs(z.scores_bmi)>3))
which(abs(z.scores_bmi)>3)

#Locating the outliers in charges column using Z-score method
z.scores_charges <- insurance$charges %>%  scores(type = "z")
z.scores_charges %>% summary()
length(which(abs(z.scores_charges)>3))
which(abs(z.scores_charges)>3)

#Removing the rows in the dataset with outlier values
outlier_rows <- c(which(abs(z.scores_bmi)>3),which(abs(z.scores_charges)>3))
insurance_clean<- insurance[-outlier_rows,]

#Creating new sex_num column based on sex column
insurance_clean <- insurance_clean %>% mutate(sex_num = case_when(sex == 'female' ~ 0, sex == 'male' ~ 1))

#Creating new smoker_num column based on smoker column
insurance_clean <- insurance_clean %>% mutate(smoker_num = case_when(smoker == 'no' ~ 0, smoker == 'yes' ~ 1))

#Creating new region_num column based on region column
insurance_clean <- insurance_clean %>% mutate(region_num = case_when(region == 'southwest' ~ 1, region == 'southeast' ~ 2, region == 'northwest' ~ 3, region == 'northeast' ~ 4))

#Saving the cleaned dataset as a csv file
write.csv(insurance_clean,'insurance_clean.csv')

#Boxplot of Charges vs Gender
insurance_clean %>%
  ggplot( aes(x=sex, y=charges, fill=sex)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  scale_fill_brewer(palette="Set3") +
  ggtitle("Boxplot of Charges vs Gender") +
  xlab("Gender") +
  ylab("Charges")

#Boxplot of Charges vs Smoker
insurance_clean %>%
  ggplot( aes(x=smoker, y=charges, fill=smoker)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  scale_fill_brewer(palette="Set2") +
  ggtitle("Boxplot of Charges vs Smoker/Non-Smoker") +
  xlab("Smoker") +
  ylab("Charges")

#Boxplot of Charges vs Region
insurance_clean %>%
  ggplot( aes(x=region, y=charges, fill=region)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Boxplot of Charges vs Region") +
  xlab("Region") +
  ylab("Charges")

#Boxplot of Charges vs Children
insurance_clean %>%
  ggplot( aes(x=factor(children), y=charges, fill=factor(children))) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  scale_fill_brewer(palette="Paired") +
  ggtitle("Boxplot of Charges vs Children") +
  xlab("Children") +
  ylab("Charges")



