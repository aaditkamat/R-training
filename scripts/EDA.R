library(readr)
library(dplyr)
library(ggplot2)
df <- read.csv("data/training2.csv")

# *******   Question 1: Histogram and Boxplot for Males and Females  *******

# Group by Sex
df <- df %>%
  mutate(Sex = if_else(Sex == 1, "Male", "Female"))

## Quiz scores
# Visual test for normality
png("plots/Q1_histogram.png")
ggplot(data = df, mapping = aes(x = Quiz)) +
  geom_histogram() +
  facet_grid(vars(Sex))

png("plots/Q1_boxplot.png")
ggplot(data = df, mapping = aes(x = Sex, y = Quiz)) +
  geom_boxplot()


MaleQuiz <- df %>% filter(Sex == "Male") %>% select(Quiz)
FemaleQuiz <- df %>% filter(Sex == "Female") %>% select(Quiz)

# *******   Question 2: EDA of Quiz across Sex  *******

# Statistical test for normality for Quiz column
print(ks.test(MaleQuiz, "pnorm", mean(MaleQuiz), sd(MaleQuiz)))
print(ks.test(FemaleQuiz, "pnorm", mean(FemaleQuiz), sd(FemaleQuiz))

## Based on the statistical test, it seems that quiz scores
## are normally distributed for males but not for females

# Statistical test for normality
cols <- c("Age", "Motivation", "SelfConfidence")
for (col in cols) {
MaleCol <- df %>% filter(Sex == "Male") %>% select(Quiz)
FemaleCol <- df %>% filter(Sex == "Female") %>% select(Quiz)
print(ks.test(MaleCol, "pnorm", mean(MaleCol), sd(MaleCol)))
princomp(ks.test(FemaleCol, "pnorm", mean(FemaleCol), sd(FemaleCol)))
}