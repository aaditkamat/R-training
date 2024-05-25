library(readr)
library(tidyr)
library(dplyr)

airlines_df <- read.csv("data/airlines.csv")
print(head(airlines_df))

# Replace invalid values with NA
airlines_df <- airlines_df %>%
  mutate(Males = na_if(Males, 999.0)) %>%
  mutate(Females = na_if(Females, 999.0))

# Remove the duplicate cases in the data set
airlines_df <- union(airlines_df, airlines_df)

# Get the descriptive statistics
print(summary(airlines_df))

# Find the case with the minimum number of suitcases
print(airlines_df %>% filter(Suitcases == min(airlines_df$Suitcases)))
## Given that there are 233 passengers on board the plane, 44 suitcases seems
## to be very low

## Find the plane where the cargois equal to 2.12 ton
print(airlines_df %>% filter(Cargo == 2120))

## Given that the average weight per passenger is 60kg and average weight
## per suitcase is 15kg, the total minimum cargo should be 204*60 + 188 *15=15k
# tons

## Change negative to positive numbers
airlines_df <- airlines_df %>% mutate(across(where(is.numeric), abs))

# Replace minimum suitcases with total number of passengers for the flight
airlines_df <- airlines_df %>%
  mutate(Suitcases = ifelse(Suitcases == min(airlines_df$Suitcases),
    Passengers, Suitcases
  ))

# Replace cargo of 2.12 ton with calculated cargo
kAveragePassengerWeight <- 60 # nolint
kAverageSuitcaseWeight <- 15 # nolint
airlines_df <- airlines_df %>%
  mutate(Cargo = ifelse(Cargo == 2120, Passengers * kAveragePassengerWeight +
      Suitcases * kAverageSuitcaseWeight, Cargo
  ))

## Replace anomalies
airlines_df <- airlines_df %>%
  mutate(Pets = ifelse(Pets == "M", "Y", Pets)) %>%
  mutate(Pets = ifelse(Pets == "B", "N", Pets)) %>%
  mutate(Satisfaction = ifelse(Satisfaction > 5 | Satisfaction < 1,
    abs(Satisfaction - 5), Satisfaction
  ))

## Replace missing values
airlines_df <- airlines_df %>%
  mutate(Females = ifelse(is.na(Females), Passengers - Males, Females)) %>%
  mutate(Males = ifelse(is.na(Males), Passengers - Females, Males))

airlines_df["No. of Children"] <- airlines_df %>%
  select("Children") %>%
  mutate(Children = ifelse(is.na(Children),
    mean(airlines_df$Children),
    Children
  ))

## Dealing with outliers
for (col in colnames(airlines_df)) {
  if (is.numeric(airlines_df$col) || is.integer(airlines_df$col)) {
    boxplot(airlines_df$col)
  }
}

# Normalize the variables using min-max scaling
cols <- c("Passengers", "Suitcases", "Cargo")

airlines_df <- airlines_df %>%
  mutate(across(cols, ~ (. - min(.)) / (max(.) - min(.))))

airlines_df$ImpIndex <- airlines_df %>%
  select(all_of(cols)) %>%
  mean()

# Create plots
print(airlines_df$ImpIndex)
boxplot(airlines_df$ImpIndex)
hist(airlines_df$ImpIndex)
