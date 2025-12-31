#dataset is Sandiego
rm(list = ls())  # clears global environment
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
options(scipen = 100) # disables scientific notion for entire R session
options(digits=2) #reduces the output of decimal places in integers
cat("\014")  # clears console

install.packages("pacman")
install.packages("tidyverse")

library(pacman)
library(tidyverse)
p_load(tidyverse)

filename <- file.choose()
data <- readRDS(filename)

names(data)
#Reduce my table to feature only relevant columns
data <- data[, c("date", "service_area", "subject_age", 
                 "subject_race", "subject_sex", "type", 
                 "arrest_made", "citation_issued", 
                 "warning_issued", "outcome")]


###Examples of other ways to change column types. these already are these types but it's good to know the functions###
data$date <- as.Date(data$date)
data$subject_race <- factor(data$subject_race)
data$subject_sex <- factor(data$subject_sex)
data$type <- factor(data$type)
data$arrest_made <- as.logical(data$arrest_made)
data$citation_issued <- as.logical(data$citation_issued)
data$warning_issued <- as.logical(data$warning_issued)
data$outcome <- factor(data$outcome)
###################################################################

install.packages("dplyr")
install.packages("lubridate")
library(dplyr)
library(lubridate)

# Add new column with day of week
data <- data |> mutate(dayofweek = wday(date, label = TRUE))


#remove Other, Unknown and NA from table
data <- data |> filter(subject_race != 'unknown', 
                       subject_race != 'other', subject_race != 'NA')

# More advanced cross-tabulation
install.packages('gmodels')
library(gmodels)

##
#check missing data
sum(is.na(data$dayofweek))

#delete missing data
data <- na.omit(data)

#ensure correct order
data$dayofweek <- factor(data$dayofweek,
                         levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
                         ordered = TRUE)

library(ggplot2) #library for visualizations
# Histogram of stops by day of week
ggplot(data, aes(x = dayofweek)) +
  geom_bar( fill = "lightblue", color = "black") +
  labs(title = "Distribution of Traffic Stops", 
       x = "Day of Week", y = "Frequency")

install.packages("tidyverse")
library(tidyverse) #need to run "summarise". If doesn't work try p_load(tidyverse)
# Calculate the proportion of arrest for each race
search_proportions <- data |>
  group_by(subject_race) |>
  summarise(arrest_made = mean(arrest_made, na.rm = TRUE))

#install package formattable to use percent function below
install.packages('formattable')
library("formattable") 

####More interactive style plots#####
install.packages('plotly')
library(plotly)
p <- ggplot(data, aes(x = subject_race, fill = subject_sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Subject Sex by Race", x = "Race", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(p)
########

# Faceted histogram using ggplot2 to compare across multiple classes
ggplot(data, aes(x = dayofweek)) +
  geom_bar(fill = "lightblue", color = "black") +
  facet_wrap(~ subject_race) +
  labs(title = "Distribution of Stop Times by Race",
       x = "Time of Day",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



############MOD 2###################
# Im going to add "subject_age" to the data df and re-run the script.
# This includes the same clean up script we ran including removing NAs

# Load necessary libraries

install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")

library(dplyr)
library(ggplot2)
library(psych)

# Clean up the age column
age <- data |> group_by(subject_age) |> summarize(counts = n())

data <- data |> filter(subject_age != 'NA') #remove NAs

data <- data |> filter(subject_age > 16) #remove age less than legal driving age (16 in MA)

#Visualizations
## Scatter plot of age vs. outcome (using jitter to handle categorical outcome)
png("scatter_plot.png", width = 800, height = 600)
ggplot(data, aes(x = subject_age, y = outcome)) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  labs(title = "Age vs. Outcome",
       x = "Age", y = "Outcome")
dev.off()

#Review the outcome frequency chart
outcome <- data |> group_by(outcome)|> summarize(counts = n())
#Reduce table to new outcome table with only the two columns needed for the chart
data_outcome <- data[, c("outcome", "subject_age")] |> 
  filter(outcome != 'NA')
#Re-run the scatter plot above but change the "data" to "data_outcome"

## Jitter plot for arrest made by race
png("jitter_plot1.png", width = 800, height = 600)
ggplot(data, aes(x = subject_race, y = arrest_made)) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  labs(title = "Arrest Made by Race",
       x = "Race", y = "Arrest Made")
dev.off()

## Boxplot of age by race to detect outliers
png("boxplot.png", width = 800, height = 600)
boxplot(subject_age ~ subject_race, data = data,
        main = "Age Distribution by Race",
        xlab = "Race", ylab = "Age")
dev.off()


#Module 3 Sandiego
# Load necessary libraries
install.packages("dplyr")
library(dplyr)

#Load dataset
filename <- file.choose()
data <- readRDS(filename)

# Conduct a one-sample t-test for mean age
## Null Hypothesis (H₀): The mean age of individuals stopped is equal to 34 years.
## Alternative Hypothesis (H₁): The mean age of individuals stopped is not equal to 34 years.

# Perform one-sample t-test
t_test_age <- t.test(data$subject_age, mu = 34, alternative = "two.sided")

#t.test(data$subject_age, mu = 34, alternative = "greater") tests if mean age is greater than 34
#t.test(data$subject_age, mu = 34, alternative = "less") tests if mean age is less than 34

# Print the results of the t-test
print(t_test_age)

# Interpretation:
cat("The one-sample t-test compares the sample mean age to a hypothesized population mean of 35 years. 
The null hypothesis assumes that the true mean age is equal to 35 years. 
If the p-value is less than our significance level (usually 0.05), we reject the null hypothesis and 
conclude that the mean age is significantly different from 35 years.\n")

# Check the p-value
if(t_test_age$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis. 
      There is evidence to suggest that the mean age of individuals stopped is significantly different from 35 years.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis. 
      There is no significant evidence to suggest that the mean age differs from 35 years.\n")
}

"Hypotheses
Null Hypothesis (H₀): There is no association between race (Black vs. White) 
and the likelihood of being searched during a traffic stop. 
In other words, Black drivers are not more likely to be searched than White drivers.

Alternative Hypothesis (H₁): There is an association between race (Black vs. White) 
and the likelihood of being searched during a traffic stop. 
In other words, Black drivers are more likely to be searched than White drivers."


# Create a contingency table for the observed frequencies
# Use frequency from race table:
#   race <- data |> group_by(subject_race) |> summarize(counts = n())
#     Black counts (Searched 3873 Not Searched 38832) | Hispanic counts (Searched 6501 Not Searched 110582)

table(data$subject_race, data$search_conducted)

observed <- matrix(c(3873, 38832, 6501, 110582), nrow = 2, byrow = TRUE,
                   dimnames = list(Race = c("Black", "Hispanic"),
                                   Search_Conducted = c("Yes", "No")))

# Print the observed contingency table
print(observed)

# Perform the Chi-square test for independence
chi_test <- chisq.test(observed)

# Print the results of the Chi-square test
print(chi_test)

# Interpretation based on p-value
if(chi_test$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis.
      There is evidence to suggest that Black drivers are more likely to be searched than White drivers.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis.
      There is no significant evidence to suggest that Black drivers are more likely to be searched than White drivers.\n")
}


####Correaltion heatmap and regression model##########

rm(list = ls())  # clears global environment
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
options(scipen = 100) # disables scientific notion for entire R session
cat("\014")  # clears console

install.packages("palmerpenguins")
install.packages("dplyr")
install.packages("lubradate")
library(palmerpenguins)
library(dplyr)
library(lubridate)

filename <- file.choose()
data <- readRDS(filename)

summary(data)

aggregated_data <- data %>%
  mutate(dayofweek = wday(date, label = TRUE),
         day = as.Date(date)) %>%
  
  group_by(day) %>%
  summarise(
    number_of_arrest = sum(arrest_made == TRUE, na.rm = TRUE),
    number_for_stop = sum(reason_for_stop == "Moving Violation", na.rm = TRUE),
    subject_age_mean = mean(subject_age, na.rm = TRUE),
    subject_race = first(subject_race),
    number_of_person = sum(search_person == TRUE, na.rm = TRUE),
    dayofweek = first(dayofweek),
    .groups = 'drop'
  )

head(aggregated_data)

install.packages("dplyr")
library(dplyr)

##Correlation
aggregated_data_metric <- aggregated_data |> select(number_of_arrest, number_for_stop, 
                                                    subject_age_mean, number_of_person) |> na.omit()

cor_matrix <- cor(aggregated_data_metric)
print(cor_matrix)  

library(corrplot)
png('corrheat_Sandiego3.png', width = 800, height = 800)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
dev.off()

#Regression  
lm_model <- lm(number_of_arrest ~ number_for_stop + subject_age_mean + number_of_person +subject_race + 
                 dayofweek , data = aggregated_data)

summary(lm_model)

#Prediction
#   y = mx + b
(0.005421*x)+(-0.104659*c)+(0.397935*v)+(subject_race)+(dayofweek.C)+(3.854331)

asian/pacific_islander = 0 #Reference
black = 0.572206
hispanic = 0.286393
white = 0.248593
dayofweek.C = -0.223979 

#Pick row #129
x = 212
c = 36.32680
v = 12
subject_race = white

