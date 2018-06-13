library(dplyr)
library(knitr)
library(ggplot2)
library(tidyr)
library(readr)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
survey_df <- read.csv(file = "survey_results_public.csv", encoding = "UTF-8")

# Plot University
plot(survey_df$University)

# Bar Chart Plot Country
plot(survey_df$Country)
summary(survey_df)
# sapply(survey_df, class)

# Table for Country
Country_table <-  survey_df %>%
  group_by(Country) %>%
  summarise(number = n())

# Bar Chart ggplot for Country
ggplot(survey_df, aes(x = Country)) +
  geom_bar()

# GGplot Bubble Chart for Country
Quantity <- runif(201,1,10)
ggplot(Country_table, aes(x = Country, y = number, label = Country)) +
  geom_point(aes(size = Quantity),alpha=.2) +
  geom_text(hjust = 1, size = 2) +
  scale_size(range = c(1,10)) +
  theme_bw()

ggplot() + 
  geom_bar(aes(y = Salary, x = FormalEducation), data = survey_df,stat="identity") +
  scale_x_discrete(labels = function(x) strwrap(x, width = 30)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot() + 
  geom_bar(aes(y = Salary, x = JobSatisfaction), data = survey_df,stat="identity") +
  #scale_x_discrete(labels = function(x) strwrap(x, width = 30)) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))

View(survey_df)
  
