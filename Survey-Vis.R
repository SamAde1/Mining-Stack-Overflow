library(dplyr)
library(knitr)
library(ggplot2)
library(tidyr)
library(readr)
survey_df <- read.csv(file = "survey_results_public.csv", encoding = "UTF-8")

# Plot University
plot(survey_df$University)

# Bar Chart Plot Country
plot(survey_df$Country)
# summary(survey_df)
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

for ( i in seq(1,length( survey_df ),1) ) plot(survey_df[,i],ylab=names(survey_df[i]),type="l")

  