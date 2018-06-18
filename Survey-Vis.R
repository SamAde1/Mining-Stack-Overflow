extrafont::loadfonts(device="win")
library(dplyr)
library(knitr)
library(ggplot2)
library(tidyr)
library(readr)
library(plotly)
library(scales)
library(stacksurveyr)
library(magrittr)
library(stringr)

globalVariables(c("respondent_id", "column", "answer", "one_of", "stack_schema", "survey_df"))


survey_df <- read.csv(file = "survey_results_public.csv", encoding = "UTF-8")

# sapply(survey_df, class)


Country_Sum = as.data.frame(table(survey_df$Country))

Country_Sum <- filter(Country_Sum, Freq >870 | Var1 == "Ireland")
print(Country_Sum)

# Bar Chart ggplot for Country
co <- ggplot(Country_Sum, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))
gg <- ggplotly(co)
layout(gg, dragmode = "pan")
rangeslider(gg)

# Bar Chart ggplot for Professional
ggplot(survey_df, aes(Professional)) +
  geom_bar() +
  aes(stringr::str_wrap(Professional, 15)) +
  #aes(x = reorder(Professional,Professional,function(x)-length(x))) +
  xlab("Professional")

# Bar Chart ggplot for ProgramHobby
ggplot(survey_df, aes(x=reorder(ProgramHobby,ProgramHobby,function(x)-length(x)))) +
  geom_bar() +
 scale_x_discrete(labels = function(y) strwrap(y, width = 40)) +
  xlab("Program Hobby")

# Bar Chart ggplot for University
ggplot(survey_df, aes(x=reorder(University,University,function(x)-length(x)))) +
  geom_bar() +
  scale_x_discrete(labels = function(y) strwrap(y, width = 40)) +
  xlab("University")

# Bar Chart ggplot for EmploymentStatus
  ggplot(survey_df, aes(x=reorder(EmploymentStatus,EmploymentStatus,function(x)-length(x)))) +
  geom_bar() +
  #scale_x_discrete(labels = function(y) strwrap(y, width = 15)) +
  xlab("Employment Status") +
  coord_flip()



ggplot() + 
  geom_bar(aes(y = Salary, x = FormalEducation), data = survey_df %>% drop_na(Salary),stat="identity") 
  #scale_x_discrete(labels = function(x) strwrap(x, width = 30)) +
  #theme(axis.title.x = element_text(margin(t = 20, r = 10, b = 10, l = 10)))

ggplot() + 
  geom_bar(aes(y = Salary, x = JobSatisfaction), data = survey_df %>% drop_na(Salary),stat="identity") 
  #scale_x_discrete(labels = function(x) strwrap(x, width = 30)) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
View(survey_df)
  
glimpse(survey_df)
summary(survey_df$Salary)

str(survey_df)

sapply(na.omit(survey_df), length)


g <- sapply(survey_df, is.factor)
for (i in names(survey_df)){
  if(is.factor(survey_df[,i])){
      print(ggplot(survey_df, aes(x = survey_df[,i])) +
            geom_bar() +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))) +
            xlab(names)
  }
}

sort(survey_df$Salary, decreasing = TRUE)

cc = is.na(survey_df$DeveloperType)
m = which(cc == c("TRUE"))
survey_df = survey_df[-m,]

salary_job <- survey_df %>%
  filter(DeveloperType != "other") %>%
  group_by(DeveloperType) %>%
  summarise(average_salary = mean(Salary, na.rm = TRUE)) %>%
  arrange(desc(average_salary))

salary_job <- salary_job[1:20,]

summary(survey_df$DeveloperType)
View(salary_job)
salary_job %>%
  mutate(DeveloperType = reorder(DeveloperType, average_salary)) %>%
    ggplot(aes(DeveloperType, average_salary)) +
    geom_bar(stat = "identity") +
    ylab("Average salary (USD)") +
    scale_y_continuous(labels = dollar_format()) +
    coord_flip()


#Salary Range of top 9 countries and Ireland
salary_each_country <- survey_df %>%
  filter(!is.na(Salary)) %>%
    filter(Country %in% Country_Sum$Var1) 

ggplot(salary_each_country, aes(x = Salary)) +
  geom_histogram( color = "white") +
  facet_wrap(~ Country, nrow = 4) 

#Overpaid for top 9 countries and Ireland
overpaid_each_country <- survey_df %>%
  filter(!is.na(Overpaid)) %>%
  filter(Country %in% Country_Sum$Var1) 

ggplot(overpaid_each_country, aes(x = Overpaid)) +
  geom_bar( color = "white") +
  facet_wrap(~ Country, nrow = 4)  +
  aes(stringr::str_wrap(Overpaid, 10)) +
  xlab("OverPaid")
  
  
summary(salary_country$Country)


