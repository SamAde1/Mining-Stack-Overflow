extrafont::loadfonts(device="win")
library(dplyr)
library(knitr)
library(ggplot2)
library(tidyr)
library(readr)
library(plotly)
library(scales)
library(ggthemes)
library(stringr)
options(scipen = 999)

survey_df <- read.csv(file = "survey_results_public.csv", encoding = "UTF-8")

# sapply(survey_df, class)




# Bar Chart ggplot for Country vs count
Country_Sum_data = as.data.frame(table(survey_df$Country))

Country_Sum <- filter(Country_Sum_data, Freq >870 | Var1 == "Ireland")
print(Country_Sum)
co <- ggplot(Country_Sum, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))
gg <- ggplotly(co)
layout(gg, dragmode = "pan")
rangeslider(gg)

# Bar Chart ggplot for Professional vs count
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

#Plotting every Column

#fact <- sapply(survey_df, is.factor)
for (i in 1:ncol(survey_df)){
  if(is.factor(survey_df[,i]) == TRUE){
    print(ggplot(survey_df, aes(x = survey_df[,i])) +
            geom_bar() +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))) +
      xlab(colnames(survey_df)[i])
  }
}


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
salary_job %>%
  mutate(DeveloperType = reorder(DeveloperType, average_salary)) %>%
  ggplot(aes(DeveloperType, average_salary)) +
  geom_bar(stat = "identity") +
  ylab("Average salary (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip()


#Career Satisfaction by Country
Country_Work_Hours <- filter(Country_Sum_data, Freq >400 | Var1 == "Ireland")
Work_hours <- survey_df %>%
  filter(EmploymentStatus == "Employed full-time") %>%
  filter(Country %in% Country_Work_Hours$Var1) %>%
  group_by(Country) %>%
  summarise(Career_Satisfaction = mean(CareerSatisfaction, na.rm = TRUE)) %>%
  arrange(desc(Career_Satisfaction))

Work_hours$Career_Satisfaction <- (sapply(Work_hours$Career_Satisfaction, function(x, subt) x - subt, subt = Work_hours$Career_Satisfaction[13]))

Work_hours$Career_Sat_Flag <- ifelse(Work_hours$Career_Satisfaction < 0, "below", "above")
#Work_hours <- Work_hours[order(Work_hours$Career_Satisfaction)]
work_hours <- arrange(Work_hours, Career_Satisfaction)

ggplot(Work_hours, aes(x=`Country`, y=Career_Satisfaction, label=Career_Satisfaction)) + 
  geom_bar(stat='identity', aes(fill=Career_Sat_Flag))  +
  scale_fill_manual(name="Career Satisfaction", 
                    labels = c("Above Ireland", "Below Ireland"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Career Satisfaction relative to Ireland", 
       title= "Diverging Bars") + 
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


#Years Programming by Gender
brks <- seq(-10000, 10000, 2000)
lbls = paste0(as.character(c(seq(10, 0, -2), seq(2, 10, 2))), "k")

ggplot(mapping =  aes(x = YearsProgram, y = Respondent, fill = Gender),data = survey_df %>% filter(Gender == "Male" | Gender == "Female")) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + # Labels
  coord_flip() +  # Flip axes
  labs(title="Time spent programming by Gender") +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")  # Colour palette


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_set(theme_classic())
g <- ggplot(survey_df %>% drop_na(MajorUndergrad)  %>% filter(Gender == "Male" | Gender == "Female"), aes(MajorUndergrad))
g + geom_bar(aes(fill=Gender), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  aes(stringr::str_wrap(MajorUndergrad, 15)) +
  xlab("Major in Undergrad")


ggplot(survey_df %>% drop_na(MajorUndergrad), aes(x=MajorUndergrad, group=Race,fill=Race)) +
  scale_fill_manual(name = "Race",values=cbPalette(4))+
  geom_bar(colour="black", position="dodge")+
  theme_bw()+
  theme(legend.key = element_rect(colour = "black")) +
  guides(fill = guide_legend(override.aes = list(colour = NULL)))
