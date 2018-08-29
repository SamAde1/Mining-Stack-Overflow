library(dplyr)
library(knitr)
library(ggplot2)
library(tidyr)
library(readr)
library(plotly)
library(scales)
library(ggthemes)
library(tidyverse)
library(maps)
library(rworldmap)
library(ggfittext)
library(treemapify)
library(treemap)
library(stringr)
options(scipen = 999)



survey_df <- read.csv(file = "survey_results_public.csv", encoding = "UTF-8")

# sapply(survey_df, class)

df = (filter(survey_df%>%drop_na(Salary), MajorUndergrad == "Psychology" & Salary <= 30000))
 # Bar Chart ggplot for Country vs count
Country_Sum_data = as.data.frame(table(survey_df$Country))
write.csv(Country_Sum_data,'Country_Sum_data.csv')

Country_Sum <- filter(Country_Sum_data, Freq >870 | Var1 == "Ireland" | Var1 == "Israel")

names(Country_Sum_data)[names(Country_Sum_data) == 'Freq'] <- 'Number of Respondents Per Country'

ggplot(Country_Sum, aes(x = reorder(Var1,Var1,function(x)-length(x)),y = Freq)) + 
  geom_bar(stat = "identity") 

Countries <- joinCountryData2Map(Country_Sum_data, joinCode = "NAME", nameJoinColumn = "Var1")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
#mapCountryData( Countries, nameColumnToPlot="Freq" )
mapParams <- mapCountryData( Countries, nameColumnToPlot="Number of Respondents Per Country", addLegend=FALSE,numCats = 6 )
do.call( addMapLegend, c(mapParams, legendWidth=1,legendIntervals="page", legendMar = 2))

ggplot(Country_Sum, aes(x = reorder(Var1, -Freq), y = Freq)) +
   geom_bar(stat = "identity") +
   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))





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
  geom_boxplot(aes(y = Salary, x = FormalEducation), data = survey_df %>% drop_na(Salary)) +
scale_x_discrete(labels = function(x) strwrap(x, width = 30)) +
theme(axis.title.x = element_text(margin(t = 10, r = 10, b = 10, l = 10)))

ggplot() + 
  geom_bar(aes(y = Salary, x = JobSatisfaction), data = survey_df %>% drop_na(Salary),stat="identity") 
#scale_x_discrete(labels = function(x) strwrap(x, width = 30)) +
#theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Plotting every Column

#fact <- sapply(survey_df, is.factor)
# for (i in 1:ncol(survey_df)){
#   if(is.factor(survey_df[,i]) == TRUE){
#     print(ggplot(survey_df, aes(x = survey_df[,i])) +
#             drop_na(survey_df[,i]) +
#             geom_bar() +
#             theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))) +
#       xlab(colnames(survey_df)[i])
#   }
# }




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

survey_df$JobSatisfaction <- factor(survey_df$JobSatisfaction)
salary_Satisfaction <- survey_df %>% 
  drop_na(JobSatisfaction) %>%
  group_by(JobSatisfaction) %>%
  summarise(average_salary = mean(Salary, na.rm = TRUE))
salary_Satisfaction %>%
  ggplot(aes(JobSatisfaction, average_salary)) +
  geom_bar(stat = "identity") +
  ylab("Average Salary (USD)") +
  scale_y_continuous(labels = dollar_format())
  


#Career Satisfaction by Country
names(Country_Sum_data)[names(Country_Sum_data) == 'Number of Respondents Per Country'] <- 'Freq'

Country_Work_Satisfaction <- filter(Country_Sum_data, Freq >400 | Var1 == "Ireland")
Work_Satisfaction <- survey_df %>%
  filter(EmploymentStatus == "Employed full-time") %>%
  filter(Country %in% Country_Work_Satisfaction$Var1) %>%
  group_by(Country) %>%
  summarise(Career_Satisfaction = mean(CareerSatisfaction, na.rm = TRUE)) %>%
  arrange(desc(Career_Satisfaction))




  Work_Satisfaction$Career_Satisfaction <- (sapply(Work_Satisfaction$Career_Satisfaction, function(x, subt) x - subt, subt = Work_Satisfaction$Career_Satisfaction[which("Ireland" == Work_Satisfaction$Country)]))
is.numeric(Work_Satisfaction$Career_Satisfaction)
Work_Satisfaction$Career_Sat_Flag <- ifelse(Work_Satisfaction$Career_Satisfaction < 0, "below", "above")
#Work_Satisfaction <- Work_Satisfaction[order(Work_Satisfaction$Career_Satisfaction)]

Work_Satisfaction <- arrange(Work_Satisfaction, Career_Satisfaction) 
Work_Satisfaction$Country <- factor(Work_Satisfaction$Country, levels = Work_Satisfaction$Country)


ggplot(Work_Satisfaction, aes(x=`Country`, y=Career_Satisfaction, label=Career_Satisfaction)) + 
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

overpaid_each_country[] <- lapply(overpaid_each_country$Overpaid, function(x) {
  levels(x)[levels(x) %in% c("Greatly overpaid")] <- "By a lot"
  levels(x)[levels(x) %in% c("either underpaid nor overpaid")] <- "No"
  levels(x)[levels(x) %in% c("somewhat overpaid")] <- "Yes I am"
  x
})


 ggplot(overpaid_each_country, aes(x = Overpaid, y = ..prop.., group = 1)) +
  geom_bar( color = "white", fill = "#42dff4") +
  facet_wrap(~ Country, nrow = 4)  +
  aes(stringr::str_wrap(Overpaid, 10)) +
  xlab("OverPaid") +
  ylab("Proportion")


#Years Programming by Gender
brks <- seq(-10000, 10000, 2000)
lbls = paste0(as.character(c(seq(10, 0, -2), seq(2, 10, 2))), "k")

ggplot(mapping =  aes(x = reorder(YearsProgram,YearsProgram,function(x)-length(x)), fill = Gender),data = survey_df %>% filter(Gender == "Male" | Gender == "Female")%>% drop_na(YearsProgram)) +   # Fill column
  geom_bar( width = .6) +   # draw the bars
  labs(title="Time spent programming by Gender") +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2") +  # Colour 
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  xlab("Years spent programming") +
  ylab("Number of Respondents")


theme_set(theme_classic())
ggplot(survey_df %>% drop_na(MajorUndergrad)  %>% filter(Gender == "Male" | Gender == "Female"), aes(x = University, group=interaction(Gender, ProgramHobby), fill=ProgramHobby))+ 
  geom_bar(aes(y = ..prop..), stat = "count", position=position_dodge()) + 
  geom_text(aes(label=scales::percent(round(..prop..,2)), y=..prop..), stat="count", vjust=-.5, position=position_dodge(.9)) +
  scale_y_continuous(limits=c(0,1),labels = scales::percent) +
  # theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  # aes(stringr::str_wrap(MajorUndergrad, 15)) +
  xlab("Major in Undergrad") +
  ylab("Proportion of People") +
  facet_grid(~Gender)
  
  ggplot(survey_df %>% drop_na(MajorUndergrad)  %>% filter(Gender == "Male" | Gender == "Female"), aes(reorder(MajorUndergrad,MajorUndergrad,function(x)-length(x)))) + 
    geom_bar(aes(fill=Gender), width = 0.5) + 
  +  theme(axis.text.x = element_text(angle=90, vjust=0.6))+
  aes(stringr::str_wrap(MajorUndergrad, 15)) +
  xlab("Major in Undergrad")


Undergrad_Race <- survey_df %>% 
  drop_na(MajorUndergrad) %>% 
  drop_na(Race) %>% 
  filter(Race != "I prefer not to say") %>%
  filter(Race != "I don’t know") 
  

 freq_race <- as.data.frame(table(Undergrad_Race$Race)) %>% filter(Freq < 150)
for(i in 1:length(freq_race$Var1)){
    l = as.character(freq_race$Var1[i]) 
    Undergrad_Race <- Undergrad_Race %>%
      filter(Race != l)
  }

  summary(Undergrad_Race$Race)
  


ggplot(Undergrad_Race %>% drop_na(ProgramHobby) %>% drop_na(Race), aes(x=ProgramHobby, group=Race,fill=Race)) +
  geom_bar(colour="black", position="dodge")+
  theme_bw()+
  theme(legend.key = element_rect(colour = "black")) +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  scale_x_discrete(labels = function(y) strwrap(y, width = 50)) 


fit <- lm(Salary ~ Race + Country + MajorUndergrad, data = survey_df)
summary(fit)


Job_Salary <- survey_df %>% drop_na(DeveloperType) %>% drop_na(Salary)
jobs <- strsplit(as.character(Job_Salary$DeveloperType), ';')
Job_Salary <- data.frame(DeveloperType = trimws(unlist(jobs)), Salary = rep(Job_Salary$Salary, sapply(jobs, FUN=length)))
jobs2 = as.data.frame(table(data.frame(DeveloperType = trimws(unlist(jobs)))))



salary_job <- Job_Salary %>%
  filter(DeveloperType != "Other") %>%
  group_by(DeveloperType) %>%
  summarise(average_salary = mean(Salary, na.rm = TRUE)) %>%
  arrange(desc(average_salary))


  

salary_job %>%
  mutate(DeveloperType = reorder(DeveloperType, average_salary)) %>%
  ggplot(aes(DeveloperType, average_salary)) +
  geom_bar(stat = "identity") +
  ylab("Average salary (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip()




ggplot(Job_Salary, aes(x = reorder(DeveloperType,DeveloperType,function(x)+length(x)))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#42dff4") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(title = "Developer Type") +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+
  coord_flip()



Lang_Salary <- survey_df %>% drop_na(HaveWorkedLanguage) %>% drop_na(Salary)
Lang <- strsplit(as.character(Lang_Salary$HaveWorkedLanguage), ';')
Lang_Salary <- data.frame(HaveWorkedLanguage = trimws(unlist(Lang)), Salary = rep(Lang_Salary$Salary, sapply(Lang, FUN=length)))
Lang2 = as.data.frame(table(data.frame(HaveWorkedLanguage = trimws(unlist(Lang)))))

ggplot(Lang_Salary, aes(x = reorder(HaveWorkedLanguage,HaveWorkedLanguage,function(x)+length(x)))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#42dff4") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(title = "Programming Language currently working in") +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+
  coord_flip()


Want_Lang_Salary <- survey_df %>% drop_na(WantWorkLanguage) %>% drop_na(Salary)
Want_Lang <- strsplit(as.character(Want_Lang_Salary$WantWorkLanguage), ';')
Want_Lang_Salary <- data.frame(WantWorkLanguage = trimws(unlist(Want_Lang)), Salary = rep(Want_Lang_Salary$Salary, sapply(Want_Lang, FUN=length)))
Want_Lang2 = as.data.frame(table(data.frame(WantWorkLanguage = trimws(unlist(Want_Lang)))))

ggplot(Want_Lang_Salary, aes(x = reorder(WantWorkLanguage,WantWorkLanguage,function(x)+length(x)))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#42dff4") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 0.4) +
  scale_y_continuous(labels = percent) +
  labs(title = "Programming Language respondents want to work in") +
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())+
  coord_flip()


salary_lang <- Lang_Salary %>%
  filter(HaveWorkedLanguage != "Other") %>%
  group_by(HaveWorkedLanguage) %>%
  summarise(average_salary = mean(Salary, na.rm = TRUE)) %>%
  arrange(desc(average_salary))

salary_lang %>%
  mutate(HaveWorkedLanguage = reorder(HaveWorkedLanguage, average_salary)) %>%
  ggplot(aes(HaveWorkedLanguage, average_salary)) +
  geom_bar(stat = "identity", fill = "#42dff4") +
  ylab("Average salary (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip()

freq_lang <- as.data.frame(table(Lang_Salary$HaveWorkedLanguage))


country_salary_lang <- survey_df %>%
  filter(!is.na(HaveWorkedLanguage)) %>%
  drop_na(Salary) %>%
  filter(Country %in% Country_Sum$Var1) 
country_lang <- strsplit(as.character(country_salary_lang$HaveWorkedLanguage), ';')
country_salary_lang <- data.frame(HaveWorkedLanguage = trimws(unlist(country_lang)), Salary = rep(country_salary_lang$Salary , sapply(country_lang, FUN=length)), Country = rep(country_salary_lang$Country, sapply(country_lang, FUN=length)))

freq_country_lang <- as.data.frame(table(country_salary_lang$HaveWorkedLanguage))

freq_country_lang <- filter(freq_country_lang, Freq <1000)

for(i in 1:length(freq_country_lang$Var1)){
  t = as.character(freq_country_lang$Var1[i]) 
  country_salary_lang <- country_salary_lang %>%
    filter(HaveWorkedLanguage != t)
}

country_lang_salary <- country_salary_lang %>%
  filter(HaveWorkedLanguage != "Other") %>%
  group_by(HaveWorkedLanguage, Country) %>%
  summarise(average_salary = mean(Salary, na.rm = TRUE)) %>%
  arrange(desc(average_salary))

country_lang_salary %>%
  ggplot(aes(x = reorder(Country, -average_salary), y = average_salary)) +
  geom_bar(stat = "identity", fill = "#42dff4") +
  ylab("Average salary (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip() +
  facet_wrap(~ HaveWorkedLanguage, nrow = 4)  +
  theme_bw()+
  theme(strip.background =element_rect(fill="red"))+
  theme(strip.text = element_text(colour = 'black'))+
  xlab("Countries")





gender_lang <- survey_df %>%
  filter(!is.na(HaveWorkedLanguage)) %>%
  filter(Gender == "Male" | Gender == "Female") 
lang_gender <- strsplit(as.character(gender_lang$HaveWorkedLanguage), ';')

gender_lang <- data.frame(HaveWorkedLanguage = trimws(unlist(lang_gender)), Gender = rep(gender_lang$Country, sapply(lang_gender, FUN=length)))

freq_lang_gender <- as.data.frame(table(gender_lang$HaveWorkedLanguage))

freq_lang_gender <- filter(freq_lang_gender, Freq <200)

for(i in 1:length(freq_lang_gender$Var1)){
  t = as.character(freq_lang_gender$Var1[i]) 
  gender_lang <- gender_lang %>%
    filter(HaveWorkedLanguage != t)
}

lang_gender_salary <- gender_lang %>%
  filter(HaveWorkedLanguage != "Other") %>%
  group_by(HaveWorkedLanguage, Country) %>%
  summarise(average_salary = mean(Salary, na.rm = TRUE)) %>%
  arrange(desc(average_salary))

lang_gender_salary %>%
  ggplot(aes(x = reorder(Country, -average_salary), y = average_salary)) +
  geom_bar(stat = "identity", fill = "#42dff4") +
  ylab("Average salary (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip() +
  facet_wrap(~ HaveWorkedLanguage, nrow = 4)  +
  theme_bw()+
  theme(strip.background =element_rect(fill="red"))+
  theme(strip.text = element_text(colour = 'black'))+
  xlab("Countries")


salary_job <- Job_Salary %>%
  filter(DeveloperType != "Other") %>%
  group_by(DeveloperType) %>%
  summarise(average_salary = mean(Salary, na.rm = TRUE)) %>%
  arrange(desc(average_salary))




salary_job %>%
  mutate(DeveloperType = reorder(DeveloperType, average_salary)) %>%
  ggplot(aes(DeveloperType, average_salary)) +
  geom_bar(stat = "identity") +
  ylab("Average salary (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip()

ggplot(survey_df %>% drop_na(TabsSpaces), aes(x = TabsSpaces)) +
  geom_bar()


EducationType_Gender <- survey_df %>%
  filter(!is.na(EducationTypes)) %>%
  filter(Gender == "Male" | Gender == "Female") 


EducGender <- strsplit(as.character(EducationType_Gender$EducationTypes), ';')
Educate_Gender <- data.frame(EducationTypes = trimws(unlist(EducGender)), Gender = rep(EducationType_Gender$Gender, sapply(EducGender, FUN=length))) 

ggplot(Educate_Gender, aes(x = EducationTypes, group=interaction(Gender)))+ 
  geom_bar(aes(y = ..prop..), stat = "count", position=position_dodge(), fill = "#42dff4") + 
  geom_text(aes(label=scales::percent(round(..prop..,2)), y=..prop..), stat="count", vjust=-.5, position=position_dodge(.9)) +
  scale_y_continuous(limits=c(0,1),labels = scales::percent) +
  theme(axis.text.x = element_text(angle=90))+
  # aes(stringr::str_wrap(MajorUndergrad, 15)) +
  xlab("Types of non-formal education done") +
  ylab("Proportion of People") +
  facet_grid(~Gender) +
  theme(axis.text.y=element_blank(), axis.ticks=element_blank())


Lang_DeveloperType <- survey_df %>% drop_na(HaveWorkedLanguage) %>% drop_na(DeveloperType)
LangCount <- strsplit(as.character(Lang_DeveloperType$HaveWorkedLanguage), ';')
Lang_DeveloperType <- data.frame(HaveWorkedLanguage = trimws(unlist(LangCount)), DeveloperType = rep(Lang_DeveloperType$DeveloperType, sapply(LangCount, FUN=length)))

LangDevCount <- strsplit(as.character(Lang_DeveloperType$DeveloperType), ';')
Lang_DeveloperType <- data.frame(DeveloperType = trimws(unlist(LangDevCount)), HaveWorkedLanguage = rep(Lang_DeveloperType$HaveWorkedLanguage, sapply(LangDevCount, FUN=length)))
Lang_Merge_Count = as.data.frame(table(Lang_DeveloperType$HaveWorkedLanguage))
DeveloperType_Lang <- as.data.frame((with(Lang_DeveloperType, table(HaveWorkedLanguage, DeveloperType))))
names(Lang_Merge_Count)[names(Lang_Merge_Count) == 'Var1'] <- 'HaveWorkedLanguage'
names(Lang_Merge_Count)[names(Lang_Merge_Count) == 'Freq'] <- 'Count'
Lang_DeveloperType_Count <- merge(DeveloperType_Lang, Lang_Merge_Count,  sort = FALSE)


Lang_Merge_Count <- filter(Lang_Merge_Count, Count <10000)

for(i in 1:length(Lang_Merge_Count$HaveWorkedLanguage)){
  t = as.character(Lang_Merge_Count$HaveWorkedLanguage[i]) 
  Lang_DeveloperType_Count <- Lang_DeveloperType_Count %>%
    filter(HaveWorkedLanguage != t)
}


ggplot(Lang_DeveloperType_Count, aes(area = Freq, fill = HaveWorkedLanguage, label = DeveloperType, subgroup = HaveWorkedLanguage)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)



