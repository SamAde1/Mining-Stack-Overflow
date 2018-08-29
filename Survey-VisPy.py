# -*- coding: utf-8 -*-
"""
Created on Wed Jun 27 07:53:45 2018

@author: Admin
"""
import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

df = pd.read_csv(r"C:\Users\Admin\Desktop\University\Computer Science\SPUR\Mining Stack Overflow\survey_results_public.csv", na_values = ['na','NA'], encoding = "utf8" )

Country_Sum = df.Country.value_counts()
Country_Sum = pd.DataFrame({'Country':Country_Sum.index, 'Count':Country_Sum.values })
Country_Sum_Data = Country_Sum.Count > 870 
Country_Sum_Data.loc[Country_Sum_Data.Sentence.isin(Country_Sum),'Type']='Bodypart'
Country_Sum_Data = Country_Sum_Data.append(Country_Sum.Country = 'Israel')

fig, ax = plt.subplots()
#Country_Sums_Data.plot(ax=ax, kind = 'bar') 

df.Currency.value_counts().plot(ax=ax, kind = 'bar')
#plt.show()
fig.savefig('foo.png', bbox_inches='tight')

df_new = df[df['ImportantBenefits'].notnull()]#

df2 = pd.melt(pd.concat([df_new['Country'],
                         df_new.loc[:, "ImportantBenefits"].apply(
                                 lambda s: s.strip().split(';')).apply(pd.Series)], axis = 1),
    id_vars='Country')[['Country','value']]
df2 = df2[df2['value'].notnull()]
df2['value'] = df2['value'].map(lambda x: x.strip())
df2 = df2.loc[df2['Country'].isin(Country_Sums_Data.Country)]

    
important_benefits_plot = df2.value.value_counts()
fig, ax = plt.subplots()
important_benefits_plot.plot(ax=ax, kind = 'bar') 


important_benefits = df2.groupby('Country')['value'].value_counts().rename(columns={'Country':'Countries','value':'values'}).reset_index()
important_benefits.columns = ['Country', 'ImportantBenefits','count']
important_benefits[['ImportantBenefits', 'Country', 'count']]
Country_Sums_Data = pd.DataFrame({'Country':Country_Sums_Data.index, 'count':Country_Sums_Data.values})

important_benefits = important_benefits.loc[important_benefits['Country'].isin(Country_Sums_Data.Country)]