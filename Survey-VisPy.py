# -*- coding: utf-8 -*-
"""
Created on Wed Jun 27 07:53:45 2018

@author: Admin
"""
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

df = pd.read_csv(r"C:\Users\Admin\Desktop\University\Computer Science\SPUR\Mining Stack Overflow\survey_results_public.csv", na_values = ['na','NA'], encoding = "utf8" )

Country_Sum_Data = df.Country.value_counts()
Country_Sum_Data = Country_Sum_Data[Country_Sum_Data > 870] 
fig, ax = plt.subplots()
Country_Sum_Data.plot(ax=ax, kind = 'bar')  

df.Professional.value_counts().plot(ax=ax, kind = 'bar')

"""for col in df.columns[1:]:
    fig, ax = plt.subplots()
    df[col].value_counts().plot(ax=ax, kind = 'bar')
"""

threshold = 500 # Anything that occurs less than this will be removed.
df.count()
value_counts = df.Race.value_counts() # 
to_remove = value_counts[value_counts <= threshold].index.values
df.Race.loc[df.Race.isin(to_remove)] = None

df.plot(x="MajorUndergrad", y = df.Race.value_counts(), kind = "bar")

