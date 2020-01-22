import numpy as np
import pandas as pd
pd.set_option('display.max_columns', 60)
import matplotlib.pylab as plt
from pandas import Series, DataFrame

## Importing MDF.csv ---------------------------------------------------------------------------------------
address = 'C:/Users/zhizh/OneDrive - Stop TB Partnership/UNOPS/10 Paper Writing/CAR software/02 Bangladesh/CAR-Bangladesh/CAR_BGD/MDF.csv'

# Making a list of missing value types
missing_values = ["n/a", "na", "--", "N/A", "-"]
BGD = pd.read_csv(address, na_values= missing_values)
BGD = BGD.drop(['Given', 'FamilyName', 'Patient.BD', 'Filename', 'Radiology.Result.Date'], axis=1)
print(BGD.info())

## checking missing symptom values ---------------------------------------------------------------------------------------
print (BGD['Cough'].isnull())
print(BGD.isnull().sum())
# Not much symptom variable with NA value, it is fair to assume that the NA were missed entry. Based on this assumption, we will replace all the NA with 'No"
BGD['Cough'].fillna('No', inplace=True)
BGD['Fever'].fillna('No', inplace=True)
BGD['Active Breathing Shortness'].fillna('No', inplace=True)
BGD['Weight Loss'].fillna('No', inplace=True)
BGD['Haemoptysis'].fillna('No', inplace=True)

## Adding Xpert + by symptoms --------------------------------------------------------------------------------------------
BGD.groupby('Symptoms').aggregate(sum)
