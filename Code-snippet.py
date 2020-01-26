# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn import metrics


dataset= pd.read_csv(r'C:/Users/iGuest/Desktop/RProjects/data.csv')

print(dataset.keys())


new =pd.DataFrame(data=dataset)



new['Predicted'] = new.ExtractedChlorophylla

X = pd.DataFrame(np.c_[new['ParticulateOrganicNitrogen'], new['ParticulateOrganicCarbon'],new['ParticulateMicrocystin'],
                       new['Turbidity'], new['DissolvedMicrocystin'],new['ExtractedPhycocyanin'],
                       new['TotalPhosphorus'], new['DissolvedOrganicCarbon'],new['ColoredDissolvedOrganicMaterialAbsorbance'],
                       new['VolatileSuspendedSolids'], new['CTDTemperature']],
                 columns=['ParticulateOrganicNitrogen','ParticulateOrganicCarbon','ParticulateMicrocystin',
                          'Turbidity','DissolvedMicrocystin','ExtractedPhycocyanin',
                          'TotalPhosphorus','DissolvedOrganicCarbon','ColoredDissolvedOrganicMaterialAbsorbance',
                          'VolatileSuspendedSolids','CTDTemperature'])
Y = new['Predicted']

X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size = 0.2, random_state=9)


lin_reg_mod = LinearRegression()


lin_reg_mod.fit(X_train, y_train)

pred = lin_reg_mod.predict(X_test)

test_set_rmse = (np.sqrt(metrics.mean_squared_error(y_test, pred)))

test_set_r2 = metrics.r2_score(y_test, pred)

print(test_set_rmse)
print(test_set_r2)