# -*- coding: utf-8 -*-
"""
Created on Sun May 30 01:02:33 2021

@author: GITEX
"""
# question 2
import pandas as pd
import numpy as np
dataset = pd.read_csv(r'C:\Users\GITEX\Desktop\train.csv')
x= dataset.iloc[:, :-1].values
y= dataset.iloc[:, 20].values
from sklearn.preprocessing import StandardScaler
scale = StandardScaler()
x = scale.fit_transform(x)
from sklearn.model_selection import train_test_split
x_train, x_test, y_train, y_test = train_test_split(x, y
                                                    , test_size=0.30)
from sklearn.svm import SVC   
clf = SVC(kernel='sigmoid')
clf.fit(x_test, y_test) 
y_pred = clf.predict(x_test)
from sklearn import metrics
print("Accuracy:",metrics.accuracy_score(y_test, y_pred))

#question 5
a = dataset.battery_power
a = list(a)
a.sort()
new = []
first = a[:666]
sec = a[666 : 1333]
third = a[1333:]
print('mean of first bin is:', np.mean(first))
print('mean of second bin is:', np.mean(sec))
print('mean of third bin is:', np.mean(third))
for i in range(1, 666):
    new.append(np.mean(first))
for i in range(666, 1333):
    new.append(np.mean(sec))
for i in range(1333,2001):
    new.append(np.mean(third))
dataset.drop('battery_power', axis = 1, inplace = True)
dataset['binning_battery']= new


#question 6
s = dataset.px_height
p = dataset.px_width
masahat = np.multiply(s, p)
dataset['masahat'] = masahat
price = dataset.price_range
dataset.drop('price_range', axis = 1, inplace = True)
dataset['price']= price


#question 7
x= dataset.iloc[:, :-1].values
y= dataset.iloc[:, 21].values
from sklearn.preprocessing import StandardScaler
scale = StandardScaler()
x = scale.fit_transform(x)
from sklearn.model_selection import train_test_split
x_train, x_test, y_train, y_test = train_test_split(x, y
                                                    , test_size=0.30)
from sklearn.svm import SVC   
clf = SVC(kernel='sigmoid')
clf.fit(x_test, y_test) 
y_pred = clf.predict(x_test)
from sklearn import metrics
print("Accuracy:",metrics.accuracy_score(y_test, y_pred))