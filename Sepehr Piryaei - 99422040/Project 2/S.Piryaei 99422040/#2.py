import pandas as pd
from sklearn.model_selection import train_test_split

dataset = pd.read_csv("heart.csv")
X = dataset.iloc[:, 0:11].values
y = dataset.iloc[:, 12].values

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)
print(len(y_train))
print(len(y_test))
