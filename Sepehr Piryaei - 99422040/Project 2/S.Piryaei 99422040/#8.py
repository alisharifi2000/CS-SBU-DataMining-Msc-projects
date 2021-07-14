import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.svm import SVC
from sklearn.metrics import f1_score, precision_score, recall_score

df = pd.read_csv('heart.csv')

x = df[['trestbps', 'chol', 'thalach']]
y = df['target']

X_train, X_test, y_train, y_test = train_test_split(x, y, test_size=0.2)

classifier = SVC(kernel='linear')
classifier.fit(X_train, y_train)

y_pred = classifier.predict(X_test)

print(f1_score(y_test, y_pred, average="macro"))
print(recall_score(y_test, y_pred, average="macro"))
print(precision_score(y_test, y_pred, average="macro"))
