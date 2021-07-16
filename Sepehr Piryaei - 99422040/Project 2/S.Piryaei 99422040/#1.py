import pandas as pd

dataset = pd.read_csv("heart.csv")

dataset = dataset.dropna()
