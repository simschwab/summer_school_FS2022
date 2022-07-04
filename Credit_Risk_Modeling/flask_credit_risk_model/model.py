import pandas as pd

# Importing the data
df_credit = pd.read_csv("german_credit_data.csv", index_col=0)
df_credit

# Remove missing values
df_credit.dropna()

# Creating the X and y
X = df_credit[["Age", "Job", "Credit amount", "Duration"]].values
y = df_credit["Risk"].values

# Label encoder
from sklearn.preprocessing import LabelEncoder
encoder = LabelEncoder()
y = encoder.fit_transform(y)

import joblib

joblib.dump(encoder, "saved_models/encoder.pkl")

# split test train
from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state=42)

# train model
# from sklearn.neighbors import KNeighborsClassifier
from xgboost import XGBClassifier

# classifier = KNeighborsClassifier(n_neighbors=5, metric='minkowski', p=2)
classifier = XGBClassifier(learning_rate=0.05, 
                    n_estimators=200, 
                    objective='binary:logistic',
                    eval_metric = "logloss",
                    nthread=2, 
                    random_state=2
                   )
classifier.fit(X_train, y_train)

# Test model
y_pred = classifier.predict(X_test)
from sklearn.metrics import accuracy_score

# Confusion matrix and classification report
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report, fbeta_score
print("Confusion matrix: \n", confusion_matrix(y_test, y_pred))
print("\n")
print(classification_report(y_test, y_pred))

# Save Model
import joblib
joblib.dump(classifier, "saved_models/model.pkl")

# Make predictions
# Read models
classifier_loaded = joblib.load("saved_models/model.pkl")
encoder_loaded = joblib.load("saved_models/encoder.pkl")

# Prediction set
X_manual_test = [[24, 3, 20000, 36]]
print("Test prediction with parameters (Age, Job, Credit amount, Duration): ", X_manual_test)

prediction = encoder_loaded.inverse_transform(classifier.predict(X_manual_test))
print("Predicted credit risk:", prediction)
