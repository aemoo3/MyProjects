import pandas as pd
import numpy as np
import time
from nba_api.stats.endpoints import playergamelog 
from nba_api.stats.static import players
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import (
    accuracy_score, confusion_matrix, classification_report, roc_auc_score, RocCurveDisplay
)
import matplotlib.pyplot as plt
import seaborn as sns
import joblib

# -------------------------------------------
# Step 1: Load player game logs using nba_api
# -------------------------------------------

player_list = players.get_active_players()
selected_players = player_list[:10]  # Sample first 10 active players

all_logs = []

for player in selected_players:
    try:
        gamelog = playergamelog.PlayerGameLog(player_id=player['id'], season='2025')
        df = gamelog.get_data_frames()[0]
        df['PLAYER_NAME'] = player['full_name']
        all_logs.append(df)
        time.sleep(0.6)
    except Exception as e:
        print(f"Error fetching {player['full_name']}: {e}")

df = pd.concat(all_logs)
df['GAME_DATE'] = pd.to_datetime(df['GAME_DATE'])

# ---------------------------------------------------
# Step 2: Feature selection and projection simulation
# ---------------------------------------------------

df = df[['PLAYER_NAME', 'GAME_DATE', 'MATCHUP', 'MIN', 'PTS', 'REB', 'AST', 'TOV', 'PLUS_MINUS']]
df.sort_values(by=['PLAYER_NAME', 'GAME_DATE'], inplace=True)

df['PTS_PROJECTION'] = df.groupby('PLAYER_NAME')['PTS'].transform(lambda x: x.shift(1).rolling(3).mean())
df.dropna(inplace=True)

df['OVER_LINE'] = (df['PTS'] > df['PTS_PROJECTION']).astype(int)

print(df.head())

# -------------------------------------
# Step 3: Prepare features and modeling
# -------------------------------------

features = ['MIN', 'REB', 'AST', 'TOV', 'PLUS_MINUS']
X = df[features]
y = df['OVER_LINE']

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, stratify=y, random_state=42
)

# Define models to compare
models = {
    "Logistic Regression": LogisticRegression(max_iter=1000),
    "Random Forest": RandomForestClassifier(n_estimators=100, random_state=42),
    "Gradient Boosting": GradientBoostingClassifier(n_estimators=100, random_state=42),
    "K-Nearest Neighbors": KNeighborsClassifier(n_neighbors=5),
}

results = {}

for name, model in models.items():
    print(f"\nTraining and evaluating {name}...")
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    y_proba = model.predict_proba(X_test)[:, 1]

    acc = accuracy_score(y_test, y_pred)
    roc_auc = roc_auc_score(y_test, y_proba)

    print(f"Accuracy: {acc:.4f}")
    print(f"ROC AUC: {roc_auc:.4f}")
    print("Classification Report:")
    print(classification_report(y_test, y_pred))

    results[name] = {
        "model": model,
        "accuracy": acc,
        "roc_auc": roc_auc,
        "y_pred": y_pred,
        "y_proba": y_proba,
    }

# ------------------------
# Step 4: Compare ROC Curves
# ------------------------

plt.figure(figsize=(10, 8))
for name, res in results.items():
    RocCurveDisplay.from_predictions(y_test, res['y_proba'], name=name)
plt.title("ROC Curves for Different Models")
plt.legend()
plt.show()

# ---------------------------------
# Step 5: Save best model and dataset 
# ---------------------------------

best_model_name = max(results, key=lambda k: results[k]['roc_auc'])
best_model = results[best_model_name]['model']

print(f"\n💾 Saving best model: {best_model_name} with ROC AUC: {results[best_model_name]['roc_auc']:.4f}")
joblib.dump(best_model, "best_points_over_model.pkl")
df.to_csv("player_game_logs.csv", index=False)
