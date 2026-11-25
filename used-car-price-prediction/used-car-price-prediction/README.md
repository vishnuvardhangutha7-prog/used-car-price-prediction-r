# Used Car Price Prediction (R Programming Project)

An end-to-end Machine Learning & Statistical Modeling project in **R** to predict used car resale prices (in INR Lakhs) based on vehicle specifications, age, mileage, fuel type, transmission, and ownership history.

## Project Structure

```text
used-car-price-prediction/
├── README.md
├── data/
│   └── used_cars.csv                         # 500-row dataset of used car listings
├── scripts/
│   ├── 01_generate_dataset.R                 # Reproducible dataset generator (seed = 42)
│   └── 02_used_car_price_prediction.R        # Main EDA, preprocessing, modeling & evaluation script
├── outputs/
│   ├── model_comparison.csv                  # RMSE, MAE, R2, MAPE across all 4 models
│   ├── test_predictions.csv                  # Actual vs predicted prices on test split
│   └── plots/                                # 7 generated EDA & diagnostic PNG charts
└── report/
    ├── Used_Car_Price_Prediction_Report.Rmd  # R Markdown report source
    ├── Used_Car_Price_Prediction_Report.md   # Markdown project report
    └── Used_Car_Price_Prediction_Report.html # Self-contained HTML report (ready for browser / Print-to-PDF)
```

## How to Run Locally

Uses only base and recommended R packages (`stats`, `MASS`, `rpart`, `graphics`, `grDevices`, `utils`) — no external internet downloads needed.

```powershell
# From the project root folder:
Rscript scripts/01_generate_dataset.R
Rscript scripts/02_used_car_price_prediction.R
```

## How to Submit on Linways Portal

Linways typically asks for **two links** (a Code Link and a Report Link):

1. **Code Link**:
   - Upload this entire `used-car-price-prediction` folder to a **public GitHub repository** (or Google Drive folder with *"Anyone with the link can view"*), and paste that URL into the **Code Link** field.
2. **Report Link**:
   - Open `report/Used_Car_Price_Prediction_Report.html` in Chrome/Edge, press **Ctrl + P → Save as PDF** to create `Used_Car_Price_Prediction_Report.pdf` (or upload the `.md`/`.html`/`.pdf` directly to **Google Drive** or **RPubs**), set sharing to *"Anyone with the link can view"*, and paste that URL into the **Report Link** field.
