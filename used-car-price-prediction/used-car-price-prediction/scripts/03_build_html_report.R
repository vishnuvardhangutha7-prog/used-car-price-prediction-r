# ==============================================================================
# Project: Used Car Price Prediction
# File: scripts/03_build_html_report.R
# Description: Builds a self-contained, print-ready HTML report with base64-
#              embedded PNG charts so the report renders cleanly anywhere.
# ==============================================================================

base64_encode_raw <- function(raw_vec) {
  chars <- c(LETTERS, letters, 0:9, "+", "/")
  n <- length(raw_vec)
  if (n == 0L) return("")
  ints <- as.integer(raw_vec)
  pad <- (3L - (n %% 3L)) %% 3L
  if (pad > 0L) ints <- c(ints, rep(0L, pad))
  m <- matrix(ints, nrow = 3L)
  b1 <- bitwShiftR(m[1L, ], 2L)
  b2 <- bitwOr(bitwShiftL(bitwAnd(m[1L, ], 3L), 4L), bitwShiftR(m[2L, ], 4L))
  b3 <- bitwOr(bitwShiftL(bitwAnd(m[2L, ], 15L), 2L), bitwShiftR(m[3L, ], 6L))
  b4 <- bitwAnd(m[3L, ], 63L)
  out <- chars[as.integer(rbind(b1, b2, b3, b4)) + 1L]
  if (pad > 0L) out[(length(out) - pad + 1L):length(out)] <- "="
  paste(out, collapse = "")
}

encode_img <- function(path) {
  raw_bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  b64 <- base64_encode_raw(raw_bytes)
  paste0("data:image/png;base64,", b64)
}

logo <- encode_img("report/presidency_logo.png")
img1 <- encode_img("outputs/plots/01_price_distribution.png")
img2 <- encode_img("outputs/plots/02_price_by_fuel_transmission.png")
img3 <- encode_img("outputs/plots/03_year_km_vs_price.png")
img4 <- encode_img("outputs/plots/04_correlation_matrix.png")
img5 <- encode_img("outputs/plots/05_actual_vs_predicted.png")
img6 <- encode_img("outputs/plots/06_residual_diagnostics.png")
img7 <- encode_img("outputs/plots/07_decision_tree.png")

html_template <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Used Car Price Prediction - R Programming Project Report</title>
<style>
  :root {
    --primary: #1a365d;
    --accent: #2b6cb0;
    --bg: #f7fafc;
    --card: #ffffff;
    --border: #e2e8f0;
    --text: #2d3748;
  }
  * { box-sizing: border-box; }
  body {
    font-family: "Segoe UI", Roboto, Arial, sans-serif;
    line-height: 1.65;
    color: var(--text);
    background: var(--bg);
    margin: 0;
    padding: 24px;
  }
  .container {
    max-width: 960px;
    margin: 0 auto;
    background: var(--card);
    padding: 40px 48px;
    border-radius: 10px;
    box-shadow: 0 4px 18px rgba(0,0,0,0.06);
  }
  header {
    border-bottom: 3px solid var(--primary);
    padding-bottom: 18px;
    margin-bottom: 28px;
  }
  h1 { color: var(--primary); margin: 0 0 8px 0; font-size: 28px; }
  .meta { color: #4a5568; font-size: 14px; }
  h2 {
    color: var(--primary);
    border-left: 4px solid var(--accent);
    padding-left: 12px;
    margin-top: 32px;
    font-size: 20px;
  }
  h3 { color: #2c5282; margin-top: 20px; font-size: 16px; }
  table {
    width: 100%%;
    border-collapse: collapse;
    margin: 16px 0 24px 0;
    font-size: 14px;
  }
  th, td {
    border: 1px solid var(--border);
    padding: 9px 12px;
    text-align: left;
  }
  th { background: #ebf4ff; color: var(--primary); font-weight: 600; }
  tr:nth-child(even) { background: #f8fafc; }
  .highlight-row { background: #e6fffa !important; font-weight: 600; }
  .kpi-grid {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    gap: 14px;
    margin: 20px 0;
  }
  .kpi {
    background: #ebf8ff;
    border: 1px solid #bee3f8;
    border-radius: 8px;
    padding: 14px;
    text-align: center;
  }
  .kpi .val { font-size: 22px; font-weight: 700; color: var(--primary); }
  .kpi .lbl { font-size: 12px; color: #4a5568; text-transform: uppercase; letter-spacing: 0.5px; }
  figure {
    margin: 20px 0;
    text-align: center;
    background: #fff;
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 12px;
  }
  figure img { max-width: 100%%; height: auto; border-radius: 4px; }
  figcaption { font-size: 13px; color: #4a5568; margin-top: 8px; }
  code {
    background: #edf2f7;
    padding: 2px 6px;
    border-radius: 4px;
    font-size: 13px;
  }
  @media print {
    body { background: #fff; padding: 0; -webkit-print-color-adjust: exact; print-color-adjust: exact; }
    .container { box-shadow: none; padding: 16px; }
    .uni-banner { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
    figure { break-inside: avoid; }
  }
  .uni-banner {
    background: #0f294a;
    padding: 18px 24px;
    border-radius: 8px;
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 22px;
    color: #ffffff;
  }
  .uni-banner img { max-height: 62px; width: auto; }
  .uni-banner .uni-sub { font-size: 13px; color: #cbd5e0; text-align: right; }
  .student-card {
    display: grid;
    grid-template-columns: repeat(2, 1fr);
    gap: 10px 24px;
    background: #f8fafc;
    border: 1px solid var(--border);
    border-left: 4px solid #0f294a;
    border-radius: 6px;
    padding: 14px 18px;
    margin-bottom: 22px;
    font-size: 14px;
  }
</style>
</head>
<body>
<div class="container">
  <div class="uni-banner">
    <img src="__LOGO__" alt="Presidency University Logo">
    <div class="uni-sub">
      <strong>School of Computer Science &amp; Engineering</strong><br>
      R Programming Project Report
    </div>
  </div>

  <header>
    <h1>Used Car Price Prediction Using R Programming</h1>
    <div class="meta">
      <strong>Domain:</strong> Data Science &amp; Predictive Analytics |
      <strong>Environment:</strong> R 4.6.2 (<code>stats</code>, <code>MASS</code>, <code>rpart</code>) |
      <strong>Date:</strong> 2026-11-25
    </div>
  </header>

  <div class="student-card">
    <div><strong>Student Name:</strong> Gutha Vishnu Vardhan Naidu</div>
    <div><strong>Roll Number:</strong> <code>20241CSD0073</code></div>
    <div><strong>Department / Section:</strong> CSE Data Science</div>
    <div><strong>University:</strong> Presidency University, Bengaluru</div>
  </div>

  <div class="kpi-grid">
    <div class="kpi"><div class="val">500</div><div class="lbl">Car Records</div></div>
    <div class="kpi"><div class="val">0.9585</div><div class="lbl">Best Test R&sup2;</div></div>
    <div class="kpi"><div class="val">0.4600 L</div><div class="lbl">Best Test RMSE</div></div>
    <div class="kpi"><div class="val">6.43%%</div><div class="lbl">Best Test MAPE</div></div>
  </div>

  <h2>1. Abstract</h2>
  <p>
    Estimating the fair resale price of a pre-owned automobile is a multivariate regression problem driven by
    vehicle age, distance driven, brand value, engine capacity, fuel type, transmission, and ownership history.
    This project implements an end-to-end <strong>Used Car Price Prediction</strong> pipeline in <strong>R</strong>
    across 500 listings spanning 10 automotive brands. Following data cleaning, median missing-value imputation,
    and feature engineering, four models are trained and benchmarked on an 80/20 train-test split:
    <strong>Multiple Linear Regression</strong>, <strong>Stepwise AIC Linear Regression</strong>,
    <strong>Log-Transformed Linear Regression</strong>, and <strong>Decision Tree Regression (<code>rpart</code>)</strong>.
    The Log-Transformed Linear Regression model achieves the strongest test performance
    (<strong>R&sup2; = 0.9585</strong>, <strong>RMSE = 0.4600 Lakhs</strong>, <strong>MAE = 0.3028 Lakhs</strong>,
    <strong>MAPE = 6.43%%</strong>), capturing exponential resale depreciation far better than additive models.
  </p>

  <h2>2. Problem Statement &amp; Objectives</h2>
  <ul>
    <li><strong>Problem Statement:</strong> Buyers and sellers in the used car market need an objective, data-driven benchmark to estimate fair resale prices from vehicle attributes.</li>
    <li><strong>Objectives:</strong>
      <ol>
        <li>Clean raw vehicle listing data, handle missing values via median imputation, and engineer <code>Car_Age</code>.</li>
        <li>Perform Exploratory Data Analysis (EDA) using summary statistics, boxplots, scatter plots, and correlation heatmaps.</li>
        <li>Train and compare four regression models in R using an 80/20 train-test split.</li>
        <li>Evaluate generalization accuracy using RMSE, MAE, R&sup2;, and MAPE, and verify model residuals.</li>
      </ol>
    </li>
  </ul>

  <h2>3. Dataset Description</h2>
  <p>The dataset (<code>data/used_cars.csv</code>) consists of <strong>500 rows</strong> and <strong>12 columns</strong>:</p>
  <table>
    <thead><tr><th>Feature</th><th>Type</th><th>Description</th></tr></thead>
    <tbody>
      <tr><td><code>Car_Name</code></td><td>Categorical</td><td>Brand and model (e.g., Hyundai Creta, Maruti Swift)</td></tr>
      <tr><td><code>Brand</code></td><td>Factor (10 levels)</td><td>Maruti, Hyundai, Honda, Tata, Toyota, Mahindra, Kia, Volkswagen, Ford, Skoda</td></tr>
      <tr><td><code>Year</code></td><td>Numeric</td><td>Manufacture year (2012&ndash;2025)</td></tr>
      <tr><td><code>Kilometers_Driven</code></td><td>Numeric</td><td>Total odometer reading (6,451 &ndash; 236,370 km)</td></tr>
      <tr><td><code>Fuel_Type</code></td><td>Factor (3 levels)</td><td>Petrol, Diesel, CNG</td></tr>
      <tr><td><code>Transmission</code></td><td>Factor (2 levels)</td><td>Manual, Automatic</td></tr>
      <tr><td><code>Owner_Type</code></td><td>Factor (3 levels)</td><td>First, Second, Third</td></tr>
      <tr><td><code>Mileage_kmpl</code></td><td>Numeric</td><td>Fuel efficiency in km/l (12.90 &ndash; 27.90)</td></tr>
      <tr><td><code>Engine_cc</code></td><td>Numeric</td><td>Engine displacement in cc (796 &ndash; 2,627)</td></tr>
      <tr><td><code>Power_bhp</code></td><td>Numeric</td><td>Engine peak power in bhp (56.10 &ndash; 202.30)</td></tr>
      <tr><td><code>Seats</code></td><td>Factor (2 levels)</td><td>Seating capacity (5 or 7 seats)</td></tr>
      <tr><td><code>Selling_Price_Lakhs</code></td><td><strong>Target</strong></td><td>Resale selling price in INR Lakhs (1.45 &ndash; 14.32)</td></tr>
    </tbody>
  </table>

  <h2>4. Methodology</h2>
  <h3>4.1 Data Cleaning &amp; Feature Engineering</h3>
  <ul>
    <li><strong>Missing-Value Imputation:</strong> 8 missing values in <code>Mileage_kmpl</code> and 6 missing values in <code>Power_bhp</code> were imputed with their respective medians (<code>19.30 kmpl</code> and <code>106.40 bhp</code>).</li>
    <li><strong>Feature Engineering:</strong> Derived <code>Car_Age = 2026 - Year</code> and converted categorical variables into R factors.</li>
    <li><strong>Train-Test Split:</strong> Partitioned into <strong>400 training rows (80%%)</strong> and <strong>100 test rows (20%%)</strong> with <code>set.seed(123)</code>.</li>
  </ul>

  <h3>4.2 Regression Models Implemented</h3>
  <ul>
    <li><strong>Multiple Linear Regression (<code>stats::lm</code>):</strong> Additive baseline using all predictors.</li>
    <li><strong>Stepwise AIC Regression (<code>MASS::stepAIC</code>):</strong> Bidirectional feature selection minimizing Akaike Information Criterion.</li>
    <li><strong>Log-Transformed Linear Regression:</strong> Fits <code>log(Selling_Price_Lakhs)</code> with bias-corrected smearing back-transformation <code>exp(pred + 0.5 * sigma^2)</code> to model exponential depreciation.</li>
    <li><strong>Decision Tree Regressor (<code>rpart::rpart</code>):</strong> Non-parametric regression tree (<code>cp = 0.01</code>, <code>maxdepth = 6</code>).</li>
  </ul>

  <h2>5. Exploratory Data Analysis (EDA)</h2>
  <table>
    <thead><tr><th>Feature</th><th>Min</th><th>Q1</th><th>Median</th><th>Mean</th><th>Q3</th><th>Max</th><th>SD</th></tr></thead>
    <tbody>
      <tr><td><code>Year</code></td><td>2012.00</td><td>2017.00</td><td>2019.00</td><td>2019.05</td><td>2021.25</td><td>2025.00</td><td>3.08</td></tr>
      <tr><td><code>Car_Age</code></td><td>1.00</td><td>4.75</td><td>7.00</td><td>6.95</td><td>9.00</td><td>14.00</td><td>3.08</td></tr>
      <tr><td><code>Kilometers_Driven</code></td><td>6,451</td><td>48,420.5</td><td>71,247.5</td><td>77,709.5</td><td>98,980.5</td><td>236,370</td><td>39,408.6</td></tr>
      <tr><td><code>Mileage_kmpl</code></td><td>12.90</td><td>17.88</td><td>19.30</td><td>19.61</td><td>21.22</td><td>27.90</td><td>2.67</td></tr>
      <tr><td><code>Engine_cc</code></td><td>796.00</td><td>1,239.75</td><td>1,427.50</td><td>1,451.95</td><td>1,652.00</td><td>2,627.00</td><td>317.10</td></tr>
      <tr><td><code>Power_bhp</code></td><td>56.10</td><td>91.22</td><td>106.40</td><td>109.35</td><td>122.42</td><td>202.30</td><td>25.78</td></tr>
      <tr><td><code>Selling_Price_Lakhs</code></td><td>1.45</td><td>2.85</td><td>4.02</td><td>4.56</td><td>5.68</td><td>14.32</td><td>2.44</td></tr>
    </tbody>
  </table>

  <figure>
    <img src="__IMG1__" alt="Selling Price Distribution">
    <figcaption>Figure 1: Right-skewed distribution of used car selling prices (Mean = 4.56 L, Median = 4.02 L).</figcaption>
  </figure>
  <figure>
    <img src="__IMG2__" alt="Price by Fuel and Transmission">
    <figcaption>Figure 2: Resale price comparison across Fuel Type and Transmission.</figcaption>
  </figure>
  <figure>
    <img src="__IMG3__" alt="Car Age and Kilometers vs Price">
    <figcaption>Figure 3: Non-linear price depreciation with Car Age and Kilometers Driven.</figcaption>
  </figure>
  <figure>
    <img src="__IMG4__" alt="Correlation Matrix">
    <figcaption>Figure 4: Pearson correlation heatmap across numeric features.</figcaption>
  </figure>

  <h2>6. Model Evaluation &amp; Results</h2>
  <table>
    <thead><tr><th>Rank</th><th>Model</th><th>RMSE (Lakhs)</th><th>MAE (Lakhs)</th><th>R&sup2;</th><th>MAPE (%%)</th></tr></thead>
    <tbody>
      <tr class="highlight-row"><td>1</td><td>Log-Transformed Linear Regression</td><td>0.4600</td><td>0.3028</td><td>0.9585</td><td>6.43%%</td></tr>
      <tr><td>2</td><td>Multiple Linear Regression</td><td>0.7258</td><td>0.5230</td><td>0.8966</td><td>12.85%%</td></tr>
      <tr><td>3</td><td>Stepwise Linear Regression (AIC)</td><td>0.7260</td><td>0.5284</td><td>0.8966</td><td>13.00%%</td></tr>
      <tr><td>4</td><td>Decision Tree (<code>rpart</code>)</td><td>1.2410</td><td>0.9478</td><td>0.6977</td><td>21.34%%</td></tr>
    </tbody>
  </table>
  <p>On the training set, the Log-Transformed Linear Regression model achieved an <strong>Adjusted R&sup2; of 0.9707</strong> and a log-scale Residual Standard Error of <strong>0.089</strong>.</p>

  <figure>
    <img src="__IMG5__" alt="Actual vs Predicted Prices">
    <figcaption>Figure 5: Actual vs Predicted selling prices on the 100-car test set.</figcaption>
  </figure>
  <figure>
    <img src="__IMG6__" alt="Residual Diagnostics">
    <figcaption>Figure 6: Residual diagnostic plots for the Log-Transformed Linear Regression model.</figcaption>
  </figure>
  <figure>
    <img src="__IMG7__" alt="Decision Tree">
    <figcaption>Figure 7: Trained <code>rpart</code> Decision Tree structure.</figcaption>
  </figure>

  <h3>Sample Test Set Predictions</h3>
  <table>
    <thead><tr><th>Car Name</th><th>Year</th><th>Fuel</th><th>Actual (L)</th><th>Log-Linear Pred (L)</th><th>Linear Pred (L)</th></tr></thead>
    <tbody>
      <tr><td>Ford Endeavour</td><td>2019</td><td>Petrol</td><td>4.91</td><td>4.52</td><td>5.12</td></tr>
      <tr><td>Tata Altroz</td><td>2021</td><td>CNG</td><td>3.46</td><td>3.48</td><td>4.30</td></tr>
      <tr><td>Skoda Kushaq</td><td>2022</td><td>CNG</td><td>9.04</td><td>8.89</td><td>8.75</td></tr>
      <tr><td>Tata Harrier</td><td>2022</td><td>Diesel</td><td>4.25</td><td>5.41</td><td>5.90</td></tr>
      <tr><td>Ford Figo</td><td>2015</td><td>Diesel</td><td>2.68</td><td>2.65</td><td>2.75</td></tr>
      <tr><td>Maruti Swift</td><td>2021</td><td>Diesel</td><td>4.91</td><td>5.28</td><td>6.05</td></tr>
      <tr><td>Maruti Dzire</td><td>2023</td><td>Petrol</td><td>3.81</td><td>3.90</td><td>4.29</td></tr>
      <tr><td>Skoda Rapid</td><td>2022</td><td>Petrol</td><td>6.55</td><td>6.45</td><td>6.96</td></tr>
      <tr><td>Volkswagen Polo</td><td>2023</td><td>Petrol</td><td>7.72</td><td>6.92</td><td>6.50</td></tr>
      <tr><td>Kia Seltos</td><td>2017</td><td>Petrol</td><td>4.04</td><td>3.68</td><td>4.28</td></tr>
    </tbody>
  </table>

  <h3>Predictions on New Unseen Cars</h3>
  <table>
    <thead><tr><th>Brand</th><th>Car Age</th><th>Fuel Type</th><th>Transmission</th><th>Kilometers</th><th>Predicted Price (Lakhs)</th></tr></thead>
    <tbody>
      <tr><td>Hyundai</td><td>4 yrs</td><td>Petrol</td><td>Automatic</td><td>38,000 km</td><td><strong>6.15</strong></td></tr>
      <tr><td>Toyota</td><td>3 yrs</td><td>Diesel</td><td>Automatic</td><td>45,000 km</td><td><strong>17.86</strong></td></tr>
      <tr><td>Maruti</td><td>6 yrs</td><td>CNG</td><td>Manual</td><td>62,000 km</td><td><strong>2.48</strong></td></tr>
    </tbody>
  </table>

  <h2>7. Conclusion &amp; Future Work</h2>
  <p>
    Modeling log-transformed used car prices captures multiplicative depreciation across age, mileage, brand,
    and drivetrain features effectively, reducing test RMSE by <strong>36.6%%</strong> (from 0.7258 to 0.4600 Lakhs)
    and cutting MAPE in half (from 12.85%% to 6.43%%) relative to standard linear regression. Future enhancements
    include training Random Forest / Gradient Boosting models and wrapping the predictor in an interactive
    <strong>R Shiny</strong> dashboard.
  </p>
</div>
</body>
</html>'

html_content <- gsub("%%", "%", html_template, fixed = TRUE)
html_content <- sub("__LOGO__", logo, html_content, fixed = TRUE)
html_content <- sub("__IMG1__", img1, html_content, fixed = TRUE)
html_content <- sub("__IMG2__", img2, html_content, fixed = TRUE)
html_content <- sub("__IMG3__", img3, html_content, fixed = TRUE)
html_content <- sub("__IMG4__", img4, html_content, fixed = TRUE)
html_content <- sub("__IMG5__", img5, html_content, fixed = TRUE)
html_content <- sub("__IMG6__", img6, html_content, fixed = TRUE)
html_content <- sub("__IMG7__", img7, html_content, fixed = TRUE)

dir.create("report", showWarnings = FALSE, recursive = TRUE)
writeLines(html_content, "report/Used_Car_Price_Prediction_Report.html", useBytes = TRUE)
cat("Standalone HTML report generated at report/Used_Car_Price_Prediction_Report.html\n")
