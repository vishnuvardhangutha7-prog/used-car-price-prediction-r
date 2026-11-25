# ==============================================================================
# Project: Used Car Price Prediction in R
# File: scripts/02_used_car_price_prediction.R
# Description: End-to-end data science pipeline in R (using base & recommended
#              packages only: stats, MASS, rpart, graphics, grDevices, utils):
#              1. Data Loading & Inspection
#              2. Data Cleaning & Missing Value Imputation
#              3. Feature Engineering (Car_Age, Price_per_BHP)
#              4. Exploratory Data Analysis (EDA) & Visualizations
#              5. Train-Test Split (80/20)
#              6. Model Training:
#                 - Multiple Linear Regression (lm)
#                 - Stepwise AIC Linear Regression (MASS::stepAIC)
#                 - Log-Transformed Linear Regression (log lm)
#                 - Decision Tree Regressor (rpart::rpart)
#              7. Model Evaluation (RMSE, MAE, R2, MAPE) & Comparison
#              8. Diagnostic Plots & Sample Predictions
# ==============================================================================

suppressPackageStartupMessages({
  library(MASS)
  library(rpart)
})

# Create output directories
dir.create("outputs/plots", showWarnings = FALSE, recursive = TRUE)

cat("==============================================================\n")
cat("          USED CAR PRICE PREDICTION - R PROJECT\n")
cat("==============================================================\n\n")

# ------------------------------------------------------------------------------
# 1. Load Dataset
# ------------------------------------------------------------------------------
data_path <- "data/used_cars.csv"
if (!file.exists(data_path)) {
  stop("Dataset not found at data/used_cars.csv. Run scripts/01_generate_dataset.R first.")
}

cars <- read.csv(data_path, stringsAsFactors = FALSE)
cat("1. DATA OVERVIEW\n")
cat("   Rows:", nrow(cars), "| Columns:", ncol(cars), "\n")
cat("   Missing values per column:\n")
print(colSums(is.na(cars)))
cat("\n")

# ------------------------------------------------------------------------------
# 2. Data Cleaning & Feature Engineering
# ------------------------------------------------------------------------------
# Impute missing numeric values using median (robust to outliers)
median_mileage <- median(cars$Mileage_kmpl, na.rm = TRUE)
median_power   <- median(cars$Power_bhp, na.rm = TRUE)

cars$Mileage_kmpl[is.na(cars$Mileage_kmpl)] <- median_mileage
cars$Power_bhp[is.na(cars$Power_bhp)]       <- median_power

# Remove duplicate rows if any
cars <- unique(cars)

# Feature Engineering
current_year <- 2026
cars$Car_Age <- current_year - cars$Year

# Convert categorical columns to factors with sensible reference levels
cars$Brand        <- factor(cars$Brand)
cars$Fuel_Type    <- factor(cars$Fuel_Type, levels = c("Petrol", "Diesel", "CNG"))
cars$Transmission <- factor(cars$Transmission, levels = c("Manual", "Automatic"))
cars$Owner_Type   <- factor(cars$Owner_Type, levels = c("First", "Second", "Third"))
cars$Seats        <- factor(cars$Seats)

cat("2. DATA CLEANING COMPLETE\n")
cat("   Imputed Mileage_kmpl NAs with median:", round(median_mileage, 2), "kmpl\n")
cat("   Imputed Power_bhp NAs with median:", round(median_power, 2), "bhp\n")
cat("   Remaining NAs:", sum(is.na(cars)), "\n\n")

# Summary statistics of numeric columns
num_cols <- c("Year", "Car_Age", "Kilometers_Driven", "Mileage_kmpl",
              "Engine_cc", "Power_bhp", "Selling_Price_Lakhs")
cat("3. SUMMARY STATISTICS (NUMERIC FEATURES)\n")
print(round(t(sapply(cars[, num_cols], function(x) {
  c(Min = min(x), Q1 = quantile(x, 0.25), Median = median(x),
    Mean = mean(x), Q3 = quantile(x, 0.75), Max = max(x), SD = sd(x))
})), 2))
cat("\n")

# ------------------------------------------------------------------------------
# 3. Exploratory Data Analysis (EDA) & Plots
# ------------------------------------------------------------------------------
cat("4. GENERATING EDA PLOTS...\n")

# Plot 1: Distribution of Selling Price (Histogram + Boxplot)
png("outputs/plots/01_price_distribution.png", width = 1000, height = 520, res = 120)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)
hist(cars$Selling_Price_Lakhs, breaks = 24, col = "#2B6CB0", border = "white",
     main = "Distribution of Used Car Selling Price",
     xlab = "Selling Price (INR Lakhs)", ylab = "Frequency")
abline(v = mean(cars$Selling_Price_Lakhs), col = "#E53E3E", lwd = 2, lty = 2)
abline(v = median(cars$Selling_Price_Lakhs), col = "#38A169", lwd = 2, lty = 3)
legend("topright", legend = c(
  paste("Mean:", round(mean(cars$Selling_Price_Lakhs), 2)),
  paste("Median:", round(median(cars$Selling_Price_Lakhs), 2))
), col = c("#E53E3E", "#38A169"), lty = c(2, 3), lwd = 2, bty = "n")

boxplot(cars$Selling_Price_Lakhs, col = "#63B3ED", border = "#1A365D",
        main = "Selling Price Boxplot", ylab = "Selling Price (INR Lakhs)")
grid(nx = NA, ny = NULL)
dev.off()

# Plot 2: Selling Price by Fuel Type & Transmission
png("outputs/plots/02_price_by_fuel_transmission.png", width = 1050, height = 520, res = 120)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)
boxplot(Selling_Price_Lakhs ~ Fuel_Type, data = cars,
        col = c("#4299E1", "#48BB78", "#ECC94B"), border = "#2D3748",
        main = "Selling Price by Fuel Type",
        xlab = "Fuel Type", ylab = "Selling Price (INR Lakhs)")
grid(nx = NA, ny = NULL)

boxplot(Selling_Price_Lakhs ~ Transmission, data = cars,
        col = c("#9F7AEA", "#ED8936"), border = "#2D3748",
        main = "Selling Price by Transmission",
        xlab = "Transmission", ylab = "Selling Price (INR Lakhs)")
grid(nx = NA, ny = NULL)
dev.off()

# Plot 3: Car Age & Kilometers Driven vs Selling Price
png("outputs/plots/03_year_km_vs_price.png", width = 1050, height = 520, res = 120)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)
plot(cars$Car_Age, cars$Selling_Price_Lakhs,
     pch = 19, col = adjustcolor("#2B6CB0", alpha.f = 0.55),
     main = "Car Age vs Selling Price",
     xlab = "Car Age (Years)", ylab = "Selling Price (INR Lakhs)")
lines(lowess(cars$Car_Age, cars$Selling_Price_Lakhs), col = "#E53E3E", lwd = 2.5)
grid()

plot(cars$Kilometers_Driven / 1000, cars$Selling_Price_Lakhs,
     pch = 19, col = adjustcolor("#2F855A", alpha.f = 0.55),
     main = "Kilometers Driven vs Selling Price",
     xlab = "Kilometers Driven ('000 km)", ylab = "Selling Price (INR Lakhs)")
lines(lowess(cars$Kilometers_Driven / 1000, cars$Selling_Price_Lakhs), col = "#E53E3E", lwd = 2.5)
grid()
dev.off()

# Plot 4: Correlation Matrix Heatmap
corr_mat <- cor(cars[, num_cols])
png("outputs/plots/04_correlation_matrix.png", width = 850, height = 750, res = 120)
par(mar = c(8, 8, 4, 3))
palette_cols <- colorRampPalette(c("#C53030", "#FFFFFF", "#2B6CB0"))(100)
image(seq_len(ncol(corr_mat)), seq_len(nrow(corr_mat)),
      t(corr_mat[nrow(corr_mat):1, ]),
      col = palette_cols, zlim = c(-1, 1),
      axes = FALSE, xlab = "", ylab = "",
      main = "Correlation Matrix of Numeric Features")
axis(1, at = seq_len(ncol(corr_mat)), labels = colnames(corr_mat), las = 2, cex.axis = 0.85)
axis(2, at = seq_len(nrow(corr_mat)), labels = rev(rownames(corr_mat)), las = 2, cex.axis = 0.85)
for (i in seq_len(nrow(corr_mat))) {
  for (j in seq_len(ncol(corr_mat))) {
    val <- corr_mat[nrow(corr_mat) - i + 1, j]
    text(j, i, sprintf("%.2f", val), cex = 0.8,
         col = ifelse(abs(val) > 0.6, "white", "black"))
  }
}
dev.off()

# ------------------------------------------------------------------------------
# 4. Train-Test Split (80% Train, 20% Test)
# ------------------------------------------------------------------------------
set.seed(123)
n_rows <- nrow(cars)
train_idx <- sample(seq_len(n_rows), size = floor(0.8 * n_rows))
train_data <- cars[train_idx, ]
test_data  <- cars[-train_idx, ]

cat("5. TRAIN-TEST SPLIT\n")
cat("   Training rows:", nrow(train_data), "| Testing rows:", nrow(test_data), "\n\n")

# Evaluation metric helper
eval_metrics <- function(actual, predicted, model_name) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2   <- 1 - ss_res / ss_tot
  mape <- mean(abs((actual - predicted) / actual)) * 100
  data.frame(
    Model = model_name,
    RMSE  = round(rmse, 4),
    MAE   = round(mae, 4),
    R2    = round(r2, 4),
    MAPE_pct = round(mape, 2),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# 5. Model Building
# ------------------------------------------------------------------------------
formula_full <- Selling_Price_Lakhs ~ Brand + Car_Age + Kilometers_Driven +
  Fuel_Type + Transmission + Owner_Type + Mileage_kmpl + Engine_cc +
  Power_bhp + Seats

# Model 1: Multiple Linear Regression
lm_model <- lm(formula_full, data = train_data)
pred_lm  <- pmax(0.5, predict(lm_model, newdata = test_data))

# Model 2: Stepwise Regression (AIC)
step_model <- stepAIC(lm_model, direction = "both", trace = FALSE)
pred_step  <- pmax(0.5, predict(step_model, newdata = test_data))

# Model 3: Log-Transformed Linear Regression (captures exponential depreciation)
formula_log <- log(Selling_Price_Lakhs) ~ Brand + Car_Age + Kilometers_Driven +
  Fuel_Type + Transmission + Owner_Type + Mileage_kmpl + Engine_cc +
  Power_bhp + Seats
log_lm_model <- lm(formula_log, data = train_data)
# Bias-corrected smearing back-transformation: exp(pred + sigma^2 / 2)
sigma2 <- summary(log_lm_model)$sigma^2
pred_log_lm <- exp(predict(log_lm_model, newdata = test_data) + 0.5 * sigma2)

# Model 4: Decision Tree Regressor (rpart)
tree_model <- rpart(formula_full, data = train_data, method = "anova",
                    control = rpart.control(cp = 0.01, minsplit = 15, maxdepth = 6))
pred_tree  <- predict(tree_model, newdata = test_data)

# ------------------------------------------------------------------------------
# 6. Model Evaluation & Comparison
# ------------------------------------------------------------------------------
results <- rbind(
  eval_metrics(test_data$Selling_Price_Lakhs, pred_lm, "Multiple Linear Regression"),
  eval_metrics(test_data$Selling_Price_Lakhs, pred_step, "Stepwise Linear Regression (AIC)"),
  eval_metrics(test_data$Selling_Price_Lakhs, pred_log_lm, "Log-Transformed Linear Regression"),
  eval_metrics(test_data$Selling_Price_Lakhs, pred_tree, "Decision Tree (rpart)")
)

results <- results[order(results$RMSE), ]
rownames(results) <- NULL

cat("6. MODEL EVALUATION ON TEST SET\n")
print(results)
cat("\n")

cat("7. BEST MODEL SUMMARY (Log-Transformed Linear Regression)\n")
cat("   Adjusted R-squared (Train):", round(summary(log_lm_model)$adj.r.squared, 4), "\n")
cat("   Residual Standard Error (log-scale):", round(summary(log_lm_model)$sigma, 4), "\n\n")

write.csv(results, "outputs/model_comparison.csv", row.names = FALSE)

# Save test set predictions
predictions_df <- data.frame(
  Car_Name = test_data$Car_Name,
  Year = test_data$Year,
  Fuel_Type = test_data$Fuel_Type,
  Transmission = test_data$Transmission,
  Kilometers_Driven = test_data$Kilometers_Driven,
  Actual_Price_Lakhs = test_data$Selling_Price_Lakhs,
  Pred_Linear = round(pred_lm, 2),
  Pred_LogLinear = round(pred_log_lm, 2),
  Pred_DecisionTree = round(pred_tree, 2)
)
write.csv(predictions_df, "outputs/test_predictions.csv", row.names = FALSE)

cat("8. SAMPLE PREDICTIONS (First 10 Test Cars)\n")
print(head(predictions_df[, c("Car_Name", "Year", "Fuel_Type",
                              "Actual_Price_Lakhs", "Pred_LogLinear", "Pred_Linear")], 10))
cat("\n")

# ------------------------------------------------------------------------------
# 7. Model Plots
# ------------------------------------------------------------------------------
# Plot 5: Actual vs Predicted Prices (Linear vs Log-Linear vs Decision Tree)
png("outputs/plots/05_actual_vs_predicted.png", width = 1200, height = 430, res = 120)
par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3.5, 1.5))

plot_avp <- function(actual, pred, title_txt, pt_col) {
  lim <- range(c(actual, pred))
  plot(actual, pred, pch = 19, col = adjustcolor(pt_col, alpha.f = 0.65),
       xlim = lim, ylim = lim,
       xlab = "Actual Price (Lakhs)", ylab = "Predicted Price (Lakhs)",
       main = title_txt)
  abline(0, 1, col = "#E53E3E", lwd = 2, lty = 2)
  grid()
}

plot_avp(test_data$Selling_Price_Lakhs, pred_lm,
         sprintf("Multiple Linear (R2=%.3f)", results$R2[results$Model == "Multiple Linear Regression"]),
         "#3182CE")
plot_avp(test_data$Selling_Price_Lakhs, pred_log_lm,
         sprintf("Log-Linear (R2=%.3f)", results$R2[results$Model == "Log-Transformed Linear Regression"]),
         "#2F855A")
plot_avp(test_data$Selling_Price_Lakhs, pred_tree,
         sprintf("Decision Tree (R2=%.3f)", results$R2[results$Model == "Decision Tree (rpart)"]),
         "#805AD5")
dev.off()

# Plot 6: Residual Diagnostics for Best Model (Log-Linear)
png("outputs/plots/06_residual_diagnostics.png", width = 1000, height = 800, res = 120)
par(mfrow = c(2, 2))
plot(log_lm_model, which = 1:4, pch = 19, col = adjustcolor("#2B6CB0", alpha.f = 0.6))
dev.off()

# Plot 7: Decision Tree Visualization
png("outputs/plots/07_decision_tree.png", width = 1150, height = 700, res = 120)
par(mar = c(2, 2, 3, 2))
plot(tree_model, uniform = TRUE, margin = 0.12,
     main = "Decision Tree Regressor for Used Car Price (INR Lakhs)")
text(tree_model, use.n = TRUE, all = TRUE, cex = 0.72)
dev.off()

# ------------------------------------------------------------------------------
# 8. Predict on New Unseen Car Examples
# ------------------------------------------------------------------------------
new_cars <- data.frame(
  Brand = factor(c("Hyundai", "Toyota", "Maruti"), levels = levels(cars$Brand)),
  Car_Age = c(4, 3, 6),
  Kilometers_Driven = c(38000, 45000, 62000),
  Fuel_Type = factor(c("Petrol", "Diesel", "CNG"), levels = levels(cars$Fuel_Type)),
  Transmission = factor(c("Automatic", "Automatic", "Manual"), levels = levels(cars$Transmission)),
  Owner_Type = factor(c("First", "First", "Second"), levels = levels(cars$Owner_Type)),
  Mileage_kmpl = c(17.2, 13.5, 25.4),
  Engine_cc = c(1497, 2393, 1197),
  Power_bhp = c(113.4, 148.0, 81.8),
  Seats = factor(c(5, 7, 5), levels = levels(cars$Seats))
)

new_cars$Predicted_Price_Lakhs <- round(
  exp(predict(log_lm_model, newdata = new_cars) + 0.5 * sigma2), 2
)

cat("9. SAMPLE NEW CAR PREDICTIONS\n")
print(new_cars[, c("Brand", "Car_Age", "Fuel_Type", "Transmission",
                   "Kilometers_Driven", "Predicted_Price_Lakhs")])
cat("\nAll outputs and plots saved in outputs/.\n")
