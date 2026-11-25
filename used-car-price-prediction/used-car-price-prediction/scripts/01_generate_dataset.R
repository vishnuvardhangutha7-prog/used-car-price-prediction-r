# ==============================================================================
# Project: Used Car Price Prediction
# File: scripts/01_generate_dataset.R
# Description: Generates a realistic, reproducible 500-row used car dataset
#              (with realistic market relationships, noise, and a few NAs)
#              and saves it to data/used_cars.csv.
# ==============================================================================

set.seed(42)

n <- 500

brands <- c("Maruti", "Hyundai", "Honda", "Tata", "Toyota",
            "Mahindra", "Kia", "Volkswagen", "Ford", "Skoda")
brand_probs <- c(0.24, 0.20, 0.12, 0.12, 0.08, 0.08, 0.05, 0.04, 0.04, 0.03)
brand_base_price <- c(
  Maruti = 6.2, Hyundai = 7.8, Honda = 9.5, Tata = 7.4, Toyota = 14.5,
  Mahindra = 11.2, Kia = 11.8, Volkswagen = 10.5, Ford = 8.4, Skoda = 12.6
)

models_by_brand <- list(
  Maruti     = c("Swift", "Baleno", "Dzire", "Brezza", "WagonR"),
  Hyundai    = c("i20", "Creta", "Verna", "Grand i10", "Venue"),
  Honda      = c("City", "Amaze", "Jazz", "WR-V"),
  Tata       = c("Nexon", "Altroz", "Harrier", "Tiago"),
  Toyota     = c("Innova Crysta", "Fortuner", "Glanza", "Urban Cruiser"),
  Mahindra   = c("XUV500", "Scorpio", "XUV300", "Thar"),
  Kia        = c("Seltos", "Sonet", "Carens"),
  Volkswagen = c("Polo", "Vento", "Taigun"),
  Ford       = c("EcoSport", "Figo", "Endeavour"),
  Skoda      = c("Rapid", "Kushaq", "Octavia")
)

Brand <- sample(brands, n, replace = TRUE, prob = brand_probs)
Car_Model <- sapply(Brand, function(b) sample(models_by_brand[[b]], 1))

Year <- sample(2012:2025, n, replace = TRUE,
               prob = c(0.02, 0.03, 0.04, 0.05, 0.07, 0.08, 0.10, 0.11,
                        0.12, 0.12, 0.11, 0.08, 0.05, 0.02))
Car_Age <- 2026 - Year

Fuel_Type <- sample(c("Petrol", "Diesel", "CNG"), n, replace = TRUE,
                    prob = c(0.55, 0.37, 0.08))
Transmission <- sample(c("Manual", "Automatic"), n, replace = TRUE,
                       prob = c(0.72, 0.28))
Owner_Type <- sample(c("First", "Second", "Third"), n, replace = TRUE,
                     prob = c(0.74, 0.21, 0.05))

# Realistic kilometers driven correlated with age
Kilometers_Driven <- round(pmax(
  3500,
  Car_Age * rnorm(n, mean = 10500, sd = 2800) + rnorm(n, mean = 3000, sd = 4000)
))

# Engine CC & Power BHP influenced by brand and fuel
Engine_cc <- round(pmax(
  796,
  ifelse(Brand %in% c("Toyota", "Mahindra", "Skoda", "Kia"),
         rnorm(n, 1750, 320),
         rnorm(n, 1280, 220)) +
    ifelse(Fuel_Type == "Diesel", 220, 0)
))

Power_bhp <- round(pmax(
  55,
  Engine_cc * runif(n, 0.062, 0.085) + ifelse(Transmission == "Automatic", 10, 0)
), 1)

Mileage_kmpl <- round(pmax(
  9.5,
  24.5 - 0.0042 * Engine_cc +
    ifelse(Fuel_Type == "Diesel", 3.2, 0) +
    ifelse(Fuel_Type == "CNG", 5.5, 0) -
    ifelse(Transmission == "Automatic", 1.4, 0) +
    rnorm(n, 0, 1.5)
), 1)

Seats <- ifelse(Car_Model %in% c("Innova Crysta", "Fortuner", "Scorpio", "XUV500", "Carens"),
                7, 5)

# True underlying price model in INR Lakhs (1 Lakh = 100,000 INR)
base <- brand_base_price[Brand]
fuel_adj <- ifelse(Fuel_Type == "Diesel", 1.12, ifelse(Fuel_Type == "CNG", 0.94, 1.00))
trans_adj <- ifelse(Transmission == "Automatic", 1.18, 1.00)
owner_adj <- ifelse(Owner_Type == "First", 1.00, ifelse(Owner_Type == "Second", 0.87, 0.74))

# Exponential depreciation with age + linear penalty for km + engine/power boost
raw_price <- (base + 0.045 * (Power_bhp - 85) + ifelse(Seats == 7, 2.2, 0)) *
  fuel_adj * trans_adj * owner_adj *
  exp(-0.095 * Car_Age) *
  exp(-0.0000032 * Kilometers_Driven)

Selling_Price_Lakhs <- round(pmax(1.45, raw_price * rnorm(n, mean = 1.0, sd = 0.075)), 2)

used_cars <- data.frame(
  Car_Name = paste(Brand, Car_Model),
  Brand = Brand,
  Year = Year,
  Kilometers_Driven = Kilometers_Driven,
  Fuel_Type = Fuel_Type,
  Transmission = Transmission,
  Owner_Type = Owner_Type,
  Mileage_kmpl = Mileage_kmpl,
  Engine_cc = Engine_cc,
  Power_bhp = Power_bhp,
  Seats = Seats,
  Selling_Price_Lakhs = Selling_Price_Lakhs,
  stringsAsFactors = FALSE
)

# Inject a small realistic number of missing values (for data cleaning demonstration)
na_idx_mileage <- sample(seq_len(n), 8)
na_idx_power   <- sample(seq_len(n), 6)
used_cars$Mileage_kmpl[na_idx_mileage] <- NA
used_cars$Power_bhp[na_idx_power]      <- NA

dir.create("data", showWarnings = FALSE, recursive = TRUE)
write.csv(used_cars, "data/used_cars.csv", row.names = FALSE)

cat("Dataset successfully generated at data/used_cars.csv with", nrow(used_cars), "rows and",
    ncol(used_cars), "columns.\n")
