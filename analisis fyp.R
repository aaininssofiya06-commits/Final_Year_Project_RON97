# Analisis FYP

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(forecast)
library(tseries)
library(readr)

# ============================================================
# IMPORT DAN BERSIHKAN SETIAP DATASET
# ============================================================

ron97 <- read.csv("C:/Users/User/Downloads/fyp/fuelprice 2.csv")
ron97
brent <- read_csv("C:/Users/User/Downloads/fyp/Crude Oil WTI Futures Historical Data 2.csv")
brent
usdmyr <- read_csv("C:/Users/User/Downloads/fyp/daily_0900.csv")
usdmyr

names(ron97)
names(brent)
names(usdmyr)

str(ron97)
str(brent)
str(usdmyr)

# ==========================================
# CLEAN RON97 DATA
# ==========================================

# Semak kategori series_type
table(ron97$series_type, useNA = "ifany")

# Kekalkan harga sebenar sahaja

ron97_clean <- ron97 %>%
  filter(series_type == "level") %>%
  transmute(
    date = dmy(date),
    price_ron97 = as.numeric(ron97)
  ) %>%
  arrange(date)

#ron97_clean <- ron97 %>%
#  filter(series_type == "level") %>%
#  mutate(
#    date = dmy(date),
#    price_ron97 = ron97
#  ) %>%
#  select(date, price_ron97) %>%
#  arrange(date)

head(ron97_clean)
tail(ron97_clean)

# Semak nilai hilang
colSums(is.na(ron97_clean))

# Semak tarikh pendua
sum(duplicated(ron97_clean$date))

# Semak bilangan rekod
nrow(ron97_clean)

# ==========================================
# CLEAN USD/MYR DATA
# ==========================================

usd_clean <- usdmyr %>%
  filter(rate_type == "middle") %>% # Tapis kadar pertengahan sahaja
  transmute(
    date = as.Date(date, format = "%d/%m/%Y"), # %d/%m/%Y untuk d/m/yyyy
    usd_myr = as.numeric(USD)
  ) %>%
  arrange(date)
head(usd_clean)
tail(usd_clean)

# Semak nilai hilang
colSums(is.na(usd_clean))

# Semak tarikh pendua
sum(duplicated(usd_clean))

# Semak bilangan rekod
nrow(usd_clean)

# ==========================================
# CLEAN BRENT DATA
# ==========================================

brent_clean <- brent %>%
  transmute(
    date = as.Date(Date, format = "%m/%d/%Y"), # %m/%d/%Y untuk format m/d/y
    price_brent = as.numeric(Price)
  ) %>%
  arrange(date) # Membetulkan susunan yang terbalik dari asal
head(brent_clean)
tail(brent_clean)

# Semak nilai hilang
colSums(is.na(brent_clean))

# Semak tarikh pendua
sum(duplicated(brent_clean))

# Semak bilangan rekod
nrow(brent_clean)

# ==============================================================================
#  TAPIS DATASET MENGIKUT SKOP KAJIAN (APRIL 2017 - SEPT 2026)
# ==============================================================================

# Skop tarikh RON97: 30 Mac 2017 hingga 30 Sept 2026
ron97_filtered <- ron97_clean %>%
  filter(date >= as.Date("2017-03-30") & date <= as.Date("2026-09-30"))
head(ron97_filtered)
tail(ron97_filtered)

# Skop tarikh eksogen (Bermula 23 Mac 2017 untuk menangkap julat minggu pertama)
usd_filtered <- usd_clean %>%
  filter(date >= as.Date("2017-03-23") & date <= as.Date("2026-09-30"))
head(usd_filtered)
tail(usd_filtered)

brent_filtered <- brent_clean %>%
  filter(date >= as.Date("2017-03-23") & date <= as.Date("2026-09-30"))
head(brent_filtered)
tail(brent_filtered)

nrow(ron97_filtered)
nrow(usd_filtered)
nrow(brent_filtered)

# ==============================================================================
#  BINA TINGKAP MINGGUAN (JUMAAT - RABU) & KIRA PURATA
# ==============================================================================


# ==============================================================================
# FASA 1: PEMBERSIHAN DAN PENYELARASAN DATA
# ==============================================================================

library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(tseries)
library(psych)
library(forecast)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1.1 PEMBERSIHAN DATA RON97
# ------------------------------------------------------------------------------

ron97_clean <- ron97 %>%
  filter(series_type == "level") %>%
  transmute(
    date = dmy(date),
    price_ron97 = as.numeric(ron97)
  ) %>%
  filter(!is.na(date)) %>%
  arrange(date)

# Tempoh kajian: April 2017 - September 2026
ron97_filtered <- ron97_clean %>%
  filter(
    date >= as.Date("2017-04-01"),
    date <= as.Date("2026-09-30")
  )

# Semakan tarikh dan bilangan pemerhatian
cat("\n=== SEMAKAN DATA RON97 ===\n")
cat("Bilangan pemerhatian:", nrow(ron97_filtered), "\n")
print(range(ron97_filtered$date))
print(head(ron97_filtered))
print(tail(ron97_filtered))

# ------------------------------------------------------------------------------
# 1.2 SEMAKAN DATA BRENT DAN USD/MYR
# ------------------------------------------------------------------------------

# Pastikan kedua-dua pemboleh ubah tarikh ialah kelas Date
stopifnot(inherits(brent_filtered$date, "Date"))
stopifnot(inherits(usd_filtered$date, "Date"))

cat("\n=== SEMAKAN DATA BRENT ===\n")
cat("Bilangan pemerhatian:", nrow(brent_filtered), "\n")
print(range(brent_filtered$date))

cat("\n=== SEMAKAN DATA USD/MYR ===\n")
cat("Bilangan pemerhatian:", nrow(usd_filtered), "\n")
print(range(usd_filtered$date))

df_final <- ron97_filtered %>%
  mutate(
    start_date = date - days(6), # Jumaat minggu sebelum
    end_date   = date - days(1), # Rabu sebelum pengumuman
    
    # Pengiraan purata USD/MYR menggunakan map_dbl 
    usd_myr = map2_dbl(start_date, end_date, ~ {
      vals <- usd_filtered$usd_myr[usd_filtered$date >= .x & usd_filtered$date <= .y]
      if (length(vals) == 0) return(NA_real_) else mean(vals, na.rm = TRUE)
    }),
    
    # Pengiraan purata Brent
    price_brent = map2_dbl(start_date, end_date, ~ {
      vals <- brent_filtered$price_brent[brent_filtered$date >= .x & brent_filtered$date <= .y]
      if (length(vals) == 0) return(NA_real_) else mean(vals, na.rm = TRUE)
    })
  ) %>%
  select(date, price_ron97, price_brent, usd_myr) %>%
  arrange(date) %>%
  drop_na()

head(df_final)
tail(df_final)

cat("\nJumlah bilangan pemerhatian (N):", nrow(df_final), "\n")

# --- Pengesanan Nilai Hilang (Missing Values) ---
cat("\n=== SEMAKAN NILAI HILANG ===\n")
print(colSums(is.na(df_final)))
df_final <- df_final %>% drop_na()

# --- Pengesanan Data Terpencil (Outliers) ---
# Menggunakan kaedah Z-score & IQR
z_scores <- scale(df_final %>% select(price_ron97, price_brent, usd_myr))
outliers_z <- which(abs(z_scores) > 3, arr.ind = TRUE)
cat("\nBilangan Data Terpencil Dikesan (Z-score > 3):", nrow(outliers_z), "\n")

# Kaedah IQR
iqr_outliers <- sapply(df_final, function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  
  sum(x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr, na.rm = TRUE)
})

cat("\nBilangan outlier mengikut kaedah IQR:\n")
print(iqr_outliers)

# --- Ujian Kepegunan (ADF & KPSS) ---
cat("\n=== UJIAN KEPEGUNAN PADA ARAS (LEVEL) ===\n")
print(adf.test(df_final$price_ron97))
print(kpss.test(df_final$price_ron97))

cat("\n=== UJIAN KEPEGUNAN PADA PERBEZAAN PERTAMA (1ST DIFFERENCE, d=1) ===\n")
ron97_diff <- diff(df_final$price_ron97)
print(adf.test(ron97_diff))
print(kpss.test(ron97_diff))

# ==============================================================================
# FASA ANALISIS PENEROKAAN DATA (EDA) & KORELASI
# ==============================================================================
library(psych)
library(ggplot2)
library(gridExtra)

# ------------------------------------------------------------------------------
# 1. STATISTIK DESKRIPTIF (Objektif 1)
# ------------------------------------------------------------------------------
# Mengira Min, Sisihan Piawai, Median, Minima, Maksima, Pencong (Skewness), & Kurtosis
desc_stats <- describe(df_final %>% select(price_ron97, price_brent, usd_myr))
print("--- STATISTIK DESKRIPTIF ---")
print(desc_stats)

# ------------------------------------------------------------------------------
# 2. PLOT SIRI MASA & PURATA BERGERAK (Objektif 1)
# ------------------------------------------------------------------------------
# Membina Plot Siri Masa Utama bagi RON97 dengan Purata Bergerak (MA 4 minggu & MA 12 minggu)
df_plot <- df_final %>%
  mutate(
    MA_4  = forecast::ma(price_ron97, order = 4),  # Purata bergerak 1 bulan
    MA_12 = forecast::ma(price_ron97, order = 12)  # Purata bergerak 3 bulan
  )

ggplot(df_plot, aes(x = date)) +
  geom_line(aes(y = price_ron97, color = "Harga RON97"), size = 0.7) +
  geom_line(aes(y = MA_4, color = "MA 4-Minggu"), size = 0.8, linetype = "dashed") +
  geom_line(aes(y = MA_12, color = "MA 12-Minggu"), size = 0.8, linetype = "solid") +
  labs(
    title = "Trend Pergerakan Harga RON97 di Malaysia (April 2017 - September 2026)",
    subtitle = "Analisis Trend Bersama Purata Bergerak (Moving Average)",
    x = "Tarikh",
    y = "Harga (RM / Liter)",
    color = "Petunjuk"
  ) +
  scale_color_manual(values = c("Harga RON97" = "#2c3e50", "MA 4-Minggu" = "#e74c3c", "MA 12-Minggu" = "#2ecc71")) +
  theme_minimal()

# ------------------------------------------------------------------------------
# 3. ANALISIS KORELASI (Objektif 2)
# ------------------------------------------------------------------------------
# Semakan Normaliti (Shapiro-Wilk Test) untuk menentukan ujian korelasi yang sesuai
shapiro.test(df_final$price_ron97)

# p-value < 0.05 (Data tidak taburan normal), gunakan Spearman. Jika tidak, Pearson.
cor_matrix_pearson  <- cor(df_final %>% select(price_ron97, price_brent, usd_myr), method = "pearson")
cor_matrix_spearman <- cor(df_final %>% select(price_ron97, price_brent, usd_myr), method = "spearman")

print("--- MATRIKS KORELASI PEARSON ---")
print(cor_matrix_pearson)

print("--- MATRIKS KORELASI SPEARMAN ---")
print(cor_matrix_spearman)

# ==============================================================================
# PEMBAHAGIAN DATASET & PEMBANGUNAN MODEL
# ==============================================================================

# Data pemodelan: April 2017 - Julai 2026
df_model <- df_final %>%
  filter(
    date >= as.Date("2017-04-01"),
    date <= as.Date("2026-07-31")
  ) %>%
  arrange(date)

# Data ramalan akhir: Ogos - September 2026
df_future <- df_final %>%
  filter(
    date >= as.Date("2026-08-01"),
    date <= as.Date("2026-09-30")
  ) %>%
  arrange(date)

cat("\n=== TEMPOH DATA PEMODELAN ===\n")
cat("Bilangan pemerhatian:", nrow(df_model), "\n")
print(range(df_model$date))

cat("\n=== TEMPOH DATA RAMALAN AKHIR ===\n")
cat("Bilangan pemerhatian:", nrow(df_future), "\n")

if (nrow(df_future) > 0) {
  print(range(df_future$date))
}

# Pastikan tiada nilai hilang
stopifnot(!anyNA(df_model))
stopifnot(!anyNA(df_future))

# Semak sela tarikh mingguan
cat("\n=== SEMAKAN SELA TARIKH DATA PEMODELAN ===\n")
print(table(as.integer(diff(df_model$date))))

# Jika terdapat sela selain 7 hari, periksa minggu yang hilang
missing_intervals <- which(as.integer(diff(df_model$date)) != 7)

if (length(missing_intervals) > 0) {
  cat("Amaran: terdapat sela tarikh yang bukan 7 hari.\n")
  print(df_model$date[missing_intervals])
  print(df_model$date[missing_intervals + 1])
}

# Semak data asal sekitar tarikh yang mencurigakan

ron97 %>%
  dplyr::filter(series_type == "level") %>%
  dplyr::mutate(date_parsed = lubridate::dmy(date)) %>%
  dplyr::filter(
    date_parsed >= as.Date("2018-05-01"),
    date_parsed <= as.Date("2019-02-01")
  ) %>%
  dplyr::select(date, date_parsed, ron97) %>%
  tibble::as_tibble() %>%
  dplyr::print(n = 50)

df_future %>%
  dplyr::select(date, price_ron97, price_brent, usd_myr) %>%
  tibble::as_tibble() %>%
  dplyr::print(n = Inf)

View(df_future)

print(
  ron97 %>%
    dplyr::filter(series_type == "level") %>%
    dplyr::mutate(date_parsed = lubridate::dmy(date)) %>%
    dplyr::filter(
      date_parsed >= as.Date("2018-05-01"),
      date_parsed <= as.Date("2019-02-01")
    ) %>%
    dplyr::select(date, date_parsed, ron97),
  na.print = "NA"
)

print(
  df_future %>%
    dplyr::select(date, price_ron97, price_brent, usd_myr),
  na.print = "NA"
)

str(ron97$date)
head(ron97$date, 10)
tail(ron97$date, 10)

class(ron97_clean$date)











# Pembahagian Data Kronologi: 80% Latihan (April 2017 - Julai 2024), 20% Ujian (Sept 2024 - April 2026)

n_total <- nrow(df_final)           # 459
n_train <- round(0.80 * n_total)    

# Pembahagian mengikut indeks kronologi
train_df <- df_final[1:n_train, ]
test_df  <- df_final[(n_train + 1):n_total, ]

cat("\nSaiz Data Latihan (80%):", nrow(train_df)) 
cat("\nSaiz Data Ujian (20%)  :", nrow(test_df), "\n") 

# Semak tarikh mula dan akhir bagi setiap himpunan data
cat("\nTarikh Data Latihan :", format(min(train_df$date)), "hingga", format(max(train_df$date)))
cat("\nTarikh Data Ujian   :", format(min(test_df$date)), "hingga", format(max(test_df$date)), "\n")



# train_df <- df_final %>% filter(date <= as.Date("2024-08-31"))
# test_df  <- df_final %>% filter(date >= as.Date("2024-09-01"))

# cat("\nSaiz Data Latihan (80%):", nrow(train_df))
# cat("\nSaiz Data Ujian (20%)  :", nrow(test_df), "\n")

# Sediakan Objek Siri Masa (ts) dan Matriks Eksogen
y_train <- ts(train_df$price_ron97, frequency = 52)
x_train <- as.matrix(train_df %>% select(price_brent, usd_myr))

y_test  <- ts(test_df$price_ron97, frequency = 52)
x_test  <- as.matrix(test_df %>% select(price_brent, usd_myr))

# --- Plot ACF & PACF untuk Penentuan Parameter (p, q) ---
par(mfrow=c(1,2))
Acf(diff(y_train), main="ACF (Data Latihan Differenced)")
Pacf(diff(y_train), main="PACF (Data Latihan Differenced)")
par(mfrow=c(1,1))

# --- A. Pembangunan Model ARIMA(p,d,q) ---
cat("\n=== MODEL ARIMA TERBAIK (Berdasarkan AIC/BIC) ===\n")
fit_arima <- auto.arima(y_train, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(fit_arima)

# Diagnostik Residual Model ARIMA
checkresiduals(fit_arima)

# --- B. Pembangunan Model ARIMAX(p,d,q) + Eksogen ---
cat("\n=== MODEL ARIMAX TERBAIK (Berdasarkan AIC/BIC) ===\n")
fit_arimax <- auto.arima(y_train, xreg = x_train, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(fit_arimax)

# Diagnostik Residual Model ARIMAX
checkresiduals(fit_arimax)


# ==============================================================================
# SEKSYEN 3.5: PENILAIAN DAN PERBANDINGAN PRESTASI MODEL
# ==============================================================================

h_test <- nrow(test_df)
h_test

# Penghasilan Ramalan di Luar Sampel (Out-of-sample Forecast)
fc_arima  <- forecast(fit_arima, h = h_test)
fc_arimax <- forecast(fit_arimax, xreg = x_test, h = h_test)

# 2. Ekstrak Nilai Sebenar & Nilai Ramalan (sebagai Vektor Nombor)
y_true     <- test_df$price_ron97
pred_arima  <- as.numeric(fc_arima$mean)
pred_arimax <- as.numeric(fc_arimax$mean)

# 3. Fungsi Pengiraan Metrik Ralat Matematik (Sama seperti Jadual 3.1 Proposal)
calc_metrics <- function(actual, pred) {
  error <- actual - pred
  mae   <- mean(abs(error))
  rmse  <- sqrt(mean(error^2))
  mape  <- mean(abs(error / actual)) * 100
  return(c(MAE = mae, RMSE = rmse, MAPE = mape))
}

# 4. Pengiraan Prestasi
m_arima  <- calc_metrics(y_true, pred_arima)
m_arimax <- calc_metrics(y_true, pred_arimax)

# 5. Pembinaan Jadual Keputusan Prestasi
jadual_prestasi <- data.frame(
  Model = c("ARIMA", "ARIMAX"),
  MAE   = c(m_arima["MAE"],   m_arimax["MAE"]),
  RMSE  = c(m_arima["RMSE"],  m_arimax["RMSE"]),
  MAPE  = c(m_arima["MAPE"],  m_arimax["MAPE"])
)

print("=== JADUAL PERBANDINGAN PRESTASI MODEL (DATA UJIAN) ===")
print(jadual_prestasi)


# Pengiraan Metrik Prestasi (MAE, RMSE, MAPE) pada Data Ujian
# acc_arima  <- accuracy(fc_arima, y_test)
# acc_arimax <- accuracy(fc_arimax, y_test)

# Pembinaan Jadual 3.1 Penilaian Prestasi
#jadual_prestasi <- data.frame(
 # Model = c("ARIMA", "ARIMAX"),
  #MAE   = c(acc_arima["Test set", "MAE"],   acc_arimax["Test set", "MAE"]),
  #RMSE  = c(acc_arima["Test set", "RMSE"],  acc_arimax["Test set", "RMSE"]),
  #MAPE  = c(acc_arima["Test set", "MAPE"],  acc_arimax["Test set", "MAPE"])
#)

#cat("\n=== JADUAL PERBANDINGAN PRESTASI MODEL (DATA UJIAN) ===\n")
#print(jadual_prestasi)

# Plot Visual Perbandingan Ramalan vs Nilai Sebenar
test_df <- test_df %>%
  mutate(
    Pred_ARIMA  = as.numeric(fc_arima$mean),
    Pred_ARIMAX = as.numeric(fc_arimax$mean)
  )

ggplot(test_df, aes(x = date)) +
  geom_line(aes(y = price_ron97, color = "Harga Sebenar"), size = 1) +
  geom_line(aes(y = Pred_ARIMA,  color = "Ramalan ARIMA"), size = 0.8, linetype = "dashed") +
  geom_line(aes(y = Pred_ARIMAX, color = "Ramalan ARIMAX"), size = 0.8, linetype = "solid") +
  labs(
    title = "Perbandingan Prestasi Ramalan RON97 pada Data Ujian",
    subtitle = "ARIMA vs ARIMAX berbanding Harga Sebenar (Sept 2024 - April 2026)",
    x = "Tarikh", y = "Harga (RM / Liter)", color = "Petunjuk"
  ) +
  scale_color_manual(values = c("Harga Sebenar" = "#2c3e50", "Ramalan ARIMA" = "#e74c3c", "Ramalan ARIMAX" = "#2ecc71")) +
  theme_minimal()





# BAWAH TEST TEST

# ==============================================================================
# FASA 3 & 4: PEMBANGUNAN MODEL ARIMA & ARIMAX SERTA PENILAIAN PRESTASI
# ==============================================================================
library(tidyverse)
library(tseries)
library(forecast)

# ------------------------------------------------------------------------------
# 1. UJIAN KEPEGUNAN (ADF & KPSS TEST)
# ------------------------------------------------------------------------------
cat("\n=== 1. UJIAN KEPEGUNAN PADA ARAS (LEVEL) ===\n")
# Null Hypothesis (H0) ADF: Data tidak pegun (ada unit root)
# Null Hypothesis (H0) KPSS: Data adalah pegun
print(adf.test(df_final$price_ron97))
print(kpss.test(df_final$price_ron97))

cat("\n=== 2. UJIAN KEPEGUNAN PADA PERBEZAAN PERTAMA (1ST DIFFERENCE, d=1) ===\n")
ron97_diff <- diff(df_final$price_ron97)
print(adf.test(ron97_diff))
print(kpss.test(ron97_diff))

# Plot ACF dan PACF bagi data perbezaan pertama untuk anggaran p dan q
par(mfrow=c(1,2))
Acf(ron97_diff, main="ACF (Difference = 1)")
Pacf(ron97_diff, main="PACF (Difference = 1)")
par(mfrow=c(1,1))

# ------------------------------------------------------------------------------
# 2. PEMBAHAGIAN DATASET (80% LATIHAN vs 20% UJIAN)
# ------------------------------------------------------------------------------
n_total <- nrow(df_final)
n_train <- round(0.80 * n_total) # ~367 pemerhatian latihan

train_df <- df_final[1:n_train, ]
test_df  <- df_final[(n_train + 1):n_total, ]

cat("\nSaiz Data Latihan (Train):", nrow(train_df))
cat("\nSaiz Data Ujian (Test)    :", nrow(test_df), "\n")

# Sediakan siri masa dan matriks eksogen untuk data latihan
y_train <- ts(train_df$price_ron97, frequency = 52)
x_train <- as.matrix(train_df %>% select(price_brent, usd_myr))

# Sediakan siri masa dan matriks eksogen untuk data ujian
y_test  <- ts(test_df$price_ron97, frequency = 52)
x_test  <- as.matrix(test_df %>% select(price_brent, usd_myr))

# ------------------------------------------------------------------------------
# 3. PEMBANGUNAN MODEL ARIMA (Model Univariat)
# ------------------------------------------------------------------------------
cat("\n=== 3. PEMODELAN ARIMA (AUTOMATIK & MANUAL) ===\n")
# auto.arima menentukan order (p,d,q) berasaskan AICc terkecil
fit_arima <- auto.arima(y_train, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(fit_arima)

# ------------------------------------------------------------------------------
# 4. PEMBANGUNAN MODEL ARIMAX (Model bersama Eksogen: Brent & USD/MYR)
# ------------------------------------------------------------------------------
cat("\n=== 4. PEMODELAN ARIMAX ===\n")
fit_arimax <- auto.arima(y_train, xreg = x_train, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(fit_arimax)

# ------------------------------------------------------------------------------
# 5. RAMALAN & PERBANDINGAN PRESTASI PADA DATA UJIAN (20% TEST SET)
# ------------------------------------------------------------------------------
h_test <- nrow(test_df) # Bilangan langkah ramalan mengikut saiz data ujian

# Ramalan ARIMA
fc_arima <- forecast(fit_arima, h = h_test)

# Ramalan ARIMAX (Memerlukan nilai sebenar eksogen pada tempoh ujian)
fc_arimax <- forecast(fit_arimax, xreg = x_test, h = h_test)

# Pengiraan Metrik Ralat Ramalan (MAE, MAPE, RMSE)
acc_arima  <- accuracy(fc_arima, y_test)
acc_arimax <- accuracy(fc_arimax, y_test)

# Ekstrak metrik untuk Data Ujian (Test Set) sahaja
metrics_summary <- data.frame(
  Model = c("ARIMA", "ARIMAX"),
  RMSE  = c(acc_arima["Test set", "RMSE"],  acc_arimax["Test set", "RMSE"]),
  MAE   = c(acc_arima["Test set", "MAE"],   acc_arimax["Test set", "MAE"]),
  MAPE  = c(acc_arima["Test set", "MAPE"],  acc_arimax["Test set", "MAPE"])
)

cat("\n=== 5. JADUAL PERBANDINGAN PRESTASI RAMALAN (DATA UJIAN) ===\n")
print(metrics_summary)

# ------------------------------------------------------------------------------
# 6. PLOT PERBANDINGAN RAMALAN ARIMA vs ARIMAX vs HARGA SEBENAR
# ------------------------------------------------------------------------------
test_df <- test_df %>%
  mutate(
    Pred_ARIMA  = as.numeric(fc_arima$mean),
    Pred_ARIMAX = as.numeric(fc_arimax$mean)
  )

ggplot(test_df, aes(x = date)) +
  geom_line(aes(y = price_ron97, color = "Harga Sebenar"), size = 1) +
  geom_line(aes(y = Pred_ARIMA,  color = "Ramalan ARIMA"), size = 0.8, linetype = "dashed") +
  geom_line(aes(y = Pred_ARIMAX, color = "Ramalan ARIMAX"), size = 0.8, linetype = "solid") +
  labs(
    title = "Perbandingan Prestasi Ramalan RON97 pada Data Ujian",
    subtitle = "ARIMA vs ARIMAX berbanding Harga Sebenar (September 2024 - April 2026)",
    x = "Tarikh",
    y = "Harga (RM / Liter)",
    color = "Petunjuk"
  ) +
  scale_color_manual(values = c("Harga Sebenar" = "#2c3e50", "Ramalan ARIMA" = "#e74c3c", "Ramalan ARIMAX" = "#2ecc71")) +
  theme_minimal()



# BAWAH NI TRIAL JE
# Tukar nama lajur kepada format yang mudah
brent_clean <- brent %>%
  transmute(
    date = if (inherits(Date, "Date")) {
      as.Date(Date)
    } else {
      as.Date(mdy(as.character(Date)))
    },
    brent = parse_number(as.character(Price))
  ) %>%
  arrange(date)

# Semak data
head(brent_clean)
tail(brent_clean)

# Semak julat tarikh
range(brent_clean$date, na.rm = TRUE)

# Semak nilai hilang
colSums(is.na(brent_clean))

# Semak tarikh pendua
sum(duplicated(brent_clean$date))

ron97 <- read_csv("C:/Users/User/Downloads/fyp/fuelprice.csv") %>%
  select(date, ron97) %>%
  mutate(date = dmy(date)) %>%          # format dd/mm/yyyy
  rename(price_ron97 = ron97)

# --- 1b. Brent Crude Oil (Harian) ---
brent <- read_csv("C:/Users/User/Downloads/fyp/Crude Oil WTI Futures Historical Data latest.csv") %>%
  select(Date, Price) %>%
  mutate(Date = mdy(Date)) %>%          # format mm/dd/yyyy
  rename(date = Date,
         price_brent = Price)

# --- 1c. USD/MYR (Harian) ---
usdmyr <- read_csv("C:/Users/User/Downloads/fyp/daily_0900.csv") %>%
  select(1, USD) %>%
  rename(date = 1, rate_usdmyr = USD) %>%
  mutate(date = as.Date(date, format = "%d-%b-%y"))  # format d-Mon-YY

# ============================================================
# STEP 2: FILTER TEMPOH KAJIAN (30 MAC 2017 - 30 APRIL 2026)
# ============================================================

start_date <- as.Date("2017-03-30")
end_date   <- as.Date("2026-04-30")

ron97 <- ron97 %>%
  filter(date >= start_date & date <= end_date)

brent <- brent %>%
  filter(date >= start_date & date <= end_date)

usdmyr <- usdmyr %>%
  filter(date >= start_date & date <= end_date)

# ============================================================
# STEP 3: AGGREGATE BRENT & USD/MYR — HARIAN KE MINGGUAN
# Minggu didefinisikan Jumaat ke Khamis (ikut kitaran APM)
# ============================================================

# Fungsi untuk label minggu APM (Jumaat = mula minggu baru)
get_apm_week <- function(date) {
  # Jumaat = wday 6 dalam R (Ahad=1)
  # Shift supaya Jumaat jadi hari pertama minggu
  floor_date(date - days(5), unit = "week") + days(5)
}

# Aggregate Brent
brent_weekly <- brent %>%
  mutate(week_start = get_apm_week(date)) %>%
  group_by(week_start) %>%
  summarise(price_brent = mean(price_brent, na.rm = TRUE),
            .groups = "drop") %>%
  rename(date = week_start)

# Aggregate USD/MYR
usdmyr_weekly <- usdmyr %>%
  mutate(week_start = get_apm_week(date)) %>%
  group_by(week_start) %>%
  summarise(rate_usdmyr = mean(rate_usdmyr, na.rm = TRUE),
            .groups = "drop") %>%
  rename(date = week_start)

# ============================================================
# STEP 4: JOIN KETIGA-TIGA DATASET
# ============================================================

data_combined <- ron97 %>%
  left_join(brent_weekly, by = "date") %>%
  left_join(usdmyr_weekly, by = "date")

# ============================================================
# STEP 5: SEMAK HASIL
# ============================================================

# Semak struktur
glimpse(data_combined)

# Semak 6 baris pertama
head(data_combined)

# Semak nilai hilang
colSums(is.na(data_combined))

# Semak julat tarikh
cat("Tarikh mula:", as.character(min(data_combined$date)), "\n")
cat("Tarikh akhir:", as.character(max(data_combined$date)), "\n")
cat("Jumlah pemerhatian:", nrow(data_combined), "\n")