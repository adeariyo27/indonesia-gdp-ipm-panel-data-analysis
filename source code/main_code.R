# -------------------------------------------------------------------
# BAGIAN 0: PERSIAPAN (SETUP)
# -------------------------------------------------------------------
library(tidyverse)    # Manipulasi data (dplyr, ggplot2, readr)
library(plm)          # Library utama untuk Regresi Data Panel
library(psych)        # Statistik deskriptif (describeBy)
library(lmtest)       # Uji asumsi (bptest, pbgtest, coeftest)
library(car)          # Uji Multikolinearitas (vif)
library(RColorBrewer) # Mewarnai ulang lines

setwd("D:/Magister/Kuliah/Pemrograman Statistika/Project Akhir/data/clean/")

# Nonaktifkan notasi ilmiah
options(scipen = 999)

df <- read_csv("data_long_cleaned.csv")
glimpse(df)
view(df)

# -------------------------------------------------------------------
# BAGIAN 1: ANALISIS DATA EKSPLORATIF (EDA)
# -------------------------------------------------------------------

# 1.1 Statistik Deskriptif
print(describeBy(df$PDRB_Growth, group = df$Provinsi))
print(describeBy(df$PDRB_Growth, group = df$Tahun))

# 1.2 Matriks Korelasi
vars_for_cor <- df %>% 
  select(PDRB_Growth, UHH, HLS, RLS, Pengeluaran)
cor_matrix <- cor(vars_for_cor, use = "complete.obs")
print("Matriks Korelasi:")
print(round(cor_matrix, 2))

# 1.3 Visualisasi Tren PDRD Top 10 Provinsi
top10 <- df %>%
  group_by(Provinsi) %>%
  summarise(mean_growth = mean(PDRB_Growth, na.rm = TRUE)) %>%
  arrange(desc(mean_growth)) %>%
  slice(1:10) %>%
  pull(Provinsi)

df_mod <- df %>%
  mutate(Provinsi_mod = ifelse(Provinsi %in% top10, Provinsi, "PROVINSI LAINNYA"))

df_avg <- df_mod %>%
  filter(Provinsi_mod == "PROVINSI LAINNYA") %>%
  group_by(Tahun, Provinsi_mod) %>%
  summarise(PDRB_Growth = mean(PDRB_Growth, na.rm = TRUE), .groups = "drop")

df_final <- df_mod %>%
  filter(Provinsi_mod != "PROVINSI LAINNYA") %>%
  select(Tahun, Provinsi_mod, PDRB_Growth) %>%
  bind_rows(df_avg)

ggplot(df_final, aes(x = Tahun, y = PDRB_Growth, colour = Provinsi_mod)) +
  geom_line(size = 1.2) +  # garis agak tebal
  ggtitle("Grafik Tren PDRB Growth per Provinsi (2019-2024)") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "right")


# -------------------------------------------------------------------
# BAGIAN 2: ESTIMASI TIGA MODEL PANEL UTAMA
# -------------------------------------------------------------------

# 2.1 Model Common Effect (Pooled OLS)
common <- plm(PDRB_Growth ~ UHH + HLS + RLS + Pengeluaran, 
              data = df, 
              index = c("Provinsi", "Tahun"), 
              model = "pooling")

# 2.2 Model Fixed Effect (FEM)
fixed <- plm(PDRB_Growth ~ UHH + HLS + RLS + Pengeluaran, 
             data = df, 
             index = c("Provinsi", "Tahun"), 
             model = "within")

# 2.3 Model Random Effect (REM)
random <- plm(PDRB_Growth ~ UHH + HLS + RLS + Pengeluaran, 
              data = df, 
              index = c("Provinsi", "Tahun"), 
              model = "random")


# -------------------------------------------------------------------
# BAGIAN 3: PEMILIHAN MODEL (UJI SPESIFIKASI)
# -------------------------------------------------------------------


# 3.1 Uji Chow (Fixed vs Common)
# H0: Common Effect (Pooled OLS) lebih baik; H1: Fixed Effect lebih baik.
print("--- Uji Chow (Fixed vs Common) ---")
print(pFtest(fixed, common))
# Kesimpulan: Jika p-value < 0.05, pilih Fixed Effect.

# 3.2 Uji Hausman (Fixed vs Random)
# H0: Random Effect lebih baik (efek individu tidak berkorelasi dg prediktor); H1: Fixed Effect lebih baik (efek individu berkorelasi dg prediktor).
print("--- Uji Hausman (Fixed vs Random) ---")
print(phtest(fixed, random))
# Kesimpulan: Jika p-value < 0.05, pilih Fixed Effect.


# -------------------------------------------------------------------
# BAGIAN 4: UJI ASUMSI KLASIK (DIAGNOSTIK MODEL 'fixed')
# -------------------------------------------------------------------

# 4.1 Uji Multikolinearitas (VIF)
pooled_lm <- lm(PDRB_Growth ~ UHH + HLS + RLS + Pengeluaran, data = df)
vif_results <- car::vif(pooled_lm)
print("--- Uji Multikolinearitas (VIF) ---")
print(vif_results)
# Aturan umum: Jika nilai VIF < 5 (atau 10), asumsi aman (Tidak ada multikol).

# 4.2 Uji Homokedastisitas (Breusch-Pagan)
# H0: Tidak ada heteroskedastisitas (Homoskedastisitas / Varians error konstan); H1: Ada heteroskedastisitas.
print("--- Uji Heteroskedastisitas (Breusch-Pagan) ---")
print(lmtest::bptest(fixed))
# Kesimpulan: Jika p-value < 0.05, model GAGAL (terkena Hetero).

# 4.3 Uji Autokorelasi (Breusch-Godfrey/Wooldridge)
# H0: Tidak ada autokorelasi serial; H1: Ada autokorelasi serial.
print("--- Uji Autokorelasi (Wooldridge) ---")
print(pbgtest(fixed))
# Kesimpulan: Jika p-value < 0.05, model GAGAL (terkena Auto).

# 4.4 Uji Normalitas Residual
# H0: Residual terdistribusi normal; H1: Residual tidak terdistribusi normal.
print("--- Uji Normalitas (Shapiro-Wilk) ---")
print(shapiro.test(residuals(fixed)))
# Kesimpulan: Jika p-value < 0.05, model GAGAL (tidak normal).

# 4.5 Uji Cross-sectional Dependence
# H0: Tidak ada cross-sectional dependence; H1: Ada cross-sectional dependence.
print("--- Uji Cross-sectional Dependence (Pesaran CD) ---")
print(pcdtest(fixed, test = "cd"))
# Kesimpulan: Jika p-value < 0.05, ada ketergantungan antar provinsi.


# -------------------------------------------------------------------
# BAGIAN 5: VISUALISASI Z-SCORE 
# -------------------------------------------------------------------

# Plot ini sangat penting untuk Menjelaskan mengapa R-squared rendah
# dan mengapa asumsi-asumsi gagal.

vars_to_plot <- c("PDRB_Growth", "HLS", "RLS", "UHH", "Pengeluaran")

data_avg_trend <- df %>%
  # Hitung rata-rata nasional per tahun
  group_by(Tahun) %>%
  summarise(across(all_of(vars_to_plot), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  # Normalisasi Z-Score
  mutate(across(all_of(vars_to_plot), ~ as.numeric(scale(.x)))) %>%
  # Ubah format dari 'wide' ke 'long' agar mudah di-plot
  pivot_longer(
    cols = all_of(vars_to_plot),
    names_to = "Variabel",
    values_to = "Z_Score"
  )

# Buat plot tren
plot_tren <- ggplot(data_avg_trend, aes(x = Tahun, y = Z_Score, color = Variabel, group = Variabel)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Tren Rata-Rata Nasional (Ternormalisasi)",
    subtitle = "Variabel IPM vs Pertumbuhan PDRB (2019-2024)",
    x = "Tahun",
    y = "Z-Score (Standar Deviasi dari Rata-rata)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Tampilkan plot tren
print(plot_tren + 
        scale_x_continuous(breaks = seq(2015, 2024, by = 1)))


# -------------------------------------------------------------------
# BAGIAN 6: INTERPRETASI MODEL
# -------------------------------------------------------------------

summary(fixed)
