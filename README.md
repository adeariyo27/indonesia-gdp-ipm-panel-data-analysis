![](assets/images/banner.png)


### 📘 **Pendahuluan**

---

Proyek ini bertujuan untuk menganalisis pengaruh empat komponen Indeks Pembangunan Manusia (IPM) terhadap laju pertumbuhan ekonomi di tingkat provinsi di Indonesia. Analisis ini menggunakan data series waktu dari tahun 2019 hingga 2024.

### 📖 **Latar Belakang**

---

Dalam konteks pembangunan suatu wilayah, terdapat dua indikator utama yang sering menjadi sorotan: Produk Domestik Regional Bruto (PDRB) sebagai cerminan pertumbuhan ekonomi, dan Indeks Pembangunan Manusia (IPM) yang mengukur kemajuan dari aspek kualitas hidup. Secara teoritis, keduanya memiliki hubungan dua arah dalam jangka panjang: PDRB yang tinggi membiayai kesehatan dan pendidikan (IPM), dan IPM yang tinggi (tenaga kerja terdidik dan sehat) akan mendorong PDRB.

Proyek ini bertujuan untuk mengeksplorasi secara lebih mendalam apakah terdapat hubungan positif dan saling memengaruhi antara IPM dan PDRB, khususnya dalam konteks provinsi di Indonesia selama periode 2019-2024. Proyek ini juga mencoba menjawab hal tersebut secara kuantitatif dengan mengukur efektivitas komponen IPM dalam mendorong laju PDRB, terutama setelah "dihantam" oleh pandemi 2019. Dengan menganalisis data pada periode tersebut, kami mencoba membedah apakah fondasi IPM terbukti signifikan melindungi laju PDRB, atau apakah hubungan keduanya secara riil tidak nyata.

### 🎯 **Tujuan Proyek**

---

1.  Mengetahui statistik deskriptif dari empat komponen IPM dan laju pertumbuhan ekonomi (PDRB).
2.  Menganalisis hubungan dan pengaruh antara IPM dan laju pertumbuhan ekonomi (PDRB).
3.  Mengidentifikasi tren dan pola pengaruh antarvariabel dari tahun 2019-2024.

### 📊 **Data & Variabel**

---

<div align="center">
  <img src="assets/images/bps.png" width="150" align="center">
</div>

##### 🔢 *Variabel Prediktor (X)*
* Umur Harapan Hidup (UHH)
* Harapan Lama Sekolah (HLS)
* Rata-Rata Lama Sekolah (RLS)
* Pengeluaran Per Kapita Disesuaikan

##### 📈 *Variabel Target (Y)*
* Laju Pertumbuhan Produk Domestik Regional Bruto Atas Dasar Harga Konstan (persen)

### 🧭 **Metodologi**

---

Proyek ini menggunakan dua pendekatan utama:

1. **Persiapan (*Data Wrangling*)**: tahap awal untuk membersihkan, merapikan, dan menyusun data lintas-waktu dan lintas-provinsi agar siap dianalisis. Termasuk penanganan missing value, konsistensi format, serta transformasi variabel bila diperlukan.

2. **Analisis Data Eksploratif**: eksplorasi awal untuk memahami pola, tren, dan distribusi variabel. Visualisasi dan statistik ringkas digunakan untuk mengidentifikasi karakteristik utama serta potensi hubungan antarvariabel.

3. **Pemilihan Model (Uji Spesifikasi)**: penentuan model panel yang paling sesuai (misalnya *Fixed Effect* atau *Random Effect*) melalui uji spesifikasi. Tahap ini memastikan model yang dipilih mampu menangkap variasi antar-provinsi dan antar-waktu secara tepat.

4. **Uji Asumsi Klasik**: pengujian asumsi dasar regresi (seperti multikolinearitas, heteroskedastisitas, autokorelasi, dan normalitas residual) untuk menjamin validitas hasil estimasi.

5. **Visualisasi Tren Ternormalisasi (Z-Score)**: pembuatan plot garis ternormalisasi (Z-Score) untuk membandingkan pola pergerakan semua variabel dalam satu grafik secara visual.

6. **Interpretasi Model**: summary model untuk menarik kesimpulan yang berfokus pada:

- *Adjusted R-squared**: seberapa kuat model menjelaskan daya beli.

- **F-statistic p-value**: apakah model signifikan secara simultan.

- **Coefficients (Estimate & p-value)**: variabel X mana yang signifikan secara parsial dan bagaimana arah pengaruhnya (positif/negatif).

### 🪜 **Tahapan**

---

#### **1. 🧹 Persiapan (Data Wrangling)**

**Import Library**

```{r0}
library(tidyverse)
library(plm)
library(psych)
library(lmtest)
library(car)
library(RColorBrewer)
```

Pertama, dilakukan inisialisasi lingkungan kerja agar semua fungsi siap digunakan dengan cara memuat pustaka utama yang dibutuhkan untuk analisis statistik, regresi panel, visualisasi, serta pembersihan data atau biasa disebut tahap.

**Pembersihan & Transformasi Data**

```{r1}
file_list <- dir_ls(glob = "*_provinsi_clean.csv")

print(paste("Ditemukan", length(file_list), "file data:"))
print(file_list)
read_and_clean_col <- function(filepath) {
  df <- read_csv(filepath, show_col_types = FALSE)
  old_name <- names(df)[2]
  tahun <- str_extract(old_name, "\\d{4}")
  new_name <- case_when(
    str_detect(old_name, "LajuPertumbuhan|Laju_Pertumbuhan") ~ paste0("PDRB_Growth_", tahun),
    str_detect(old_name, "PengeluaranPerKapita") ~ paste0("Pengeluaran_", tahun),
    str_detect(old_name, "HLS") ~ paste0("HLS_", tahun),
    str_detect(old_name, "RLS") ~ paste0("RLS_", tahun),
    str_detect(old_name, "UHH") ~ paste0("UHH_", tahun),
    TRUE ~ old_name 
  )
  names(df)[2] <- new_name
  return(df)
}

data_wide_cleaned <- map(file_list, read_and_clean_col) %>%
  reduce(left_join, by = "Wilayah")

print("--- Data Wide ---")
glimpse(data_wide_cleaned)
view(data_wide_cleaned)
write_csv(data_wide_cleaned, "data_wide_cleaned.csv")

data_long <- data_wide_cleaned %>%
  pivot_longer(
    cols = -Wilayah,
    names_to = c(".value", "Tahun"),
    names_sep = "_(?=\\d{4})" 
  ) %>%
  mutate(Tahun = as.numeric(Tahun)) 

data_full <- data_long %>%
  rename(Provinsi = Wilayah) %>%
  filter(Tahun >= 2019 & Tahun <= 2024) %>%
  filter(!Provinsi %in% c("PAPUA TENGAH", "PAPUA PEGUNUNGAN", 
                          "PAPUA SELATAN", "PAPUA BARAT DAYA")) %>%
  arrange(Provinsi, Tahun)

print("--- Data Panel Final ---")
glimpse(data_full)
view(data_full)
print(paste("Total Baris Data:", nrow(data_full)))
write_csv(data_full, "data_long_cleaned.csv")
```

Kemudian, dilakukan **standarisasi** dan **penggabungan** seluruh file data provinsi menjadi satu dataset panel yang konsisten. Proses ini mencakup **penamaan ulang variabel agar seragam**, transformasi dari **format wide ke long**, serta penyaringan periode (2019–2024) dan wilayah yang relevan. Hasil akhirnya adalah dataset panel bersih yang siap dipakai untuk analisis deskriptif maupun regresi panel.

#### **2. 🔍 Analisis Data Eksploratif**

**Statistik Deskriptif**

```{r3}
print(describeBy(df$PDRB_Growth, group = df$Provinsi))
print(describeBy(df$PDRB_Growth, group = df$Tahun))
```
Pada tahap ini, statistik deskriptif dihitung untuk menggambarkan karakteristik dasar pertumbuhan PDRB, baik menurut provinsi maupun menurut tahun.

**Matriks Korelasi**

```{r4}
vars_for_cor <- df %>% 
  select(PDRB_Growth, UHH, HLS, RLS, Pengeluaran)
cor_matrix <- cor(vars_for_cor, use = "complete.obs")
print("Matriks Korelasi:")
print(round(cor_matrix, 2))
```

<img src="assets/images/1.2 Matriks Korelasi.png" width="500">

Pada tahap ini, hubungan antarvariabel utama dianalisis melalui matriks korelasi sehingga pola keterkaitan antarindikator dapat diidentifikasi. Hasilnya: 

* PDRB_Growth: Variabel Y ini memiliki **korelasi yang sangat lemah** (hampir nol) dengan semua variabel lainnya. Nilai korelasinya berkisar antara -0.08 hingga 0.11.
* Variabel Lain: Terdapat korelasi positif yang cukup kuat (moderat) di antara variabel X: RLS, UHH, dan Pengeluaran. (*Pengeluaran vs UHH = 0.61, Pengeluaran vs RLS = 0.59, RLS vs UHH = 0.42, RLS VS HLS = 0.49*)
* Daya Prediksi Rendah: Korelasi yang sangat lemah antara variabel X dan Y (PDRB_Growth) menunjukkan bahwa variabel-variabel tersebut **bukan prediktor linear yang baik**. Model regresi linear yang dihasilkan kemungkinan besar **tidak akan signifikan** atau memiliki **$R^2$ yang sangat rendah**.
* Potensi Multikolinearitas: Adanya korelasi yang cukup kuat di antara variabel-variabel independen (misalnya UHH dan Pengeluaran = 0.61) mengindikasikan adanya **potensi masalah multikolinearitas**. Ini dapat membuat koefisien regresi **tidak stabil** dan **sulit untuk diinterpretasikan**.

**Visualisasi Tren PDRB Top 10 Provinsi**


```{r5}
top10 <- df %>%
  group_by(Provinsi) %>%
  summarise(mean_growth = mean(PDRB_Growth, na.rm = TRUE)) %>%
  arrange(desc(mean_growth)) %>%
  slice(1:10) %>%
  pull(Provinsi)

df_mod <- df %>%
  mutate(Provinsi_mod = ifelse(Provinsi %in% top10, Provinsi, "Lainnya"))

df_avg <- df_mod %>%
  filter(Provinsi_mod == "Lainnya") %>%
  group_by(Tahun, Provinsi_mod) %>%
  summarise(PDRB_Growth = mean(PDRB_Growth, na.rm = TRUE), .groups = "drop")

df_final <- df_mod %>%
  filter(Provinsi_mod != "Lainnya") %>%
  select(Tahun, Provinsi_mod, PDRB_Growth) %>%
  bind_rows(df_avg)

ggplot(df_final, aes(x = Tahun, y = PDRB_Growth, colour = Provinsi_mod)) +
  geom_line(size = 1.2) +  # garis agak tebal
  ggtitle("Grafik Tren PDRB Growth per Provinsi (2019-2024)") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "right")
```

<div align="center">
  <img src="assets/images/1.3 Visualisasi Tren PDRB Top 10 Provinsi.png" width="600" height="450" align="center"> 
</div>

Pada tahap ini, tren pertumbuhan PDRB divisualisasikan dengan menampilkan 10 provinsi teratas secara individual, sedangkan provinsi lainnya digabungkan ke dalam satu garis rata-rata untuk menjaga keterbacaan grafik.

#### **3. 🧩 Pemilihan Model (Uji Spesifikasi)**

**Estimasi Tiga Model Panel Utama**

```{r6}
common <- plm(PDRB_Growth ~ UHH + HLS + RLS + Pengeluaran, 
              data = df, 
              index = c("Provinsi", "Tahun"), 
              model = "pooling")

fixed <- plm(PDRB_Growth ~ UHH + HLS + RLS + Pengeluaran, 
             data = df, 
             index = c("Provinsi", "Tahun"), 
             model = "within")

random <- plm(PDRB_Growth ~ UHH + HLS + RLS + Pengeluaran, 
              data = df, 
              index = c("Provinsi", "Tahun"), 
              model = "random")
```

Pada tahap ini, beberapa pendekatan regresi data panel dijalankan untuk menentukan model yang paling sesuai:
- Pooled OLS (*common*)
Model ini diperlakukan seolah-olah data gabungan lintas-provinsi dan lintas-waktu adalah satu kesatuan tanpa membedakan efek individual.
- Fixed Effect (*within*)
Model ini memperhitungkan perbedaan karakteristik tetap antarprovinsi dengan menghilangkan variasi yang bersifat konstan, sehingga fokus diberikan pada variasi dalam suatu provinsi sepanjang waktu.
- Random Effect (*random*)
Model ini mengasumsikan bahwa variasi antarprovinsi bersifat acak dan tidak berkorelasi dengan variabel penjelas, sehingga memungkinkan adanya efek individual sekaligus efisiensi estimasi.

**Uji Chow (Fixed vs Common)**

```{r7}
print("--- Uji Chow (Fixed vs Common) ---")
print(pFtest(fixed, common)
```

<img src="assets/images/3.1 Uji Chow.png" width="500">

Uji Chow ini bertugas sebagai **juri** untuk memutuskan satu hal:

"Apakah harus memperlakukan semua provinsi sebagai satu kelompok yang sama, atau setiap provinsi itu unik dan harus diperlakukan berbeda?"

- Model *common* (Gabungan): Menganggap semua provinsi itu **sama**. Data dari Aceh sampai Papua digabung jadi satu.
- Model *fixed* (Pengaruh Tetap): Menganggap setiap provinsi itu **unik**. Ada karakteristik khusus (misal: budaya, geografi, kebijakan lokal) yang membedakan mereka.

Kesimpulan: hasil p-value adalah **0.000003161**.

Nilai ini sangat kecil, jauh di bawah taraf nyata umum (0.05), berarti **tolak model yang sederhana** (*common*) dan **menerima model yang lebih kompleks** (*fixed*).

**Uji Hausman (Fixed vs Random)**

```{r8}
print("--- Uji Hausman (Fixed vs Random) ---")
print(phtest(fixed, random))
```

<img src="assets/images/3.2 Uji Hausman.png" width="500">

Setelah Uji Chow (juri pertama) menyingkirkan model *common* (gabungan), Uji Hausman adalah **juri final**. Tugasnya adalah memilih satu pemenang di antara dua model yang tersisa:

- Model *random* (Pengaruh Acak): Model ini berasumsi **keunikan tiap provinsi itu ada**, tapi sifatnya *random* (acak) dan tidak ada hubungannya dengan variabel prediktor (HLS, RLS, UHH, Pengeluaran).
- Model *fixed* (Pengaruh Tetap): Model ini berasumsi **keunikan tiap provinsi itu sangat penting** dan berkaitan erat dengan variabel prediktor.

Kesimpulan: hasil p-value adalah **0.000002341**.

Nilai ini sangat kecil, jauh di bawah taraf nyata umum (0.05), berarti **tolak model model *random* **(yang lebih sederhana) dan **menerima model yang lebih kompleks (*fixed*)**.

Kedua uji ini dengan yakin memberi tahu bahwa keunikan tiap provinsi berkorelasi atau berkaitan erat dengan variabel-variabel prediktor. Oleh karena itu, **Model *Fixed Effect* (Pengaruh Tetap)** adalah model yang tepat dipilih untuk pemodelan dalam analisis ini.

#### **4. 📐 Uji Asumsi Klasik**

**Uji Multikolinearitas (VIF)**

```{r9}
pooled_lm <- lm(PDRB_Growth ~ UHH + HLS + RLS + Pengeluaran, data = df)
vif_results <- car::vif(pooled_lm)
print("--- Uji Multikolinearitas (VIF) ---")
print(vif_results)
```

<img src="assets/images/4.1 Uji Multikolinearitas.png" width="450">

Kesimpulan: Semua nilai VIF jauh di bawah 5. Ini berarti **tidak ada masalah Multikolinearitas**. Variabel-variabel prediktor (UHH, HLS, dll.) tidak tumpang tindih secara berlebihan.

**Uji Heteroskedastisitas (Breusch-Pagan)**

```{r10}
print("--- Uji Heteroskedastisitas (Breusch-Pagan) ---")
print(lmtest::bptest(fixed))
```

<img src="assets/images/4.2 Uji Heteroskedastisitas.png" width="500">

Kesimpulan: p-value yang sangat kecil berarti tes ini positif menemukan 'kejanggalan'. Model terbukti **mengalami Heteroskedastisitas**, sehingga asumsi ini dilanggar.

**Uji Autokorelasi (Breusch-Godfrey/Wooldridge)**

```{r11}
print("--- Uji Autokorelasi (Wooldridge) ---")
print(pbgtest(fixed))
```

<img src="assets/images/4.3 Uji Autokorelasi.png" width="680">

Kesimpulan: p-value yang sangat kecil berarti tes ini positif menemukan 'kejanggalan'. Model terbukti **mengalami Autokorelasi**, sehingga asumsi ini dilanggar.

**Uji Normalitas Residual**

```{r12}
print("--- Uji Normalitas (Shapiro-Wilk) ---")
print(shapiro.test(residuals(fixed)))
```

<img src="assets/images/4.4 Uji Normalitas.png" width="450">

Kesimpulan: p-value yang kecil berarti "Gagal". Tes ini menemukan bahwa **sisaan model tidak terdistribusi normal**, sehingga asumsi normalitas dilanggar.

**Uji Cross-sectional Dependence**

```{r13}
print("--- Uji Cross-sectional Dependence (Pesaran CD) ---")
print(pcdtest(fixed, test = "cd"))
```

<img src="assets/images/4.5 Uji Cross-sectional Dependence.png" width="630">

Kesimpulan: p-value yang kecil berarti tes ini positif menemukan 'kejanggalan'. Dengan kata lain, pertumbuhan ekonomi suatu provinsi **tidak sepenuhnya independen dari provinsi lain**, ada keterkaitan atau pengaruh lintas wilayah.


#### **5. 📈 Visualisasi Tren Ternormalisasi (Z-Score)**

```{r14}
vars_to_plot <- c("PDRB_Growth", "HLS", "RLS", "UHH", "Pengeluaran")

data_avg_trend <- df %>%
  group_by(Tahun) %>%
  summarise(across(all_of(vars_to_plot), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(across(all_of(vars_to_plot), ~ as.numeric(scale(.x)))) %>%
  pivot_longer(
    cols = all_of(vars_to_plot),
    names_to = "Variabel",
    values_to = "Z_Score"
  )

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

print(plot_tren + 
        scale_x_continuous(breaks = seq(2015, 2024, by = 1)))
```

<img src="assets/images/5. Z-Score Normalisasi.png" width="650">

Plot ini dibuat untuk membandingkan **pola pergerakan beberapa variabel** dari tahun 2019 hingga 2024 dengan menggunakan **normalisasi Z-Score** karena variabel prediktor memiliki satuan yang sangat berbeda (misalnya Pengeluaran dalam ribu/jutaan Rupiah, UHH, RLS, dan HLS dalam tahun, serta PDRB_Growth dalam persentase). Nilai pada sumbu Y ditampilkan sebagai **standar deviasi** sebuah nilai berada dari **rata-ratanya**, sehingga perbedaan tren antarvariabel dapat diamati secara sebanding. 

Empat variabel komponen IPM (HLS, RLS, UHH, dan Pengeluaran) terlihat **bergerak secara serempak** dan menunjukkan tren kenaikan yang **konsisten serta stabil** selama periode pengamatan. Di sisi lain, pola yang **sangat berbeda dan tidak stabil (volatil)** ditunjukkan oleh variabel PDRB_Growth. Penurunan yang **sangat tajam** dialami oleh PDRB_Growth pada tahun 2020, yang kemudian diikuti oleh **pemulihan**, namun trennya **cenderung mendatar (stagnan**) dari 2022 hingga 2024, berlawanan dengan variabel IPM lainnya yang terus meningkat.

Plot ini secara visual menjelaskan **semua masalah** yang ditemukan dalam Uji Asumsi klasik sebelumnya:

- Model regresi linear mencari **pola yang konsisten**. Di sini, polanya **jelas tidak konsisten**.

- **Menjelaskan Kegagalan Uji Asumsi** Normalitas, Heteroskedastisitas, dan Autokorelasi.

- Guncangan besar (*shock*) di tahun 2020 adalah outlier ekstrem yang membuat **sisaan (residual)** model  menjadi **tidak normal** (melanggar Uji Shapiro-Wilk).

- Perbedaan volatilitas (liar di 2020, tenang di 2023) menyebabkan **varians error** yang **tidak konstan** (melanggar Uji Heteroskedastisitas).

- Efek *rebound* (pemulihan) di 2021 yang terjadi karena anjlok di 2020 adalah bentuk dari **Autokorelasi** (error tahun ini terkait dengan tahun lalu).

Singkatnya, ini adalah visualisasi "baik" karena berhasil menangkap akar masalah analisis, yaitu PDRB_Growth dipengaruhi oleh guncangan jangka pendek (pandemi) yang polanya sangat berbeda dari tren jangka panjang variabel IPM.


#### **6. 💡 Interpretasi Model**

```{r15}
summary(fixed)
```

<img src="assets/images/6. Interpretasi Model.png" width="450">

**1. Uji Kelayakan Model (F-statistic)**

  - Temuan: p-value = **0.00001221 (sangat kecil)**.
  
  - Artinya: Secara bersama-sama, keempat variabel Anda (UHH, HLS, RLS, Pengeluaran) memang **memiliki pengaruh yang signifikan** terhadap PDRB_Growth. 

**2. Uji Kekuatan Model (R-Squared)**
  
  - Temuan: R-Squared = **0.15461 (15.5%)** dan Adj. R-Squared = **-0.033823 (negatif)**.

  - Artinya: Ini **buruk**. R-Squared sebesar **0.15461** berarti model hanya mampu menjelaskan **15.5%** dari variasi PDRB_Growth. Ini sangat rendah. Adj. R-Squared yang **negatif** adalah penanda kuat bahwa model sangat lemah. Ini menunjukkan bahwa setelah memperhitungkan jumlah variabel, model ini pada dasarnya tidak memiliki daya jelas sama sekali. Hal ini juga sudah terlihat pada ploT normalisasi Z-Score sebelumnya.

**3. Uji Pengaruh per Variabel (Coefficients)**

  - Temuan: Kolom Pr(>|t|) atau p-value: 
    
    - UHH (Usia Harapan Hidup): p-value = **0.01427 (di bawah 0.05)**.

    - Variabel Lain (HLS, RLS, Pengeluaran): p-value-nya besar (**0.94, 0.19, 0.11**).

  - Artinya: Hanya UHH yang **terbukti berpengaruh signifikan** (dan positif) terhadap PDRB_Growth. Tiga variabel lainnya (HLS, RLS, dan Pengeluaran) **tidak terbukti memiliki pengaruh yang nyata** secara statistik.

**Kesimpulan Akhir**

Meskipun sudah dipilih model yang paling tepat secara metode (*Fixed Effect*), hasil dari model tersebut menunjukkan bahwa:

- Daya jelas model **sangat lemah** (R-Squared sangat kecil).

- Dari 4 variabel komponen IPM, **hanya UHH (Usia Harapan Hidup)** yang berpengaruh **signifikan** terhadap Pertumbuhan PDRB.

Ini sangat konsisten dengan temuan di awal (matriks korelasi dan plot tren) yang menunjukkan bahwa variabel-variabel IPM memang punya hubungan yang lemah dengan PDRB_Growth.




### 👥 **Tim Penyusun**

---

* Ade Ariyo Yudanto
* Daumi Rahmatika
* Fitri Hayati
* Nurqalbu Abd. Mutalip
* Putri Aqila
