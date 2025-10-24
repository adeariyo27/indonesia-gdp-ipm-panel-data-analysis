![](assets/images/banner.png)


### 📘 **Pendahuluan**

---

Proyek ini bertujuan untuk menganalisis pengaruh empat komponen Indeks Pembangunan Manusia (IPM) terhadap laju pertumbuhan ekonomi di tingkat provinsi di Indonesia. Analisis ini menggunakan data series waktu dari tahun 2019 hingga 2024.

### 📖 **Latar Belakang**

---

Dalam konteks pembangunan suatu wilayah, terdapat dua indikator utama yang sering menjadi sorotan: Produk Domestik Regional Bruto (PDRB) sebagai cerminan pertumbuhan ekonomi, dan Indeks Pembangunan Manusia (IPM) yang mengukur kemajuan dari aspek kualitas hidup. Meskipun PDRB menunjukkan seberapa besar output ekonomi suatu daerah, IPM menawarkan gambaran yang lebih holistik dengan mengukur kesehatan, pendidikan, dan standar hidup masyarakat. Keterkaitan antara keduanya menjadi esensial, di mana pertumbuhan ekonomi yang tinggi idealnya harus sejalan dengan peningkatan kualitas pembangunan manusia.

Studi ini bertujuan untuk mengeksplorasi secara lebih mendalam apakah terdapat hubungan positif dan saling memengaruhi antara IPM dan PDRB, khususnya dalam konteks provinsi di Indonesia. Pertumbuhan ekonomi yang berkelanjutan tidak hanya diukur dari angka, tetapi juga dari peningkatan kualitas hidup penduduk. Oleh karena itu, analisis ini sangat relevan untuk memahami seberapa efektif IPM sebagai pendorong pertumbuhan ekonomi, serta sebaliknya.

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

4. **Uji Asumsi Klasik**
Melakukan pengujian asumsi dasar regresi (seperti multikolinearitas, heteroskedastisitas, autokorelasi, dan normalitas residual) untuk menjamin validitas hasil estimasi.

### 🪜 **Tahapan**

---

#### **1. 🧹 Persiapan (Data Wrangling)**

*Import Library:*

```{r0}
library(tidyverse)
library(plm)
library(psych)
library(lmtest)
library(car)
library(RColorBrewer)
```

Pertama, dilakukan inisialisasi lingkungan kerja agar semua fungsi siap digunakan dengan cara memuat pustaka utama yang dibutuhkan untuk analisis statistik, regresi panel, visualisasi, serta pembersihan data atau biasa disebut tahap.

*Pembersihan & Transformasi Data*

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

Kemudian, dilakukan standarisasi dan penggabungan seluruh file data provinsi menjadi satu dataset panel yang konsisten. Proses ini mencakup penamaan ulang variabel agar seragam, transformasi dari format wide ke long, serta penyaringan periode (2019–2024) dan wilayah yang relevan. Hasil akhirnya adalah dataset panel bersih yang siap dipakai untuk analisis deskriptif maupun regresi panel.

#### **2. 🔍 Analisis Data Eksploratif**

*Statistik Deskriptif*

```{r3}
print(describeBy(df$PDRB_Growth, group = df$Provinsi))
print(describeBy(df$PDRB_Growth, group = df$Tahun))
```
Pada tahap ini, statistik deskriptif dihitung untuk menggambarkan karakteristik dasar pertumbuhan PDRB, baik menurut provinsi maupun menurut tahun.

*Matriks Korelasi*

```{r4}
vars_for_cor <- df %>% 
  select(PDRB_Growth, UHH, HLS, RLS, Pengeluaran)
cor_matrix <- cor(vars_for_cor, use = "complete.obs")
print("Matriks Korelasi:")
print(round(cor_matrix, 2))
```

<img src="assets/images/1.2 Matriks Korelasi.png" width="500">

Pada tahap ini, hubungan antarvariabel utama dianalisis melalui matriks korelasi sehingga pola keterkaitan antarindikator dapat diidentifikasi. Hasilnya: 

* PDRB_Growth: Variabel Y ini memiliki korelasi yang sangat lemah (hampir nol) dengan semua variabel lainnya. Nilai korelasinya berkisar antara -0.08 hingga 0.11.
* Variabel Lain: Terdapat korelasi positif yang cukup kuat (moderat) di antara variabel X: RLS, UHH, dan Pengeluaran. (*Pengeluaran vs UHH = 0.61, Pengeluaran vs RLS = 0.59, RLS vs UHH = 0.42, RLS VS HLS = 0.49*)
* Daya Prediksi Rendah: Korelasi yang sangat lemah antara variabel X dan Y (PDRB_Growth) menunjukkan bahwa variabel-variabel tersebut bukan prediktor linear yang baik. Model regresi linear yang dihasilkan kemungkinan besar tidak akan signifikan atau memiliki $R^2$ yang sangat rendah.
* Potensi Multikolinearitas: Adanya korelasi yang cukup kuat di antara variabel-variabel independen (misalnya UHH dan Pengeluaran = 0.61) mengindikasikan adanya potensi masalah multikolinearitas. Ini dapat membuat koefisien regresi tidak stabil dan sulit untuk diinterpretasikan.

*Visualisasi Tren PDRB Top 10 Provinsi*


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

<p align="center">
  <img src="assets/images/1.3 Visualisasi Tren PDRB Top 10 Provinsi.png" width="600" height="450">
</p>

Pada tahap ini, tren pertumbuhan PDRB divisualisasikan dengan menampilkan 10 provinsi teratas secara individual, sedangkan provinsi lainnya digabungkan ke dalam satu garis rata-rata untuk menjaga keterbacaan grafik.

#### **3. 🧩 Pemilihan Model (Uji Spesifikasi)**

*Estimasi Tiga Model Panel Utama*

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

*Uji Chow (Fixed vs Common)*

```{r7}
print("--- Uji Chow (Fixed vs Common) ---")
print(pFtest(fixed, common)
```

<img src="assets/images/3.1 Uji Chow.png" width="500">

Uji Chow ini bertugas sebagai "juri" untuk memutuskan satu hal:

"Apakah harus memperlakukan semua provinsi sebagai satu kelompok yang sama, atau setiap provinsi itu unik dan harus diperlakukan berbeda?"

- Model *common* (Gabungan): Menganggap semua provinsi itu sama. Data dari Aceh sampai Papua digabung jadi satu.
- Model *fixed* (*Fixed Effect*): Menganggap setiap provinsi itu unik. Ada karakteristik khusus (misal: budaya, geografi, kebijakan lokal) yang membedakan mereka.

Kesimpulan: Hasil p-value adalah 0.000003161.

Nilai ini sangat kecil, jauh di bawah batas umum (0.05), berarti tolak model yang sederhana (*common*) dan menerima model yang lebih kompleks (*fixed*).




### 👥 **Tim Penyusun**

---

* Ade Ariyo Yudanto
* Daumi Rahmatika
* Fitri Hayati
* Nurqalbu Abd. Mutalip
* Putri Aqila