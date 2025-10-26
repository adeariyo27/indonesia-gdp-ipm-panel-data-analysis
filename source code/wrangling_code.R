library(tidyverse)
setwd("D:/Magister/Kuliah/Pemrograman Statistika/Project Akhir/data/raw/")


#==============================================================
# WRANGLING: UHH, RLS, HLS
#==============================================================

exclude_list <- c(
  "INDONESIA",
  "PAPUA TENGAH",
  "PAPUA PEGUNUNGAN",
  "PAPUA SELATAN",
  "PAPUA BARAT DAYA"
)

df_clean <- read_csv(
  file_path,
  skip = 3, # Melewatkan 3 baris header yang tidak terstruktur
  col_names = c("Wilayah", "PerKapita_2019"), # Memberi nama kolom secara manual
  na = c("-") # Menginterpretasikan "-" sebagai NA (Missing Value)
) %>%
  # Filter hanya baris yang merupakan data Provinsi
  # Asumsi: Nama provinsi ditulis dalam ALL CAPS
  filter(Wilayah == toupper(Wilayah)) %>%
  
  # Filter untuk MENGHAPUS provinsi dari 'exclude_list'
  filter(!Wilayah %in% exclude_list) %>%
  
  # Mengatur ulang index/baris (opsional, tapi membuat rapi)
  rownames_to_column(var = "ID") %>%
  select(Wilayah, PerKapita_2019) # Hanya pilih kolom yang relevan 


print("--- Data Provinsi (Sudah Bersih) ---")
print(head(df_clean))
view(df_clean)

print(paste("Total jumlah provinsi setelah dibersihkan:", nrow(df_clean)))

write_csv(df_clean, "PerKapita_2019_provinsi_clean.csv")



#==============================================================
# WRANGLING: PENGELUARAN PER KAPITA
#==============================================================
file_path <- "[Metode Baru] Pengeluaran per Kapita Disesuaikan, 2019.csv"
exclude_list <- c(
  "INDONESIA",
  "PAPUA TENGAH",
  "PAPUA PEGUNUNGAN",
  "PAPUA SELATAN",
  "PAPUA BARAT DAYA"
)

df_clean <- read_csv(
  file_path,
  skip = 3, 
  col_names = c("Wilayah", "PengeluaranPerKapita_2024"), 
  na = c("-") 
) %>%
  filter(Wilayah == toupper(Wilayah)) %>%
  filter(!Wilayah %in% exclude_list) %>%
  mutate(PengeluaranPerKapita_2024 = PengeluaranPerKapita_2024 * 1000) %>%
  rownames_to_column(var = "ID") %>%
  select(Wilayah, PengeluaranPerKapita_2024)


print("--- Data Provinsi (Sudah Bersih) ---")
print(head(df_clean))
view(df_clean)
glimpse(df_clean)

print(paste("Total jumlah provinsi setelah dibersihkan:", nrow(df_clean)))

write_csv(df_clean, "PengeluaranPerKapita_2024_provinsi_clean.csv")



#==============================================================
# WRANGLING: LAJU PDRB
#==============================================================
file_path <- "Laju Pertumbuhan Produk Domestik Regional Bruto Atas Dasar Harga Konstan 2010 Menurut Provinsi (persen), 2019.csv"

exclude_list <- c(
  "Papua Tengah",
  "Papua Pegunungan",
  "Papua Selatan",
  "Papua Barat Daya"
)


df_pdrb_clean <- read_csv(
  file_path,
  skip = 1, 
  col_names = c("Wilayah", "Laju_Pertumbuhan_2019"), 
  na = c("-"), 
  locale = locale(decimal_mark = ".") # Memastikan desimal dibaca sebagai titik
) %>%
  filter(!Wilayah %in% exclude_list) %>%
  mutate(Laju_Pertumbuhan_2019 = as.numeric(Laju_Pertumbuhan_2019)) %>% 
  mutate(Wilayah = toupper(Wilayah))

print("--- Data Laju Pertumbuhan PDRB (34 Provinsi) ---")
view(df_pdrb_clean)
glimpse(df_pdrb_clean)

write_csv(df_pdrb_clean, "LajuPertumbuhan_2019_provinsi_clean.csv")



#==============================================================
# WRANGLING: OTHER
#==============================================================
setwd("D:/Magister/Kuliah/Pemrograman Statistika/Project Akhir/data/clean/")

data <- read_csv("UHH_2024_provinsi_clean.csv") %>%
  mutate(Wilayah = case_when(
    Wilayah == "D I YOGYAKARTA" ~ "DI YOGYAKARTA",
    Wilayah == "KEP. BANGKA BELITUNG" ~ "KEPULAUAN BANGKA BELITUNG",
    TRUE ~ Wilayah
  ))
write_csv(data, "UHH_2024_provinsi_clean.csv")
view(data)



#==============================================================
# WRANGLING: FORMAT DATA PANEL
#==============================================================
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

print("--- Data Wide (Setelah Digabung & Kolom Distandardisasi) ---")
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

print("--- Data Panel Final (Siap Analisis) ---")
glimpse(data_full)
view(data_full)
print(paste("Total Baris Data:", nrow(data_full)))
write_csv(data_full, "data_long_cleaned.csv")
