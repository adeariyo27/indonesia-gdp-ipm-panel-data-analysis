dataKP=read.csv(file.choose(),header = TRUE, sep = ";") 
View(dataKP) 

#melihat deskripsi data 
summary(dataKP) 
str(dataKP) 
summary(is.na(dataKP)) 
plot(dataKP) 
library(plm) 
library(lmtest) 
attach(dataKP) 

#mencari model terbaik 
# uji chow 
common=plm(IPM~Tingkat_Kemiskinan+RLS+PDRB+Keluhan_Kesehatan, data
           = dataKP, model = "pooling",  
           index = c("Kabupaten.Kota","Tahun")) 
fixed=plm(IPM~Tingkat_Kemiskinan+RLS+PDRB+Keluhan_Kesehatan, data 
          = dataKP, model = "within", 
          index = c("Kabupaten.Kota","Tahun")) 
pooltest(common,fixed) 

# uji hausman 
fixed=plm(IPM~Tingkat_Kemiskinan+RLS+PDRB+Keluhan_Kesehatan, data 
          = dataKP, model = "within", 
          index = c("Kabupaten.Kota","Tahun")) 
random=plm(IPM~Tingkat_Kemiskinan+RLS+PDRB+Keluhan_Kesehatan, data
           = dataKP, model = "random", 
           index = c("Kabupaten.Kota","Tahun")) 
phtest(fixed,random) 

###Mengetahui ada tidaknya efek kali-silang,  
#efek waktu atau efek kali-silang maupun waktu (2 arah) 
# uji breusch pagan 
### model 1 
g=plm(IPM~Tingkat_Kemiskinan+PDRB+RLS+Keluhan_Kesehatan, data = da
      taKP, model = "pooling") 
plmtest(g,effect="twoways", type="bp") #uji efek dua arah 
plmtest(g,effect="individual", type="bp") #uji efek individu 
plmtest(g, effect="time", type="bp") #uji efek waktu 


library(foreign) 
#estimasi model 
# Model I: model efek tetap, dengan efek individu 

g = NULL  
g = plm(IPM~Tingkat_Kemiskinan+PDRB+RLS+Keluhan_Kesehatan, 
        data=dataKP, model="within", effect = "individual") 
summary(g) 


# Keluhan_kesehatan tidak signifikan 
g = NULL 

g = plm(IPM~Tingkat_Kemiskinan+PDRB+RLS, 
        data=dataKP, model="within", effect = "individual") 
summary(g) 
fixef(g, type = "level") 
#Uji kenormalan 
shapiro.test(g$residuals) 
##Uji Diagnostik 
#Uji Korelasi Serial : gagal tolak maka tidak ada korelasi serial 
pbgtest(g) 
#Uji Heteroskedastisitas pvalue > alfa : asumsi terpenuhi 
library(car) 
library(carData) 
bptest(g) 