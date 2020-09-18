#==============================================================================#
# Disni merupakan rentang harga buah avokado di Amerika Serikat dengan data    #
# diambil dari berbegai negara bagian selama tahun 2015-2018
# 
# Selain itu, dalam data ini juga disajikan 2 tipe buah avokado yang dijual::
# 1. Avokado 'conventional'
# 2. Avokado 'organic'
#==============================================================================#
avokado <- read.csv("avocado.csv", sep=",")
avokado
# import packages
options(warn=-1)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(corrgram))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(treemap))
suppressPackageStartupMessages(library(repr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(plotrix))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tibbletime))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(smooth))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(fpp2))
options(scipen=5)
#==============================================================================#
# Exploratory Data Analysis -> pada projek kali ini akan fokus kepada 2 aspek  #
# 1. Pola Musim :: yaitu mengacu kepada pola yang sering muncul dari tahun     #
#                  ke tahun dan dari bulan ke bulan untuk kedua tipe avokado.  #
#                                                                              #
# 2. Pola Siklus:: berfokus kepada faktor yang menyebabkan perubahan           #
#                  harga avokado sepanjang tahun 2015-2018 dan seberapa besar  #
#                  pengaruhnya.                                                #
#==============================================================================#

options(repr.plot.width = 8, repr.plot.height = 4)
ggplot(avokado, aes(x=AveragePrice, fill=type)) + geom_density() +
  facet_wrap(~type) + theme_minimal() +
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") + 
  labs(title="Harga Avokado", x="Rentang Harga (US$)", y="Tingkat Penjualan") + 
  scale_fill_brewer(palette="Set2")

# sekarang kita ingin melihat pergerakan rentang 2 harga buah avokado berbeda
# sepanjang tahun 2015 hingga 2018
avokado$Date <- as.Date(avokado$Date, "%Y-%m-%d")#merubah tipe data menjadi Date
class(avokado$Date)

#==============================================================================#
# Menyusun ulang penanggalan 
avokado <- avokado[order(as.Date(avokado$Date, format="%Y-%m-%d")),]
# VISUALISAI HARGA AVOKADO
tren_harga <- avokado %>% select(Date, AveragePrice, type)
ggplot(tren_harga, aes(x = Date, y = AveragePrice)) + 
  geom_line(aes(color = type), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal() + labs(x="Tahun", y="Harga Rata-rata") +
  ggtitle("Pergerakan Harga Buah Avokado Conventional & Organic") +
  guides(color=guide_legend("Tipe Buah")) +
  theme(plot.title = element_text(hjust=0.55))

# VISUALISASI PERGERAKAN HARGA 2 TIPE BUAH AVOKADO 
ggplot(avokado, aes(x=Date, y=AveragePrice)) +
  geom_line(aes(col=type)) + facet_wrap(~type) +
  theme_minimal() + theme(legend.position = "bottom") +
  labs(x="Tahun", y="Harga Rata-rata")
# TERNYATA harga buah avokado organik mencapai 2 kali lipat hingga
# 3 kali lipat ketika mendekati tahun 2017 lalu normal kembali
#==============================================================================#
#==============================================================================#
# | HUBUNGAN ANTARA HARGA && TOTAL VOLUME |                                    #
# => biasanya terdapat hubungan antara suplai buah avokado dengan harga        #
# => Secara umum, ketika harga buah naik maka suplai akan turun dan sebaliknya #
# => Tetapi, apakah hal ini berlaku untuk kedua tipe buah avokado?             #
#==============================================================================#

# 1. FUNGSI AVOKDAO ORGANIK
organik <- avokado %>% select(Date, AveragePrice, type, Total.Volume) %>%
  filter(type == 'organic')
# 2. FUNGSI AVOKADO KONVENSIONAL
konvensional <- avokado %>% select(Date, AveragePrice, type, Total.Volume) %>%
  filter(type == 'conventional')

organik <- as_tbl_time(organik, Date)
organik <- as_period(organik, '1 month')
head(organik, 10)

konvensional <- as_tbl_time(konvensional, Date)
konvensional <- as_period(konvensional, '1 month')
head(konvensional, 10)
#==============================================================================#
# setelah membuat fungsi dataframe penjualan buah berurutan setiap bulannya, maka
# kita perlu memvisualisasikan pergerakan harganya 
library(cowplot)
options(repr.plot.width = 8, repr.plot.height = 6)
# fungsi pergerakan harga rata" avokado
konvensional_bulanan <- ggplot(konvensional,aes(x=Date, y=AveragePrice)) + 
  geom_line(color="#7FB3D5") + 
  theme_economist_white() + 
  theme(plot.title = element_text(hjust=0.5), 
        plot.background = element_rect(fill="#D5D8DC")) +
  labs(x="Tahun",y="Rata-rata Harga (US$)",
       title = "Pergerakan Harga Rerata Buah Avokado Konvensional") +
  geom_hline(yintercept = max(konvensional$AveragePrice),
             color="red") + 
  geom_hline(yintercept = min(konvensional$AveragePrice),
             color="blue") 

# fungsi grafik suplai avokado konvensional
konvensional_volume <- ggplot(konvensional, aes(x=Date, y=Total.Volume)) + 
  geom_bar(stat = 'identity', fill = "#7FB3D5", color='black') +
  theme_economist_white() + 
  theme(plot.title = element_text(hjust=0.5), 
        plot.background = element_rect(fill="#D5D8DC")) +
  geom_smooth(method="loess", color='red') +
  labs(x="Tahun",y="Volume Total", title="Suplai Buah Avokado Konvensional")

# menggabungkan kedua fungsi sehingga diperoleh 1 gambaran
plot_grid(konvensional_bulanan, konvensional_volume, labels="AUTO")
#==============================================================================#
# TAMPILAN VISUALISASI SUPLAI BUAH AVOKADO ORGANIK
organik_bulanan <- ggplot(organik, aes(x=Date, y=AveragePrice)) +
  geom_line(color="black") + theme_economist_white() +
  theme(plot.title = element_text(hjust=0.5),
        plot.background = element_rect(fill = "#F0E442")) +
  labs(x="Tahun",y="Harga Rata-rata") + 
  ggtitle("Pergerakan Harga Rerata Buah Avokado Organik") +
  geom_hline(yintercept = max(organik$AveragePrice),
             color="red") + 
  geom_hline(yintercept = min(organik$AveragePrice),
             color="blue")

organik_volume <- ggplot(organik, aes(x=Date,y=Total.Volume))  +
  geom_bar(stat = 'identity', fill = "#7FB3D5", color='black') +
  theme_economist_white() + 
  theme(plot.title = element_text(hjust=0.5), 
        plot.background = element_rect(fill="#F0E442")) +
  geom_smooth(method="loess", color='red') +
  labs(x="Tahun",y="Volume Total", title="Suplai Buah Avokado Organik")

plot_grid(organik_bulanan, organik_volume, labels="AUTO")

# Dari kedua gambar plot tsb dapat diketahui bahwa::
# 1. Harga avokado konvensional menunjukan penurunan di awal tahun dan suplai 
#    masih mengalami peningkatan dan pada pertengahan tahun harga meningkat 
#    sedangkan suplai mengalami penurunan.
# 2. Untuk buah avokado organik, tidak mengalami tren yang serupa dengan versi 
#    konvensional dimana khusus pertengahan tahun antara 2016-2017 
#    harga mengalami penurunan drastis sedangkan suplai masih melimpah. 
#==============================================================================#
#==============================================================================#

#==============================================================================#
#==============================================================================#
# Untuk memahami lebih lanjut mengenai bisnis buah avokado organik, maka
# perlu dilakukan *analisa pola musiman* dengan parameter sebagai berikut.
# 1. Standar deviasi sebagai alat ukur volatilitas ketika tahun 2017 terjadi
#    volatilitas tertinggi baik untuk avokado konvensional & organik
#
# 2. Perbedaan %% untuk avokado dimana kita ingin melihat besar
#    persentase peningkatan dari tahun ke tahun terhadap rata-rata harga
#    avokado setiap bulannya.

musim_avokado <- avokado #fungsi musiman avokado
musim_avokado$bulan_tahun <- format(as.Date(avokado$Date), "%Y-m")
musim_avokado$bulan       <- format(as.Date(avokado$Date), "%m")
musim_avokado$tahun       <- format(as.Date(avokado$Date), "%Y")

ggplot(musim_avokado, aes(x=AveragePrice, fill=as.factor(year))) +
  geom_density(alpha= 0.5) + theme_economist_white() +
  facet_wrap(~year) + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill="gold2")) +
  guides(fill =FALSE) +
  labs(title="Distribusi Harga Avokado Per Tahun", x="Rerata Harga (US$)", y="Densitas") +
  scale_fill_manual(values=c("skyblue","midnightblue","firebrick","darkcyan"))

# sekarang perlu kita lihat perkembangannya harga rerata pada setiap bulan
# dari tahun ke tahun

musim_avokado$monthabb <- sapply(musim_avokado$bulan, function(x) month.abb[as.numeric(x)])
musim_avokado$monthabb <- factor(musim_avokado$monthabb, levels=month.abb)

pola_musim_k <- musim_avokado %>% select(monthabb, AveragePrice, type) %>%
  filter(type == 'conventional') %>%
  group_by(monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) +
  geom_point(color="#F35D5D",aes(size=avg)) +
  geom_line(group=1, color="royalblue4") +
  theme_economist_white() + 
  theme(legend.position = "none", plot.title = element_text(hjust=0.5),
        plot.background = element_rect(fill="grey98")) +
  labs(x="Bulan",y="Harga Rata-rata", title="Avokado Konvensional")

pola_musim_o <- musim_avokado %>% select(monthabb, AveragePrice, type) %>%
  filter(type == 'organic') %>%
  group_by(monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) +
  geom_point(color="#F35D5D",aes(size=avg)) +
  geom_line(group=1, color="royalblue4") +
  theme_economist_white() + 
  theme(legend.position = "none", plot.title = element_text(hjust=0.5),
        plot.background = element_rect(fill="grey98")) +
  labs(x="Bulan",y="Harga Rata-rata", title="Avokado Organik")

plot_grid(pola_musim_k, pola_musim_o, labels="AUTO")
# Dari grafik diatas, dapat dipahami bahwa rata-rata harga buah avokado akan naik 
# di awal musim gugur kemudian jatuh selama musim gugur berlangsung
# di Amerika Serikat. Harga buah avokado terendah umumnya terjadi di bulan Feb
#==============================================================================#
# Selain itu kita juga dapat memberikan gambaran harga avokado 
# di 4 musim berbeda di US
musim_avokado$Season <- ifelse(musim_avokado$bulan %in% c("03","04","05"), 
                               "Semi",
                               ifelse(musim_avokado$bulan %in% c("06","07","08"),"Panas",
                                      ifelse(musim_avokado$bulan %in% c("09","10","11"), "Gugur",
                                             "Dingin")))
plot_musim_kon <- musim_avokado %>% select(Season, year, AveragePrice, type) %>%
  filter(type == "conventional", year == c("2015","2016","2017")) %>%
  group_by(Season,year) %>%
  summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=Season, y=avg, color=Season)) + geom_point(size=2.5) +
  geom_segment(aes(x=Season, xend=Season,y=0,yend=avg)) +
  coord_flip() + facet_wrap(~as.factor(year)) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = "cornsilk1")) +
  scale_color_manual(values=c("limegreen","red2","orchid4","cyan2")) +
  labs(x="Musim",y="Harga Rata-rata (US$)", title="Harga Avokado Konvensional") +
  geom_text(aes(x=Season, y=0.01, label=paste0("$",round(avg,2))),
            hjust=0.5, vjust=-0.5, size=3.5,
            colour="black",fontface="italic", angle=360) +
  guides(color=guide_legend("Musim"))

plot_musim_org <- musim_avokado %>% select(Season, year, AveragePrice, type) %>%
  filter(type == "organic", year == c("2015","2016","2017")) %>%
  group_by(Season,year) %>%
  summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=Season, y=avg, color=Season)) + geom_point(size=2.5) +
  geom_segment(aes(x=Season, xend=Season,y=0,yend=avg)) +
  coord_flip() + facet_wrap(~as.factor(year)) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = "cornsilk1")) +
  scale_color_manual(values=c("limegreen","red2","orchid4","cyan2")) +
  labs(x="Musim",y="Harga Rata-rata (US$)", title="Harga Avokado Organik") +
  geom_text(aes(x=Season, y=0.01, label=paste0("$",round(avg,2))),
            hjust=0.5, vjust=-0.5, size=3.5,
            colour="black",fontface="italic", angle=360) +
  guides(color=guide_legend("Musim"))

plot_grid(plot_musim_kon, plot_musim_org, labels="AUTO", nrow=2)
# Dari sini dapat diketahui bahwa sepanjang tahun 2015-2017 
# secara umum harga buah avokado baik konvensional/organik 
# terbaik dibeli di musim dingin/awal tahun baru
#==============================================================================#
# VISUALISASI SETIAP REGIONAL::
# NORTHERN U.S | SOUTHERN U.S | EASTERN U.S | WESTERN U.S
# NOT 100% "CORRECT REGION TERM"

eastern  = c("Albany","Atlanta","BaltimoreWashington","Boston",
             "BuffaloRochester","Chicago","Columbus","Detroit",
             "GrandRapids","MiamiFtLauderdale","Northeast",
             "Philadelphia","Pittsburgh","RaleighGreensboro",
             "SouthCarolina","StLouis")
northern = c("Greatlakes","HarrisburgScranton","HartfordSpringfield",
             "NewYork","Northeast","NorthernNewEngland","Plains",
             "Seattle","Spokane","Syracuse")
western  = c("Boise","Denver","LasVegas","LosAngeles","Portland","SanDiego",
             "Seattle","Spokane","WestTexNewMexico")
southern = c("California","Charlotte","CincinnatiDayton","DallasFtWorth",
             "Houston","Indianapolis","Jacksonville","Louisville",
             "Midsouth","Nashville","NewOrlenasMobile","Orlando",
             "PhoenixTucson","Plains","RichmondNorfolk","Roanoke",
             "Sacramento","SanDiego","SouthCarolina","SouthCentral",
             "StLouis","Tampa","WestTexNewMexico")

# Karena data tahun 2018 belum selesai, maka data yang tepat untuk perbandingan
# ialah tahun penjualan dari 2015-2017 dari setiap wilayah
data <- avokado
df_timur   <- data[data$year %in% 2015:2017 & data$region %in% eastern,]
df_barat   <- data[data$year %in% 2015:2017 & data$region %in% western,]
df_utara   <- data[data$year %in% 2015:2017 & data$region %in% northern,]
df_selatan <- data[data$year %in% 2015:2017 & data$region %in% southern,]

#==============================================================================#
# 1. TIMUR/EAST
df_timur_org = df_timur %>%
  select(year, region, type, AveragePrice) %>%
  filter(type == "organic")
min_timur_org = round(min(df_timur$AveragePrice),1)-0.1 
max_timur_org = round(max(df_timur$AveragePrice),1)+0.1

options(repr.plot.height = 12, repr.plot.width = 9)
ggplot(df_timur_org, aes(x=region, y=AveragePrice)) +
  geom_tufteboxplot() +
  facet_grid(.~df_timur_org$year, scales = "free") +
  labs(colour = "Tahun", x="Wilayah",y="Harga Rerata US$",
       title = "Harga Avokado Organik Wilayah Timur U.S 2015 - 2017") +
  scale_y_continuous(breaks = c(seq(min_timur_org,max_timur_org,0.2)),
                     limits = c(min_timur_org, max_timur_org)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0,
                                   hjust = 50))

df_timur_conv = df_timur %>%
  select(year, region, type, AveragePrice) %>%
  filter(type == "conventional")
min_timur_conv = round(min(df_timur$AveragePrice),1)-0.1 
max_timur_conv = round(max(df_timur$AveragePrice),1)+0.1
ggplot(df_timur_conv, aes(x=region, y=AveragePrice)) +
  geom_tufteboxplot() +
  facet_grid(.~df_timur_conv$year, scales = "free") +
  labs(colour = "Tahun", x="Wilayah",y="Harga Rerata US$",
       title = "Harga Avokado Konvensional Wilayah Timur U.S 2015 - 2017") +
  scale_y_continuous(breaks = c(seq(min_timur_conv,max_timur_conv,0.2)),
                     limits = c(min_timur_conv, max_timur_conv)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0,
                                   hjust = 50))

#==============================================================================#
# 2. BARAT/WEST
df_barat_org = df_barat %>%
  select(year, region, type, AveragePrice) %>%
  filter(type == "organic")
min_barat_org = round(min(df_barat$AveragePrice),1)-0.1 
max_barat_org = round(max(df_barat$AveragePrice),1)+0.1
ggplot(df_barat_org, aes(x=region, y=AveragePrice)) +
  geom_tufteboxplot() +
  facet_grid(.~df_barat_org$year, scales = "free") +
  labs(colour = "Tahun", x="Wilayah",y="Harga Rerata US$",
       title = "Harga Avokado Organik Wilayah Barat U.S 2015 - 2017") +
  scale_y_continuous(breaks = c(seq(min_barat_org,max_barat_org,0.2)),
                     limits = c(min_barat_org, max_barat_org)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0,
                                   hjust = 50))

df_barat_conv = df_barat %>%
  select(year, region, type, AveragePrice) %>%
  filter(type == "conventional")
min_barat_conv = round(min(df_barat$AveragePrice),1)-0.1 
max_barat_conv = round(max(df_barat$AveragePrice),1)+0.1
ggplot(df_barat_conv, aes(x=region, y=AveragePrice)) +
  geom_tufteboxplot() +
  facet_grid(.~df_barat_conv$year, scales = "free") +
  labs(colour = "Tahun", x="Wilayah",y="Harga Rerata US$",
       title = "Harga Avokado Konvensional Wilayah Barat U.S 2015 - 2017") +
  scale_y_continuous(breaks = c(seq(min_barat_conv,max_barat_conv,0.2)),
                     limits = c(min_barat_conv, max_barat_conv)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0,
                                   hjust = 50))
#==============================================================================#
# 3. UTARA/NORTHERN
df_utara_org <- df_utara %>%
  select(year, region, type, AveragePrice) %>%
  filter(type == "organic")
min_utara_org = round(min(df_utara$AveragePrice),1)-0.1 
max_utara_org = round(max(df_utara$AveragePrice),1)+0.1
ggplot(df_utara_org, aes(x=region, y=AveragePrice)) +
  geom_tufteboxplot() +
  facet_grid(.~df_utara_org$year, scales = "free") +
  labs(colour = "Tahun", x="Wilayah",y="Harga Rerata US$",
       title = "Harga Avokado Organik Wilayah Utara U.S 2015 - 2017") +
  scale_y_continuous(breaks = c(seq(min_utara_org,max_utara_org,0.2)),
                     limits = c(min_utara_org, max_utara_org)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0,
                                   hjust = 50))

df_utara_conv = df_utara %>%
  select(year, region, type, AveragePrice) %>%
  filter(type == "conventional")
min_utara_conv = round(min(df_utara$AveragePrice),1)-0.1 
max_utara_conv = round(max(df_utara$AveragePrice),1)+0.1
ggplot(df_utara_conv, aes(x=region, y=AveragePrice)) +
  geom_tufteboxplot() +
  facet_grid(.~df_utara_conv$year, scales = "free") +
  labs(colour = "Tahun", x="Wilayah",y="Harga Rerata US$",
       title = "Harga Avokado Konvensional Wilayah Utara U.S 2015 - 2017") +
  scale_y_continuous(breaks = c(seq(min_utara_conv,max_utara_conv,0.2)),
                     limits = c(min_utara_conv, max_selatan_conv)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0,
                                   hjust = 50))
#==============================================================================#
# 4. SELATAN/SOUTHERN
df_selatan_org <- df_selatan %>%
  select(year, region, type, AveragePrice) %>%
  filter(type == "organic")
min_selatan_org = round(min(df_selatan$AveragePrice),1)-0.1 
max_selatan_org = round(max(df_selatan$AveragePrice),1)+0.1
ggplot(df_selatan_org, aes(x=region, y=AveragePrice)) +
  geom_tufteboxplot() +
  facet_grid(.~df_selatan_org$year, scales = "free") +
  labs(colour = "Tahun", x="Wilayah",y="Harga Rerata US$",
       title = "Harga Avokado Organik Wilayah Selatan U.S 2015 - 2017") +
  scale_y_continuous(breaks = c(seq(min_selatan_org,max_selatan_org,0.2)),
                     limits = c(min_selatan_org, max_selatan_org)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0,
                                   hjust = 50))

df_selatan_conv <- df_selatan %>%
  select(year, region, type, AveragePrice) %>%
  filter(type == "conventional")
min_selatan_conv = round(min(df_selatan$AveragePrice),1)-0.1 
max_selatan_conv = round(max(df_selatan$AveragePrice),1)+0.1
ggplot(df_selatan_conv, aes(x=region, y=AveragePrice)) +
  geom_tufteboxplot() +
  facet_grid(.~df_selatan_conv$year, scales = "free") +
  labs(colour = "Tahun", x="Wilayah",y="Harga Rerata US$",
       title = "Harga Avokado Konvensional Wilayah Selatan U.S 2015 - 2017") +
  scale_y_continuous(breaks = c(seq(min_selatan_org,max_selatan_org,0.2)),
                     limits = c(min_selatan_org, max_selatan_org)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0,
                                   hjust = 50))

#==============================================================================#
# Sekarang kita perlu mengetahui volatilitas harga kedua tipe avokado
# dengan nilai standar deviasinya

std_conv <- musim_avokado %>% select(year, monthabb, AveragePrice, type) %>%
  filter(type == "conventional", year == c("2015","2016","2017")) %>%
  group_by(year, monthabb) %>% summarize(std=sd(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=std)) +
  geom_point(aes(size=std), col="red3") +
  geom_segment(aes(x=monthabb,
                   xend=monthabb,
                   y=min(std),
                   yend=max(std)),
               linetype="dashed",
               size=0.09) + coord_flip() + facet_wrap(~year) +
  theme_tufte() +
  theme(plot.title = element_text(hjust=0.5), 
        plot.background = element_rect(fill = "cornsilk2"),
        legend.position = "none") +
  labs(title="Volatilitas Harga \n Avokado Konvensional",
       x="Bulan",y="Standar Deviasi")

std_org <- musim_avokado %>% select(year, monthabb, AveragePrice, type) %>%
  filter(type == "organic", year == c("2015","2016","2017")) %>%
  group_by(year, monthabb) %>% summarize(std=sd(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=std)) +
  geom_point(aes(size=std), col="gold3") +
  geom_segment(aes(x=monthabb,
                   xend=monthabb,
                   y=min(std),
                   yend=max(std)),
               linetype="dashed",
               size=0.09) + coord_flip() + facet_wrap(~year) +
  theme_tufte() +
  theme(plot.title = element_text(hjust=0.5), 
        plot.background = element_rect(fill = "cornsilk2"),
        legend.position = "none") +
  labs(title="Volatilitas Harga \n Avokado Konvensional",
       x="Bulan",y="Standar Deviasi")

plot_grid(std_conv, std_org, labels="AUTO",nrow=2)