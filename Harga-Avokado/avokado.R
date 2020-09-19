# input library 
options(warn=-1)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(cowplot)

# input data
data <- read.csv("avocado.csv", sep=",")
str(data)

# ubah tipe kolom menjadi tipe faktor
data$year = as.factor(data$year)
data$Date = as.Date(data$Date)
data$month = factor(months(data$Date), levels = month.name)

#==============================================================================#
# VISUALISASI HARGA AVOKADO TAHUNAN
# 1.BOXPLOT
options(repr.plot.height = 5.5, repr.plot.width = 7)
ggplot(data, aes(type, AveragePrice)) + geom_boxplot(aes(colour=year)) +
  labs(colour="Tahun", x="Tipe Avokado", y="Harga Rerata US$", 
       title="Harga Rerata Avokado di U.S Tahun 2015 - 2018") +
  theme(plot.title = element_text(hjust = 0.5))

# 2.GEOM_AREA
df <- data[order(as.Date(data$Date, format="%Y-%m-%d")),] 
#mengurutkan penanggalan
ggplot(df, aes(Date,AveragePrice)) + 
  geom_area(aes(color=type), alpha=0.3, position=position_dodge(0.7)) +
  theme_minimal() + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x="Tahun",y="Harga Rerata US$", 
       title="Harga Rerata Avokado di U.S", colour = "Tipe Avokado") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom")

# 3.GEOM_POINT
ggplot(df[df$region == "TotalUS",], aes(Date, AveragePrice, color=type)) + 
  geom_point(size=0.8, alpha=0.8) + theme_light() +
  labs(x="Tahun", y="Harga Rerata US$", 
       title = "Harga Rerata Avokado di U.S", color="Tipe Avokado") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "bottom") 
#==============================================================================#
# VISUALISASI HARGA RERATA AVOKADO SETIAP BULAN
bulanan <- data
bulanan$month_year <- format(as.Date(data$Date), "%Y-%m")
bulanan$month      <- format(as.Date(data$Date), "%m")
bulanan$year       <- format(as.Date(data$Date), "%Y")

bulanan$monthabb   <- sapply(bulanan$month, function(x) month.abb[as.numeric(x)])
bulanan$monthabb   <- factor(bulanan$monthabb, levels=month.abb)

# 1. GEOM_POINT + GEOM_LINE
bulan_organik <- bulanan %>% select(monthabb, AveragePrice, type) %>%
  filter(type=="organic") %>% group_by(monthabb) %>% 
  summarise(rata2=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=rata2)) + geom_point(color="skyblue", aes(size=rata2)) +
  geom_line(group=1, color="gold2") + theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.5),
        plot.background = element_rect(fill="grey100")) +
  labs(x="Bulan",y="Harga Rerata US$", title="Harga Avokado Organik")

bulan_konvensional <- bulanan %>% select(monthabb, AveragePrice, type) %>%
  filter(type=="conventional") %>% group_by(monthabb) %>%
  summarise(rata2=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y =rata2)) + 
  geom_point(color="cyan2", aes(size=rata2)) +
  geom_line(group=1, color="gold1") +
  theme_minimal() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill="grey100")) + 
  labs(x="Bulan",y="Harga Rerata US$", title="Harga Avokado Konvensional")

ggarrange(bulan_organik, bulan_konvensional,
          ncol=2, nrow=1)

#==============================================================================#
# BERDASARKAN PER MUSIM
bulanan$Season <- ifelse(bulanan$month %in% c("03","04","05"),"Semi",
                         ifelse(bulanan$month %in% c("06","07","08"),"Panas",
                         ifelse(bulanan$month %in% c("09","10","11"), "Gugur",
                         "Dingin")))

season_konv <- bulanan %>% select(Season, year, AveragePrice, type) %>%
  filter(type == "conventional", year == c("2015","2016","2017")) %>%
  group_by(Season, year) %>%
  summarize(average=mean(AveragePrice)) %>%
  ggplot(aes(Season, average, color=Season)) +
  geom_point(size=3.0) +
  geom_segment(aes(x=Season, xend=Season,y=0, yend=average)) +
  coord_flip() + facet_wrap(~as.factor(year)) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = "cornsilk1")) +
  scale_color_manual(values=c("limegreen","red2","orchid4","cyan2")) +
  labs(x="Musim",y="Harga Rata-rata (US$)", title="Harga Avokado Konvensional") +
  geom_text(aes(x=Season, y=0.01, label=paste0("$",round(average,2))),
            hjust=0.5, vjust=-0.5, size=3.5,
            colour="black",fontface="italic", angle=360) +
  guides(color=guide_legend("Musim"))


season_org <- bulanan %>% select(Season, year, AveragePrice, type) %>%
  filter(type == "organic", year == c("2015","2016","2017")) %>%
  group_by(Season, year) %>%
  summarize(average=mean(AveragePrice)) %>%
  ggplot(aes(Season, average, color=Season)) +
  geom_point(size=3.0) +
  geom_segment(aes(x=Season, xend=Season,y=0, yend=average)) +
  coord_flip() + facet_wrap(~as.factor(year)) + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = "cornsilk1")) +
  scale_color_manual(values=c("limegreen","red2","orchid4","cyan2")) +
  labs(x="Musim",y="Harga Rata-rata (US$)", title="Harga Avokado Organik") +
  geom_text(aes(x=Season, y=0.01, label=paste0("$",round(average,2))),
            hjust=0.5, vjust=-0.5, size=3.5,
            colour="black",fontface="italic", angle=360) +
  guides(color=guide_legend("Musim"))

plot_grid(season_konv, season_org, labels="AUTO", nrow=2)
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




















