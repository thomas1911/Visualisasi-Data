# Pembacaan data penduduk obesitas 
data <- read.csv("jumlah-penduduk-obesitas-usia-di-15-tahun-ke-atas-di-provinsi-jawa-barat-2018.csv",sep = ",")

# Ambil kolom yang penting saja, yaitu::
# nama kabupaten/kota, jenis kelamin, dan jumlah
data_obesitas <- data[,3:5]

# Lakukan pengelompokan data yang berfokus kepada pembagian jenis kelamin
library(tidyr)
data_obesitas <- tidyr::spread(data_obesitas, nama_kota_kabupaten, jumlah)
# Perhitungan jumlah laki-laki & perempuan 
data_obesitas %>% select('KAB. BANDUNG':'KOTA TASIKMALAYA') %>% 
  rowSums(na.rm = TRUE) -> data_obesitas$Total
jumlah_laki <- data_obesitas[1, 29]
jumlah_perempuan <- data_obesitas[2, 29]

# Visualisasi Grafik Donut digunakan untuk menampilkan perbandingan 
# penduduk obesitas antara laki-laki dan perempuan
pie <- data.frame(Jenis_Kelamin = c('Laki-laki','Perempuan'),
                    Jumlah = c(jumlah_laki, jumlah_perempuan))
ggplot(pie, aes(x="", y=Jumlah, fill=factor(Jenis_Kelamin))) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label=paste(round(Jumlah/sum(Jumlah) * 100,1), "%"),
                x=1.2), position = position_stack(vjust=0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(fill = "Jenis Kelamin", x=NULL, y=NULL,
       title = "Penduduk Obesitas 15 Tahun Ke Atas Provinsi Jawa Barat") +
  coord_polar(theta="y")

# Selain itu, dapat juga dilakukan penggambaran besaran penduduk yang mengalami
# obesitas antar kabupaten/kota yang tercantum dalam data
data_bar <- data_obesitas %>% gather(key=key, value=value, 
                                     'KAB. BANDUNG':'KOTA TASIKMALAYA')
judul_legenda <- "Jenis Kelamin"
ggplot(data_bar, aes(key, value, fill = jenis_kelamin)) +
  geom_col(position = "dodge", aes(color=jenis_kelamin)) +
  theme_minimal() +
  labs(x="Kabupaten/Kota", y="Jumlah (orang)", 
       title = "Jumlah Penduduk Obesitas 15 Tahun Ke Atas Provinsi Jawa Barat Tahun 2018") +
  theme(axis.text.x = element_text(angle=90, hjust=1),
        plot.title = element_text(hjust=0.5),
        text = element_text(size=11.5))










