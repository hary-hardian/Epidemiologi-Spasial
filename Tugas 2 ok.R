#Tugas 2: Aggregasi Data Nakes

#1. Import data
library(dplyr)
library(readr)
library(tidyverse)

master_nakes <- read_csv("https://raw.githubusercontent.com/dwi-agustian/ahs_unpad/main/master_nakes.csv")

#2. Identifikasi variabel yang tersedia dari dataset
names(master_nakes)
str(master_nakes)
glimpse(master_nakes)
summary(master_nakes)

#3. Eksplorasi variabel jenis SDMK (jenis_sdmk)

#check jumlah observasi variabel jenis_sdmk
length(master_nakes$jenis_sdmk)

#tabel frekuensi variabel jenis_sdmk
table(master_nakes$jenis_sdmk)


#4. Pelajari seluruh kategori jenis sdmk, recoding menjadi variabel baru (Dokter Umum, Dokter Spesialis, Dokter Subspesialis, Non Dokter)

master_nakes1 <- master_nakes

#membuat variabel baru
 master_nakes2 <- mutate(.data=master_nakes1, sdmk_dokter=jenis_sdmk)

glimpse (master_nakes2)
head(master_nakes2)
names(master_nakes2)

#recode variabel dengan kategori yang lebih sederhana
master_nakes2$sdmk_dokter[master_nakes2$sdmk_dokter %in% c("Dokter", "Dokter Gigi")] <- "Dokter Umum"
master_nakes2$sdmk_dokter[master_nakes2$sdmk_dokter %in% c("Dokter Gigi Spesialis Anak - Pedodontis (Sp.KGA)",
"Dokter Gigi Spesialis Bedah mulut / Maksilofasial (Sp.BM)", 
"Dokter Gigi Spesialis Gigi Tiruan (Prostodontis) (Sp.Pros)", 
"Dokter Gigi Spesialis Kawat Gigi - Orthodontis (Sp.Ort)",
"Dokter Gigi Spesialis Konservasi Gigi / Endodonsi (Sp.KG)", 
"Dokter Gigi Spesialis lainnya yang belum tercantum", 
"Dokter Gigi Spesialis Penyakit Mulut (Sp.PM)", 
"Dokter Gigi Spesialis Periodonsia (Sp.Perio)", 
"Dokter Gigi Spesialis Radiologi kedokteran gigi (Sp.RKG)", 
"Dokter Spesialis Akupunktur Klinik (Sp.Ak)",
"Dokter Spesialis Anak (Sp.A)",
"Dokter Spesialis Anastesiologi (Sp.An)", 
"Dokter Spesialis Andrologi (Sp.And)",
"Dokter Spesialis Bedah (Sp.B)",
"Dokter Spesialis Bedah Anak (Sp.BA)", 
"Dokter Spesialis Bedah Konsultan Vaskuler (Sp. B Vaskular)",
"Dokter Spesialis Bedah Orthopedi", 
"Dokter Spesialis Bedah Plastik (Sp.BP)", 
"Dokter Spesialis Bedah Syaraf (Sp.BS)", 
"Dokter Spesialis Bedah Thoraks Dan Kardiovaskuler (Sp.BTKV)", 
"Dokter Spesialis Farmakologi Klinik (Sp.FK)", 
"Dokter Spesialis Forensik (Sp.F)", 
"Dokter Spesialis Gizi Klinik (Sp.GK)", 
"Dokter Spesialis Gizi Medik", 
"Dokter Spesialis Ilmu Kesehatan Kulit Dan Kelamin (Sp.KK)", 
"Dokter Spesialis Ilmu Kesehatan THT Kl  (Sp.THT-KL)", 
"Dokter Spesialis Jantung dan Pembuluh Darah (Sp.JP)", 
"Dokter Spesialis Kedaruratan Medik - Emergency (Sp.EM)", 
"Dokter Spesialis Kedokteran Fisik Dan Rehabilitasi (Sp.KFR)", 
"Dokter Spesialis Lainnya yang belum tercantum", 
"Dokter Spesialis Mata (Sp.M)", 
"Dokter Spesialis Mikrobiologi Klinik (Sp.MK)", 
"Dokter Spesialis Neorologi/Saraf (Sp.S)", 
"Dokter Spesialis Nuklir (Sp.KN)", 
"Dokter Spesialis Obstetri & Ginekologi (Sp.OG)", 
"Dokter Spesialis Ofthalmologi", 
"Dokter Spesialis Okupasi (Sp.OK)", 
"Dokter Spesialis Onkologi Radiasi (Sp.Onk.Rad)", 
"Dokter Spesialis Orthopedi & Traumatologi (Sp.OT)", 
"Dokter Spesialis Parasitologi Klinik (Sp.ParK)", 
"Dokter Spesialis Paru  - Pulmonologi (Sp.P)", 
"Dokter Spesialis Patologi Anatomi (Sp.PA)", 
"Dokter Spesialis Patologi Forensik", 
"Dokter Spesialis Patologi Klinik (SP.PK)", 
"Dokter Spesialis Penyakit Dalam (Sp.PD)", 
"Dokter Spesialis Psikiatri - Kedokteran Jiwa (Sp.KJ)", 
"Dokter Spesialis Radiologi (Sp.Rad)", 
"Dokter Spesialis Radioterapi", 
"Dokter Spesialis Rehabilitasi Medik (Sp.RM)", 
"Dokter Spesialis Urologi (Sp.U)")] <- "Dokter Spesialis"

master_nakes2$sdmk_dokter[master_nakes2$sdmk_dokter %in% c("Subspesialis  Hip and Knee",
                                                           "Subspesialis  Nefrologi",
                                                           "Subspesialis Alergi imunologi",
                                                           "Subspesialis Aritmia", 
                                                           "Subspesialis Bedah Digestif Anak", 
                                                           "Subspesialis Bedah Estetik Lanjut", 
                                                           "Subspesialis Digestif",
                                                           "Subspesialis Ekokardiografi",
                                                           "Subspesialis Emergensi dan Rawat Intensif Anak (ERIA)",
                                                           "Subspesialis Endokrin metabolik",
                                                           "Subspesialis Endokrinologi",
                                                           "Subspesialis Fertilitas dan endokrinologi reproduksi", 
                                                           "Subspesialis Feto Maternal", 
                                                           "Subspesialis Gastroenterologi-hepatologi", 
                                                           "Subspesialis Gastrohepatologi", 
                                                           "Subspesialis Geriatri",
                                                           "Subspesialis Ginjal hipertensi",
                                                           "Subspesialis Hematologi",
                                                           "Subspesialis Hematologi onkologi",
                                                           "Subspesialis Hematologi Onkologi", 
                                                           "Subspesialis Infeksi dan penyakit tropik", 
                                                           "Subspesialis Infeksi Menular Seksual",
                                                           "Subspesialis Infeksi Paru", 
                                                           "Subspesialis Intensive Care", 
                                                           "Subspesialis Intervensi dan gawat nafas", 
                                                           "Subspesialis Jantung Anak dan PJB", 
                                                           "Subspesialis Kardiologi", 
                                                           "Subspesialis Kardiologi Intervensi", 
                                                           "Subspesialis Kardiovaskuler", 
                                                           "Subspesialis Kedokteran Vaskular", 
                                                           "Subspesialis Kesehatan Jiwa Anak dan Remaja", 
                                                           "Subspesialis Neonatologi", 
                                                           "Subspesialis Neurointervensi", 
                                                           "Subspesialis Neurologi", 
                                                           "Subspesialis Neuroradiologi kepala leher", 
                                                           "Subspesialis Nutrisi dan penyakit metabolik", 
                                                           "Subspesialis Obstetri ginekologi sosial", 
                                                           "Subspesialis Onkologi", 
                                                           "Subspesialis Onkologi bedah kepala leher", 
                                                           "Subspesialis Onkologi ginekologi", 
                                                           "Subspesialis Orthopedic Spine", 
                                                           "Subspesialis Otologi", 
                                                           "Subspesialis Pediatri", 
                                                           "Subspesialis Pediatri sosial-tumbuh kembang", 
                                                           "Subspesialis Pediatric Orthopaedic", 
                                                           "Subspesialis Perawatan Intensif dan Kegawatan Kardiovaskular",
                                                           "Subspesialis Psikiatri Adiksi", 
                                                           "Subspesialis Psikogeriatri", 
                                                           "Subspesialis Psikoterapi",
                                                           "Subspesialis Pulmonologi", 
                                                           "Subspesialis Radiologi Abdomen", 
                                                           "Subspesialis Radiologi Anak", 
                                                           "Subspesialis Radiologi Thoraks", 
                                                           "Subspesialis Rematologi", 
                                                           "Subspesialis Respirologi", 
                                                           "Subspesialis THT komunitas", 
                                                           "Subspesialis Tropik infeksi", 
                                                           "Subspesialis Uroginekologi dan rekonstruksi", 
                                                           "Subspesialis Urologi Onkologi", 
                                                           "Dokter Sub Spesialis Lainnya (yang belum tercantum)")] <- "Dokter Subspesialis" 

master_nakes2$sdmk_dokter[master_nakes2$sdmk_dokter %in% c("Administrasi dan Kebijakan Kesehatan", 
  "Ahli Madya Farmasi (Asisten Apoteker)", 
  "Ahli Teknologi Laboratorium Medik (Analis Kesehatan)", 
  "Analis Farmasi", 
  "Analis Kesehatan (Asisten)", 
  "Apoteker", 
  "Aset", 
  "Bidan (Asisten)", 
  "Bidan Desa", 
  "Bidan Klinis", 
  "Bidan Pendidik", 
  "Dietisien", 
  "Direktur Jenderal / Kepala Badan Setingkat Es.1", 
  "Elektromedis", 
  "Entomolog Kesehatan", 
  "Epidemiolog Kesehatan", 
  "Evaluasi", 
  "Farmasi (Asisten)", 
  "Fisikawan Medik", 
  "Fisioterapis", 
  "Gaji", 
  "Gaji dan Umum", 
  "Gizi (asisten)", 
  "Guru", 
  "Hubungan Masyarakat", 
  "Hukum", 
  "Informatika Kesehatan", 
  "Instruktur", 
  "Jaminan Kesehatan", 
  "Jenis Bidan Lainnya yang belum tercantum",
  "Jenis Perawat Lainnya yang belum tercantum", 
  "Juru Mudi", 
  "Keamanan", 
  "Kepala Bagian / Kepala UPT setingkat Es. 3",
  "Kepala Bidang", 
  "Kepala Dinas", 
  "Kepala Pusat / Direktur Setingkat Es.2",
  "Kepala Puskesmas", 
  "Kepala Seksi", 
  "Kepala Subbagian / Kepala UPT setingkat Es.4",
  "Kepala Subbidang", 
  "Kesehatan Kerja",
  "Kesehatan Lingkungan (Asisten)", 
  "Kesehatan Masyarakat (Lainnya)", 
  "Keuangan", 
  "Lektor", 
  "Mutasi Pegawai", 
  "Ners", 
  "Nutrisionis", 
  "Okupasi Terapis", 
  "Organisasi", 
  "Pekarya", 
  "Pekerja sosial", 
  "Pelaporan", 
  "Penata Anestesi", 
  "Pengarsipan", 
  "Pengelola Data",
  "Pengelola Jaringan Komputer",
  "Pengelola Program Gizi dan KIA", 
  "Pengelola Program Kefarmasian dan Alat Kesehatan", 
  "Pengelola Program Kesehatan Lainnya (yang belum tercantum)",
  "Pengelola Program Penelitian dan Pengembangan Kesehatan",
  "Pengelola Program Pengembangan dan Pemberdayaan SDM Kesehatan", 
  "Pengelola Program Pengendalian Penyakit dan Penyehatan Lingkungan", 
  "Pengelola Program Upaya Kesehatan", 
  "Pengelola Sistem Informasi dan Basis Data (Database)",
  "Pengembangan Pegawai", 
  "Perawat (Non Ners)", 
  "Perawat Asisten)", 
  "Perawat Geriatri - Lansia",
  "Perawat Kesehatan Anak",
  "Perawat Kesehatan Jiwa", 
  "Perawat Komunitas", 
  "Perawat Maternitas", 
  "Perawat Medikal Bedah", 
  "Perekam Medis dan Informasi Kesehatan", 
  "Perencanaan", 
  "Perpustakaan", 
  "Petugas IPSRS / Teknisi pemeliharaan fasilitas", 
  "Petugas Kamar Jenazah / Pemulasaran Jenazah", 
  "Petugas Pengelola Limbah", 
  "Program", 
  "Promosi Kesehatan", 
  "Psikologi Klinis", 
  "Publikasi dan Informasi Publik", 
  "Radiografer", 
  "Radioterapis", 
  "Refraksionis Optisien/Optometris", 
  "Sanitasi Lingkungan", 
  "Sarjana, Magister Farmasi (Non Apoteker)", 
  "Sekretaris / Direktur Setingkat Es.2",
  "Teknisi Gigi", 
  "Teknisi Kardiovaskular", 
  "Teknisis Pelayanan Darah", 
  "Tenaga Umum Lainnya yang belum tercantum", 
  "Terapis Gigi dan Mulut", 
  "Terapis Gigi dan Mulut (asisten)", 
  "Terapis Wicara", 
  "Wakil Direktur / Deputi Setingkat Es.2", 
  "Widyaiswara Madya", 
  "Widyaiswara Muda")] <- "Non Dokter"

table(master_nakes2$sdmk_dokter)  
length(master_nakes2$sdmk_dokter)


#5. Buat Dataset baru yang berisikan Dokter saja (Dokter Umum, Dokter Spesialis, Dokter Subspesialis)
#menghapus row "Non Dokter"
master_dokter_jabar = filter(master_nakes2, sdmk_dokter!="Non Dokter")

glimpse(master_dokter_jabar)

#Filter dokter yang praktek di Jawa Barat
table(master_dokter_jabar$nama_prov)
master_dokter_jabar2 = filter (master_dokter_jabar, nama_prov == "JAWA BARAT")

glimpse(master_dokter_jabar2)
table(master_dokter_jabar2$nama_prov)

#6. Identifikasi adanya tempat ijin praktek dokter yang lebih dari satu
#Mengidentifikasi tempat praktek lebih dari 1 berdasarkan sip dan str
table(master_dokter_jabar2$sip)

master_dokter_jabar2 %>%
  count(sip) %>% 
  filter(n > 1)

table(master_dokter_jabar2$str)

master_dokter_jabar2 %>%
  count(str) %>% 
  filter(n > 1)

#7. Hitung jumlah dokter berdasarkan NIK per kota Kab di Jawa Barat
#mengidentifikasi dan menghapus duplikasi NIK
master_dokter_jabar2 %>%
  count(NIK) %>% 
  filter(n > 1)

master_dokter_jabar3 = master_dokter_jabar2 %>% distinct(NIK, .keep_all = TRUE)

#jumlah dokter (berdasarkan NIK) per kota kabupaten
table(master_dokter_jabar3$nama_kab)

#8. Upload Code R dan dataset yang berisikan tabel agregat
write.csv(master_dokter_jabar3, file="master_dokter_jabar3.csv", row.names = F)
library(writexl)
write_xlsx(master_dokter_jabar3, "master_dokter_jabar3.xlsx")
