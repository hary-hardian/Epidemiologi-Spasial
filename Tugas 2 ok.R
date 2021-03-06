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
master_nakes2$sdmk_dokter[master_nakes2$sdmk_dokter %in% c("Dokter")] <- "Dokter Umum"

#Dokter Gigi Umum
master_nakes2$sdmk_dokter[master_nakes2$sdmk_dokter %in% c("Dokter Gigi")] <- "Dokter Gigi Umum"

#Dokter Gigi Spesialis
master_nakes2$sdmk_dokter[master_nakes2$sdmk_dokter %in% c("Dokter Gigi Spesialis Anak - Pedodontis (Sp.KGA)",
                                                            "Dokter Gigi Spesialis Bedah mulut / Maksilofasial (Sp.BM)", 
                                                            "Dokter Gigi Spesialis Gigi Tiruan (Prostodontis) (Sp.Pros)", 
                                                            "Dokter Gigi Spesialis Kawat Gigi - Orthodontis (Sp.Ort)",
                                                            "Dokter Gigi Spesialis Konservasi Gigi / Endodonsi (Sp.KG)", 
                                                            "Dokter Gigi Spesialis lainnya yang belum tercantum", 
                                                            "Dokter Gigi Spesialis Penyakit Mulut (Sp.PM)", 
                                                            "Dokter Gigi Spesialis Periodonsia (Sp.Perio)", 
                                                            "Dokter Gigi Spesialis Radiologi kedokteran gigi (Sp.RKG)")] <- "Dokter Gigi Spesialis"


#Dokter Spesialis
master_nakes2$sdmk_dokter[master_nakes2$sdmk_dokter %in% c ("Dokter Spesialis Akupunktur Klinik (Sp.Ak)",
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

#Dokter Sub

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
#Non Dokter

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

#Recode bisa juga dengan menggunakan kode berikut:
master_nakes2$jenis_sdmk=as.factor(master_nakes2$jenis_sdmk)
master_nakes2$sdmk_dokter <- recode_factor(master_nakes2$jenis_sdmk, 'Dokter' = "Dokter Umum", 'Bidan (Asisten)' =  "Non Dokter", 'Ahli Madya Farmasi (Asisten Apoteker)' = "Non Dokter", 'Perawat Medikal Bedah' = "Non Dokter", 'Perawat (Non Ners)' = "Non Dokter", 'Administrasi dan Kebijakan Kesehatan' = "Non Dokter", 'Ahli Teknologi Laboratorium Medik (Analis Kesehatan)' = "Non Dokter", 'Analis Farmasi' = "Non Dokter", 'Analis Kesehatan (Asisten)' = "Non Dokter", 'Apoteker' = "Non Dokter", 'Aset' = "Non Dokter", 'Bidan Desa' = "Non Dokter", 'Bidan Klinis'="Non Dokter", 'Bidan Pendidik'="Non Dokter", 'Dietisien'="Non Dokter",  'Direktur Jenderal / Kepala Badan Setingkat Es.1'="Non Dokter", 'Elektromedis'="Non Dokter", 'Entomolog Kesehatan'="Non Dokter", 'Epidemiolog Kesehatan'="Non Dokter", 'Evaluasi'="Non Dokter", 'Farmasi (Asisten)'="Non Dokter", 'Fisikawan Medik'="Non Dokter", 'Fisioterapis'="Non Dokter", 'Gaji'="Non Dokter", 'Gaji dan Umum'="Non Dokter", 'Gizi (asisten)'="Non Dokter", 'Guru'="Non Dokter", 'Hubungan Masyarakat'="Non Dokter",'Hukum'="Non Dokter", 'Informatika Kesehatan'="Non Dokter", 'Instruktur'="Non Dokter", 'Jaminan Kesehatan'="Non Dokter", 'Jenis Bidan Lainnya yang belum tercantum'="Non Dokter", 'Jenis Perawat Lainnya yang belum tercantum'="Non Dokter", 'Juru Mudi'="Non Dokter", 'Keamanan'="Non Dokter",  'Kepala Bagian / Kepala UPT setingkat Es. 3'="Non Dokter", 'Kepala Bidang'="Non Dokter", 'Kepala Dinas'="Non Dokter",  'Kepala Pusat / Direktur Setingkat Es.2'="Non Dokter", 'Kepala Puskesmas'="Non Dokter", 'Kepala Seksi'="Non Dokter", 'Kepala Subbagian / Kepala UPT setingkat Es.4'="Non Dokter", 'Kepala Subbidang'="Non Dokter", 'Kesehatan Kerja'="Non Dokter", 'Kesehatan Lingkungan (Asisten)'="Non Dokter", 'Kesehatan Masyarakat (Lainnya)'="Non Dokter", 'Keuangan'="Non Dokter", 'Lektor'="Non Dokter", 'Mutasi Pegawai'="Non Dokter", 'Ners'="Non Dokter", 'Nutrisionis'="Non Dokter", 'Okupasi Terapis'="Non Dokter", 'Organisasi'="Non Dokter", 'Pekarya'="Non Dokter", 'Pekerja sosial'="Non Dokter", 'Pelaporan'="Non Dokter", 'Penata Anestesi'="Non Dokter", 'Pengarsipan'="Non Dokter", 'Pengelola Data'="Non Dokter", 'Pengelola Jaringan Komputer'="Non Dokter", 'Pengelola Program Gizi dan KIA'="Non Dokter", 'Pengelola Program Kefarmasian dan Alat Kesehatan'="Non Dokter", 'Pengelola Program Kesehatan Lainnya (yang belum tercantum)'="Non Dokter", 'Pengelola Program Penelitian dan Pengembangan Kesehatan'="Non Dokter", 'Pengelola Program Pengembangan dan Pemberdayaan SDM Kesehatan'="Non Dokter", 'Pengelola Program Pengendalian Penyakit dan Penyehatan Lingkungan'="Non Dokter", 'Pengelola Program Upaya Kesehatan'="Non Dokter", 'Pengelola Sistem Informasi dan Basis Data (Database)'="Non Dokter", 'Pengembangan Pegawai'="Non Dokter", 'Perawat Asisten)'="Non Dokter", 'Perawat Geriatri - Lansia'="Non Dokter", 'Perawat Kesehatan Anak'="Non Dokter", 'Perawat Kesehatan Jiwa'="Non Dokter", 'Perawat Komunitas'="Non Dokter", 'Perawat Maternitas'="Non Dokter", 'Perekam Medis dan Informasi Kesehatan'="Non Dokter", 'Perencanaan'="Non Dokter", 'Perpustakaan'="Non Dokter", 'Petugas IPSRS / Teknisi pemeliharaan fasilitas'="Non Dokter", 'Petugas Kamar Jenazah / Pemulasaran Jenazah'="Non Dokter", 'Petugas Pengelola Limbah'="Non Dokter", 'Program'="Non Dokter", 'Promosi Kesehatan'="Non Dokter", 'Psikologi Klinis'="Non Dokter", 'Publikasi dan Informasi Publik'="Non Dokter", 'Radiografer'="Non Dokter", 'Radioterapis'="Non Dokter", 'Refraksionis Optisien/Optometris'="Non Dokter", 'Sanitasi Lingkungan'="Non Dokter", 'Sarjana, Magister Farmasi (Non Apoteker)'="Non Dokter", 'Sekretaris / Direktur Setingkat Es.2'="Non Dokter", 'Teknisi Gigi'="Non Dokter", 'Teknisi Kardiovaskular'="Non Dokter", 'Teknisis Pelayanan Darah'="Non Dokter", 'Tenaga Umum Lainnya yang belum tercantum'="Non Dokter", 'Terapis Gigi dan Mulut'="Non Dokter", 'Terapis Gigi dan Mulut (asisten)'="Non Dokter", 'Terapis Wicara'="Non Dokter", 'Wakil Direktur / Deputi Setingkat Es.2'="Non Dokter", 'Widyaiswara Madya'="Non Dokter", 'Widyaiswara Muda'="Non Dokter")
master_nakes2$sdmk_dokter <- recode_factor(master_nakes2$sdmk_dokter, 'Dokter Spesialis Akupunktur Klinik (Sp.Ak)'="Dokter Spesialis", 'Dokter Spesialis Anak (Sp.A)'="Dokter Spesialis", 'Dokter Spesialis Anastesiologi (Sp.An)'="Dokter Spesialis", 'Dokter Spesialis Andrologi (Sp.And)'="Dokter Spesialis", 'Dokter Spesialis Bedah (Sp.B)'="Dokter Spesialis", 'Dokter Spesialis Bedah Anak (Sp.BA)'="Dokter Spesialis", 'Dokter Spesialis Bedah Konsultan Vaskuler (Sp. B Vaskular)'="Dokter Spesialis", 'Dokter Spesialis Bedah Orthopedi'="Dokter Spesialis", 'Dokter Spesialis Bedah Plastik (Sp.BP)'="Dokter Spesialis", 'Dokter Spesialis Bedah Syaraf (Sp.BS)'="Dokter Spesialis", 'Dokter Spesialis Bedah Thoraks Dan Kardiovaskuler (Sp.BTKV)'="Dokter Spesialis", 'Dokter Spesialis Farmakologi Klinik (Sp.FK)'="Dokter Spesialis", 'Dokter Spesialis Forensik (Sp.F)'="Dokter Spesialis", 'Dokter Spesialis Gizi Klinik (Sp.GK)'="Dokter Spesialis", 'Dokter Spesialis Gizi Medik'="Dokter Spesialis", 'Dokter Spesialis Ilmu Kesehatan Kulit Dan Kelamin (Sp.KK)'="Dokter Spesialis", 'Dokter Spesialis Ilmu Kesehatan THT Kl  (Sp.THT-KL)'="Dokter Spesialis", 'Dokter Spesialis Jantung dan Pembuluh Darah (Sp.JP)'="Dokter Spesialis", 'Dokter Spesialis Kedaruratan Medik - Emergency (Sp.EM)'="Dokter Spesialis", 'Dokter Spesialis Kedokteran Fisik Dan Rehabilitasi (Sp.KFR)'="Dokter Spesialis", 'Dokter Spesialis Lainnya yang belum tercantum'="Dokter Spesialis", 'Dokter Spesialis Mata (Sp.M)'="Dokter Spesialis", 'Dokter Spesialis Mikrobiologi Klinik (Sp.MK)'="Dokter Spesialis", 'Dokter Spesialis Neorologi/Saraf (Sp.S)'="Dokter Spesialis", 'Dokter Spesialis Nuklir (Sp.KN)'="Dokter Spesialis", 'Dokter Spesialis Obstetri & Ginekologi (Sp.OG)'="Dokter Spesialis", 'Dokter Spesialis Ofthalmologi'="Dokter Spesialis", 'Dokter Spesialis Okupasi (Sp.OK)'="Dokter Spesialis", 'Dokter Spesialis Onkologi Radiasi (Sp.Onk.Rad)'="Dokter Spesialis", 'Dokter Spesialis Orthopedi & Traumatologi (Sp.OT)'="Dokter Spesialis", 'Dokter Spesialis Parasitologi Klinik (Sp.ParK)'="Dokter Spesialis", 'Dokter Spesialis Paru  - Pulmonologi (Sp.P)'="Dokter Spesialis", 'Dokter Spesialis Patologi Anatomi (Sp.PA)'="Dokter Spesialis", 'Dokter Spesialis Patologi Forensik'="Dokter Spesialis", 'Dokter Spesialis Patologi Klinik (SP.PK)'="Dokter Spesialis", 'Dokter Spesialis Penyakit Dalam (Sp.PD)'="Dokter Spesialis", 'Dokter Spesialis Psikiatri - Kedokteran Jiwa (Sp.KJ)'="Dokter Spesialis", 'Dokter Spesialis Radiologi (Sp.Rad)'="Dokter Spesialis", 'Dokter Spesialis Radioterapi'="Dokter Spesialis", 'Dokter Spesialis Rehabilitasi Medik (Sp.RM)'="Dokter Spesialis", 'Dokter Spesialis Urologi (Sp.U)'="Dokter Spesialis", 'Dokter Sub Spesialis Lainnya (yang belum tercantum)'="Dokter Subspesialis")
master_nakes2$sdmk_dokter <- recode_factor(master_nakes2$sdmk_dokter, 'Subspesialis  Hip and Knee'="Dokter Subspesialis", 'Subspesialis  Nefrologi'="Dokter Subspesialis", 'Subspesialis Alergi imunologi'="Dokter Subspesialis", 'Subspesialis Aritmia'="Dokter Subspesialis", 'Subspesialis Bedah Digestif Anak'="Dokter Subspesialis", 'Subspesialis Bedah Estetik Lanjut'="Dokter Subspesialis", 'Subspesialis Digestif'="Dokter Subspesialis", 'Subspesialis Ekokardiografi'="Dokter Subspesialis", 'Subspesialis Emergensi dan Rawat Intensif Anak (ERIA)'="Dokter Subspesialis", 'Subspesialis Endokrin metabolik'="Dokter Subspesialis", 'Subspesialis Endokrinologi'="Dokter Subspesialis", 'Subspesialis Fertilitas dan endokrinologi reproduksi'="Dokter Subspesialis", 'Subspesialis Feto Maternal'="Dokter Subspesialis", 'Subspesialis Gastroenterologi-hepatologi'="Dokter Subspesialis", 'Subspesialis Gastrohepatologi'="Dokter Subspesialis", 'Subspesialis Geriatri'="Dokter Subspesialis", 'Subspesialis Ginjal hipertensi'="DOkter Subspesialis", 'Subspesialis Hematologi'="Dokter Subspesialis", 'Subspesialis Hematologi onkologi'="Dokter Subspesialis", 'Subspesialis Hematologi Onkologi'="Dokter Subspesialis", 'Subspesialis Infeksi dan penyakit tropik'="Dokter Subspesialis", 'Subspesialis Infeksi Menular Seksual'="Dokter Subspesialis", 'Subspesialis Infeksi Paru'="Dokter Subspesialis", 'Subspesialis Intensive Care'="Dokter Subspesialis", 'Subspesialis Intervensi dan gawat nafas'="Dokter Subspesialis", 'Subspesialis Jantung Anak dan PJB'="Dokter Subspesialis", 'Subspesialis Kardiologi'="Dokter Subspesialis", 'Subspesialis Kardiologi Intervensi'="Dokter Subspesialis", 'Subspesialis Kardiovaskuler'="Dokter Subspesialis", 'Subspesialis Kedokteran Vaskular'="Dokter Subspesialis", 'Subspesialis Kesehatan Jiwa Anak dan Remaja'="Dokter Subspesialis", 'Subspesialis Neonatologi'="Dokter Subspesialis", 'Subspesialis Neurointervensi'="Dokter Subspesialis", 'Subspesialis Neurologi'="Dokter Subspesialis", 'Subspesialis Neuroradiologi kepala leher'="Dokter Subspesialis", 'Subspesialis Nutrisi dan penyakit metabolik'="Dokter Subspesialis", 'Subspesialis Obstetri ginekologi sosial'="Dokter Subspesialis", 'Subspesialis Onkologi'="Dokter Subspesialis", 'Subspesialis Onkologi bedah kepala leher'="Dokter Subspesialis", 'Subspesialis Onkologi ginekologi'="Dokter Subspesialis", 'Subspesialis Orthopedic Spine'="Dokter Subspesialis", 'Subspesialis Otologi'="Dokter Subspesialis", 'Subspesialis Pediatri'="Dokter Subspesialis", 'Subspesialis Pediatri sosial-tumbuh kembang'="Dokter Subspesialis", 'Subspesialis Pediatric Orthopaedic'="Dokter Subspesialis", 'Subspesialis Perawatan Intensif dan Kegawatan Kardiovaskular'="Dokter Subspesialis", 'Subspesialis Psikiatri Adiksi'="Dokter Subspesialis", 'Subspesialis Psikogeriatri'="Dokter Subspesialis", 'Subspesialis Psikoterapi'="Dokter Subspesialis", 'Subspesialis Pulmonologi'="Dokter Subspesialis", 'Subspesialis Radiologi Abdomen'="Dokter Subspesialis", 'Subspesialis Radiologi Anak'="Dokter Subspesialis", 'Subspesialis Radiologi Thoraks'="Dokter Subspesialis", 'Subspesialis Rematologi'="Dokter Subspesialis", 'Subspesialis Respirologi'="Dokter Subspesialis", 'Subspesialis THT komunitas'="Dokter Subspesialis", 'Subspesialis Tropik infeksi'="Dokter Subspesialis", 'Subspesialis Uroginekologi dan rekonstruksi'="Dokter Subspesialis", 'Subspesialis Urologi Onkologi'="Dokter Subspesialis")
master_nakes2$sdmk_dokter <- recode_factor(master_nakes2$sdmk_dokter, 'Dokter Gigi Spesialis Anak - Pedodontis (Sp.KGA)'="drg spesialis", 'Dokter Gigi Spesialis Bedah mulut / Maksilofasial (Sp.BM)'="drg spesialis", 'Dokter Gigi Spesialis Gigi Tiruan (Prostodontis) (Sp.Pros)'="drg spesialis", 'Dokter Gigi Spesialis Kawat Gigi - Orthodontis (Sp.Ort)'="drg spesialis", 'Dokter Gigi Spesialis Konservasi Gigi / Endodonsi (Sp.KG)'="drg spesialis", 'Dokter Gigi Spesialis lainnya yang belum tercantum'="drg spesialis", 'Dokter Gigi Spesialis Penyakit Mulut (Sp.PM)'="drg spesialis", 'Dokter Gigi Spesialis Periodonsia (Sp.Perio)'="drg spesialis", 'Dokter Gigi Spesialis Radiologi kedokteran gigi (Sp.RKG)'="drg spesialis")
master_nakes2$sdmk_dokter <- recode_factor(master_nakes2$sdmk_dokter, 'drg Spesialis'="drg spesialis", 'DOkter Subspesialis'="Dokter Subspesialis")                                          




table(master_nakes2$sdmk_dokter)  
length(master_nakes2$sdmk_dokter)


#5. Buat Dataset baru yang berisikan Dokter saja (Dokter Umum, Dokter Spesialis, Dokter Subspesialis)
#menghapus row "Non Dokter"
master_dokter_jabar = filter(master_nakes2, sdmk_dokter!="Dokter Gigi Umum", sdmk_dokter!="Dokter Gigi Spesialis",  sdmk_dokter!="Non Dokter")

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
