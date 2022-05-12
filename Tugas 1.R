#Tugas 1: Subsetting dan konversi variabel 
#Aktivasi package dplyr
library(dplyr)

#membaca data dari URL
library(readr)
c_data <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/c_data.csv")

library(data.table)
pef <- fread("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv")

w5 <- fread("https://raw.githubusercontent.com/dwi-agustian/biostat/main/w5.csv")

glimpse(pef)
summary(pef)
glimpse(w5)
summary(w5)

#1. Join data pef dan w5
# Convert character to number: pidlink
w5 <- w5 %>%
  mutate(pidlink_num = as.integer(pidlink))

#mengcopy paste variable pidlink asli
w5$pidlink_chr = w5$pidlink

#mereplace pidlink dengan versi int(num)
w5$pidlink = w5$pidlink_num

glimpse(w5)

#combining dataset (menggabungkan data)
# menggabungkan variables dengan common variable
w5_pef_fj = full_join(w5, pef, by = "pidlink")

summary(w5_pef_fj)

#2. subsetting variables
#memilih berdasarkan nama variables


names(w5_pef_fj)
w5_pef_fj_ss  = select(w5_pef_fj, pidlink, age, agegr, sex, pef, height, Asthma)

w5_pef_fj_komplit = filter(w5_pef_fj_ss,!is.na(pef)&!is.na(height)&!is.na(age)&!is.na(sex)&!is.na(Asthma)&!is.na(agegr))

glimpse(w5_pef_fj_komplit)

w5_pef_fj_komplit2 = filter(w5_pef_fj_komplit, Asthma!="NAAsth", Asthma!="Don't Know-Asth")

glimpse(w5_pef_fj_komplit2)


#3. Konversi variabel yang bersifat seharusnya category (factor)

table(w5_pef_fj_komplit2$agegr)
table(w5_pef_fj_komplit2$Asthma)
table(w5_pef_fj_komplit2$sex)

class(w5_pef_fj_komplit2$agegr)
class(w5_pef_fj_komplit2$Asthma)
class(w5_pef_fj_komplit2$sex)

w5_pef_fj_komplit2$agegr = as.factor(w5_pef_fj_komplit2$agegr)
w5_pef_fj_komplit2$Asthma = as.factor(w5_pef_fj_komplit2$Asthma)
w5_pef_fj_komplit2$sex = as.factor(w5_pef_fj_komplit2$sex)

glimpse(w5_pef_fj_komplit2)

summary(w5_pef_fj_komplit2)

#4. Code dan Dataset komplit di upload di github

#Ekxport dataset
#Cara 1:
write.table(w5_pef_fj_komplit2, file="w5_pef_fj_komplit_OK.csv", row.names=F, sep=",")

#Cara 2: 
write.csv(w5_pef_fj_komplit2, file="w5_pef_fj_komplit2.csv", row.names = F)
