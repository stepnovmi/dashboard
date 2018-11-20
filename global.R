
library(dplyr)

df = read.csv2("base.csv",encoding  = "UTF-8")
df = as.data.frame(df)
df$Family_Status = as.character(df$Family_Status)
names(df)[names(df) == "Age.range"] <- "Age_Range"
df$Main_SN_profile[df$Main_SN_profile == "facebook"] = "Facebook"
df$Life_attitude[df$Life_attitude == "работа"] = "Работа"
df$Life_attitude[df$Life_attitude == "друзья"] = "Друзья"
df$Family_Status[df$Family_Status == "Замужем / женат / живу в гражданском браке"] = "Замужем / женат / гр. брак"
df$Family_Status[df$Family_Status == "Разведен/ живем порознь"] = "Разведен / живем порознь"
df$Family_Status = as.factor(df$Family_Status)

colnames(df)[75:92] = c("Автомобили","Алкоголь","Банки /Финансы","Безалкогольные напитки","Доставка еды",
                        "Косметика / бытовая химия","Масс медиа","Одежда / обувь","ПО / Приложения",
                        "Покупки онлайн","Продукты питания","Рестораны / кафе","Сеть магазинов","Спортивная одежда",
                        "Телекоммуникации","Техника / электроника","Украшения / часы","Прочее")

df[203:231][df[203:231] == 2] = 1
df[203:231][df[203:231] == 3] = 1



library(readr)
mystring <- read_file("start.txt")


#library(RColorBrewer)
#my.cols <- brewer.pal(9, "Blues")
my.cols = c("#CC3227","#4F6F8C","grey","#7792A6","#C23C33","#3D668C")

my.cols = append(my.cols,my.cols)
my.cols = append(my.cols,my.cols)
my.cols = append(my.cols,my.cols)
my.cols = append(my.cols,my.cols)