install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(dplyr)
police_df = read.csv("C:/Users/kamjo/geonggi_pol.csv", header = T)
crime_df = read.csv("C:/Users/kamjo/crime.csv", header = T)

police <- police_df %>% filter(구분명 == '파출소')
earth <- police_df %>% filter(구분명 == '지구대')
View(police)
View(earth)
View(crime_df)

#--------------------------파출소 지역별 나누기-----------
pol_ga = police %>%filter(시군명 == '가평군')
pol_ko =  police%>%filter(시군명 == '고양시')
pol_kwa = police %>%filter(시군명 == '과천시')
pol_kwang = police %>%  filter(시군명 == '광명시')
pol_kj = police %>%filter(시군명 == '광주시')
pol_ku = police %>%filter(시군명 == '구리시')
pol_gun = police %>%  filter(시군명 == '군포시')
pol_kim = police %>% filter(시군명 == '김포시')
pol_nam = police %>%  filter(시군명 == '남양주시')
pol_dong = police %>%  filter(시군명 == '동두천시')
pol_bu = police %>%  filter(시군명 == '부천시')
pol_sung= police %>%  filter(시군명 == '성남시')
pol_su = police %>%  filter(시군명 == '수원시')
pol_si = police %>%  filter(시군명 == '시흥시')
pol_an = police %>%  filter(시군명 == '안산시')
pol_ansung = police %>%  filter(시군명 == '안성시')
pol_anyang = police %>%  filter(시군명 == '안양시')
pol_yangj = police %>%  filter(시군명 == '양주시')
pol_yangp= police %>%  filter(시군명 == '양평군')
pol_yj = police %>%  filter(시군명 == '여주시')
pol_yc = police %>%  filter(시군명 == '연천군')
pol_os = police %>%  filter(시군명 == '오산시')
pol_yi = police %>%  filter(시군명 == '용인시')
pol_oo = police %>%  filter(시군명 == '의왕시')
pol_ojb = police %>%  filter(시군명 == '의정부시')
pol_ec = police %>%  filter(시군명 == '이천시')
pol_pj = police %>%  filter(시군명 == '파주시')
pol_peong = police %>%  filter(시군명 == '평택시')
pol_po = police %>%  filter(시군명 == '포천시')
pol_ha = police %>%  filter(시군명 == '하남시')
pol_hwa = police %>%  filter(시군명 == '화성시')

#---------------------------파출소 데이터프레임만들기-----------
pol_df <-data.frame(count(pol_ga),count(pol_ko),count(pol_kwa),count(pol_kwang),
                    count(pol_kj),count(pol_ku),count(pol_gun),count(pol_kim),
                    count(pol_nam),count(pol_dong),count(pol_bu),count(pol_sung),
                    count(pol_su),count(pol_si),count(pol_an),count(pol_ansung),
                    count(pol_anyang),count(pol_yangj),count(pol_yangp),count(pol_yj),
                    count(pol_yc),count(pol_os),count(pol_yi),count(pol_oo),
                    count(pol_ojb),count(pol_ec),count(pol_pj),count(pol_peong),
                    count(pol_po),count(pol_ha),count(pol_hwa))
pol_df
pol_c <- c(count(pol_ga),count(pol_ko),count(pol_kwa),count(pol_kwang),
           count(pol_kj),count(pol_ku),count(pol_gun),count(pol_kim),
           count(pol_nam),count(pol_dong),count(pol_bu),count(pol_sung),
           count(pol_su),count(pol_si),count(pol_an),count(pol_ansung),
           count(pol_anyang),count(pol_yangj),count(pol_yangp),count(pol_yj),
           count(pol_yc),count(pol_os),count(pol_yi),count(pol_oo),
           count(pol_ojb),count(pol_ec),count(pol_pj),count(pol_peong),
           count(pol_po),count(pol_ha),count(pol_hwa))

poll <- list(pol_c)
poll
#-----------------------지구대 지역별나누기----------
earth_ga = earth %>% filter(시군명 == '가평군')
earth_ko =  earth%>%filter(시군명 == '고양시')
earth_kwa = earth %>%filter(시군명 == '과천시')
earth_kwang = earth %>%  filter(시군명 == '광명시')
earth_kj = earth %>%filter(시군명 == '광주시')
earth_ku = earth %>%filter(시군명 == '구리시')
earth_gun = earth %>%  filter(시군명 == '군포시')
earth_kim = earth %>% filter(시군명 == '김포시')
earth_nam = earth %>%  filter(시군명 == '남양주시')
earth_dong = earth %>%  filter(시군명 == '동두천시')
earth_bu = earth %>%  filter(시군명 == '부천시')
earth_sung= earth %>%  filter(시군명 == '성남시')
earth_su = earth %>%  filter(시군명 == '수원시')
earth_si = earth %>%  filter(시군명 == '시흥시')
earth_an = earth %>%  filter(시군명 == '안산시')
earth_ansung = earth %>%  filter(시군명 == '안성시')
earth_anyang = earth %>%  filter(시군명 == '안양시')
earth_yangj = earth %>%  filter(시군명 == '양주시')
earth_yp = earth %>% filter(시군명 == '양평군')
earth_yj = earth %>%  filter(시군명 == '여주시')
earth_yc = earth %>% filter(시군명 == '연천군')
earth_os = earth %>%  filter(시군명 == '오산시')
earth_yi = earth %>%  filter(시군명 == '용인시')
earth_oo = earth %>%  filter(시군명 == '의왕시')
earth_ojb = earth %>%  filter(시군명 == '의정부시')
earth_ec = earth %>%  filter(시군명 == '이천시')
earth_pj = earth %>%  filter(시군명 == '파주시')
earth_peong = earth %>%  filter(시군명 == '평택시')
earth_po = earth %>%  filter(시군명 == '포천시')
earth_ha = earth %>%  filter(시군명 == '하남시')
earth_hwa = earth %>%  filter(시군명 == '화성시')

#------------------------------지구대 데이터프레임만들기-----
earth_df <-data.frame(c(count(earth_ga),count(earth_ko),count(earth_kwa),count(earth_kwang),
                        count(earth_kj),count(earth_ku),count(earth_gun),count(earth_kim),
                        count(earth_nam),count(earth_dong),count(earth_bu),count(earth_sung),
                        count(earth_su),count(earth_si),count(earth_an),count(earth_ansung),
                        count(earth_anyang),count(earth_yangj),count(earth_yp),count(earth_yj),
                        count(earth_yc),count(earth_os),count(earth_yi),count(earth_oo),
                        count(earth_ojb),count(earth_ec),count(earth_pj),count(earth_peong),
                        count(earth_po),count(earth_ha),count(earth_hwa)))

earth_c <- c(count(earth_ga),count(earth_ko),count(earth_kwa),count(earth_kwang),
             count(earth_kj),count(earth_ku),count(earth_gun),count(earth_kim),
             count(earth_nam),count(earth_dong),count(earth_bu),count(earth_sung),
             count(earth_su),count(earth_si),count(earth_an),count(earth_ansung),
             count(earth_anyang),count(earth_yangj),count(earth_yp),count(earth_yj),
             count(earth_yc),count(earth_os),count(earth_yi),count(earth_oo),
             count(earth_ojb),count(earth_ec),count(earth_pj),count(earth_peong),
             count(earth_po),count(earth_ha),count(earth_hwa))
earth_c
earthh <- list(earth_c)
earth_df

#--------------------범죄율 지역별 나누기-----------
crime_ga = 0
crime_ko = crime_df %>% select(경기.고양)
sum(crime_ko)
crime_kwa = crime_df %>% select(경기.과천)
crime_kwang = crime_df %>% select(경기.광명)
crime_kj= crime_df %>% select(경기.광주)
crime_ku = crime_df %>% select(경기.구리)
crime_gun = crime_df %>% select(경기.군포)
crime_kim = crime_df %>% select(경기.김포)
crime_nam = crime_df %>% select(경기.남양주)
crime_dong = crime_df %>% select(경기.동두천)
crime_bu= crime_df %>% select(경기.부천)
crime_sung = crime_df %>% select(경기.성남)
crime_su = crime_df %>% select(경기.수원)
crime_si = crime_df %>% select(경기.시흥)
crime_an = crime_df %>% select(경기.안산)
crime_ansung = crime_df %>% select(경기.안성)
crime_anyang = crime_df %>% select(경기.안양)
crime_yangj = crime_df %>% select(경기.양주)
crime_yp = 0
crime_yj = crime_df %>% select(경기.여주)
crime_yc = 0
crime_os = crime_df %>% select(경기.오산)
crime_yi = crime_df %>% select(경기.용인)
crime_oo = crime_df %>% select(경기.의왕)
crime_ojb = crime_df %>% select(경기.의정부)
crime_ec = crime_df %>% select(경기.이천)
crime_pj = crime_df %>% select(경기.파주)
crime_peong = crime_df %>% select(경기.평택)
crime_po = crime_df %>% select(경기.포천)
crime_ha = crime_df %>% select(경기.하남)
crime_hwa = crime_df %>% select(경기.화성)

#---------------------범죄율 데이터프레임만들기----------
crime_df1 <-data.frame(c(crime_ga,sum(crime_ko),sum(crime_kwa),sum(crime_kwang),
                         sum(crime_kj),sum(crime_ku),sum(crime_gun),sum(crime_kim),
                         sum(crime_nam),sum(crime_dong),sum(crime_bu),sum(crime_sung),
                         sum(crime_su),sum(crime_si),sum(crime_an),sum(crime_ansung),
                         sum(crime_anyang),sum(crime_yangj),crime_yp,sum(crime_yj),
                         crime_yc,sum(crime_os),sum(crime_yi),sum(crime_oo),
                         sum(crime_ojb),sum(crime_ec),sum(crime_pj),sum(crime_peong),
                         sum(crime_po),sum(crime_ha),sum(crime_hwa)))

str(crime_df1)
earth_list <-list(count(earth_ga),count(earth_ko),count(earth_kwa),count(earth_kwang),
                  count(earth_kj),count(earth_ku),count(earth_gun),count(earth_kim),
                  count(earth_nam),count(earth_dong),count(earth_bu),count(earth_sung),
                  count(earth_su),count(earth_si),count(earth_an),count(earth_ansung),
                  count(earth_anyang),count(earth_yangj),count(earth_yp),count(earth_yj),
                  count(earth_yc),count(earth_os),count(earth_yi),count(earth_oo),
                  count(earth_ojb),count(earth_ec),count(earth_pj),count(earth_peong),
                  count(earth_po),count(earth_ha),count(earth_hwa))

crime_list <-list(crime_ga,count(crime_ko),count(crime_kwa),count(crime_kwang),
                  count(crime_kj),count(crime_ku),count(crime_gun),count(crime_kim),
                  count(crime_nam),count(crime_dong),count(crime_bu),count(crime_sung),
                  count(crime_su),count(crime_si),count(crime_an),count(crime_ansung),
                  count(crime_anyang),count(crime_yangj),crime_yp,count(crime_yj),
                  crime_yc,count(crime_os),count(crime_yi),count(crime_oo),
                  count(crime_ojb),count(crime_ec),count(crime_pj),count(crime_peong),
                  count(crime_po),count(crime_ha),count(crime_hwa))

crime_c <- c(crime_ga,count(crime_ko),count(crime_kwa),count(crime_kwang),
             count(crime_kj),count(crime_ku),count(crime_gun),count(crime_kim),
             count(crime_nam),count(crime_dong),count(crime_bu),count(crime_sung),
             count(crime_su),count(crime_si),count(crime_an),count(crime_ansung),
             count(crime_anyang),count(crime_yangj),crime_yp,count(crime_yj),
             crime_yc,count(crime_os),count(crime_yi),count(crime_oo),
             count(crime_ojb),count(crime_ec),count(crime_pj),count(crime_peong),
             count(crime_po),count(crime_ha),count(crime_hwa))

crimee_c <-c(crime_ga,sum(crime_ko),sum(crime_kwa),sum(crime_kwang),
             sum(crime_kj),sum(crime_ku),sum(crime_gun),sum(crime_kim),
             sum(crime_nam),sum(crime_dong),sum(crime_bu),sum(crime_sung),
             sum(crime_su),sum(crime_si),sum(crime_an),sum(crime_ansung),
             sum(crime_anyang),sum(crime_yangj),crime_yp,sum(crime_yj),
             crime_yc,sum(crime_os),sum(crime_yi),sum(crime_oo),
             sum(crime_ojb),sum(crime_ec),sum(crime_pj),sum(crime_peong),
             sum(crime_po),sum(crime_ha),sum(crime_hwa))

View(crimee_c)
crimee_c
crimee <- list(crime_c)
str(crime_list)
earth_list
crime_list
earth_df
crime_df
(is.na(crime_df))

test_d <- data_frame(earth_c, crimee_c)
View(test_d)
test_d1 <- data_frame(pol_c, crimee_c)
View(test_d1)
ggplot(data = test_d) + geom_point()
as.numeric(test_d$earth_c)
print(test_d$earth_c)

#-----지역별 범죄사건, 지구대 수 -----
cor(as.numeric(test_d$earth_c), test_d$crimee_c)
barplot(test_d1$crimee_c, as.numeric(test_d$earth_c),col = 1:40)
plot(test_d$earth_c, test_d$crimee_c, col = 1:40)

#-------지역별 범죄사건, 파출소 수-----
cor(as.numeric(test_d1$pol_c),test_d$crimee_c)

barplot(test_d$crimee_c,as.numeric(test_d1$pol_c), col = 1:40)

plot(as.numeric(test_d1$pol_c), test_d$crimee_c, col = 1:40)



crime_ko = crime_df %>% select(경기.고양)sum(crime_df1)
count(police_ko)
View(police_ko)
View(crime_df1)
View(crime_df)
View(police_df)
table(is.na(police_df))
table(is.na(crime_df))
police_df %>% filter(is.na(police_df))

str(police_df)
str(crime_df)
cor(police_df, crime_df)
