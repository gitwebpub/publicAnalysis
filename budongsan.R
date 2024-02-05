library(httr)
library(jsonlite)
library(writexl)
library(data.table)
library(httr)
library(janitor)
# LAWD_CD
# DEAL_YMD
# serviceKey

# u <- GET('http://openapi.molit.go.kr/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcOffiRent'
# ?serviceKey=서비스키&LAWD_CD=11110&DEAL_YMD=201512')
# 
# ServiceKey <- 'DLu%2Fm6NPW3yIM7asWMTt6U%2BqHAgdGo7ivPVv9EV7xasQ8HFUhuPr5KtOTBCOJ8AbFm8Js6FDrqOtKndgPDPMPw%3D%3D'
# 
# ju <- content(u, as = 'text', encoding = 'utf-8') |> fromJSON()
# ju
# 
# pb <- progress_bar$new(total = 100)
# #
# 오피스텔 전월세 신고정보 조회 기술문서
officeRentData <- function(LAWD_CD, DEAL_YMD) {
  URL <- 'http://openapi.molit.go.kr/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcOffiRent'
  ServiceKey <- 'DLu%2Fm6NPW3yIM7asWMTt6U%2BqHAgdGo7ivPVv9EV7xasQ8HFUhuPr5KtOTBCOJ8AbFm8Js6FDrqOtKndgPDPMPw%3D%3D'
  ServiceKey <- URLdecode(ServiceKey)
  params <- list(
    ServiceKey = ServiceKey,
    LAWD_CD = LAWD_CD,
    DEAL_YMD = DEAL_YMD
  )
  res <- GET(url = URL, query = params)
  json <- content(res, as = 'text', encoding = 'utf-8') |> fromJSON()
  str(json)
  dat <- json$response$body$items$item |> as.data.table()
  dat
}
# 11110
# 202210
# 남양주 41360
Rdat.ujb12 <- officeRentData(41150, 202312)
Rdat.ujb11 <- officeRentData(41150, 202311)

Rdat.nyj12 <- officeRentData(41360, 202312)
Rdat.nyj11 <- officeRentData(41360, 202311)

Rdat.hs12 <- officeRentData(41590, 202312)
Rdat.hs11 <- officeRentData(41590, 202311)

Rdat.sn.bd12 <- officeRentData(41135, 202312)
Rdat.sn.bd11 <- officeRentData(41135, 202311)


#411300
#41135


Rdat.ujb <- rbind(Rdat.ujb11,Rdat.ujb12)
Rdat.nyj <- rbind(Rdat.nyj11,Rdat.nyj12)
Rdat.hs <- rbind(Rdat.hs11,Rdat.hs12)
Rdat.sn.bd <- rbind(Rdat.sn.bd11,Rdat.sn.bd12)

Rdat.ujb %>% janitor::tabyl(법정동)
Rdat.ujb.MEAN <- Rdat.ujb |> group_by(법정동) |> summarise(Rent = mean(월세, na.rm = TRUE))
Rdat.ujb.MEAN$EMD_KOR_NM <- Rdat.ujb.MEAN$법정동


Rdat.nyj %>% janitor::tabyl(법정동)
Rdat.nyj$법정동
library(stringr)
# Splitting the 법정동 values and taking the first part
Rdat.ujb$법정동 <- str_split_fixed(Rdat.ujb$법정동, " ", 2)[, 1]
Rdat.nyj$법정동 <- str_split_fixed(Rdat.nyj$법정동, " ", 2)[, 1]
Rdat.hs$법정동 <- str_split_fixed(Rdat.hs$법정동, " ", 2)[, 1]
Rdat.sn.bd$법정동 <- str_split_fixed(Rdat.sn.bd$법정동, " ", 2)[, 1]

Rdat.nyj.MEAN <- Rdat.nyj |> group_by(법정동) |> summarise(Rent = mean(월세, na.rm = TRUE))
Rdat.nyj.MEAN$EMD_KOR_NM <- Rdat.nyj.MEAN$법정동

Rdat.hs.MEAN <- Rdat.hs |> group_by(법정동) |> summarise(Rent = mean(월세, na.rm = TRUE))
Rdat.hs.MEAN$EMD_KOR_NM <- Rdat.hs.MEAN$법정동

Rdat.sn.bd.MEAN <- Rdat.sn.bd |> group_by(법정동) |> summarise(Rent = mean(월세, na.rm = TRUE))
Rdat.sn.bd.MEAN$EMD_KOR_NM <- Rdat.sn.bd.MEAN$법정동

Rdat.sn.bd.MEAN
table(Rdat.sn.bd$월세)
#mean(Rdat.sn.bd$월세)
#median(Rdat.sn.bd$월세)







ujb <- tdf %>% 
  filter(str_starts(EMD_CD, "4115")) |>
  left_join(Rdat.ujb.MEAN)

nyj <- tdf %>% 
  filter(str_starts(EMD_CD, "41360")) |>
  left_join(Rdat.nyj.MEAN)

hs <- tdf %>% 
  filter(str_starts(EMD_CD, "41590")) |>
  left_join(Rdat.hs.MEAN)

sn.bd <- tdf %>% 
  filter(str_starts(EMD_CD, "41135")) |>
  left_join(Rdat.sn.bd.MEAN)





##
library(tidyverse)
library(sf)
library(mapproj)
library(tibble)
# 대한민국 최신 행정구역(SHP) 다운로드
# http://www.gisdeveloper.co.kr/?p=2332
st_read('/Users/dshin/DataspellProjects/Pcheon/Map/emd_20230729/emd.shp') %>% 
  as.tibble |>
  mutate(EMD_KOR_NM = iconv(EMD_KOR_NM, from = "cp949", to ="utf-8"))->tdf
# https://lostineconomics.netlify.app/post/2017/11/03/sf-package-with-ggplot2/

#head(tdf)
# utf-8로 리딩
# https://statkclee.github.io/spatial/geo-gangnam.html

#pcheon <- tdf %>% 
#mutate(SIG_CD = str_sub(EMD_CD, 1, 5)) %>% 
#filter(SIG_CD == 11680 | SIG_CD == 11650) # 강남구(11680)와 서초구(11650) 
# https://m.blog.naver.com/janghanui/222390360384
# 법정동코드목록조회 : https://www.code.go.kr/stdcode/regCodeL.do
# 41 경기 65 포천 000000
# 41 경기 15 의정부 000000
#41150

library(extrafont);library(showtext)
font_add_google(name = "Noto Serif KR",
                family = "noto-serif")
showtext_auto(TRUE)

nyj
ujb_shp <- ujb |> 
  mutate(cn = st_centroid(ujb$geometry)) |>
  mutate(cn_coords = st_coordinates(cn),
         X_coord = cn_coords[, 1],
         Y_coord = cn_coords[, 2])
nyj_shp <- nyj |> 
  mutate(cn = st_centroid(nyj$geometry)) |>
  mutate(cn_coords = st_coordinates(cn),
         X_coord = cn_coords[, 1],
         Y_coord = cn_coords[, 2])
hs_shp <- hs |> 
  mutate(cn = st_centroid(hs$geometry)) |>
  mutate(cn_coords = st_coordinates(cn),
         X_coord = cn_coords[, 1],
         Y_coord = cn_coords[, 2])
sn.bd_shp <- sn.bd |> 
  mutate(cn = st_centroid(sn.bd$geometry)) |>
  mutate(cn_coords = st_coordinates(cn),
         X_coord = cn_coords[, 1],
         Y_coord = cn_coords[, 2])





ujb.plot <- ggplot(data = ujb, aes(geometry = geometry)) + 
  geom_sf(aes(fill = Rent)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 80, type = "continuous")) +
  geom_text(data = ujb_shp,
            family = "noto-serif",
            fontface = "bold",
            size = 2.6,
            aes(x = X_coord,
                y = Y_coord,
                label = paste(EMD_KOR_NM, sep = "\n"))) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

nyj.plot <- ggplot(data = nyj, aes(geometry = geometry)) + 
  geom_sf(aes(fill = Rent)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 80, type = "continuous")) +
  geom_text(data = nyj_shp,
            family = "noto-serif",
            fontface = "bold",
            size = 2.6,
            aes(x = X_coord,
                y = Y_coord,
                label = paste(EMD_KOR_NM, sep = "\n"))) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
hs.plot
hs.plot <- ggplot(data = hs, aes(geometry = geometry)) + 
  geom_sf(aes(fill = Rent)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 80, type = "continuous")) +
  geom_text(data = hs_shp,
            family = "noto-serif",
            fontface = "bold",
            size = 2.6,
            aes(x = X_coord,
                y = Y_coord,
                label = paste(EMD_KOR_NM, sep = "\n"))) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

sn.bd.plot <- ggplot(data = sn.bd, aes(geometry = geometry)) + 
  geom_sf(aes(fill = Rent)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 80, type = "continuous")) +
  geom_text(data = sn.bd_shp,
            family = "noto-serif",
            fontface = "bold",
            size = 2.6,
            aes(x = X_coord,
                y = Y_coord,
                label = paste(EMD_KOR_NM, sep = "\n"))) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
sn.bd.plot

hs.plot
Rdat.hs.MEAN

colnames(pcheon_shp)


#font_import(pattern = "D2")

# pcheon.1 <- pcheon |> filter(EMD_KOR_NM %in% c('신읍동','어룡동')) |>
#   mutate(se  = 1)
pcheon.12 <- pcheon.1 |> group_by(se) |>
  mutate(geometry = st_union(geometry)) |> filter(EMD_KOR_NM %in% c('신읍동'))

# pcheon.2 <- pcheon |> filter(EMD_KOR_NM %in% c('선단동','자작동','설운동','동교동')) |>
#   mutate(sd  = 1)
pcheon.22 <- pcheon.2 %>% 
  group_by(sd) |>
  mutate(geometry = st_union(geometry)) |> filter(EMD_KOR_NM %in% c('선단동'))
pcheon.22
# pcheon.n1 <- pcheon |> 
#   filter(!EMD_KOR_NM %in% c('신읍동','어룡동','선단동','자작동','설운동','동교동'))
pcheon.n <- pcheon.n1 |> bind_rows(pcheon.12,pcheon.22) |>
  mutate(area = st_area(geometry))


pcheon_shp <- pcheon.n |> 
  mutate(cn = st_centroid(pcheon.n$geometry)) |>
  mutate(cn_coords = st_coordinates(cn),
         X_coord = cn_coords[, 1],
         Y_coord = cn_coords[, 2])



ujb
pcheon.n
PCpop <- data.frame(readxl::read_excel('/Users/dshin/DataspellProjects/Pcheon/Map/2312EMD.xlsx'))
names(PCpop) <- c("EMD_KOR_NM","house","total","M","F" )
pcheon.nz <- pcheon.n |> left_join(PCpop) |> 
  mutate(popden = total/area) |> select(-se,-sd)



# pcheon.nz

library(wesanderson)
ggplot(data = pcheon.nz, aes(geometry = geometry)) + 
  geom_sf(aes(fill = popden)) + # index_rnd
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 80, type = "continuous")) +
  geom_text(data = pcheon_shp,
            family = "noto-serif",
            fontface = "bold",
            size = 2.6,
            aes(x = X_coord, 
                y = Y_coord, 
                label = paste(EMD_KOR_NM, sep = "\n"))) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())












ggplot(data = pcheon.2, aes(geometry = geometry)) + geom_sf(aes(fill = index_rnd))
nc_union <- nc_new.region %>% 
  group_by(new.region) %>% 
  summarize()





glimpse(pcheon)
# st_union : Creates multiple geometries into a a single geometry, 
# consisiting of all geometry elements
?st_union

# https://www.jla-data.net/eng/merging-geometry-of-sf-objects-in-r/

head(pcheon$EMD_KOR_NM)
# 포천동(신읍동, 어룡동) 
# 선단동(자작동, 설운동, 동교동) 





# https://prohannah.tistory.com/25







head(pcheon_shp)


geom_text(data = gu_name, 
          aes(x = X_coord, 
              y = Y_coord, 
              label = paste(gu, sum_n, sep = "\n")))

# https://hwangknock.tistory.com/6







map_korea$CTPRVN_CD <- iconv(map_korea$CTPRVN_CD,
                             from='CP949',
                             to='UTF-8', 
                             sub=NA,
                             mark=TRUE,
                             toRaw=FALSE)


map_korea_shp <-  as(map_korea, 'Spatial')
map_korea_df <- fortify(map_korea_shp)



str(map_korea_df)


map_korea_ggplot <- map_korea_df %>% 
  ggplot(aes(x=long, y=lat, group = group))

map_korea_ggplot+
  geom_polygon(fill='white', color='black')+
  coord_quickmap()







