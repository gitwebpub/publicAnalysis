library(tidyverse)
library(sf)
library(mapproj)

# 대한민국 최신 행정구역(SHP) 다운로드
# http://www.gisdeveloper.co.kr/?p=2332
st_read('/Users/dshin/DataspellProjects/Pcheon/Map/emd_20230729/emd.shp') ->tdf
# https://lostineconomics.netlify.app/post/2017/11/03/sf-package-with-ggplot2/

head(tdf)

# utf-8로 리딩
tdf <-tdf %>% 
  as.tibble %>%
  mutate(EMD_KOR_NM = iconv(EMD_KOR_NM, from = "cp949", to ="utf-8"))
tdf
# https://statkclee.github.io/spatial/geo-gangnam.html

#pcheon <- tdf %>% 
  #mutate(SIG_CD = str_sub(EMD_CD, 1, 5)) %>% 
  #filter(SIG_CD == 11680 | SIG_CD == 11650) # 강남구(11680)와 서초구(11650) 
# https://m.blog.naver.com/janghanui/222390360384
# 법정동코드목록조회 : https://www.code.go.kr/stdcode/regCodeL.do
# 41 경기 65 포천 000000

pcheon <- tdf %>% 
  filter(str_starts(EMD_CD, "4165")) |>
  mutate(index_rnd = runif(n()))



colnames(pcheon_shp)

library(extrafont)
#font_import(pattern = "D2")


library(showtext)

font_add_google(name = "Noto Serif KR",
                family = "noto-serif")
showtext_auto(TRUE)




pcheon.1 <- pcheon |> filter(EMD_KOR_NM %in% c('신읍동','어룡동')) |>
  mutate(se  = 1)
pcheon.12 <- pcheon.1 |> group_by(se) |>
  mutate(geometry = st_union(geometry)) |> filter(EMD_KOR_NM %in% c('신읍동'))

pcheon.2 <- pcheon |> filter(EMD_KOR_NM %in% c('선단동','자작동','설운동','동교동')) |>
  mutate(sd  = 1)
pcheon.22 <- pcheon.2 %>% 
  group_by(sd) |>
  mutate(geometry = st_union(geometry)) |> filter(EMD_KOR_NM %in% c('선단동'))
pcheon.22
pcheon.n1 <- pcheon |> 
  filter(!EMD_KOR_NM %in% c('신읍동','어룡동','선단동','자작동','설운동','동교동'))
pcheon.n <- pcheon.n1 |> bind_rows(pcheon.12,pcheon.22) |>
  mutate(area = st_area(geometry))

pcheon_shp <- pcheon.n |> 
  mutate(cn = st_centroid(pcheon.n$geometry)) |>
  mutate(cn_coords = st_coordinates(cn),
         X_coord = cn_coords[, 1],
         Y_coord = cn_coords[, 2])

pcheon.n
PCpop <- data.frame(readxl::read_excel('/Users/dshin/DataspellProjects/Pcheon/Map/2312EMD.xlsx'))
names(PCpop) <- c("EMD_KOR_NM","house","total","M","F" )
pcheon.nz <- pcheon.n |> left_join(PCpop) |> 
  mutate(popden = total/area) |> select(-se,-sd)



# pcheon.nz

install.packages("wesanderson")
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




