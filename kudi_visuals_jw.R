library(tidyverse)
library(sf)
library(tigris)
library(urbnthemes)
library(osmdata)
library(ggmap)
library(tidycensus)
library(ggrepel)
library(lubridate)

options(tigris_use_cache = FALSE)

set_urbn_defaults(style = "map")



# Data read -------------------------------------------------------------


#Read in state, county and MSA maps from Tigris
mi_counties <- counties(state = "MI", cb = FALSE, class = "sf")
detr_msa <- core_based_statistical_areas(cb = FALSE, class = "sf") %>% 
  filter(str_detect(NAME, "Detroit"))

mi <- states(cb = FALSE, class = "sf") %>% 
  filter(NAME == "Michigan")

#Read in city boundaries from Detroit city's open data portal
detr_city <- st_read("https://opendata.arcgis.com/datasets/86b221bb68ca4364afe81d156e54f95c_0.geojson") %>% 
  st_transform(crs = 4269)


county_match <- mi_counties %>% 
  st_join(detr_msa) %>% 
  filter(!is.na(NAME.y)) %>% 
  select(NAME.x) %>% 
  st_drop_geometry() %>%  
  pull()

detroit_tracts <- function(county_name) {
tracts(state = "MI", county = county_name, cb = FALSE, class = "sf")
}


#Select only tracts that fall in the detroit MSA
msa_tracts <- tracts %>% 
  st_join(detr_msa) %>% 
  filter(!is.na(NAME.y))

set_urbn_defaults(style = "map")

holc_detroit <- read_sf("MIDetroit1939.geojson") %>% 
  mutate(grade = case_when(holc_grade == "A" ~ "Best",
                           holc_grade == "B" ~ "Desirable",
                           holc_grade == "C" ~ "Declining",
                           holc_grade == "D" ~ "Hazardous"))



# HOLC Map ----------------------------------------------------------------


bm_holc <- get_stamenmap(bbox = c(left = -83.4050, top = 42.6100, 
                                     right = -82.8000, bottom = 42.1146), zoom = 13, 
                            maptype = "terrain")


ggmap(bm_holc)+
  geom_sf(data = holc_detroit, mapping = aes(fill = factor(grade)),
          col = NA, inherit.aes = FALSE, alpha = 0.7)+
  scale_fill_manual(values = c("#55b748", "#46abdb", "#fccb41", "#db2b27")) +
  labs(fill = "HOLC Score")+
  theme(
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 5)
  )

bm_city_highlight <- get_stamenmap(bbox = c(left = -84.7440, top = 43.4778, 
                                  right = -82.1896, bottom = 42.000), zoom = 10, 
                         maptype = "terrain")



det_centroid <- st_centroid(detr_city) %>% 
  mutate(X = unlist(map(.$geometry,1)),
         Y = unlist(map(.$geometry,2)))

det_msa_centroid <- st_centroid(detr_msa) %>% 
  mutate(X = unlist(map(.$geometry,1)),
         Y = unlist(map(.$geometry,2)))

png('output/map_city_msa.png', width = 6.5, height = 3.5, units = 'in', res = 300)
ggplot() +
  geom_sf(data = mi_counties %>% 
            filter(NAME %in% c(county_match)),
          col = "White") +
  geom_sf(data = detr_msa, mapping = aes(),
          fill = NA, col = "Black", size = 1) + 
  geom_sf(data = detr_city, mapping = aes(),
          fill = NA, col = "#1696d2", size =1) +
  geom_text_repel(data = det_msa_centroid, aes(X, Y, label = NAMELSAD), 
                  fontface = "bold", 
                  size = 3,
                  nudge_x = c(10), 
                  nudge_y = c(1.1))+ 
  geom_text_repel(data = det_centroid, aes(X, Y, label = label), 
                  fontface = "bold", 
                  size = 3,
                  nudge_x = c(-10), 
                  nudge_y = c(-0.40))

dev.off()


ggmap(bm_city_highlight) + 
  geom_sf(data = detr_msa, mapping = aes(),
          fill = NA, col = "Black", size = 0.5,
          inherit.aes = FALSE) + 
  geom_sf(data = detr_city, mapping = aes(),
          fill = NA, col = "#1696d2", size =0.5,
          inherit.aes = FALSE) 



# Census Maps -------------------------------------------------------------


acs_09 <- read_csv("data/2005-09 MI TN OH.csv") %>% 
  mutate(acs_year = "2009")
acs_14 <- read_csv("data/2010-14 MI TN OH.csv") %>% 
  mutate(acs_year = "2014")
acs_19 <- read_csv("data/2015-19 MI TN OH.csv") %>% 
  mutate(acs_year = "2019")

acs_race <- bind_rows(acs_09, acs_14, acs_19)

detroit_msa_map <- get_map(location = c(left = -83.428096, bottom = 42.041991, right = -82.767724, top = 42.536375),
                           source = "stamen", maptype = "terrain", zoom = 13)

race_share_geo <- function(race_var, year, race) {
  
wayne_df <- acs_race %>% 
  filter(Geo_COUNTY == 163, Geo_STATE == 26, acs_year == year) %>% 
  group_by(Geo_TRACT) %>% 
  mutate(perc_black = SE_A04001_004 / SE_A04001_001,
         perc_white = SE_A04001_003 / SE_A04001_001,
         perc_asian = SE_A04001_006 / SE_A04001_001,
         perc_hipi = (SE_A04001_007 + SE_A04001_005) / SE_A04001_001,
         perc_hisp = SE_A04001_010 / SE_A04001_001) %>% 
  ungroup() %>% 
  select(Geo_FIPS, perc_black:perc_hisp) 

wayne_geo <- wayne_tracts %>% 
  select(GEOID) %>% 
  mutate(GEOID = as.numeric(GEOID)) %>% 
  left_join(wayne_df, by = c("GEOID" = "Geo_FIPS"))

ggmap(bm_race)+
  geom_sf(data = wayne_geo %>% 
            filter(!is.na({{race_var}})),
          mapping = aes(fill = {{race_var}}),
          col = NA, inherit.aes = FALSE, alpha = 0.7) +
  scale_fill_continuous(labels = scales::percent_format(accuracy = 1.0),
                        breaks = c(0.0, 0.25, 0.50, 0.75, 1.0),
                        limits = c(0, 1)) +
  ggtitle(year) +
  labs(fill = paste("Percntage", race)) +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(
    legend.key.size = unit(0.35, 'cm'),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
  )


}

asian_09 <- race_share_geo(year = "2009", race = "Asian", race_var = perc_asian)
asian_14 <- race_share_geo(year = "2014", race = "Asian", race_var = perc_asian)
asian_19 <- race_share_geo(year = "2019", race = "Asian", race_var = perc_asian)

black_09 <- race_share_geo(year = "2009", race = "Black", race_var = perc_black)
black_14 <- race_share_geo(year = "2014", race = "Black", race_var = perc_black)
black_19 <- race_share_geo(year = "2019", race = "Black", race_var = perc_black)

hisp_09 <- race_share_geo(year = "2009", race = "Hisp/Latinx", race_var = perc_hisp)
hisp_14 <- race_share_geo(year = "2014", race = "Hisp/Latinx", race_var = perc_hisp)
hisp_19 <- race_share_geo(year = "2019", race = "Hisp/Latinx", race_var = perc_hisp)

white_09 <- race_share_geo(year = "2009", race = "White", race_var = perc_white)
white_14 <- race_share_geo(year = "2014", race = "White", race_var = perc_white)
white_19 <- race_share_geo(year = "2019", race = "White", race_var = perc_white)


# Segregation Score Data --------------------------------------------------
#Note for Peter: I failed to replicate the segregation score maps in R
#This code just organizes the data to be visualized in excel (I know)

set_urbn_defaults(style = "print")

city <- readxl::read_xls("data/cityalld.xls") 
msa <- readxl::read_xls("data/msaalld.xls")

city_clean <- city %>% 
  filter(CITYNAME == "Detroit city") %>% 
  select(CITYNAME, starts_with("D")) %>% 
  pivot_longer(!CITYNAME, 
               names_to = "race_group",
               values_to = "dis_index") %>% 
  separate(col = race_group, into = c("race_comparison", "pop", "year"), sep = "_") %>% 
  mutate(comparison_group = case_when(race_comparison == "DWB" ~ "White-Black/Black-White",
                                      race_comparison == "DWH" ~ "White-Hispanic/Hispanic-White",
                                      race_comparison == "DWA" ~ "White-Asian/Asian-White",
                                      race_comparison == "DBH" ~ "Black-Hispanic/Hispanic-Black",
                                      race_comparison == "DBA" ~ "Black-Asian/Asian-Black",
                                      race_comparison == "DHA" ~ "Hispanic-Asian/Asian-Hispanic"),
         year = case_when(year == "80" ~ "1980",
                          year == "90" ~ "1990",
                          year == "00" ~ "2000",
                          year == "09" ~ "2009", 
                          year == "10" ~ "2010")) %>% 
  filter(!is.na(comparison_group)) %>% 
  select(year, comparison_group, dis_index) %>% 
  pivot_wider(id_cols = year, names_from = comparison_group, values_from = dis_index) %>% 
  write_csv("dis_index_city.csv")

msa_clean <- msa %>% 
  filter(metroname == "Detroit-Warren-Livonia, MI Metropolitan Statistical Area") %>% 
  select(metroname, starts_with("m_d")) %>% 
  pivot_longer(!metroname, 
               names_to = "race_group",
               values_to = "dis_index") %>% 
  separate(col = race_group, into = c("metro", "race_comparison", "pop", "year"), 
           sep = "_") %>% 
  mutate(comparison_group = case_when(race_comparison == "dwb" ~ "White-Black/Black-White",
                                      race_comparison == "dwh" ~ "White-Hispanic/Hispanic-White",
                                      race_comparison == "dwa" ~ "White-Asian/Asian-White",
                                      race_comparison == "dbh" ~ "Black-Hispanic/Hispanic-Black",
                                      race_comparison == "dba" ~ "Black-Asian/Asian-Black",
                                      race_comparison == "dha" ~ "Hispanic-Asian/Asian-Hispanic"),
         year = case_when(year == "80" ~ "1980",
                          year == "90" ~ "1990",
                          year == "00" ~ "2000",
                          year == "09" ~ "2009", 
                          year == "10" ~ "2010")) %>% 
  filter(!is.na(comparison_group)) %>% 
  select(year, comparison_group, dis_index) %>% 
  pivot_wider(id_cols = year, names_from = comparison_group, values_from = dis_index) %>% 
  write_csv("dis_index_msa.csv")


md_clean <- msa %>% 
  filter(metroname == "Detroit-Livonia-Dearborn, MI Metropolitan Division") %>% 
  select(metroname, starts_with("m_d")) %>% 
  pivot_longer(!metroname, 
               names_to = "race_group",
               values_to = "dis_index") %>% 
  separate(col = race_group, into = c("metro", "race_comparison", "pop", "year"), 
           sep = "_") %>% 
  mutate(comparison_group = case_when(race_comparison == "dwb" ~ "White-Black/Black-White",
                                      race_comparison == "dwh" ~ "White-Hispanic/Hispanic-White",
                                      race_comparison == "dwa" ~ "White-Asian/Asian-White",
                                      race_comparison == "dbh" ~ "Black-Hispanic/Hispanic-Black",
                                      race_comparison == "dba" ~ "Black-Asian/Asian-Black",
                                      race_comparison == "dha" ~ "Hispanic-Asian/Asian-Hispanic"),
         year = case_when(year == "80" ~ "1980",
                          year == "90" ~ "1990",
                          year == "00" ~ "2000",
                          year == "09" ~ "2009", 
                          year == "10" ~ "2010")) %>% 
  filter(!is.na(comparison_group)) %>% 
  select(year, comparison_group, dis_index) %>% 
  pivot_wider(id_cols = year, names_from = comparison_group, values_from = dis_index) %>% 
  write_csv("dis_index_md.csv")



# HPI Data ----------------------------------------------------------------


city_hpi <- read_csv("data/detroit_city_hpi.csv") %>% 
  mutate(city = str_sub(geo, 1, -4L),
         city = str_to_title(city),
         state = str_sub(geo, -2L),
         geo = paste(city, state))%>% 
  select(-city, -state) %>% 
  filter(date > "2003-01-01")

city_tiers <- read_csv("data/detroit_city_tiers.csv") %>% 
  mutate(city = str_sub(geo, 1, -4L),
         city = str_to_title(city),
         state = str_sub(geo, -2L),
         geo = paste(city, state),
         price_tier = str_sub(name, 1, -16L)) %>% 
  filter(date > "2003-01-01")


msa_hpi <- read_csv("data/detroit_msa_hpi.csv") %>% 
  mutate(city = str_sub(geo, 1, -4L),
         city = str_to_title(city),
         state = str_sub(geo, -2L),
         geo = paste(city, state)) %>% 
  select(-city, -state) %>% 
  filter(date > "2003-01-01")


national <- readxl::read_xlsx("data/cct_data.xlsx", sheet = "prices_local") %>% 
  filter(geo == "United States")

med_price <- read_csv("data/med_price_det.csv")

med_price_clean <- med_price %>% 
  filter(AggValueName != "United States") %>% 
  mutate(city = str_sub(AggValueName, 1, -4L),
         city = str_to_title(city),
         state = str_sub(AggValueName, -2L),
         geo = paste(city, state)) %>% 
  select(-city, -state, -AggValueName) %>% 
  bind_rows(med_price %>% 
              filter(AggValueName == "United States") %>% 
              mutate(geo = AggValueName) %>% 
              select(-AggValueName)) %>% 
  mutate(date = as.character(yyyymm),
         date = paste0(date, "01"),
         date = ymd(date))


city_comp <- city_hpi %>% 
  bind_rows(msa_hpi) %>% 
  bind_rows(national)

png('output/city_comp_hpi.png', width = 6.5, height = 3.5, units = 'in', res = 300)
city_comp %>%
  ggplot(aes(date, `BlackKnight HPI year-over-year`, color = geo)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Date",
       y = "Year-over-year home price change")

dev.off()

png('output/city_tiers.png', width = 6.5, height = 3.5, units = 'in', res = 300)
city_tiers %>%
  ggplot(aes(date, value, color = price_tier)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Date",
       y = "Year-over-year home price change")

dev.off()


png('output/city_price.png', width = 6.5, height = 3.5, units = 'in', res = 300)
med_price_clean %>%
  ggplot(aes(date, hpi, color = geo)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Date",
       y = "Home Price Estimate")

dev.off()



# Map section - try 2 -----------------------------------------------------

wayne_df <- acs_race %>% 
  filter(Geo_STATE == 26, acs_year == "2019") %>% 
  group_by(Geo_TRACT) %>% 
  mutate(perc_black = SE_A04001_004 / SE_A04001_001,
         perc_white = SE_A04001_003 / SE_A04001_001,
         perc_asian = SE_A04001_006 / SE_A04001_001,
         perc_hipi = (SE_A04001_007 + SE_A04001_005) / SE_A04001_001,
         perc_hisp = SE_A04001_010 / SE_A04001_001) %>% 
  ungroup() %>% 
  select(Geo_FIPS, perc_black:perc_hisp) 

det_centroid <- st_centroid(detr_city) %>% 
  mutate(X = unlist(map(.$geometry,1)),
         Y = unlist(map(.$geometry,2)))

msa_geo <- msa_tracts %>% 
  select(GEOID.x) %>% 
  mutate(GEOID = as.numeric(GEOID.x)) %>% 
  left_join(wayne_df, by = c("GEOID" = "Geo_FIPS"))

ggmap(detroit_msa_map) +
  geom_sf(data = msa_geo %>% 
            filter(!is.na(perc_white)),
          mapping = aes(fill = perc_white),
          col = NA, inherit.aes = FALSE, alpha = 0.7) +
  scale_fill_continuous(labels = scales::percent_format(accuracy = 1.0),
                        breaks = c(0.0, 0.25, 0.50, 0.75, 1.0),
                        limits = c(0, 1)) +
  geom_sf(data = detr_city, mapping = aes(), 
          col = "Black", fill = NA, inherit.aes = FALSE) +
  labs(fill = paste("Percntage", "white")) +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(
    legend.key.size = unit(0.35, 'cm'),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
  ) + 
  geom_text_repel(data = det_centroid, aes(X, Y, label = label), 
                  fontface = "bold", 
                  size = 3,
                  nudge_x = c(-10), 
                  nudge_y = c(-0.30))


  
