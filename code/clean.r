rm(list = ls())

### Packages
library(here)
library(dplyr)
library(foreign)
library(sf)

### Set working directory
setwd(here())
raw_data_dir <- here("data", "raw")
clean_data_dir <- here("data", "processed")

### Import Data
fascism_db <- read_dta(file.path(raw_data_dir, "fascism_db.dta")) %>%
  st_as_sf(coords = c("longitude", "latitude"))  %>%  # set coordinates
  st_set_crs("EPSG:4326")

statuti <- read.csv(file.path(raw_data_dir, "statuti_clean.csv")) 
statuti$PRO_COM <- as.numeric(statuti$PRO_COM) 
statuti <- statuti %>%
  filter(!is.na(PRO_COM)) %>% mutate(PRO_COM = as.numeric(PRO_COM))

statuti_years <- read.csv(file.path(raw_data_dir, "statuti/statuti_clean_years.csv")) %>%
  select(PRO_COM_T, year_earliest_statuto) %>% rename(PRO_COM = PRO_COM_T) %>%
  filter(!is.na(PRO_COM)) %>% mutate(PRO_COM = as.numeric(PRO_COM))

statuti <- left_join(statuti, statuti_years, by = "PRO_COM")

st_ass <- read_excel(file.path(raw_data_dir, "st_laws_matched.xlsx")) %>% 
  filter(!is.na(cod_istat)) %>%
  mutate(stat_ass = 1) %>%
  select(cod_istat, stat_ass) %>% mutate(cod_istat = as.numeric(cod_istat))
colnames(st_ass) <- c('PRO_COM', 'stat_ass')

df_reg = fascism_db %>% mutate(veterans = veterans74_95 + veterans96_00, 
                               province_fe = as.factor(provincia1921), 
                               fascist_spread_21 = (fascist1921_vv - fascist1919_vv),
                               fascist_spread_24 = (fascist1924_vv - fascist1921_vv)) 
df_reg<-df_reg[-c(80:101,107:1334)]
df_reg_nosouth <- df_reg %>% filter(regione1921 != "abruzzi" & 
                                      regione1921 != "basilicata" & 
                                      regione1921 != "calabrie" & 
                                      regione1921 != "campania" &
                                      regione1921 != "puglie" &
                                      regione1921 != "sicilia")

df_communes = read_dta(file.path(raw_data_dir, "Dataset_municipalities.dta")) %>%
  st_as_sf(coords = c("loncentr", "latcentr"))  %>%  # set coordinates
  st_set_crs("EPSG:4326")

df_iv <- st_join(df_reg_nosouth, df_communes, join = st_nearest_feature)
df_iv <- left_join(df_iv, statuti, by = "PRO_COM")
df_iv <- left_join(df_iv, st_ass, by = "PRO_COM")
df_iv <- df_iv %>%
  mutate(stat = ifelse(!is.na(stat),1,0),
         ln_assmemb = ifelse(ass_memb1900s_pop>0,log(ass_memb1900s_pop),NA),
         stat_p1000 = stat / exp(lpop1911),
         ln_stat_p1000 = ifelse(stat>0,log(stat_p1000),NA),
         stat_ass = ifelse(is.na(stat_ass), 0, stat_ass),
         exposure_stat = cut(
           1900 - year_earliest_statuto,
           breaks = seq(0, 500, by = 50), 
           right = FALSE),
         exposure_stat = as.integer(exposure_stat) - 1
          )

### Export clean data
write.csv(df_reg, file.path(clean_data_dir, "df_reg.csv"), row.names = FALSE)
write.csv(df_iv, file.path(clean_data_dir, "df_iv.csv"), row.names = FALSE)