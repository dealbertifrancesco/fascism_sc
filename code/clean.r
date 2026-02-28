rm(list = ls())

### Packages
library(here)
library(dplyr)
library(foreign)
library(sf)
library(haven)
library(stringr)

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
  filter(!is.na(PRO_COM)) %>%
  distinct()

st_ass <- read.csv(file.path(raw_data_dir, "st_laws_matched.csv"), sep = ";") %>%
  filter(!is.na(cod_istat)) %>%
  mutate(stat_ass = 1) %>%
  select(cod_istat, stat_ass) %>%
  distinct()
colnames(st_ass) <- c('PRO_COM', 'stat_ass')

statuti_years <- read.csv(file.path(raw_data_dir, "statuti/statuti_clean_years.csv")) %>%
  select(PRO_COM_T, year_earliest_statuto) %>% rename(PRO_COM = PRO_COM_T) %>%
  filter(!is.na(PRO_COM)) %>% 
  mutate(PRO_COM = as.numeric(PRO_COM),
         exposure_stat = cut(
           1900 - year_earliest_statuto,
           breaks = seq(0, 500, by = 25), 
           right = FALSE),
         exposure_stat = as.integer(exposure_stat) - 1) %>%
  distinct()

dlf <- read.csv(file.path(raw_data_dir, "dlf_1926.csv"), sep = ";") %>%
  distinct()

surveillance_opponents <- read_dta(file.path(raw_data_dir, "cp_clean.dta")) %>% 
  mutate(municipality = 
  case_when(str_detect(res_mun, ",") ~ str_trim(str_extract(res_mun, "^[^,]+")),
    TRUE ~ res_mun
  ))   %>% filter(!is.na(res_mun)) %>%
  mutate(
    municipality_clean = municipality %>%
      # Extract only the first part before comma (Italian city)
      str_extract("^[^,]+") %>%
      # Remove all types of whitespace (spaces, tabs, newlines)
      str_trim() %>%
      str_squish() %>%
      # Remove parentheses and their contents
      str_remove_all("\\s*\\([^)]*\\)") %>%
      # Remove square brackets and their contents
      str_remove_all("\\s*\\[[^]]*\\]") %>%
      # Remove extra punctuation at the end
      str_remove_all("[.;:!?]+$") %>%
      # Remove leading/trailing dashes or hyphens
      str_remove_all("^[-–—]+|[-–—]+$") %>%
      # Convert to title case for consistency
      str_to_title() %>%
      # Final trim to catch any remaining spaces
      str_trim() %>%
      # Replace empty strings with NA
      na_if("") %>%
      # Convert to lowercase
      str_to_lower() %>%
      # Remove ALL spaces (internal and external)
      str_remove_all("\\s+")
  ) %>%
    group_by(municipality_clean) %>% 
  summarise(n_antifascists = n(),.groups = "drop") %>% distinct()

montidipieta = read_dta(file.path(raw_data_dir, "MontiPieta_Pascali.dta")) %>% 
                rename(PRO_COM = n_istat) 


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
df_iv <- st_drop_geometry(df_iv)
df_iv <- left_join(df_iv, statuti, by = "PRO_COM")
df_iv <- left_join(df_iv, st_ass, by = "PRO_COM")
df_iv <- left_join(df_iv, statuti_years, by = "PRO_COM")
df_iv <- left_join(df_iv, dlf, by = "PRO_COM")
df_iv <- left_join(df_iv, montidipieta, by = "PRO_COM")
df_iv <- df_iv %>%
  mutate(stat = ifelse(is.na(stat), 0, stat),
         ln_assmemb = ifelse(ass_memb1900s_pop>0,log(ass_memb1900s_pop),NA),
         stat_p1000 = stat / exp(lpop1911),
         ln_stat_p1000 = ifelse(stat>0,log(stat_p1000),NA),
         stat_ass = ifelse(is.na(stat_ass), 0, stat_ass),
         exposure_stat = ifelse(is.na(exposure_stat), 0, exposure_stat),
         dlf_1926 = ifelse(is.na(dlf_1926), 0, dlf_1926)) %>%
  distinct()
df_iv <- df_iv %>% 
  left_join(surveillance_opponents, by = c("comune1921" = "municipality_clean")) %>%
  mutate(n_antifascists = ifelse(is.na(n_antifascists), 0, n_antifascists),
         share_antifa_pop11 = n_antifascists / exp(lpop1911),
         antifa_d = ifelse(n_antifascists > 0, 1, 0))

### Export clean data
write.csv(df_reg, file.path(clean_data_dir, "df_reg.csv"), row.names = F)
write.csv(df_iv, file.path(clean_data_dir, "df_iv.csv"), row.names = F)