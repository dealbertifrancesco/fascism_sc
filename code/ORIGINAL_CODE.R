rm(list = ls())

library(readxl)
library(dplyr)
library(tidyverse)
library(readr)
library(data.table)
library(foreign)
library(stringr)
library(terra)
library(sf)
library(haven)
library(modelsummary)
library(tidyverse)
library(lfe)
library(stargazer)
library(fixest)
library(here)

#set your own working directory
setwd("C:/Users/dealb/Documents/Fascism")
fascism_db <- read_dta("fascism_db.dta") %>%
  st_as_sf(coords = c("longitude", "latitude"))  %>%  # set coordinates
  st_set_crs("EPSG:4326") #try also 7030
# stlaws <- read_xlsx("st_laws_matched.xlsx")
# stlaws$cod_istat <- as.numeric(stlaws$cod_istat)
# stlaws <- stlaws %>%
#   filter(!is.na(cod_istat))
# stlaws <- stlaws %>%
#   mutate(stat_ass = ifelse(!is.na(fr_corporazioni),1,0))
statuti <- read.csv("Statuti/statuti_clean.csv")
statuti$PRO_COM <- as.numeric(statuti$PRO_COM) 
statuti <- statuti %>%
  filter(!is.na(PRO_COM))
st_ass <- read.csv("Other data/st_laws_matched.csv") %>%
  filter(!is.na(cod_istat)) %>%
  mutate(stat_ass = 1) %>%
  select(cod_istat, stat_ass)
colnames(st_ass) <- c('PRO_COM', 'stat_ass')

#### mappe ----------------------------
library(sf) 
library(spData)
library(tmap)
library(RColorBrewer)

circondari <- st_read("Circondari_1921/Circondari_1921.shp") %>% st_make_valid  %>% st_transform(crs = st_crs("EPSG:4326"))
polygons_2011 <- polygons_2011 %>% rename(COMUNE=NOME) %>% select(-c(18:20))

df <- fascism_db %>% st_join(circondari)

#using ISTAT 2011 shapefile
polygons <- st_read("Com2011/Com2011_WGS84.shp") %>% st_make_valid  %>% st_transform(crs = st_crs("EPSG:4326"))
df <- polygons %>% st_join(fascism_db)

#Association density
pdf("mappe/assoc_memb.pdf")
tm_shape(df) + tm_fill("ass_memb1900s_pop", style = "jenks", palette = "-viridis", na.color = "black", 
                       title = "# ass. per 1000 people") + 
  tm_layout (main.title = "Association membership in 1900") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

pdf("mappe/assoc_d.pdf")
tm_shape(df)  + tm_fill("ass1900s_d", style = "cat", palette = "YlOrRd", na.color = "black", 
                        title = "Ass. present") + 
  tm_layout (main.title = "Association membership in 1900") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

#Fascist activity and support 
pdf("mappe/violenza_fascista.pdf")
tm_shape(df) + tm_fill("fascist_violence", style = "jenks", palette = "-viridis", na.color = "black", 
                       title = "# events per 1000 people") + 
  tm_layout (main.title = "Fascist violence, 1920-22") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

pdf("mappe/fascist1919_vv.pdf")
tm_shape(df) + tm_fill("fascist1919_vv", style = "jenks", palette = "-viridis", na.color = "black") + 
  tm_layout (main.title = "Fascist vote share 1919") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

pdf("mappe/fascist1921_vv.pdf")
tm_shape(df) + tm_fill("fascist1921_vv", style = "quantile", palette = "-viridis", na.color = "black") + 
  tm_layout (main.title = "Fascist vote share 1921") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

pdf("mappe/fascist1924_vv.pdf")
tm_shape(df) + tm_fill("fascist1924_vv", style = "quantile", palette = "-viridis", na.color = "black") + 
  tm_layout (main.title = "Fascist vote share 1924") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()


pdf("mappe/fascist_branch.pdf")
tm_shape(df) + tm_fill("fascist_branch", style = "cat", palette = "YlOrRd", na.color = "black", 
                       title = "Ass. present") + 
  tm_layout (main.title = "Fascist local branches, Autumn 1921") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

#density plot
ggplot(df, aes(x = ass_memb1900s_pop)) +
  geom_histogram(fill = "blue", color = "black") +  # Customize color and transparency
  labs(title = "Density Plot", x = "# ass. per 1000 people", y = "Density") + theme_minimal()


#bin scatterplot for bivariate relationships
ggplot(df, aes(ass_memb1900s_pop, fascist_violence)) + geom_point() + geom_smooth(method = "lm") + theme_minimal()
ggplot(df, aes(ass_memb1900s_pop, fascist1921_vv)) + geom_point() + geom_smooth(method = "lm") + theme_minimal()

# this will create 25 quantiles using y and assign the observations in each quantile to a separate bin
df_binscatter = fascism_db %>% mutate(bin = ntile(fascist1921_vv, n=30)) %>% 
  group_by(bin) %>% summarise(xmean = mean(ass_memb1900s_pop), ymean = mean(fascist1921_vv)) #find the x and y mean of each bin

pdf("mappe/binscatterplot.pdf")
ggplot(df_binscatter, aes(x=xmean, y=ymean)) + geom_point() + 
  labs(title = "Bin scatterplot", x = "Fascist vote share 1921", y = "# ass. per 1000 people") + theme_minimal()
dev.off()



------------------------------------------------------------------------------
  ### REGRESSION ANALYSIS  ####
## the variables that are going to be used are:
# ass1900s_d: Associations in 1900s (Binary)
# ass_memb1900s_pop: Assoc. Members in 1900s

df_reg = fascism_db %>% mutate(veterans = veterans74_95 + veterans96_00, 
                               province_fe = as.factor(provincia1921), 
                               fascist_spread_21 = (fascist1921_vv - fascist1919_vv),
                               fascist_spread_24 = (fascist1924_vv - fascist1921_vv)) 

# %>% filter(!is.na(fascist1919_vv) & !is.na(fascist1924_vv) | !is.na(fascist1921_vv))

## define controls ####
geo_ctrls = c("lpop1911", "larea", "centre_alt", "max_alt")
military_ctrls <- c("veterans")#, "veterans96_00", "ard_vol_pop6m", "cruent_d", "army_suppliers_d")
economic_ctrls <- c("ind_workers", "dlab", "bourgeoisie", "landlord_ass", "literacy") #"shcrop" "ind_firms", , "elites", )


df_desc_stats = df_reg %>% select(ass1900s_d, ass_memb1900s_pop, fascist_branch, 
                                  fascist_violence, fascist1921_vv, lpop1911, 
                                  larea, centre_alt, max_alt, veterans, ind_workers,
                                  dlab, bourgeoisie, landlord_ass, literacy, psu1919_vv, 
                                  deportations_d, depo_pop_cap1, foots_pop6m, casualties_pop6m)

datasummary_skim(df_desc_stats)

datasummary(All(df_desc_stats) ~ N + Mean + SD + Min + Median + Max, 
            data = df_desc_stats, output = "latex_tabular")


## Fascist branches ####
branch0a = feols(fascist_branch ~  ass1900s_d, vcov = ~ province_fe, data = df_reg)
branch1a = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
branch2a = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls]| province_fe, data = df_reg)
branch3a = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
branch4a = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
branch5a = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(branch0a, branch1a, branch2a, branch3a, branch4a, branch5a, tex = TRUE)


branch0b = feols(fascist_branch ~  ass_memb1900s_pop, vcov = ~ province_fe, data = df_reg)
branch1b = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
branch2b = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls]| province_fe, data = df_reg)
branch3b = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
branch4b = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
branch5b = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)


etable(branch0b, branch1b, branch2b, branch3b, branch4b, branch5b, tex = TRUE)


## Fascist violence ####
violence0a = feols(fascist_violence ~  ass1900s_d, vcov = ~ province_fe, data = df_reg)
violence1a = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
violence2a = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls]| province_fe, data = df_reg)
violence3a = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
violence4a = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
violence5a = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(violence0a, violence1a, violence2a, violence3a, violence4a, violence5a, tex = TRUE)


violence0b = feols(fascist_violence ~  ass_memb1900s_pop, vcov = ~ province_fe, data = df_reg)
violence1b = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
violence2b = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls]| province_fe, data = df_reg)
violence3b = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
violence4b = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
violence5b = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)


etable(violence0b, violence1b, violence2b, violence3b, violence4b, violence5b, tex = TRUE)


## Fascist support ####
support0a = feols(fascist1921_vv ~  ass1900s_d, vcov = ~ province_fe, data = df_reg)
support1a = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
support2a = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls]| province_fe, data = df_reg)
support3a = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
support4a = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
support5a = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(support0a, support1a, support2a, support3a, support4a, support5a, tex = TRUE)

support0b = feols(fascist1921_vv ~  ass_memb1900s_pop, vcov = ~ province_fe, data = df_reg)
support1b = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
support2b = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls]| province_fe, data = df_reg)
support3b = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
support4b = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
support5b = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)


etable(support0b, support1b, support2b, support3b, support4b, support5b, tex = TRUE)

## Arditi casualties ####
arditi0a = feols(ard_vol_pop6m ~  ass1900s_d, vcov = ~ province_fe, data = df_reg)
arditi1a = feols(ard_vol_pop6m ~  ass1900s_d + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
arditi2a = feols(ard_vol_pop6m ~  ass1900s_d + .[geo_ctrls]| province_fe, data = df_reg)
arditi3a = feols(ard_vol_pop6m ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
arditi4a = feols(ard_vol_pop6m ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
arditi5a = feols(ard_vol_pop6m ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(arditi0a, arditi1a, arditi2a, arditi3a, arditi4a, arditi5a, tex = TRUE)


arditi0b = feols(ard_vol_pop6m ~  ass_memb1900s_pop, vcov = ~ province_fe, data = df_reg)
arditi1b = feols(ard_vol_pop6m ~  ass_memb1900s_pop + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
arditi2b = feols(ard_vol_pop6m ~  ass_memb1900s_pop + .[geo_ctrls]| province_fe, data = df_reg)
arditi3b = feols(ard_vol_pop6m ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
arditi4b = feols(ard_vol_pop6m ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
arditi5b = feols(ard_vol_pop6m ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)


etable(arditi0b, arditi1b, arditi2b, arditi3b, arditi4b, arditi5b, tex = TRUE)

## Heterogeneity: South ####
df_reg_nosouth <- df_reg %>% filter(regione1921 != "abruzzi" & 
                                      regione1921 != "basilicata" & 
                                      regione1921 != "calabrie" & 
                                      regione1921 != "campania" &
                                      regione1921 != "puglie" &
                                      regione1921 != "sicilia")

nosouth0 = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_nosouth)
nosouth1 = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_nosouth)
nosouth2 = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_nosouth)
nosouth3 = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_nosouth)
nosouth4 = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_nosouth)
nosouth5 = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_nosouth)

etable(nosouth0, nosouth1, nosouth2, nosouth3, nosouth4, nosouth5, keep = c("ass1900s_d", "ass_memb1900s_pop"), tex = TRUE)


## Heterogeneity: landlord associations ####
landlord1 = feols(fascist_branch ~  ass1900s_d + ass1900s_d*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
landlord2 = feols(fascist_branch ~  ass_memb1900s_pop + ass_memb1900s_pop*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
landlord3 = feols(fascist_violence ~  ass1900s_d + ass1900s_d*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
landlord4 = feols(fascist_violence ~  ass_memb1900s_pop + ass_memb1900s_pop*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
landlord5 = feols(fascist1921_vv ~  ass1900s_d + ass1900s_d*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
landlord6 = feols(fascist1921_vv ~  ass_memb1900s_pop + ass_memb1900s_pop*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(landlord1, landlord2, landlord3, landlord4, landlord5, landlord6, keep = c("ass1900s_d", "ass_memb1900s_pop", "landlord_ass", "ass1900s_d*landlord_ass", "ass_memb1900s_pop*landlord_ass"), tex = TRUE)

## Heterogeneity: elites ####
#c("elites", "bourgeoisie", "large_donors")
elites0 = feols(fascist_branch ~  ass1900s_d + elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites1 = feols(fascist_branch ~  ass1900s_d + elites +ass1900s_d*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites2 = feols(fascist_branch ~  ass_memb1900s_pop + elites + ass_memb1900s_pop*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites3 = feols(fascist_violence ~  ass1900s_d + elites + ass1900s_d*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites4 = feols(fascist_violence ~  ass_memb1900s_pop + elites + ass_memb1900s_pop*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites5 = feols(fascist1921_vv ~  ass1900s_d + elites + ass1900s_d*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites6 = feols(fascist1921_vv ~  ass_memb1900s_pop + elites + ass_memb1900s_pop*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(elites0, elites1, elites2, elites3, elites4, elites5, elites6, keep = c("ass1900s_d", "ass_memb1900s_pop", "elites", "ass1900s_d*elites", "ass_memb1900s_pop*elites"), tex = TRUE)

large_donors0 = feols(fascist_branch ~  ass1900s_d + large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors1 = feols(fascist_branch ~  ass1900s_d + large_donors +ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors2 = feols(fascist_branch ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors3 = feols(fascist_violence ~  ass1900s_d + large_donors + ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors4 = feols(fascist_violence ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors5 = feols(fascist1921_vv ~  ass1900s_d + large_donors + ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors6 = feols(fascist1921_vv ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(large_donors1, large_donors2, large_donors3, large_donors4, large_donors5, large_donors6, keep = c("ass1900s_d", "ass_memb1900s_pop", "large_donors", "ass1900s_d*large_donors", "ass_memb1900s_pop*large_donors"), tex = TRUE)


## Heterogeneity: gvt stability ####
#socialist mayor? agrarian strikes?

large_donors0 = feols(fascist_branch ~  ass1900s_d + large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors1 = feols(fascist_branch ~  ass1900s_d + large_donors +ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors2 = feols(fascist_branch ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors3 = feols(fascist_violence ~  ass1900s_d + large_donors + ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors4 = feols(fascist_violence ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors5 = feols(fascist1921_vv ~  ass1900s_d + large_donors + ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors6 = feols(fascist1921_vv ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(large_donors0, large_donors1, large_donors2, large_donors3, large_donors4, large_donors5, large_donors6, keep = c("ass1900s_d", "ass_memb1900s_pop", "large_donors", "ass1900s_d*large_donors", "ass_memb1900s_pop*large_donors"), tex = TRUE)


## Placebos: selection into war (unplausible, see Arditi results), other political parties (Cattolici)
c("ppi1919_vv", "fascist1919_vv", "nationalist1919_vv", "traditional1919_vv")


## Sensibility to outliers ####
df_outlier = df_reg %>% filter(ass_memb1900s_pop <= 0.3)

outlier0 = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_outlier)
outlier1 = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_outlier)
outlier2 = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_outlier)
outlier3 = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_outlier)
outlier4 = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_outlier)
outlier5 = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_outlier)

etable(outlier0, outlier1, outlier2, outlier3, outlier4, outlier5, keep = c("ass1900s_d", "ass_memb1900s_pop"), tex = TRUE)

## controlling for early fascist support ##
violence1 = feols(fascist_violence ~  ass1900s_d +  fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
violence2 = feols(fascist_violence ~  ass_memb1900s_pop + fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
depo1 = feols(deportations_d ~  ass1900s_d + fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
depo2 = feols(deportations_d ~  ass_memb1900s_pop + fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
depo3 = feols(depo_pop_cap1 ~  ass1900s_d + fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
depo4 = feols(depo_pop_cap1 ~  ass_memb1900s_pop + fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(violence1, violence2, depo1, depo2, depo3, depo4, keep = c("ass1900s_d", "ass_memb1900s_pop", "fascist1919_vv"), tex = TRUE)


## control for casualties ##
foot_casualties0 <- feols(fascist_branch ~  ass1900s_d + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
foot_casualties1 <- feols(fascist_branch ~  ass_memb1900s_pop + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
foot_casualties2 <- feols(fascist_violence ~  ass1900s_d + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
foot_casualties3 <- feols(fascist_violence ~  ass_memb1900s_pop + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
foot_casualties4 <- feols(fascist1921_vv ~  ass1900s_d + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
foot_casualties5 <- feols(fascist1921_vv ~  ass_memb1900s_pop + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(foot_casualties0, foot_casualties1, foot_casualties2, foot_casualties3, foot_casualties4, foot_casualties5, keep = c("ass1900s_d", "ass_memb1900s_pop", "foots_pop6m"), tex = TRUE)


casualties0 <- feols(fascist_branch ~  ass1900s_d + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
casualties1 <- feols(fascist_branch ~  ass_memb1900s_pop + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
casualties2 <- feols(fascist_violence ~  ass1900s_d + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
casualties3 <- feols(fascist_violence ~  ass_memb1900s_pop + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
casualties4 <- feols(fascist1921_vv ~  ass1900s_d + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
casualties5 <- feols(fascist1921_vv ~  ass_memb1900s_pop + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(casualties0, casualties1, casualties2, casualties3, casualties4, casualties5, keep = c("casualties_pop6m", "ass1900s_d", "ass_memb1900s_pop"), tex = TRUE)

## regress into fascist spread ##
spread0a = feols(fascist_spread ~  ass1900s_d, vcov = ~ province_fe, data = df_reg)
spread1a = feols(fascist_spread ~  ass1900s_d + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
spread2a = feols(fascist_spread ~  ass1900s_d + .[geo_ctrls]| province_fe, data = df_reg)
spread3a = feols(fascist_spread ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
spread4a = feols(fascist_spread ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
spread5a = feols(fascist_spread ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(spread0a, spread1a, spread2a, spread3a, spread4a, spread5a, tex = TRUE)

spread0b = feols(fascist_spread ~  ass_memb1900s_pop, vcov = ~ province_fe, data = df_reg)
spread1b = feols(fascist_spread ~  ass_memb1900s_pop + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
spread2b = feols(fascist_spread ~  ass_memb1900s_pop + .[geo_ctrls]| province_fe, data = df_reg)
spread3b = feols(fascist_spread ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
spread4b = feols(fascist_spread ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
spread5b = feols(fascist_spread ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(spread0b, spread1b, spread2b, spread3b, spread4b, spread5b, tex = TRUE)



spread21a = feols(fascist_spread_21 ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
spread21b = feols(fascist_spread_21 ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
spread24a = feols(fascist_spread_24 ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
spread24b = feols(fascist_spread_24 ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(spread21a, spread21b, spread24a, spread24b, keep = c("ass1900s_d", "ass_memb1900s_pop"), tex = TRUE)


## keep only the large ones, ie more than 3 associations per 1000 people 
df_reg_large = df_reg %>% filter(ass_memb1900s_pop > median(ass_memb1900s_pop, na.rm = TRUE))
median(df_reg$ass_memb1900s_pop)

large = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_large)
large1 = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_large)
large2 = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_large)
large3 = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_large)
large4 = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_large)
large5 = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg_large)

etable(large0, large1, large2, large3, large4, large5, keep = c("ass1900s_d", "ass_memb1900s_pop"), tex = TRUE)




### Bowling for Fascism: Social Capital and the Rise of the Nazi Party, by Satyanath, Voigtlander, Voth

#interaction w/ gvt stability -> higher social capital and weaker gvts favored Nazi party even more: can do in our case by interacting w/ political violence

#"The historical record suggests that associations facilitated Nazi recruitment by helping to spread the party???s message and by increasing trust in its intentions and officials. Party membership, in turn, predicts electoral success."
#can do a similar thing by interacting w/ presence of landowners of association and local elites, already shown to be very important for the rise of fascism (Acemoglu et al. 2022, De Felice)

#Riley (2005, 2010) analyzes the role of civic associations and the rise of fascism in Italy and Spain. 
#On the basis of evidence from 20 Italian regions, he argues that associations fostered the rise of fascism. 
#In Spain, associational life was dominated by the Catholic Church and was
#largely compatible with a more traditionalist form of fascism. 
#Riley contends that in countries without strong hegemonic organizations - that is, well-established parties - social capital can undermine the development ofdemocracy.

### IV specifications ####

df_communes = read_dta("Replication/Data/Dataset_municipalities.dta") %>%
  st_as_sf(coords = c("loncentr", "latcentr"))  %>%  # set coordinates
  st_set_crs("EPSG:4326") #try also 7030

df_reg<-df_reg[-c(80:101,107:1334)]
df_reg_nosouth <- df_reg %>% filter(regione1921 != "abruzzi" & 
                                      regione1921 != "basilicata" & 
                                      regione1921 != "calabrie" & 
                                      regione1921 != "campania" &
                                      regione1921 != "puglie" &
                                      regione1921 != "sicilia")

df_iv<-st_join(df_reg_nosouth, df_communes, join = st_nearest_feature)
df_iv <- left_join(df_iv, statuti, by = "PRO_COM")
df_iv <- left_join(df_iv, st_ass, by = "PRO_COM")
df_iv <- df_iv %>%
  mutate(stat = ifelse(!is.na(stat),1,0),
         ln_assmemb = ifelse(ass_memb1900s_pop>0,log(ass_memb1900s_pop),NA),
         stat_p1000 = stat / exp(lpop1911),
         ln_stat_p1000 = ifelse(stat>0,log(stat_p1000),NA),
         stat_ass = ifelse(is.na(stat_ass), 0, stat_ass))

df_iv_ass <- df_iv %>%
  filter(ass1900s_d==1) %>%
  mutate(ass_memb_am = ifelse(ass_memb1900s_pop > quantile(ass_memb1900s_pop, 0.5), 1, 0))

#####CLEANING ST_ASS######
# com_codes <- read.csv("C:/Users/dealb/Documents/Fascism/Other data/Elenco-comuni-italiani.csv",
#                       header=F, sep=";", encoding = "ISO-8859-1")
# com_codes <- com_codes [-1,c(7,17)]
# colnames(com_codes) <- c("comune", "cod_istat")
# st_laws <- read.csv("C:/Users/dealb/Documents/Fascism/Other data/statuti_associazioni.csv")
# 
# has_broken_chars <- function(text) {
#   converted_text <- tryCatch({
#     iconv(text, from = "ISO-8859-1", to = "UTF-8")
#   }, warning = function(w) {
#     return(NA)
#   })
#   is.na(converted_text)
# }
# com_codes <- com_codes[!sapply(com_codes$comune, has_broken_chars), ]
# 
# clean_text <- function(text) {
#   text <- stri_trans_general(text, "Latin-ASCII")
#   text <- tolower(text)
#   text <- gsub("[^a-z]", "", text)
#   text <- gsub(" ", "", text)
#   return(text)
# }
# com_codes$comune_clean <- clean_text(com_codes$comune)
# st_laws$comune_clean <- clean_text(st_laws$comune)
# st_laws <- st_laws[-1]
# 
# st_laws <- left_join(st_laws, com_codes , by = "comune_clean")
# write.csv(st_laws, file = "C:/Users/dealb/Documents/Fascism/Other data/st_laws_matched.csv", row.names = FALSE)


  
 

#####IV######

df_iv_filtered<-df_iv%>%filter(stat>0)

## Test first stage from Table 5 in Historical Social Contracts (Prarolo et al. JOEG) ####
rep_experience2 = c("tcomrep", "tmarrep")
rep_experience4 = c("tcomrep", "tmarrep", "tcomrep_a", "tmarrep_s", "tmarrep_a")
allcontrols =  c("costal", "nearsea", "sea_distance100", "dist_river100", 
                      "alt_com100", "rug_med100", "CaloricSuitpre15nr100", 
                      "lnpop1000", "dist_romanrd100", "dist_bishop1000Km_GSZ100", 
                      "freecity", "comune_lega_lombarda_allnord")

#need to find way to correct for spatial autocorrelation
#province FE?
#replicate spatial weights in the original datasets?
firstage1 = feols(ass1900s_d ~ .[rep_experience2] + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_iv)
summary(firstage1)
firstage2 = feols(ass1900s_d ~ .[rep_experience4] + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_iv)
summary(firstage2)
firstage3 = feols(ass_memb1900s_pop ~ .[rep_experience2] + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_iv)
summary(firstage3)
firstage4 = feols(ass_memb1900s_pop ~ .[rep_experience4] + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_iv)
summary(firstage4)

## Branch
iv_1 = feols(fascist_branch ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
               ass1900s_d ~ stat, 
             data = df_iv,
             cluster = ~provincia1921)
summary(iv_1)
iv_2 = feols(fascist_branch ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
               ass_memb1900s_pop ~ stat_ass, 
                    data = df_iv_filtered)
summary(iv_2)

## Violence
iv_3 = feols(fascist_violence ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
               ass1900s_d ~ stat, 
             data = df_iv)
summary(iv_3)
iv_4 = feols(fascist_violence ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
               ass_memb1900s_pop ~ stat, 
             data = df_iv)
summary(iv_4)

## 1924 Votes
iv_5 = feols(fascist1924_vv ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
               ass1900s_d ~ stat, 
             data = df_iv)
summary(iv_5)
iv_6 = feols(fascist1921_vv ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
               ass_memb1900s_pop ~stat, 
             data = df_iv)
summary(iv_6)

## 1919 Votes
iv_7 = feols(fascist1919_vv ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
               ass1900s_d ~ stat, 
             data = df_iv)
summary(iv_7)
iv_8 = feols(fascist1919_vv ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
               ass_memb1900s_pop ~ stat, 
             data = df_iv)
summary(iv_8)

## 1921 Votes
iv_9 = feols(fascist1921_vv ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
               ass1900s_d ~ stat, 
             data = df_iv)
summary(iv_9)
iv_10 = feols(fascist1921_vv ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
                ass_memb1900s_pop ~ stat, 
             data = df_iv)
summary(iv_10)

## Deportations (dummy)
iv_11 = feols(deportations_d ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
               ass1900s_d ~ stat, 
             data = df_iv)
summary(iv_11)
iv_12 = feols(deportations_d ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
                ass_memb1900s_pop ~ stat, 
              data = df_iv)
summary(iv_12)

## Deportations (pc)
iv_13 = feols(depo_pop_cap1 ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
                ass1900s_d ~ stat, 
              data = df_iv)
summary(iv_13)
iv_14 = feols(depo_pop_cap1 ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | 
                ass_memb1900s_pop ~ stat, 
              data = df_iv)
summary(iv_14)

# Export grouped regressions
export_iv_table <- function(model1, model2, title, file_name) {
  etable(model1, model2, stage=1:2, fitstat=~n+ivf, coefstat="tstat",
         order = c("ass1900s_d", "ass_memb1900s_pop"),
         title = title, tex = TRUE, file = file_name)
}

export_iv_table(iv_1, iv_2, "Fascist Branch", "Fascist_Branch.tex")
export_iv_table(iv_3, iv_4, "Fascist Violence", "Fascist_Violence.tex")
export_iv_table(iv_5, iv_6, "Fascist 1921 Vote Share", "Fascist_1921_Vote_Share.tex")
export_iv_table(iv_7, iv_8, "Fascist 1919 Vote Share", "Fascist_1919_Vote_Share.tex")
export_iv_table(iv_15, iv_16, "Fascist Spread 1921", "Fascist_Spread_1921.tex")
export_iv_table(iv_17, iv_18, "Fascist Spread 1924", "Fascist_Spread_1924.tex")
export_iv_table(iv_11, iv_12, "Deportations", "Deportations.tex")


#Conley standard errors
# See: http://www.trfetzer.com/conley-spatial-hac-errors-with-fixed-effects/
firstage1 = feols(ass1900s_d ~ instability +  .[rep_experience4] + .[allcontrols] | province_fe, data = df_iv %>% filter(trepublics > 0), vcov = conley("70km") )
firstage2 = feols(ass1900s_d ~ instability + .[rep_experience2]  + .[allcontrols] | province_fe, data = df_iv %>% filter(trepublics > 0), vcov = conley("70km") )

#do the same for ass_memb1900s_pop




####PROPSCORE
ps_model <- glm(ass1900s_d ~ lpop1911 + larea + centre_alt + max_alt +
                  I(lpop1911^2) + I(larea^2) + I(centre_alt^2) + I(max_alt^2) +
                  veterans + ind_workers + dlab + bourgeoisie + landlord_ass + literacy +
                  I(veterans^2) + I(ind_workers^2) + I(dlab^2) + I(bourgeoisie^2) +
                  I(landlord_ass^2) + I(literacy^2), 
                data = df_iv, 
                family = binomial)

df_iv$pscore <- ps_model$fitted.values
p_treated <- mean(df_iv$ass1900s_d)
p_control <- 1 - p_treated

df_iv <- df_iv %>%
  mutate(weight = ifelse(ass1900s_d == 1, 
                         p_treated / pscore,
                         p_control / (1 - pscore)))

##DIAGNOSTICS
ggplot(df_iv, aes(x = pscore, fill = as.factor(ass1900s_d))) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(fill = "Treatment Group") +
  theme_minimal()

library(tableone)
library(survey)
survey_design <- svydesign(ids = ~1, data = df_iv, weights = ~weight)
table1 <- svyCreateTableOne(vars = c("lpop1911", "larea", "centre_alt", "max_alt", 
                                     "veterans", "ind_workers", "dlab", "bourgeoisie", 
                                     "landlord_ass", "literacy"), 
                            strata = "ass1900s_d", 
                            data = survey_design, 
                            test = T)
print(table1)

dfl_1 <- feols(fascist_branch ~ ass1900s_d, 
                   data = df_iv%>%filter(pscore<0.9), 
                   weights = ~weight)
summary(dfl_1)

dfl_2 <- feols(fascist_violence ~ ass1900s_d, 
                   data = df_iv, 
                   weights = ~weight)
summary(dfl_2)

dfl_2_1 <- feols(fascist_violence ~ ass1900s_d+fascist1921_vv, 
               data = df_iv, 
               weights = ~weight)
summary(dfl_2_1)

dfl_3 <- feols(fascist1921_vv ~ ass1900s_d, 
                   data = df_iv, 
                   weights = ~weight)
summary(dfl_3)

dfl_4 <- feols(fascist1919_vv ~ ass1900s_d, 
               data = df_iv, 
               weights = ~weight)
summary(dfl_4)

dfl_5 <- feols(fascist1924_vv ~ ass1900s_d, 
               data = df_iv, 
               weights = ~weight)
summary(dfl_5)

dfl_6 <- feols(fascist_spread_21 ~ ass1900s_d, 
               data = df_iv, 
               weights = ~weight)
summary(dfl_6)

dfl_7 <- feols(fascist_spread_24 ~ ass1900s_d, 
               data = df_iv, 
               weights = ~weight)
summary(dfl_7)
