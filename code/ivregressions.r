rm(list = ls())

### Packages
library(lfe)
library(fixest)

### Load data
setwd(here())
clean_data_dir <- here("data", "processed")
tables_dir <- here("output", "tables")

df_iv <- read.csv(file.path(clean_data_dir, "df_iv.csv"))

## Define controls ####
geo_ctrls = c("lpop1911", "larea", "centre_alt", "max_alt")
military_ctrls <- c("veterans")#, "veterans96_00", "ard_vol_pop6m", "cruent_d", "army_suppliers_d")
economic_ctrls <- c("ind_workers", "dlab", "bourgeoisie", "landlord_ass", "literacy") #"shcrop" "ind_firms", , "elites", )

rep_experience2 = c("tcomrep", "tmarrep")
rep_experience4 = c("tcomrep", "tmarrep", "tcomrep_a", "tmarrep_s", "tmarrep_a")
allcontrols =  c("costal", "nearsea", "sea_distance100", "dist_river100", 
                      "alt_com100", "rug_med100", "CaloricSuitpre15nr100", 
                      "lnpop1000", "dist_romanrd100", "dist_bishop1000Km_GSZ100", 
                      "freecity", "comune_lega_lombarda_allnord")

## Regressions
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

export_iv_table(iv_1, iv_2, "Fascist Branch", file.path(tables_dir, "Fascist_Branch.tex"))
export_iv_table(iv_3, iv_4, "Fascist Violence", file.path(tables_dir, "Fascist_Violence.tex"))
export_iv_table(iv_5, iv_6, "Fascist 1921 Vote Share", file.path(tables_dir, "Fascist_1921_Vote_Share.tex"))
export_iv_table(iv_7, iv_8, "Fascist 1919 Vote Share", file.path(tables_dir, "Fascist_1919_Vote_Share.tex"))
export_iv_table(iv_9, iv_10, "Fascist 1921 Vote Share", file.path(tables_dir, "Fascist_1921_Vote_Share.tex"))
export_iv_table(iv_11, iv_12, "Deportations (Dummmy)", file.path(tables_dir, "Deportations_d.tex"))
export_iv_table(iv_13, iv_14, "Deportations (PC)", file.path(tables_dir, "Deportations_pc.tex"))
