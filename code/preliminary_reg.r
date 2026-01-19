rm(list = ls())

## Packages
library(lfe)
library(fixest)

## Load
setwd(here())
clean_data_dir <- here("data", "processed")
tables_dir <- here("output", "tables")

df_reg <- read.csv(file.path(clean_data_dir, "df_reg.csv"))

## Define controls ####
geo_ctrls = c("lpop1911", "larea", "centre_alt", "max_alt")
military_ctrls <- c("veterans")#, "veterans96_00", "ard_vol_pop6m", "cruent_d", "army_suppliers_d")
economic_ctrls <- c("ind_workers", "dlab", "bourgeoisie", "landlord_ass", "literacy") #"shcrop" "ind_firms", , "elites", )


df_desc_stats = df_reg %>% select(ass1900s_d, ass_memb1900s_pop, fascist_branch, 
                                  fascist_violence, fascist1921_vv, lpop1911, 
                                  larea, centre_alt, max_alt, veterans, ind_workers,
                                  dlab, bourgeoisie, landlord_ass, literacy, psu1919_vv, 
                                  deportations_d, depo_pop_cap1, foots_pop6m, casualties_pop6m)

## Summary Stats 
##datasummary_skim(df_desc_stats)

##datasummary(All(df_desc_stats) ~ N + Mean + SD + Min + Median + Max, 
            ##data = df_desc_stats, output = "latex_tabular")


## Fascist branches ####
branch0a = feols(fascist_branch ~  ass1900s_d, vcov = ~ province_fe, data = df_reg)
branch1a = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
branch2a = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls]| province_fe, data = df_reg)
branch3a = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
branch4a = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
branch5a = feols(fascist_branch ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(branch0a, branch1a, branch2a, branch3a, branch4a, branch5a, tex = TRUE, file = file.path(tables_dir, "prel_branch_a.tex"))


branch0b = feols(fascist_branch ~  ass_memb1900s_pop, vcov = ~ province_fe, data = df_reg)
branch1b = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
branch2b = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls]| province_fe, data = df_reg)
branch3b = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
branch4b = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
branch5b = feols(fascist_branch ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)


etable(branch0b, branch1b, branch2b, branch3b, branch4b, branch5b, tex = TRUE, file = file.path(tables_dir, "prel_branch_b.tex"))


## Fascist violence ####
violence0a = feols(fascist_violence ~  ass1900s_d, vcov = ~ province_fe, data = df_reg)
violence1a = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
violence2a = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls]| province_fe, data = df_reg)
violence3a = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
violence4a = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
violence5a = feols(fascist_violence ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(violence0a, violence1a, violence2a, violence3a, violence4a, violence5a, tex = TRUE, file = file.path(tables_dir, "prel_violence_a.tex"))


violence0b = feols(fascist_violence ~  ass_memb1900s_pop, vcov = ~ province_fe, data = df_reg)
violence1b = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
violence2b = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls]| province_fe, data = df_reg)
violence3b = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
violence4b = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
violence5b = feols(fascist_violence ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)


etable(violence0b, violence1b, violence2b, violence3b, violence4b, violence5b, tex = TRUE, file = file.path(tables_dir, "prel_violence_b.tex"))


## Fascist support ####
support0a = feols(fascist1921_vv ~  ass1900s_d, vcov = ~ province_fe, data = df_reg)
support1a = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
support2a = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls]| province_fe, data = df_reg)
support3a = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
support4a = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
support5a = feols(fascist1921_vv ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(support0a, support1a, support2a, support3a, support4a, support5a, tex = TRUE, file = file.path(tables_dir, "prel_support_a.tex"))

support0b = feols(fascist1921_vv ~  ass_memb1900s_pop, vcov = ~ province_fe, data = df_reg)
support1b = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
support2b = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls]| province_fe, data = df_reg)
support3b = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
support4b = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
support5b = feols(fascist1921_vv ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)


etable(support0b, support1b, support2b, support3b, support4b, support5b, tex = TRUE, file = file.path(tables_dir, "prel_support_b.tex"))


etable(arditi0a, arditi1a, arditi2a, arditi3a, arditi4a, arditi5a, tex = TRUE, file = file.path(tables_dir, "prel_arditi_a.tex"))


arditi0b = feols(ard_vol_pop6m ~  ass_memb1900s_pop, vcov = ~ province_fe, data = df_reg)
arditi1b = feols(ard_vol_pop6m ~  ass_memb1900s_pop + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
arditi2b = feols(ard_vol_pop6m ~  ass_memb1900s_pop + .[geo_ctrls]| province_fe, data = df_reg)
arditi3b = feols(ard_vol_pop6m ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
arditi4b = feols(ard_vol_pop6m ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
arditi5b = feols(ard_vol_pop6m ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)


etable(arditi0b, arditi1b, arditi2b, arditi3b, arditi4b, arditi5b, tex = TRUE, file = file.path(tables_dir, "prel_arditi_b.tex"))

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

etable(nosouth0, nosouth1, nosouth2, nosouth3, nosouth4, nosouth5, keep = c("ass1900s_d", "ass_memb1900s_pop"), tex = TRUE,
file = file.path(tables_dir, "prel_nosouth.tex"))


## Heterogeneity: landlord associations ####
landlord1 = feols(fascist_branch ~  ass1900s_d + ass1900s_d*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
landlord2 = feols(fascist_branch ~  ass_memb1900s_pop + ass_memb1900s_pop*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
landlord3 = feols(fascist_violence ~  ass1900s_d + ass1900s_d*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
landlord4 = feols(fascist_violence ~  ass_memb1900s_pop + ass_memb1900s_pop*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
landlord5 = feols(fascist1921_vv ~  ass1900s_d + ass1900s_d*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
landlord6 = feols(fascist1921_vv ~  ass_memb1900s_pop + ass_memb1900s_pop*landlord_ass + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(landlord1, landlord2, landlord3, landlord4, landlord5, landlord6, keep = c("ass1900s_d", "ass_memb1900s_pop", "landlord_ass", "ass1900s_d*landlord_ass", "ass_memb1900s_pop*landlord_ass"), 
tex = TRUE, file = file.path(tables_dir, "prel_landlord.tex"))

## Heterogeneity: elites ####
#c("elites", "bourgeoisie", "large_donors")
elites0 = feols(fascist_branch ~  ass1900s_d + elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites1 = feols(fascist_branch ~  ass1900s_d + elites +ass1900s_d*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites2 = feols(fascist_branch ~  ass_memb1900s_pop + elites + ass_memb1900s_pop*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites3 = feols(fascist_violence ~  ass1900s_d + elites + ass1900s_d*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites4 = feols(fascist_violence ~  ass_memb1900s_pop + elites + ass_memb1900s_pop*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites5 = feols(fascist1921_vv ~  ass1900s_d + elites + ass1900s_d*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
elites6 = feols(fascist1921_vv ~  ass_memb1900s_pop + elites + ass_memb1900s_pop*elites + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(elites0, elites1, elites2, elites3, elites4, elites5, elites6, keep = c("ass1900s_d", "ass_memb1900s_pop", "elites", "ass1900s_d*elites", "ass_memb1900s_pop*elites"),
 tex = TRUE, file = file.path(tables_dir, "prel_elites.tex"))

large_donors0 = feols(fascist_branch ~  ass1900s_d + large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors1 = feols(fascist_branch ~  ass1900s_d + large_donors +ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors2 = feols(fascist_branch ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors3 = feols(fascist_violence ~  ass1900s_d + large_donors + ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors4 = feols(fascist_violence ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors5 = feols(fascist1921_vv ~  ass1900s_d + large_donors + ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors6 = feols(fascist1921_vv ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(large_donors1, large_donors2, large_donors3, large_donors4, large_donors5, large_donors6, keep = c("ass1900s_d", "ass_memb1900s_pop", "large_donors", "ass1900s_d*large_donors", "ass_memb1900s_pop*large_donors"),
 tex = TRUE, file = file.path(tables_dir, "prel_large_donors.tex"))


## Heterogeneity: gvt stability ####
#socialist mayor? agrarian strikes?

large_donors0 = feols(fascist_branch ~  ass1900s_d + large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors1 = feols(fascist_branch ~  ass1900s_d + large_donors +ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors2 = feols(fascist_branch ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors3 = feols(fascist_violence ~  ass1900s_d + large_donors + ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors4 = feols(fascist_violence ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors5 = feols(fascist1921_vv ~  ass1900s_d + large_donors + ass1900s_d*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
large_donors6 = feols(fascist1921_vv ~  ass_memb1900s_pop + large_donors + ass_memb1900s_pop*large_donors + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(large_donors0, large_donors1, large_donors2, large_donors3, large_donors4, large_donors5, large_donors6, keep = c("ass1900s_d", "ass_memb1900s_pop", "large_donors", "ass1900s_d*large_donors", "ass_memb1900s_pop*large_donors"),
 tex = TRUE, file = file.path(tables_dir, "prel_large_donors.tex"))


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

etable(outlier0, outlier1, outlier2, outlier3, outlier4, outlier5, keep = c("ass1900s_d", "ass_memb1900s_pop"), tex = TRUE, file = file.path(tables_dir, "prel_outliers.tex"))

## controlling for early fascist support ##
violence1 = feols(fascist_violence ~  ass1900s_d +  fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
violence2 = feols(fascist_violence ~  ass_memb1900s_pop + fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
depo1 = feols(deportations_d ~  ass1900s_d + fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
depo2 = feols(deportations_d ~  ass_memb1900s_pop + fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
depo3 = feols(depo_pop_cap1 ~  ass1900s_d + fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
depo4 = feols(depo_pop_cap1 ~  ass_memb1900s_pop + fascist1919_vv + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(violence1, violence2, depo1, depo2, depo3, depo4, keep = c("ass1900s_d", "ass_memb1900s_pop", "fascist1919_vv"), tex = TRUE, file = file.path(tables_dir, "prel_early_fascist_support.tex"))


## control for casualties ##
foot_casualties0 <- feols(fascist_branch ~  ass1900s_d + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
foot_casualties1 <- feols(fascist_branch ~  ass_memb1900s_pop + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
foot_casualties2 <- feols(fascist_violence ~  ass1900s_d + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
foot_casualties3 <- feols(fascist_violence ~  ass_memb1900s_pop + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
foot_casualties4 <- feols(fascist1921_vv ~  ass1900s_d + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
foot_casualties5 <- feols(fascist1921_vv ~  ass_memb1900s_pop + foots_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(foot_casualties0, foot_casualties1, foot_casualties2, foot_casualties3, foot_casualties4, foot_casualties5, keep = c("ass1900s_d", "ass_memb1900s_pop", "foots_pop6m"),
tex = TRUE, file = file.path(tables_dir, "prel_foot_casualties.tex"))


casualties0 <- feols(fascist_branch ~  ass1900s_d + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
casualties1 <- feols(fascist_branch ~  ass_memb1900s_pop + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
casualties2 <- feols(fascist_violence ~  ass1900s_d + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
casualties3 <- feols(fascist_violence ~  ass_memb1900s_pop + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
casualties4 <- feols(fascist1921_vv ~  ass1900s_d + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
casualties5 <- feols(fascist1921_vv ~  ass_memb1900s_pop + casualties_pop6m + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(casualties0, casualties1, casualties2, casualties3, casualties4, casualties5, keep = c("casualties_pop6m", "ass1900s_d", "ass_memb1900s_pop"),
tex = TRUE, file = file.path(tables_dir, "prel_casualties.tex"))

## regress into fascist spread ##
spread0a = feols(fascist_spread ~  ass1900s_d, vcov = ~ province_fe, data = df_reg)
spread1a = feols(fascist_spread ~  ass1900s_d + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
spread2a = feols(fascist_spread ~  ass1900s_d + .[geo_ctrls]| province_fe, data = df_reg)
spread3a = feols(fascist_spread ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
spread4a = feols(fascist_spread ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
spread5a = feols(fascist_spread ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(spread0a, spread1a, spread2a, spread3a, spread4a, spread5a, tex = TRUE, file = file.path(tables_dir, "prel_spread_a.tex"))

spread0b = feols(fascist_spread ~  ass_memb1900s_pop, vcov = ~ province_fe, data = df_reg)
spread1b = feols(fascist_spread ~  ass_memb1900s_pop + .[geo_ctrls], vcov = ~ province_fe, data = df_reg)
spread2b = feols(fascist_spread ~  ass_memb1900s_pop + .[geo_ctrls]| province_fe, data = df_reg)
spread3b = feols(fascist_spread ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls]| province_fe, data = df_reg)
spread4b = feols(fascist_spread ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls]| province_fe, data = df_reg)
spread5b = feols(fascist_spread ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(spread0b, spread1b, spread2b, spread3b, spread4b, spread5b, tex = TRUE, file = file.path(tables_dir, "prel_spread_b.tex"))


spread21a = feols(fascist_spread_21 ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
spread21b = feols(fascist_spread_21 ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
spread24a = feols(fascist_spread_24 ~  ass1900s_d + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)
spread24b = feols(fascist_spread_24 ~  ass_memb1900s_pop + .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv| province_fe, data = df_reg)

etable(spread21a, spread21b, spread24a, spread24b, keep = c("ass1900s_d", "ass_memb1900s_pop"), tex = TRUE, file = file.path(tables_dir, "prel_spread_21_24.tex"))