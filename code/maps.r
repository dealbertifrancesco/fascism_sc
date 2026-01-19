rm(list = ls())

### Packages
library(sf) 
library(spData)
library(tmap)
library(RColorBrewer)

## Load Data
setwd(here())
raw_data_dir <- here("data", "raw")
clean_data_dir <- here("data", "processed")
figures_dir <- here("output", "figures")

# Cervellati et al. JOEG shp
circondari <- st_read(file.path(raw_data_dir, "Circondari_1921", "Circondari_1921.shp")) %>% st_make_valid  %>% st_transform(crs = st_crs("EPSG:4326"))
polygons_2011 <- polygons_2011 %>% rename(COMUNE=NOME) %>% select(-c(18:20))

fascism_db <- read.csv(file.path(clean_data_dir, "df_reg.csv")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("EPSG:4326"), remove = FALSE)
df <- fascism_db %>% st_join(circondari)

# ISTAT 2011 shp (DA AGGIUNGERE)
polygons <- st_read(file.path(raw_data_dir, "Com2011", "Com2011_WGS84.shp")) %>% st_make_valid  %>% st_transform(crs = st_crs("EPSG:4326"))
df <- polygons %>% st_join(fascism_db)

#### Maps
#Association density
pdf(file.path(figures_dir, "assoc_memb.pdf"))
tm_shape(df) + tm_fill("ass_memb1900s_pop", style = "jenks", palette = "-viridis", na.color = "black", 
                       title = "# ass. per 1000 people") + 
  tm_layout (main.title = "Association membership in 1900") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

pdf(file.path(figures_dir, "assoc_d.pdf"))
tm_shape(df)  + tm_fill("ass1900s_d", style = "cat", palette = "YlOrRd", na.color = "black", 
                        title = "Ass. present") + 
  tm_layout (main.title = "Association membership in 1900") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

#Fascist activity and support 
pdf(file.path(figures_dir, "violenza_fascista.pdf"))
tm_shape(df) + tm_fill("fascist_violence", style = "jenks", palette = "-viridis", na.color = "black", 
                       title = "# events per 1000 people") + 
  tm_layout (main.title = "Fascist violence, 1920-22") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

pdf(file.path(figures_dir, "fascist1919_vv.pdf"))
tm_shape(df) + tm_fill("fascist1919_vv", style = "jenks", palette = "-viridis", na.color = "black") + 
  tm_layout (main.title = "Fascist vote share 1919") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

pdf(file.path(figures_dir, "fascist1921_vv.pdf"))
tm_shape(df) + tm_fill("fascist1921_vv", style = "quantile", palette = "-viridis", na.color = "black") + 
  tm_layout (main.title = "Fascist vote share 1921") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

pdf(file.path(figures_dir, "fascist1924_vv.pdf"))
tm_shape(df) + tm_fill("fascist1924_vv", style = "quantile", palette = "-viridis", na.color = "black") + 
  tm_layout (main.title = "Fascist vote share 1924") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()


pdf(file.path(figures_dir, "fascist_branch.pdf"))
tm_shape(df) + tm_fill("fascist_branch", style = "cat", palette = "YlOrRd", na.color = "black", 
                       title = "Ass. present") + 
  tm_layout (main.title = "Fascist local branches, Autumn 1921") + 
  tm_layout(frame=FALSE)  +
  tm_compass(type = "8star", position = c("left", "bottom")) 
dev.off()

## Other graphs

# Density histogram
ggplot(df, aes(x = ass_memb1900s_pop)) +
  geom_histogram(fill = "blue", color = "black") +  
  labs(title = "Density Plot", x = "# ass. per 1000 people", y = "Density") + theme_minimal()

# Scatterplot for bivariate relationships
ggplot(df, aes(ass_memb1900s_pop, fascist_violence)) + geom_point() + geom_smooth(method = "lm") + theme_minimal()
ggplot(df, aes(ass_memb1900s_pop, fascist1921_vv)) + geom_point() + geom_smooth(method = "lm") + theme_minimal()

# Binned scatted
df_binscatter = fascism_db %>% mutate(bin = ntile(fascist1921_vv, n=30)) %>% 
  group_by(bin) %>% summarise(xmean = mean(ass_memb1900s_pop), ymean = mean(fascist1921_vv))

pdf(file.path(figures_dir, "binscatterplot.pdf"))
ggplot(df_binscatter, aes(x=xmean, y=ymean)) + geom_point() + 
  labs(title = "Bin scatterplot", x = "Fascist vote share 1921", y = "# ass. per 1000 people") + theme_minimal()
dev.off()