---
  title: "Prediction Ticks Illinois"
author: "Abrar"
date: "`r Sys.Date()`"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



## Load Required Libraries
```{r, message=FALSE, warning=FALSE} 

#install.packages(c("sf", "terra", "dplyr", "exactextractr", "geodata", "elevatr", "rgee"), dependencies = TRUE)

library(sf)
library(sp)
library(terra)
library(dplyr)
library(exactextractr)
library(geodata)
library(elevatr) 
library(rgee)
library(ggplot2)
library(patchwork)


```


## Loading tick data & Join with Shapefile
```{r, message=FALSE, warning=FALSE} 
#data

IL_tick <- read.csv("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Data/ticks_survillance_data.csv")

# In all columns if "0" make that "NA"
IL_tick[IL_tick == 0] <- NA

#View(IL_tick)


#shape file
IL_shape <- st_read("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Illinois Shape File/Shape file of interest/IL_BNDY_County_Py.shp")

#View(IL_shape)

# REMOVING CONFLICTING OR DUPLICATE COLUMN SHAPE FILE ALREADY HAVE
library(dplyr)

IL_shape <- IL_shape %>% 
  dplyr::select(COUNTY_NAM, geometry)

# Standardize names for merging
IL_tick$COUNTY_Survillance <- gsub("\\s+", "",tolower(trimws(IL_tick$COUNTY_Survillance)))
IL_shape$COUNTY_NAM <- gsub("\\s+", "",tolower(trimws(IL_shape$COUNTY_NAM)))

# Merge tick data with shapefile
il_tick<- left_join(IL_shape, IL_tick, by = c("COUNTY_NAM" = "COUNTY_Survillance"))

#View(il_tick)


##Looking at distribution

# Drop geometry for plotting
df <- il_tick %>% st_drop_geometry()

# Remove NAs before plotting
p1 <- ggplot(filter(df, !is.na(aa_count)), aes(x = COUNTY_NAM, y = is_count)) +
  geom_point(color = "darkblue", size = 2, alpha = 0.7) +
  coord_flip() +
  labs(title = "A. americanum", x = "County", y = "Count") +
  theme_minimal(base_size = 12)

p2 <- ggplot(filter(df, !is.na(dv_count)), aes(x = COUNTY_NAM, y = aa_count)) +
  geom_point(color = "darkorange", size = 2, alpha = 0.7) +
  coord_flip() +
  labs(title = "D. variabilis", x = "County", y = "Count") +
  theme_minimal(base_size = 12)

p3 <- ggplot(filter(df, !is.na(is_count)), aes(x = COUNTY_NAM, y = dv_count)) +
  geom_point(color = "firebrick", size = 2, alpha = 0.7) +
  coord_flip() +
  labs(title = "I. scapularis", x = "County", y = "Count") +
  theme_minimal(base_size = 12)

# Combine all three plots side-by-side
combined_plot <- (p1 | p2 | p3) 


# Display the panel
combined_plot

setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")

# Save to file
ggsave("tick_counts_panel.png", combined_plot, width = 12, height = 10, dpi = 300)

```

## transform count
```{r}

# drop geometry and keep counts
df <- as.data.frame(sf::st_drop_geometry(il_tick)) %>%
  dplyr::select(aa_count, dv_count, is_count)

# A. americanum
x <- na.omit(df$aa_count)
shapiro.test(x)
ggplot(data.frame(x), aes(x)) + geom_histogram(bins = 20, boundary = 0)
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line()
x_t <- log1p(x + 1)
ggplot(data.frame(x_t), aes(x_t)) + geom_histogram(bins = 20, boundary = 0)
ggplot(data.frame(x_t), aes(sample = x_t)) + stat_qq() + stat_qq_line()
shapiro.test(x_t)

# D. variabilis
x <- na.omit(df$dv_count)
shapiro.test(x)
ggplot(data.frame(x), aes(x)) + geom_histogram(bins = 20, boundary = 0)
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line()
x_t <- log1p(x + 1)
ggplot(data.frame(x_t), aes(x_t)) + geom_histogram(bins = 20, boundary = 0)
ggplot(data.frame(x_t), aes(sample = x_t)) + stat_qq() + stat_qq_line()
shapiro.test(x_t)

# I. scapularis
x <- na.omit(df$is_count)
shapiro.test(x)
ggplot(data.frame(x), aes(x)) + geom_histogram(bins = 20, boundary = 0)
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line()
x_t <- log1p(x + 1)
ggplot(data.frame(x_t), aes(x_t)) + geom_histogram(bins = 20, boundary = 0)
ggplot(data.frame(x_t), aes(sample = x_t)) + stat_qq() + stat_qq_line()
shapiro.test(x_t)

# write transformed values back to il_tick as aa_scaled dv_scaled is_scaled
il_tick <- il_tick %>%
  dplyr::mutate(
    aa_scaled = log1p(aa_count + 1),
    dv_scaled = log1p(dv_count + 1),
    is_scaled = log1p(is_count + 1)
  )

# build combined before and after panels for all species
raw_long <- as.data.frame(sf::st_drop_geometry(il_tick)) %>%
  dplyr::select(aa_count, dv_count, is_count) %>%
  tidyr::pivot_longer(everything(), names_to = "species", values_to = "value") %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(
    species = factor(
      species,
      levels = c("aa_count","dv_count","is_count"),
      labels = c("A. americanum","D. variabilis","I. scapularis")
    )
  )

post_long <- as.data.frame(sf::st_drop_geometry(il_tick)) %>%
  dplyr::select(aa_scaled, dv_scaled, is_scaled) %>%
  tidyr::pivot_longer(everything(), names_to = "species", values_to = "value_t") %>%
  dplyr::filter(!is.na(value_t)) %>%
  dplyr::mutate(
    species = factor(
      species,
      levels = c("aa_scaled","dv_scaled","is_scaled"),
      labels = c("A. americanum","D. variabilis","I. scapularis")
    )
  )

make_panel <- function(dat, xvar, main_title) {
  p_hist <- ggplot(dat, aes(x = {{xvar}})) +
    geom_histogram(bins = 20, boundary = 0, color = "black", linewidth = 0.2) +
    facet_wrap(~ species, ncol = 1, scales = "free_y") +
    labs(title = main_title, x = NULL, y = "Frequency") +
    theme_minimal(base_size = 12)
  p_qq <- ggplot(dat, aes(sample = {{xvar}})) +
    stat_qq() + stat_qq_line(color = "red") +
    facet_wrap(~ species, ncol = 1) +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal(base_size = 12)
  p_hist + p_qq + plot_layout(ncol = 2)
}

before_panel <- make_panel(raw_long, value, "Before transformation")
after_panel  <- make_panel(post_long, value_t, "After transformation")


setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")

ggsave("distribution_before_log.png", before_panel, width = 10, height = 8, dpi = 300)
ggsave("distribution_after_log.png", after_panel, width = 10, height = 8, dpi = 300)

#View(il_tick)


```



## Mapping tick occurence count data
```{r, message=FALSE, warning=FALSE} 

library(ggplot2)
library(viridis)

## A. americanum

plot_aa_tick <- ggplot() +
  geom_sf(data = il_tick , aes(fill = `aa_scaled`), color = NA) +
  geom_sf(data = il_tick , fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(
    name = "log abundance",
    option = "plasma",
    na.value = "transparent"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 1)
    
  ) +
  labs(
    title = expression(italic("A. americanum"))
  )

print(plot_aa_tick)


## D. variabillis

plot_dv_tick <- ggplot() +
  geom_sf(data = il_tick , aes(fill = `dv_scaled`), color = NA) +
  geom_sf(data = il_tick , fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(
    name = "log abundance",
    option = "plasma",
    na.value = "transparent"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 1)
  ) +
  labs(
    title = expression(italic("D. variabilis"))
  )

print(plot_dv_tick)


## I. scapularis

plot_is_tick <- ggplot() +
  geom_sf(data = il_tick , aes(fill = `is_scaled`), color = NA) +
  geom_sf(data = il_tick , fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(
    name = "log abundance",
    option = "plasma",
    na.value = "transparent"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 1)
  ) +
  labs(
    title = expression(italic("I. scapularis"))
  )

print(plot_is_tick)


library(patchwork)

# Combine all 3 plots in one row
panel_plot_tick <- plot_aa_tick + plot_dv_tick + plot_is_tick +
  plot_layout(ncol = 3)

panel_plot_tick

setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")

# Save the plot to file at 300 DPI
ggsave(
  filename = "tick_scaled_panel.png",
  plot = panel_plot_tick,
  width = 12,     # width in inches
  height = 6,   # height in inches
  dpi = 300
)


```


## Extracting climate varibale
```{r}

#remotes::install_github("rspatial/predicts")  # for predicts::bcvars()
library(terra); library(sf); library(dplyr); library(tigris); library(predicts)

# Illinois counties (WGS84)
il_sf <- counties(state = "IL", year = 2022, cb = TRUE) |> st_as_sf() |> st_transform(4326)
il_v  <- vect(il_sf)

# Correct base for historical monthly (CRU-TS 4.09, bias-corrected) ---
base_hist <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.09"

# We need tmin, tmax, prec for two periods to cover 2019–2022
files <- c(
  "wc2.1_cruts4.09_2.5m_tmin_2010-2019.zip",
  "wc2.1_cruts4.09_2.5m_tmax_2010-2019.zip",
  "wc2.1_cruts4.09_2.5m_prec_2010-2019.zip",
  "wc2.1_cruts4.09_2.5m_tmin_2020-2024.zip",
  "wc2.1_cruts4.09_2.5m_tmax_2020-2024.zip",
  "wc2.1_cruts4.09_2.5m_prec_2020-2024.zip"
)

dir.create(data_dir <- file.path(tempdir(), "wc21_hist"), showWarnings = FALSE)

# Download & unzip 
for (f in files) {
  url  <- file.path(base_hist, f)
  dest <- file.path(data_dir, f)
  if (!file.exists(dest)) download.file(url, dest, mode = "wb", quiet = TRUE)
  unzip(dest, exdir = file.path(data_dir, tools::file_path_sans_ext(f)))
}

# getting 12 monthly GeoTIFFs for a given var/year 
get_year_stack <- function(var, year) {
  if (year == 2019) {
    period <- "2010-2019"; start <- 2010
  } else {
    period <- "2020-2024"; start <- 2020
  }
  folder <- file.path(data_dir, paste0("wc2.1_cruts4.09_2.5m_", var, "_", period))
  tifs <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
  # sort by trailing index (…_001.tif or …_01.tif etc.)
  idx  <- as.integer(sub(".*_(\\d+)\\.tif$", "\\1", tifs))
  tifs <- tifs[order(idx)]
  
  start_idx <- (year - start) * 12 + 1
  rast(tifs[start_idx:(start_idx + 11)]) |> crop(il_v) |> mask(il_v)
}

# Compute BIO1–BIO19 for 2019–2022 and average to a single county value
years <- 2019:2022
bio_by_year <- lapply(years, function(y) {
  tmin <- get_year_stack("tmin", y)
  tmax <- get_year_stack("tmax", y)
  prec <- get_year_stack("prec", y)
  
  bio  <- predicts::bcvars(prec = prec, tmin = tmin, tmax = tmax)  # 19 layers
  ex   <- terra::extract(bio, il_v, fun = mean, na.rm = TRUE)
  out  <- as.data.frame(ex)[,-1]
  names(out) <- paste0("BIO", 1:19)
  cbind(County = il_sf$NAME, Year = y, out)
})

bio_il_2019_2022_mean <- dplyr::bind_rows(bio_by_year) |>
  dplyr::group_by(County) |>
  dplyr::summarise(dplyr::across(starts_with("BIO"), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Result
print(bio_il_2019_2022_mean)


```



## Extracting elevation, land cover, and deer density
```{r}
# install.packages(c("sf","raster","elevatr","FedData","exactextractr","dplyr"))
library(sf); library(raster); library(elevatr); library(FedData)
library(exactextractr); library(dplyr)

# il_sf already made below
il_sf <- tigris::counties("IL", year = 2022, cb = TRUE) |> sf::st_as_sf() |> sf::st_transform(4326)

n_cnty <- nrow(il_sf)

# 1) Elevation (area-weighted mean, meters)
elev_r <- elevatr::get_elev_raster(il_sf, z = 8, clip = "locations")   
il_elev <- sf::st_transform(il_sf, crs(elev_r))
Elevation_m <- exactextractr::exact_extract(elev_r, il_elev, 'mean')

elev_df <- il_sf |>
  sf::st_drop_geometry() |>
  dplyr::transmute(County = NAME, Elevation_m = Elevation_m)

#View(elev_df)

# 2) Land cover 

library(exactextractr)
#  % of county in a set of NLCD codes
pct <- function(codes) exact_extract(
  nlcd_lc, il_lc,
  function(values, frac){
    tot <- sum(frac, na.rm=TRUE); if (tot == 0) return(NA_real_)
    100 * sum(frac[values %in% codes], na.rm=TRUE) / tot
  }
)



options(timeout = 600)

nlcd_terra <- FedData::get_nlcd(
  template = il_sf,
  label = "IL",
  year = 2019,
  dataset = "landcover"
)



nlcd_lc <- raster::raster(nlcd_terra)         
il_lc   <- sf::st_transform(il_sf, crs(nlcd_lc))  


LC_water_pct     <- pct(11)
LC_developed_pct <- pct(c(21,22,23,24))
LC_barren_pct    <- pct(31)
LC_forest_pct    <- pct(c(41,42,43))
LC_grassland_pct <- pct(71)
LC_cropland_pct  <- pct(c(81,82))
LC_wetland_pct   <- pct(c(90,95))

landcover_il <- data.frame(
  County = il_sf$NAME,
  LC_water_pct, LC_developed_pct, LC_barren_pct, LC_forest_pct,
  LC_grassland_pct, LC_cropland_pct, LC_wetland_pct,
  check.names = FALSE
)


#View(landcover_il)

# 3) Deer habitat: % suitable (local USGS GAP raster; set path) 

# Deer suitability (mean %, area-weighted)
deer_path <- "C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Tick Passive Survillance/Data/Deer Habitate Data (Illinois)/Deer Habitat Rasters/Deer_Habitat_2022.tif"
deer_r  <- raster(deer_path)                          
il_deer <- st_transform(il_sf, crs(deer_r))
DeerSuitability_pct <- exact_extract(
  deer_r, il_deer,
  function(v,f){ 10 * sum(v*f, na.rm=TRUE) / sum(f, na.rm=TRUE) }  
)

# two clean columns (County, DeerSuitability_pct)
deer_df <- il_sf |>
  sf::st_drop_geometry() |>
  dplyr::transmute(County = NAME, DeerSuitability_pct = DeerSuitability_pct)


#View(deer_df)

# Final county table 
covar3 <- elev_df %>%
  left_join(deer_df,      by = "County") %>%
  left_join(landcover_il, by = "County")



#View(covar3)

```


## Merging climate variable, other covariates and shape file
```{r}

# Standardize names for merging
covar3$County <- gsub("\\s+", "",tolower(trimws(covar3$County)))
bio_il_2019_2022_mean$County <- gsub("\\s+", "",tolower(trimws(bio_il_2019_2022_mean$County)))

# Merge both
covariate <- left_join(bio_il_2019_2022_mean, covar3, by = c("County" = "County"))

#View(covariate)

##Now join with shape file 

# Standardize names for merging
covariate$County <- gsub("\\s+", "",tolower(trimws(covariate$County)))
il_tick$COUNTY_NAM <- gsub("\\s+", "",tolower(trimws(il_tick$COUNTY_NAM)))

# Merge both
il_tick<- left_join(il_tick, covariate, by = c("COUNTY_NAM" = "County"))

#il_tick

#View(il_tick)
```


## Fragmentation
```{r}
#install.packages(c("FedData","raster","sf","landscapemetrics","dplyr"))
library(FedData); library(raster); library(sf)
library(landscapemetrics); library(dplyr)

# 1) Get NLCD (land cover) for Illinois and align with counties
nlcd_terra <- FedData::get_nlcd(template = il_sf, year = 2019, dataset = "landcover", label = "IL")
nlcd       <- raster(nlcd_terra)                                # terra -> raster
il_nlcd    <- st_transform(il_sf, crs(nlcd))

# 2) Helper: metrics for one polygon (here, forest = 41,42,43)
forest_classes <- c(41, 42, 43)

get_frag_one <- function(poly_sf) {
  poly_sp <- as(poly_sf, "Spatial")                                   
  r <- raster::mask(raster::crop(nlcd, poly_sp), poly_sp)            
  if (is.null(r) || all(is.na(r[]))) {                                
    return(data.frame(forest_pd = NA_real_, forest_area_mn = NA_real_,
                      forest_ed = NA_real_, forest_clumpy = NA_real_))
  }
  m <- landscapemetrics::calculate_lsm(
    r,
    what   = c("lsm_c_pd", "lsm_c_area_mn", "lsm_c_ed", "lsm_c_clumpy"),
    level  = "class",
    classes = forest_classes
  )
  
  m |>
    dplyr::group_by(metric) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = metric, values_from = value, names_prefix = "forest_")
}

# 3) Run per county (can take a few minutes)
frag_list <- lapply(split(il_nlcd, seq_len(nrow(il_nlcd))), get_frag_one)  # pass one-row sf features
frag_df   <- bind_rows(frag_list)

# 4) Attach to counties
il_frag <- bind_cols(st_drop_geometry(il_sf), frag_df)
# il_frag now has: forest_pd, forest_area_mn, forest_ed, forest_clumpy by county

#View(il_frag)


#Combine to single fragmentation variable

library(dplyr)
library(scales)

il_frag <- il_frag |>
  mutate(
    z_ed     = as.numeric(scale(forest_ed)),
    z_pd     = as.numeric(scale(forest_pd)),
    z_area   = as.numeric(scale(forest_area_mn)),
    z_clumpy = as.numeric(scale(forest_clumpy)),
    
    forest_frag_index_raw = z_ed + z_pd - z_area - z_clumpy,
    forest_fragmentation  = scales::rescale(forest_frag_index_raw, to = c(0, 1), na.rm = TRUE)
  ) |>
  dplyr::select(-z_ed, -z_pd, -z_area, -z_clumpy, -forest_frag_index_raw)


il_frag <- il_frag |> dplyr::select(NAME, forest_fragmentation)

#View(il_frag)

#Merg with major file

# Standardize names for merging
il_frag$NAME <- gsub("\\s+", "",tolower(trimws(il_frag$NAME)))
il_tick$COUNTY_NAM <- gsub("\\s+", "",tolower(trimws(il_tick$COUNTY_NAM)))

# Merge both
il_tick<- left_join(il_tick, il_frag, by = c("COUNTY_NAM" = "NAME"))


#View(il_tick)

```




## correlation matrix for covariates and outcome variables 
```{r}

#install.packages(c("dplyr","ggcorrplot", "plyr"))  
library(dplyr)
library(ggcorrplot)

dat <- il_tick |>
  sf::st_drop_geometry() |>
  dplyr::select(
    BIO1:BIO19,
    DeerSuitability_pct,
    LC_forest_pct, LC_grassland_pct, LC_wetland_pct,
    Elevation_m, LC_water_pct, LC_developed_pct, LC_barren_pct, LC_cropland_pct, forest_fragmentation,
    aa_scaled, dv_scaled, is_scaled
  ) |>
  dplyr::mutate(dplyr::across(everything(), as.numeric))

# Keep only complete rows (no NA)
dat_complete <- dat |> tidyr::drop_na()

# Correlation + p-values
cmat <- cor(dat_complete, method = "pearson")
pmat <- ggcorrplot::cor_pmat(dat_complete)

#  ARRANGE VARIABLES 
var_order <- c(
  paste0("BIO", 1:19),
  "Elevation_m",
  "DeerSuitability_pct",
  "LC_forest_pct", "LC_grassland_pct", "LC_wetland_pct",
  "LC_water_pct", "LC_developed_pct", "LC_barren_pct", "LC_cropland_pct", "forest_fragmentation",
  "aa_scaled", "dv_scaled", "is_scaled"
)

cmat <- cmat[var_order, var_order]
pmat <- pmat[var_order, var_order]


# Plot
corr_plot <- ggcorrplot(
  cmat,
  type = "full", lab = TRUE, lab_size = 3,
  hc.order = FALSE, outline.color = NA,     # disable auto ordering (we did it manually)
  ggtheme = theme_minimal(),
  colors = c("#313695", "#FFFFFF", "#A50026")
) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  ggtitle("Correlation Matrix")

corr_plot

# Save high-quality image
setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")

ggsave(
  filename = "tick_scaled_correlation.png",
  plot = corr_plot,
  width = 12, height = 12, dpi = 300
)



```



## regression
```{r}
library(dplyr)
library(car)

# Define predictor set
predictors <- "BIO8 + BIO10 + BIO16 + BIO18  + DeerSuitability_pct +
                LC_grassland_pct + LC_wetland_pct + forest_fragmentation"

# Prepare data: drop geometry & remove rows with NA
data_mod <- il_tick |>
  sf::st_drop_geometry() |>
  dplyr::select(dplyr::all_of(c(
    "aa_scaled", "dv_scaled", "is_scaled",
    "BIO8","BIO10","BIO16","BIO18",
    "DeerSuitability_pct",
    "LC_grassland_pct","LC_wetland_pct", "forest_fragmentation"
  ))) |>
  tidyr::drop_na()


#GLM 

model_aa <- lm(as.formula(paste("aa_scaled ~", predictors)), data = data_mod)
summary(model_aa)
car::vif(model_aa)

model_dv <- lm(as.formula(paste("dv_scaled ~", predictors)), data = data_mod)
summary(model_dv)
car::vif(model_dv)

model_is <- lm(as.formula(paste("is_scaled ~", predictors)), data = data_mod)
summary(model_is)
car::vif(model_is)

```


## Mapping Covariates
```{r}
# Load libraries
library(ggplot2)
library(patchwork)
library(viridis)
library(sf)


# Base theme
my_theme <- theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), # Centered title
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8)
  )

# Individual plots
p1 <- ggplot(il_tick) +
  geom_sf(aes(fill = BIO8), color = NA) +
  scale_fill_viridis_c(
    name = "BIO8\n(Mean Temp. of Wettest Qtr)",
    option = "plasma", na.value = "transparent",
    guide = guide_colorbar(direction = "horizontal",
                           barwidth = unit(6, "cm"), barheight = unit(0.6, "cm"),
                           title.position = "top")
  ) +
  ggtitle("Temperature 1") +
  my_theme

p2 <- ggplot(il_tick) +
  geom_sf(aes(fill = BIO10), color = NA) +
  scale_fill_viridis_c(
    name = "BIO10\n(Mean Temp. of Warmest Qtr)",
    option = "plasma", na.value = "transparent",
    guide = guide_colorbar(direction = "horizontal",
                           barwidth = unit(6, "cm"), barheight = unit(0.6, "cm"),
                           title.position = "top")
  ) +
  ggtitle("Temperature 2") +
  my_theme

p3 <- ggplot(il_tick) +
  geom_sf(aes(fill = BIO16), color = NA) +
  scale_fill_viridis_c(
    name = "BIO16\n(Precipitation of Wettest Qtr)",
    option = "plasma", na.value = "transparent",
    guide = guide_colorbar(direction = "horizontal",
                           barwidth = unit(6, "cm"), barheight = unit(0.6, "cm"),
                           title.position = "top")
  ) +
  ggtitle("Precipitation 1") +
  my_theme

p4 <- ggplot(il_tick) +
  geom_sf(aes(fill = BIO18), color = NA) +
  scale_fill_viridis_c(
    name = "BIO18\n(Precipitation of Warmest Qtr)",
    option = "plasma", na.value = "transparent",
    guide = guide_colorbar(direction = "horizontal",
                           barwidth = unit(6, "cm"), barheight = unit(0.6, "cm"),
                           title.position = "top")
  ) +
  ggtitle("Precipitation 2") +
  my_theme


p5 <- ggplot(il_tick) +
  geom_sf(aes(fill = LC_grassland_pct), color = NA) +
  scale_fill_viridis_c(
    name = "Grassland Cover (%)",
    option = "plasma", na.value = "transparent",
    guide = guide_colorbar(direction = "horizontal",
                           barwidth = unit(6, "cm"), barheight = unit(0.6, "cm"),
                           title.position = "top")
  ) +
  ggtitle("Grassland") +
  my_theme

p6 <- ggplot(il_tick) +
  geom_sf(aes(fill = LC_wetland_pct), color = NA) +
  scale_fill_viridis_c(
    name = "Wetland Cover (%)",
    option = "plasma", na.value = "transparent",
    guide = guide_colorbar(direction = "horizontal",
                           barwidth = unit(6, "cm"), barheight = unit(0.6, "cm"),
                           title.position = "top")
  ) +
  ggtitle("Wetland") +
  my_theme

p7 <- ggplot(il_tick) +
  geom_sf(aes(fill = forest_fragmentation), color = NA) +
  scale_fill_viridis_c(
    name = "Forest Fragmentation Index",
    option = "plasma", na.value = "transparent",
    guide = guide_colorbar(direction = "horizontal",
                           barwidth = unit(6, "cm"), barheight = unit(0.6, "cm"),
                           title.position = "top")
  ) +
  ggtitle("Fragmentation") +
  my_theme

p8 <- ggplot(il_tick) +
  geom_sf(aes(fill = LC_forest_pct), color = NA) +
  scale_fill_viridis_c(
    name = "Forest Cover (%)",
    option = "plasma", na.value = "transparent",
    guide = guide_colorbar(direction = "horizontal",
                           barwidth = unit(6, "cm"), barheight = unit(0.6, "cm"),
                           title.position = "top")
  ) +
  ggtitle("Forest") +
  my_theme

p9 <- ggplot(il_tick) +
  geom_sf(aes(fill = DeerSuitability_pct), color = NA) +
  scale_fill_viridis_c(
    name = "Deer Suitability (%)",
    option = "plasma", na.value = "transparent",
    guide = guide_colorbar(direction = "horizontal",
                           barwidth = unit(6, "cm"), barheight = unit(0.6, "cm"),
                           title.position = "top")
  ) +
  ggtitle("Deer") +
  my_theme

# Combine into 3x2 panel
panel_plot <- (p1 | p2 | p3 | p4 | p5) / (p6 | p7 | p8| p9)

# Show the panel
panel_plot

# Save high-quality image
setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")

# Save output (set your desired path)
ggsave("covariates_panel_il_tick.png", plot = panel_plot,
       dpi = 300, width = 14, height = 12, units = "in", bg = "white")

```


## PCA
```{r, message=FALSE, warning=FALSE}
## PCA 1: Climate Variables (BIO8, BIO10, BIO16, BIO18)

library(factoextra)
library(ggplot2)
library(dplyr)
library(sf)

#  Prepare data (drop geometry if present)
env_clim <- il_tick %>%
  st_drop_geometry() %>%
  dplyr::select(COUNTY_NAM, BIO8, BIO10, BIO16, BIO18) %>%
  mutate(across(c(BIO8, BIO10, BIO16, BIO18), as.numeric))

# Run PCA
pca_clim <- prcomp(env_clim[, c("BIO8", "BIO10", "BIO16", "BIO18")],
                   center = TRUE, scale. = TRUE)

# PCA scores
pca_scores_clim <- as.data.frame(pca_clim$x[, 1:4])
colnames(pca_scores_clim) <- c("PC1_clim", "PC2_clim", "PC3_clim", "PC4_clim")
pca_scores_clim$COUNTY_NAM <- env_clim$COUNTY_NAM

# Join back to il_tick
il_tick <- left_join(il_tick, pca_scores_clim, by = "COUNTY_NAM")

# Summaries
summary(pca_clim)
pca_clim$sdev
pca_clim$rotation
pca_clim$center
pca_clim$scale

# Scree plot
screeplot_clim <- fviz_eig(pca_clim, addlabels = TRUE, ylim = c(0, 100)) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())
print(screeplot_clim)

# Loadings
loadings_clim <- as.data.frame(pca_clim$rotation)
print(loadings_clim)

# Save Scree Plot
setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")
ggsave("IL_PCA_climate_screeplot.png", plot = screeplot_clim, width = 8, height = 6, dpi = 300)

# Biplot
biplot_clim <- fviz_pca_var(pca_clim,
                            col.var = "cos2",
                            gradient.cols = c("red", "blue", "green"),
                            repel = TRUE) +
  scale_color_gradientn(colors = c("red", "blue", "green"),
                        breaks = c(0.25, 0.50, 0.75),
                        limits = c(0, 1)) +
  labs(color = "Correlation") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(biplot_clim)
ggsave("IL_PCA_climate_biplot_cos2.png", plot = biplot_clim, width = 8, height = 6, dpi = 300)


## PCA 2: Land Cover Variables (LC_grassland_pct, LC_wetland_pct, LC_forest_pct, forest_fragmentation) 

# Prepare data
env_LC <- il_tick %>%
  st_drop_geometry() %>%
  dplyr::select(COUNTY_NAM, LC_grassland_pct, LC_wetland_pct, LC_forest_pct, forest_fragmentation) %>%
  mutate(across(c(LC_grassland_pct, LC_wetland_pct, LC_forest_pct, forest_fragmentation), as.numeric))

#  Run PCA
pca_LC <- prcomp(env_LC[, c("LC_grassland_pct", "LC_wetland_pct", "LC_forest_pct", "forest_fragmentation")],
                 center = TRUE, scale. = TRUE)

# PCA scores
pca_scores_LC <- as.data.frame(pca_LC$x[, 1:4])
colnames(pca_scores_LC) <- c("PC1_LC", "PC2_LC", "PC3_LC", "PC4_LC")
pca_scores_LC$COUNTY_NAM <- env_LC$COUNTY_NAM

# Step 4: Join back to il_tick
il_tick <- left_join(il_tick, pca_scores_LC, by = "COUNTY_NAM")

# Summaries
summary(pca_LC)
pca_LC$sdev
pca_LC$rotation
pca_LC$center
pca_LC$scale

# Scree plot
screeplot_LC <- fviz_eig(pca_LC, addlabels = TRUE, ylim = c(0, 100)) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())
print(screeplot_LC)

# Loadings
loadings_LC <- as.data.frame(pca_LC$rotation)
print(loadings_LC)

# Save Scree Plot
ggsave("IL_PCA_landcover_screeplot.png", plot = screeplot_LC, width = 8, height = 6, dpi = 300)

# Biplot
biplot_LC <- fviz_pca_var(pca_LC,
                          col.var = "cos2",
                          gradient.cols = c("red", "blue", "green"),
                          repel = TRUE) +
  scale_color_gradientn(colors = c("red", "blue", "green"),
                        breaks = c(0.25, 0.50, 0.75),
                        limits = c(0, 1)) +
  labs(color = "Correlation") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(biplot_LC)
ggsave("IL_PCA_landcover_biplot_cos2.png", plot = biplot_LC, width = 8, height = 6, dpi = 300)

##Correlation among PCs
library(ggcorrplot)
cmat <- cor(st_drop_geometry(il_tick)[, c("PC1_clim", "PC2_clim", "PC1_LC", "PC2_LC", "DeerSuitability_pct", "aa_scaled", "dv_scaled", "is_scaled")], use = "pairwise.complete.obs")

ggcorrplot(cmat, lab = TRUE, ggtheme = theme_minimal(),
           colors = c("#313695", "#FFFFFF", "#A50026")) +
  ggtitle("Correlation among Climate and Land Cover PCs")


```


## Removing the duplicate columns if needed
```{r}
library(dplyr)
#View(il_tick)

#il_tick <- dplyr::select(il_tick, -PC1.y, -PC2.y, -PC3.y, -PC4.y)


#il_tick <- dplyr::rename(
# il_tick,
#PC1 = PC1.x,
# PC2 = PC2.x,
#PC3 = PC3.x,
#PC4 = PC4.x,

#)


#View(il_tick)

```


## setting up Neighbour weigght matrix
```{r, message=FALSE, warning=FALSE}
library(sf)
library(INLA)
library(spdep)
library(dplyr)
library(GGally)
library(ggplot2)
library(mapview)



## Correlation analysis

final_covars <- il_tick %>%
  st_drop_geometry() %>%
  dplyr::select(
    aa_scaled, dv_scaled, is_scaled,
    BIO8, BIO10, BIO16, BIO18,
    DeerSuitability_pct,
    LC_grassland_pct, LC_wetland_pct,
    forest_fragmentation
  )

# Pairwise correlation matrix
p_corr <- ggpairs(final_covars)

p_corr

#Set directory
setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")

ggsave("selected_covarites_Correlation_Plot.png", p_corr, width = 10, height = 10, dpi = 300)


#Heatmap-style correlation plot (paste below your ggpairs)

library(ggcorrplot)

# Manually specify variable order
var_order <- c(
  "aa_scaled", "dv_scaled", "is_scaled",
  "BIO8", "BIO10", "BIO16", "BIO18",
  "DeerSuitability_pct",
  "LC_grassland_pct", "LC_wetland_pct",
  "forest_fragmentation"
)

# Compute correlation matrix in this exact order
cmat <- cor(final_covars[, var_order], use = "pairwise.complete.obs")

# Plot correlation heatmap
corr_plot <- ggcorrplot(
  cmat, type = "full", lab = TRUE, lab_size = 3,
  hc.order = FALSE, outline.color = NA,
  ggtheme = theme_minimal(),
  colors = c("#313695", "#FFFFFF", "#A50026")
) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  ggtitle("Correlation Matrix")

corr_plot

#Set directory
setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")

ggsave("selected_covarites_Correlation_heatmap.png", corr_plot, width = 10, height = 10, dpi = 300)



## Neighbourhood matrix

# Geometry preparation
il_tick <- st_make_valid(il_tick)
il_tick <- st_transform(il_tick, 26916)   # UTM Zone 16N (meters)

# KNN (k = 5) neighbors
pts    <- st_point_on_surface(st_geometry(il_tick))
coords <- st_coordinates(pts)

knn5   <- knearneigh(coords, k = 5)
nb_knn5 <- knn2nb(knn5, sym = TRUE)

# Sanity check
table(card(nb_knn5))
stopifnot(min(sapply(nb_knn5, length)) >= 5)


# Save adjacency for INLA
nb2INLA("il_tick_knn5.adj", nb_knn5)
g <- inla.read.graph("il_tick_knn5.adj")

# Structured/unstructured effects
il_tick$re_u <- 1:nrow(il_tick)  # structured (spatial)
il_tick$re_v <- 1:nrow(il_tick)  # unstructured (iid)

#Visualize neighbor connections
sl <- nb2lines(nb_knn5, coords = coords,
               proj4string = as(st_crs(il_tick), "CRS"))
lines_sf <- st_as_sf(sl)

# Clean and styled plot
p_knn <- ggplot() +
  geom_sf(data = il_tick, fill = "white", color = "grey60", linewidth = 0.3) +
  geom_sf(data = lines_sf, linewidth = 0.4, alpha = 0.9, color = "red") +
  geom_sf(data = pts, size = 1.2, color = "blue") +
  labs(
    title = "Neighbourhood Matrix"
  ) +
  theme_void() +  # removes background & gridlines completely
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # center title
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

# Display
p_knn

# Save in your specified directory
setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")

ggsave("il_tick_KNN5_map.png", p_knn, width = 8, height = 12, dpi = 300, bg = "white")


```


## Non-spatial
```{r}

formula_nsp_aa <- aa_scaled ~ 
  PC1_clim + PC2_clim +  PC1_LC + PC2_LC + DeerSuitability_pct

model_nsp_aa <- inla(
  formula_nsp_aa,
  family = "gaussian",   
  data   = il_tick,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
)



summary(model_nsp_aa)
model_nsp_aa$summary.fixed
model_nsp_aa$summary.hyperpar   

# CPO diagnostics ---
table(model_nsp_aa$cpo$failure)   # ideally all 0

plot(-log(model_nsp_aa$cpo$cpo),
     main = "-log(CPO) (larger = worse fit)",
     ylab = "-log(CPO)", xlab = "Obs. index")

hist(model_nsp_aa$cpo$pit, breaks = 20,
     main = "PIT histogram", xlab = "PIT")

# Fitted & residuals
fhat <- model_nsp_aa$summary.fitted.values$mean
il_tick$predicted_nsp_aa <- fhat
il_tick$residuals_nsp_aa <- il_tick$aa_scaled - fhat

plot(il_tick$predicted_nsp_aa, il_tick$residuals_nsp_aa,
     main = "Residuals vs Fitted (non-spatial",
     xlab = "Fitted", ylab = "Residuals"); abline(h = 0, col = "red")


library(tmap)
tm_shape(il_tick) +
  tm_fill("residuals_nsp_aa", palette = "RdBu", style = "cont",
          title = "Residuals (non-spatial)") +
  tm_borders()

```


##  BYM2 (Besag-York-Mollié 2) Model [AA]
```{r, message=FALSE, warning=FALSE}

# Formula 
formula_bym2_aa <- aa_scaled ~ 
  PC1_clim + PC2_clim + PC1_LC + PC2_LC + DeerSuitability_pct +
  f(re_u, model = "bym2", graph = g, scale.model = TRUE)

# Model
model_bym2_aa <- inla(
  formula_bym2_aa,
  family = "gaussian",
  data = il_tick,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
)


# Summary
summary(model_bym2_aa)

#Significance
model_bym2_aa$summary.fixed
model_bym2_aa$summary.hyperpar


#Check for Poor Fit Using CPO (Conditional Predictive Ordinate)

# Failures (should be 0 ideally)
table(model_bym2_aa$cpo$failure)

# Use -log(CPO): larger = worse predictive support
plot(-log(model_bym2_aa$cpo$cpo),
     main = "-log(CPO) (larger = worse fit)",
     ylab = "-log(CPO)", xlab = "Area index")


# PIT histogram: should be ~Uniform if calibrated
hist(model_bym2_aa$cpo$pit, breaks = 20, main = "PIT histogram", xlab = "PIT")


# Fitted & residuals 
fhat <- model_bym2_aa$summary.fitted.values$mean         
il_tick$predicted_bym2_aa <- fhat                    
il_tick$residuals_bym2_aa <- il_tick$aa_scaled - fhat 

plot(il_tick$predicted_bym2_aa, il_tick$residuals_bym2_aa,
     main = "Residuals vs Fitted",
     xlab = "Fitted ", ylab = "Residuals")
abline(h = 0, col = "red")


# Map log-scale residuals 
library(tmap)
tm_shape(il_tick) +
  tm_fill("residuals_bym2_aa", palette = "RdBu", style = "cont", title = "Residuals") +
  tm_borders()



library(car)

# fit a simple linear model (same predictors as INLA fixed effects)
lm_vif <- lm(aa_scaled ~ PC1_clim + PC2_clim + PC1_LC + PC2_LC + DeerSuitability_pct,
             data = as.data.frame(sf::st_drop_geometry(il_tick)))

# compute VIF
vif_values <- car::vif(lm_vif)
print(vif_values)

## INTERPERTATION
# DIC = -276.7, WAIC = -286.7 → strong overall model fit  
# Residuals small (±3e-4), PIT uniform SO good calibration  
# Covariates not significant; spatial BYM2 explains variation  


```


## Spatial k-Fold Cross-Validation (aa)

```{r}
library(sf)
library(dplyr)
library(blockCV)
library(INLA)
library(ggplot2)

#  Ensure BYM2 index 
if (!"re_u" %in% names(il_tick)) il_tick$re_u <- seq_len(nrow(il_tick))

#  Create spatial folds (5-fold) (50KM) 
set.seed(123)
centroids <- st_point_on_surface(st_geometry(il_tick))
pts <- st_as_sf(data.frame(id = il_tick$re_u), geometry = centroids)
sp_folds <- spatialBlock(speciesData = pts, theRange = 50000, k = 5,
                         selection = "random", iteration = 200,
                         showBlocks = FALSE, verbose = FALSE)
fold_id <- sp_folds$foldID

# Model formula (BYM2, Poisson) 
formula_bym2 <- aa_scaled ~ PC1_clim + PC2_clim + PC1_LC +
  f(re_u, model = "bym2", graph = g, scale.model = TRUE)

#  Spatial k-fold loop
cv <- lapply(1:5, function(i) {
  dat <- il_tick; dat$aa_scaled[fold_id == i] <- NA
  fit <- inla(formula_bym2, family = "gaussian", data = dat,
              control.predictor = list(compute = TRUE))
  data.frame(fold = i,
             obs = il_tick$aa_scaled[fold_id == i],
             pred = fit$summary.fitted.values$mean[fold_id == i])
})
cv_results <- bind_rows(cv)

# Metrics 
rmse <- sqrt(mean((cv_results$obs - cv_results$pred)^2, na.rm=TRUE))
mae  <- mean(abs(cv_results$obs - cv_results$pred), na.rm=TRUE)
r2   <- cor(cv_results$obs, cv_results$pred, use="complete.obs")^2
rho  <- cor(cv_results$obs, cv_results$pred, use="complete.obs", method="spearman")
cat(sprintf("RMSE=%.2f  MAE=%.2f  R²=%.2f  Spearmanρ=%.2f\n", rmse, mae, r2, rho))


# Interpretation:
# 5-fold spatial CV (Gaussian on aa_scaled): RMSE = 0.81, MAE = 0.62, R² = 0.24, Spearman ρ = 0.47  SO moderate predictive skill
# Rank correlation is decent (ρ = 0.53) but variance explained is modest (R² = 0.25) → captures ordering better than magnitudes

```


## Prediting [aa]
```{r}
library(INLA)
library(sf)
library(dplyr)
library(tmap)

## Prepare data: allow NAs in response for unsampled counties
dat <- st_drop_geometry(il_tick)

# Check: which are unsampled
unsampled_idx <- which(is.na(dat$aa_scaled))
length(unsampled_idx)  # just to see how many

## Fit BYM2 model including NA outcomes (INLA will predict them)
form <- aa_scaled ~ PC1_clim + PC2_clim + PC1_LC +
  f(re_u, model="bym2", graph=g, scale.model=TRUE)

fit <- inla(
  formula_bym2_aa,
  family = "gaussian",
  data = il_tick,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
)




## Attach predictions + 95% CrI to the sf
pred_sum <- fit$summary.fitted.values
il_tick$pred_mean   = pred_sum$mean
il_tick$pred_q025   = pred_sum$`0.025quant`
il_tick$pred_q975   = pred_sum$`0.975quant`
il_tick$pred_ci_w   = il_tick$pred_q975 - il_tick$pred_q025  # CI width
il_tick$residual    = ifelse(is.na(il_tick$aa_scaled), NA_real_,
                             il_tick$aa_scaled - il_tick$pred_mean)

## Diagnostics (only for sampled counties; unsampled will be NA in CPO/PIT)
cat("CPO failures (should be 0 ideally):\n")
print(table(fit$cpo$failure, useNA="ifany"))

par(mfrow=c(1,2))
hist(fit$cpo$pit, breaks=20, main="PIT (sampled only)", xlab="PIT")
plot(-log(fit$cpo$cpo), main="-log(CPO) (sampled only)", ylab="-log(CPO)", xlab="Area index")
par(mfrow=c(1,1))

## Quick residuals vs fitted (sampled only)
sampled <- !is.na(il_tick$aa_scaled)
plot(il_tick$pred_mean[sampled], il_tick$residual[sampled],
     main="Residuals vs Fitted (sampled only)",
     xlab="Fitted (mean)", ylab="Residual"); abline(h=0, col=2)

## 5) Maps: predicted mean and CrI for ALL counties
tmap_mode("plot")

map_mean_aa <- tm_shape(il_tick) +
  tm_fill("pred_mean", palette="plasma", style="cont", title="Predicted mean") +
  tm_borders()

map_lo_aa <- tm_shape(il_tick) +
  tm_fill("pred_q025", palette="plasma", style="cont", title="Lower 95%") +
  tm_borders()

map_hi_aa <- tm_shape(il_tick) +
  tm_fill("pred_q975", palette="plasma", style="cont", title="Upper 95%") +
  tm_borders()

map_ci_aa <- tm_shape(il_tick) +
  tm_fill("pred_ci_w", palette="-viridis", style="cont", title="95% CI width") +
  tm_borders()

map_mean_aa
map_lo_aa
map_hi_aa
map_ci_aa


# place legend inside without overlap
place_inside <- function(p) {
  p +
    tm_layout(
      frame = FALSE,                    
      legend.outside = FALSE,
      legend.position = c("left", "bottom"),
      legend.just = c("left", "bottom"),
      legend.bg.color = "white",
      legend.frame = TRUE,
      legend.text.size = 0.6,
      legend.title.size = 0.8,
      inner.margins = c(0.15, 0.02, 0.02, 0.02)
    )
}

#Apply to each
map_mean_aa2 <- place_inside(map_mean_aa) +
  tm_compass(position = c("left", "top"), type = "8star")                 

map_lo_aa2 <- place_inside(map_lo_aa) 
map_hi_aa2 <- place_inside(map_hi_aa) +     
  tm_scale_bar(position = c("right", "bottom"))

# panel
panel_aa <- tmap_arrange(map_mean_aa2, map_lo_aa2, map_hi_aa2, ncol = 3)

panel_aa

# Save in your specified directory
setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")

# save high-resolution
tmap_save(panel_aa,
          filename = "amblyoma_americanum_panel_legends_inside.tiff",
          width = 14, height = 8, units = "in", dpi = 300)

```



##  BYM2 (Besag-York-Mollié 2) Model  [DV]
```{r, message=FALSE, warning=FALSE}

# Formula 
formula_bym2_dv <- dv_scaled ~ 
  PC1_clim + PC2_clim + PC1_LC + PC2_LC + DeerSuitability_pct +
  f(re_u, model = "bym2", graph = g, scale.model = TRUE)


# Model
model_bym2_dv <- inla(
  formula_bym2_dv,
  family = "gaussian",
  data = il_tick,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
)



# Summary
summary(model_bym2_dv)

#Significance
model_bym2_dv$summary.fixed
model_bym2_dv$summary.hyperpar


#Check for Poor Fit Using CPO (Conditional Predictive Ordinate)

# Failures (should be 0 ideally)
table(model_bym2_dv$cpo$failure)

# Use -log(CPO): larger = worse predictive support
plot(-log(model_bym2_dv$cpo$cpo),
     main = "-log(CPO) (larger = worse fit)",
     ylab = "-log(CPO)", xlab = "Area index")


# PIT histogram: should be ~Uniform if calibrated
hist(model_bym2_dv$cpo$pit, breaks = 20, main = "PIT histogram", xlab = "PIT")


# Fitted & residuals 
fhat <- model_bym2_dv$summary.fitted.values$mean         
il_tick$predicted_bym2_dv <- fhat                    
il_tick$residuals_bym2_dv <- il_tick$dv_scaled - fhat 

plot(il_tick$predicted_bym2_dv, il_tick$residuals_bym2_dv,
     main = "Residuals vs Fitted",
     xlab = "Fitted ", ylab = "Residuals")
abline(h = 0, col = "red")


# Map log-scale residuals 
library(tmap)
tm_shape(il_tick) +
  tm_fill("residuals_bym2_dv", palette = "RdBu", style = "cont", title = "Residuals") +
  tm_borders()


#INTERPERTATION
# model: DIC = -431.18, WAIC = -446.30 SO strong fit  
# PC1_LC negative (-0.26, CI -0.49 to -0.03) SO higher land-cover PC1 reduces tick abundance  
# Other predictors not significant; spatial BYM2 term explains main pattern  
# Residuals small, PIT ~ uniform → model well calibrated with mild spatial structure remaining  

```


## Spatial k-Fold Cross-Validation (dv)
```{r}

library(sf)
library(dplyr)
library(blockCV)
library(INLA)

# Ensure BYM2 index
if (!"re_u" %in% names(il_tick)) il_tick$re_u <- seq_len(nrow(il_tick))

# Create spatial folds (5-fold; ~50 km blocks)
set.seed(123)
centroids <- st_point_on_surface(st_geometry(il_tick))
pts <- st_as_sf(data.frame(id = il_tick$re_u), geometry = centroids)
sp_folds <- spatialBlock(speciesData = pts, theRange = 50000, k = 5,
                         selection = "random", iteration = 200,
                         showBlocks = FALSE, verbose = FALSE)
fold_id <- sp_folds$foldID

# BYM2 formula for DV
formula_bym2_dv <- dv_scaled ~ 
  PC1_clim + PC2_clim + PC1_LC + PC2_LC + DeerSuitability_pct +
  f(re_u, model = "bym2", graph = g, scale.model = TRUE)

# Spatial k-fold loop (held-out counts = NA)
cv_dv <- lapply(1:5, function(i) {
  dat <- il_tick
  dat$dv_scaled[fold_id == i] <- NA
  fit <- inla(formula_bym2_dv, family = "gaussian", data = dat,
              control.predictor = list(compute = TRUE))
  data.frame(
    fold = i,
    obs  = il_tick$dv_scaled[fold_id == i],
    pred = fit$summary.fitted.values$mean[fold_id == i]
  )
})
cv_results_dv <- bind_rows(cv_dv)

# Metrics
rmse_dv <- sqrt(mean((cv_results_dv$obs - cv_results_dv$pred)^2, na.rm = TRUE))
mae_dv  <- mean(abs(cv_results_dv$obs - cv_results_dv$pred), na.rm = TRUE)
r2_dv   <- cor(cv_results_dv$obs, cv_results_dv$pred, use = "complete.obs")^2
rho_dv  <- suppressWarnings(cor(cv_results_dv$obs, cv_results_dv$pred,
                                use = "complete.obs", method = "spearman"))
cat(sprintf("DV Spatial 5-fold CV — RMSE=%.2f  MAE=%.2f  R²=%.2f  Spearmanρ=%.2f\n",
            rmse_dv, mae_dv, r2_dv, rho_dv))


# Interpretation:
# Spatial 5-fold CV (50 km): RMSE = 0.78, MAE = 0.62, R² = 0.19, ρ = 0.50 SO moderate predictive accuracy and improved spatial generalization.


```


## Predicting [DV]
```{r}
library(INLA)
library(sf)
library(dplyr)
library(tmap)

## Prep (optional check of unsampled)
dat <- st_drop_geometry(il_tick)
unsampled_idx <- which(is.na(dat$dv_scaled))
length(unsampled_idx)

## Model (DV): BYM2 
formula_bym2_dv <- dv_scaled ~ 
  PC1_clim + PC2_clim + PC1_LC + PC2_LC + DeerSuitability_pct +
  f(re_u, model = "bym2", graph = g, scale.model = TRUE)

fit_dv <- inla(
  formula_bym2_dv,
  family = "gaussian",
  data = il_tick,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
)


## Attach predictions + 95% CrI to sf
pred_sum <- fit_dv$summary.fitted.values
il_tick$pred_mean_dv <- pred_sum$mean
il_tick$pred_q025_dv <- pred_sum$`0.025quant`
il_tick$pred_q975_dv <- pred_sum$`0.975quant`
il_tick$pred_ci_w_dv <- il_tick$pred_q975_dv - il_tick$pred_q025_dv
il_tick$residual_dv  <- ifelse(is.na(il_tick$dv_scaled), NA_real_,
                               il_tick$dv_scaled - il_tick$pred_mean_dv)

## 3) Diagnostics (sampled only)
cat("CPO failures (dv):\n"); print(table(fit_dv$cpo$failure, useNA="ifany"))

par(mfrow=c(1,2))
hist(fit_dv$cpo$pit, breaks=20, main="PIT (dv, sampled only)", xlab="PIT")
plot(-log(fit_dv$cpo$cpo), main="-log(CPO) (dv, sampled only)",
     ylab="-log(CPO)", xlab="Area index")
par(mfrow=c(1,1))

## 4) Residuals vs fitted (sampled only)
sampled_dv <- !is.na(il_tick$dv_scaled)
plot(il_tick$pred_mean_dv[sampled_dv], il_tick$residual_dv[sampled_dv],
     main="Residuals vs Fitted (dv, sampled only)",
     xlab="Fitted (mean)", ylab="Residual"); abline(h=0, col=2)

##Maps for ALL counties
tmap_mode("plot")

map_mean_dv <- tm_shape(il_tick) +
  tm_fill("pred_mean_dv", palette="plasma", style="cont", title="Predicted mean") +
  tm_borders()

map_lo_dv <- tm_shape(il_tick) +
  tm_fill("pred_q025_dv", palette="plasma", style="cont", title="Lower 95%") +
  tm_borders()

map_hi_dv <- tm_shape(il_tick) +
  tm_fill("pred_q975_dv", palette="plasma", style="cont", title="Upper 95%") +
  tm_borders()

map_ci_dv <- tm_shape(il_tick) +
  tm_fill("pred_ci_w_dv", palette="viridis", style="cont", title="DV: 95% CI width") +
  tm_borders()

map_mean_dv
map_lo_dv
map_hi_dv
map_ci_dv


# place legend inside without overlap
place_inside <- function(p) {
  p +
    tm_layout(
      frame = FALSE,                    
      legend.outside = FALSE,
      legend.position = c("left", "bottom"),
      legend.just = c("left", "bottom"),
      legend.bg.color = "white",
      legend.frame = TRUE,
      legend.text.size = 0.6,
      legend.title.size = 0.8,
      inner.margins = c(0.15, 0.02, 0.02, 0.02)
    )
}

#Apply to each
map_mean_dv2 <- place_inside(map_mean_dv) +
  tm_compass(position = c("left", "top"), type = "8star")                 

map_lo_dv2 <- place_inside(map_lo_dv) 
map_hi_dv2 <- place_inside(map_hi_dv) +     
  tm_scale_bar(position = c("right", "bottom"))

# panel
panel_dv <- tmap_arrange(map_mean_dv2, map_lo_dv2, map_hi_dv2, ncol = 3)

panel_dv

# Save in your specified directory
setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")

# save high-resolution
tmap_save(panel_dv,
          filename = "dermacentor_variabilis_panel_legends_inside.tiff",
          width = 14, height = 8, units = "in", dpi = 300)

```


##  BYM2 (Besag-York-Mollié 2) Model  [IS]
```{r, message=FALSE, warning=FALSE}

# Formula 
formula_bym2_is <- is_scaled ~ 
  PC1_clim + PC2_clim + PC1_LC + PC2_LC + DeerSuitability_pct +
  f(re_u, model = "bym2", graph = g, scale.model = TRUE)


# Model
model_bym2_is <- inla(
  formula_bym2_is,
  family = "gaussian",
  data = il_tick,
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
)


# Summary
summary(model_bym2_is)

#Significance
model_bym2_is$summary.fixed
model_bym2_is$summary.hyperpar


#Check for Poor Fit Using CPO (Conditional Predictive Ordinate)

# Failures (should be 0 ideally)
table(model_bym2_is$cpo$failure)

# Use -log(CPO): larger = worse predictive support
plot(-log(model_bym2_is$cpo$cpo),
     main = "-log(CPO) (larger = worse fit)",
     ylab = "-log(CPO)", xlab = "Area index")


# PIT histogram: should be ~Uniform if calibrated
hist(model_bym2_is$cpo$pit, breaks = 20, main = "PIT histogram", xlab = "PIT")


# Fitted & residuals 
fhat <- model_bym2_is$summary.fitted.values$mean         
il_tick$predicted_bym2_is <- fhat                    
il_tick$residuals_bym2_is <- il_tick$is_scaled - fhat 

plot(il_tick$predicted_bym2_is, il_tick$residuals_bym2_is,
     main = "Residuals vs Fitted",
     xlab = "Fitted ", ylab = "Residuals")
abline(h = 0, col = "red")


# Map log-scale residuals 
library(tmap)
tm_shape(il_tick) +
  tm_fill("residuals_bym2_is", palette = "RdBu", style = "cont", title = "Residuals") +
  tm_borders()

#INTERPERTATION
# Gaussian BYM2 model shows good fit (DIC = -169.5, WAIC = -175.3).  
# Deer suitability negatively associated (−0.015, 95% CI −0.027 to −0.002), others weakly related.  
# Residuals spatially balanced, PIT near uniform SO no major bias or spatial autocorrelation.  


```


## Spatial k-Fold Cross-Validation (dv)
```{r}

library(sf)
library(dplyr)
library(blockCV)
library(INLA)

# Ensure BYM2 index
if (!"re_u" %in% names(il_tick)) il_tick$re_u <- seq_len(nrow(il_tick))

# Create spatial folds (5-fold; ~50 km blocks)
set.seed(123)
centroids <- st_point_on_surface(st_geometry(il_tick))
pts <- st_as_sf(data.frame(id = il_tick$re_u), geometry = centroids)
sp_folds <- spatialBlock(speciesData = pts, theRange = 50000, k = 5,
                         selection = "random", iteration = 200,
                         showBlocks = FALSE, verbose = FALSE)
fold_id <- sp_folds$foldID

# BYM2 Poisson formula for IS
formula_bym2_is <- is_scaled ~ 
  PC1_clim + PC2_clim + PC1_LC + PC2_LC + DeerSuitability_pct +
  f(re_u, model = "bym2", graph = g, scale.model = TRUE)

# Spatial k-fold loop (held-out counts = NA)
cv_is <- lapply(1:5, function(i) {
  dat <- il_tick
  dat$is_scaled[fold_id == i] <- NA
  fit <- inla(formula_bym2_is, family = "gaussian", data = dat,
              control.predictor = list(compute = TRUE))
  data.frame(
    fold = i,
    obs  = il_tick$is_scaled[fold_id == i],
    pred = fit$summary.fitted.values$mean[fold_id == i]
  )
})
cv_results_is <- bind_rows(cv_is)

# Metrics
rmse_is <- sqrt(mean((cv_results_is$obs - cv_results_is$pred)^2, na.rm = TRUE))
mae_is  <- mean(abs(cv_results_is$obs - cv_results_is$pred), na.rm = TRUE)
r2_is   <- cor(cv_results_is$obs, cv_results_is$pred, use = "complete.obs")^2
rho_is  <- suppressWarnings(cor(cv_results_is$obs, cv_results_is$pred,
                                use = "complete.obs", method = "spearman"))
cat(sprintf("IS Spatial 5-fold CV — RMSE=%.2f  MAE=%.2f  R²=%.2f  Spearmanρ=%.2f\n",
            rmse_is, mae_is, r2_is, rho_is))


# Interpretation:
# Spatial 5-fold CV : RMSE = 0.48, MAE = 0.38, R² = 0.14, ρ = 0.42 SO modest predictive performance with moderate spatial correlation capture.

```



##Predicting [IS]
```{r}
library(INLA)
library(sf)
library(dplyr)
library(tmap)

## Prep (optional check of unsampled)
dat <- st_drop_geometry(il_tick)
unsampled_idx_is <- which(is.na(dat$is_scaled))
length(unsampled_idx_is)

## Model (IS): BYM2 
formula_bym2_is <- is_scaled ~ 
  PC1_clim + PC2_clim + PC1_LC + PC2_LC + DeerSuitability_pct +
  f(re_u, model = "bym2", graph = g, scale.model = TRUE)

fit_is <- inla(
  formula_bym2_is,
  family = "gaussian",
  data = il_tick,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE)
)


## Attach predictions + 95% CrI to sf
pred_is <- fit_is$summary.fitted.values
il_tick$pred_mean_is <- pred_is$mean
il_tick$pred_q025_is <- pred_is$`0.025quant`
il_tick$pred_q975_is <- pred_is$`0.975quant`
il_tick$pred_ci_w_is <- il_tick$pred_q975_is - il_tick$pred_q025_is
il_tick$residual_is  <- ifelse(is.na(il_tick$is_scaled), NA_real_,
                               il_tick$is_scaled - il_tick$pred_mean_is)

## Diagnostics )
cat("CPO failures (is):\n"); print(table(fit_is$cpo$failure, useNA="ifany"))

par(mfrow=c(1,2))
hist(fit_is$cpo$pit, breaks=20, main="PIT (is, sampled only)", xlab="PIT")
plot(-log(fit_is$cpo$cpo), main="-log(CPO) (is, sampled only)",
     ylab="-log(CPO)", xlab="Area index")
par(mfrow=c(1,1))

## Residuals vs fitted (sampled only)
sampled_is <- !is.na(il_tick$is_scaled)
plot(il_tick$pred_mean_is[sampled_is], il_tick$residual_is[sampled_is],
     main="Residuals vs Fitted (is, sampled only)",
     xlab="Fitted (mean)", ylab="Residual"); abline(h=0, col=2)

## Maps for ALL counties
tmap_mode("plot")

map_mean_is <- tm_shape(il_tick) +
  tm_fill("pred_mean_is", palette="plasma", style="cont", title="Predicted mean") +
  tm_borders()

map_lo_is <- tm_shape(il_tick) +
  tm_fill("pred_q025_is", palette="plasma", style="cont", title="Lower 95%") +
  tm_borders()

map_hi_is <- tm_shape(il_tick) +
  tm_fill("pred_q975_is", palette="plasma", style="cont", title="Upper 95%") +
  tm_borders()

map_ci_is <- tm_shape(il_tick) +
  tm_fill("pred_ci_w_is", palette="viridis", style="cont", title="95% CI width") +
  tm_borders()

map_mean_is
map_lo_is
map_hi_is
map_ci_is



# place legend inside without overlap
place_inside <- function(p) {
  p +
    tm_layout(
      frame = FALSE,                    
      legend.outside = FALSE,
      legend.position = c("left", "bottom"),
      legend.just = c("left", "bottom"),
      legend.bg.color = "white",
      legend.frame = TRUE,
      legend.text.size = 0.6,
      legend.title.size = 0.8,
      inner.margins = c(0.15, 0.02, 0.02, 0.02)
    )
}

#Apply to each
map_mean_is2 <- place_inside(map_mean_is) +
  tm_compass(position = c("left", "top"), type = "8star")                 

map_lo_is2 <- place_inside(map_lo_is) 
map_hi_is2 <- place_inside(map_hi_is) +     
  tm_scale_bar(position = c("right", "bottom"))

# panel
panel_is <- tmap_arrange(map_mean_is2, map_lo_is2, map_hi_is2, ncol = 3)

panel_is

# Save in your specified directory
setwd("C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output")

# save high-resolution
tmap_save(panel_is,
          filename = "ixodes_scapularis_panel_legends_inside.tiff",
          width = 14, height = 8, units = "in", dpi = 300)


```




## Sensetivity analysis
```{r}
## Libraries
library(INLA); library(dplyr); library(ggplot2); library(sf)

## Helper R2 on observed rows
r2_ <- function(y, yhat) {
  obs <- is.finite(y) & is.finite(yhat)
  1 - sum((y[obs]-yhat[obs])^2) / sum((y[obs]-mean(y[obs]))^2)
}

## Helper fit plus R2 for a response and covariates
fit_get_R2 <- function(resp, covs, data_sf, include_spatial = TRUE) {
  dat <- sf::st_drop_geometry(data_sf)
  if (!all(c(resp, covs) %in% names(dat))) {
    stop("Missing variables in data for ", resp)
  }
  f_rhs <- paste(covs, collapse = " + ")
  if (include_spatial) {
    f_rhs <- paste0(f_rhs, " + f(re_u, model='bym2', graph=g, scale.model=TRUE)")
  }
  fml <- as.formula(paste(resp, "~", f_rhs))
  
  fit <- INLA::inla(
    fml, family = "gaussian", data = dat,
    control.predictor = list(compute = TRUE),
    control.compute   = list(waic = TRUE, dic = TRUE, cpo = TRUE)
  )
  y    <- dat[[resp]]
  yhat <- fit$summary.fitted.values$mean
  list(R2 = r2_(y, yhat), fit = fit)
}

## Species specific covariate sets
covs_aa <- c("PC1_clim","PC2_clim","PC1_LC")
covs_dv <- c("PC1_clim","PC2_clim","PC1_LC","PC2_LC","DeerSuitability_pct")
covs_is <- c("PC1_clim","PC2_clim","PC1_LC","PC2_LC","DeerSuitability_pct")

## Build contribution table for one species by drop one
contrib_table <- function(resp, covs, sp_label) {
  full <- fit_get_R2(resp, covs, il_tick, include_spatial = TRUE)
  R2_full <- full$R2
  
  drop_covs <- lapply(covs, function(v) {
    r2_drop <- fit_get_R2(resp, setdiff(covs, v), il_tick, include_spatial = TRUE)$R2
    data.frame(
      variable = v,
      R2_drop = r2_drop,
      contrib_pct = pmax(0, 100 * (R2_full - r2_drop) / max(R2_full, .Machine$double.eps))
    )
  }) %>% bind_rows()
  
  r2_no_sp <- fit_get_R2(resp, covs, il_tick, include_spatial = FALSE)$R2
  drop_sp  <- data.frame(
    variable = "Spatial (BYM2)",
    R2_drop = r2_no_sp,
    contrib_pct = pmax(0, 100 * (R2_full - r2_no_sp) / max(R2_full, .Machine$double.eps))
  )
  
  bind_rows(drop_covs, drop_sp) %>%
    mutate(species = sp_label) %>%
    arrange(desc(contrib_pct))
}

## Run for three Illinois species
tab_aa <- contrib_table("aa_scaled", covs_aa, "A. americanum")
tab_dv <- contrib_table("dv_scaled", covs_dv, "D. variabilis")
tab_is <- contrib_table("is_scaled", covs_is, "I. scapularis")

contrib_all <- bind_rows(tab_aa, tab_dv, tab_is)

## Clean labels
contrib_all$variable <- dplyr::recode(
  contrib_all$variable,
  PC1_clim = "PC1 (Climate)",
  PC2_clim = "PC2 (Climate)",
  PC1_LC   = "PC1 (Land cover)",
  PC2_LC   = "PC2 (Land cover)",
  DeerSuitability_pct = "Deer suitability",
  "Spatial (BYM2)" = "Spatial (BYM2)"
)

## Factor order
lvl_order <- c("PC1 (Climate)","PC2 (Climate)","PC1 (Land cover)","PC2 (Land cover)","Deer suitability","Spatial (BYM2)")
contrib_all <- contrib_all %>%
  mutate(variable = factor(variable, levels = lvl_order))

## Plot
pd <- position_dodge(0.8)
sensetivity_plot <- ggplot(contrib_all, aes(x = variable, y = contrib_pct, fill = species)) +
  geom_col(position = pd, width = 0.7, color = "black") +
  scale_fill_manual(values = c("#12436D", "#28A197", "#801650")) +
  labs(x = NULL, y = "% contribution to R²",
       title = "Sensitivity by covariate and spatial effect drop one R²") +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(sensetivity_plot)

## Save
out_dir <- "C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output"
if (dir.exists(out_dir)) {
  ggsave(file.path(out_dir, "Sensitivity_drop_one_Illinois_combined.png"),
         sensetivity_plot, width = 12, height = 10, dpi = 300, bg = "white")
}


```


## Global Sensetivity analysis

```{r}
## Libraries
library(INLA); library(dplyr); library(ggplot2); library(sf)

## Map your fitted models
model_aa <- model_bym2_aa
model_dv <- model_bym2_dv
model_is <- model_bym2_is

## Input ranges from Illinois data for uniform variance estimates
il_df <- st_drop_geometry(il_tick)
rng_ <- function(v) range(il_df[[v]], na.rm = TRUE)

rngs <- list(
  PC1_clim = rng_("PC1_clim"),
  PC2_clim = rng_("PC2_clim"),
  PC1_LC   = rng_("PC1_LC"),
  PC2_LC   = rng_("PC2_LC"),
  DeerSuitability_pct = rng_("DeerSuitability_pct")
)

## Uniform variance from observed range
uvar <- function(a, b) ((b - a)^2) / 12

## Get spatial sd from BYM2 component
get_sp_sd <- function(fit) {
  cand <- names(fit$summary.random)
  if (length(cand) == 0L) return(0.25)
  nm <- if ("re_u" %in% cand) "re_u" else cand[1]
  sdv <- suppressWarnings(mean(fit$summary.random[[nm]]$sd, na.rm = TRUE))
  if (!is.finite(sdv) || sdv <= 1e-9) sdv <- 0.25
  sdv
}

## Build a Sobol table analytically for one species
sobol_analytic <- function(fit, covs, pretty_labels = NULL) {
  get_beta <- function(term) if (term %in% rownames(fit$summary.fixed)) fit$summary.fixed[term, "mean"] else 0
  
  betas <- c(
    PC1_clim = get_beta("PC1_clim"),
    PC2_clim = get_beta("PC2_clim"),
    PC1_LC   = get_beta("PC1_LC"),
    PC2_LC   = get_beta("PC2_LC"),
    Deer     = get_beta("DeerSuitability_pct")
  )
  
  vars <- c(
    PC1_clim = uvar(rngs$PC1_clim[1], rngs$PC1_clim[2]),
    PC2_clim = uvar(rngs$PC2_clim[1], rngs$PC2_clim[2]),
    PC1_LC   = uvar(rngs$PC1_LC[1],   rngs$PC1_LC[2]),
    PC2_LC   = uvar(rngs$PC2_LC[1],   rngs$PC2_LC[2]),
    Deer     = uvar(rngs$DeerSuitability_pct[1], rngs$DeerSuitability_pct[2])
  )
  
  sd_sp <- get_sp_sd(fit)
  var_sp <- sd_sp^2
  
  use_keys <- ifelse(covs == "DeerSuitability_pct", "Deer", covs)
  
  num_terms <- betas[use_keys]^2 * vars[use_keys]
  total_var <- sum(num_terms, na.rm = TRUE) + var_sp
  
  S_covs <- pmax(0, 100 * num_terms / total_var)
  S_sp   <- pmax(0, 100 * var_sp / total_var)
  
  pretty_map <- c(
    "PC1_clim" = "PC1 (Climate)",
    "PC2_clim" = "PC2 (Climate)",
    "PC1_LC"   = "PC1 (Land cover)",
    "PC2_LC"   = "PC2 (Land cover)",
    "Deer"     = "Deer suitability"
  )
  
  tibble::tibble(
    variable    = c(unname(pretty_map[use_keys]), "Spatial (BYM2)"),
    contrib_pct = c(as.numeric(S_covs),           as.numeric(S_sp))
  ) %>%
    mutate(variable = factor(variable, levels = c("PC1 (Climate)","PC2 (Climate)","PC1 (Land cover)","PC2 (Land cover)","Deer suitability","Spatial (BYM2)"))) %>%
    arrange(desc(contrib_pct))
}

## Species covariate sets and labels
tab_aa <- sobol_analytic(model_aa, covs_aa) %>% dplyr::mutate(species = "A. americanum")
tab_dv <- sobol_analytic(model_dv, covs_dv) %>% dplyr::mutate(species = "D. variabilis")
tab_is <- sobol_analytic(model_is, covs_is) %>% dplyr::mutate(species = "I. scapularis")


## Compute analytic Sobol tables
tab_aa <- sobol_analytic(model_aa, covs_aa, pretty_labels = NULL) %>% mutate(species = "A. americanum")
tab_dv <- sobol_analytic(model_dv, covs_dv, pretty_labels = NULL) %>% mutate(species = "D. variabilis")
tab_is <- sobol_analytic(model_is, covs_is, pretty_labels = NULL) %>% mutate(species = "I. scapularis")

contrib_all <- dplyr::bind_rows(tab_aa, tab_dv, tab_is)

## Plot
pd <- position_dodge(0.45)
sensetivity_plot <- ggplot(contrib_all, aes(x = variable, y = contrib_pct, fill = species)) +
  geom_col(position = pd, width = 0.35, color = "black") +
  scale_fill_manual(values = c("#12436D", "#28A197", "#801650")) +
  scale_fill_manual(
    values = c("A. americanum" = "#12436D",
               "D. variabilis" = "#28A197",
               "I. scapularis" = "#801650"),
    breaks = c("A. americanum","D. variabilis","I. scapularis"),
    labels = c(expression(italic(A.~americanum)),
               expression(italic(D.~variabilis)),
               expression(italic(I.~scapularis)))
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(x = NULL, y = bquote("% Contribution to " ~ R^2), fill = NULL) +
  theme_classic(base_size = 13) +
  theme(
    panel.border      = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid        = element_blank(),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    axis.text.x       = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.text.y       = element_text(colour = "black"),
    legend.position   = c(0.03, 0.97),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    legend.key.width  = unit(14, "pt"),
    legend.key.height = unit(10, "pt")
  )


print(sensetivity_plot)

## Optional export of the table
out_dir <- "C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
readr::write_csv(contrib_all, file.path(out_dir, "Sobol_analytic_linear_predictor_Illinois.csv"))
ggsave(file.path(out_dir, "Sobol_analytic_linear_predictor_Illinois.png"),
       sensetivity_plot, width = 12, height = 10, dpi = 300, bg = "white")


```

## Global Sensetivity Analysis with uncertainity

```{r}
## Bootstrap CIs for analytic Sobol indices on linear predictor

library(dplyr)
library(tibble)
set.seed(2025)

## pick empirical or uniform variance for inputs
## "empirical" uses bootstrapped sample variance from il_df
## "uniform" uses range^2 / 12 from your rngs list
input_var_mode <- "empirical"  # change to "uniform" if you prefer

## helper to grab beta mean and sd safely
get_beta_stats <- function(fit, term) {
  if (term %in% rownames(fit$summary.fixed)) {
    c(mean = fit$summary.fixed[term, "mean"], sd = fit$summary.fixed[term, "sd"])
  } else c(mean = 0, sd = 0)
}

## bootstrap function for one species
sobol_analytic_boot <- function(fit, covs, B = 1500) {
  use_keys <- ifelse(covs == "DeerSuitability_pct", "Deer", covs)
  
  get_beta_stats <- function(fit, term) {
    if (term %in% rownames(fit$summary.fixed)) {
      c(mean = fit$summary.fixed[term, "mean"], sd = fit$summary.fixed[term, "sd"])
    } else c(mean = 0, sd = 0)
  }
  
  bst <- list(
    PC1_clim = get_beta_stats(fit, "PC1_clim"),
    PC2_clim = get_beta_stats(fit, "PC2_clim"),
    PC1_LC   = get_beta_stats(fit, "PC1_LC"),
    PC2_LC   = get_beta_stats(fit, "PC2_LC"),
    Deer     = get_beta_stats(fit, "DeerSuitability_pct")
  )
  
  # safe BYM2 SD source
  if (is.null(fit$summary.random) || length(fit$summary.random) == 0L) {
    # fall back to overall spatial SD via helper, replicate to a vector
    sd_by_area <- rep(get_sp_sd(fit), max(5L, nrow(il_df)))
  } else {
    re_name <- if ("re_u" %in% names(fit$summary.random)) "re_u" else names(fit$summary.random)[1]
    sd_by_area <- fit$summary.random[[re_name]]$sd
    sd_by_area <- sd_by_area[is.finite(sd_by_area)]
    if (length(sd_by_area) == 0L) {
      sd_by_area <- rep(get_sp_sd(fit), max(5L, nrow(il_df)))
    }
  }
  
  out <- matrix(NA_real_, nrow = B, ncol = length(use_keys) + 1)
  colnames(out) <- c(unname(c(use_keys)), "BYM2")
  
  n_rows <- nrow(il_df)
  
  for (b in seq_len(B)) {
    beta_b <- c(
      PC1_clim = rnorm(1, bst$PC1_clim["mean"], bst$PC1_clim["sd"]),
      PC2_clim = rnorm(1, bst$PC2_clim["mean"], bst$PC2_clim["sd"]),
      PC1_LC   = rnorm(1, bst$PC1_LC["mean"],   bst$PC1_LC["sd"]),
      PC2_LC   = rnorm(1, bst$PC2_LC["mean"],   bst$PC2_LC["sd"]),
      Deer     = rnorm(1, bst$Deer["mean"],     bst$Deer["sd"])
    )
    
    rows_b <- sample.int(n_rows, n_rows, replace = TRUE)
    v_PC1  <- var(il_df$PC1_clim[rows_b], na.rm = TRUE)
    v_PC2  <- var(il_df$PC2_clim[rows_b], na.rm = TRUE)
    v_LC1  <- var(il_df$PC1_LC[rows_b],   na.rm = TRUE)
    v_LC2  <- var(il_df$PC2_LC[rows_b],   na.rm = TRUE)
    v_Deer <- var(il_df$DeerSuitability_pct[rows_b], na.rm = TRUE)
    
    var_x <- c(PC1_clim = v_PC1, PC2_clim = v_PC2, PC1_LC = v_LC1, PC2_LC = v_LC2, Deer = v_Deer)
    var_x[!is.finite(var_x)] <- 0
    
    if (length(sd_by_area) >= 5) {
      sd_sp_b <- mean(sample(sd_by_area, length(sd_by_area), replace = TRUE), na.rm = TRUE)
    } else {
      sd_sp_b <- mean(sd_by_area, na.rm = TRUE)
    }
    if (!is.finite(sd_sp_b)) sd_sp_b <- get_sp_sd(fit)
    var_sp_b <- sd_sp_b^2
    
    num_terms <- beta_b[use_keys]^2 * var_x[use_keys]
    tot_var   <- sum(num_terms, na.rm = TRUE) + var_sp_b
    
    out[b, c(unname(use_keys), "BYM2")] <- 100 * c(num_terms, BYM2 = var_sp_b) / tot_var
  }
  
  smry <- apply(out, 2, function(v) c(mean = mean(v, na.rm = TRUE),
                                      lo = quantile(v, 0.025, na.rm = TRUE),
                                      hi = quantile(v, 0.975, na.rm = TRUE)))
  smry <- as.data.frame(t(smry))
  smry$variable <- rownames(smry)
  
  pretty_map <- c(
    "PC1_clim" = "PC1 (Climate)",
    "PC2_clim" = "PC2 (Climate)",
    "PC1_LC"   = "PC1 (Land cover)",
    "PC2_LC"   = "PC2 (Land cover)",
    "Deer"     = "Deer suitability",
    "BYM2"     = "Spatial (BYM2)"
  )
  
  tibble::tibble(
    variable    = unname(pretty_map[smry$variable]),
    contrib_pct = smry$mean,
    lo          = smry$lo,
    hi          = smry$hi
  ) %>%
    dplyr::mutate(variable = factor(variable,
                                    levels = c("PC1 (Climate)","PC2 (Climate)","PC1 (Land cover)","PC2 (Land cover)","Deer suitability","Spatial (BYM2)"))) %>%
    dplyr::arrange(dplyr::desc(contrib_pct))
}

## run for species
tab_aa_ci <- sobol_analytic_boot(model_aa, covs_aa, B = 1500) %>% dplyr::mutate(species = "A. americanum")
tab_dv_ci <- sobol_analytic_boot(model_dv, covs_dv, B = 1500) %>% dplyr::mutate(species = "D. variabilis")
tab_is_ci <- sobol_analytic_boot(model_is, covs_is, B = 1500) %>% dplyr::mutate(species = "I. scapularis")


contrib_all_ci <- bind_rows(tab_aa_ci, tab_dv_ci, tab_is_ci)

## plot with error bars
pd <- position_dodge(width = 0.45)
sensetivity_plot_ci <- ggplot(contrib_all_ci, aes(x = variable, y = contrib_pct, fill = species)) +
  geom_col(position = pd, width = 0.35, color = "black") +
  geom_errorbar(
    aes(ymin = lo, ymax = hi),
    position = pd,
    width = 0.05,
    linewidth = 0.4,
    color = "black",
    alpha = 0.6
  ) +
  scale_fill_manual(
    values = c("A. americanum" = "#12436D",
               "D. variabilis" = "#28A197",
               "I. scapularis" = "#801650"),
    breaks = c("A. americanum","D. variabilis","I. scapularis"),
    labels = c(expression(italic(A.~americanum)),
               expression(italic(D.~variabilis)),
               expression(italic(I.~scapularis)))
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(x = NULL, y = bquote("% Contribution to " ~ R^2), fill = NULL) +
  theme_classic(base_size = 13) +
  theme(
    panel.border      = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid        = element_blank(),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    axis.text.x       = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.text.y       = element_text(colour = "black"),
    legend.position   = c(0.03, 0.97),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    legend.key.width  = unit(14, "pt"),
    legend.key.height = unit(10, "pt")
  )

print(sensetivity_plot_ci)


## export table and figure
out_dir <- "C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
readr::write_csv(contrib_all_ci, file.path(out_dir, "Sobol_analytic_bootstrap_CI_Illinois.csv"))
ggsave(file.path(out_dir, "Sobol_analytic_bootstrap_CI_Illinois.png"),
       sensetivity_plot_ci, width = 12, height = 10, dpi = 300, bg = "white")

```

## Exporting the data in CSV along with predictions
```{r, message=FALSE, warning=FALSE}
## Exporting the data in CSV along with predictions (count-scale)
library(dplyr)
library(sf)

df <- il_tick

## pick county name column robustly
county_col <- c("COUNTY_NAM","COUNTY_NAME","County","county","NAME")
county_col <- county_col[county_col %in% names(df)]
if (length(county_col) == 0L) stop("No county name column found. Add one of: COUNTY_NAM, COUNTY_NAME, County, county, NAME.")
county_col <- county_col[1]

## recompute CI widths if needed
df <- df %>%
  mutate(
    pred_ci_w    = ifelse(is.finite(pred_q975)    & is.finite(pred_q025),    pred_q975    - pred_q025,    NA_real_),
    pred_ci_w_dv = ifelse(is.finite(pred_q975_dv) & is.finite(pred_q025_dv), pred_q975_dv - pred_q025_dv, NA_real_),
    pred_ci_w_is = ifelse(is.finite(pred_q975_is) & is.finite(pred_q025_is), pred_q975_is - pred_q025_is, NA_real_)
  )

## species tables without geometry
aa_df <- st_drop_geometry(df) %>%
  transmute(
    !!county_col := .data[[county_col]],
    PM_aa  = pred_mean,
    LL_aa  = pred_q025,
    UL_aa  = pred_q975,
    CIw_aa = pred_ci_w,
    OBS_aa = aa_scaled
  )

dv_df <- st_drop_geometry(df) %>%
  transmute(
    !!county_col := .data[[county_col]],
    PM_dv  = pred_mean_dv,
    LL_dv  = pred_q025_dv,
    UL_dv  = pred_q975_dv,
    CIw_dv = pred_ci_w_dv,
    OBS_dv = dv_scaled
  )

is_df <- st_drop_geometry(df) %>%
  transmute(
    !!county_col := .data[[county_col]],
    PM_is  = pred_mean_is,
    LL_is  = pred_q025_is,
    UL_is  = pred_q975_is,
    CIw_is = pred_ci_w_is,
    OBS_is = is_scaled
  )

## combined wide export
combined_df <- aa_df %>%
  left_join(dv_df,  by = county_col) %>%
  left_join(is_df,  by = county_col) %>%
  relocate(all_of(county_col), .before = everything())

## write files
out_dir <- "C:/Users/abrar/OneDrive - University of Illinois - Urbana/Documents/Abrar Hussain/Ph.D/Ph.D. Papers/As Authors/Bayesian Spatial Modeling (Illinois)/Output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(aa_df,       file.path(out_dir, "predictions_amblyomma_americanum_counts.csv"), row.names = FALSE)
write.csv(dv_df,       file.path(out_dir, "predictions_dermacentor_variabilis_counts.csv"), row.names = FALSE)
write.csv(is_df,       file.path(out_dir, "predictions_ixodes_scapularis_counts.csv"),     row.names = FALSE)
write.csv(combined_df, file.path(out_dir, "predictions_all_species_combined_counts.csv"),  row.names = FALSE)

```


##---------------------
##---------------------
##---------------------
