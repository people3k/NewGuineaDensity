###Central New Guinea Model Code, Data Production, and Model fitting Code 
##2026. Code associated with ``Technology and climate impacted population density in contact-era New Guinea"
##By Freeman and Roscoe. Submitted to JAS September 2026

##Set working directory.
##load packages
library(ggplot2)
library(cowplot)
library(broom)
library(nlme)
library(dplyr)
library(tidyr)
library(scales)
library(effects)
library(splines)
library(raster)
library(sf)
library(spdep)
library(viridis)
library(elevatr)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(xtable)

# Plotting Theme
# ------------------------------------------------------------
my_theme <- theme_bw() +
  theme(
    axis.text.x  = element_text(size=20, colour="black"),
    axis.text.y  = element_text(size=20, colour="black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    plot.title   = element_text(size=18),
    legend.title = element_text(size=14),
    legend.text  = element_text(size=12),
    strip.text.x = element_text(size = 18, face = "bold"),
    strip.text.y = element_text(size = 18, face = "bold"),
    
    # optional: facet background cleaner
    strip.background = element_rect(fill = "grey90", colour = "black"))

##--------------------------------------
#Part I. Model Code to replicate Figure 2 in the main text, and Figure S1
#in the Supporting Information. 
##-------------------------------------------------------------

##Non-trival equilibrium solution for equation 1 in the main text of the
##manuscript:
## ------------------------------------------------------------
## 1. Define the equilibrium function
## ------------------------------------------------------------
# p_bar_j = T_p * A_j  +  T_p * B * C^alpha * exp(-C)
#            = T_p * ( A_j + B * C^alpha * exp(-C) )
#T_p=r/S_j.

p_bar <- function(C, Tp, A, alpha, B) {
  Tp * A + Tp * B * ((C^alpha) * exp(-C))
}

## ------------------------------------------------------------
## 2. Fixed (baseline) parameters. Note: We assume that r=0.25. Hence, 
#varying T_p assumes that S changes relative to the fixed quanitity of r.
## ------------------------------------------------------------
alpha_fixed <- 5       # shape parameter (given)
A_fixed     <- 1       # baseline A_j when varying T_p
Tp_fixed    <- 1       # baseline T_p when varying A_j
B_fixed     <- 1       # baseline B (multiplicative term on hump)

##Sequence of climate values
C_seq   <- seq(0, 12, length.out = 400)
C_max   <- max(C_seq)

## The C^alpha * exp(-C) term is unimodal, peaking at C = alpha.
## The "exponential decay portion" of every curve is therefore
## C >= alpha_fixed, regardless of A, Tp, or B (those only shift
## or rescale the curve; they don't move the location of the peak).
decay_xmin <- alpha_fixed
decay_xmax <- C_max

## ------------------------------------------------------------
## Reusable theme addition: put legend inside plot, top-right,
## with a fully transparent legend background/key
## ------------------------------------------------------------
inset_legend_theme <- theme(
  legend.position      = c(0.98, 0.98),
  legend.justification = c("right", "top"),
  legend.background    = element_rect(fill = "transparent", color = NA),
  legend.key           = element_rect(fill = "transparent", color = NA),
  legend.box.background = element_rect(fill = "transparent", color = NA)
)

## Reusable shaded band marking the decay region, drawn first so that the decay curves drawn on top of the shading.
decay_shade <- annotate(
  "rect",
  xmin = decay_xmin, xmax = decay_xmax,
  ymin = -Inf, ymax = Inf,
  fill = "grey40", alpha = 0.15
)

## -----------------------------------------------------------------
## 3. Panel 1: Effect of A_j (holding T_p, alpha, B, and r constant)
## -----------------------------------------------------------------
A_values <- c(0, 2, 4)

df_A <- expand_grid(C = C_seq, A = A_values) %>%
  mutate(
    p_bar = p_bar(C, Tp = Tp_fixed, A = A, alpha = alpha_fixed, B = B_fixed),
    A_lab = factor(A, levels = A_values,
                   labels = paste0("A[j] == ", A_values))
  )

plot_A <- ggplot(df_A, aes(x = C, y = p_bar, color = A_lab, group = A_lab)) +
  decay_shade +
  geom_line(linewidth = 1.1) +
  scale_color_viridis_d(
    name = expression(A[j]),
    labels = scales::parse_format()
  ) +
  labs(
    title = expression("A. Effect of " * A[j] * " on Equilibrium Population"),
    subtitle = bquote(T[p]==.(Tp_fixed)*","~ alpha==.(alpha_fixed)*","~ B==.(B_fixed)),
    x = "Climate controlled productivity (C)",
    y = expression("Equilibrium population " * bar(p)[j]) 
  ) +
  my_theme+
  #theme_bw() +
  #theme(plot.title = element_text(face = "bold")) +
  inset_legend_theme

plot_A
## ------------------------------------------------------------
## 4. Panel 2: Effect of T_p (holding A_j, alpha, B, and r constant)
## ------------------------------------------------------------
Tp_values <- c(.25, 1, 1.25)

df_Tp <- expand_grid(C = C_seq, Tp = Tp_values) %>%
  mutate(
    p_bar = p_bar(C, Tp = Tp, A = A_fixed, alpha = alpha_fixed, B = B_fixed),
    Tp_lab = factor(Tp, levels = Tp_values,
                    labels = paste0("T[p] == ", Tp_values))
  )

plot_Tp <- ggplot(df_Tp, aes(x = C, y = p_bar, color = Tp_lab, group = Tp_lab)) +
  decay_shade +
  geom_line(linewidth = 1.1) +
  scale_color_viridis_d(
    name = expression(T[p]),
    labels = scales::parse_format(),
    option = "magma", end = 0.85
  ) +
  labs(
    title = expression("B. Effect of " * T[p] * " on Equilibrium Population"),
    subtitle = bquote(A[j]==.(A_fixed)*","~ alpha==.(alpha_fixed)*","~ B==.(B_fixed)),
    x = "Climate controlled productivity (C)",
    y = expression("Equilibrium population " * bar(p)[j]) 
  ) +
  my_theme+
  #theme_bw() +
  #theme(plot.title = element_text(face = "bold")) +
  inset_legend_theme

plot_Tp

## Combine the panels with cowplot

final_plot <- plot_grid(
  plot_A, plot_Tp,
  ncol = 2,
  align = "v",
  label_size = 14
)
final_plot

###Save the output
ggsave( filename = "Figures/Figure2.pdf", plot = final_plot, width = 12, height = 10)

##-----------------------------------------------------------
## 5. Graphic example of the derivative and the impact of varying B
## on the maximum population density
##------------------------------------------------------------

##Set alpha to a given value
alpha <- 5
##set the range of C
C <- seq(0, 12, length.out = 800)

##Create a data frame with multiple values of C and the gamma-like function
##proposed in the main text to describe the effect of C on maximum population density.
##In the supporting information, we call this function f_j: B*f_j=B*C^alpha*e^-C. 
##Also, the data frame includes the derivative of the gamma-like function.

##Data frame
df <- data.frame(
  C     = rep(C, 4),
  B     = factor(rep(c(1, 2, 1, 2), each = length(C))),
  type  = factor(rep(c("f", "f", "df/dC", "df/dC"), each = length(C)),
                 levels = c("f", "df/dC")),
  value = c(1 * C^alpha * exp(-C),
            2 * C^alpha * exp(-C),
            1 * C^(alpha - 1) * exp(-C) * (alpha - C),
            2 * C^(alpha - 1) * exp(-C) * (alpha - C))
)

##Scale the derivative axis for visual ease. Change as needed
k <- 2.5

##Plot the gamma function and the derivative for the alpha value and B 
##values set above. Figure S1 in the Supporting Information.

fj<-ggplot(subset(df, type == "f"), aes(C, value, colour = B)) +
  geom_line(linewidth = 0.9) +
  geom_line(data = subset(df, type == "df/dC"),
            aes(y = value * k), linetype = "dashed", linewidth = 0.9) +
  scale_y_continuous(sec.axis = sec_axis(~ . / k, name = "d(Bf)/dC")) +
  labs(x = expression(C[j]), y = expression(B*f[j]),
       title = expression(f[j] == C[j]^5 * e^-C[j])) +
  geom_vline(xintercept=5, color="blue")+
  my_theme
fj

###Save the plot
ggsave(
  filename = "Figures/SIFigureBscale.pdf",
  plot = fj,
  width = 10, height = 8
)

##Part II: Mapping and Extracting Climate Data for each New Guinea Polity======================
#$This code re-produces Figure 1 in the main text and the data necessary to conduct the
##regression analyses reported in the main text and for constructing Figures in Part II of the code.
#------------------------------------------------------------------
##1.-----Set working directory and Load Ethnographic Data

Roscoe2025<-read.csv(file="Roscoe2025.csv", header=T)


##2. Create Map of New Guinea and Extract raster data

# Define a bounding box for New Guinea
new_guinea_bbox <- st_bbox(c(xmin = 130, xmax = 150, ymin = -11, ymax = 0), crs = st_crs(4326))

# Get world countries data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Crop the world data to the New Guinea bounding box
world_crop <- st_crop(world, new_guinea_bbox)

# Select the New Guinea polygon from the cropped world data
new_guinea_polygon <- world_crop %>%
  filter(name_en == "Papua New Guinea" | name_en == "Indonesia") %>%
  st_union() %>% # Union the parts of the island (Papua New Guinea and Indonesia)
  st_cast("POLYGON") %>% # Ensure it's a single polygon
  st_sf()

# Get elevation raster data for the New Guinea polygon
elev_data <- get_elev_raster(locations = new_guinea_polygon, z = 7, clip = "location")

# Convert to terra raster for masking and better performance
elev_terra <- rast(elev_data)

# Mask the elevation raster with the New Guinea polygon to clip it to land only
elev_terra_clipped <- mask(elev_terra, new_guinea_polygon)

# Convert the clipped raster to a data frame for ggplot2
elev_df <- as.data.frame(elev_terra_clipped, xy = TRUE, na.rm = TRUE)
colnames(elev_df)[3] <- "elevation"

##3Corrdinates of New Guinea Polities
lon=Roscoe2025$LONG
lat=Roscoe2025$LAT

###Extract elevation to points
coords1 <- data.frame(lon, lat)

# Convert to spatial points
points1 <- vect(coords1, crs = "EPSG:4326")

# Extract elevation values
elev_values <- terra::extract(elev_terra, points1)

# Combine with original coordinates
result <- cbind(coords1, elevation = elev_values[,2])  # column 1 is ID, column 2 is elevation
print(result)

Elevation<-result$elevation

RoscoeElv<-cbind(Elevation,Roscoe2025)

##-SKIP 2 IF YOU ALREADY HAVE CLIMATE RASTERS---------------------------------------------------------------
##2.--Extract bioclimate variables. To access the climate rasters used in the paper, go to 
#WorldClim.org and download the 30 arch second bioclimatic variables 
#and place the unzipped rasters in the Climate directory.
##One can also use the code below to extract the necessary files.
##The files are very large for 30 arc seconds tifs.
##--------------------------------------------------------
###Automatically download and prepare WorldClim bioclim rasters---
##This will take a while..........
climate_dir <- "Climate"

# --- Bioclim variables (bundled together in one zip) ---
bio_zip_path <- file.path(climate_dir, "wc2.1_30s_bio.zip")
bio_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_bio.zip"

needed_vars <- c(1, 6, 12)  # only the variables this script uses
needed_bio_names <- paste0("wc2.1_30s_bio_", needed_vars, ".tif")
required_bio_files <- file.path(climate_dir, needed_bio_names)

## --- Elevation (its own separate zip, single layer) -
##Will need this to construct Figure S1 at the end of Part III below.

elev_zip_path <- file.path(climate_dir, "wc2.1_30s_elev.zip")
elev_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_elev.zip"
elev_name <- "wc2.1_30s_elev.tif"
required_elev_file <- file.path(climate_dir, elev_name)

# Create the Climate directory if it doesn't exist
if (!dir.exists(climate_dir)) {
  dir.create(climate_dir, recursive = TRUE)
}

# --- Download/extract bioclim variables ---
if (!all(file.exists(required_bio_files))) {
  
  if (!file.exists(bio_zip_path)) {
    message("Downloading WorldClim 2.1 30s bioclim archive (all 19 variables bundled, ~9-10 GB)...")
    options(timeout = max(3600, getOption("timeout")))
    download.file(bio_url, destfile = bio_zip_path, mode = "wb")
  }
  
  message("Extracting needed bioclim rasters (", paste(needed_bio_names, collapse = ", "), ") ...")
  unzip(bio_zip_path, files = needed_bio_names, exdir = climate_dir)
  
  file.remove(bio_zip_path)
  
} else {
  message("Needed bioclim rasters already present in ", climate_dir, " — skipping download.")
}

# --- Download/extract elevation ---
if (!file.exists(required_elev_file)) {
  
  message("Downloading WorldClim 2.1 30s elevation raster...")
  options(timeout = max(3600, getOption("timeout")))
  download.file(elev_url, destfile = elev_zip_path, mode = "wb")
  
  message("Extracting elevation raster...")
  unzip(elev_zip_path, files = elev_name, exdir = climate_dir)
  
  file.remove(elev_zip_path)
  
} else {
  message("Elevation raster already present in ", climate_dir, " — skipping download.")
}

##3.----Extract bioclimate variables----------------
##------------------------------------------------------------------
###Extract Mean Annual Temperature
bioclim <- rast("Climate/wc2.1_30s_bio_1.tif")  # load Mean annual temp. layer

lon=Roscoe2025$LONG
lat=Roscoe2025$LAT

###Attempt to extract bio 1
coords <- data.frame(lon, lat)

# Convert to spatial points
points <- vect(coords, crs = "EPSG:4326")

# Extract values

bio_values <- terra::extract(bioclim, points)

final_result <- cbind(points, bio_values[,-1])  # remove ID column

MAT<-bio_values$wc2.1_30s_bio_1
# View result
print(final_result)
RoscoeTemp<-cbind(MAT, RoscoeElv)

###Extract rainfall data bio 12
bioclim2 <- rast("Climate/wc2.1_30s_bio_12.tif")  # load rainfall layer

# Convert to spatial points
points <- vect(coords, crs = "EPSG:4326")

# Extract values

bio_values2 <- terra::extract(bioclim2, points)
CRR<-bio_values2$wc2.1_30s_bio_12

RoscoePrecip<-cbind(CRR, RoscoeTemp)

##Extract Minimum temperature of the coldest month
bioclim6 <- rast("Climate/wc2.1_30s_bio_6.tif")  # load one layer

coords <- data.frame(lon, lat)

# Convert to spatial points
points <- vect(coords, crs = "EPSG:4326")

# Extract values

bio_values2 <- terra::extract(bioclim6, points)
MinTemp<-bio_values2$wc2.1_30s_bio_6
Roscoetempmin<-cbind(MinTemp, RoscoePrecip)

# save the dataframe to a CSV file
write.csv(Roscoetempmin, "Roscoe2025Climate.csv", row.names = FALSE)

#Load Roscoe original data with climate variables appended=============
Roscoe2025c<-read.csv(file="Roscoe2025SIData.csv", header=T)


# Plot the clipped elevation data

#3a.---Create Figure 1
NGelv <- ggplot() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = new_guinea_polygon, fill = NA, color = "white", size = 0.4) + # Island borders
  scale_fill_viridis_c(
    na.value = "transparent",
    direction = -1,
    limits = c(0, max(elev_df$elevation, na.rm = TRUE))  # Start Viridis scale at 0
  ) +
  coord_sf(xlim = c(130, 150), ylim = c(-11, 0), expand = FALSE) +
  labs(
    title = "Elevation and Contact-Era Polities in New Guinea",
    x = "Longitude",
    y = "Latitude",
    fill = "Elevation (m)"
  ) +
  geom_point(
    data = Roscoe2025c,
    aes(LONG, LAT, color = factor(SUB2), shape = factor(SUB2)),
    inherit.aes = FALSE,
    #alpha = 0.5,
    size = 3
  ) +
  scale_color_manual(values = c("black","gray","red","blue"), name = "Food production") +
  scale_shape_manual(values = c(17, 17,18,18), name = "Food production") +
  # geom_text(data = Roscoe2025, aes(x = LONG, y = LAT, label = rownames(Roscoe2025)),
  #   inherit.aes = FALSE, size = 2.5, vjust = -1)+
  theme_minimal()
NGelv

###Export Figure 1 to your directory
pdf("Figures/NGmap.pdf", width=12.55, height=10)
NGelv
dev.off()


#====================================================
###Part III: Data Imputation. We impute missing data using the missForest package. Note:
#We only impute missing information on polity size and polity territory, and the ADP variable.
#The data set has complete information on population density and subsistence data.
#=========================================================================================

###read in data file with climate and elevation=====================================
Roscoe2025c<-read.csv(file="Roscoe2025SIData.csv", header=T)
# Remove unnecessary columns before imputation
Roscoe2025c$SOCIETY <- NULL
Roscoe2025c$SUB <- NULL
Roscoe2025c$SUB2 <- NULL
Roscoe2025c$PROTEIN<-NULL

##Rename dataframe
Roscoe2025c_original <- Roscoe2025c

###Impute missing values of Polity Size, Territory Size, ADP, 
#rainfall, temperature, and elevation.

#missForest
#install.packages("missForest")
library(missForest)

##ConvertADP2 to factor variable. Not necessary in this analysis.
#Roscoe2025c <- Roscoe2025c %>%
 # mutate(ADP = as.factor(ADP))

str(Roscoe2025c)

#impute missing values, using all parameters as default values
iris.imp <- missForest(Roscoe2025c, ntree = 10000, variablewise = TRUE)

#check imputation error
iris.imp$OOBerror
iris.imp$ximp

##Produce plots that check the relationship between observed and imputed
##variables
plot(Roscoe2025c$DENSITY~iris.imp$ximp$DENSITY)
plot(Roscoe2025c$CRR~iris.imp$ximp$CRR)
plot(Roscoe2025c$PLTYSIZE~iris.imp$ximp$PLTYSIZE)


#Replace only missing values in the original data set
Roscoe2025c_filled <- Roscoe2025c_original  # Start with the data set that has NAs
for (col in names(Roscoe2025c_filled)) {
  missing_idx <- which(is.na(Roscoe2025c_filled[[col]]))
  if (length(missing_idx) > 0) {
    Roscoe2025c_filled[[col]][missing_idx] <- iris.imp$ximp[[col]][missing_idx]
  }
}

# Add back the previously removed columns
#Reload Roscoe2025Climate and Link the imputed data with the deleted columns
Roscoe2025c<-read.csv(file="Roscoe2025SIData.csv", header=T)

Roscoe2025c_filled$SOCIETY <- Roscoe2025c$SOCIETY
Roscoe2025c_filled$SUB <- Roscoe2025c$SUB
Roscoe2025c_filled$SUB2 <- Roscoe2025c$SUB2
Roscoe2025c_filled$PROTEIN <- Roscoe2025c$PROTEIN

sum(is.na(Roscoe2025c_filled))  # Should be 0 if all NAs are filled

# Check the result
summary(Roscoe2025c_filled)

###Write table
write.table(Roscoe2025c_filled, file = "RoscoeImpF2025Replication.csv", sep = ",", row.names = FALSE)


##Check the sampling of polities relative to the the distribution
#of NPP and Elevation in New Guinea. 
##Build Figure S1 in the Supporting Information

#Imputed data
Roscoe2025imp<-read.csv(file="RoscoeImpFinal2025.csv", header=T)

####Build a map as above 
new_guinea_bbox <- st_bbox(c(xmin = 130, xmax = 150, ymin = -11, ymax = 0), crs = st_crs(4326))
world <- ne_countries(scale = "medium", returnclass = "sf")
world_crop <- st_crop(world, new_guinea_bbox)

#Bound to NG
new_guinea_polygon <- world_crop %>%
  filter(name_en == "Papua New Guinea" | name_en == "Indonesia") %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  st_sf()

# --- load rasters ---
bioclim  <- rast("Climate/wc2.1_30s_bio_1.tif")   # temp
bioclim2 <- rast("Climate/wc2.1_30s_bio_12.tif")  # rainfall
elevation<-rast("Climate/wc2.1_30s_elev.tif")  # Elevation
# stack and name
clim_stack <- c(bioclim, bioclim2,elevation)
names(clim_stack) <- c("MAT", "CRR", "Elevation")

# convert sf polygon to SpatVector for terra
ng_vect <- vect(new_guinea_polygon)

# crop to bounding box, then mask to the actual polygon shape
clim_crop <- crop(clim_stack, ng_vect)
clim_mask <- mask(clim_crop, ng_vect)

# convert to data frame for ggplot
clim_df <- as.data.frame(clim_mask, na.rm = TRUE)

# Calculate Miami Model NPP for each 30 arc second raster.
clim_df$MATNPP <- 3000 / (1 + exp(1.315 - 0.119 * clim_df$MAT))
clim_df$CRRNPP<- 3000 * (1 - exp(-0.000664 * clim_df$CRR))
clim_df$NPP <- pmin(clim_df$MATNPP, clim_df$CRRNPP, na.rm = TRUE)


# scatterplot Figure S1 in the Supporting Information
clim_map<-ggplot(clim_df, aes(x = Elevation, y = NPP)) +
  geom_point(alpha = 0.3, size = 0.5, color = "steelblue") +
  geom_point(data=Roscoe2025imp, aes(color=factor(SUB2)),size=2)+
  theme_bw()+
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28), plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Elevation (msl)",
       y = "Net primary productivity (g/m/year)",
       title = "Elevation vs Modeled NPP — New Guinea") 
clim_map

#Save Figure S1
pdf("Figures/Sampling.pdf", width=12.55, height=10)
clim_map
dev.off()


##------------------------------------------------------------------------
##Part IV: Analysis of the data to replicate the graphs and model fitting presented
##in the results section of the main manuscript.
##--------------------------------------------------------------------------------

##read in data files with climate and elevation-----------------------------
#Imputed data
Roscoe2025imp<-read.csv(file="RoscoeImpFinal2025.csv", header=T)

# Calculate Miami Model NPP on imputed data
Roscoe2025imp$MATNPP <- 3000 / (1 + exp(1.315 - 0.119 * Roscoe2025imp$MAT))
Roscoe2025imp$CRRNPP<- 3000 * (1 - exp(-0.000664 * Roscoe2025imp$CRR))
Roscoe2025imp$NPP <- pmin(Roscoe2025imp$MATNPP, Roscoe2025imp$CRRNPP, na.rm = TRUE)

##Exploratory graph of NPP (estimate of C) against 
##population density (p_bar) for the imputed data

den1 <- ggplot(Roscoe2025imp, aes((NPP), (DENSITY)))+
  theme_bw() +
  geom_point(aes(color=factor(SUB2)), size=4)+ 
  #scale_x_continuous(limit=c(-1,5))+
  #scale_color_gradient(low ="#F8766D", high = "#619CFF", breaks=c(2,4,6)) +
  scale_color_viridis_d(option = "D")+
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28), plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Net Primary Productivity", y="ln Population density", title = "")
  #facet_wrap(~PROTEIN)
#geom_vline(xintercept = 2.99)
den1

##Set-up data.Revised the data frame above to match the variables
#described in the main text of the paper (i.e., relabled DENSITY>>p_bar)
#Fitting:   pbar_j = beta_1j  +  beta_2 * exp(-k * C_j)  +  error (equation 4 of the main text)
#   pbar    = population density (ratio scale)-- outcome
#   beta_1j = T_p * A_j -- a FREE parameter per rank level j. No independent data to estimate Tp,
#   so beta_1j is estimated directly as a single combined quantity per level of food production,
#             rather than trying to recover T_p and A_j individually.
#   Cj       = net primary productivity (continuous) of each habitat   
#   beta_2  = T_p * B --A combined quantity (no independent data on
#             B or T_p).
#   k       = decay rate on C; required because raw exp(-C) goes to
#             0 at the scale of these data: C (~1500-3000 g/m2/yr)


##load data-------------------------------------------------
dat <- read.csv("RoscoeRev2026.csv", stringsAsFactors = FALSE)
#dat<-subset(dat, SUB2_Aj %in% c("HORT", "SAGO", "AGG"))

##numeric description of the A_j variable. Assume that AGG polities have a greater investment
##in food production than HORT polities and HORT polities greater than SAGO
rank_levels <- sort(unique(dat$A_j))
dat$rank_idx <- match(dat$A_j, rank_levels)
dat$A_j_f    <- factor(dat$A_j, levels = rank_levels)   # for plotting/legend

#   - Gamma family assumes Var(pbar) ~ mu^2, i.e. SD scales with the mean --
#     this matches the data: the coefficient of variation of pbar is roughly
#     constant across A_j groups (~0.7-1.2) despite means ranging from ~1 to
#     ~40.

## ---- 1. Profile over k (Gamma GLM, identity link) ------------------------

##Function to find hat k decay constant. x= exp(-k * dat$Cj)
##from equation 4
rss_for_k_gamma <- function(k) {
  x <- exp(-k * dat$Cj)
  fit <-
    suppressWarnings(
      glm(pbar ~ factor(A_j)-1 + x, data = dat, family = Gamma(link = "identity"),### -1 estimates parameters for beta1j and does not compare groups.
          start = c(rep(mean(dat$pbar), length(rank_levels)), 1),
          control = glm.control(maxit = 200)
    ),
  )
  if (is.null(fit) || !fit$converged) return(NA_real_)
  deviance(fit)
}

###Create a grid of plausible values of k
k_grid   <- 10^seq(-6, -2, length.out = 150)
#Run the grid of values through the rss_for_k_gamma
dev_grid <- sapply(k_grid, rss_for_k_gamma)
k0       <- k_grid[which.min(dev_grid)]
#Find the fit model with the minimum residual deviance()
opt      <- optimize(rss_for_k_gamma, interval = c(k0 / 3, k0 * 3))
##k value for the best fit model
k_hat    <- opt$minimum

##Call the values above
opt
k0
k_hat

##Visualize k_hat. Create a data frame of the vector of k values and
##residual deviances from the 150 fit GLMs above
profile_df <- data.frame(k = k_grid, deviance = dev_grid)

##Deviance profile over k [diagnostic plot A] 
##Figure S3 A in the Supporting Information
p_profile <- ggplot(profile_df, aes(x = k, y = deviance)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = k_hat, color = "red", linetype = "dashed") +
  annotate("text", x = k_hat, y = max(dev_grid, na.rm = TRUE),
           label = "hat(k)", parse = TRUE, color = "red",
           hjust = -0.2, vjust = 1, size = 6) +
  labs(title = "Profile of deviance over k",
       x = "Scaling coefficient, k", y = "Gamma deviance")+
  my_theme
p_profile

## ---- 2. Final fit at k_hat -------------------------------------------
x_final <- exp(-k_hat * dat$Cj)
fit <- glm(pbar ~ factor(A_j)-1 + x_final, data = dat,
           family = Gamma(link = "identity"),
           start = c(rep(mean(dat$pbar), length(rank_levels)), 1),
           control = glm.control(maxit = 200))
print(summary(fit))

# beta1_hat: one free coefficient per rank level j (beta_1j = T_p*A_j, combined)
# beta2_hat: the single shared coefficient on exp(-k*C) (beta_2 = T_p*B, combined)
# These are the POINT ESTIMATES used everywhere below -- in particular, they
# (together with k_hat) are what draws the fitted curves in Figure 3 of the main text
beta1_hat <- setNames(coef(fit)[paste0("factor(A_j)", rank_levels)], rank_levels)
beta2_hat <- unname(coef(fit)["x_final"])

##Call fitted values and check for negative fitted population density values
cat("All fitted values positive",
    !any(fitted(fit) <= 0), "\n\n")

## ---- Overall model-fit summary --------------------------------------
phi <- summary(fit)$dispersion
##Fit a ``null" model with intercept only.
null_fit <- glm(pbar ~ 1, data = dat, family = Gamma(link = "identity"))
##Calculate a psudo R2 as 1 less the ratio of residual deviance/null residual deviance
pseudo_r2 <- 1 - deviance(fit) / deviance(null_fit)
pseudo_r2

##Residual degrees of freedom
fit$df.residual

##Fit and complexity
AIC(fit)
BIC(fit)


##Tidy coefficient table (GLM estimate/SE/p + bootstrap CIs) ----
##Save for use below. We will append mean, median, and quntiles of
##estimated parameters via bootstrapping.
coef_table <- tidy(fit)
coef_table$term <- gsub("factor\\(A_j\\)", "beta1_", coef_table$term)
coef_table$term <- gsub("^x_final$", "beta2", coef_table$term)
lab_lookup <- c("beta1_1" = "beta1_SAGO", "beta1_1.5" = "beta1_SAGO-FISH",
                "beta1_2" = "beta1_HORT", "beta1_3" = "beta1_AGG")
coef_table$term <- ifelse(coef_table$term %in% names(lab_lookup),
                          lab_lookup[coef_table$term], coef_table$term)

# Append k as its own row. k has no native GLM standard error (it's chosen
# by profiling, not estimated as a GLM coefficient) -- std.error/statistic/
# p.value are left NA here; the bootstrap below quantifies the uncertainty
# of the estimate for k.
coef_table <- rbind(
  coef_table,
  data.frame(term = "k", estimate = k_hat, std.error = NA_real_,
             statistic = NA_real_, p.value = NA_real_)
)
coef_table

##Figure 3 in the main text. Plot Fitted curves by beta_1j groups over the
##raw data. 

##Build a table of fitted curves, for plotting ------------------
# Plot A draws one decay curve per A_j group using the point estimates from
# the final GLM fit above:
#   - beta1_hat[j]  (one intercept-like level per rank j, from section 2)
#   - beta2_hat     (single shared coefficient on exp(-k*C), from section 2)
#   - k_hat         (single shared decay rate, from section 1's profiling)
# i.e. each curve is  fitted = beta1_hat[j] + beta2_hat * exp(-k_hat * C)
# evaluated over a fine grid of C. These are the same point estimates
# reported in coefficient_table.csv ("estimate" column)

##Sequence of values of Cj
Cseq <- seq(min(dat$Cj), max(dat$Cj), length.out = 200)

##Data frame for the fittted curves
curve_df <- do.call(rbind, lapply(rank_levels, function(lev) {
  data.frame(
    Cj    = Cseq,
    fitted = beta1_hat[as.character(lev)] + beta2_hat * exp(-k_hat * Cseq),
    A_j_f = factor(lev, levels = rank_levels)
  )
}))

# Nicer labels for the legend, matching the subsistence categories
lab_map <- c("1" = "SAGO (1)", "1.5" = "SAGO-FISH (1.5)", "2" = "HORT (2)", "3" = "AGG (3)")
dat$A_j_lab      <- factor(lab_map[as.character(dat$A_j)], levels = lab_map[as.character(rank_levels)])
curve_df$A_j_lab <- factor(lab_map[as.character(curve_df$A_j_f)], levels = lab_map[as.character(rank_levels)])

#Plot Figure 3: Raw data + fitted decay curves by A_j group
# Curves use beta1_hat[j], beta2_hat, k_hat as documented above;


##optional caption:
#fit_caption <- sprintf(
 # "Fitted with beta_1j = [%s], beta_2 = %.0f, k = %.5g",
  #paste(names(beta1_hat), "=", round(beta1_hat, 2), collapse = ", "),
  #beta2_hat, k_hat
#)

##Figure 3 plot
p_fit <- ggplot() +
  geom_point(data = dat, aes(x = Cj, y = pbar, color = A_j_lab),
             alpha = 0.65, size = 2) +
  geom_line(data = curve_df, aes(x = Cj, y = fitted, color = A_j_lab),
            linewidth = 1.1) +
  scale_color_viridis_d(name = "Food\nproduction (A_j)") +
  labs(
    #title = "Population density vs. net primary productivity",
    subtitle = expression(paste("Fitted: ", bar(p)[j], " = ", beta["1j"],
                                " + ", beta[2], " ", e^{-kC})),
   # caption = fit_caption,
    x = "Net primary productivity (Cj)",
    y = expression(paste("Population density (", bar(p), ")"))
  ) +
  my_theme
p_fit

##Save figure 3 as a pdf
ggsave("Figures/Figure3.pdf", p_fit, width = 8, height = 6.5)


###Base plots for Figure S3 in the Supporting Information
##Diagnostic plots for the fit GLM with gamma distribution

# (Plot b) Residuals vs fitted [diagnostic plot B]
resid_df <- data.frame(fitted = fitted(fit),
                       pearson_resid = residuals(fit, type = "pearson"),
                       Pro_lab = dat$PROTEIN)
p_resid <- ggplot(resid_df, aes(x = fitted, y = pearson_resid, color = Pro_lab)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_color_viridis_d() +
  labs(title = "Pearson residuals vs. fitted values",
       x = "Fitted value", y = "Pearson residual")+
  my_theme
p_resid

# (Plot c) Observed vs fitted [diagnostic plot C]
obsfit_df <- data.frame(observed = dat$pbar, fitted = fitted(fit), Pro_lab = dat$PROTEIN)
p_obsfit <- ggplot(obsfit_df, aes(x = fitted, y = observed, color = Pro_lab)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "grey40", linetype = "dashed") +
  scale_color_viridis_d() +
  labs(title = "Observed vs. fitted", x = "Fitted", y = "Observed")+
  my_theme
p_obsfit

# diagnostics panel ##Figure S3 A, B and C
diagnostics_grid <- plot_grid(
  p_profile, p_resid, p_obsfit,
  ncol = 3, labels = c("A", "B", "C")
)
diagnostics_grid

##Save the model diagnostics plot. Figure S3
save_plot("Figures/model_diagnostics.pdf", diagnostics_grid,
          base_width = 16, base_height = 5.5)


##------------------------------------------------------
##Decay curve only. Estimate of population density based on fitting C
##alone. Calculations used in the results section to compare the E. 
##Mianmin and Enga
##--------------------------------------------------------------
decay_df <- data.frame(Polity=dat$Polity_habitat_j,
                       pbar= dat$pbar,
                       Cj      = dat$Cj,
                       decay   = beta2_hat * exp(-k_hat * dat$Cj),
                       diff= dat$pbar - (beta2_hat * exp(-k_hat * dat$Cj)),
                       A_j_lab = dat$A_j_lab
)

###Write the file for decay only density
write.csv(decay_df, "decayOnlyDen.csv")

####Plot the fit decline of population density as a function of C alone
p_decay <- ggplot(decay_df, aes(x = Cj, y = decay)) +
  geom_line(aes(), linewidth=1.1, color="darkblue") +
  #geom_smooth(se = FALSE, linewidth = 1.1) +
  geom_point(data=dat, aes(x = Cj, y = pbar, color = A_j_lab))+
  scale_color_viridis_d(name = "Food\nProduction (A_j)") +
  labs(
    title = expression(paste("Decay term ", hat(beta)[2], " ", e^{-hat(k)*C}, " vs. C")),
    x = "Net primary productivity (Cj)",
    y = expression(paste(hat(beta)[2], " ", e^{-hat(k)*C}))
  ) +
  my_theme
p_decay

##-----------------------------------------------------------------
##3. Bootstrapping
##------------------------------------------------------------------
##Case-re-sampling bootstrap for k, beta_1j, beta_2 -----------------
# Refits the GLM profile over k, then GLM at k_hat) on
# bootstrap re-samples of the rows. The idea is to propagate uncertainty in k into the
# beta_1j/beta_2 estimates rather than treating k_hat as fixed/known
# (which is what the single-fit GLM SE assume).
set.seed(20260901)
n_boot <- 5000
boot_results <- matrix(NA_real_, nrow = n_boot, ncol = length(rank_levels) + 2,
                       dimnames = list(NULL, c(paste0("beta1_", rank_levels), "beta2", "k")))

fit_once <- function(dat_b) {
  rss_k <- function(k) {
    x <- exp(-k * dat_b$Cj)
    f <-
      suppressWarnings(glm(pbar ~ factor(A_j)-1 + x, data = dat_b,
                           family = Gamma(link = "identity"),
                           start = c(rep(mean(dat_b$pbar), length(rank_levels)), 1),
                           control = glm.control(maxit = 200)),
    )
    if (is.null(f) || !f$converged) return(1e10)
    deviance(f)
  }
  # coarse grid over the SAME wide range used for the fit above, not a
  # narrow window around k_hat -- avoids optimize() drifting into the
  # degenerate high-k region where the design becomes nearly collinear
  # and beta_2 blows up to compensate.
  grid_k    <- 10^seq(-6, -2, length.out = 35)
  grid_dev  <- sapply(grid_k, rss_k)
  k0        <- grid_k[which.min(grid_dev)]
  lo <- grid_k[max(1, which.min(grid_dev) - 2)]
  hi <- grid_k[min(length(grid_k), which.min(grid_dev) + 2)]
  g <- tryCatch(suppressWarnings(optimize(rss_k, interval = c(lo, hi))), error = function(e) NULL)
  if (is.null(g)) return(rep(NA_real_, length(rank_levels) + 2))
  kb <- g$minimum
  xb <- exp(-kb * dat_b$Cj)
  fb <- tryCatch(
    suppressWarnings(glm(pbar ~ factor(A_j)-1 + xb, data = dat_b,
                         family = Gamma(link = "identity"),
                         start = c(rep(mean(dat_b$pbar), length(rank_levels)), 1),
                         control = glm.control(maxit = 200))),
    error = function(e) NULL
  )
  if (is.null(fb) || !fb$converged) return(rep(NA_real_, length(rank_levels) + 2))
  out <- c(coef(fb)[paste0("factor(A_j)", rank_levels)], coef(fb)["xb"], k = kb)
  # plausibility filter: a resample that lands near the edge of the k grid,
  # or produces a beta_2 orders of magnitude outside the master estimate, is
  # a numerical artifact (near-collinear design), not real sampling variability
  if (kb <= grid_k[2] || kb >= grid_k[length(grid_k) - 1]) return(rep(NA_real_, length(rank_levels) + 2))
  if (!is.finite(out["xb"]) || abs(out["xb"]) > beta2_hat * 100 || abs(out["xb"]) < beta2_hat / 100) {
    return(rep(NA_real_, length(rank_levels) + 2))
  }
  out
}

for (b in 1:n_boot) {
  idx <- sample(seq_len(nrow(dat)), nrow(dat), replace = TRUE)
  dat_b <- dat[idx, ]
  # require at least 2 obs per rank level in the resample, else skip (rare)
  if (any(table(factor(dat_b$A_j, levels = rank_levels)) < 2)) next
  res <- tryCatch(fit_once(dat_b), error = function(e) rep(NA_real_, length(rank_levels) + 2))
  if (length(res) == length(rank_levels) + 2) boot_results[b, ] <- res
}

##Number of bootstrapp resamples that meet the above criteria
n_ok <- sum(complete.cases(boot_results))
n_ok

##Identify the mean, median, and quantile of the parameter estimate
##distributions from the sample
boot_ci <- apply(boot_results, 2, function(col) {
  col <- col[!is.na(col)]
  c(estimate = mean(col), boot_median = median(col),
    ci_lo = quantile(col, 0.05), ci_hi = quantile(col, 0.95))
})

boot_ci <- as.data.frame(t(boot_ci))
boot_ci$term <- rownames(boot_ci)
rownames(boot_ci) <- NULL
boot_ci <- boot_ci[, c("term", "estimate", "boot_median", "ci_lo.5%", "ci_hi.95%")]
names(boot_ci) <- c("term", "boot_mean", "boot_median", "boot_ci_lo", "boot_ci_hi")


#Print the table of mean, median, and quantile of the parameter estimate
##distributions from the sample
print(boot_ci, row.names = FALSE)


##Write the table to a csv file.
write.csv(boot_ci, "bootstrap_ci.csv", row.names = FALSE)


## ----. Diagnostic: bootstrapped k vs. beta2 ----------------------------
# Checks whether beta2's right tail corresponds to k draws sitting near the
# low edge of the profiling grid used inside fit_once() -- if so, those are
# likely near-degenerate fits slipping past the plausibility filter, not
# genuine sampling variability, and the filter (or the k grid) may need
# tightening.
boot_k_grid <- 10^seq(-6, -2, length.out = 35)  # same grid as in fit_once()
k_edge_lo   <- boot_k_grid[2]                    # filter excludes kb <= this
k_edge_hi   <- boot_k_grid[length(boot_k_grid) - 1]  # filter excludes kb >= this
k_near_lo   <- boot_k_grid[3]                    # one grid step inside the low edge
k_near_hi   <- boot_k_grid[length(boot_k_grid) - 2]  # one grid step inside the high edge

k_vs_beta2_df <- data.frame(
  k     = boot_results[, "k"],
  beta2 = boot_results[, "beta2"]
)
k_vs_beta2_df <- k_vs_beta2_df[complete.cases(k_vs_beta2_df), ]

n_near_edge <- sum(k_vs_beta2_df$k <= k_near_lo | k_vs_beta2_df$k >= k_near_hi)
cat(sprintf("Surviving k bootstrap draws within one grid step of the plausibility filter edge (k <= %.3g or k >= %.3g): %d / %d\n\n",
            k_near_lo, k_near_hi, n_near_edge, nrow(k_vs_beta2_df)))


# Diagnostic: bootstrapped k vs. beta2, to check whether beta2's right
# tail traces back to k draws sitting near the plausibility-filter edge
p_boot_k_vs_beta2 <- ggplot(k_vs_beta2_df, aes(x = k, y = beta2)) +
  geom_point(alpha = 0.4, size = 1.6, color = "grey30") +
  geom_vline(xintercept = c(k_near_lo, k_near_hi), color = "red",
             linetype = "dashed", linewidth = 0.6) +
  geom_hline(yintercept = beta2_hat, color = "steelblue",
             linetype = "dotted", linewidth = 0.7) +
  annotate("text", x = k_near_lo, y = max(k_vs_beta2_df$beta2, na.rm = TRUE),
           label = "near filter\nedge (low k)", color = "red", size = 3.5,
           hjust = -0.05, vjust = 1) +
  labs(
    title = "Bootstrapped k vs. beta2",
    subtitle = "Dashed red lines: one grid step inside the plausibility-filter edge. Dotted blue: point-estimate beta2_hat.",
    x = "Bootstrapped k", y = "Bootstrapped beta2"
  ) +
  my_theme
p_boot_k_vs_beta2


# Attach bootstrap confidence envelope to every row of coef_table (beta_1j, beta_2, k all refit within
# each re-sample, so this reflects the full estimation procedure's
# uncertainty -- not just a single fixed-k GLM's uncertainty)
boot_term_map <- c(
  "beta1_1" = "beta1_SAGO", "beta1_1.5" = "beta1_SAGO-FISH",
  "beta1_2" = "beta1_HORT", "beta1_3" = "beta1_AGG",
  "beta2" = "beta2", "k" = "k"
)
boot_lookup <- boot_ci
boot_lookup$term <- boot_term_map[boot_lookup$term]

# long-format bootstrap draws, for the coefficient histograms in section 4
boot_long <- do.call(rbind, lapply(colnames(boot_results), function(cn) {
  data.frame(term = boot_term_map[cn], value = boot_results[, cn], stringsAsFactors = FALSE)
}))
boot_long <- boot_long[!is.na(boot_long$value), ]
term_order <- c("beta1_SAGO", "beta1_SAGO-FISH", "beta1_HORT", "beta1_AGG", "beta2", "k")
boot_long$term <- factor(boot_long$term, levels = term_order)

# bootstrap mean per term, for a reference line on each histogram panel
boot_ci_lab <- boot_lookup
boot_ci_lab$term <- factor(boot_ci_lab$term, levels = term_order)

coef_table <- merge(coef_table, boot_lookup[, c("term","boot_mean", "boot_median", "boot_ci_lo", "boot_ci_hi")],
                    by = "term", all.x = TRUE, sort = FALSE)

# reorder rows: beta_1j (rank order), beta_2, k
row_order <- c("beta1_SAGO", "beta1_SAGO-FISH", "beta1_HORT", "beta1_AGG", "beta2", "k")
coef_table <- coef_table[match(row_order, coef_table$term), ]

num_cols <- c("estimate", "std.error", "statistic", "p.value",
              "boot_mean","boot_median", "boot_ci_lo", "boot_ci_hi")
for (col in num_cols) coef_table[[col]] <- signif(coef_table[[col]], 8)

##Coefficient Ttble with coefficients from the main GLM model fit and the bootstsrapped 
##GLM fit with bootstrapped k, etc.
print(coef_table, row.names = FALSE)

##Write coefficient table that includes booststrapped confidence envelopes
write.csv(coef_table, "coefficient_table.csv", row.names = FALSE)


#Figure S4 in the Supporting Information
##Histograms of the bootstrapped coefficient estimates
p_boot_hist <- ggplot(boot_long, aes(x = value)) +
  geom_histogram(bins = 40, fill = "grey75", color = "black", linewidth = 0.2) +
  geom_vline(data = boot_ci_lab, aes(xintercept = boot_mean),
             color = "red", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~term, scales = "free", ncol = 3) +
  labs(title = "Bootstrap distributions of estimated coefficients",
       subtitle = sprintf("Dashed line = bootstrap mean; %d case-resamples", n_boot),
       x = "Bootstrapped estimate", y = "Count") +
  my_theme +
  theme(strip.text = element_text(size = 14))
p_boot_hist


## ---Build Figure S6 in the Supporting Information
##Full joint bootstrap envelope, per A_j group ---------------------
#This uses every bootstrap sample's full
# parameter draw (beta1_j, beta2, k together) to build a pointwise 95%
# envelope specific to each A_j group -- i.e. it reflects each group's own
# beta1_j uncertainty as well as the shared beta2/k uncertainty.
boot_ok   <- boot_results[complete.cases(boot_results), , drop = FALSE]
k_b       <- boot_ok[, "k"]
beta2_b   <- boot_ok[, "beta2"]
decay_mat <- exp(-outer(k_b, Cseq))   # n_ok x length(Cseq); decay_mat[i,c] = exp(-k_b[i]*Cseq[c])


##Envelope data frame for Figure S6
envelope_df2 <- do.call(rbind, lapply(rank_levels, function(lev) {
  beta1_b    <- boot_ok[, paste0("beta1_", lev)]
  fitted_mat <- beta1_b + beta2_b * decay_mat   # recycled down columns -> n_ok x length(Cseq)
  data.frame(
    Cj     = Cseq,
    fitted = apply(fitted_mat, 2, mean, na.rm = TRUE),
    lo     = apply(fitted_mat, 2, quantile, probs = 0.05, na.rm = TRUE),
    hi     = apply(fitted_mat, 2, quantile, probs = 0.95, na.rm = TRUE),
    A_j_f  = factor(lev, levels = rank_levels)
  )
}))
envelope_df2$A_j_lab <- factor(lab_map[as.character(envelope_df2$A_j_f)],
                               levels = lab_map[as.character(rank_levels)])


# Plot Figure S6: Fitted curves paneled by A_j group, with each group's own envelope from
# the full joint bootstrap distribution of (beta1_j, beta2, k)
p_boot_envelope_group <- ggplot() +
  geom_point(data = dat, aes(x = Cj, y = pbar, color = A_j_lab),
             alpha = 0.55, size = 1.8) +
  geom_ribbon(data = envelope_df2, aes(x = Cj, ymin = lo, ymax = hi, fill = A_j_lab),
              alpha = 0.3, color = NA) +
  geom_line(data = envelope_df2, aes(x = Cj, y = fitted, color = A_j_lab),
            linewidth = 1.1) +
  scale_color_viridis_d(guide = "none") +
  scale_fill_viridis_d(guide = "none") +
  facet_wrap(~A_j_lab, scales = "free_y") +
  labs(
    title = "Fitted decay curves with group-specific bootstrap envelopes",
    subtitle = "Pointwise 95% envelope from the joint bootstrap distribution of beta1_j, beta2, k",
    x = "Net primary productivity (Cj)",
    y = expression(paste("Population density (", bar(p), ")"))
  ) +
  my_theme
p_boot_envelope_group

# Figure S6: fitted curves + group-specific joint-bootstrap envelope, paneled by A_j
ggsave("Figures/fit_envelope_by_group.pdf", p_boot_envelope_group, width = 10, height = 7)

##Part V. Spatial Autocorrelation and Model Fitting with a Spatial Parameter--
##-----------------------------------------------------------------
# A: residual deviance of the main GLM (fit, from section 1/2, at k_hat),
#     tested for spatial autocorrelation (Moran's I) at several spatial
#     scales, and mapped.
# B: a GAMM that adds a 2-D spatial basis function (thin-plate spline on
#     LONG/LAT) to the main GLM's structure. Compared against A's GLM on
#     paired maps (common color scale), a fit/parameter comparison, GAM
#     diagnostics, and its own Moran's I correlogram.
# ============================================================================


library(mgcv)   # gam() with spatial smooth s(LONG, LAT)
library(ape)    # Moran.I() for spatial autocorrelation of residual deviance
library(sp)     # great-circle distances (spDists), point.in.polygon
library(maps)   # map_data("world") basemap for ggplot


##New Guinea base map
world_map <- map_data("world")
map_xlim  <- range(dat$LONG) + c(-5, 5)
map_ylim  <- range(dat$LAT)  + c(-5, 5)

## Residual deviance of the main GLM (Part IV, sections 1 and 2 fit) -----------
# `fit` is the same Gamma GLM (identity link, at k_hat)
# from section 2. Its deviance residuals are what section 6a analyzes.
dat$fitted_glm    <- fitted(fit)
dat$resid_dev_glm <- residuals(fit, type = "deviance")

## ---- Shared spatial-weights helpers (used for both GLM and GAMM) ---------
# Great-circle distances (km) between all pairs of polity locations.
coords_mat <- as.matrix(dat[, c("LONG", "LAT")])
dist_mat   <- spDists(coords_mat, longlat = TRUE)
max_dist   <- max(dist_mat)

# ape::Moran.I() takes a plain weight matrix rather than a neighbor-list
# object, so each spatial scale below is expressed as its own binary
# contiguity matrix built directly from the great-circle distance matrix.
band_weights <- function(dmat, d1, d2) {
  W <- (dmat >= d1 & dmat <= d2)
  diag(W) <- FALSE
  storage.mode(W) <- "double"
  W
}



# Distance-band correlogram: non-overlapping bands spanning the full range
# of inter-polity distances. Moran's I tested at several spatial scales. We
#are most interested in scales < 501 KM. At these scales, polities can display
#clustering due to common social norms or physical circumstances.
n_bins <- 6
breaks <- seq(0, max_dist, length.out = n_bins + 1)

moran_correlogram <- function(resid_vec) {
  do.call(rbind, lapply(seq_len(n_bins), function(i) {
    d1 <- breaks[i]; d2 <- breaks[i + 1]
    W  <- band_weights(dist_mat, d1, d2)
    n_links <- sum(W)
    if (n_links == 0) {
      return(data.frame(dist_lo = d1, dist_hi = d2, dist_mid = (d1 + d2) / 2,
                        n_links = 0, morans_I = NA_real_, expected_I = NA_real_,
                        z_stat = NA_real_, p_value = NA_real_))
    }
    mt <- tryCatch(ape::Moran.I(resid_vec, W), error = function(e) NULL)
    if (is.null(mt)) {
      return(data.frame(dist_lo = d1, dist_hi = d2, dist_mid = (d1 + d2) / 2,
                        n_links = n_links, morans_I = NA_real_,
                        expected_I = NA_real_, z_stat = NA_real_, p_value = NA_real_))
    }
    data.frame(
      dist_lo = d1, dist_hi = d2, dist_mid = (d1 + d2) / 2,
      n_links    = n_links,
      morans_I   = mt$observed,
      expected_I = mt$expected,
      z_stat     = (mt$observed - mt$expected) / mt$sd,
      p_value    = mt$p.value
    )
  }))
}


plot_moran_correlogram <- function(moran_df, subtitle) {
  ggplot(moran_df, aes(x = dist_mid, y = morans_I)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_line(color = "steelblue", linewidth = 1, na.rm = TRUE) +
    geom_point(aes(color = p_value < 0.05), size = 3, na.rm = TRUE) +
    scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "grey30"),
                       name = "p < 0.05", na.translate = FALSE) +
    labs(title = "Moran's I correlogram of residual deviance",
         subtitle = subtitle,
         x = "Distance (km, band midpoint)", y = "Moran's I") +
    my_theme
}


##Moran's I for the GLM residual deviance, at several scales ----
moran_by_scale_glm <- moran_correlogram(dat$resid_dev_glm)

## Moran's I correlogram: residual deviance (main GLM) by distance band (km)
print(moran_by_scale_glm, row.names = FALSE)

#write.csv(moran_by_scale_glm, "moran_by_scale_glm.csv", row.names = FALSE)


#Figure S7 A
sp_moran_glm <- plot_moran_correlogram(
  moran_by_scale_glm,
  "Main GLM (fit, section 1/2); non-overlapping distance bands"
)
sp_moran_glm ##Moran's I values by distance band

#ggsave("Figures/moran_correlogram_glm.pdf", sp_moran_glm, width = 8, height = 6)

## Figure S8 A: Map of residual deviance (main GLM) ----------------------------
p_map_glm_only <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey80", linewidth = 0.2) +
  geom_point(data = dat, aes(x = LONG, y = LAT, color = resid_dev_glm),
             size = 3, alpha = 0.85) +
  scale_color_viridis_c(name = "Deviance\nresidual") +
  coord_quickmap(xlim = map_xlim, ylim = map_ylim) +
  labs(title = "Spatial distribution of residual deviance",
       subtitle = "Main GLM (fit, section 1/2)",
       x = "Longitude", y = "Latitude") +
  my_theme
p_map_glm_only
#$ggsave("Figures/residual_deviance_map_GLM.pdf", p_map_glm_only, width = 9, height = 6.5)


## B: GAMM: add a spatial basis function to the main GLM structure ---
# Same parametric structure as the main GLM (factor(A_j)-1 + x_final, x_final
# = exp(-k_hat*Cj) at k_hat from PART IV sections 1 and 2), plus a 2-D thin-plate spline
# smooth on (LONG, LAT) to absorb residual spatial structure.
#
# Fit via mgcv::gam(..., method="REML"): a penalized thin-plate spline
# selected by REML is mathematically equivalent to giving the spatial term a
# random-effect / Gaussian-process prior.


##Data frame of the non-linear term e^(-khatC_j)
dat$x_final <- x_final   # add to the data frame so gam() can find it

##Regression model
gamm_used      <- "GAMM (gam, REML)"
gamm_link_used <- "identity"
gam_fit <- gam(pbar ~ factor(A_j) - 1 + x_final +s(LONG, LAT, bs = "tp", k = 20),
               data = dat, family = Gamma(link = "identity"), method = "REML") 

##Summary of the fitted model
print(summary(gam_fit))

##Fitted values and residual deviance
dat$fitted_gamm    <- fitted(gam_fit)
dat$resid_dev_gamm <- residuals(gam_fit, type = "deviance")

##Compare fit: main GLM vs. GLM + spatial smooth -----------------
fit_comparison <- data.frame(
  Model       = c("GLM (main, point-estimate)", paste0("GLM + spatial smooth, ", gamm_used)),
  AIC         = c(AIC(fit), AIC(gam_fit)),
  BIC         = c(BIC(fit), BIC(gam_fit)),
  Deviance    = c(deviance(fit), deviance(gam_fit)),
  Residual_df = c(df.residual(fit), df.residual(gam_fit)),
  Dispersion  = c(summary(fit)$dispersion, summary(gam_fit)$dispersion)
)
##Model fit comparison: main GLM vs. GLM + spatial smooth
print(fit_comparison, row.names = FALSE)

##Option write the table comparing the fits
#write.csv(fit_comparison, "glm_vs_gamm_fit_comparison.csv", row.names = FALSE)

##Compare parameter estimates: main GLM vs. GAMM parametric part -
glm_coefs  <- coef(fit)#main glm coefficients
gamm_coefs <- coef(gam_fit)[names(glm_coefs)]#gamm coefficients

##Compare the estimated parameters
param_compare <- data.frame(
  term          = names(glm_coefs),
  GLM_estimate  = as.numeric(glm_coefs),
  GAMM_estimate = as.numeric(gamm_coefs),
  Difference    = as.numeric(gamm_coefs - glm_coefs)
)

##Print the table of parameter comparison. Table S2 in the Supporting Information
print(param_compare, row.names = FALSE)

##Write to csv file
write.csv(param_compare, "glm_vs_gamm_parameter_comparison.csv", row.names = FALSE)

##Moran's I for the GAMM residual deviance, at several scales ----
moran_by_scale_gamm <- moran_correlogram(dat$resid_dev_gamm)

#Moran's I correlogram: residual deviance (%s) by distance band (km)

#write.csv(moran_by_scale_gamm, "moran_by_scale_gamm.csv", row.names = FALSE)

##Figure S7 B. Moran's I by distance bands for the gamm fit
p_moran_gamm <- plot_moran_correlogram(
  moran_by_scale_gamm,
  paste0(gamm_used, "; non-overlapping distance bands")
)
p_moran_gamm

##Full Figure S7 in the Supporting Information
paired_moran <- plot_grid(sp_moran_glm, p_moran_gamm, ncol = 2, labels = c("A", "B"))
paired_moran

##Save the figure
ggsave("Figures/moran_correlogram.pdf", paired_moran, width = 14, height = 12)

##Figure S8 in the Supporting Information
##Paired maps: residual deviance, main GLM vs GAMM ---------------
# Same color scale on both maps (shared min/max across the two residual sets)
# so the spatial patterns are directly comparable.
common_limits <- range(c(dat$resid_dev_glm, dat$resid_dev_gamm), na.rm = TRUE)

##Plot each map
p_map_glm <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey80", linewidth = 0.2) +
  geom_point(data = dat, aes(x = LONG, y = LAT, color = resid_dev_glm),
             size = 3, alpha = 0.85) +
  scale_color_viridis_c(name = "Deviance\nresidual", limits = common_limits) +
  coord_quickmap(xlim = map_xlim, ylim = map_ylim) +
  labs(title = "Main GLM (fit, section 1/2)",
       x = "Longitude", y = "Latitude") +
  my_theme

p_map_gamm <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey80", linewidth = 0.2) +
  geom_point(data = dat, aes(x = LONG, y = LAT, color = resid_dev_gamm),
             size = 3, alpha = 0.85) +
  scale_color_viridis_c(name = "Deviance\nresidual", limits = common_limits) +
  coord_quickmap(xlim = map_xlim, ylim = map_ylim) +
  labs(title = paste0("GLM + spatial smooth (", gamm_used, ")"),
       x = "Longitude", y = "Latitude") +
  my_theme

##pair the maps
paired_maps <- plot_grid(p_map_glm, p_map_gamm, ncol = 2, labels = c("A", "B"))
paired_maps

##Save the maps for Figure S8
save_plot("Figures/paired_residual_deviance_maps.pdf", paired_maps,
          base_width = 16, base_height = 6.5)

##Optional: plots not used in the Supporting Information
##GAMM diagnostic plots -------------------------------------------
diag_df <- data.frame(
  linpred  = predict(gam_fit, type = "link"),
  fitted   = fitted(gam_fit),
  observed = dat$pbar,
  resid    = residuals(gam_fit, type = "deviance")
)

p_gam_qq <- ggplot(diag_df, aes(sample = resid)) +
  stat_qq(size = 1.8, alpha = 0.7) +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(title = "Q-Q plot of deviance residuals",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  my_theme

p_gam_resid_lp <- ggplot(diag_df, aes(x = linpred, y = resid)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Deviance residuals vs. linear predictor",
       x = "Linear predictor", y = "Deviance residual") +
  my_theme

p_gam_resid_hist <- ggplot(diag_df, aes(x = resid)) +
  geom_histogram(bins = 25, fill = "grey75", color = "black", linewidth = 0.2) +
  labs(title = "Histogram of deviance residuals",
       x = "Deviance residual", y = "Count") +
  my_theme

p_gam_obsfit <- ggplot(diag_df, aes(x = fitted, y = observed)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "grey40", linetype = "dashed") +
  labs(title = "Observed vs. fitted",
       x = "Fitted", y = "Observed") +
  my_theme

gam_diag_grid <- plot_grid(p_gam_qq, p_gam_resid_lp, p_gam_resid_hist, p_gam_obsfit,
                           ncol = 2, labels = c("A", "B", "C", "D"))
gam_diag_grid


##Part VI: Control for climate and compare the means and distributions
##of population density in cold (sago stress) and warm (non sago stress)
##environments
##-------------------------------------------------------
##1. Figure 4A, Sago strategy distribution-----------------------------
psago <- ggplot(Roscoe2025imp, aes((NPP), (MinTemp-15)))+
  theme_bw() +
  geom_point(aes(color=factor(SUB2)), size=4)+ 
  scale_color_viridis_d(option = "D")+
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28), plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Net primary productivity", y="Sago temperature stress", color="Food production", title = "A. NPP and Temperature Mapping of Sago Dominance")+
  # geom_smooth(method="lm")+
  #facet_wrap(~ADP)
  geom_hline(yintercept = 0)
psago

##2.--Figure 4B. Compare population density in Highland Settings; 
##that is in cold (sago stressed) environments
Roscoe2025impc<-subset(Roscoe2025imp, MinTemp<15.01)

pcar1 <- ggplot(Roscoe2025impc, aes(factor(SUB2), log(DENSITY)))+
  geom_violin(fill="white")+
  geom_jitter(aes(color=factor(PROTEIN)),size=3, width = 0.05)+
  scale_color_viridis_d(option = "D")+
  stat_summary(fun=median, geom="point", size=4, color="black")+
  stat_boxplot(geom ='errorbar')+
  #facet_wrap( ~ factor(CompID1))+
  #scale_fill_manual(values=c("#FC4E07", "#00A4CCFF"))+
  theme_bw() +
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28),  plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Food production", y=" ln Population density", 
       title = "B. Population Density by Food Production (Sago Stress <0)", color="Protein source")+
  annotate("text", x =1.5, y = .75, label = "BF=1577.827", size = 6)
#annotate("text", x =2.55, y = -.10, label = "W=53", size = 8)+
#annotate("text", x =2.1, y = -.15, label = "W=87**", size = 8)
#facet_wrap(~Roscoe2023c.ADP)+
#geom_hline(yintercept=0)
pcar1

library(BayesFactor)

#Subset the data into two dataframes, one for period 1 and one for period 2
period1<-subset(Roscoe2025impc,SUB2=="AGG")
period2<-subset(Roscoe2025impc,SUB2=="HORT")

# Perform Bayesian t-test on maize attributes by periods 1 and 2
bayesian_ttest <- ttestBF(x = log(period1$DENSITY) , y = log(period2$DENSITY), rscale=.707) 

# Show the result
print(bayesian_ttest)

###Create and Export Figure 4 from the main text.
library(cowplot)

Fig4<-plot_grid(psago, pcar1, ncol=2, align="hv", axis = "rl")
Fig4

pdf("Figures/Figure4.pdf", width=16.55, height=14)
Fig4
dev.off()

##3.-----Compare cases in warm settings Horticultre vs. Sago
##Not used in the Supporting Information
Roscoe2025impc2 <- subset(Roscoe2025imp, MinTemp > 15.01 & SUB2 %in% c("HORT", "SAGO"))

pcar2 <- ggplot(Roscoe2025impc2, aes(factor(SUB2), log(DENSITY)))+
  geom_violin(fill="white")+
  geom_jitter(aes(color=factor(PROTEIN)),size=3, width = 0.05)+
  scale_color_viridis_d(option = "D")+
  stat_summary(fun=median, geom="point", size=4, color="black")+
  stat_boxplot(geom ='errorbar')+
  #facet_wrap( ~ factor(CompID1))+
  #scale_fill_manual(values=c("#FC4E07", "#00A4CCFF"))+
  theme_bw() +
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28),  plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Food production", y=" ln Population density", 
       title = "B. Population Density by Food Production (Sago Stress <0)", color="Protein source")+
  annotate("text", x =1.5, y = .75, label = "BF=3309.89", size = 6)
#annotate("text", x =2.55, y = -.10, label = "W=53", size = 8)+
#annotate("text", x =2.1, y = -.15, label = "W=87**", size = 8)
#facet_wrap(~Roscoe2023c.ADP)+
#geom_hline(yintercept=0)
pcar2

#Subset the data into two dataframes, one for period 1 and one for period 2
period1<-subset(Roscoe2025impc2,SUB2=="SAGO")
period2<-subset(Roscoe2025impc2,SUB2=="HORT")

# Perform Bayesian t-test on maize attributes by periods 1 and 2
bayesian_ttest <- ttestBF(x = log(period1$DENSITY) , y = log(period2$DENSITY), rscale=.707) 

# Show the result
print(bayesian_ttest)

pdf("Figures/SIWarmComp.pdf", width=12.55, height=10)
pcar2
dev.off()



##4. Full graph for all environments=================================
##Not used in the SI
pcar3 <- ggplot(Roscoe2025imp, aes(factor(SUB2), log(DENSITY)))+
  geom_violin(fill="white")+
  geom_jitter(aes(color=factor(PROTEIN)),size=3, width = 0.05)+
  scale_color_viridis_d(option = "D")+
  stat_summary(fun=median, geom="point", size=4, color="black")+
  stat_boxplot(geom ='errorbar')+
  #facet_wrap( ~ factor(CompID1))+
  #scale_fill_manual(values=c("#FC4E07", "#00A4CCFF"))+
  theme_bw() +
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28),  plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Food production", y=" ln Population density", 
       title = "Population Density by Food Production ID", color="Protein source")
  #annotate("text", x =1.5, y = .75, label = "BF=1577.827", size = 6)
#annotate("text", x =2.55, y = -.10, label = "W=53", size = 8)+
#annotate("text", x =2.1, y = -.15, label = "W=87**", size = 8)
#facet_wrap(~Roscoe2023c.ADP)+
#geom_hline(yintercept=0)
pcar3





