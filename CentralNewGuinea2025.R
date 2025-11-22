###Central New Guinea data and analysis
library(ggplot2)
library(nlme)
library(dplyr)
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

#Part IA Mapping and Extracting Climate Data for each New Guinea Polity======================
#This code re-produces Figure 1 in the main text and the data necessary to conduct the
#regression analyses reported in the main text and for constructing Figures in Part II of the code.
#==========================================================================================

##1. Set working directory and Load Ethnographic Data

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
elev_values <- extract(elev_terra, points1)

# Combine with original coordinates
result <- cbind(coords1, elevation = elev_values[,2])  # column 1 is ID, column 2 is elevation
print(result)

Elevation<-result$elevation

RoscoeElv<-cbind(Elevation,Roscoe2025)

###Extract bioclimate variables. To access the climate rasters used in the paper, go to 
#WorldClim.org and download the 30 arch second bioclimatic variables 
#and place the unzipped rasters in the Climate directory.


bioclim <- rast("Climate/wc2.1_30s_bio_1.tif")  # load one layer

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

###Extract rainfall data
bioclim2 <- rast("Climate/wc2.1_30s_bio_12.tif")  # load one layer

# Convert to spatial points
points <- vect(coords, crs = "EPSG:4326")

# Extract values

bio_values2 <- terra::extract(bioclim2, points)
CRR<-bio_values2$wc2.1_30s_bio_12

RoscoePrecip<-cbind(CRR, RoscoeTemp)

##Minimum temperature of the coldest month
bioclim6 <- rast("Climate/wc2.1_30s_bio_6.tif")  # load one layer

lon=Roscoe2023c$LONG
lat=Roscoe2023c$LAT
coords <- data.frame(lon, lat)

# Convert to spatial points
points <- vect(coords, crs = "EPSG:4326")

# Extract values

bio_values2 <- terra::extract(bioclim6, points)
MinTemp<-bio_values2$wc2.1_30s_bio_6
Roscoetempmin<-cbind(MinTemp, RoscoePrecip)



# save to CSV
write.csv(Roscoetempmin, "Roscoe2025Climate.csv", row.names = FALSE)

#Load Roscoe original data with climate variables extracted=============
Roscoe2025c<-read.csv(file="Roscoe2025Climate.csv", header=T)
# Plot the clipped elevation data

#Create Figure 1
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
  scale_color_manual(values = c("black","gray","red","blue"), name = "Subsistence regime") +
  scale_shape_manual(values = c(17, 17,18,18), name = "Subsistence regime") +
  # geom_text(data = Roscoe2025, aes(x = LONG, y = LAT, label = rownames(Roscoe2025)),
  #   inherit.aes = FALSE, size = 2.5, vjust = -1)+
  theme_minimal()
NGelv

###Export Figure 1 to your directory
pdf("Figures/NGmap.pdf", width=12.55, height=10)
NGelv
dev.off()

#=========================================================================
#Part 1B: Data on energy return on investment and population density used to 
#help inform modeling constraints on maximum population density.

#Figure 3: Energy return on investment ~ ln population density
#Load data
EROI<-read.csv(file="energydensity.csv", header=T)

EROI2<- EROI%>% group_by(group) %>%
  summarize(EROI = mean(EROI))
EROI2

density<-EROI%>% group_by(group) %>%
  summarize(density = mean(density))

density$group<-NULL

EROI3<-cbind(EROI2,density)

NGcases <- EROI3 %>% 
  filter(group %in% c("Miyanmin", "Enga (Raiapu)"))

p3 <- ggplot(EROI3, aes(y=log(density), x=log(EROI)))+
  theme_bw() +
  geom_point(aes(), size=4)+ 
  #scale_x_continuous(limit=c(-1,5))+
  #scale_color_gradient(low ="#F8766D", high = "#619CFF", breaks=c(2,4,6)) +
  # scale_size_manual(values=c(5,3,4))+
  geom_text(data=NGcases, aes(label=group), # Add row names as labels
            nudge_x = -0.2, nudge_y = -0.2, # Adjust label position to avoid overlap
            check_overlap = FALSE) + # Prevent overlapping labels
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28), plot.title = element_text(size=18, face = "bold"))+
  labs(x = "ln Energy return on investment", y="ln Population density",
       color = "Production ID",title = "Population Density vs. Energy Return")+
  geom_smooth(se=FALSE, method="lm")
# geom_hline(yintercept = 2.99)
p3

#Export Figure3
pdf("Figures/Figure3.pdf", width=8.55, height=6)
p3
dev.off()

#Simple regression of density on EROI
fit1<- glm(log(density) ~ log(EROI), data=Keepmj)
summary(fit1)

#====================================================
###Part II: Data Imputation. We impute missing data using the missForest package. Note:
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

###Part III: Analysis of the data to replicate the graphs and regression analyses presented
##in the results section of the main manuscript.
##=================================================================

##Analysis of Imputed Data
###read in data files with climate and elevation=====================================
#Imputed data
Roscoe2025imp<-read.csv(file="RoscoeImpFinal2025.csv", header=T)
#Original data with no imputation
Roscoe2025c<-read.csv(file="Roscoe2025SIData.csv", header=T)


# Calculate Miami Model NPP on imputed data
Roscoe2025imp$MATNPP <- 3000 / (1 + exp(1.315 - 0.119 * Roscoe2025imp$MAT))
Roscoe2025imp$CRRNPP<- 3000 * (1 - exp(-0.000664 * Roscoe2025imp$CRR))
Roscoe2025imp$NPP <- pmin(Roscoe2025imp$MATNPP, Roscoe2025imp$CRRNPP, na.rm = TRUE)

###Exploratory graphs of NPP against DENSITY for the imputed data

den1 <- ggplot(Roscoe2025imp, aes((NPP), log(DENSITY), color=factor(SUB2)))+
  theme_bw() +
  geom_point(aes(), size=4)+ 
  #scale_x_continuous(limit=c(-1,5))+
  #scale_color_gradient(low ="#F8766D", high = "#619CFF", breaks=c(2,4,6)) +
  scale_color_viridis_d(option = "D")+
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28), plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Net Primary Productivity", y="ln Population density", title = "")+
  geom_smooth(se=FALSE, method="lm")
  #facet_wrap(~PROTEIN)
#geom_vline(xintercept = 2.99)
den1

den1n <- ggplot(Roscoe2025imp, aes((NPP), log(DENSITY), color=factor(SUB2)))+
  theme_bw() +
  geom_point(aes(), size=4)+ 
  #scale_x_continuous(limit=c(-1,5))+
  #scale_color_gradient(low ="#F8766D", high = "#619CFF", breaks=c(2,4,6)) +
  scale_color_viridis_d(option = "D")+
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28), plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Net Primary Productivity", y="ln Population density", title = "")+
  geom_smooth(se=FALSE, method="lm")+
  facet_wrap(~PROTEIN)
#geom_vline(xintercept = 2.99)
den1n

#Export SI Figure 1
pdf("Figures/SIFig1.pdf", width=14.55, height=12)
den1n
dev.off()

#Exploratory graph of NPP vs. Territory Size
ter1<-ggplot(Roscoe2025imp, aes((NPP), log(AREA)))+
  theme_bw() +
  geom_point(aes(color=factor(SUB2)), size=4)+ 
  #scale_x_continuous(limit=c(-1,5))+
  #scale_color_gradient(low ="#F8766D", high = "#619CFF", breaks=c(2,4,6)) +
  # scale_size_manual(values=c(5,3,4))+
  scale_color_viridis_d(option = "D")+
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28), plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Net Primary Productivity", y="ln Territory size (sq. km)", title = "")+
  geom_smooth(se=FALSE, method="lm")
#facet_wrap(~ADP)
#geom_vline(xintercept = 2.99)
ter1

##------------------------------------------------------------------------------
# Calculate Miami Model NPP on original data
Roscoe2025c$MATNPP <- 3000 / (1 + exp(1.315 - 0.119 * Roscoe2025c$MAT))
Roscoe2025c$CRRNPP<- 3000 * (1 - exp(-0.000664 * Roscoe2025c$CRR))
Roscoe2025c$NPP <- pmin(Roscoe2025c$MATNPP, Roscoe2025c$CRRNPP, na.rm = TRUE)

###Graph NPP against Population DENSITY for the original data
den2 <- ggplot(Roscoe2025c, aes((NPP), log(DENSITY), color=factor(SUB2)))+
  theme_bw() +
  geom_point(aes(), size=4)+ 
  #scale_x_continuous(limit=c(-1,5))+
  #scale_color_gradient(low ="#F8766D", high = "#619CFF", breaks=c(2,4,6)) +
  scale_color_viridis_d(option = "D")+
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28), plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Net Primary Productivity", y="ln Population density", title = "")+
  geom_smooth(se=FALSE, method="lm")
#geom_vline(xintercept = 2.99)
den2

###Regression Analysis===========================================
#Load mgcv package for running a General Additive Model
#install.packages("mgcv")
library(mgcv)

#Model #1 NPP regressed on ln population density

fit_npp <- gam(log(DENSITY) ~ NPP +s(LONG, LAT),
               data = Roscoe2025imp,
               family = gaussian())
predicted<-cbind(predict(fit_npp), Roscoe2025imp)


##Model #2: Main result presented in the text
fit_gam <- gam(log(DENSITY) ~ NPP + SUB2 +s(LONG, LAT),
               data = Roscoe2025imp,
               family = gaussian())
predicted2<-cbind(predict(fit_gam), predicted)

#Model #3: SI model: Codes subsistence infrastructure into a carbohydrate and protein capture variable
fit_gam2 <- gam(log(DENSITY) ~ NPP+PROTEIN+SUB2 +s(LONG, LAT),
               data = Roscoe2025imp,
               family = gaussian())
predicted3<-cbind(predict(fit_gam2), predicted2)

#Summary of model fit and coeffs.
summary(fit_npp)
summary(fit_gam)
summary(fit_gam2)

##Compare the fit and complexity of the models
#anova(fit_npp, fit_gam, fit_gam2, test = "LRT")
BICImp<-BIC(fit_npp, fit_gam, fit_gam2)
xtable(BICImp)

#Plot the spatial splines of the main result model
plot(fit_gam2)

#save the residuals of a given GAM.
resid_glm <- residuals(fit_gam2)  

# Create an sf object
coords <- cbind(Roscoe2025imp$LONG, Roscoe2025imp$LAT)

#Create neighbors list (e.g., using k-nearest neighbors or distance)
# Example using K nearest neighbors
nb <- knn2nb(knearneigh(coords, k = 5))
lw <- nb2listw(nb, style = "W")  # Row-standardized weights

#Test for spatial autocorrelation (Moran's I)
moran_test <- moran.test(resid_glm, lw)
print(moran_test)


####Sago strategy distribution===========================================
psago <- ggplot(Roscoe2025imp, aes((NPP), (MinTemp-15)))+
  theme_bw() +
  geom_point(aes(color=factor(SUB2)), size=4)+ 
  scale_color_viridis_d(option = "D")+
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28), plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Net primary productivity", y="Sago temperature stress", color="Subsistence ID", title = "A. NPP and Temperature Mapping of Sago Dominance")+
  # geom_smooth(method="lm")+
  #facet_wrap(~ADP)
  geom_hline(yintercept = 0)
psago


##Compare population density in Highland Settings; that is in cold (sago stressed) environments
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
  labs(x = "Subsistence ID", y=" ln Population density", 
       title = "B. Population Density by Subsistence ID (Sago Stress <0)", color="Protein source")+
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


###Full graph for all environments=================================

pcar2 <- ggplot(Roscoe2025imp, aes(factor(SUB2), log(DENSITY)))+
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
  labs(x = "Subsistence ID", y=" ln Population density", 
       title = "Population Density by Subsistence ID", color="Protein source")
  #annotate("text", x =1.5, y = .75, label = "BF=1577.827", size = 6)
#annotate("text", x =2.55, y = -.10, label = "W=53", size = 8)+
#annotate("text", x =2.1, y = -.15, label = "W=87**", size = 8)
#facet_wrap(~Roscoe2023c.ADP)+
#geom_hline(yintercept=0)
pcar2

pdf("Figures/SIFigure2.pdf", width=12.55, height=10)
pcar2
dev.off()

###Add NG data to global data from Freeman et al. 2020
#Freeman, J., Robinson, E., Beckman, N. G., Bird, D., Baggio, J. A., & Anderies, J. M. (2020). 
#The global ecology of human population density and interpreting changes in paleo-population density. 
#Journal of Archaeological Science, 120, 105168
##Make Figure 5==============================================================
globesamp<-read.csv(file="Freeman2020Agg.csv", header=T)
globesamp2<-subset(globesamp, sampleID < 1)

pglden1 <- ggplot(globesamp2, aes((NPP), log(density)))+
  theme_bw() +
  geom_point(aes(color=factor(ID)), size=4)+ 
  scale_color_viridis_d(option = "D")+
  scale_x_continuous(breaks=c(0,500,1000,1500,2000,2500))+
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28), plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Net primary productivity", y="ln Population density", color="Subsistence ID", title = "A. New Guinea NPP VS. Density")+
  geom_smooth(se=FALSE)
  # facet_wrap(~sampleID)
  #geom_hline(yintercept = 0)
  pglden1

pglden <- ggplot(globesamp, aes((NPP), log(density)))+
  theme_bw() +
  geom_point(aes(color=factor(ID)), size=4)+ 
  scale_color_viridis_d(option = "D")+
  scale_x_continuous(breaks=c(0,500,1000,1500,2000,2500))+
  theme(axis.text.x = element_text(size=28, colour = "black"), axis.title.x=element_text(size=24),
        axis.title.y=element_text(size=24), axis.text.y = element_text(
          size=28), plot.title = element_text(size=18, face = "bold"))+
  labs(x = "Net primary productivity", y="ln Population density", color="Subsistence ID", title = "B. Global Scale NPP VS. Density")+
  geom_smooth(se=FALSE)
 # facet_wrap(~sampleID)
  #geom_hline(yintercept = 0)
pglden

library(cowplot)
Fig5<-plot_grid(pglden1, pglden, ncol=2, align="hv", axis = "rl")
Fig5

pdf("Figures/Figure5.pdf", width=16.55, height=12)
Fig5
dev.off()


####Extra Analysis. Simple GLM described in the footnote of the main paper.
###GLM REGRESSION ANALYSIS of NPP and Subsistence on Population Density=======================================================

fit1<-glm(log(DENSITY)~(NPP)+SUB2, data=Roscoe2025imp)
summary(fit)
#xtable(summary(fit))
#plot(fit)

library(spdep)

resid_glm <- residuals(fit, type = "pearson")  # Or type = "response" if preferred

# Get spatial coordinates
coords <- cbind(Roscoe2025imp$LONG, Roscoe2025imp$LAT)  # Replace x and y with your coordinate column names

# Create neighbors list (e.g., using k-nearest neighbors or distance)
# Example using 5 nearest neighbors
nb <- knn2nb(knearneigh(coords, k = 5))
lw <- nb2listw(nb, style = "W")  # Row-standardized weights

#Test for spatial autocorrelation (Moran's I)
moran_test <- moran.test(resid_glm, lw)
print(moran_test)

