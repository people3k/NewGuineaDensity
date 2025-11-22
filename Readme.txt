
(1) Citation to the manuscript: This document describes the data files and R-code that accompany ``Technology and climate impacted population density in contact-era New Guinea" by Jacob Freeman and Paul Roscoe. This README document describes the csv files and how to get started with the analysis. 


(2) Description of the files included in the repository and their relationship to the figures and tables in the manuscript
Files: 


(i) Ethnographic data and climate data. These files are needed to run the code: CentralNewGuinea2025.R in R-studio. CentralNewGuinea2025.R is divided into four sections: I--mapping and extract climate data from 228 polities. II-Statistically imputed missing data. II-Main analyses presented in the paper. IV-supporting analyses. To get started, open CentralNewGuinea2025.R, install and load the relevant packages, and then load Roscoe2025.csv. As you move through the code, you will also need to load the relevant WorldClim raster files where prompted. The raster files that we used are included. 

**Note, please see PrimaryDataReferences for the references used to construct the data below.

All of the csv files contain data on the attributes of Central New Guinea Polities.

Typical Columns include:

SOCIETY-denotes the society’s name.
LAT- denotes the society’s latitudinal centroid.

LONG -denotes the longitude of the society’s centroid.

SUB- identifies the staple plant and staple animal food in the diet. Primary crops were yam, taro, sweet-potato, and banana. Sago comprises starch leached from the pith of the sago palm shortly before it floresces. Pigs are the primary livestock. Agriculture is defined as crop cultivation followed by a fallow period of 10 years or less; horticulture as crop cultivation followed by a fallow period of more than 10 years. 

	AH = Agriculture + Hunting
	AL = Agriculture + Livestocking
	HL = Horticulture + Livestocking
	HH = Horticulture + Hunting
	H–HL = Horticulture + Hunting/Livestocking (in approximately equal proportion)
	SF = Sago + Fishing
	S–FH = Sago + Fishing/Hunting (in approximately equal proportion)
	SH–FH = Sago/Horticulture (sago dominant) + Fishing/Hunting 
	(in approximately equal 	proportion)
	SH = Sago + Hunting
	SH–H = Sago/Horticulture (sago dominant) + Hunting

SUB2-A re-coding of SUB by dominant carbohydrate production strategy.
		AGG=Agricultural production—greatest investment in weeding, fencing, 					mounding and water control to produce sweet potatoes, taro, etc.
		HORT= Cultivation of domesticated plants but less investment in weeding, etc. 				than AGG cases.
		SAGO=The exploitation of sago as the primary carbohydrate source and 					terrestrial protein sources.
		SAGO-FISH=Mixed economies focused on fishing and sago exploitation.

PROTEIN-A re-coding of SUB by dominant protein production strategy.
		Livestock=Raising domesticated pigs for protein
		Hunting=Hunting wild pigs and other terrestrial animals for protein.
		Fishing=Exploiting marine and riverine prey for protein.

DENSITY- denotes population density (/square kilometer).

PLTYSIZE- denotes numerical population size of polity.

AREA-Denotes the square kilometers claimed by a polity.

Elevation--Extracted elevation at a society's latitude, longitude centroid.

MinTemp- Extracted mean temperature of the coldest month at a society's latitude, longitude centroid.

CRR-Extracted mean annual rainfall at a society's latitude, longitude centroid.

MAT-Extracted mean annual temperature at a society's latitude, longitude centroid.

(ii) The figure presented in the discussion uses the file Freeman2020Agg.csv. This file combines the data published in Freeman, J., Robinson, E., Beckman, N. G., Bird, D., Baggio, J. A., & Anderies, J. M. (2020).The global ecology of human population density and interpreting changes in paleo-population density. Journal of Archaeological Science, 120, 105168

Columns include:
SampleID-1=Freeman et al 2020 Study; 0=the current study
density-Population density of an ethnographic society.
NPP-Estimated net primary productivity in a groups territory using the Miami model
ID-A description of the primary subsistence. 
	PS=Pastoralism
	AG=Domesticated plants
	SG=Sago


(3)The code to analyze the data is contained in CentralNewGuinea2025.R. The analysis was run in "R version 4.2.2 (2022-10-31 ucrt)" with the Rstudio integrated development environment version ``2023.06.0 Build 421." ChatGPT was used to help create the base map of New Guinea.


(4) Getting started working with the project:

To begin working with the data, (i) open the R files. (ii) Install and load all of the relevant packages at the beginning of the document, and (iii) follow the comments to begin reproducing the results reported in the manuscript. 
