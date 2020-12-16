# Author: Ankur Shringi (ankurshringi@iisc.ac.in)
# title: 01aa-AGB-Calculations.R
# subtitle: Above-Ground Biomass (AGB) calculations for Pune trees
# abstract: Processed data from 00aa-Preprocessing_Raw_Data.R has been used to calculate AGB and its error estimates. Code is RAM consuming. It requires ~16GB of RAM for AGB error estimation (as it needs to process 100 simulation for ~40lacs trees).
# Project: AGB_Pune
# Date created: 2020-Dec-04 14:34:10 Friday
# Enter following command to render the code as html
# `r2html()`
# Initialization ----------------------------------------------------------
# Loading custom made utility functions
source("02-Code/util_AGB_Pune.R")
# Deleting R-Environment Variables (Except utility functions)
clr()
# Forced garbage collection for freeing up the RAM
gc()
# Loading required packages
Packages <-     c("tidyverse", "BIOMASS")
install(Packages); rm(Packages)

# Loading and processing data ---------------------------------------------------------------------------
# Loading the processed tree census data 
load("03-Tables/00aa-01-pune.raw.processed.rdata")

# Loading the species information (and wood density estimates)
load("03-Tables/00aa-02-Pune_species_processed.Rdata")

# Removing unnecessary columns from sp data.frame
sp <- sp %>%  select(-c("n", "n.dbh", "n.ward", "mean.dbh", "mean.height", "mean.canopy"))

# Merging the species information with the main data.frame
df <- left_join(df, sp)  

# Following is the command visualize the different height-dbh models' fit
# allo.H_D <- modelHD(D = df$dbh, H = df$height_m, useWeight = TRUE)

df$agb = computeAGB(D = df$dbh, WD = df$wd.mean, H = df$height_m)

# Selecting columns for saving the tree location with bare minminum information (for plotting in GIS)
col.names <- c("id", "botanical_name", "dbh", "height_m", "canopy_dia_m", "agb", "longitude", "latitude")
write_csv.adv(df %>% select(all_of(col.names)), "01aa-01-Pune-AGB-Tree-Wise")

# AGB Error Propagation ---------------------------------------------------------------------------------
resultMC <- AGBmonteCarlo(D = df$dbh, H = df$height_m, WD = df$wd.mean, errWD = df$wd.sd, Dpropag = "chave2004", errH = 0.05*df$height_m, n = 100)

# Naming the simulated columns
colnames(resultMC$AGB_simu) <- paste0("simu.", 1:ncol(resultMC$AGB_simu))

# Saving the MC simulation results
save(resultMC, file = "03-Tables/01aa-01-Pune-AGB-MC-Simulation.Rdata")

# Total AGB in Pune City (all trees)
print("mean estimate of total AGB in Pune (Tonne): " %+%  round(resultMC$meanAGB, 2))
print("median estimate of total AGB in Pune (Tonne): " %+%  round(resultMC$medAGB, 2))
print("Standard deviation estimate of total AGB in Pune (Tonne): " %+%  round(resultMC$sdAGB, 2))

# AGB 95% Confidence interval-
print(round(resultMC$credibilityAGB, 2))

# Plotting the AGB estimate histogram
# Green line shows the mean AGB estimate, while the values between red lines comes under 95% confidence interval
hist(colSums(resultMC$AGB_simu, na.rm = TRUE), main = "Histogram of Total AGB Estimates \n Pune Muncipal Corporation Area (~40lac Trees) \n (No. of Monte-Carlo simulations = 100)", xlab = "AGB (Tonne)")
abline(v = resultMC$meanAGB, col = "green")
abline(v = resultMC$credibilityAGB, col = "red")
