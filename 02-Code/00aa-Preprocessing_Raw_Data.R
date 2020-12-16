# Author: Ankur Shringi (ankurshringi@iisc.ac.in)
# title: 00aa-initial-data-exploration.R
# subtitle: Importing and data exploration for the Pune Tree Census Raw data...
# abstract: Pune tree census Raw data was available in five csv files. This code imports these five csv files (p1.csv, p2.csv, p3.csv, p4.csv and p5.csv). Removed unnecessary or empty columns. It also fixes typos, standardizes column factors, extracts latitude and longitude of each tree , and saves the processed data in a compressed .Rdata format. The code also extracts the botanical names of the trees, extract species, genus, family information, and assigns wood-density values from a global wood-density database. This species wise summary table is also saved as .csv as well as .Rdata format. Executing the code takes a few minutes and upto eight GB of RAM. It might also require the internet access to execute some of the functions from BIOMASS package successfully.
# Project: AGB_Pune
# Date created: 2020-Oct-17 12:30:23 Saturday
# Enter following command to render the code as html
# `r2html()`
# Initialization ----------------------------------------------------------
# Loading custom made utility functions
source("02-Code/util_AGB_Pune.R")
# Deleting R-Environment Variables (Except utility functions)
clr()
# Loading required packages
Packages <-     c("tidyverse", "BIOMASS")
install(Packages); rm(Packages)

# Importing the raw data  ----------------------------------------------------------------
file.names <- list.files("01-Data/01-Raw/Pune Tree Census August 2019/", pattern = "^p", full.names = TRUE)

# Loading the raw data files.
# Following command takes a minute or two to load the dataframe.
df_raw <- map_df(file.names, read.csv)

# Viewing the data 
head(df_raw)

# Checking if any of the columns are either empty or having an identical values in all the rows
uni.values.by.cols <- df_raw %>% map_df(~length(unique(.))) %>% t()

# Following are the names of the empty columns
empty.cols.names <- rownames(uni.values.by.cols)[uni.values.by.cols == 1]
empty.cols.names

# Processing and refining the raw data
df <- df_raw %>% 
  select(-all_of(empty.cols.names)) %>% # Removing empty columns
  mutate(across(where(is.character), str_squish)) %>% # Trimming extra white space, and newlines
  mutate(dbh = girth_cm/pi, # Appending a column: dbh
         id = c(1:dim(.)[1]), #  Appending a column: id (unique id for a tree)
         remarks = recode(remarks, 
                          `Mechanically cut` = "Cut", 
                          `Mechanical Cut` = "Cut"),
         ownership = recode(ownership,
                            `On Road` = "Roadside",
                            `On Foot Path` = "Footpath",
                            `Govt` = "Government"),
         special_collar = recode(special_collar,
                                 `N/A` = NA_character_),
         phenology = recode(phenology,
                            `Throughout year` = "Perennial",
                            `N/A` = NA_character_)) %>% 
  separate(geom, c(NA, "longitude", "latitude"), sep =  " \\(| |\\)", convert = T, extra = "drop") %>% # Separating lat-long
  select(-c("FID", "girth_cm", "ward_name", "easting", "northing", "saar_uid")) # Removing unnecessary columns

# Saving the processed raw data for future use
save(df, file =  "03-Tables/00aa-01-pune.raw.processed.rdata")

# Preparing species list --------------------------------------------------------------------------------
# Preparing a summary by unique species names
sp_raw <- df %>% 
  group_by(botanical_name, common_name, local_name, economic_i, phenology, flowering, is_rare) %>% 
  summarise(n = n(), # Total data entries
            n.dbh = sum.adv(!is.na(dbh)), # Population with non NA dbh
            n.ward = n_distinct(ward), # Present in how many wards
            mean.dbh = mean(dbh, na.rm = TRUE), # Mean dbh values
            mean.height = mean(height_m, na.rm = TRUE), # Mean tree-height
            mean.canopy = mean(canopy_dia_m, na.rm = TRUE)) # Mean canopy diameter

# Getting genus, species, family information after the spelling correction of botanical names
# Resolving the botanical names, using TNRS database (see help for "correctTaxo" function)
sp.mod <- sp_raw %>% 
  filter(botanical_name != "N/A") %>% # Removing NA
  separate(botanical_name, into = c("genus", "species"), sep = " ", extra = "drop", remove = FALSE) %>% 
  mutate(species = if_else((species == "x") & (genus == "Citrus"), "limon", species)) %>% 
  mutate(correctTaxo(genus = genus, species = species, verbose = FALSE)  %>% 
           rename(genus = genusCorrected,
                  species = speciesCorrected,
                  mod.name = nameModified)) %>% 
  mutate(getTaxonomy(genus = genus, findOrder = T) %>% # Getting family names
           rename(order.APG = order) %>% 
           select(-inputGenus)) %>% # Next: Fixing problematic cases manually
  mutate(family = if_else(family == "Palmae", "Arecaceae", family),
         family = if_else(genus == "Crataeva", "Capparaceae", family),
         family = if_else(genus == "Sreculia", "Malvaceae", family))

# Assigning wood-density to the species not in wood density database.
wd.NA <- data.frame(genus = c("Cycas"),
                    species = c("circinalis"),
                    family = "Cycadaceae",
                    wd = 0.5)

# Assigning wood-density to each species  
sp <- sp.mod %>% 
  mutate(getWoodDensity(genus = genus,
                        species = species,
                        family = family,
                        stand = NULL, verbose = FALSE,
                        addWoodDensityData = wd.NA) %>%
           rename(wd.mean = meanWD,
                  wd.sd = sdWD) %>%
           select(-c(family, genus, species)))

# Listing the species synonyms  
sp.syn <- sp %>% 
  group_by(genus, species, family) %>% 
  summarise(n = n(), names = paste(botanical_name, collapse = "; ")) %>% 
  filter(n > 1)

sp.syn

# Saving species wise information for future reference
save(sp, file = "03-Tables/00aa-02-Pune_species_processed.Rdata")
write_csv.adv(sp, "00aa-02-pune_species_processed")
