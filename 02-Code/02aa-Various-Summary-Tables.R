# Author: Ankur Shringi (ankurshringi@iisc.ac.in)
# title: 02aa-Various-Summary-Tables.R
# subtitle: Preparing various summary tables by family, genus, species, ward etc.
# abstract: 
# Project: AGB_Pune
# Date created: 2020-Dec-17 13:27:35 Thursday
# Enter following command to render the code as html
# `r2html()`
# Initialization ----------------------------------------------------------
# Loading custom made utility functions
source("02-Code/util_AGB_Pune.R")
# Deleting R-Environment Variables (Except utility functions)
clr()
# Loading required packages
Packages <-     c("tidyverse")
install(Packages); rm(Packages)
