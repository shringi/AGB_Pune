# Author: Ankur Shringi (ankurshringi@iisc.ac.in)
# title: util_AGB_Pune.R
# subtitle: Compilation of companion custom R-functions used in the main code files.
# abstract: A list of custom R functions, used occasionally in all .R files located in 02-Code folder. Adapted from util_Global.R  located in the github repository <https://github.com/shringi/util_Global>
# Project: AGB_Pune
# Date created: 2020-Dec-15 17:09:23 Tuesday
# Enter following command to render the code as html
# `r2html()`
# Installing and loading packages ------------------------------------
# A modified version of library command with an additional feature--if a package is not found then it downloads and installs the package automatically.
# `install("dplyr")`
install <- function(package1, ...)
{
  # convert arguments to vector
  packages <- c(package1, ...)
  
  # check if loaded and installed
  loaded        <- packages %in% (.packages())
  names(loaded) <- packages
  
  installed        <- packages %in% rownames(installed.packages())
  names(installed) <- packages
  
  # start loop to determine if each package is installed
  load_it <- function(p, loaded, installed)
  {
    if (loaded[p])
    {
      #print(paste(p, "is already loaded!"))
    } else {
      print(paste(p, "not loaded."))
      
      if (installed[p]) {
        print(paste(p, "is already installed!"))
        suppressPackageStartupMessages(do.call("library", list(p)))
        print(paste(p, "has been loaded now."))
      } else {
        print(paste(p, "is not installed!"))
        print(paste(p, "is being installed."))
        install.packages(p, dependencies = TRUE)
        print(paste(p, "has been installed."))
        suppressPackageStartupMessages(do.call("library", list(p)))
        print(paste(p, "has been loaded now."))
      }
    }
  }
  invisible(lapply(packages, load_it, loaded, installed));
}
# Save data as csv file (no need to worry about path and extension) ------------
# `write.csv.adv (data, file.name, path = getwd(), subfolder = "Output-Tables" )`
write_csv.adv <- function(data, file.name, path = getwd(),
                          subfolder = "03-Tables", quote = FALSE){
  install("readr")
  # if subfolder doesn't exist then create it.
  if (!file.exists(subfolder)) {
    dir.create(file.path(path, subfolder))
  }
  # In case my file name already have .csv in it
  # we don't wan't to add another .csv
  if (substr(file.name, nchar(file.name) - 3, nchar(file.name)) != ".csv") {
    file = paste0(file.name, ".csv")
  } else {
    file = gsub("(.csv)+$", ".csv", file.name)
  }
  write_csv(data, path %/% subfolder %/% file, quote = quote)
}

# Open Current Directory directly from R ---------------------------------------
# `openwd()`
openwd <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows") {
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))}
}
# sum.adv ------
# Modified sum function to handle values like all NA
sum.adv <- function(x){
  if (sum(!is.na(x)) == 0) {
    out <- NA
  } else {
    out <- sum(x, na.rm = TRUE)
  }
  return(out)
}

# max.adv ------
# Modified max function to handle values like all NA
max.adv <- function(x){
  if (sum(!is.na(x)) == 0) {
    out <- NA
  } else {
    out <- max(x, na.rm = TRUE)
  }
  return(out)
}

# max.adv ------
# Modified min function to handle values like all NA
min.adv <- function(x){
  if (sum(!is.na(x)) == 0) {
    out <- NA
  } else {
    out <- min(x, na.rm = TRUE)
  }
  return(out)
}

# export2html -------------------------------------------------------------------------------------------
# `export2html(".R")`
export2html <- function(filename, folder = '05-Html', suppress_warnings = TRUE, browse = TRUE, output_file = NULL) {
  if (suppress_warnings) {
    suppressWarnings(rmarkdown::render(filename, output_dir = folder, clean = TRUE, quiet = TRUE,
                                       output_file = output_file))
  } else {
    rmarkdown::render(filename, output_dir = folder, clean = TRUE, quiet = TRUE, output_file = output_file)
  }
  if (is.null(output_file)) {
    output_file = folder %/% substr(filename, start = 1, nchar(filename) - 2)
  }
  if (browse) {
    file_wt_ext <- folder %/% output_file
    if (file.exists(file_wt_ext %+% ".html")) {
      browseURL(file_wt_ext %+% ".html", getOption("browser"))
    } else if (file.exists(file_wt_ext %+% ".nb.html")) {
      browseURL(file_wt_ext %+% ".nb.html", getOption("browser"))
    } else {
      stop("Corresponding html file doesnt exists!!")
    }
  }
}
# r2html -------------------------------------------------------------------------------------------------
# Funtion which converts normal R code into a rmarkdown code. There already exists some implementation, for example see <https://rmarkdown.rstudio.com/articles_report_from_r_script.html> for more details.
# This function however does an additional things.
# 1. Converts R-section to R markdown sections
# 2. Converts normal comments to roxygen comments automatically
# 3. Appends detailed output format
# `r2html()`
r2html <- function(){
  invisible(install("dplyr"))
  file <- rstudioapi::getSourceEditorContext()$path
  flIn  <- readLines(file)
  # open the file and read in all the lines
  head <- unlist(strsplit(flIn[2:4], split = '\n'))
  render <- unlist(strsplit(flIn[5:8], split = '\n'))
  time_now <- "# Last Rendered: " %+%
    format(Sys.time(), format = "%Y-%b-%d %H:%M:%S " %+% weekdays(as.Date(Sys.Date(),'%d-%m-%Y')))
  flIn <- flIn[-c(1:8)]
  block <- "#'author:
#'  - name: Ankur Shringi
#'    email: ankurshringi@iisc.ac.in
#'    affiliation: Centre for Ecological Sciences, Indian Institute of Science, Bangalore, India
#'output:
#'  html_notebook:
#'    self_contained: true
#'    toc: true
#'    toc_depth: 2
#'    number_sections: true
#'    toc_float: true
#'    highlight: kate
#'---
#'*****"
  text_block <- unlist(strsplit(block, split = '\n'))
  # concatenate the old file with the new text
  flIn <- c("#'---", head, text_block, render[1:2], time_now, render[3:4], "#'", "#'*****", flIn)
  secStrt <- which(grepl(flIn, pattern = "# ", perl = TRUE))
  secEnd <- which(grepl(flIn, pattern = "----", perl = TRUE))
  comLines <- which(grepl(flIn, pattern = "^+# "))
  secLines <- intersect(secStrt, secEnd)
  sketchLines <- which(grepl(flIn, pattern = "sketch\\(", perl = TRUE))
  for (i in 1:length(flIn)) {
    if (i %in% secLines) {
      flIn[i] <-  flIn[i] %>%
        gsub(pattern = "[-]+$", replacement = "", x = .) %>%
        gsub(pattern = "^+# ", replacement =  "#' ## ", x = .)
    } else if (i %in% comLines) {
      flIn[i] <-  flIn[i] %>%
        gsub(pattern = "^+# ", replacement = "#' ", x = .) %+% "<br>"
    }
    if (i %in% sketchLines) {
      flIn[i] <-  flIn[i] %>%
        gsub(pattern = ')$', replacement = ', export = FALSE)', x = .)
    }
  }
  filename = basename(file)
  fn = substr(filename, start = 1, nchar(filename) - 2)
  writeLines(flIn, con = "temp_rmd.R")
  export2html("temp_rmd.R", folder = '05-Html', suppress_warnings = TRUE, browse = TRUE, output_file = fn)
  if (file.exists("temp_rmd.R"))
    #Delete file if it exists
    file.remove("temp_rmd.R")
}

# Clear data or screen -------------------------------------------
# `clr()`
# `clr("all")`
clr <- function(mode="notall", except = NULL) {
  ENV <- globalenv()
  ll <- ls(envir = ENV)
  
  if (is.null(except)) {
    except = c("clr", "clc")
  } else {
    except = c("clr", "clc", except)
  }
  
  ll <- ll[!ll %in% except]
  
  if ((mode == "all") || (mode == "All")) {
    rm(list = ll, envir = ENV)
  } else if (mode == "notall") {
    functions <- lsf.str(envir = ENV)
    rm(list = setdiff(ll, functions), envir = ENV)}
}
# clc()
clc <- function(){cat("\014")}
# Concenate object as Text -------------------------------------------
# `"a" %+% "b"`
"%+%" = function(obj_1, obj_2) {
  paste(obj_1, obj_2, sep = "")
}

# Add Path to filename ----------------------------------------------------
# `getwd()`
"%/%" = function(path, file) {
  if (substr(file, 1, 1) == "/") {
    file = substr(file, 2, nchar(file))
  }
  if (substr(file, nchar(file), nchar(file)) == "/" ) {
    file = substr(file, 1, nchar(file) - 1 )
  }
  if (substr(path, nchar(path), nchar(path)) != "/" ) {
    path = paste0(path, "/")
  }
  return(paste0(path, file ))
}

# Calculating various AGB summary using agbSummary() ----------------------------------------------------
# Function to calculate a detailed summary by ward or species.
agbSummary <- function(df, group_by = "ward", col.dbh = "dbh", col.height = "height_m") {
  out <- df %>% group_by(!!!syms(group_by)) %>%
    mutate(dbh.min = min(c_across(all_of(col.dbh)), na.rm = TRUE),
           dbh.q.025 = quantile(c_across(all_of(col.dbh)), na.rm = TRUE, probs = .025),
           dbh.q.25 = quantile(c_across(all_of(col.dbh)), na.rm = TRUE, probs = .25),
           dbh.median = median(c_across(all_of(col.dbh)), na.rm = TRUE),
           dbh.q.75 = quantile(c_across(all_of(col.dbh)), na.rm = TRUE, probs = .25),
           dbh.q.975 = quantile(c_across(all_of(col.dbh)), na.rm = TRUE, probs = .975),
           dbh.max = max(c_across(all_of(col.dbh)), na.rm = TRUE),
           dbh.mean = mean(c_across(all_of(col.dbh)), na.rm = TRUE),
           dbh.sd = sd(c_across(all_of(col.dbh)), na.rm = TRUE),
           dbh.n = sum(!is.na(c_across(all_of(col.dbh)))),
           dbh.cv = dbh.sd*100/dbh.mean,
           dbh.se = dbh.sd/sqrt(dbh.n),
           H.min = min(c_across(all_of(col.height)), na.rm = TRUE),
           H.q.025 = quantile(c_across(all_of(col.height)), na.rm = TRUE, probs = .025),
           H.q.25 = quantile(c_across(all_of(col.height)), na.rm = TRUE, probs = .25),
           H.median = median(c_across(all_of(col.height)), na.rm = TRUE),
           H.q.75 = quantile(c_across(all_of(col.height)), na.rm = TRUE, probs = .25),
           H.q.975 = quantile(c_across(all_of(col.height)), na.rm = TRUE, probs = .975),
           H.max = max(c_across(all_of(col.height)), na.rm = TRUE),
           H.mean = mean(c_across(all_of(col.height)), na.rm = TRUE),
           H.sd = sd(c_across(all_of(col.height)), na.rm = TRUE),
           H.n = sum(!is.na(c_across(all_of(col.height)))),
           H.cv = H.sd*100/H.mean,
           H.se = H.sd/sqrt(H.n),
           wd.min = min(wd.mean, na.rm = TRUE),
           wd.q.025 = quantile(wd.mean, na.rm = TRUE, probs = .025),
           wd.q.25 = quantile(wd.mean, na.rm = TRUE, probs = .25),
           wd.median = median(wd.mean, na.rm = TRUE),
           wd.q.75 = quantile(wd.mean, na.rm = TRUE, probs = .25),
           wd.q.975 = quantile(wd.mean, na.rm = TRUE, probs = .975),
           wd.max = max(wd.mean, na.rm = TRUE),
           wd.mean = mean(wd.mean, na.rm = TRUE),
           wd.sd = sd(wd.mean, na.rm = TRUE),
           wd.n = sum(!is.na(wd.mean)),
           wd.cv = wd.sd*100/wd.mean,
           wd.se = wd.sd/sqrt(wd.n)) %>%
    group_by(dbh.min, dbh.min, dbh.q.025, dbh.q.25, dbh.median, dbh.q.75, dbh.q.975, dbh.max, dbh.mean, dbh.sd, dbh.n, dbh.cv, dbh.se,
             H.min, H.min, H.q.025, H.q.25, H.median, H.q.75, H.q.975, H.max, H.mean, H.sd, H.n, H.cv, H.se,
             wd.min, wd.min, wd.q.025, wd.q.25, wd.median, wd.q.75, wd.q.975, wd.max, wd.mean, wd.sd, wd.n, wd.cv, wd.se,
             .add = TRUE) %>%
    summarise_at(vars(starts_with("simu.")), sum, na.rm = TRUE) %>%
    rowwise() %>%
    mutate(agb.min = min(c_across(starts_with("simu.")), na.rm = TRUE),
           agb.q.025 = quantile(c_across(starts_with("simu.")), na.rm = TRUE, probs = .025),
           agb.q.25 = quantile(c_across(starts_with("simu.")), na.rm = TRUE, probs = .25),
           agb.median = median(c_across(starts_with("simu.")), na.rm = TRUE),
           agb.q.75 = quantile(c_across(starts_with("simu.")), na.rm = TRUE, probs = .25),
           agb.q.975 = quantile(c_across(starts_with("simu.")), na.rm = TRUE, probs = .975),
           agb.max = max(c_across(starts_with("simu.")), na.rm = TRUE),
           agb.mean = mean(c_across(starts_with("simu.")), na.rm = TRUE),
           agb.sd = sd(c_across(starts_with("simu.")), na.rm = TRUE),
           agb.n = sum(!is.na(c_across(starts_with("simu."))))) %>%
    select(-starts_with("simu.")) %>%
    ungroup() %>%
    mutate(agb.cv = agb.sd*100/agb.mean,
           agb.se = agb.sd/sqrt(agb.n))
  return(out)  
}

