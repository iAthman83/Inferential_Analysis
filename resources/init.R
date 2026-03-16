###############################################################################
# loading packages, functions, pre-defined list of variables and KOBO tool
###############################################################################

#-------------------------------------------------------------------------------
# load packages
#-------------------------------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  devtools,
  tidyverse,
  readxl,
  writexl,
  openxlsx,
  randomcoloR,
  anytime,
  qdapRegex,
  sf,
  leaflet,
  leaflet.extras,
  crayon,
  listr,
  zip,
  httr,
  glue,
  googlesheets4,
  robotoolbox,
  shiny
)

options(scipen = 999) # to prevent scientific notation
options(dplyr.summarise.inform = FALSE)
