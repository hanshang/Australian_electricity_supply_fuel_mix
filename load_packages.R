################
# Package names
################

#devtools::install_github("mpascariu/MortalityForecast")

packages <- c("hts", "ftsa", "compositions", "Compositional", "LaplacesDemon", "dplyr", "flexmix")

# Install packages not yet installed

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading

invisible(lapply(packages, library, character.only = TRUE))
