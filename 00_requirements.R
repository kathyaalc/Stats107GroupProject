# 00_requirements.R
# Installs (if missing) and loads every package used in the analysis.

required_packages <- c("tidyverse", "knitr", "scales")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
