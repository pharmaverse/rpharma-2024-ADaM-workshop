# Helper program to check and install packages

check_and_install <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    } else {
      message(paste(package, "already installed"))
    }
  }
  message("Let's program some ADaMs!")
}

# Use the function
check_and_install(c(
  "dplyr", "lubridate", "stringr", "metacore", "metatools",
  "xportr", "pharmaversesdtm", "xportr", "admiral"
))
