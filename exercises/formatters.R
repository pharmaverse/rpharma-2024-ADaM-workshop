# User defined functions ----
# Here are some examples of how you can create your own functions that
#  operates on vectors, which can be used in `mutate`.

# Race Grouping ----
format_racegr1 <- function(x) {
  case_when(
    x == "WHITE" ~ "White",
    x != "WHITE" ~ "Non-white",
    TRUE ~ "Missing"
  )
}

# Age Grouping ----
format_agegr1 <- function(x) {
  case_when(
    x < 18 ~ "<18",
    between(x, 18, 64) ~ "18-64",
    x > 64 ~ ">64",
    TRUE ~ "Missing"
  )
}

# Region Grouping ----
format_region1 <- function(x) {
  case_when(
    x %in% c("CAN", "USA") ~ "NA",
    !is.na(x) ~ "RoW",
    TRUE ~ "Missing"
  )
}

# EOSSTT mapping ----
format_eosstt <- function(x) {
  case_when(
    x %in% c("COMPLETED") ~ "COMPLETED",
    x %in% c("SCREEN FAILURE") ~ NA_character_,
    !is.na(x) ~ "DISCONTINUED",
    TRUE ~ "ONGOING"
  )
}

# Assign Randomization Flag

assign_randfl <- function(x) {
  if_else(!is.na(x), "Y", NA_character_)
}
  
  
  
  
  
  
  
  