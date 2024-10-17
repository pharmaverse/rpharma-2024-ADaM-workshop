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

# Assign Randomization Flag ----
assign_randfl <- function(x) {
  if_else(!is.na(x), "Y", NA_character_)
}

# DTHCGR1 mapping
format_dthcgr1 <- function(x, y){
  case_when(
  is.na(x) ~ NA_character_,
  x == "AE" ~ "ADVERSE EVENT",
  str_detect(y, "(PROGRESSIVE DISEASE|DISEASE RELAPSE)") ~ "PROGRESSIVE DISEASE",
  TRUE ~ "OTHER")
}
  
# AVALCAT1N mapping ----  
format_avalcat1n <- function(param, aval) {
  case_when(
    param == "HEIGHT" & aval > 140 ~ 1,
    param == "HEIGHT" & aval <= 140 ~ 2,
    TRUE ~ NA
  )
} 

# Lookup tables for ADVS
# Lookup tables ----

# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~VSTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "SYSBP", "SYSBP", "Systolic Blood Pressure (mmHg)", 1,
  "DIABP", "DIABP", "Diastolic Blood Pressure (mmHg)", 2,
  "PULSE", "PULSE", "Pulse Rate (beats/min)", 3,
  "WEIGHT", "WEIGHT", "Weight (kg)", 4,
  "HEIGHT", "HEIGHT", "Height (cm)", 5,
  "TEMP", "TEMP", "Temperature (C)", 6,
  "MAP", "MAP", "Mean Arterial Pressure (mmHg)", 7,
  "BMI", "BMI", "Body Mass Index(kg/m^2)", 8,
  "BSA", "BSA", "Body Surface Area(m^2)", 9
)

# Assign ANRLO/HI, A1LO/HI
range_lookup <- tibble::tribble(
  ~PARAMCD, ~ANRLO, ~ANRHI, ~A1LO, ~A1HI,
  "SYSBP", 90, 130, 70, 140,
  "DIABP", 60, 80, 40, 90,
  "PULSE", 60, 100, 40, 110,
  "TEMP", 36.5, 37.5, 35, 38
)

# ASSIGN AVALCAT1
avalcat_lookup <- tibble::tribble(
  ~PARAMCD, ~AVALCA1N, ~AVALCAT1,
  "HEIGHT", 1, ">140 cm",
  "HEIGHT", 2, "<= 140 cm"
)
  