library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)

data("dm")
data("ds")
data("ex")
data("ae")

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)

# 1) Create a new dataset “ds_ext” with all of “ds”, plus derive a numeric date
# version of DS.DSSTDTC without any imputation as DSSTDT.
# Hint: use derive_vars_dt()

??derive_vars_dt

ds_ext <- ds %>%
  derive_vars_dt(
    dtc = DSSTDTC,
    new_vars_prefix = "DSST"
  )

class(ds_ext$DSSTDTC)
class(ds_ext$DSSTDT)

# 2) Create a new dataset “adsl_1” with all of “dm”, plus derive an end of study
# date (EOSDT) using the DSSTDT from “ds_ext” where DSCAT is "DISPOSITION EVENT"
# & DSDECOD is NOT "SCREEN FAILURE".
# Hint: use derive_vars_merged()

adsl_1 <- dm %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )

# 3) Create a new dataset “adsl_2” with all of “adsl_1”, plus derive a safety
# population flag (SAFFL) as “Y” for any patient from “ex” with a record where
# EXDOSE > 0.
# Hint: use derive_var_merged_exist_flag()

adsl_2 <- adsl_1 %>%
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = EXDOSE > 0
  )

# Note that in practice you'd probably add an extra condition for placebo patients

# 4) Create a new dataset “adsl_3” with all of “adsl_2”, plus derive a last
# known alive date (LSTALVDT) as the latest date of the following variables from
# “ae”: AESTDTC and AEENDTC, imputing missing day and month to the first.
# Hint: use derive_var_extreme_dt()

ae_start_date <- date_source(
  dataset_name = "ae",
  date = convert_dtc_to_dt(AESTDTC, highest_imputation = "M")
)

ae_end_date <- date_source(
  dataset_name = "ae",
  date = convert_dtc_to_dt(AEENDTC, highest_imputation = "M")
)

adsl_3 <- adsl_2 %>%
  derive_var_extreme_dt(
    new_var = LSTALVDT,
    ae_start_date, ae_end_date,
    source_datasets = list(ae = ae),
    mode = "last"
  )

print(ae_start_date)

# BONUS) If time permits, explore the higher order functions that give greater
# flexibility to all {admiral} functions. 


