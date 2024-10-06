# Name: ADSL
#
# Label: Subject Level Analysis Dataset
#
# Input: dm, ds, ex, ae, lb, suppdm, rphrama_specs.xlsx
library(admiral)
library(pharmaversesdtm) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)
library(metacore)
library(metatools)
library(xportr)

# ---- Load Specs for Metacore ----
metacore <- spec_to_metacore(
  path = "metadata/rpharma_specs.xlsx",
  where_sep_sheet = FALSE
) %>%
  select_dataset("ADSL")

# ---- Load User-defined function ----
source("exercises/formatters.R")

# ---- Load source datasets ----
# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in sdtm test data
dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
lb <- pharmaversesdtm::lb
vs <- pharmaversesdtm::vs
suppdm <- pharmaversesdtm::suppdm

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
lb <- convert_blanks_to_na(lb)
vs <- convert_blanks_to_na(vs)
suppdm <- convert_blanks_to_na(suppdm)

# Combine Parent and Supp - very handy! ----
dm_suppdm <- combine_supp(dm, suppdm)

# Derivations ----
# impute start and end time of exposure to first and last respectively, do not impute date
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    time_imputation = "last",
    flag_imputation = "time"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last",
    flag_imputation = "time"
  )

# Derive treatment start date (TRTSDTM, TRT01P, TRT01A) ----
adsl01 <- dm_suppdm %>%
  mutate(TRT01P = ARM, TRT01A = ACTARM) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  )

# Derive treatment end date (TRTEDTM) ----
adsl02 <- adsl01 %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

# Derive treatment end/start date (TRTSDT/TRTEDT) ----
adsl03 <- adsl02 %>%
  derive_vars_dtm_to_dt(
    source_vars = exprs(TRTSDTM, TRTEDTM)
  )

# Derive treatment start time (TRTSDTM) ----
adsl04 <- adsl03 %>%
  derive_vars_dtm_to_tm(
    source_vars = exprs(TRTSDTM)
  )

# Derive treatment duration (TRTDURD) ----
adsl05 <- adsl04 %>%
  derive_var_trtdurd(
    start_date = TRTSDT,
    end_date = TRTEDT
  )

# Disposition dates, status ----
# convert character date to numeric date without imputation
ds_ext <- derive_vars_dt(
  ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

# Screen fail date (SCRFDT) ----
adsl06 <- adsl05 %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(SCRFDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD == "SCREEN FAILURE"
  )

# End of Study Date (EOSDT) ----
adsl07 <- adsl06 %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )

# End of Study Status (EOSSTT) ----
adsl08 <- adsl07 %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = DSCAT == "DISPOSITION EVENT",
    new_vars = exprs(EOSSTT = format_eosstt(DSDECOD)),
    missing_values = exprs(EOSSTT = "ONGOING")
  )

# Last Retrieval Date (FRVDT) ----
adsl09 <- adsl08 %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(FRVDT = DSSTDT),
    filter_add = DSCAT == "OTHER EVENT" & DSDECOD == "FINAL RETRIEVAL VISIT"
  )

# Derive Randomization Date (RANDDT) ----
adsl10 <- adsl09 %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    filter_add = DSDECOD == "RANDOMIZED",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(RANDDT = DSSTDT)
  )

# Death date - impute partial date to first day/month (DTHDT) ----
adsl11 <- adsl10 %>%
  derive_vars_dt(
    new_vars_prefix = "DTH",
    dtc = DTHDTC,
    highest_imputation = "M",
    date_imputation = "first"
  )

# Relative Day of Death (DTHADY) ----
adsl12 <- adsl11 %>%
  derive_vars_duration(
    new_var = DTHADY,
    start_date = TRTSDT,
    end_date = DTHDT
  )

# Elapsed Days from Last Dose to Death (LDDTHELD) ----
adsl13 <- adsl12 %>%
  derive_vars_duration(
    new_var = LDDTHELD,
    start_date = TRTEDT,
    end_date = DTHDT,
    add_one = FALSE
  )

# Cause of Death and Traceability Variables (DTHCAUS, DTHDOM) ----
adsl14 <- adsl13 %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "ae",
        condition = AEOUT == "FATAL",
        set_values_to = exprs(DTHCAUS = AEDECOD, DTHDOM = DOMAIN),
      ),
      event(
        dataset_name = "ds",
        condition = DSDECOD == "DEATH" & grepl("DEATH DUE TO", DSTERM),
        set_values_to = exprs(DTHCAUS = DSTERM, DTHDOM = DOMAIN),
      )
    ),
    source_datasets = list(ae = ae, ds = ds),
    tmp_event_nr_var = event_nr,
    order = exprs(event_nr),
    mode = "first",
    new_vars = exprs(DTHCAUS = DTHCAUS, DTHDOM = DTHDOM)
  )

# Death Cause Category (DTHCGR1) ----
adsl15 <- adsl14 %>%
  mutate(DTHCGR1 = case_when(
    is.na(DTHDOM) ~ NA_character_,
    DTHDOM == "AE" ~ "ADVERSE EVENT",
    str_detect(DTHCAUS, "(PROGRESSIVE DISEASE|DISEASE RELAPSE)") ~ "PROGRESSIVE DISEASE",
    TRUE ~ "OTHER"
  ))

# Groupings variables (RACEGR1, AGEGR1, REGION1) ----
## Using Format functions are from source("exercises/formatters.R") ----
adsl16 <- adsl15 %>%
  mutate(
    RACEGR1 = format_racegr1(RACE),
    AGEGR1 = format_agegr1(AGE),
    REGION1 = format_region1(COUNTRY),
    HEIGHTBL = "",
    WEIGHTBL = "",
    BMIBL = ""
  )

# Flagging variables (RANDFL, ITTFL, SAFFL) ----
## Using assign functions from source("exercises/formatters.R") ----
adsl17 <- adsl16 %>%
  mutate(
    RANDFL = assign_randfl(RANDDT)
  ) %>%
  rename(
    ITTFL = ITT,
    SAFFL = SAFETY
  )

# vs_ <- vs %>% 
#   rename(
#   PARAMCD = VSTESTCD,
#   AVAL = VSSTRESN,
#   ABLFL = VSBLFL
# )
# 
# derive_vars_computed(
#   dataset = adsl17,
#   dataset_add = vs_,
#   by_vars = exprs(STUDYID, USUBJID),
#   parameters = c("WEIGHT", "HEIGHT"),
#   new_vars = exprs(BMIBL = compute_bmi(height = AVAL.HEIGHT, weight = AVAL.WEIGHT)),
#   filter_add = ABLFL == "Y"
# )

# Numeric Variables are from Spec File ----
## (AGEGR1N, RACEN, RACEGR1N, REGION1N, TRT01PN, TRT01AN)
adsl18 <- adsl17 %>%
  create_var_from_codelist(metacore, input_var = AGEGR1, out_var = AGEGR1N) %>%
  create_var_from_codelist(metacore, input_var = RACE, out_var = RACEN) %>%
  create_var_from_codelist(metacore, input_var = RACEGR1, out_var = RACEGR1N) %>%
  create_var_from_codelist(metacore, input_var = REGION1, out_var = REGION1N) %>%
  create_var_from_codelist(metacore, input_var = TRT01P, out_var = TRT01PN) %>%
  create_var_from_codelist(metacore, input_var = TRT01A, out_var = TRT01AN)


adsl <- adsl17 %>%
  drop_unspec_vars(metacore) %>% # Drop unspecified variables from specs
  check_variables(metacore, dataset_name = "ADSL") %>% # Check all variables specified are present and no more
  order_cols(metacore) %>% # Orders the columns according to the spec
  sort_by_key(metacore) %>% # Sorts the rows by the sort keys
  xportr_type(metacore) %>%
  xportr_length(metacore) %>%
  xportr_label(metacore) %>%
  # xportr_format(metacore, domain = "ADSL") %>%
  xportr_df_label(metacore, domain = "ADSL") %>%
  xportr_write("datasets/adsl.xpt", metacore, domain = "ADSL")
