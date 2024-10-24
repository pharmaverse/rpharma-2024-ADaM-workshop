# Name: ADVS
#
# Label: Vital Signs Analysis Dataset
#
# If not using posit.cloud - run this script to install packages
# source("exercises/setup.R")
#
# Input: adsl, vs
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
  where_sep_sheet = FALSE,
  quiet = TRUE
) %>%
  select_dataset("ADVS")


# ---- Load User-defined function & Lookup tables----

source("exercises/adams_little_helpers.R")

# Load source datasets ----

# Use e.g. `haven::read_sas()` to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

vs <- pharmaversesdtm::vs
# If you missed previous ADSL ADaM development, just load the XPT file
adsl <- haven::read_xpt("datasets/adsl.xpt")

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values # nolint

vs <- convert_blanks_to_na(vs)

# Derivations ----

# Get list of ADSL vars required for derivations
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P)

advs_0 <- vs %>%
  # Join ADSL with VS (need TRTSDT for ADY derivation)
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  ## Calculate ADT, ADY ----
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = VSDTC,
    # Below arguments are default values and not necessary to add in our case
    highest_imputation = "n", # means no imputation is performed on partial/missing dates
    flag_imputation = "auto", # To automatically create ADTF variable when highest_imputation is "Y", "M" or "D"
  ) %>%
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = exprs(ADT)
  )

# View(advs_0 %>% select(STUDYID, USUBJID, VISIT, VISITNUM, VSTESTCD, VSTEST, VSDTC, !!!adsl_vars, ADT, ADY))

advs_1 <- advs_0 %>%
  ## Add PARAMCD only - add PARAM etc later ----
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD),
    by_vars = exprs(VSTESTCD),
    # Below arguments are default values and not necessary to add in our case
    print_not_mapped = TRUE # Printing whether some parameters are not mapped
  ) %>%
  ## Calculate AVAL and AVALU ----
  mutate(
    AVAL = VSSTRESN,
    AVALU = VSSTRESU
  ) %>%
  ## Derive new parameters based on existing records ----
  ## using wrapper functions for the more generic derive_param_computed()
  # Note that, for the following three `derive_param_*()` functions, only the
  # variables specified in `by_vars` will be populated in the newly created
  # records.

  # Derive Mean Arterial Pressure
  derive_param_map(
    by_vars = exprs(STUDYID, USUBJID, !!!adsl_vars, VISIT, VISITNUM, ADT, ADY, VSTPT, VSTPTNUM, AVALU), # Other variables than the defined ones here won't be populated
    set_values_to = exprs(PARAMCD = "MAP"),
    get_unit_expr = VSSTRESU,
    filter = VSSTAT != "NOT DONE" | is.na(VSSTAT),
    # Below arguments are default values and not necessary to add in our case
    sysbp_code = "SYSBP",
    diabp_code = "DIABP",
    hr_code = NULL
  )
  ## Exercise n1 ----
  
  ### Derive Body Mass Index ----
  ### Have a look to {admiraldiscovery}(https://pharmaverse.github.io/admiraldiscovery/articles/reactable.html)
  ### Which function could be used to derive "BMI" parameter?
  # ---- ??? ---- #
  
  ## Exercise n2 ----
  
  ### Derive Body Surface Area ----
  ### Have a look to {admiraldiscovery}(https://pharmaverse.github.io/admiraldiscovery/articles/reactable.html)
  ### Which wrapper function could be used to derive "BSA" parameter?
  # ---- ??? ---- #

# View(advs_1 %>% select(STUDYID, USUBJID, VISIT, VISITNUM, VSTESTCD, VSTEST, VSSTRESN, VSSTRESU, VSDTC, VSSTAT, ADT, ADY,  PARAMCD, AVAL, AVALU))

## Get visit info ----
# See also the "Visit and Period Variables" vignette
# (https://pharmaverse.github.io/admiral/articles/visits_periods.html#visits)
advs_2 <- advs_1 %>%
  # Derive Timing
  mutate(
    ATPTN = VSTPTNUM,
    ATPT = VSTPT,
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN|UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_,
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = as.numeric(case_when(
      VISIT == "BASELINE" ~ "0",
      str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", "")),
      TRUE ~ NA_character_
    ))
  )

# View(advs_2 %>% select(STUDYID, USUBJID, VISIT, VISITNUM, VSTESTCD, VSTEST, VSSTRESN, VSSTRESU, VSDTC, VSSTAT, ADT, ADY, PARAMCD, AVAL, AVALU, ATPTN, ATPT, AVISIT, AVISITN))

## Derive a new record as a summary record (e.g. mean of the triplicates at each time point) ----
# e.g. subject ="01-701-1015", PARAMCD ="DIABP", AVISIT = "Baseline", ADT="2014-01-02" -> Mean = 56
advs_3 <- advs_2 %>%
  derive_summary_records(
    dataset_add = advs_2, # Observations from the specified dataset are going to be used to calculate and added as new records to the input dataset.
    by_vars = exprs(STUDYID, USUBJID, !!!adsl_vars, PARAMCD, AVISITN, AVISIT, ADT, ADY, AVALU),
    filter_add = !is.na(AVAL),
    set_values_to = exprs(
      AVAL = mean(AVAL),
      DTYPE = "AVERAGE"
    )
  )

# View(advs_3 %>% select(STUDYID, USUBJID, VISIT, VISITNUM, VSTESTCD, VSTEST, VSSTRESN, VSSTRESU, VSDTC, VSSTAT, ADT, ADY, PARAMCD, AVAL, AVALU, ATPTN, ATPT, AVISIT, AVISITN, DTYPE))

advs_4 <- advs_3 %>%
  ## Exercise n3 ----

  ### Calculate ONTRTFL ----
  ### With the help of {admiraldiscovery}(https://pharmaverse.github.io/admiraldiscovery/articles/reactable.html)
  ### Which function could be used to derive ONTRTFL variable?
  # ---- ??? ---- #

  # View(advs_4 %>% select(STUDYID, USUBJID, VISIT, VISITNUM, VSSTAT, PARAMCD, DTYPE, AVAL, AVALU, AVISIT, AVISITN, ADT, ADY, TRTSDT, TRTEDT, ONTRTFL))

## Calculate ANRIND : requires the reference ranges ANRLO, ANRHI ----
# Also accommodates the ranges A1LO, A1HI
advs_5 <- advs_4 %>%
  derive_vars_merged(
    dataset_add = range_lookup, # derive_vars_merged() already used in previous steps
    by_vars = exprs(PARAMCD)
  ) %>%
  # Calculate ANRIND
  derive_var_anrind(
    # Below arguments are default values and not necessary to add in our case
    # signif_dig = get_admiral_option("signif_digits"),
    # use_a1hia1lo = FALSE
  )

# View(advs_5 %>% select(STUDYID, USUBJID, VISIT, VISITNUM, VSTESTCD, VSTEST, VSSTRESN, VSSTRESU, VSDTC, VSSTAT, ADT, ADY, PARAMCD, AVAL, AVALU, ATPTN, ATPT, AVISIT, AVISITN, DTYPE, ANRLO, ANRHI, A1LO, A1HI, ANRIND))

## Derive baseline flags ----
advs_6 <- advs_5 %>%
  # Calculate BASETYPE
  derive_basetype_records(
    basetypes = exprs(
      "LAST: AFTER LYING DOWN FOR 5 MINUTES" = ATPTN == 815,
      "LAST: AFTER STANDING FOR 1 MINUTE" = ATPTN == 816,
      "LAST: AFTER STANDING FOR 3 MINUTES" = ATPTN == 817,
      "LAST" = is.na(ATPTN)
    )
  ) %>%
  # Calculate ABLFL
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID, BASETYPE, PARAMCD),
      order = exprs(ADT, VISITNUM, VSSEQ),
      new_var = ABLFL,
      mode = "last", # Determines of the first or last observation is flagged
      # Below arguments are default values and not necessary to add in our case
      true_value = "Y"
    ),
    filter = (!is.na(AVAL) &
      ADT <= TRTSDT & !is.na(BASETYPE) & is.na(DTYPE))
  )

# View(advs_6 %>% select(STUDYID, USUBJID, VISIT, VISITNUM, VSTESTCD, VSTEST, VSSTRESN, VSSTRESU, VSDTC, VSSTAT, ADT, ADY, PARAMCD, AVAL, AVALU, ATPTN, ATPT, AVISIT, AVISITN, DTYPE, ONTRTFL, BASETYPE, ABLFL))

## Derive baseline information ----
advs_7 <- advs_6 %>%
  # Calculate BASE
  derive_var_base(
    by_vars = exprs(STUDYID, USUBJID, PARAMCD, BASETYPE),
    source_var = AVAL,
    new_var = BASE,
    # Below arguments are default values and not necessary to add in our case
    filter = ABLFL == "Y"
  ) %>%
  # Calculate BNRIND
  derive_var_base(
    by_vars = exprs(STUDYID, USUBJID, PARAMCD, BASETYPE),
    source_var = ANRIND,
    new_var = BNRIND
  ) %>%
  # Calculate CHG - Note that CHG is populated for both Baseline & Post-Baseline records
  derive_var_chg() %>%
  ## Exercise n4 ----

  ### Calculate PCHG ----
  ### only for Post-Baseline records: which functions to use?
  # ---- ??? ---- #

# View(advs_7 %>% select(STUDYID, USUBJID, VISIT, VISITNUM, VSTESTCD, VSTEST, VSSTRESN, VSSTRESU, VSDTC, VSSTAT, ADT, ADY, PARAMCD, AVAL, AVALU, AVISIT, AVISITN, DTYPE, ONTRTFL, BASETYPE, ABLFL, ANRIND, BNRIND, BASE, CHG, PCHG))

## ANL01FL: Flag last result within an AVISIT and ATPT for post-baseline records ----
advs_8 <- advs_7 %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      new_var = ANL01FL,
      by_vars = exprs(STUDYID, USUBJID, PARAMCD, AVISIT, ATPT, DTYPE),
      order = exprs(ADT, AVAL),
      mode = "last", # Determines of the first or last observation is flagged - As seen while deriving ABLFL
      # Below arguments are default values and not necessary to add in our case
      true_value = "Y"
    ),
    filter = !is.na(AVISITN) & ONTRTFL == "Y"
  )

# View(advs_8 %>% select(STUDYID, USUBJID, VISIT, VISITNUM, VSTESTCD, VSTEST, VSSTRESN, VSSTRESU, VSDTC, VSSTAT, ADT, ADY, PARAMCD, AVAL, AVALU, AVISIT, AVISITN, ATPT, DTYPE, ONTRTFL, ABLFL, ANL01FL))

## Get treatment information ----
advs_9 <- advs_8 %>%
  # Assign TRTA, TRTP
  mutate(
    TRTP = TRT01P,
    TRTA = TRT01A
  )

# View(advs_9 %>% select(STUDYID, USUBJID, VISIT, VISITNUM, VSTESTCD, VSTEST, VSSTRESN, VSSTRESU, VSDTC, VSSTAT, ADT, ADY, PARAMCD, AVAL, AVALU, AVISIT, AVISITN, ATPT, DTYPE, ONTRTFL, ABLFL, ANL01FL, TRTP, TRTA))

## Get ASEQ and AVALCATx and add PARAM/PARAMN ----
advs_10 <- advs_9 %>%
  ## Exercise n5 ----

  ### Calculate ASEQ ----
  ## With the help of the Reference page (https://pharmaverse.github.io/admiral/reference/)
  ## Which function could be used to Number the Observations Within Each Group
  # ---- ??? ---- # %>%

  # Derive AVALCA1N and AVALCAT1
  ## Using Format functions from source("exercises/adams_little_helpers.R")
  mutate(AVALCA1N = format_avalcat1n(param = PARAMCD, aval = AVAL)) %>%
  derive_vars_merged(
    dataset_add = avalcat_lookup,
    by_vars = exprs(PARAMCD, AVALCA1N)
  ) %>%
  # Derive PARAM and PARAMN
  # derive_vars_merged(dataset_add = select(param_lookup, -VSTESTCD), by_vars = exprs(PARAMCD))
  # Note that PARAM/PARAMN could also be derived using the metatools package, as seen during ADSL development
  # with the function create_var_from_codelist()
  # (https://pharmaverse.github.io/metatools/reference/create_var_from_codelist.html)
  create_var_from_codelist(metacore,
    input_var = PARAMCD,
    out_var = PARAM,
    decode_to_code = FALSE # input_var is the code column of the codelist
  ) %>%
  create_var_from_codelist(metacore,
    input_var = PARAMCD,
    out_var = PARAMN
  )

# View(advs_10 %>% select(STUDYID, USUBJID, VISIT, VISITNUM, VSTESTCD, VSTEST, VSSTRESN, VSSTRESU, VSDTC, VSSTAT, ADT, ADY, AVAL, AVALU, AVISIT, AVISITN, ATPT, DTYPE, ONTRTFL, ABLFL, ANL01FL, TRTP, TRTA, AVALCAT1, AVALCA1N, PARAMCD, PARAM, PARAMN, ASEQ))

# Add all ADSL variables
advs_final <- advs_10 %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = exprs(STUDYID, USUBJID)
  )

# Final Steps, Select final variables and Add labels
advs <- advs_final %>%
  drop_unspec_vars(metacore) %>% # Drop unspecified variables from specs
  check_variables(metacore, dataset_name = "ADVS") %>% # Check all variables specified are present and no more
  order_cols(metacore) %>% # Orders the columns according to the spec
  sort_by_key(metacore) %>% # Sorts the rows by the sort keys
  xportr_type(metacore) %>%
  xportr_length(metacore) %>%
  xportr_label(metacore) %>%
  xportr_df_label(metacore, domain = "ADVS") %>%
  xportr_write("datasets/advs.xpt", metadata = metacore, domain = "ADVS")
