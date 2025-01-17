#' util_redcap_parent1: Organize parent questionnaire data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from the first set of parent questionnaires
#'
#'
#' @param data data from REDCap event parent_visit_1_arm_1
#' @param return_data return organized data (default = TRUE)
#'
#' @return If return_data is set to TRUE, will return a list including separate clean raw parent 1 datasets for each survey
#'
#' @examples
#'
#' # process REDCap data
#' parent1_data <- util_redcap_parent1(data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_redcap_parent1 <- function(data, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
  }


  ## Sleep Week data ####
  #get data
  sleep_data <- data[, grepl('record_id', names(data)) | grepl('^date_', names(data)) | grepl('^bedtime_', names(data)) | grepl('^attempt_', names(data)) | grepl('^asleep_', names(data)) | grepl('^times_', names(data)) | grepl('^waso_', names(data)) | grepl('^awake_', names(data)) | grepl('^out_', names(data))]

  #update names
  names(sleep_data)[names(sleep_data) == 'record_id'] <- 'participant_id'

  #process data
  sleep_data <- sleep_data[!rowSums(is.na(sleep_data) | sleep_data == "") == ncol(sleep_data)-1, ]

  sleep_wk_scored <- dataprepr::score_sleeplog(sleep_data, id = 'participant_id', summer_start = '2024-06-06', summer_end = '2024-08-23')

  ## demographics data ####
  #get data
  household_data <- data[, grepl('record_id', names(data)) | grepl('demo', names(data))]
  household_data <- household_data[, !(names(household_data) %in% c('demographics_timestamp', 'demo_v1_missingcheck', 'demographics_complete', 'demo_missingcheck_2'))]

  #update names
  names(household_data)[names(household_data) == 'record_id'] <- 'participant_id'
  names(household_data)[names(household_data) == 'demo_race___0'] <- 'demo_race_AmericanIndian_AlaskanNative'
  names(household_data)[names(household_data) == 'demo_race___1'] <- 'demo_race_Asian'
  names(household_data)[names(household_data) == 'demo_race___2'] <- 'demo_race_Black_AfricanAmerican'
  names(household_data)[names(household_data) == 'demo_race___3'] <- 'demo_race_White'
  names(household_data)[names(household_data) == 'demo_race___4'] <- 'demo_race_Hawaiian_PacificIslander'
  names(household_data)[names(household_data) == 'demo_race___5'] <- 'demo_race_Other'
  names(household_data)[names(household_data) == 'demo_child_other_race'] <- 'demo_race_Other_txt'
  names(household_data)[names(household_data) == 'demo_mod_ed'] <- 'demo_mom_ed'
  names(household_data)[names(household_data) == 'demo_programs___0'] <- 'demo_programs_none'
  names(household_data)[names(household_data) == 'demo_programs___1'] <- 'demo_programs_SNAP'
  names(household_data)[names(household_data) == 'demo_programs___2'] <- 'demo_programs_WIC'
  names(household_data)[names(household_data) == 'demo_programs___3'] <- 'demo_programs_TNAF'
  names(household_data)[names(household_data) == 'demo_programs___4'] <- 'demo_programs_Medicaid'
  names(household_data)[names(household_data) == 'demo_programs___5'] <- 'demo_programs_LIHEAP'
  names(household_data)[names(household_data) == 'demo_programs___6'] <- 'demo_programs_NSLP_partial'
  names(household_data)[names(household_data) == 'demo_programs___7'] <- 'demo_programs_NSLP_full'
  names(household_data)[names(household_data) == 'demo_programs___8'] <- 'demo_programs_other'
  names(household_data)[names(household_data) == 'demo_programs_other'] <- 'demo_programs_other_txt'

  ## Puberty Data ####
  #get data
  puberty_data <- data[, grepl('record_id', names(data)) | grepl('pds', names(data))]

  #update names
  names(puberty_data)[names(puberty_data) == 'record_id'] <- 'participant_id'
  names(puberty_data)[names(puberty_data) == 'pds_sex'] <- 'sex'

  #re-code sex to match demo_c_sex
  puberty_data[['sex']] <- ifelse(puberty_data[['sex']] == 0, 1, 0)

  #process data
  puberty_scored <- dataprepr::score_pds(puberty_data, base_zero = FALSE, respondent = 'parent', male = 0, female = 1, id = 'participant_id')

  #add tanner data
  parent_tanner_dat <- as.data.frame(puberty_scored$bids_phenotype[['participant_id']])
  names(parent_tanner_dat) <- 'participant_id'
  parent_tanner_dat['parent_tanner'] <- sapply(parent_tanner_dat[['participant_id']], function(x) ifelse(data[data['record_id'] == x, 'demo_c_sex'] == 0, data[data['record_id'] == x, 'tanner_m'], data[data['record_id'] == x, 'tanner_f']), USE.NAMES = TRUE)

  puberty_scored$bids_phenotype <- merge(puberty_scored$bids_phenotype, parent_tanner_dat, by = 'participant_id')
  puberty_scored$score_dat <- merge(puberty_scored$score_dat, parent_tanner_dat, by = 'participant_id')

  ## EFCR Data ####
  #get data
  efcr_data <- data[, grepl('record_id', names(data)) | grepl('efcr', names(data))]
  efcr_data <- efcr_data[, !(names(efcr_data) %in% c('efcr_missingcheck'))]

  #update names
  names(efcr_data)[names(efcr_data) == 'record_id'] <- 'participant_id'

  #process data
  efcr_scored <- dataprepr::score_efcr(efcr_data, base_zero = FALSE, id = 'participant_id')

  ## SPSRQ Data ####
  #get data
  spsrq_data <- data[, grepl('record_id', names(data)) | grepl('spsrq', names(data))]
  spsrq_data <- spsrq_data[, !(names(spsrq_data) %in% c('spsrq_missingcheck'))]

  #update names
  names(spsrq_data)[names(spsrq_data) == 'record_id'] <- 'participant_id'

  #process data
  spsrq_scored <- dataprepr::score_spsrq(spsrq_data, base_zero = TRUE, id = 'participant_id')

  ## HCQ Data ####
  #get data
  hcq_data <- data[, grepl('record_id', names(data)) | grepl('hcq', names(data))]

  #update names
  names(hcq_data)[names(hcq_data) == 'record_id'] <- 'participant_id'

  ## CHSQ data ####
  #get data
  cshq_data <- data[, grepl('record_id', names(data)) | grepl('cshq', names(data))]
  cshq_data <- cshq_data[, !(names(cshq_data) %in% c('cshq_missingcheck'))]

  #update names
  names(cshq_data)[names(cshq_data) == 'record_id'] <- 'participant_id'

  #process data
  cshq_scored <- dataprepr::score_cshq(cshq_data, base_zero = FALSE, id = 'participant_id', reverse_score = TRUE)

  ## CEBQ Data ####
  #get data
  cebq_data <- data[, grepl('record_id', names(data)) | grepl('cebq', names(data))]
  cebq_data <- cebq_data[, !(names(cebq_data) %in% c('cebq_missingcheck'))]

  #update names
  names(cebq_data)[names(cebq_data) == 'record_id'] <- 'participant_id'

  #process data
  cebq_scored <- dataprepr::score_cebq(cebq_data, base_zero = TRUE, id = 'participant_id')

  ## compile and return data ####
  if (isTRUE(return_data)){
    return(list(
      demo_data = household_data,
      sleeplog_data = sleep_data,
      puberty_data = puberty_scored,
      efcr_data = efcr_scored,
      spsrq_data = spsrq_scored,
      hcq_data = hcq_data,
      cshq_data = cshq_scored,
      cebq_data = cebq_scored))
  }
}

