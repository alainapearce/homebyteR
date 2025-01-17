#' util_redcap_parent2: Organize parent questionnaire data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from the second set of parent questionnaires
#'
#'
#' @inheritParams util_redcap_parent1
#' @inheritParams util_redcap_parent1
#'
#' @return If return_data is set to TRUE, will return a list including separate clean raw parent 2 datasets for each survey
#'
#' @examples
#'
#' # process REDCap data
#' parent2_data <- util_redcap_parent2(data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_redcap_parent2 <- function(data, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
  }


  ## FFQ Data ####
  #get data
  ffq_data <- data[, grepl('record_id', names(data)) | grepl('ffq', names(data))]
  ffq_data <- ffq_data[, !(names(ffq_data) %in% c('ffq1_check', 'ffq2_check', 'ffq3_check', 'ffq4_check', 'ffq5_check', 'ffq_sup_check'))]

  #update names
  names(ffq_data)[names(ffq_data) == 'record_id'] <- 'participant_id'
  names(ffq_data)[names(ffq_data) == 'ffq_milk'] <- 'ffq_dairy1'
  names(ffq_data)[names(ffq_data) == 'ffq_yogurt'] <- 'ffq_dairy2'
  names(ffq_data)[names(ffq_data) == 'ffq_prebiotic'] <- 'ffq_dairy3'
  names(ffq_data)[names(ffq_data) == 'ffq_cheese'] <- 'ffq_dairy4'
  names(ffq_data)[names(ffq_data) == 'ffq_egg'] <- 'ffq_egg1'
  names(ffq_data)[names(ffq_data) == 'ffq_poultry'] <- 'ffq_meat1'
  names(ffq_data)[names(ffq_data) == 'ffq_redmeat'] <- 'ffq_meat2'
  names(ffq_data)[names(ffq_data) == 'ffq_sausage'] <- 'ffq_meat3'
  names(ffq_data)[names(ffq_data) == 'ffq_ham'] <- 'ffq_meat4'
  names(ffq_data)[names(ffq_data) == 'ffq_leanfish'] <- 'ffq_fish1'
  names(ffq_data)[names(ffq_data) == 'ffq_fattyfish'] <- 'ffq_fish2'
  names(ffq_data)[names(ffq_data) == 'ffq_canfish'] <- 'ffq_fish3'
  names(ffq_data)[names(ffq_data) == 'ffq_seafood'] <- 'ffq_fish4'
  names(ffq_data)[names(ffq_data) == 'ffq_dairy_desert'] <- 'ffq_dairy5'
  names(ffq_data)[names(ffq_data) == 'ffq_raw_veg'] <- 'ffq_veg1'
  names(ffq_data)[names(ffq_data) == 'ffq_cook_veg'] <- 'ffq_veg2'
  names(ffq_data)[names(ffq_data) == 'ffq_potatoes'] <- 'ffq_potato1'
  names(ffq_data)[names(ffq_data) == 'ffq_legumes'] <- 'ffq_legume1'
  names(ffq_data)[names(ffq_data) == 'ffq_frenchfries'] <- 'ffq_potato2'
  names(ffq_data)[names(ffq_data) == 'ffq_fruits'] <- 'ffq_fruit1'
  names(ffq_data)[names(ffq_data) == 'ffq_fruitjuice'] <- 'ffq_fruit2'
  names(ffq_data)[names(ffq_data) == 'ffq_nuts'] <- 'ffq_nuts1'
  names(ffq_data)[names(ffq_data) == 'ffq_canfruit'] <- 'ffq_fruit3'
  names(ffq_data)[names(ffq_data) == 'ffq_dryfruit'] <- 'ffq_fruit4'
  names(ffq_data)[names(ffq_data) == 'ffq_whitebread'] <- 'ffq_cereal1'
  names(ffq_data)[names(ffq_data) == 'ffq_wheatbread'] <- 'ffq_cereal2'
  names(ffq_data)[names(ffq_data) == 'ffq_sugarcereal'] <- 'ffq_cereal3'
  names(ffq_data)[names(ffq_data) == 'ffq_othercereal'] <- 'ffq_cereal4'
  names(ffq_data)[names(ffq_data) == 'ffq_ricepasta'] <- 'ffq_cereal5'
  names(ffq_data)[names(ffq_data) == 'ffq_ricecakes'] <- 'ffq_cereal6'
  names(ffq_data)[names(ffq_data) == 'ffq_cookies'] <- 'ffq_bakery1'
  names(ffq_data)[names(ffq_data) == 'ffq_cakes'] <- 'ffq_bakery2'
  names(ffq_data)[names(ffq_data) == 'ffq_chocolate'] <- 'ffq_sweet1'
  names(ffq_data)[names(ffq_data) == 'ffq_sugar'] <- 'ffq_sweet2'
  names(ffq_data)[names(ffq_data) == 'ffq_sweets'] <- 'ffq_sweet3'
  names(ffq_data)[names(ffq_data) == 'ffq_soda'] <- 'ffq_bev1'
  names(ffq_data)[names(ffq_data) == 'ffq_dietsoda'] <- 'ffq_bev2'
  names(ffq_data)[names(ffq_data) == 'ffq_oliveoil'] <- 'ffq_fats1'
  names(ffq_data)[names(ffq_data) == 'ffq_otheroil'] <- 'ffq_fats2'
  names(ffq_data)[names(ffq_data) == 'ffq_butter'] <- 'ffq_fats3'
  names(ffq_data)[names(ffq_data) == 'ffq_margarine'] <- 'ffq_fats4'
  names(ffq_data)[names(ffq_data) == 'ffq_sauces'] <- 'ffq_dressing1'
  names(ffq_data)[names(ffq_data) == 'ffq_chips'] <- 'ffq_saltysnack1'

  #process data
  ffq_scored <- dataprepr::score_ffq_helix(ffq_data, base_zero = TRUE, id = 'participant_id')

  ## FMCB Data  ####
  #get data
  fmcb_data <- data[, grepl('record_id', names(data)) | grepl('fmcb', names(data))]

  #update names
  names(fmcb_data)[names(fmcb_data) == 'record_id'] <- 'participant_id'

  #process data
  fmcb_scored <- dataprepr::score_fmcb(fmcb_data, base_zero = TRUE, id = 'participant_id')

  ## SCPF Data  ####
  #get data
  scpf_data <- data[, grepl('record_id', names(data)) | grepl('scpf', names(data))]

  #update names
  names(scpf_data)[names(scpf_data) == 'record_id'] <- 'participant_id'

  #process data
  scpf_scored <- dataprepr::score_scpf(scpf_data, base_zero = TRUE, id = 'participant_id')

  ## FFBS Data ####
  #get data
  ffbs_data <- data[, grepl('record_id', names(data)) | grepl('ffbs', names(data))]

  #update names
  names(ffbs_data)[names(ffbs_data) == 'record_id'] <- 'participant_id'

  #process data
  ffbs_scored <- dataprepr::score_ffbs(ffbs_data, base_zero = TRUE, id = 'participant_id')

  ## HFE Data ####
  #get data
  hfe_data <- data[, grepl('record_id', names(data)) | grepl('hfe', names(data))]
  hfe_data <- hfe_data[, !(names(hfe_data) %in% c('hfe_fam_qcheck', 'hfe_p1_qcheck', 'hfe_p2_qcheck', 'hfe_p3_qcheck',  'hfe_p5_qcheck', 'hfe_p6_qcheck'))]

  #update names
  names(hfe_data)[names(hfe_data) == 'record_id'] <- 'participant_id'

  #process data
  hfe_scored <- dataprepr::score_nik_hfe(hfe_data, base_zero = TRUE, id = 'participant_id')

  ## PWLB Data ####
  #get data
  pwlb_data <- data[, grepl('record_id', names(data)) | grepl('pwlb', names(data))]
  pwlb_data <- pwlb_data[, !(names(pwlb_data) %in% c('pwlb_missingcheck'))]

  #update names
  names(pwlb_data)[names(pwlb_data) == 'record_id'] <- 'participant_id'

  #process data
  pwlb_scored <- dataprepr::score_pwlb(pwlb_data, base_zero = TRUE, id = 'participant_id')

  ## TFEQ Data ####
  #get data
  tfeq_data <- data[, grepl('record_id', names(data)) | grepl('tfeq', names(data))]

  #update names
  names(tfeq_data)[names(tfeq_data) == 'record_id'] <- 'participant_id'

  #process data
  tfeq_scored <- dataprepr::score_tfeq18(tfeq_data, base_zero = TRUE, id = 'participant_id')

  ## CFQ Data ####
  #get data
  cfq_data <- data[, grepl('record_id', names(data)) | grepl('cfq', names(data))]
  cfq_data <- cfq_data[, !(names(cfq_data) %in% c('cfq_presrest_missingcheck', 'cfq_mon_missingcheck'))]

  #update names
  names(cfq_data)[names(cfq_data) == 'record_id'] <- 'participant_id'

  #process data
  cfq_scored <- dataprepr::score_cfq(cfq_data, base_zero = TRUE, restriction_split = FALSE, id = 'participant_id', pcw_na_value = 5)

  ## compile and return data ####
  if (isTRUE(return_data)){
    return(list(
      ffq_data = ffq_scored,
      fmcb_data = fmcb_scored,
      scpf_data = scpf_scored,
      ffbs_data = ffbs_scored,
      hfe_data = hfe_scored,
      pwlb_data = pwlb_scored,
      tfeq_data = tfeq_scored,
      cfq_data = cfq_scored))
  }
}

