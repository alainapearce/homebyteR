#' proc_redcap: Process raw data downloaded from HOME Bytes REDCap
#'
#' This function:
#' 1) reads REDCap data from rawdata/redcap
#' 2) cleans data to save in Frictionless Data Package format. Produces the following files:
#'    *
#' 3) calls metadata function to create datapackage.json file
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#'
#' @param survey_data_path full path to the redcap visit data in sourcedata directory
#' @param data_de_path full path to the redcap double-entry data in sourcedata directory
#' @param overwrite overwrite existing files (default = FALSE)
#' @param return_data return phenotype to console (default = FLASE)
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw phenotype datasets for each task
#'  2) datapackage.json file for all data
#'
#' @examples
#'
#' # process REDCap data
#' phenotype_data <- proc_redcap(survey_data_path, data_de_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

proc_redcap <- function(survey_data_path, data_de_path, overwrite = FALSE, return_data = FALSE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(survey_data_path)

  if (isTRUE(data_arg)) {
    if (!is.character(survey_data_path)) {
      stop("survey_data_path must be entered as a string")
    } else if (!file.exists(survey_data_path)) {
      stop("survey_data_path entered, but file does not exist. Check survey_data_path string.")
    }
  } else if (isFALSE(data_arg)) {
    stop("survey_data_path must be entered as a string")
  }

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The proc_tasks.R has not been thoroughly tested on Windows systems, may have survey_data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }

  # find location of slashes so can decompose filepaths
  slash_loc <- unlist(gregexpr(slash, survey_data_path))

  # set paths for other directories
  base_wd <- substr(survey_data_path, 1, tail(slash_loc, 3))
  datapackage_wd <- paste0(substr(survey_data_path, 1, tail(slash_loc, 3)), 'datapackage', sep = slash)
  phenotype_wd <- paste0(datapackage_wd, slash, 'phenotype', slash)

  # add file ending if it is missing
  if (!grep('.csv', survey_data_path)) {
    visit_data_file <- paste0(survey_data_path, '.csv')
  } else {
    visit_data_file <- survey_data_path
  }

  if (!grep('.csv', data_de_path)) {
    data_de_file <- paste0(data_de_path, '.csv')
  } else {
    data_de_file <- data_de_path
  }

  # check file existis
  if (!file.exists(survey_data_path)) {
    stop ('entered survey_data_path is not an existing file - be sure it is entered as a string and contains the full data path and file name')
  }

  if (!file.exists(data_de_file)) {
    stop ('entered data_de_path is not an existing file - be sure it is entered as a string and contains the full data path and file name')
  }


  #### Load and organize visit data ####
  redcap_survey_data <- read.csv(survey_data_path, header = TRUE)

  # subset events and remove unnecessary columns
  sub_dat <- redcap_survey_data[redcap_survey_data[['redcap_event_name']] == 'event_1_arm_1', ]
  sub_dat <- sub_dat[, !colSums(is.na(sub_dat) | sub_dat == "") == nrow(sub_dat)]

  # remove withdrawn and pilot rows
  sub_dat <- sub_dat[sub_dat['record_id'] != '16' & sub_dat['record_id'] != 'Pilot', ]

  # process visit data ####

  parent_survey1 <- sub_dat[, grepl('^record_id', names(sub_dat)) | grepl('^date_', names(sub_dat)) | grepl('^bedtime_', names(sub_dat)) | grepl('^attempt_', names(sub_dat)) | grepl('^asleep_', names(sub_dat)) | grepl('^times_', names(sub_dat)) | grepl('^waso_', names(sub_dat)) | grepl('^awake_', names(sub_dat)) | grepl('^out_', names(sub_dat)) | grepl('^demo', names(sub_dat)) | grepl('^pds', names(sub_dat)) | grepl('^tanner', names(sub_dat)) | grepl('^efcr', names(sub_dat)) | grepl('^spsrq', names(sub_dat)) | grepl('^hcq', names(sub_dat)) | grepl('^cshq', names(sub_dat)) | grepl('^cebq', names(sub_dat))]

  parent_survey2 <- sub_dat[, grepl('^record_id', names(sub_dat)) | grepl('^ffq', names(sub_dat)) | grepl('^fmcb', names(sub_dat)) | grepl('^scpf', names(sub_dat)) | grepl('^ffbs', names(sub_dat)) | grepl('^hfe', names(sub_dat)) | grepl('^pwlb', names(sub_dat)) | grepl('^tfeq', names(sub_dat)) | grepl('^cfq', names(sub_dat))]

  # organize event data
  parent_survey1_data <- util_redcap_parent1(parent_survey1)
  parent_surveyw_data <- util_redcap_parent2(parent_survey2)

  #### Load and organize double-entry data ####
  redcap_de_data <- read.csv(data_de_path, header = TRUE)

  # all validated so can just take reviewer 1 data
  redcap_de_data <- redcap_de_data[grepl('--2', redcap_de_data[['record_id']]), ]

  ## interview quick work
  bod_pod_data <- redcap_de_data[, grepl('record_id', names(redcap_de_data)) | grepl('bodpod', names(redcap_de_data))]
  names(bod_pod_data)[1] <- 'participant_id'
  bod_pod_data$participant_id <- sub('--2', '', bod_pod_data$participant_id)
  bod_pod_data$participant_id <- sub('^00|^0', '', bod_pod_data$participant_id)
  bod_pod_data <- bod_pod_data[bod_pod_data$participant_id != 'pilot-006', ]

  wasi_data <- redcap_de_data[, grepl('record_id', names(redcap_de_data)) | grepl('wasi', names(redcap_de_data))]
  names(wasi_data)[1] <- 'participant_id'
  wasi_data$participant_id <- sub('--2', '', wasi_data$participant_id)
  wasi_data$participant_id <- sub('^00|^0', '', wasi_data$participant_id)
  wasi_data <- wasi_data[wasi_data$participant_id != 'pilot-006', ]

  intake_data <- redcap_de_data[, grepl('record_id', names(redcap_de_data)) | grepl('_g', names(redcap_de_data)) | grepl('_kcal', names(redcap_de_data)) | grepl('meal', names(redcap_de_data))]
  intake_data <- intake_data[, !grepl('cams', names(intake_data)) & !grepl('complete', names(intake_data)) & !grepl('tt', names(intake_data)) & !grepl('fnirs', names(intake_data)) & !grepl('bodpod', names(intake_data))]
  names(intake_data)[1] <- 'participant_id'
  intake_data$participant_id <- sub('--2', '', intake_data$participant_id)
  intake_data$participant_id <- sub('^00|^0', '', intake_data$participant_id)
  intake_data <- intake_data[intake_data$participant_id != 'pilot-006', ]

  ## ure_dat -- wasi
  ure_dat <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id')
  ure_dat <- merge(ure_dat, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id')
  ure_dat <- merge(ure_dat, bod_pod_data, by = 'participant_id')
  ure_dat <- merge(ure_dat, wasi_data, by = 'participant_id', all = TRUE)
  write.csv(ure_dat, paste0(datapackage_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'ure_pilot_data.csv'), row.names = FALSE)

  ## R01 pilot data -- intake
  r01_dat <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id')
  r01_dat <- merge(r01_dat, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id')
  r01_dat <- merge(r01_dat, bod_pod_data, by = 'participant_id', all = TRUE)
  r01_dat <- merge(r01_dat, wasi_data, by = 'participant_id', all = TRUE)
  r01_dat <- merge(r01_dat, intake_data, by = 'participant_id', all = TRUE)
  r01_dat <- merge(r01_dat, parent_v1_data$efcr_data$data$score_dat, by = 'participant_id', all = TRUE)
  r01_dat <- merge(r01_dat, parent_v1_data$cebq_data$data$score_dat, by = 'participant_id', all = TRUE)
  r01_dat <- merge(r01_dat, child_v2_data$loc_data$data, by = 'participant_id', all = TRUE)
  write.csv(r01_dat, paste0(datapackage_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'r01_pilot_data.csv'), row.names = FALSE)


  ## ure_dat -- metabolites
  ure_dat_metabolite <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id')
  ure_dat_metabolite <- merge(ure_dat_metabolite, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id')
  ure_dat_metabolite <- merge(ure_dat_metabolite, bod_pod_data, by = 'participant_id')
  ure_dat_metabolite <- merge(ure_dat_metabolite, child_v1_data$hfi_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  ure_dat_metabolite <- merge(ure_dat_metabolite, parent_v1_data$ffq_data$data$bids_phenotype, by = 'participant_id', all.x = TRUE)

  write.csv(ure_dat_metabolite, paste0(datapackage_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'ure_pilot_data_metabolite.csv'), row.names = FALSE)


  ## dairy grant pilot data
  dairy_pilot <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, parent_v1_data$brief_data$data$score_dat, by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, parent_v1_data$ffq_data$data$score_dat, by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, parent_v1_data$ffq_data$data$bids_phenotype[c(1, 3:4)], by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, child_v1_data$hfi_data$data$score_dat, by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, child_v1_data$hfi_data$data$bids_phenotype[c(1, 21:23)], by = 'participant_id', all = TRUE)
  write.csv(dairy_pilot, paste0(datapackage_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'dairy_pilot.csv'), row.names = FALSE)

  interview_dat <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id')
  interview_dat <- merge(interview_dat, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id')
  interview_dat <- merge(interview_dat, bod_pod_data, by = 'participant_id')
  interview_dat <- merge(interview_dat, parent_v2_data$hfe_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v1_data$cfq_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v2_data$ffbs_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, child_v1_data$hfi_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v1_data$ffq_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v1_data$efcr_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v1_data$cebq_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, child_v2_data$loc_data$data, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, child_v1_data$sleep_wk_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v2_data$cshq_data$data$score_dat, by = 'participant_id', all.x = TRUE)

  write.csv(interview_dat, paste0(datapackage_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'interview_pilot_data.csv'), row.names = FALSE)

  # nasa presentation
  nasa_dat <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id')
  nasa_dat <- merge(nasa_dat, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id')
  nasa_dat <- merge(nasa_dat, bod_pod_data, by = 'participant_id')
  nasa_dat <- merge(nasa_dat, intake_data, by = 'participant_id')
  nasa_dat <- merge(nasa_dat, child_v2_data$loc_data$data, by = 'participant_id', all = TRUE)
  nasa_dat <- merge(nasa_dat, parent_v1_data$cebq_data$data$score_dat, by = 'participant_id', all = TRUE)
  write.csv(nasa_dat, paste0(datapackage_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'nasa_pilot_data.csv'), row.names = FALSE)

  # export data

  ## interview data
  write.table(child_v1_data[['otherdata']][['fnirs_cap']], paste0(datapackage_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'ses-baseline_nirs-fitcap.tsv'), sep='\t', quote = FALSE, row.names = FALSE)

  ## fNIRS - raw_untouched
  write.table(child_v1_data[['otherdata']][['fnirs_cap']], paste0(datapackage_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'ses-baseline_nirs-fitcap.tsv'), sep='\t', quote = FALSE, row.names = FALSE)






  # merge
  participant_data <- merge(prepost_v1_data$prepost_data$data, prepost_v2_data$data, by = 'participant_id')
  participant_data <- merge(participant_data, child_v1_data$child_visit1_data$data, by = 'participant_id')
  participant_data <- merge(participant_data, child_v2_data$child_visit2_data$data, by = 'participant_id')
  participant_data <- merge(participant_data, child_v2_data$loc_data$data, by = 'participant_id')
  participant_data <- merge(participant_data, parent_v1_data$demo_data$data, by = 'participant_id')

  participant_data$participant_id <- as.numeric(participant_data$participant_id)

  double_enter_data$bodpod_data$data$participant_id <- as.numeric(double_enter_data$bodpod_data$data$participant_id)
  #### Load and organize data double entry ####
  redcap_de_data <- read.csv(data_de_path, header = TRUE)

  double_enter_data <- util_redcap_de(redcap_de_data)
  participant_data <- merge(participant_data, double_enter_data$bodpod_data$data, by = 'participant_id')


  #### Merge visit data/notes ###

  #### Export Phenotype Data ####

  ## child visit 1

  #sleep log
  write.csv(child_v1_data$sleep_wk_data$data, paste0(phenotype_wd, slash, 'sleep_log.tsv'), row.names = FALSE)
  #write(child_v1_data$sleep_wk_data$meta, paste0(phenotype_wd, slash, 'sleep_log.json'))

  #hfi
  write.csv(child_v1_data$hfi_data$data, paste0(phenotype_wd, slash, 'hfi.tsv'), row.names = FALSE)
  #write(child_v1_data$hfi_data$meta, paste0(phenotype_wd, slash, 'home_food_inventory.json'))

  ## child visit 2

  #loc
  write.csv(child_v2_data$loc_data$data, paste0(phenotype_wd, slash, 'loc.tsv'), row.names = FALSE)
  #write(child_v2_data$loc_data$meta, paste0(phenotype_wd, slash, 'loc.json'))

  #sic
  write.csv(child_v2_data$sic_data$data, paste0(phenotype_wd, slash, 'stess_children.tsv'), row.names = FALSE)
  #write(child_v2_data$loc_data$meta, paste0(phenotype_wd, slash, 'stress_children.json'))

  ## parent visit 1

  #cfq
  write.csv(parent_v1_data$cfq_data$data, paste0(phenotype_wd, slash, 'cfq.tsv'), row.names = FALSE)
  #write(parent_v1_data$cfq_data$meta, paste0(phenotype_wd, slash, 'cfq.json'))

  #cebq
  write.csv(parent_v1_data$cebq_data$data, paste0(phenotype_wd, slash, 'cebq.tsv'), row.names = FALSE)
  #write(parent_v1_data$cebq_data$meta, paste0(phenotype_wd, slash, 'cebq.json'))

  #efcr
  write.csv(parent_v1_data$efcr_data$data, paste0(phenotype_wd, slash, 'efcr.tsv'), row.names = FALSE)
  #write(parent_v1_data$efcr_data$meta, paste0(phenotype_wd, slash, 'efcr.json'))

  #lbc
  write.csv(parent_v1_data$lbc_data$data, paste0(phenotype_wd, slash, 'lbc.tsv'), row.names = FALSE)
  #write(parent_v1_data$lbc_data$meta, paste0(phenotype_wd, slash, 'lbc.json'))

  #brief
  write.csv(parent_v1_data$brief_data$data, paste0(phenotype_wd, slash, 'brief.tsv'), row.names = FALSE)
  #write(parent_v1_data$brief_data$meta, paste0(phenotype_wd, slash, 'brief.json'))

  #ffq
  write.csv(parent_v1_data$ffq_data$data, paste0(phenotype_wd, slash, 'ffq.tsv'), row.names = FALSE)
  #write(parent_v1_data$ffq_data$meta, paste0(phenotype_wd, slash, 'ffq.json'))

  if (isTRUE(return_data)){
    return(list( foodchoice_dat = dat,
                 foodchoice_labels = meta_json))
  }
}

