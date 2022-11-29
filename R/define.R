#' Identify Treatment history cohorts
#' @description this function takes the meta file for all cohorts in the cohort table
#' and allows the user to identify which is the target cohort and which are the
#' event cohorts
#' @param cohortMeta meta table describing the cohorts in the cohort table
#' @param targetId a cohort id integer identifying the target cohort for the
#' treatment history analysis to search in the cohortMeta
#' @param eventIds an integer vector of cohortIds used to look identifying the
#' events cohorts for the treatment history analysis to searach in the cohortMeta
#' @export
identifyThCohorts <- function(cohortMeta, targetId, eventIds) {

  allIds <- c(targetId, eventIds)

  cohortMeta %>%
    dplyr::filter(cohort_id %in% allIds) %>%
    dplyr::select(cohort_id, cohort_name) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        cohort_id %in% targetId ~ "target",
        cohort_id %in% eventIds ~ "event",
        TRUE ~ NA_character_
      )
    )
}

#' Define the treatment history settings
#'
#' @description This function defines the parameters for an individual treatment history analysis.
#' These settings are based on TreatmentPatterns package available from DARWIN.
#' Upon cohort specification, the user can input parameters for the treatment history construction.
#' A treatment history data.frame is constructed based on the following options:
#' \itemize{
#'   \item{minEraDuration}{the minimum number of days for an event era}
#'   \item{eraCollapseSize}{the maximum gap, defined in days, between two eras of the same event cohort which would still allow the eras to be collapesed into one era}
#'   \item{combinationWindow}{the maximum gap, defined in days, between two eras of the same event cohort which would still allow the eras to be collapesed into one era}
#'   \item{minPostCombinationDuration}{the minimum time, defined in days, that an event era before or after a generated combination treatment should last to be included in the pathway as a separate treatment}
#'   \item{filterTreatments}{a character string describing the filtering preference for including into a pathway.}
#' }
#' Options for filterTreatments include 'First" which limits the pathway to the first occurrence of a treatment,
#' 'Changes' which removed sequential repeated treatments or 'All' which includes all treatments.
#' There are additional options such as maxPathLength, minCellCount, minCellMethod, groupCombinations,
#' periodPriorToIndex, and addNoPaths which are trivial but can be modified.
#'
#' @param cohorts a tibble of cohorts that are built manually or using the identifyThCohorts
#' function from this package. A valid cohorts tibble has a cohort_id, cohort_name and type columns.
#' @param ... a dot list of other options to modify in the treatment history settings
#' @export
defineTreatmentHistory <- function(cohorts, ...) {


  ths <- structure(
    list(
      cohorts = cohorts,
      minEraDuration = 0L,
      eraCollapseSize = 30L,
      combinationWindow = 30L,
      minPostCombinationDuration =30L,
      filterTreatments = "Changes",
      periodPriorToIndex = 0L,
      includeTreatments = "startDate",
      maxPathLength = 5L,
      minCellCount = 5L,
      minCellMethod = "Remove",
      groupCombinations = 10L,
      addNoPaths = FALSE),
    class = "treatmentHistorySettings")


  ths <- purrr::list_modify(ths, ...)
  return(ths)

}

