# Modified version of addPathwaySettings from TreatmentPatterns

#' Function to create analysis settings for drug utilization
#'
#' This function adopts ideas from TreatmentPatterns::addPathwayAnalysis and
#' builds the settings needed for a drug utilization study.
#'
#' @param database identify the database for the analysis
#' @param studyName provide a study name defaults to main
#' @param targetCohortId the ids for the target cohort
#' @param targetCohortName the name of the target cohort
#' @param eventCohortIds the ids for the event cohorts
#' @param eventCohortNames the names for the event cohorts
#' @param includeTreatments Include treatments starting ('startDate') or ending ('endDate') after target cohort start date
#' @param periodPriorToIndex 	Number of days prior to the index date of the target cohort that event cohorts are allowed to start
#' @param minEraDuration Minimum time an event era should last to be included in analysis
#' @param eraCollapseSize Window of time between which two eras of the same event cohort are collapsed into one era
#' @param combinationWindow 	Window of time two event cohorts need to overlap to be considered a combination treatment
#' @param minPostCombinationDuration 	Minimum time an event era before or after a generated combination treatment should last to be included in analysis
#' @param filterTreatments 	Select first occurrence of ("First") / changes between ("Changes') / all event cohorts ("All")
#' @param maxPathLength Maximum number of steps included in treatment pathway (max 5)
#' @param minCellCount Minimum number of persons with a specific treatment pathway for the pathway to be included in analysis
#' @param minCellMethod Select to completely remove / sequentially adjust (by removing last step as often as necessary) treatment pathways below minCellCount
#' @param groupCombinations Select to group all non-fixed combinations in one category 'other’ in the sunburst plot
#' @param addNoPaths Select to include untreated persons without treatment pathway in the sunburst plot
#' @param outputFolder the outputfolder to save data to csv
#' @return a DrugUtilizationAnalysisSettings object defining the treatment analysis
#' @export
create_analysis_settings <- function(database,
                              studyName = "main",
                              targetCohortId,
                              targetCohortName = NULL,
                              eventCohortIds,
                              eventCohortNames = NULL,
                              includeTreatments = "startDate",
                              periodPriorToIndex = 0,
                              minEraDuration = 0,
                              eraCollapseSize = 30,
                              combinationWindow = 30,
                              minPostCombinationDuration = 30,
                              filterTreatments = "First",
                              maxPathLength = 5,
                              minCellCount = 5,
                              minCellMethod = "Remove",
                              groupCombinations = 10,
                              addNoPaths = FALSE,
                              outputFolder) {

  #argument checks
  checkmate::assert_int(targetCohortId)
  checkmate::assert_integer(eventCohortIds)

  if (is.null(eventCohortNames)) {
    eventCohortNames <- paste("Event_Cohort", eventCohortIds, sep = "_")
  }

  if (is.null(targetCohortName)) {
    targetCohortName <- paste("Target Cohort", targetCohortId, sep = "_")
  }
  checkmate::assert_set_equal(length(eventCohortIds), length(eventCohortNames))
  checkmate::assert_choice(includeTreatments, c("startDate", "endDate"))
  checkmate::assert_choice(filterTreatments, c("First", "Changes", "All"))

  #create settings
  settings <- structure(list(
                   database = database,
                   studyName = studyName,
                   targetCohortId = targetCohortId,
                   targetCohortName = targetCohortName,
                   eventCohortIds = eventCohortIds,
                   eventCohortNames = eventCohortNames,
                   includeTreatments = includeTreatments,
                   periodPriorToIndex = periodPriorToIndex,
                   minEraDuration = minEraDuration,
                   # splitEventCohorts = splitEventCohorts,
                   # splitTime = splitTime, not used for now
                   eraCollapseSize = eraCollapseSize,
                   combinationWindow = combinationWindow,
                   minPostCombinationDuration = minPostCombinationDuration,
                   filterTreatments = filterTreatments,
                   maxPathLength = maxPathLength,
                   minCellCount = minCellCount,
                   minCellMethod = minCellMethod,
                   groupCombinations = groupCombinations,
                   addNoPaths = addNoPaths,
                   outputFolder = outputFolder), class = "DrugUtilizationAnalysisSettings")

  return(settings)
}
