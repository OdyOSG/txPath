# Modified version of addPathwaySettings from TreatmentPatterns


create_analysis_settings <- function(database,
                              studyName = "main",
                              targetCohortId,
                              targetCohortName = NULL,
                              eventCohortIds,
                              eventCohortNames = NULL,
                              includeTreatments = "startDate",
                              periodPriorToIndex = 0,
                              minEraDuration = 0,
                              # splitEventCohorts = "",
                              # splitTime = 30,
                              eraCollapseSize = 30,
                              combinationWindow = 30, 
                              minPostCombinationDuration = 30,
                              filterTreatments = "First",
                              maxPathLength = 5, 
                              minCellCount = 5,
                              minCellMethod = "Remove",
                              groupCombinations = 10,
                              addNoPaths = FALSE,
                              outputFolder = "treatmentPatterns") {
  
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
                   outputFolder = outputFolder), class = "MeandrosAnalysisSettings")    
  
  return(settings)
}
