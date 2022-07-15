addLabels <- function(treatment_history, eventCohortIds, eventCohortNames) {
  
  labels <- tibble(event_cohort_id = eventCohortIds, 
                           event_cohort_name = eventCohortNames) %>%
    mutate(event_cohort_id = as.character(event_cohort_id))
  
  th <- treatment_history %>%
    dplyr::left_join(labels, by = c("event_cohort_id"))
  
  th$event_cohort_name[is.na(th$event_cohort_name)] <- sapply(th$event_cohort_id[is.na(th$event_cohort_name)], function(x) {
    
    # Revert search to look for longest concept_ids first
    for (l in nrow(labels):1)
    {
      # If treatment occurs twice in a combination (as monotherapy and as part of fixed-combination) -> remove monotherapy occurrence
      if (any(grep(labels$event_cohort_name[l], x))) {
        x <- gsub(labels$event_cohort_id[l], "", x)
      } else {
        x <- gsub(labels$event_cohort_id[l], labels$event_cohort_name[l], x)
      }
    }
    
    return(x)
  })
  
  
  # Filter out + at beginning/end or repetitions
  th$event_cohort_name <- gsub("\\++", "+", th$event_cohort_name)
  th$event_cohort_name <- gsub("^\\+", "", th$event_cohort_name)
  th$event_cohort_name <- gsub("\\+$", "", th$event_cohort_name)
  
  return(th)
}

#create treatment history
create_treatment_history <- function(connectionDetails,
                                     cdmDatabaseSchema,
                                     resultsDatabaseSchema,
                                     database,
                                     cohortTable,
                                     analysisSettings) {
  
  checkmate::assert_class(analysisSettings, "MeandrosAnalysisSettings")
  
  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))
  
  #set parameters
  targetCohortId <- analysisSettings$targetCohortId
  eventCohortIds <- analysisSettings$eventCohortIds
  eventCohortNames <- analysisSettings$eventCohortNames
  includeTreatments <- analysisSettings$includeTreatments
  periodPriorToIndex <- analysisSettings$periodPriorToIndex
  minEraDuration <- analysisSettings$minEraDuration
  eraCollapseSize <- analysisSettings$eraCollapseSize
  combinationWindow <- analysisSettings$combinationWindow
  minPostCombinationDuration <- analysisSettings$minPostCombinationDuration
  filterTreatments <- analysisSettings$filterTreatments
  maxPathLength <-analysisSettings$maxPathLength
  
  #extract cohorts
  sql <-"WITH CTE_person AS(
          SELECT SUBJECT_ID FROM @resultsSchema.@tableName WHERE COHORT_DEFINITION_ID = @targetId
          )
         SELECT a.* FROM @resultsSchema.@tableName a
         INNER JOIN CTE_person b ON a.SUBJECT_ID = b.SUBJECT_ID;"
  
  current_cohorts <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    resultsSchema = resultsDatabaseSchema,
    tableName = cohortTable,
    targetId = targetCohortId
  ) %>%
    data.table::as.data.table()
  colnames(current_cohorts) <- c("cohort_id", "person_id", "start_date", "end_date")
  
  #create treatment history using TreatmentPatterns package
  treatment_history <- TreatmentPatterns:::doCreateTreatmentHistory(current_cohorts,
                                                                    targetCohortId,
                                                                    eventCohortIds,
                                                                    periodPriorToIndex,
                                                                    includeTreatments) %>%
    TreatmentPatterns:::doEraDuration(minEraDuration) %>%
    TreatmentPatterns:::doEraCollapse(eraCollapseSize) %>%
    TreatmentPatterns:::doCombinationWindow(combinationWindow, minPostCombinationDuration) %>%
    TreatmentPatterns:::doFilterTreatments(filterTreatments)
  
  # Add event_seq number to determine order of treatments in pathway
  treatment_history <- treatment_history[order(person_id, event_start_date, event_end_date),]
  treatment_history[, event_seq:=seq_len(.N), by= .(person_id)]
  
  treatment_history <- TreatmentPatterns:::doMaxPathLength(treatment_history, maxPathLength) %>%
    # Add event_cohort_name (instead of only event_cohort_id)
    addLabels(eventCohortIds, eventCohortNames)
  
  #some clean up for the combination names
  combi <- grep("+", treatment_history$event_cohort_name, fixed=TRUE)
  cohort_names <- strsplit(treatment_history$event_cohort_name[combi], split="+", fixed=TRUE)
  treatment_history$event_cohort_name[combi] <- sapply(cohort_names, function(x) paste(sort(x), collapse = "+"))
  treatment_history$event_cohort_name <- unlist(treatment_history$event_cohort_name)
  

  return(treatment_history)
}


save_treatment_history <- function(treatment_history,
                                   analysisSettings) {
  
  treatmentPatternsFolder <- analysisSettings$outputFolder
  database <- analysisSettings$database
  
  #save treatment history to treatment patterns folder
  if(!dir.exists(file.path(treatmentPatternsFolder, database))) {
    dir.create(file.path(treatmentPatternsFolder, database), recursive = TRUE)
  }
  readr::write_csv(treatment_history,
                   file = file.path(treatmentPatternsFolder, database,"treatmentHistory.csv"))
  usethis::ui_info(
    "data.frame of treatment history saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"treatmentHistory.csv\"))}"
  )
}