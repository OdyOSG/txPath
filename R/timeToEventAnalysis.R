#' Function that creates survival tables from the treatment history
#'
#' @param treatment_history the dataframe output from create_treatment_history
#' @param connectionDetails a list of connectionDetails for DatabaseConnector
#' @param resultsDatabaseSchema the schema that hosts the users writeable results tables (or scratch), defaults to value
#' in config.yml of the active configuration
#' @param cohortTable the name where the cohorts are stored in the results schema
#' @param analysisSettings a DrugUtilizationAnalysisSettings object that defines the elements of the analysis
#' @include utils.R
#' @return a SurvivalAnalysis object containing the data for the survival
#' analysis, the survfit object and the surv_summary object from survminer
#' @export
create_survival_table <- function(treatment_history,
                                 connectionDetails,
                                 resultsDatabaseSchema,
                                 cohortTable,
                                 analysisSettings) {

  #set connection
  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  targetCohortId <- analysisSettings$targetCohortId
  minCellCount <- analysisSettings$minCellCount

  #extract target cohort for censoring
  sql <- "SELECT * FROM @cohortDatabaseSchema.@cohortTable WHERE COHORT_DEFINITION_ID = @targetCohortId"

  targetCohort <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                          sql = sql,
                                          cohortDatabaseSchema = resultsDatabaseSchema,
                                          cohortTable = cohortTable,
                                          targetCohortId = targetCohortId,
                                          snakeCaseToCamelCase = TRUE)
  #create survival table
  survTab <- treatment_history %>%
    left_join(targetCohort, by = c("person_id" = "subjectId")) %>%
    mutate(event = ifelse(event_end_date < cohortEndDate, 1, 0)) %>%
    select(event_cohort_id, duration_era, event) %>%
    tidyr::nest(data = !event_cohort_id) %>%
    dplyr::mutate(nn = purrr::map_int(data, ~nrow(.x))) %>%
    dplyr::filter(nn >= minCellCount) %>%
    dplyr::select(event_cohort_id, data) %>%
    tidyr::unnest(cols = c(data))

  survFit <- survival::survfit(
    survival::Surv(duration_era, event) ~ event_cohort_id,
    data = survTab)
  attr(survFit$strata, "names") <- gsub("event_cohort_id=", "", attr(survFit$strata, "names"))

  survInfo <- survminer::surv_summary(survFit, data = survTab)


  survival_table <- structure(
    list(survTab = survTab,
         survFit = survFit,
         survInfo = survInfo),
    class = "SurvivalAnalysis")

  return(survival_table)

}


#' Function to save the survival analysis as multiple csv
#'
#' @param survival_table a SurvivalAnalysis object with information about the survival analysis
#' @param analysisSettings a DrugUtilizationAnalysisSettings object that defines the elements of the analysis
#' @include utils.R
#' @import usethis
#' @export
save_survival_table <- function(survival_table,
                                    analysisSettings) {

  #extract relevant tables
  survTab <- survival_table$survTab
  surv_summary <- survival_table$survInfo
  surv_curves <- attr(surv_summary, "table") %>%
    tibble::rownames_to_column(var = "treatmentLine") %>%
    tibble::as_tibble()


  treatmentPatternsFolder <- analysisSettings$outputFolder
  database <- analysisSettings$database

  #save treatment patterns to treatment patterns folder
  if(!dir.exists(file.path(treatmentPatternsFolder, database))) {
    dir.create(file.path(treatmentPatternsFolder, database), recursive = TRUE)
  }
  #save survival data
  readr::write_csv(survTab,
                   file = file.path(treatmentPatternsFolder, database,"survivalData.csv"))
  usethis::ui_info(
    "data.frame of survival data saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"survivalData.csv\"))}"
  )

  #save surv_summary
  readr::write_csv(surv_summary,
                   file = file.path(treatmentPatternsFolder, database,"survivalSummary.csv"))
  usethis::ui_info(
    "data.frame of survival summary saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"survivalSummary.csv\"))}"
  )


  #save surv_curves
  readr::write_csv(surv_curves,
                   file = file.path(treatmentPatternsFolder, database,"survivalCurves.csv"))
  usethis::ui_info(
    "data.frame of survival curves saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"survivalCurves.csv\"))}"
  )

}

#' Function to plot the survival analysis for time to discontinuation
#'
#' @param survival_table a SurvivalAnalysis object with information about the survival analysis
#' @param type identify the treatment line type to display, Either single, only combinations or all lines
#' @param selectTop display an integer number of lines of treatment to display in the kaplan meier plot
#' @return a kaplan meier plot suing survminer. We implement the lancet color schema from ggsci
#' @include utils.R
#' @export
plot_kaplan_meier <- function(survival_table,
                               type = c("Single", "Combinations", "All"),
                               selectTop = 7) {


  nn <- survival_table$survTab %>%
    count(event_cohort_id) %>%
    slice_max(n, n = selectTop) %>%
    pull(event_cohort_id)

  survInfo <- survival_table$survInfo %>%
    filter(strata %in% nn)
  survTab <- survival_table$survTab %>%
    filter(event_cohort_id %in% nn)
  if (type == "Single") {
    si <- survInfo %>%
      dplyr::filter(!grepl("\\+", strata))
    st <- survTab %>%
      dplyr::filter(!grepl("\\+", event_cohort_id))
  }

  if (type == "Combinations") {
    si <- survInfo %>%
      dplyr::filter(grepl("\\+", strata))
    st <- survTab %>%
      dplyr::filter(grepl("\\+", event_cohort_id))
  }



  survminer::ggsurvplot(fit = si,
                        data = st,
                        size = 0.8,
                        palette = ggsci::pal_lancet()(selectTop),
                        xlab = "Time in days",
                        break.time.by = 30)


}

