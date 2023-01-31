#' Create an example cdm with event cohorts
#'
#' Return an example cdm object with a target cohort of patients with GIBleed
#' and event cohorts for every drug ingredient exposure in the Eunomia test
#' database.
#'
#' @param n Number of unique persons to include in the target and event cohorts
#' By default all persons with a GI bleed condition will be included.
#'
#' @return A cdm reference object with two cohort tables: target_cohort and event_cohorts.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- createTestCdm()
#' cdm <- createTestCdm(n = 10)
#' }
createTestCdm <- function(n = NULL) {
  checkmate::expect_integerish(n, lower = 1, upper = 1e6, null.ok = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main")

  cdm$event_cohorts <- cdm$drug_era %>%
    dplyr::rename(subject_id = person_id,
                  cohort_definition_id = drug_concept_id,
                  cohort_start_date = drug_era_start_date,
                  cohort_end_date = drug_era_end_date) %>%
    dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)

  cdm$target_cohort <- cdm$concept_ancestor %>%
    dplyr::filter(ancestor_concept_id %in% c(192671)) %>% # GI Bleed
    dplyr::select(condition_concept_id = descendant_concept_id) %>%
    dplyr::inner_join(cdm$condition_occurrence, by = c("condition_concept_id")) %>%
    dplyr::select(cohort_definition_id = 1, person_id, cohort_start_date = condition_start_date) %>%
    dplyr::inner_join(cdm$observation_period, by = "person_id") %>%
    dplyr::filter(cohort_start_date < observation_period_end_date) %>%
    dplyr::select(cohort_definition_id, subject_id = person_id, cohort_start_date, cohort_end_date = observation_period_end_date)

  if (!is.null(n)) {
    ids <- dplyr::intersect(
        cdm$event_cohorts %>%
          distinct(subject_id) %>%
          pull(subject_id),

      cdm$target_cohort %>%
          distinct(subject_id) %>%
          pull(subject_id)
      ) %>%
      unique() %>%
      sort()

    n <- min(n, length(ids))

    ids_subset <- ids[seq_len(n)]
  }

  cdm$target_cohort <-  cdm$target_cohort %>%
    { if (!is.null(n)) dplyr::filter(., subject_id %in% local(ids_subset)) else . } %>%
    CDMConnector::computeQuery("target_cohort", temporary = FALSE, overwrite = TRUE, schema = "main")

  cdm$event_cohorts <-  cdm$event_cohorts %>%
    { if (!is.null(n)) dplyr::filter(., subject_id %in% local(ids_subset)) else . } %>%
    CDMConnector::computeQuery("event_cohorts", temporary = FALSE, overwrite = TRUE, schema = "main")

  return(cdm)
}




#' Compute treatment pathways from a set of event cohorts
#'
#' @param cdm A cdm reference object with at least two cohort tables: A target
#' cohort table and an event cohort table.
#' @param targetCohortTable The name of the target cohort table
#' @param eventCohortTable The name of the event cohort table
#' @param periodPriorToIndex The number of days prior to index to look for event
#' cohort start dates.
#' @param verbose Should progress messages be printed?
#' @param maxIterations Maximum number of iterations to allow for collapse algorithm
#' @param minEraDuration  Minimum time an event era or combination
#' @param eraCollapseSize Maximum time interval between treatments
#' should last to be included in analysis
#'
#' @return A cdm reference with a new table treatment_patterns added
computeTreatmentPathways <- function(cdm,
                                     targetCohortTable,
                                     eventCohortTable,
                                     periodPriorToIndex = 0,
                                     minEraDuration = 0,
                                     eraCollapseSize = 1,
                                     verbose = TRUE,
                                     maxIterations = 1000) {

  # preprocess
  event_cohorts_filtered <- cdm[[eventCohortTable]] %>%
    dplyr::rename(event_id = cohort_definition_id, event_start_date = cohort_start_date, event_end_date = cohort_end_date) %>%
    dplyr::inner_join(cdm[[targetCohortTable]], by = "subject_id") %>%
    dplyr::mutate(lookback_date = !!CDMConnector::dateadd("cohort_start_date", periodPriorToIndex)) %>%
    dplyr::filter(event_start_date >= lookback_date) %>%
    dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, lookback_date,
                  event_id, event_start_date, event_end_date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
    CDMConnector::computeQuery()


  # add duration, filter on minEraDuration,
  # add gap time and collapse if gap < eraCollapseSize
  trace <- event_cohorts_filtered %>%
    dplyr::mutate(duration = !!CDMConnector::datediff("event_start_date", "event_end_date")) %>%
    dplyr::filter(duration >= minEraDuration) %>%
    dplyr::group_by(cohort_definition_id, subject_id, event_id) %>%
    dbplyr::window_order(event_start_date, event_start_date) %>%
    dplyr::mutate(lag_end_date = lag(event_end_date)) %>%
    dplyr::mutate(gap_days = !!CDMConnector::datediff("lag_end_date", "event_start_date")) %>%
    dplyr::mutate(new_event = dplyr::if_else(is.na(gap_days) || gap_days > eraCollapseSize, 1L, 0L)) %>%
    dplyr::mutate(event_counter = cumsum(new_event)) %>%
    dplyr::group_by(cohort_definition_id, subject_id, event_id, event_counter) %>%
    dplyr::mutate(new_event_start_date = min(event_start_date, na.rm = TRUE),
                  new_event_end_date = max(event_end_date, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # TODO give user option to inspect the trace
  preprocessed <- trace %>%
    dplyr::mutate(event_id = as.character(as.integer(event_id))) %>%
    dplyr::select(cohort_definition_id,
                  subject_id,
                  cohort_start_date,
                  cohort_end_date,
                  event_id,
                  event_start_date = new_event_start_date,
                  event_end_date = new_event_end_date) %>%
    dplyr::distinct() %>%
    CDMConnector::computeQuery()


  # iteratively collapse ----------
  x <- preprocessed
  for (i in seq_len(maxIterations)) {
    currentRowcount <- dplyr::tally(x) %>% dplyr::pull(n)
    if (verbose) {
      print(glue::glue("Iteration {i} rowcount: {currentRowcount}"))
    }
    x <- collapseEras(x, minEraDuration = minEraDuration)
    newRowcount <- dplyr::tally(x) %>% dplyr::pull(n)
    if (currentRowcount == newRowcount) {
      # TODO parameterize output table name?
      cdm[["treatment_patterns"]] <- x
      return(cdm)
    }
  }
  rlang::abort("Error with collapse function. Set verbose to TRUE or increase maxIterations.")
}

# Internal function to collapse eras
collapseEras <- function(preprocessed, minEraDuration = 0) {

  # TODO add persistant table option

  # identify the first pair of records that overlap for each person. Flag them with filter_flag.
  qry <- preprocessed %>%
    # mutate(event_id = as.character(as.integer(event_id))) %>%
    dplyr::group_by(cohort_definition_id, subject_id) %>%
    dplyr::mutate(lag_event_end_date = lag(event_end_date, order_by = c(event_start_date, event_end_date))) %>%
    dplyr::mutate(overlap_flag = case_when(event_start_date <= lag_event_end_date ~ 1L, TRUE ~ 0L)) %>%
    dplyr::group_by(cohort_definition_id, subject_id, overlap_flag) %>%
    dbplyr::window_order(cohort_definition_id, subject_id, event_start_date, event_end_date) %>%
    dplyr::mutate(row_flag = if_else(overlap_flag*row_number() == 1L, 1L, 0L)) %>%
    dplyr::group_by(cohort_definition_id, subject_id) %>%
    dplyr::mutate(filter_flag = case_when(row_flag == 1L || lead(row_flag, order_by = c(event_start_date, event_end_date)) == 1L ~ 1L, TRUE ~ 0L)) %>%
    dplyr::arrange(cohort_definition_id, subject_id, event_start_date, event_end_date) %>%
    dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, event_id, event_start_date, event_end_date, filter_flag) %>%
    dplyr::ungroup() %>%
    CDMConnector::computeQuery()

  # quality check
  check <- qry %>%
    dplyr::filter(filter_flag == 1L) %>%
    dplyr::count(cohort_definition_id, subject_id, name = "n_rows_per_person") %>%
    dplyr::count(n_rows_per_person, name = "n_persons") %>%
    dplyr::collect()

  if (nrow(check) == 0) {
    # no overlaps to process
    return(preprocessed)
  } else if (!all(check$n_rows_per_person == 2)) {
    print(check)
    rlang::abort("There should be two rows per person in the collapse step! Error with era collapse algorithm!")
  }

  # TODO add combinationWindow
  # combinationWindow should be greater than min era length
  # stopifnot(combinationWindow >= minEraDuration)

  combos <- qry %>%
    dplyr::filter(filter_flag == 1) %>%
    dplyr::select(-filter_flag) %>%
    dplyr::group_by(cohort_definition_id, subject_id) %>%
    # collapse to one row per person
    dplyr::mutate(
      event_id_a = event_id,
      event_id_b = lead(event_id, order_by = c(event_start_date, event_end_date)),
      start_a = event_start_date, end_a = event_end_date,
      start_overlap = max(event_start_date, na.rm = TRUE),
      end_overlap = min(event_end_date, na.rm = TRUE),
      start_b = lead(event_start_date, order_by = c(event_start_date, event_end_date)),
      end_b = lead(event_end_date, order_by = c(event_start_date, event_end_date))) %>%
    dplyr::filter(!is.na(event_id_b)) %>%
    dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, matches("_(a|b|overlap)$")) %>%
    dplyr::mutate(overlap = !!CDMConnector::datediff("start_overlap", "end_overlap"),
           event_id_overlap = paste(event_id_a, event_id_b, sep = "-")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      new_start_a = case_when(
        start_a == start_b ~ NA,
        TRUE ~ start_a),
      new_end_a = case_when(
        start_a == start_b ~ NA,
        TRUE ~ start_overlap
      ),
      # start and end overlap are unchanged
      new_start_b = case_when(
        end_b > end_overlap ~ end_overlap,
        end_a > end_overlap ~ end_overlap,
        TRUE ~ NA
      ),
      new_end_b = case_when(
        end_b > end_overlap ~ end_b,
        end_a > end_overlap ~ end_a,
        TRUE ~ NA
      )
    ) %>%
    dplyr::arrange(cohort_definition_id, subject_id) %>%
    CDMConnector::computeQuery()

  output <- list(
    dplyr::filter(qry, filter_flag == 0L) %>% dplyr::select(-filter_flag),
    dplyr::select(combos,
                  cohort_definition_id,
                  subject_id,
                  cohort_start_date,
                  cohort_end_date,
                  event_id = event_id_a,
                  event_start_date = new_start_a,
                  event_end_date = new_end_a) %>%
      dplyr::filter(!!CDMConnector::datediff("event_start_date", "event_end_date") >= minEraDuration),
    dplyr::select(combos,
                  cohort_definition_id,
                  subject_id,
                  cohort_start_date,
                  cohort_end_date,
                  event_id = event_id_b,
                  event_start_date = new_start_b,
                  event_end_date = new_end_b) %>%
      dplyr::filter(!!CDMConnector::datediff("event_start_date", "event_end_date") >= minEraDuration),
    dplyr::select(combos,
                  cohort_definition_id,
                  subject_id,
                  cohort_start_date,
                  cohort_end_date,
                  event_id = event_id_overlap,
                  event_start_date = start_overlap,
                  event_end_date = end_overlap) %>%
      dplyr::filter(!!CDMConnector::datediff("event_start_date", "event_end_date") >= minEraDuration)
    ) %>%
    purrr::reduce(dplyr::union) %>%
    dplyr::filter(!is.na(event_start_date) && !is.na(event_end_date)) %>%
    dplyr::filter(!!CDMConnector::datediff("event_start_date", "event_end_date") >= minEraDuration) %>%
    dplyr::distinct() %>%
    dplyr::arrange(cohort_definition_id, subject_id, event_start_date, event_end_date) %>%
    CDMConnector::computeQuery()

  return(output)
}
















