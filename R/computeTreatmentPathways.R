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
    dplyr::rename(subject_id = "person_id",
                  cohort_definition_id = "drug_concept_id",
                  cohort_start_date = "drug_era_start_date",
                  cohort_end_date = "drug_era_end_date") %>%
    dplyr::select(.data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date)

  cdm$target_cohort <- cdm$concept_ancestor %>%
    dplyr::filter(.data$ancestor_concept_id %in% c(192671)) %>% # GI Bleed
    dplyr::select(condition_concept_id = .data$descendant_concept_id) %>%
    dplyr::inner_join(cdm$condition_occurrence, by = c("condition_concept_id")) %>%
    dplyr::select(cohort_definition_id = 1,
                  .data$person_id,
                  cohort_start_date = .data$condition_start_date) %>%
    dplyr::inner_join(cdm$observation_period, by = "person_id") %>%
    dplyr::filter(.data$cohort_start_date < .data$observation_period_end_date) %>%
    dplyr::select(.data$cohort_definition_id,
                  subject_id = .data$person_id,
                  .data$cohort_start_date,
                  cohort_end_date = .data$observation_period_end_date)

  if (!is.null(n)) {
    ids <- dplyr::intersect(
        cdm$event_cohorts %>%
          dplyr::distinct(.data$subject_id) %>%
          dplyr::pull(.data$subject_id),

      cdm$target_cohort %>%
          dplyr::distinct(.data$subject_id) %>%
          dplyr::pull(.data$subject_id)
      ) %>%
      unique() %>%
      sort()

    n <- min(n, length(ids))

    ids_subset <- ids[seq_len(n)]
  }
  . <- "" # to aviod RCheck warning
  cdm$target_cohort <-  cdm$target_cohort %>%
    { if (!is.null(n)) dplyr::filter(., .data$subject_id %in% local(ids_subset)) else . } %>%
    CDMConnector::computeQuery("target_cohort", temporary = FALSE, overwrite = TRUE, schema = "main")

  cdm$event_cohorts <-  cdm$event_cohorts %>%
    { if (!is.null(n)) dplyr::filter(., .data$subject_id %in% local(ids_subset)) else . } %>%
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
    dplyr::rename(event_id = .data$cohort_definition_id,
                  event_start_date = .data$cohort_start_date,
                  event_end_date = .data$cohort_end_date) %>%
    dplyr::inner_join(cdm[[targetCohortTable]], by = "subject_id") %>%
    dplyr::mutate(lookback_date = !!CDMConnector::dateadd("cohort_start_date", periodPriorToIndex)) %>%
    dplyr::filter(.data$event_start_date >= .data$lookback_date) %>%
    dplyr::select(.data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  .data$lookback_date,
                  .data$event_id,
                  .data$event_start_date,
                  .data$event_end_date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$cohort_definition_id,
                   .data$subject_id,
                   .data$cohort_start_date,
                   .data$cohort_end_date) %>%
    CDMConnector::computeQuery()

  # add duration, filter on minEraDuration,
  # add gap time and collapse if gap < eraCollapseSize
  trace <- event_cohorts_filtered %>%
    dplyr::mutate(duration = !!CDMConnector::datediff("event_start_date", "event_end_date")) %>%
    dplyr::filter(.data$duration >= .env$minEraDuration) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id, .data$event_id) %>%
    dbplyr::window_order(.data$event_start_date, .data$event_start_date) %>%
    dplyr::mutate(lag_end_date = dplyr::lag(.data$event_end_date)) %>%
    dplyr::mutate(gap_days = !!CDMConnector::datediff("lag_end_date", "event_start_date")) %>%
    dplyr::mutate(new_event = dplyr::if_else(is.na(.data$gap_days) || .data$gap_days > .env$eraCollapseSize, 1L, 0L)) %>%
    dplyr::mutate(event_counter = cumsum(.data$new_event)) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id, .data$event_id, .data$event_counter) %>%
    dplyr::mutate(new_event_start_date = min(.data$event_start_date, na.rm = TRUE),
                  new_event_end_date = max(.data$event_end_date, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # TODO give user option to inspect the trace
  preprocessed <- trace %>%
    dplyr::mutate(event_id = as.character(as.integer(.data$event_id))) %>%
    dplyr::select(.data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  .data$event_id,
                  event_start_date = .data$new_event_start_date,
                  event_end_date = .data$new_event_end_date) %>%
    dplyr::distinct() %>%
    CDMConnector::computeQuery()


  # iteratively collapse ----------
  x <- preprocessed
  for (i in seq_len(maxIterations)) {
    currentRowcount <- dplyr::tally(x) %>% dplyr::pull(.data$n)
    if (verbose) {
      print(glue::glue("Iteration {i} rowcount: {currentRowcount}"))
    }
    x <- collapseEras(x, minEraDuration = minEraDuration)
    newRowcount <- dplyr::tally(x) %>% dplyr::pull(.data$n)
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

  # TODO add persistent table option

  # identify the first pair of records that overlap for each person. Flag them with filter_flag.
  qry <- preprocessed %>%
    # mutate(event_id = as.character(as.integer(event_id))) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dplyr::mutate(lag_event_end_date = dplyr::lag(.data$event_end_date,
                                                  order_by = c(.data$event_start_date, .data$event_end_date))) %>%
    dplyr::mutate(overlap_flag = dplyr::case_when(.data$event_start_date <= .data$lag_event_end_date ~ 1L, TRUE ~ 0L)) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id, .data$overlap_flag) %>%
    dbplyr::window_order(.data$cohort_definition_id,
                         .data$subject_id,
                         .data$event_start_date,
                         .data$event_end_date) %>%
    dplyr::mutate(row_flag = dplyr::if_else((.data$overlap_flag * dplyr::row_number()) == 1L, 1L, 0L)) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dplyr::mutate(filter_flag = dplyr::case_when(
      .data$row_flag == 1L ~ 1L,
      dplyr::lead(.data$row_flag, order_by = c(.data$event_start_date, .data$event_end_date)) == 1L ~ 1L,
      TRUE ~ 0L)) %>%
    dplyr::arrange(.data$cohort_definition_id, .data$subject_id, .data$event_start_date, .data$event_end_date) %>%
    dplyr::select(.data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  .data$event_id,
                  .data$event_start_date,
                  .data$event_end_date,
                  .data$filter_flag) %>%
    dplyr::ungroup() %>%
    CDMConnector::computeQuery()

  # quality check
  check <- qry %>%
    dplyr::filter(.data$filter_flag == 1L) %>%
    dplyr::count(.data$cohort_definition_id, .data$subject_id, name = "n_rows_per_person") %>%
    dplyr::count(.data$n_rows_per_person, name = "n_persons") %>%
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
    dplyr::filter(.data$filter_flag == 1) %>%
    dplyr::select(-"filter_flag") %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    # collapse to one row per person
    dplyr::mutate(
      event_id_a = .data$event_id,
      event_id_b = dplyr::lead(.data$event_id,
                               order_by = c(.data$event_start_date, .data$event_end_date)),
      start_a = .data$event_start_date, end_a = .data$event_end_date,
      start_overlap = max(.data$event_start_date, na.rm = TRUE),
      end_overlap = min(.data$event_end_date, na.rm = TRUE),
      start_b = dplyr::lead(.data$event_start_date,
                            order_by = c(.data$event_start_date, .data$event_end_date)),
      end_b = dplyr::lead(.data$event_end_date,
                          order_by = c(.data$event_start_date, .data$event_end_date))) %>%
    dplyr::filter(!is.na(.data$event_id_b)) %>%
    dplyr::select(.data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  dplyr::matches("_(a|b|overlap)$")) %>%
    dplyr::mutate(overlap = !!CDMConnector::datediff("start_overlap", "end_overlap"),
                  event_id_overlap = paste(.data$event_id_a, .data$event_id_b, sep = "-")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      new_start_a = dplyr::case_when(
        .data$start_a == .data$start_b ~ NA,
        TRUE ~ .data$start_a),
      new_end_a = dplyr::case_when(
        .data$start_a == .data$start_b ~ NA,
        TRUE ~ .data$start_overlap
      ),
      # start and end overlap are unchanged
      new_start_b = dplyr::case_when(
        .data$end_b > .data$end_overlap ~ .data$end_overlap,
        .data$end_a > .data$end_overlap ~ .data$end_overlap,
        TRUE ~ NA
      ),
      new_end_b = dplyr::case_when(
        .data$end_b > .data$end_overlap ~ .data$end_b,
        .data$end_a > .data$end_overlap ~ .data$end_a,
        TRUE ~ NA
      )
    ) %>%
    dplyr::arrange(.data$cohort_definition_id, .data$subject_id) %>%
    CDMConnector::computeQuery()

  output <- list(
    dplyr::filter(qry, .data$filter_flag == 0L) %>% dplyr::select(-"filter_flag"),
    dplyr::select(combos,
                  .data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  event_id = .data$event_id_a,
                  event_start_date = .data$new_start_a,
                  event_end_date = .data$new_end_a) %>%
      dplyr::filter(!!CDMConnector::datediff("event_start_date", "event_end_date") >= .env$minEraDuration),
    dplyr::select(combos,
                  .data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  event_id = .data$event_id_b,
                  event_start_date = .data$new_start_b,
                  event_end_date = .data$new_end_b) %>%
      dplyr::filter(!!CDMConnector::datediff("event_start_date", "event_end_date") >= .env$minEraDuration),
    dplyr::select(combos,
                  .data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  event_id = .data$event_id_overlap,
                  event_start_date = .data$start_overlap,
                  event_end_date = .data$end_overlap) %>%
      dplyr::filter(!!CDMConnector::datediff("event_start_date", "event_end_date") >= .env$minEraDuration)
    ) %>%
    purrr::reduce(dplyr::union) %>%
    dplyr::filter(!is.na(.data$event_start_date) && !is.na(.data$event_end_date)) %>%
    dplyr::filter(!!CDMConnector::datediff("event_start_date", "event_end_date") >= .env$minEraDuration) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$cohort_definition_id,
                   .data$subject_id,
                   .data$event_start_date,
                   .data$event_end_date) %>%
    CDMConnector::computeQuery()

  return(output)
}




