

createTestCdm <- function() {
  library(CDMConnector)
  library(dplyr, warn.conflicts = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb(), eunomia_dir())
  cdm <- cdmFromCon(con, cdmSchema = "main")

  cdm

  cdm$event_cohorts <- cdm$drug_era %>%
    rename(subject_id = person_id,
           cohort_id = drug_concept_id,
           cohort_start_date = drug_era_start_date,
           cohort_end_date = drug_era_end_date) %>%
    select(cohort_id, subject_id, cohort_start_date, cohort_end_date) %>%
    computeQuery("event_cohorts", temporary = FALSE, overwrite = TRUE, schema = "main")


  cdm$event_cohorts <- cdm$drug_era %>%
    rename(subject_id = person_id,
           cohort_id = drug_concept_id,
           cohort_start_date = drug_era_start_date,
           cohort_end_date = drug_era_end_date) %>%
    select(cohort_id, subject_id, cohort_start_date, cohort_end_date) %>%
    computeQuery("event_cohorts", temporary = FALSE, overwrite = TRUE, schema = "main")

  df <- cdm$condition_occurrence %>%
    count(condition_occurrence_id, sort = T) %>%
    collect()


  cdm$target_cohort <- cdm$concept_ancestor %>%
    filter(ancestor_concept_id %in% c(192671)) %>% # GI Bleed
    select(condition_concept_id = descendant_concept_id) %>%
    inner_join(cdm$condition_occurrence, by = c("condition_concept_id")) %>%
    select(cohort_definition_id = 1, person_id, cohort_start_date = condition_start_date) %>%
    inner_join(cdm$observation_period, by = "person_id") %>%
    filter(cohort_start_date < observation_period_end_date) %>%
    select(cohort_definition_id, subject_id = person_id, cohort_start_date, cohort_end_date = observation_period_end_date)
}


#' @param includeTreatments Include treatments starting ('startDate') or ending ('endDate') after target cohort start date

#' @param periodPriorToIndex Number of days prior to the index date of the target cohort that event cohorts are allowed to start
#' @param minEraDuration  Minimum time an event era should last to be included in analysis
#' @param eraCollapseSize  Window of time between which two eras of the same event cohort are collapsed into one era
#' @param combinationWindow Window of time two event cohorts need to overlap to be considered a combination treatment
#' @param minPostCombinationDuration Minimum time an event era before or after a generated combination treatment should last to be included in analysis
#' @param filterTreatments  Select first occurrence of ("First") / changes between ("Changes') / all event cohorts ("All")
#' @param maxPathLength Maximum number of steps included in treatment pathway (max 5)

periodPriorToIndex = 0
minEraDuration = 10
eraCollapseSize = 9000

combinationWindow
minPostCombinationDuration
filterTreatments
maxPathLength

# preprocess
event_cohorts_filtered <- cdm$event_cohorts %>%
  rename(event_id = cohort_id, event_start_date = cohort_start_date, event_end_date = cohort_end_date) %>%
  inner_join(cdm$target_cohort, by = "subject_id") %>%
  mutate(lookback_date = !!dateadd("cohort_start_date", periodPriorToIndex)) %>%
  filter(event_start_date >= lookback_date) %>%
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, lookback_date,
         event_id, event_start_date, event_end_date) %>%
  distinct() %>%
  arrange(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
  computeQuery()


# add duration, filter on minEraDuration,
# add gap time and collapse if gap < eraCollapseSize
# event_cohorts_filtered2 <-
trace <- event_cohorts_filtered %>%
  mutate(duration = !!datediff("event_start_date", "event_end_date")) %>%
  filter(duration >= minEraDuration) %>%
  group_by(cohort_definition_id, subject_id, event_id) %>%
  dbplyr::window_order(event_start_date, event_start_date) %>%
  mutate(lag_end_date = lag(event_end_date)) %>%
  mutate(gap_days = !!datediff("lag_end_date", "event_start_date")) %>%
  mutate(new_event = if_else(is.na(gap_days) || gap_days > eraCollapseSize, 1L, 0L)) %>%
  mutate(event_counter = cumsum(new_event)) %>%
  group_by(cohort_definition_id, subject_id, event_id, event_counter) %>%
  mutate(new_event_start_date = min(event_start_date, na.rm = TRUE),
         new_event_end_date = max(event_end_date, na.rm = TRUE)) %>%
  ungroup()

# give user option to inspect the trace

preprocessed <- trace %>%
  mutate(event_id = as.character(as.integer(event_id))) %>%
  select(cohort_definition_id,
         subject_id,
         cohort_start_date,
         cohort_end_date,
         event_id,
         event_start_date = new_event_start_date,
         event_end_date = new_event_end_date) %>%
  distinct() %>%
  computeQuery()



# iteratively collapse ----------



collapseEras <- function(preprocessed, combinationWindow = 10) {

  # identify the first pair of records that overlap for each person. Flag them with filter_flag.
  qry <- preprocessed %>%
    # mutate(event_id = as.character(as.integer(event_id))) %>%
    group_by(cohort_definition_id, subject_id) %>%
    mutate(lag_event_end_date = lag(event_end_date, order_by = c(event_start_date, event_end_date))) %>%
    mutate(overlap_flag = case_when(event_start_date < lag_event_end_date ~ 1L, TRUE ~ 0L)) %>%
    group_by(cohort_definition_id, subject_id, overlap_flag) %>%
    dbplyr::window_order(cohort_definition_id, subject_id, event_start_date, event_end_date) %>%
    mutate(row_flag = if_else(overlap_flag*row_number() == 1L, 1L, 0L)) %>%
    group_by(cohort_definition_id, subject_id) %>%
    mutate(filter_flag = case_when(row_flag == 1L || lead(row_flag, order_by = c(event_start_date, event_end_date)) == 1L ~ 1L, TRUE ~ 0L)) %>%
    arrange(cohort_definition_id, subject_id, event_start_date, event_end_date) %>%
    select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, event_id, event_start_date, event_end_date, filter_flag) %>%
    ungroup() %>%
    computeQuery()

  # quality check
  check <- qry %>%
    filter(filter_flag == 1L) %>%
    count(cohort_definition_id, subject_id, name = "n_rows_per_person") %>%
    count(n_rows_per_person, name = "n_persons") %>%
    collect()

  if (nrow(check) == 0) {
    # no overlaps to process
    return(preprocessed)
  } else if (!all(check$n_rows_per_person == 2)) {
    print(check)
    rlang::abort("There should be two rows per person in the collapse step! Error with era collapse algorithm!")
  }

  # combinationWindow should be greater than min era length

  stopifnot(minEraDuration >= combinationWindow)

  # miaEraLength shoud be greater than or equal to conbination window

  combos <- qry %>%
    filter(filter_flag == 1) %>%
    select(-filter_flag) %>%
    group_by(cohort_definition_id, subject_id) %>%
    mutate(
      event_id_a = event_id,
      event_id_b = lead(event_id, order_by = c(event_start_date, event_end_date)),
      start_a = event_start_date, end_a = event_end_date,
      start_overlap = max(event_start_date, na.rm = TRUE),
      end_overlap = min(event_end_date, na.rm = TRUE),
      start_b = lead(event_start_date, order_by = c(event_start_date, event_end_date)),
      end_b = lead(event_end_date, order_by = c(event_start_date, event_end_date))) %>%
      filter(!is.na(event_id_b)) %>%
      select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, matches("_(a|b|overlap)$")) %>%
    mutate(overlap = !!datediff("start_overlap", "end_overlap"),
           event_id_overlap = paste(event_id_a, event_id_b, sep = "-")) %>%
    mutate(case = case_when(
      overlap < combinationWindow ~ "switch",
      overlap >= combinationWindow ~ "combo"
    )) %>%
    ungroup() %>%
    arrange(cohort_definition_id, subject_id) %>%
    computeQuery()

  output <- list(
    filter(qry, filter_flag == 0L) %>% select(-filter_flag),
    select(combos,
           cohort_definition_id,
           subject_id,
           cohort_start_date,
           cohort_end_date,
           event_id = event_id_a,
           event_start_date = start_a,
           event_end_date = end_a) %>%
      filter(!!datediff("event_start_date", "event_end_date") >= minEraDuration),
    select(combos,
           cohort_definition_id,
           subject_id,
           cohort_start_date,
           cohort_end_date,
           event_id = event_id_b,
           event_start_date = start_b,
           event_end_date = end_b) %>%
      filter(!!datediff("event_start_date", "event_end_date") >= minEraDuration),
    select(filter(combos, case == "combo"),
           cohort_definition_id,
           subject_id,
           cohort_start_date,
           cohort_end_date,
           event_id = event_id_overlap,
           event_start_date = start_overlap,
           event_end_date = end_overlap)
    ) %>%
    purrr::reduce(union) %>%
    distinct() %>%
    arrange(cohort_definition_id, subject_id, event_start_date, event_end_date) %>%
    computeQuery()

  return(output)
}


iterateEraCollapse <- function(x, maxIterations = 100) {
  for (i in seq_len(maxIterations)) {
    current_rowcount <- tally(x) %>% pull(n)
    print(glue::glue("Iteration {i} rowcount: {current_rowcount}"))
    x <- collapseEras(x)
    new_rowcount <- tally(x) %>% pull(n)
    if (current_rowcount == new_rowcount) {
      return(x)
    }
  }
}


x <- iterateEraCollapse(preprocessed)















