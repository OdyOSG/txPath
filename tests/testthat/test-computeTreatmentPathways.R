test_that("test case: A is equal to B", {

  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())

  target_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    100,                   1,           "2020-01-01",       "2021-01-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  event_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-05-01",       "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  DBI::dbWriteTable(con, "target_cohort", target_cohort)
  DBI::dbWriteTable(con, "event_cohort", event_cohort)

  cdm <- CDMConnector::cdm_from_con(con,
                                    "main",
                                    cohort_tables = c("target_cohort", "event_cohort"))

  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort")

  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
    100,           1,       "2020-01-01",     "2021-01-01", "101-102",      "2020-05-01",    "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  expect_equal(result, expected_result)

  DBI::dbDisconnect(con, shutdown = TRUE)
})
