# Test cases: Allen's interval algebra
# https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

# Assume there are two eras to be combined. They are different events.
# Without loss of generality, assume A always starts first or on the same day as B.

# A equals B
# A precedes B
# A meets B
# A overlaps with B
# A starts B
# A contains B
# A is finished by B

test_that("case: A is equal to B", {

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
                                    cdm_schema = "main",
                                    write_schema = "main",
                                    cohort_tables = c("target_cohort", "event_cohort"))

  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort")

  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
    100,           1,       "2020-01-01",     "2021-01-01", "101-102",      "2020-05-01",    "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  expect_equal(result, expected_result)

  # case when eras are too short
  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort", minEraDuration = 100)
  result <- dplyr::collect(cdm$treatment_patterns)
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.frame")

  DBI::dbDisconnect(con, shutdown = TRUE)
})


test_that("case: A precedes B", {

  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())

  target_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    100,                   1,           "2020-01-01",       "2021-01-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  event_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-06-02",       "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  DBI::dbWriteTable(con, "target_cohort", target_cohort)
  DBI::dbWriteTable(con, "event_cohort", event_cohort)

  cdm <- CDMConnector::cdm_from_con(con,
                                    cdm_schema = "main",
                                    write_schema = "main",
                                    cohort_tables = c("target_cohort", "event_cohort"))

  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort")

  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
                      100,           1,       "2020-01-01",     "2021-01-01",     "101",      "2020-05-01",    "2020-06-01",
                      100,           1,       "2020-01-01",     "2021-01-01",     "102",      "2020-06-02",    "2020-07-01"
    ) %>% dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  expect_equal(result, expected_result)

  # case when overlap is less than min era length
  # TODO prior ero should end before next one starts. Need to subtract one day.
  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort", minEraDuration = 500)
  result <- dplyr::collect(cdm$treatment_patterns)
  expect_equal(nrow(result), 0)

  DBI::dbDisconnect(con, shutdown = TRUE)
})


test_that("case: A meets B", {

  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())

  target_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    100,                   1,           "2020-01-01",       "2021-01-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  event_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-06-01",       "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  DBI::dbWriteTable(con, "target_cohort", target_cohort)
  DBI::dbWriteTable(con, "event_cohort", event_cohort)

  cdm <- CDMConnector::cdm_from_con(con,
                                    cdm_schema = "main",
                                    write_schema = "main",
                                    cohort_tables = c("target_cohort", "event_cohort"))

  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort")

  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,     ~event_id, ~event_start_date, ~event_end_date,
                      100,           1,       "2020-01-01",     "2021-01-01",         "101",      "2020-05-01",    "2020-05-31",
                      100,           1,       "2020-01-01",     "2021-01-01",     "101-102",      "2020-06-01",    "2020-06-01",
                      100,           1,       "2020-01-01",     "2021-01-01",         "102",      "2020-06-02",    "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  expect_equal(result, expected_result)

  # case when overlap is less than min era length
  # TODO prior ero should end before next one starts. Need to subtract one day.
  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort", minEraDuration = 5)
  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
                      100,           1,       "2020-01-01",     "2021-01-01",     "101",      "2020-05-01",    "2020-05-31",
                      100,           1,       "2020-01-01",     "2021-01-01",     "102",      "2020-06-01",    "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  expect_equal(result, expected_result)

  DBI::dbDisconnect(con, shutdown = TRUE)
})



test_that("case: A overlaps B", {

  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())

  target_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    100,                   1,           "2020-01-01",       "2021-01-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  event_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    101,                   1,           "2020-05-01",       "2020-06-20",
    102,                   1,           "2020-06-15",       "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  DBI::dbWriteTable(con, "target_cohort", target_cohort)
  DBI::dbWriteTable(con, "event_cohort", event_cohort)

  cdm <- CDMConnector::cdm_from_con(con,
                                    cdm_schema = "main",
                                    write_schema = "main",
                                    cohort_tables = c("target_cohort", "event_cohort"))

  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort")

  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
                      100,           1,       "2020-01-01",     "2021-01-01",     "101",      "2020-05-01",    "2020-06-14",
                      100,           1,       "2020-01-01",     "2021-01-01", "101-102",      "2020-06-15",    "2020-06-20",
                      100,           1,       "2020-01-01",     "2021-01-01",     "102",      "2020-06-21",    "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))


  expect_equal(result, expected_result)

  # case when overlap is less than min era length
  # TODO 102 event start should be 6-15 not 6-20
  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort", minEraDuration = 10)
  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
                      100,           1,       "2020-01-01",     "2021-01-01",     "101",      "2020-05-01",    "2020-06-14",
                      100,           1,       "2020-01-01",     "2021-01-01",     "102",      "2020-06-15",    "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  expect_equal(result, expected_result)

  DBI::dbDisconnect(con, shutdown = TRUE)
})


test_that("case: A starts B", {

  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())

  target_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    100,                   1,           "2020-01-01",       "2021-01-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  event_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-05-01",       "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  DBI::dbWriteTable(con, "target_cohort", target_cohort)
  DBI::dbWriteTable(con, "event_cohort", event_cohort)

  cdm <- CDMConnector::cdm_from_con(con,
                                    cdm_schema = "main",
                                    write_schema = "main",
                                    cohort_tables = c("target_cohort", "event_cohort"))

  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort")

  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
    100,           1,       "2020-01-01",     "2021-01-01", "101-102",      "2020-05-01",    "2020-06-01",
    100,           1,       "2020-01-01",     "2021-01-01",     "102",      "2020-06-02",    "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))


  expect_equal(result, expected_result)

  # case when overlap is less than min era length
  # TODO Not sure if this case is correct. What should the correct answer be in this case?
  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort", minEraDuration = 32)
  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
                      100,           1,       "2020-01-01",     "2021-01-01",     "102",      "2020-05-01",    "2020-07-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  expect_equal(result, expected_result)

  DBI::dbDisconnect(con, shutdown = TRUE)
})



test_that("case: A contains B", {

  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())

  target_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    100,                   1,           "2020-01-01",       "2021-01-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  event_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-05-11",       "2020-05-21"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  DBI::dbWriteTable(con, "target_cohort", target_cohort)
  DBI::dbWriteTable(con, "event_cohort", event_cohort)

  cdm <- CDMConnector::cdm_from_con(con,
                                    cdm_schema = "main",
                                    write_schema = "main",
                                    cohort_tables = c("target_cohort", "event_cohort"))

  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort")

  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
                      100,           1,       "2020-01-01",     "2021-01-01",     "101",      "2020-05-01",    "2020-05-10",
                      100,           1,       "2020-01-01",     "2021-01-01", "101-102",      "2020-05-11",    "2020-05-21",
                      100,           1,       "2020-01-01",     "2021-01-01",     "102",      "2020-05-22",    "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  expect_equal(result, expected_result)

  # case when overlap is less than min era length
  # TODO Not sure if this case is correct. What should the correct answer be in this case?
  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort", minEraDuration = 20)
  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
                      100,           1,       "2020-01-01",     "2021-01-01",     "101",      "2020-05-01",    "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  expect_equal(result, expected_result)

  DBI::dbDisconnect(con, shutdown = TRUE)
})



test_that("case: A is finished by B", {

  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())

  target_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    100,                   1,           "2020-01-01",       "2021-01-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  event_cohort <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    101,                   1,           "2020-05-01",       "2020-06-01",
    102,                   1,           "2020-05-21",       "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  DBI::dbWriteTable(con, "target_cohort", target_cohort)
  DBI::dbWriteTable(con, "event_cohort", event_cohort)

  cdm <- CDMConnector::cdm_from_con(con,
                                    cdm_schema = "main",
                                    write_schema = "main",
                                    cohort_tables = c("target_cohort", "event_cohort"))

  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort")

  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
    100,           1,       "2020-01-01",     "2021-01-01",     "101",      "2020-05-01",    "2020-05-20",
    100,           1,       "2020-01-01",     "2021-01-01", "101-102",      "2020-05-21",    "2020-06-01",
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  expect_equal(result, expected_result)

  # case when overlap is less than min era length
  # TODO Not sure if this case is correct. What should the correct answer be in this case?
  cdm <- computeTreatmentPathways(cdm, "target_cohort", "event_cohort", minEraDuration = 30)
  result <- dplyr::collect(cdm$treatment_patterns)

  expected_result <- tibble::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date, ~event_id, ~event_start_date, ~event_end_date,
    100,           1,       "2020-01-01",     "2021-01-01",     "101",      "2020-05-01",    "2020-06-01"
  ) %>%
    dplyr::mutate(dplyr::across(ends_with("date"), as.Date))

  expect_equal(result, expected_result)

  DBI::dbDisconnect(con, shutdown = TRUE)
})


