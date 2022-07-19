
#' A function to create an age strata
#'
#' @param name the name of the strata
#' @param op the operator for the strata
#' @param value the value for the age strata
#' @return an age strata object
#' @export
create_age_stratification <- function(name, op, value) {

  ageStrata <- list(name = name,
                    op = op,
                    value = value)
  ageStrata$call <- rlang::call2("age_strata",
                                 treatment_history = sym("treatment_history"),
                                 connectionDetails = sym("connectionDetails"),
                                 cdmDatabaseSchema = sym("cdmDatabaseSchema"),
                                 strata = sym(".x"))
  ageStrata$varName <- paste("age", name, sep = "_")
  structure(ageStrata, class = "age_strata")
}

#' A function to create a date strata
#'
#' @param name the name of the strata
#' @param op the operator for the strata
#' @param value the value for the date strata
#' @return a date strata object
#' @export
create_date_stratification <- function(name, op, value) {
  dateStrata <- list(name = name,
                     op = op,
                     value = as.Date(value))
  dateStrata$call <- rlang::call2("date_strata",
                                  treatment_history = sym("treatment_history"),
                                  strata = sym(".x"))
  dateStrata$varName <- paste("date", name, sep = "_")
  structure(dateStrata, class = "date_strata")
}

#' A function to create a concept strata
#'
#' @param name the name of the strata
#' @param domain the domain of the concept
#' @param concept the OMOP concept id for the strata
#' @return a concept strata object
#' @export
create_concept_stratification <- function(name,
                                          domain,
                                          concept) {

  conceptStrata <- list(name = name,
                         domain = domain,
                         concept = concept)
  conceptStrata$call <- rlang::call2("concept_strata",
                                    treatment_history = sym("treatment_history"),
                                    connectionDetails = sym("connectionDetails"),
                                    cdmDatabaseSchema = sym("cdmDatabaseSchema"),
                                    strata = sym(".x"))
  conceptStrata$varName <- paste(name, concept, sep = "_")

  structure(conceptStrata, class = "concept_strata")
}

#' #' A function to create a cohort strata
#' #'
#' #' @param name the name of the strata
#' #' @param concept the OMOP concept id for the strata
#' #' @return a concept strata object
#' #' @export
#' create_cohort_stratification <- function(name,
#'                                          cohort) {
#'   structure(list(name = name, cohort = cohort), class = "cohortStrata")
#' }

#' A function to create strata list object
#'
#' @param ... a list of strata objects
#' @return a strata list object
#' @export
stratification_list <- function(...){
  structure(list(...), class = "strataList")
}

age_strata <- function(treatment_history,
                       connectionDetails,
                       cdmDatabaseSchema,
                       strata) {

  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  logicTest <- rlang::call2(strata$op, sym("age"), strata$value)

  personIds <- unique(treatment_history$person_id)
  sql1 <- "SELECT person_id, year_of_birth, month_of_birth
           FROM @cdmDatabaseSchema.person
           WHERE person_id IN (@person_id)"

  th <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql1,
    cdmDatabaseSchema = cdmDatabaseSchema,
    person_id = personIds,
    snakeCaseToCamelCase = TRUE) %>%
    mutate(
      monthOfBirth = stringr::str_pad(monthOfBirth, 2, side = "left", pad = "0"),
      dob = lubridate::mdy(paste(monthOfBirth, "01", yearOfBirth, sep = "/"))
    ) %>%
    select(personId, dob) %>%
    dplyr::right_join(treatment_history, c("personId" = "person_id")) %>%
    dplyr::mutate(
      age = floor(lubridate::time_length(difftime(event_start_date, dob), "years")),
      ageStrata = ifelse(eval(logicTest), "Y", "N")
    )%>%
    dplyr::pull(ageStrata)
  return(th)
}



concept_strata <- function(treatment_history,
                       connectionDetails,
                       cdmDatabaseSchema,
                       strata) {

  suppressMessages(connection <- DatabaseConnector::connect(connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection))

  concept <- paste0(strata$name, "ConceptId")
  concept_sym <- sym(concept)
  concept2 <- SqlRender::camelCaseToSnakeCase(concept)
  logicTest <- rlang::call2("==", sym(concept), strata$concept)
  #varName <- paste(strata$name, strata$concept, sep = "_")


  personIds <- unique(treatment_history$person_id)
  sql1 <- "SELECT person_id, @domain_concept_id
           FROM @cdmDatabaseSchema.@domain
           WHERE person_id IN (@person_id)"

  th <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql1,
    cdmDatabaseSchema = cdmDatabaseSchema,
    person_id = personIds,
    domain = strata$domain,
    domain_concept_id = concept2,
    snakeCaseToCamelCase = TRUE) %>%
    select(personId, !!concept_sym) %>%
    dplyr::right_join(treatment_history, c("personId" = "person_id")) %>%
    dplyr::mutate(
      concept = ifelse(eval(logicTest), "Y", "N")
    ) %>%
    dplyr::pull(concept)

  return(th)
}


date_strata <- function(treatment_history,
                       strata) {

  logicTest <- rlang::call2(strata$op, sym("event_start_date"), strata$value)


  th <- treatment_history %>%
    dplyr::mutate(
      dateStrata = ifelse(eval(logicTest), "Y", "N")
    ) %>%
    dplyr::pull(dateStrata)

  return(th)
}

