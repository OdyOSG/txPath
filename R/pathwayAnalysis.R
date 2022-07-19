#' Function to construct the treatment patterns
#'
#' @param treatment_history the dataframe output from create_treatment_history
#' @param analysisSettings a DrugUtilizationAnalysisSettings object that defines the elements of the analysis
#' @return a PathwayAnalysis object with the treatment patterns and attrition
#' @include utils.R
#' @export
create_treatment_patterns <- function(treatment_history,
                                      analysisSettings) {

  checkmate::assert_class(analysisSettings, "DrugUtilizationAnalysisSettings")
  checkmate::assert_data_table(treatment_history)

  minCellCount <- analysisSettings$minCellCount

  treatment_pathways <- treatment_history %>%
    tidyr::pivot_wider(id_cols = person_id,
                names_from = event_seq,
                names_prefix = "event_cohort_name",
                values_from = event_cohort_name)
  #get number of individuals
  numPersons <- nrow(treatment_pathways)

  #
  treatment_pathways <- treatment_pathways %>%
    dplyr::count(dplyr::across(tidyselect::starts_with("event_cohort_name"))) %>%
    dplyr::mutate(End = "end", .before = "n")


  numPathways <- nrow(treatment_pathways)

  treatment_pathways <- treatment_pathways %>%
    dplyr::filter(n >= minCellCount)

  numPersons2 <- sum(treatment_pathways$n)
  numPathways2 <- nrow(treatment_pathways)


  df <- data.frame(
    'TotalNumberOfPersons' = numPersons,
    'TotalNumberOfPathways' = numPathways,
    'minCellCount' = minCellCount,
    'FilteredNumberOfPersons' = numPersons2,
    'FilteredNumberOfPathways' = numPathways2
  )


  pathway_analysis <- structure(
    list(
      treatmentPathways = treatment_pathways,
      attrition = df
    ),
    class = "PathwayAnalysis")

  return(pathway_analysis)

}

#' Function to save the treatment patterns as multiple csv
#'
#' @param pathway_analysis a PathwayAnalysis object with information about the treatment patterns
#' @param analysisSettings a DrugUtilizationAnalysisSettings object that defines the elements of the analysis
#' @import usethis
#' @export
save_treatment_patterns <- function(treatment_patterns,
                                   analysisSettings) {

  treatment_pathways <- treatment_patterns$treatmentPathways
  pathway_attrition <- treatment_patterns$attrition

  treatmentPatternsFolder <- analysisSettings$outputFolder
  database <- analysisSettings$database

  #save treatment patterns to treatment patterns folder
  if(!dir.exists(file.path(treatmentPatternsFolder, database))) {
    dir.create(file.path(treatmentPatternsFolder, database), recursive = TRUE)
  }
  #save treatment_pathways
  readr::write_csv(treatment_pathways,
                   file = file.path(treatmentPatternsFolder, database,"treatmentPathways.csv"))
  usethis::ui_info(
    "data.frame of treatment pathways saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"treatmentPathways.csv\"))}"
  )

  #save pathway_attrition
  readr::write_csv(pathway_attrition,
                   file = file.path(treatmentPatternsFolder, database,"pathwayAttrition.csv"))
  usethis::ui_info(
    "data.frame of pathway attrition saved to: {ui_path(file.path(treatmentPatternsFolder, database, \"pathwayAttrition.csv\"))}"
  )

}

#' Function to plot the sankey for treatment patterns
#'
#' @param pathway_analysis a PathwayAnalysis object with information about the treatment patterns
#' @return a sankey diagram using networkD3::sankeyNetwork. We implement the lancet color schema from ggsci
#' @export
plot_treatment_patterns <- function(pathway_analysis, type = "sankey") {


  treatment_pathways <- pathway_analysis$treatmentPathways %>%
    dplyr::slice_max(n, n = 18)

  #set up data for sankey
  links <- treatment_pathways %>%
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_longer(cols = c(-row, -n),
                        names_to = 'column', values_to = 'source') %>%
    dplyr::mutate(column = match(column, names(treatment_pathways))) %>%
    tidyr::drop_na(source) %>%
    dplyr::mutate(source = paste0(source, '__', column)) %>%
    dplyr::group_by(row) %>%
    dplyr::mutate(target = lead(source, order_by = column)) %>%
    tidyr::drop_na(target, source) %>%
    dplyr::group_by(source, target) %>%
    dplyr::summarise(value = sum(n), .groups = 'drop')

  nodes <- data.frame(name = unique(c(links$source, links$target)))
  nodes <- data.table::data.table(nodes)
  links <- data.table::data.table(links)
  links$source <- match(links$source, nodes$name) - 1
  links$target <- match(links$target, nodes$name) - 1
  nodes$name <- sub('__[0-9]+$', '', nodes$name)
  links$type <- sub(' .*', '',
                    as.data.frame(nodes)[links$source + 1, 'name'])
  data.table::setkey(links, type)

  #create custom colors
  label <- unique(links$type)
  label2 <- paste0("'", paste(label, collapse = "','"), "',", "'end'")


  allCols <- c(ggsci::pal_lancet("lanonc")(8),
               ggsci::pal_lancet("lanonc", alpha = 0.45)(9))


  col <- allCols[seq_along(label)]
  col2 <- paste0("'", paste(col, collapse = "','"), "',", "'#1B1919FF'")



  myCol <- glue::glue('d3.scaleOrdinal() .domain([{label2}]) .range([{col2}])')


  #plot sankeyNetwork
  networkD3::sankeyNetwork(Links=links, Nodes=nodes, Source='source', Target='target',
                           Value='value', NodeID='name', fontSize=10, colourScale = myCol)
}
