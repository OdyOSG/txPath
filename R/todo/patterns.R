
patternAttrition <- function(th,
                             minNumPatterns) {
  tp <- th %>%
    tidyr::pivot_wider(id_cols = person_id,
                       names_from = event_seq,
                       names_prefix = "event_cohort_name",
                       values_from = event_cohort_name)
  #get number of individuals
  numPersons <- nrow(tp)

  #get number of patterns
  tp <- tp %>%
    dplyr::count(dplyr::across(tidyselect::starts_with("event_cohort_name"))) %>%
    dplyr::mutate(End = "end", .before = "n")


  numPathways <- nrow(tp)

  tp <- tp %>%
    dplyr::filter(n >= minNumPatterns)

  numPersons2 <- sum(tp$n)
  numPathways2 <- nrow(tp)

  df <- tibble::tibble(
    'StartingNumberOfPersons' = numPersons,
    'StartingNumberOfPathways' = numPathways,
    'minPatternCount' = minNumPatterns,
    'FilteredNumberOfPersons' = numPersons2,
    'FilteredNumberOfPathways' = numPathways2
  )

  return(df)

}

prepSankey <- function(treatment_pathways) {

  links <- treatment_pathways %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = c(-row, -n),
                        names_to = 'column', values_to = 'source') %>%
    dplyr::mutate(column = match(column, names(treatment_pathways))) %>%
    tidyr::drop_na(source) %>%
    dplyr::mutate(source = paste0(source, '__', column)) %>%
    dplyr::group_by(row) %>%
    dplyr::mutate(target = dplyr::lead(source, order_by = column)) %>%
    tidyr::drop_na(target, source) %>%
    dplyr::group_by(source, target) %>%
    dplyr::summarise(value = sum(n), .groups = 'drop') %>%
    dplyr::arrange(desc(value))

  nodes <- data.frame(name = unique(c(links$source, links$target)))
  nodes <- data.table::data.table(nodes)
  links <- data.table::data.table(links)
  links$source <- match(links$source, nodes$name) - 1
  links$target <- match(links$target, nodes$name) - 1
  nodes$name <- sub('__[0-9]+$', '', nodes$name)
  links$type <- sub(' .*', '',
                    as.data.frame(nodes)[links$source + 1, 'name'])
  data.table::setkey(links, type)
  data.table::setorder(links, cols = - "value")

  res <- list(
    'links' = links,
    'nodes' = nodes
  )

  return(res)

}

#' Function to determine treatment patterns
#' @description this function aggregates the treatment history into a
#' treatment patterns analysis. The function returns information on the attrition,
#' a data structure for a sankey diagram and a clean table of the treatment patterns.
#'
#' The treatment patterns output can be saved or used to create a sankey diagram
#'
#' @param th the treatment history data.frame
#' @param minNumPatterns the minimum number of patterns needed to remain as a eligible pathway
#' @export
findPatterns <- function(th, minNumPatterns = 30) {

  #get attrition
  attrition <- patternAttrition(th, minNumPatterns = minNumPatterns)

  # get patterns
  treatment_pathways <- th %>%
    tidyr::pivot_wider(id_cols = person_id,
                       names_from = event_seq,
                       names_prefix = "event_cohort_name",
                       values_from = event_cohort_name) %>%
    dplyr::count(dplyr::across(tidyselect::starts_with("event_cohort_name"))) %>%
    dplyr::mutate(End = "end", .before = "n") %>%
    dplyr::filter(n >= minNumPatterns)

  #prep sankey
  sankeyDat <- prepSankey(treatment_pathways)

  #clean patterns
  clean <- treatment_pathways %>%
    dplyr::select(-End) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::mutate(seq = paste(event_cohort_name1, event_cohort_name2, event_cohort_name3)) %>%
    dplyr::mutate(seq = gsub(" NA", "", seq),
           seq = gsub(" ", " -> ", seq)) %>%
    dplyr::select(seq, n)


  ll <- structure(
    list(
      'sankey' = sankeyDat,
      'treatmentPatterns' = clean,
      'attrition' = attrition
    ), class = "treatmentPatterns"
  )

  return(ll)
}

#' Function to plot sankey
#' @description a sankey diagram describing the treatment patterns
#' @param treatmentPatterns object to use for sankey
#' @export
plot_patterns <- function(treatmentPatterns) {

  links <- treatmentPatterns$sankey$links
  nodes <- treatmentPatterns$sankey$nodes

  label <- unique(links$type)
  label2 <- paste0("'", paste(label, collapse = "','"), "',", "'end'")


  martin_colors <- unname(colorBlindness::paletteMartin)[-1]


  col <- martin_colors[seq_along(label)]
  col2 <- paste0("'", paste(col, collapse = "','"), "',", "'#1B1919FF'")



  myCol <- glue::glue('d3.scaleOrdinal() .domain([{label2}]) .range([{col2}])')

  #plot sankeyNetwork
  sankey <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = 'source',
    Target = 'target',
    Value = 'value',
    NodeID = 'name',
    fontSize = 11,
    colourScale = myCol
  )
  return(sankey)


}
