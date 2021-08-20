
overviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Current tables in database"),
    uiOutput(ns("tables"))
  )
}

overview <- function(input, output, session, pool) {
  output$tables <- renderUI({
    all_tables <- tbls()
    bullets <- list()
    for (i in seq_len(length(all_tables))) {
      tblName <- all_tables[[i]]
      fieldNames <- db_query_fields(pool, tblName)
      query <- sqlInterpolate(pool, 
                              "SELECT COUNT(*) FROM ?table",
                              .dots =list(table = tblName)
      )
      nRows <- dbGetQuery(pool, query)
      bullets[[i]] <- tags$li(paste0(
        all_tables[[i]], ": ", nRows, " rows. Field names: ", 
        paste(fieldNames, collapse = ", "), "."
      ))
    }
    tags$ul(bullets)
  })
}
