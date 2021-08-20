
deleteUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("deleteTable"), "Remove whole table", class = "pull-right btn-danger"),
    selectInput(ns("tableName"), "Choose a table", character(0)),
    actionButton(ns("deleteRows"), "Remove selected rows", class = "pull-right btn-warning"),
    selectInput(ns("col"), "Choose column to filter on", NULL),
    checkboxGroupInput(ns("vals"), "Choose values to include (will remove whole row)"),
    box(title = "Selected rows (to be deleted)", status = "warning", 
      solidHeader = TRUE, width = 12,
        tableOutput(ns("current"))
    )
  )
}

delete <- function(input, output, session, pool, reqTable, reqColInTable, goHome) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  observe({
    reqTable(input$tableName)
    cols <- db_query_fields(pool, input$tableName)
    updateSelectInput(session, "col", choices = cols)
  })
  
  observe({
    reqColInTable(input$tableName, input$col)
    query <- sqlInterpolate(pool, 
                            "SELECT COUNT(*) FROM ?table",
                            .dots = list(table = input$tableName)
    )
    req(dbGetQuery(pool, query) > 0)
    
    sql <- "SELECT ?col FROM ?table"
    query <- sqlInterpolate(pool, sql, .dots = c(
      list(table = input$tableName,
           col = sym(input$col))
    ))
    df <- dbGetQuery(pool, query)
    allUniqueVals <- unique(df[[input$col]])
    updateCheckboxGroupInput(session, "vals", choices = allUniqueVals, inline = TRUE)
  })
  
  output$current <- renderTable({
    reqTable(input$tableName)
    req(input$col)
    req(input$vals)
    
    sql <- "SELECT * FROM ?table WHERE ?col IN (?vals)"
    vals <- lapply(input$vals, function(x) dbQuoteString(pool, x))
    query <- sqlInterpolate(pool, sql, .dots = c(
      list(table = input$tableName,
           col = sym(input$col),
           vals = sym(toString(vals)))
    ))
    dbGetQuery(pool, query)
  })
  
  observeEvent(input$deleteTable, {
    dbRemoveTable(pool, input$tableName)
    goHome()
  })
  
  observeEvent(input$deleteRows, {
    
    col <- if (input$col %in% dbListFields(pool, input$tableName)) {
      input$col
    } else {
       showModal(modalDialog(
          title = "Invalid column name",
          "The selected column must be a column of the DB table",
          easyClose = TRUE, footer = NULL
        ))
        return()
    }
    
    df <- as_data_frame(pool %>% tbl(input$tableName) %>% select(col))
    allUniqueVals <- unique(df[[col]])
    results <- lapply(as_list(input$vals), `%in%`, allUniqueVals)
    
    vals <- if (all(results)) {
      input$vals
    } else {
       showModal(modalDialog(
          title = "Invalid column values",
          "The selected values do not exist in the selected table column",
          easyClose = TRUE, footer = NULL
        ))
        return()
    }
    
    sql <- paste0("DELETE FROM ?table WHERE ", col, " IN (",
      paste0(vals, collapse = ", "), ");")
    
    query <- sqlInterpolate(pool, sql, table = input$tableName)
    
    dbExecute(pool, query)
    goHome()
  })
}
