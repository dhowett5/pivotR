library(shiny)
library(readr)
library(tidyverse)
ui <- fluidPage(
  titlePanel("pivotR - Interactive pivoting between wide and long formats"),
  tabsetPanel(
    tabPanel("Background",
             mainPanel(
               img(src='justPivot.jpg', align = "center"),
               HTML('<br><h3>What is Long Format?</h3>'),
               HTML('<p>Long Format data is common with within subjects or repeated measures design.<br></p>'),
               HTML('<p>The following is an example, each participant undergoes multiple trials per condition e.g. trials 1-4 condition is fat-rich foods with kcal info, whereas trials 5-8 does not have kcal info.</p>'),
               HTML('<p>Each Participant has more than 1 row. This is the prefered modality for R and Python, but not for SPSS</p>'),
               img(src='longExampleNew.png', align = "center"),
               
               HTML('<h3>What is Wide Format?</h3>'),
               HTML('<p>Wide Format data is the preferred format for SPSS - Each participant has their own row</p>'),
               HTML('<b>Note</b> if you have repeated measures data this usually requires aggregating data within participant (mean, median etc)</p>'),

               img(src='wideExampleNew.png', align = "center"),
               HTML('<br><br>')
             )),
    tabPanel("How to: Long To Wide",
             mainPanel(
                     HTML('<h3>I have Long Data, how do i make it wide? </h3>'),
                     HTML('<ol><li>Import your data (lots of formats accepted including .sav)</li>
                     <li>Pivot Data: From Wide to Long</li>
                      <li>Values From: Choose the column that contains the numbers of your DV (in the example this is rating) </li>
                      <li>Names From: Choose the column(s) that you want to extract the labels from to create the new column (e.g. Condition & Kcal Info) </li>
                      <li>Participant IDs: Select the column with participant identifier </li>
                      <li>Aggregate Function: If you have multiple observations for each Condition you may need to aggregate each participants trials using a function (in this example the mean), or click noAggregation</li>
                      <li>Selected Cols/Vars: In this example we need to get rid of trial column (see uh oh! below) as the desired result has aggregated across trials so we do not need it </li>
                      <li>Click Pivvvottttt!</li>
                      <li>Download pivoted data </li>
                          </ol>'),
                     HTML("<br><b>Note:</b>If your uploaded data has a column of row numbers (it could be called something like '...1') remove this <i>before</i> upload"),
                     
                     HTML('<h3>Example</h3>'),
                     tags$a("Download Example Dataset", href = "longToWide_ExampleDataset.csv", 
                            download = "longToWide_ExampleDataset.csv", target = "_blank"),
                     
                     HTML("<br><br><p>This Example can be read as 'pivot from long to wide extracting values from 'Rating'. Make new columns from the levels in Condition and kcal_info <b> Aggregate using Mean </b>. Keep only the variables selected above (i.e. drop trial column)</p>"), 
                     HTML("Compare Original to Pivoted.</p><br>"),
                     img(src='longexample1new.png', align = "center"),
                     
             )
               ),
    tabPanel("How to: Wide to Long",
             mainPanel(
               HTML('<h3>I have Wide Data, how do i make it Long? </h3>'),
               
               HTML('<ol><li>Import your data (lots of formats accepted including .sav)</li>
               <li>Pivot Data: From Wide to Long</li>
               <li>Select Columns: Choose the columns that contains the variables with the data you want to pivot (e.g. variables Fat_True, Fat_False etc)</li>
               <li>Names to: Write what you want the new variables to be called <b>seperated by a comma</b> in the order they are written in the variable name (e.g. Condition<b>,</b> kcal_Info)</li>
               <li>Values to: Write what you want the new variable with the numbers in to be called</li>
               <li>Click Pivvvottttt!</li>
               <li>Download pivoted data </li>
                          </ol>'),
               HTML('<h3>Example</h3>'),
               tags$a("Download Example Dataset", href = "wideToLong_ExampleDataset.csv", 
                      download = "wideToLong_ExampleDataset.csv", target = "_blank"),
               HTML("<br><br><p>This Example can be read as 'pivot from wide to long extracting values from the columns Carb_False, Carb_True etc etc. Make new columns/variables from the names (Condition, kcal_Info) and a new column for the values (ratings) </p>"), 
               HTML("Compare Original to Pivoted.</p><br>"),
               img(src='wideexample1.png', align = "center"),
             )
    ),
                      
                      
    tabPanel("Import and Pivot Data",
             HTML('<h2>An interactive tool to pivot your data between wide and long formats</h2>'),
             
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose file to upload",
                           accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".rds", ".rda", ".sav")),
                 radioButtons("action", "Pivot data:",
                              choices = list("From long to wide" = "long_to_wide",
                                             "From wide to long" = "wide_to_long")),
                 uiOutput("long_var_selector"),
                 uiOutput("wide_var_selector"),
                 uiOutput("agg_function_selector"),
                 uiOutput("pptIDs"),
                 uiOutput("grpby_options"),
                 actionButton("pivot", "Pivot Data"),
                 br(), br(),
                 downloadButton("download_data", "Download Pivoted Data")
               ),
               mainPanel(
                 HTML('<h2>Original Data</h2>'),
                 tableOutput("original_data"),
                 HTML('<h2>Pivoted Data</h2>'),
                 tableOutput("pivoted_data")
               )
             )
    )
  )
)
# Define server for app
server <- function(input, output, session) {
  
  # Read in data
  dataset <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           "csv" = read_csv(input$file$datapath, col_types = cols()),
           "tsv" = read_tsv(input$file$datapath, col_types = cols()),
           "txt" = read_delim(input$file$datapath, delim = "\t", col_types = cols()),
           "xlsx" = read_excel(input$file$datapath),
           "xls" = read_excel(input$file$datapath),
           "rds" = readRDS(input$file$datapath),
           "rda" = readRDS(input$file$datapath),
           "sav" = haven::read_spss(input$file$datapath),
           stop("Unsupported file format. Please upload a .csv, .tsv, .txt, .xlsx, .xls, .rds, .rda or .sav file.")
    )
  })
  
  observe({
    if (!is.null(dataset())) {
      updateSelectizeInput(session, "select_cols", choices = names(dataset()))
    }
  })
  
  output$original_data <- renderTable({
    head(dataset())
  })
  
  pivoted_data <- eventReactive(input$pivot, {

    if (input$action == "long_to_wide") {
      if (input$agg_function == "NoAggregation") {
        if (input$grpby != "Keep All Variables") {
          keep_vars <- unique(c(input$values_from, unlist(input$names_from), input$pptIDs))
          
          pivoted_data <- dataset() %>%
          select(all_of(keep_vars)) %>%
          pivot_wider(names_from = input$names_from, values_from = input$values_from) 
        } else { 
          
          pivoted_data <- dataset() %>%
            pivot_wider(names_from = input$names_from, values_from = input$values_from) 
          }
      } else {
        
        if (input$grpby != "Keep All Variables") {
          pivoted_data <- dataset() %>%
            group_by(!!!syms(input$names_from),
                     !!!syms(input$pptIDs)) %>% 
            summarise(!!input$values_from := match.fun(input$agg_function)(!!sym(input$values_from))) %>% 
            ungroup() %>%
            pivot_wider(names_from = input$names_from, values_from = input$values_from)
          
        } else {
        pivoted_data <- dataset() %>%
          group_by(!!!syms(input$names_from),
                   !!!syms(input$pptIDs)) %>% 
          mutate(!!input$values_from := match.fun(input$agg_function)(!!sym(input$values_from))) %>% 
          ungroup() %>%
          pivot_wider(names_from = input$names_from, values_from = input$values_from)
        }
      }
    } else {
      dataset() %>% 
        pivot_longer(cols = input$selectedCols, 
                     names_to = "temp_names_to", 
                     values_to = input$values_to) %>%
        separate(col = temp_names_to, 
                 into = str_split(input$names_to, ",", simplify = TRUE), 
                 sep = "_", remove = TRUE, convert = FALSE, 
                 extra = "drop", fill = "right")
      
      }
    })
  
  # Show UI for selecting variables for pivoting from long to wide
  output$long_var_selector <- renderUI({
    if (input$action == "long_to_wide") {
      tagList(
        selectInput("values_from", "Values From:",
                    choices = names(dataset())),
        selectInput("names_from", "Names From:",
                    choices = names(dataset()), multiple = TRUE),
        selectInput("pptIDs", "Participant IDs",
                    choices = names(dataset()))
      )
    }
  })
  
  output$wide_var_selector <- renderUI({
    if (input$action == "wide_to_long") {
      tagList(
        selectInput("selectedCols", "Select Columns that you wish to pivot:",
                    choices = names(dataset()), multiple = TRUE),
        textInput("names_to", label = "Name of Category Column(s) (if > 1 seperate with comma):", value = ""),
        textInput("values_to", label = "Name of Value Column(s):", value = "")
      )
    }
  })
  
  # Show pivoted data
  output$pivoted_data <- renderTable({
    head(pivoted_data())
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("pivoted_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write_csv(pivoted_data(), file)
    }
  )
  output$grpby_options <- renderUI({
    if (input$action == "long_to_wide") {
      tagList(
        radioButtons("grpby", "Selected Columns/Variables", 
                     choices = c("Keep All Variables", "Only Keep the Variables Specified Above"))
      )
    }
  })
  
  find_mode <- function(x) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
  }
  
  output$agg_function_selector <- renderUI({
    if (input$action == "long_to_wide") {
      selectInput("agg_function", "Aggregate Function:", selected = 'NoAggregation',
                  choices = c("Counts" = "sum", "Mean" = "mean", 
                              "Median" = "median", "NoAggregation" = "NoAggregation"
                              ))
    }
  })
  
}



shinyApp(ui, server)