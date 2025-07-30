library(shiny)
library(shinyWidgets)
library(dplyr)
library(stringr)
library(DT)
library(bslib)
library(shinycssloaders)
library(words)
data("words")
# Rename for clarity
words_data <- words


ui <- fluidPage(
  theme = bs_theme(
    version    = 5,
    bootswatch = "minty",
    bg         = "#f0f2f5",
    fg         = "#2c3e50",
    primary    = "#1abc9c",
    secondary  = "#34495e",
    base_font  = font_google("Lato"),
    code_font  = font_google("Source Code Pro")
  ),
  tags$head(
    tags$style(HTML(
      ".filter-box { background: #ffffff; padding: 25px; border-radius: 12px; box-shadow: 0 6px 20px rgba(0,0,0,0.05); }
 .title-panel { margin: 30px 0 20px; text-align: center; color: #1abc9c; font-size: 2.8em; font-weight: 700; }
 .data-table-wrapper { background: #ffffff; padding: 20px; border-radius: 12px; box-shadow: 0 6px 20px rgba(0,0,0,0.05); margin-top: 20px; }"
    ))
  ),
  div(class = "title-panel", "Spelling Bee Solver"),
  sidebarLayout(
    sidebarPanel(
      div(class = "filter-box",
          pickerInput(
            inputId = "allowed",
            label   = tagList(icon("font"), "Allowed Letters"),
            choices = letters,
            multiple  = TRUE,
            selected  = strsplit("e,g,o,a,t,n", ",")[[1]],
            options   = pickerOptions(liveSearch = TRUE)
          ),
          textInputIcon(
            inputId = "key",
            label   = tagList(icon("key"), "Key Letter"),
            value   = "c"
          ),
          numericInputIcon(
            inputId = "minlen",
            label = tagList(icon("sort-numeric-asc"), "Minimum Word Length"),
            value = 4,
            min   = 1,
            step  = 1
          )
      ),
      width = 4
    ),
    mainPanel(
      div(class = "data-table-wrapper",
          shinycssloaders::withSpinner(
            DTOutput("table"),
            type  = 6,
            color = "#1abc9c"
          )
      ),
      width = 8
    )
  )
)


app_server <- function(words_data) {
  function(input, output, session) {
    filtered <- reactive({
      req(input$allowed, input$key)
      allowed_set <- unique(c(input$key, input$allowed))
      forbid_rx   <- paste0("^[", paste0(allowed_set, collapse = ""), "]+$")
      words_data %>%
        filter(
          nchar(word) >= input$minlen,
          str_detect(word, fixed(input$key)),
          str_detect(word, regex(forbid_rx))
        ) %>%
        select(word)
    })
    
    output$table <- renderDT({
      datatable(
        filtered(),
        options  = list(pageLength = 10, dom = 't<"bottom"ip>'),
        rownames = FALSE
      )
    })
  }
}


shinyApp(ui = ui, server = app_server(words_data))
