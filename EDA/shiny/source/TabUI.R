#All tab UI Outputs
tabKPI <- function(idPlot,idTable) {
  tagList(
    fluidRow(
      tabBox(
        title=NULL,
        id = "kpisTabset", width=12,
        tabPanel(title=tagList(shiny::icon("line-chart"),"Categórico"),
                 value="TSDash",
                 fluidRow(
                   box(title = 'plotly:',
                       width=12,
                       shinycssloaders::withSpinner(plotlyOutput(paste0(idPlot,
                                                                        "TS1")))
                   )
                 )
        ),
        tabPanel(title=tagList(shiny::icon("line-chart"),"Histogramas:"),
                 value="TS1Dash2",
                 fluidRow(
                   box(title = 'Distribución valores:',
                       width=12,
                       shinycssloaders::withSpinner(plotOutput(paste0(idPlot,
                                                                        "TS2")))
                   )
                 )
                 
        )
      )
    )
  )
}

tabKPI2 <- function(idPlot,idTable) {
  tagList(
    fluidRow(
      tabBox(
        title=NULL,
        id = "kpisTabset2", width=12,
        tabPanel(title=tagList(shiny::icon("line-chart"),"Pie"),
                 value="TS2Dash",
                 fluidRow(
                   box(title = 'ggplot:',
                       width=12,
                       shinycssloaders::withSpinner(plotOutput(paste0(idPlot,
                                                                        "TSa1")))
                   )
                 )
        ),
        tabPanel(title=tagList(shiny::icon("line-chart"),"Histogramas"),
                 value="TS2Dash2",
                 fluidRow(
                   box(title = 'ggplot:',
                       width=12,
                       shinycssloaders::withSpinner(plotOutput(paste0(idPlot,
                                                                        "TSa2")))
                   )
                 )
                 
        ),
        tabPanel(title=tagList(shiny::icon("line-chart"),"Mixto"),
                 value="TS2Dash3",
                 fluidRow(
                   box(title = 'plotly:',
                       width=12,
                       shinycssloaders::withSpinner(plotlyOutput(paste0(idPlot,
                                                                        "TSa3")))
                   )
                 )
                 
        )
      )
    )
  )
}

tabKPI3 <- function(idPlot,idTable) {
  tagList(
    fluidRow(
      tabBox(
        title=NULL,
        id = "kpisTabset3", width=12,
        tabPanel(title=tagList(shiny::icon("line-chart"),"Mixto"),
                 value="TS3Dash",
                 fluidRow(
                   box(title = 'ggplot:',
                       width=12,
                       shinycssloaders::withSpinner(plotOutput(paste0(idPlot,
                                                                        "TSa")))
                   )
                 )
        )
      )
    )
  )
}
