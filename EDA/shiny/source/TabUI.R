#All tab UI Outputs
tabKPI <- function(idPlot,idTable) {
  tagList(
    fluidRow(
      tabBox(
        title=NULL,
        id = "kpisTabset", width=12,
        tabPanel(title=tagList(shiny::icon("line-chart"),"Donas"),
                 value="TSDash",
                 fluidRow(
                   box(title = 'Distribución de Categorías:',
                       width=12,
                       shinycssloaders::withSpinner(plotlyOutput(paste0(idPlot,
                                                                        "TS1")))
                   )
                 )
        ),
        tabPanel(title=tagList(shiny::icon("line-chart"),"Histogramas"),
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
        tabPanel(title=tagList(shiny::icon("line-chart"),"Barras"),
                 value="TS2Dash",
                 fluidRow(
                   box(title = 'Composición entre categorías',
                       width=12,
                       shinycssloaders::withSpinner(plotOutput(paste0(idPlot,
                                                                        "TSa1")))
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
        tabPanel(title=tagList(shiny::icon("line-chart"),"HeatMap"),
                 value="TS3Dash",
                 fluidRow(
                   box(title = '',
                       width=12,
                       shinycssloaders::withSpinner(plotOutput(paste0(idPlot,
                                                                      "TS"),
                                                               height = 1250))
                   )
                 )
                 
        ),
        tabPanel(title=tagList(shiny::icon("line-chart"),"Centrality"),
                 value="TS3Dash2",
                 fluidRow(
                   box(title = '',
                       width=12,
                       shinycssloaders::withSpinner(plotOutput(paste0(idPlot,
                                                                        "TSa"),
                                                                      height = 1250))
                   )
                 ),
                 fluidRow(
                   box(title = 'Tabla de la medida de Centralidad entre grafos:',
                       width=12,
                       shinycssloaders::withSpinner(DTOutput(paste0(idPlot,
                                                                    "outa")))
                   )
                 )
        ),  
        tabPanel(title=tagList(shiny::icon("line-chart"),"Betweeness"),
                        value="TS3Dash3",
                        fluidRow(
                          box(title = '',
                              width=12,
                              shinycssloaders::withSpinner(plotOutput(paste0(idPlot,
                                                                             "TSb"),
                                                                             height = 1250))
                          )),
                          fluidRow(
                            box(title = 'Tabla de la medida de Betweeness entre grafos:',
                                width=12,
                                shinycssloaders::withSpinner(DTOutput(paste0(idPlot,
                                                                               "outb")))
                            )
                        )
        
        )
      )
    )
  )
}
