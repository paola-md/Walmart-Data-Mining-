##############All dashboard structure is Dynamic###############
dashboardPage(
  dashboardHeader(title = "EDA"),
  dashboardSidebar(
    tags$head(
      includeCSS('www/styles.css')
      
    ),
    ############Dynamic Menu################ 
    sidebarMenu( id= "sidebarMenu",
                 sideMenuKPI(idTS="kpiTS",idTop="kpiTop"))),
  dashboardBody(
    tabItems(
      tabItem("kpisNav", tabKPI(idPlot="kpiPlot",idTable="kpiTable")),
      tabItem("kpisNav2", tabKPI2(idPlot="kpiPlot2",idTable="kpiTable2")),
      tabItem("kpisNav3", tabKPI3(idPlot="kpiPlot3",idTable="kpiTable3"))
     
    )
  )
)
#roboto