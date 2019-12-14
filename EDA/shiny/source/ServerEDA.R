# Univariado
output$kpiPlotTS1 <- renderPlotly({
# browser()
  req(input$kpiTScat)
  
  x <- input$kpiTScat
  
 gr <- data %>% 
    select_if(names(.)==x) %>% 
    purrr::set_names(nm = c('x')) %>%
   mutate(id = 1:n()) %>% 
    group_by(x) %>%
    tally() %>% 
   rename( count = n)
 
 gr%>%
    plot_ly(labels = ~x, values = ~count) %>%
    add_pie(hole = 0.6) %>%
    layout(title = paste("Composición de la variable:", 
                         input$kpiTScat),  
                         showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                        showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                        showticklabels = FALSE))
  
})

output$kpiPlotTS2 <- renderPlot({
   # browser()
  req(input$kpiTSnum)
  x <- input$kpiTSnum
  
vect <- data %>% 
  select_if(names(.)==x) %>% 
  purrr::set_names(nm = c('x')) 


vect %>%   
  ggplot(aes(x= x))+
    geom_histogram(stat = 'count')+
  labs(title = paste('Variable:', x))
  
})

# Bivariado


output$kpiPlot2TSa1 <- renderPlot({
  # browser()
  req(input$kpiTScat1)
  req(input$kpiTScat2)
  
  x <- input$kpiTScat1
  y <- input$kpiTScat2
  
  data %>% 
    select_if(names(.)%in% c(x,y)) %>% 
    set_names(nm = c('x','y')) %>%
    group_by(x,y) %>% 
    tally %>% 
    ggplot(aes(x = x, y = n))+
    geom_col(aes(fill = y), width = 0.7)+
    theme_minimal()+
    labs(title = 'Composición de variables categóricas',
         x = paste(input$kpiTScat1),
         y = 'conteo',
         fill = paste(input$kpiTScat2))+
    theme(axis.text.x = element_text(angle = 90))
  
 
  
})

output$kpiPlot2TSb2 <- renderPlot({
   # browser()
  req(input$kpiTSnum1)
  req(input$kpiTSnum2)
  
  x <- input$kpiTSnum1
  y <- input$kpiTSnum2
  
  # data %>% 
  #   select_if(names(.)%in% c(x,y)) %>% 
  #   set_names(nm = c('x','y')) %>%
  # 
  vect <- data %>% 
    select_if(names(.)%in% c(x,y)) %>% 
    set_names(nm = c('x','y')) 
  
  with(vect, hdr.boxplot.2d(x, y,
                              show.points=TRUE, prob=c(0.01,0.05,0.5,0.75)))
  
})

output$kpiPlot2TSa2 <- renderPlot({
  # browser()

  req(input$kpiTSnum1)
  req(input$kpiTSnum2)
  
  x <- input$kpiTSnum1
  y <- input$kpiTSnum2
  
  data %>% 
    select_if(names(.)%in% c(x,y)) %>% 
    set_names(nm = c('x','y')) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = 'lm')+
    theme_minimal()+
    labs(title = 'Gráfica de dispersión para variables numéricas',
         x = paste(input$kpiTSnum1),
         y = paste(input$kpiTSnum2))
  
  
})

output$kpiPlot2TSa3 <- renderPlotly({
    # browser()
  req(input$kpiTSmix1)
  req(input$kpiTSmix2)
  categorica <- input$kpiTSmix1
  numerica <- input$kpiTSmix2
  
  x <- list(
    title = paste('Variable categótica:', input$kpiTSmix1),
    titlefont = f
  )
  
  y <- list(
    title = paste('Variable numérica:', input$kpiTSmix2),
    titlefont = f
  )
 
  plot_ly(data = data, y = ~eval(parse_expr(numerica)),
          color = ~eval(parse_expr(categorica)),
          type = "box") %>%
    layout(xaxis = x, yaxis = y)
  
})

#Multivariado
output$kpiPlot3TSa <- renderPlot({
   # browser()
  req(input$kpiTSmixa1)
  req(input$kpiTSmixa2)
  req(input$kpiTSmixa3)
  req(input$kpiTSmixa4)
  
  graf <- data %>% 
    select_if(names(.)%in% c(input$kpiTSmixa1,
                             input$kpiTSmixa2,
                             input$kpiTSmixa3,
                             input$kpiTSmixa4)) 
  
  ggpairs(graf)+
    theme_minimal()+
    labs(title ='SPLOM')+
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 45, hjust = 1))
})