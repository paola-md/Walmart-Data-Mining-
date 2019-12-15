############-------------- Univariado
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
                        showticklabels = FALSE)) %>%
   layout(legend = list(orientation = 'h'))
  
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

##################--------- Bivariado


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

####################------------Multivariado

#Heatmap
corruki <- reactive({
  req(input$kpiTSheat1)
  req(input$kpiTSheat2)
  req(input$kpiTSheat3)
  
 tt <-  data %>% 
   select(contains(input$kpiTSheat1),contains(input$kpiTSheat2),
          contains(input$kpiTSheat3)) %>% 
   purrr::set_names(nm = c('x','y','z')) 

if(input$kpiTSheat3 %in%  c('visita_ticket','producto_identificador',
                             'producto_categoria')){
tt %<>% 
   group_by(y, x) %>% 
   # summarise(val = sum(producto_cantidad,na.rm =T)) %>%
   summarise(val = n_distinct(z)) %>% 
   ungroup %>% 
   group_by(y) %>% 
   mutate(val = val/sum(val)) %>% 
   ungroup %>% 
   mutate(x = str_replace_all(x,'(\\s+)|(-)','_')) %>% 
   spread(x, val, fill = 0) 
  
}else{
  
  tt %<>% 
    group_by(y, x) %>% 
    summarise(val = sum(z,na.rm =T)) %>%
    ungroup %>% 
    group_by(y) %>% 
    mutate(val = val/sum(val)) %>% 
    ungroup %>% 
    mutate(x = str_replace_all(x,'(\\s+)|(-)','_')) %>% 
    spread(x, val, fill = 0) 
  
}
 
corruki <- tt %>%
   .[,-1]%>%  
   cor() %>% 
   as.data.frame() %>% 
   rownames_to_column(var = 'var1') %>% 
   gather(var2, val, -var1) %>% 
   unique %>% 
   filter(val > input$kpiTSheat4)
 
corruki
})

output$kpiPlot3TS <- renderPlot({

req(input$kpiTSheat4)
  
  graf <- corruki() 
  
  graf %>% 
    ggplot( aes(var1, var2)) + # x and y axes => Var1 and Var2
    geom_tile(aes(fill = val)) + # background colours are mapped according to the value column
    # geom_text(aesX(fill = correlation$val, label = round(correlation$val, 2))) + # write the values
    scale_fill_gradient2(low = muted("darkred"), 
                         mid = "white", 
                         high = muted("midnightblue"), 
                         midpoint = 0) + # determine the colour
    theme(panel.grid.major.x=element_blank(), #no gridlines
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.y=element_blank(), 
          panel.grid.minor.y=element_blank(),
          panel.background=element_rect(fill="white"), # background=white
          axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
          plot.title = element_text(size=20,face="bold"),
          axis.text.y = element_text(size = 12,face = "bold")) + 
    ggtitle("Mapa de Calor") + 
    theme(legend.title=element_text(face="bold", size=14),
          legend.position = 'bottom') + 
    scale_x_discrete(name="") +
    scale_y_discrete(name="") +
    labs(fill="Medida de Asociación")
  

})

#Grafos Centrality
corruki2 <- reactive({
  req(input$kpiTSgra1)
  req(input$kpiTSgra2)
  req(input$kpiTSgra3)
  
  tt <-  data %>% 
    select(contains(input$kpiTSgra1),contains(input$kpiTSgra2),
           contains(input$kpiTSgra3)) %>% 
    purrr::set_names(nm = c('x','y','z')) 
  
  if(input$kpiTSgra3 %in%  c('visita_ticket','producto_identificador',
                              'producto_categoria')){
    tt %<>% 
      group_by(y, x) %>% 
      # summarise(val = sum(producto_cantidad,na.rm =T)) %>%
      summarise(val = n_distinct(z)) %>% 
      ungroup %>% 
      group_by(y) %>% 
      mutate(val = val/sum(val)) %>% 
      ungroup %>% 
      mutate(x = str_replace_all(x,'(\\s+)|(-)','_')) %>% 
      spread(x, val, fill = 0) 
    
  }else{
    
    tt %<>% 
      group_by(y, x) %>% 
      summarise(val = sum(z,na.rm =T)) %>%
      ungroup %>% 
      group_by(y) %>% 
      mutate(val = val/sum(val)) %>% 
      ungroup %>% 
      mutate(x = str_replace_all(x,'(\\s+)|(-)','_')) %>% 
      spread(x, val, fill = 0) 
    
  }
  
  corruki <- tt %>%
    .[,-1]%>%  
    cor() %>% 
    as.data.frame() %>% 
    rownames_to_column(var = 'var1') %>% 
    gather(var2, val, -var1) %>% 
    unique %>% 
    filter(val > input$kpiTSgra4)
  
  corruki
})

output$kpiPlot3TSa <- renderPlot({
  req(input$kpiTSgra1)
  req(input$kpiTSgra2)
  req(input$kpiTSgra3)
  req(input$kpiTSgra4)
   # browser()
  correlation <- corruki2() 
  
  graf <- correlation %>% 
    as_tbl_graph() %>%
    mutate(importancia = centrality_degree(mode = "all")) %>%
    activate(nodes) %>%
    mutate(centrality = centrality_betweenness(directed = FALSE),
           nombre = unique(correlation%>%
                             select(var1) %>%
                             rbind(correlation%>%
                                     select(var1 = var2)) %>% .$var1))
  
  
  graficar_red_nd(graf, paste(input$kpiTSgra2,'respecto a',input$kpiTSgra1))
  
})

output$kpiPlot3outa <- renderDT({
  corruki2() %>% filter(val != 1)
})



#Grafos betweenes
corruki3 <- reactive({
  req(input$kpiTSph1)
  req(input$kpiTSph2)
  req(input$kpiTSph3)
  
  tt <-  data %>% 
    select(contains(input$kpiTSph1),contains(input$kpiTSph2),
           contains(input$kpiTSph3)) %>% 
    purrr::set_names(nm = c('x','y','z')) 
  
  if(input$kpiTSph3 %in%  c('visita_ticket','producto_identificador',
                             'producto_categoria')){
    tt %<>% 
      group_by(y, x) %>% 
      # summarise(val = sum(producto_cantidad,na.rm =T)) %>%
      summarise(val = n_distinct(z)) %>% 
      ungroup %>% 
      group_by(y) %>% 
      mutate(val = val/sum(val)) %>% 
      ungroup %>% 
      mutate(x = str_replace_all(x,'(\\s+)|(-)','_')) %>% 
      spread(x, val, fill = 0) 
    
  }else{
    
    tt %<>% 
      group_by(y, x) %>% 
      summarise(val = sum(z,na.rm =T)) %>%
      ungroup %>% 
      group_by(y) %>% 
      mutate(val = val/sum(val)) %>% 
      ungroup %>% 
      mutate(x = str_replace_all(x,'(\\s+)|(-)','_')) %>% 
      spread(x, val, fill = 0) 
    
  }
  
  corruki <- tt %>%
    .[,-1]%>%  
    cor() %>% 
    as.data.frame() %>% 
    rownames_to_column(var = 'var1') %>% 
    gather(var2, val, -var1) %>% 
    unique %>% 
    filter(val > input$kpiTSph4)
  
  corruki
})

output$kpiPlot3TSb <- renderPlot({
  req(input$kpiTSph1)
  req(input$kpiTSph2)
  req(input$kpiTSph3)
  req(input$kpiTSph4)
  
  correlation <- corruki3() 
  
  graf <- correlation %>% 
    as_tbl_graph() %>%
    mutate(importancia = centrality_degree(mode = "all")) %>%
    activate(nodes) %>%
    mutate(centrality = centrality_betweenness(directed = FALSE),
           nombre = unique(correlation %>%
                             select(var1) %>%
                             rbind(correlation%>%
                                     select(var1 = var2)) %>% .$var1))
  
  
  graficar_red_nd(graf, paste(input$kpiTSph2,'respecto a',input$kpiTSph1),
                  size = 'importancia')
  
})

output$kpiPlot3outb <- renderDT({
  corruki3() %>% filter(val != 1)
})
