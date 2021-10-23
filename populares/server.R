library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(tidytext)
library(shinydashboard)
library(flexdashboard)
library(ggplot2)

#Base de datos
reddit <- read.csv("../reddit_worldnews_start_to_2016-11-22.csv")

#Limpieza de datos
reddit <- reddit %>%
  mutate(date_created = ymd(date_created)) %>%
  mutate(year = format(as.Date(date_created),  "%Y"))

#Estos son los personajes públicos más admirados del mundo 
famosos <- c("jolie", "obama", "winfrey", "isabel II", "clinton", "watson", "yousafzai", 
             "merkel", "swift", "madonna", "rai", "chopra", "padukone", "gadot", "yifei",
             "bingbing", "wei", "warren", "gates", "barack", "chan", "jinping", "jack", 
             "putin", "lama", "modi", "bachchan", "ronaldo", "messi", "buffett", "beckham", 
             "musk", "jordan", "papa", "trump", "lau", "erdogan", "khan", "alba", "graham", 
             "bieber", "timberlake", "spiers", "sheeran", "drake", "gaga", "adele")

#Shiny
shinyServer(function(input, output) {

  #Populares (posts con up votes arriba del promedio por año)
  output$tablaPopularesA <- DT::renderDataTable({
    upVotes <- reddit %>%
      select(year, title, up_votes) %>%
      filter(year == input$datePopularesA) %>%
      group_by(year, title)  %>%
      summarise(populares = up_votes) %>%
      filter(populares > promedioUpVotes)
  })
  
  #valuebox Post más popular entre todos los años
  output$vbox1 <- renderInfoBox({
    top <- upVotes %>%
      group_by(year) %>%
      summarise(topPost = max(populares))
    
    maxTop <- top %>%
      select(year, topPost) %>%
      summarise(maxPost = max(topPost))

    valueBox("Post con más Up Votes", 
             maxTop$maxPost, 
             icon = icon("angellist"))
  })
  
  #Grafica Post más popular de cada año
  output$postsPop <- renderPlot({
    upVotes %>%
      group_by(year) %>%
      summarise(topPost = max(populares))%>%
      ggplot(aes(year,topPost))+
      geom_col(fill = "paleturquoise") +
      labs(x = "Año", y = "Up Votes") +
      geom_text(aes(label = topPost), hjust = 1.2, colour = "white", fontface = "bold")
  })
  
  #Grafica Personas famosas populares en posts de reddit
  output$famosos <- renderPlot({
    reddit %>% 
      unnest_tokens(input = title, output = persona) %>% 
      count(persona, sort = TRUE) %>%
      mutate(pf = persona %in% famosos) %>%
      filter(pf == TRUE)%>%
      ggplot(aes(persona, n))+
      geom_col(fill = "pink") +
      labs(x = "Persona", y = "Up Votes") +
      geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")
  })
  
  # Print info de 'opciones'
  output$plot_click_option_values <- renderPrint({
    #One Click
    df <- NULL
    if(!is.null(input$clk$x)){
      x <- round(input$clk$x, 2)
      y <- round(input$clk$y, 2)
      click <- paste0('(', x, ', ', y, ')', collapse = '')
      click <- paste0('Coordenada del click: ', click, collapse = '')
      df <- nearPoints(mtcars, input$clk, xvar = 'wt', yvar = 'mpg')
    }else{click <- NULL}
    print(click)
    
    #Double Click
    if(!is.null(input$dblclick$x)){
      x <- round(input$dblclick$x, 2)
      y <- round(input$dblclick$y, 2)
      dblclick <- paste0('(', x, ', ', y, ')', collapse = '')
      dblclick <- paste0('Coordenada del doble click: ', dblclick, collapse = '')
    }else{dblclick <- NULL}
    print(dblclick)
    
    #Hover
    if(!is.null(input$hover$x)){
      x <- round(input$hover$x, 2)
      y <- round(input$hover$y, 2)
      hover <- paste0('(', x, ', ', y, ')', collapse = '')
      hover <- paste0('Coordenada de hover: ', hover, collapse = '')
    }else{hover <- NULL}
    print(hover)
    
  })

  
  
  
})










