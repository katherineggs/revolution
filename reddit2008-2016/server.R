library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(tidytext)
library(shinydashboard)
library(flexdashboard)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)
library(tidyr)

#Base de datos
reddit <- read.csv("reddit_worldnews_start_to_2016-11-22.csv")

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

commonWords <- c(
  "the","of","and","a","to","in","you","that","it","he","was","for","on","are","as","with","his",
  "they","I","is","at","be","this","have","from","or","one","had","by","word","but","not","what",
  "all","were","we","when", "your","can","said","there","use","an","each","which","she","do",
  "how","their","if","s","over","new","al","old", "year")

countries <- c("afghanistan","albania","algeria","andorra","angola","antigua","barbuda","argentina",
               "armenia","australia","austria","azerbaijan","bahamas","bahrain","bangladesh","barbados",
               "belarus","belgium","belize","benin","bhutan","bolivia","bosnia", "herzegovina","botswana",
               "brazil","brunei","bulgaria","burkina", "faso","burundi","cabo","verde","cambodia","cameroon",
               "canada","central","african","republic","chad","chile","china","colombia","comoros","congo",
               "costa", "rica","croatia","cuba","cyprus","czech","côte","d'ivoire","denmark","djibouti",
               "dominica","dominican","congo","ecuador","egypt","salvador","equatorial", "guinea","eritrea",
               "estonia","eswatini","ethiopia","fiji","finland","france","gabon","gambia","georgia","germany",
               "ghana","greece","grenada","guatemala","guinea","guinea-bissau","haiti","holy","honduras",
               "hungary","iceland","india","indonesia","iran","iraq","ireland","israel","italy","jamaica",
               "japan","jordan","kazakhstan","kenya","kiribati","kuwait","kyrgyzstan","laos","latvia","lebanon",
               "lesotho","liberia","libya","liechtenstein","lithuania","luxembourg","madagascar","malawi",
               "malaysia","maldives","mali","malta","marshall", "islands","mauritania","mauritius","mexico",
               "micronesia","moldova","monaco","mongolia","montenegro","morocco","mozambique","myanmar","namibia",
               "nauru","nepal","netherlands","zealand","nicaragua","nigeria","korea","north", "macedonia","norway",
               "oman","pakistan","palau","panama","papua", "guinea","paraguay","peru","philippines","poland",
               "portugal","qatar","romania","russia","rwanda","kitts","nevis","saint","lucia","samoa","san",
               "marino","sao","tome", "principe","saudi","arabia","senegal","serbia","seychelles","sierra","leone",
               "singapore","slovakia","slovenia","solomon","islands","somalia","africa","korea","south", "sudan",
               "spain", "lanka","st.","vincent","grenadines","state","palestine","sudan","suriname","sweden",
               "switzerland","syria","tajikistan","tanzania","thailand","timor-leste","togo","tonga","trinidad",
               "tobago","tunisia","turkey","turkmenistan","tuvalu","uganda","ukraine","united","arab", "emirates",
               "kingdom","states","uruguay","uzbekistan","vanuatu","venezuela","vietnam","yemen","zambia","zimbabwe")


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
  
  # Fechas & mas18
  output$dateAge <- renderPrint({
    fecha <- input$dateRange
    print(fecha)
    edad <- input$mas18
    #print(edad)
  })
  # Grafica
  output$masAparecen <- renderPlot({
    reddit %>% 
      select(date_created, title, over_18, up_votes, down_votes)%>%
      filter(date_created >= as_date(input$dateRange[1]) & 
               date_created <= as_date(input$dateRange[2])) %>%
      filter(over_18 == ifelse(input$mas18 == TRUE, "True", "False")) %>%
      unnest_tokens(output = word, input = title) %>% 
      count(word, sort = TRUE) %>%
      mutate(isCommon = word %in% commonWords) %>%
      filter(isCommon == FALSE) %>%
      mutate(word = reorder(word,n)) %>%
      #filter(n > 8) %>%
      filter(n > ifelse(input$mas18 == TRUE, 8, 15000)) %>%
      ggplot(aes(n,word))+
      geom_col(fill = "green") +
      labs(x = "Cant. veces", y = "Palabra") +
      geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")
  })
  #grafica paises
  output$paises <- renderPlot({
    reddit %>% 
      filter(over_18 == ifelse(input$paises18 == TRUE, 
                               "True", 
                               "False")) %>%
      unnest_tokens(output = pais, input = title) %>% 
      count(pais, sort = TRUE) %>%
      mutate(isCountr = pais %in% countries) %>%
      filter(isCountr == TRUE) %>%
      mutate(pais = reorder(pais,n)) %>%
      #filter(n > 2) %>%
      filter(n > ifelse(input$paises18 == TRUE, 2, 10000)) %>%
      ggplot(aes(n,pais))+
      geom_col(fill = "purple") +
      labs(x = "Cant. veces", y = "País") +
      geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")
  })
  output$tblPais <- DT::renderDataTable({
    reddit %>% 
      select(date_created,up_votes,title,author) %>%
      mutate(hasCountry = str_extract_all(title, input$selectCountry))%>%
      filter(hasCountry == input$selectCountry) %>%
      DT::datatable()
  })
  
  output$tblAutores <- DT::renderDataTable({
    reddit %>% 
      select(author, title, date_created,up_votes) %>%
      filter(grepl(input$inputStrAuthor, author, fixed = TRUE)) %>%
      group_by(author) %>% 
      summarise(total_publicaciones = n(), total_upvotes = sum(up_votes), promedio= sum(up_votes)/n()) %>% 
      filter(total_publicaciones >= input$selectMinPublicaciones) %>% 
      mutate(author = fct_reorder(author, promedio)) %>%
      DT::datatable()
  })
  
  output$tblPalabras <- DT::renderDataTable({
    reddit %>% 
      select(date_created, title, over_18, up_votes, down_votes)%>%
      filter(date_created >= as_date(input$dateRangepop[1]) & 
               date_created <= as_date(input$dateRangepop[2])) %>%
      filter(over_18 == ifelse(input$mas18pop == TRUE, "True", "False")) %>%
      unnest_tokens(output = word, input = title) %>% 
      group_by(word) %>% 
      summarise(n = n(), upvotes = sum(up_votes), promedio = sum(up_votes)/n()) %>% 
      mutate(isCommon = word %in% commonWords) %>%
      filter(isCommon == FALSE) %>%
      filter(n >= input$selectMinPalabras) %>% 
      mutate(word = reorder(word,promedio)) %>%
      DT::datatable(options = list(order = list(list(4, 'desc'))))
  })
  output$palabrasPop <- renderPlot({
    reddit %>% 
      select(date_created, title, over_18, up_votes, down_votes)%>%
      filter(date_created >= as_date(input$dateRangepop[1]) & 
               date_created <= as_date(input$dateRangepop[2])) %>%
      filter(over_18 == ifelse(input$mas18pop == TRUE, "True", "False")) %>%
      unnest_tokens(output = word, input = title) %>% 
      group_by(word) %>% 
      summarise(n = n(), upvotes = sum(up_votes), promedio = sum(up_votes)/n()) %>% 
      mutate(isCommon = word %in% commonWords) %>%
      filter(isCommon == FALSE) %>%
      filter(n >= input$selectMinPalabras) %>% 
      mutate(word = reorder(word,promedio)) %>%
      top_n(20, promedio) %>%
      ggplot(aes(promedio,word))+
      geom_col(fill = "green") +
      labs(x = "Palabra", y = "promedio") +
      geom_text(aes(label = promedio), hjust = 1.2, colour = "white", fontface = "bold")
  })
  output$autoresPlot <- renderPlot({
    reddit %>% 
      select(author, title, date_created,up_votes) %>%
      filter(grepl(input$inputStrAuthor, author, fixed = TRUE)) %>%
      group_by(author) %>% 
      summarise(total_publicaciones = n(), total_upvotes = sum(up_votes), promedio= round(sum(up_votes)/n())) %>% 
      filter(total_publicaciones >= input$selectMinPublicaciones) %>% 
      top_n(20, promedio) %>%
      mutate(author = fct_reorder(author, promedio)) %>%
      ggplot(aes(promedio,author))+
      geom_col(fill = "green") +
      labs(x = "promedio", y = "autor") +
      geom_text(aes(label = promedio), hjust = 1.2, colour = "white", fontface = "bold")
  })
  
  
  
})










