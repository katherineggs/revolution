library(forcats)
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidytext)

data <- read.csv("../reddit_worldnews_start_to_2016-11-22.csv")
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

# Modificaciones al dataset
data <- data %>%
  mutate(date_created = as_date(date_created, format = "%Y-%m-%d"))


shinyServer(function(input, output) {
  # Fechas & mas18
  output$dateAge <- renderPrint({
    fecha <- input$dateRange
    print(fecha)
    edad <- input$mas18
    #print(edad)
  })
  # Grafica
  output$masAparecen <- renderPlot({
    data %>% 
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
    data %>% 
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
      labs(x = "Cant. veces", y = "País", title = "--- Países que mas aparecen ---") +
      geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")
  })
  output$tblPais <- DT::renderDataTable({
    data %>% 
      select(date_created,up_votes,title,author) %>%
      mutate(hasCountry = str_extract_all(title, input$selectCountry))%>%
      filter(hasCountry == input$selectCountry) %>%
      DT::datatable()
  })
  
  output$tblAutores <- DT::renderDataTable({
    data %>% 
      select(author, title, date_created,up_votes) %>%
      filter(grepl(input$inputStrAuthor, author, fixed = TRUE)) %>%
      group_by(author) %>% 
      summarise(total_publicaciones = n(), total_upvotes = sum(up_votes), promedio= sum(up_votes)/n()) %>% 
      filter(total_publicaciones >= input$selectMinPublicaciones) %>% 
      mutate(author = fct_reorder(author, promedio)) %>%
      DT::datatable()
  })
  
  output$tblPalabras <- DT::renderDataTable({
    data %>% 
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
    data %>% 
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
    data %>% 
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
  

  
  
  
  
  
