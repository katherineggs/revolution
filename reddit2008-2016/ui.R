library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(tidytext)
library(shinydashboard)
library(stringr)
library(purrr)
library(tidyr)

#Base de datos
reddit <- read.csv("reddit_worldnews_start_to_2016-11-22.csv")

countries <- c("Afghanistan","Albania","Algeria","Andorra","Angola","Antigua","Barbuda","Argentina",
               "Armenia","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados",
               "Belarus","Belgium","Belize","Benin","Bhutan","Bolivia","Bosnia", "Herzegovina","Botswana",
               "Brazil","Brunei","Bulgaria","Burkina", "Faso","Burundi","Cabo","Verde","Cambodia","Cameroon",
               "Canada","Central","African","Republic","Chad","Chile","China","Colombia","Comoros","Congo",
               "Costa", "Rica","Croatia","Cuba","Cyprus","Czech","Côte","d'Ivoire","Denmark","Djibouti",
               "Dominica","Dominican","Congo","Ecuador","Egypt","Salvador","Equatorial","Guinea","Eritrea",
               "Estonia","Eswatini","Ethiopia","Fiji","Finland","France","Gabon","Gambia","Georgia","Germany",
               "Ghana","Greece","Grenada","Guatemala","Guinea","Guinea-Bissau","Haiti","Holy","Honduras",
               "Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Jamaica",
               "Japan","Jordan","Kazakhstan","Kenya","Kiribati","Kuwait","Kyrgyzstan","Laos","Latvia","Lebanon",
               "Lesotho","Liberia","Libya","Liechtenstein","Lithuania","Luxembourg","Madagascar","Malawi",
               "Malaysia","Maldives","Mali","Malta","Marshall", "Islands","Mauritania","Mauritius","Mexico",
               "Micronesia","Moldova","Monaco","Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia",
               "Nauru","Nepal","Netherlands","Zealand","Nicaragua","Nigeria","Korea","North", "Macedonia","Norway",
               "Oman","Pakistan","Palau","Panama","Papua","Guinea","Paraguay","Peru","Philippines","Poland",
               "Portugal","Qatar","Romania","Russia","Rwanda","Kitts","Nevis","Saint","Lucia","Samoa","San",
               "Marino","Sao","Tome", "Principe","Saudi","Arabia","Senegal","Serbia","Seychelles","Sierra","Leone",
               "Singapore","Slovakia","Slovenia","Solomon","Islands","Somalia","Africa","South", "Sudan",
               "Spain", "Lanka","St.","Vincent","Grenadines","State","Palestine","Sudan","Suriname","Sweden",
               "Switzerland","Syria","Tajikistan","Tanzania","Thailand","Timor-Leste","Togo","Tonga","Trinidad",
               "Tobago","Tunisia","Turkey","Turkmenistan","Tuvalu","Uganda","Ukraine","United","Arab","Emirates",
               "Kingdom","States","Uruguay","Uzbekistan","Vanuatu","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe")


#Shiny
shinyUI(fluidPage(
   navbarPage("Reddit DB",
              tabPanel("Populares",
                       h2("Qué ha sido popular según Up Votes: "), br(),
                       sidebarPanel(
                           width = 3,
                           selectInput('datePopularesA', 'Seleccione un año:',
                               choices = 2008:2010)
                          
                       #sidebarPanel
                       ),
                       
                       mainPanel(
                           verbatimTextOutput("outputDatePopularesA"),
                           
                           fluidRow(
                               box(title = "Posts con Up Votes arriba del promedio: ",
                                   solidHeader = T,
                                   width = 6,
                                   div(DT::dataTableOutput("tablaPopularesA"), style = "font-size: 75%;")
                                   ),
                               box(title = "Post más popular de cada año",
                                   solidHeader = T,
                                   width = 6,
                                   plotOutput("postsPop")
                                 )
                            )
                       #mainPanel
                       )
                       #tabPanel
                       ),
              

              tabPanel("Famosos",
                       fluidRow(
                         box(title = "Personas famosas populares en reddit",
                             solidHeader = T,
                             width = 12,
                             plotOutput("famosos",
                                        click = 'clk',
                                        dblclick = 'dblclick',
                                        hover = 'hover',
                                        brush = 'brush')
                             ),
                         box(title = "Información de la gráfica",
                             solidHeader = T,
                             width = 12,
                             verbatimTextOutput('plot_click_option_values')
                           ),
                         h5("*Puede ver la informacion de un click, doble click y hover"),
                       )
                       ),
              
              
              tabPanel("Palabras",
                       h2("Palabras que más aparecen: "), br(),
                       wellPanel(
                         fluidRow(
                           column(
                             width = 11, align = "center",
                             column(width = 5,
                                    dateRangeInput("dateRange", "Seleccione un rango de fecha",
                                                   start = "2008-01-25",
                                                   end = "2016-11-22",
                                                   weekstart = 1,
                                                   language = "es",
                                                   separator = "a"),
                                    checkboxInput("mas18","Mensajes para +18", value = TRUE)
                             ),
                             column(width = 2),
                             column(width = 5, align = "center",
                                    h4("Rango de fecha seleccionado: "),
                                    verbatimTextOutput("dateAge"))))
                       ),
                       br(), br(), 
                       fluidRow(
                         column(width = 11,plotOutput("masAparecen", height = 500))), br(), br(), 
                       wellPanel(
                         fluidRow(width = 11, align = "center",
                                  h4("Palabras y su popularidad: "),
                                  column(width = 6, 
                                         numericInput("selectMinPalabras",
                                                      "Seleccione un mínimo de frecuencia para las palabras", 
                                                      value = 10
                                         ),
                                  ),
                                  column(width = 6,
                                    dateRangeInput("dateRangepop", "Seleccione un rango de fecha",
                                                   start = "2008-01-25",
                                                   end = "2016-11-22",
                                                   weekstart = 1,
                                                   language = "es",
                                                   separator = "a"),
                                    checkboxInput("mas18pop","Mensajes para +18", value = FALSE)
                                  )
                         )
                       ),
                       fluidRow(column(width = 11,plotOutput("palabrasPop", height = 500))),
                       
                       DT::dataTableOutput("tblPalabras")
              ),
              tabPanel(
                "Países",
                h2("Países que mas aparecen: "), br(),
                column(width = 5,
                       plotOutput("paises", height = 500),
                       checkboxInput("paises18","Mensajes para +18", value = TRUE)),
                column(width = 7,
                       wellPanel(fluidRow(
                                          selectInput("selectCountry", "Seleccione un país para ver los titulos que lo incluyan",
                                                      choices = countries,
                                                      selected = "china"))),
                       DT::dataTableOutput("tblPais"))
                
              ),
              tabPanel(
                "Autores",
                h2("Análisis de Autores: "), br(),
                fluidRow( width = 11,
                          column(width = 6,
                                 plotOutput("autoresPlot", height = 500)),
                          column(width = 5,
                                 h3("Autores: "),
                                 numericInput("selectMinPublicaciones", "Seleccione un mínimo de publicaciones del autor",
                                              value = 10
                                 ),
                                 textInput("inputStrAuthor", "Buscar un autor\n(dejar vacío para no usar este filtro)", value="")
                          ),
                          DT::dataTableOutput("tblAutores")
                )
              )
              )
   
))
