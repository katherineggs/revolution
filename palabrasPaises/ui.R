#Parcial 2 Data Product —  2021

# - Palabras que mas aparecen en cierto rango de fecha, cuantos son +18 [Katy]
# - Países que más aparecen explícitamente [Katy]

library(shiny)
library(lubridate)

data <- read.csv("../reddit_worldnews_start_to_2016-11-22.csv")
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

shinyUI(fluidPage(
    
    titlePanel("Palabras & Paises"),
    
    tabsetPanel(
    tabPanel("+Palabras",
             h2(""),
             wellPanel(
                 fluidRow(
                     column(
                         width = 11, align = "center",
                         column(width = 5,
                           h4("-- Palabras que mas aparecen --"),
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
        fluidRow(column(width = 11,plotOutput("masAparecen", height = 500))),
        fluidRow(width = 11, align = "center",
            column(width = 5, h4("-- Palabras y su popularidad --")),
            column(width = 6, 
                   numericInput("selectMinPalabras",
                                "Seleccione un minimo de frecuencia para las palabras", 
                                value = 10
                                ),
                   dateRangeInput("dateRangepop", "Seleccione un rango de fecha",
                                  start = "2008-01-25",
                                  end = "2016-11-22",
                                  weekstart = 1,
                                  language = "es",
                                  separator = "a"),
                   checkboxInput("mas18pop","Mensajes para +18", value = FALSE)
                   ),
        ),
        fluidRow(column(width = 11,plotOutput("palabrasPop", height = 500))),
        
        DT::dataTableOutput("tblPalabras")
    ),
    tabPanel(
        "+Paises",
        h2(""),
        column(width = 5,
               h4("--- Palabras que mas aparecen ---"),
               plotOutput("paises", height = 500),
               checkboxInput("paises18","En segmentos para +18", value = TRUE)),
        column(width = 7,
               wellPanel(fluidRow(h4("Seleccione un pais para ver los titulos que lo incluyan"),
               selectInput("selectCountry", "Seleccione un país",
                           choices = countries,
                           selected = "china"))),
               DT::dataTableOutput("tblPais"))
        
    ),
    tabPanel(
        "Autores",
        h2("Analisis de Autores"),
        fluidRow( width = 11,
              column(width = 6,
                     plotOutput("autoresPlot", height = 500)),
              column(width = 5,
                   h4("--- Autores ---"),
                   numericInput("selectMinPublicaciones", "Seleccione un mínimo de publicaciones del autor",
                                value = 10
                   ),
                   textInput("inputStrAuthor", "Buscar un autor\n(dejar vacío para no usar este filtro)", value="")
                    ),
                   DT::dataTableOutput("tblAutores")
        )
        )

        
    )
    )
)
