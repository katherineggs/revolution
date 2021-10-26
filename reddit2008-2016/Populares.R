#---------------------------------------
# INSTRUCCIONES
# 1. Tablas x
# 2. Graficas x
# 3. Input x
# 4. Layouts x
# 5. Interactividad x
# 6. Reactividad x
# 7. Update Function
# 8. Parámetros en el URL
#---------------------------------------

# Que es lo popular (malo o bueno) por fecha [Andrea]
# Personas más populares [Andrea]

#---------------------------------------

library(readr)
library(dplyr)
library(lubridate)
library(tidytext)
library(ggplot2)
library(stringr)
library(purrr)
library(tidyr)

#Base de datos
reddit <- read.csv("C:/Users/JPMR0/Desktop/DPparcial/revolution/reddit2008-2016/reddit_worldnews_start_to_2016-11-22.csv")

#Limpieza de datos
reddit <- reddit %>%
  mutate(date_created = ymd(date_created)) %>%
  mutate(year = format(as.Date(date_created),  "%Y"))

View(reddit)

#------------- Que ha sido popular según Up Votes (bueno) por año (2008 - 2016) -------------

#Promedio de Up Votes
filas <- nrow(reddit)
suma <- sum(reddit$up_votes)

promedioUpVotes <- round(suma / filas) 
promedioUpVotes

#Populares (posts con up votes arriba del promedio) (todos los años)
upVotes <- reddit %>%
  select(year, title, up_votes) %>%
  group_by(year, title)  %>%
  summarise(populares = up_votes) %>%
  filter(populares > promedioUpVotes)

View(upVotes)

#Tabla solo del año 2008
upVotes2008 <- reddit %>%
  select(year, title, up_votes) %>%
  filter(year == 2008) %>%
  group_by(year, title)  %>%
  summarise(populares = up_votes) %>%
  filter(populares > promedioUpVotes)

View(upVotes2008)

#Tabla solo del año 2009
upVotes2009 <- reddit %>%
  select(year, title, up_votes) %>%
  filter(year == 2009) %>%
  group_by(year, title)  %>%
  summarise(populares = up_votes) %>%
  filter(populares > promedioUpVotes)

View(upVotes2009)

#Tabla solo del año 2010
upVotes2010 <- reddit %>%
  select(year, title, up_votes) %>%
  filter(year == 2010) %>%
  group_by(year, title)  %>%
  summarise(populares = up_votes) %>%
  filter(populares > promedioUpVotes)

View(upVotes2010)

#Tabla solo del año 2011
upVotes2011 <- reddit %>%
  select(year, title, up_votes) %>%
  filter(year == 2011) %>%
  group_by(year, title)  %>%
  summarise(populares = up_votes) %>%
  filter(populares > promedioUpVotes)

View(upVotes2011)

#Tabla solo del año 2012
upVotes2012 <- reddit %>%
  select(year, title, up_votes) %>%
  filter(year == 2012) %>%
  group_by(year, title)  %>%
  summarise(populares = up_votes) %>%
  filter(populares > promedioUpVotes)

View(upVotes2012)

#Tabla solo del año 2013
upVotes2013 <- reddit %>%
  select(year, title, up_votes) %>%
  filter(year == 2013) %>%
  group_by(year, title)  %>%
  summarise(populares = up_votes) %>%
  filter(populares > promedioUpVotes)

View(upVotes2013)

#Tabla solo del año 2014
upVotes2014 <- reddit %>%
  select(year, title, up_votes) %>%
  filter(year == 2014) %>%
  group_by(year, title)  %>%
  summarise(populares = up_votes) %>%
  filter(populares > promedioUpVotes)

View(upVotes2014)

#Tabla solo del año 2015
upVotes2015 <- reddit %>%
  select(year, title, up_votes) %>%
  filter(year == 2015) %>%
  group_by(year, title)  %>%
  summarise(populares = up_votes) %>%
  filter(populares > promedioUpVotes)

View(upVotes2015)

#Tabla solo del año 2016
upVotes2016 <- reddit %>%
  select(year, title, up_votes) %>%
  filter(year == 2016) %>%
  group_by(year, title)  %>%
  summarise(populares = up_votes) %>%
  filter(populares > promedioUpVotes)

View(upVotes2016)

#Post más popular de cada año
topGrafica <- upVotes %>%
  group_by(year) %>%
  summarise(topPost = max(populares))%>%
  ggplot(aes(year,topPost))+
  geom_col(fill = "paleturquoise") +
  labs(x = "Año", y = "Up Votes", title = "Post más popular de cada año") +
  geom_text(aes(label = year), hjust = 1.2, colour = "white", fontface = "bold")
topGrafica

top <- upVotes %>%
  group_by(year) %>%
  summarise(topPost = max(populares))
View(top)

#Post más popular entre todos los años
maxTop <- top %>%
  select(year, topPost) %>%
  summarise(maxPost = max(topPost))

View(maxTop)


#------------- Que ha sido popular según Down Votes (malo) por año (2008 - 2016) -------------

#Promedio de Down Votes
filas <- nrow(reddit)
suma <- sum(reddit$down_votes)
max(reddit$down_votes)

promedioDownVotes <- round(suma / filas) 
promedioDownVotes

#Populares
DownVotes <- reddit %>%
  select(year, title, down_votes) %>%
  group_by(year, title)  %>%
  summarise(populares = down_votes) %>%
  filter(populares > promedioDownVotes)

View(DownVotes)

##### No hay down votes




#------------- Personas (famosas) más populares -------------

#Estos son los personajes públicos más admirados del mundo 
famosos <- c("jolie", "obama", "winfrey", "isabel II", "clinton", "watson", "yousafzai", 
             "merkel", "swift", "madonna", "rai", "chopra", "padukone", "gadot", "yifei",
             "bingbing", "wei", "warren", "gates", "barack", "chan", "jinping", "jack", 
             "putin", "lama", "modi", "bachchan", "ronaldo", "messi", "buffett", "beckham", 
             "musk", "jordan", "papa", "trump", "lau", "erdogan", "khan", "alba", "graham", 
             "bieber", "timberlake", "spiers", "sheeran", "drake", "gaga", "adele")


pFamosas <- reddit %>% 
  unnest_tokens(input = title, output = persona) %>% 
  count(persona, sort = TRUE) %>%
  mutate(pf = persona %in% famosos) %>%
  filter(pf == TRUE)
  
View(pFamosas)

pFamosasGrafica <- reddit %>% 
  unnest_tokens(input = title, output = persona) %>% 
  count(persona, sort = TRUE) %>%
  mutate(pf = persona %in% famosos) %>%
  filter(pf == TRUE)%>%
  ggplot(aes(persona, n))+
  geom_col(fill = "pink") +
  labs(x = "Persona", y = "Up Votes", title = "Famosos populares") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")
pFamosasGrafica



commonWords <- c(
  "the","of","and","a","to","in","you","that","it","he","was","for","on","are", 
  "as","with","his","they", "I","is","at","be","this","have","from","or","one",
  "had","by","word","but","not","what","all","were","we","when", "your","can",
  "said", "there", "use","an","each", "which","she","do","how","their","if")

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
               "spain", "lanka","St.","Vincent","Grenadines","State","Palestine","Sudan","Suriname","Sweden",
               "switzerland","syria","tajikistan","tanzania","thailand","timor-leste","togo","tonga","trinidad",
               "tobago","tunisia","turkey","turkmenistan","tuvalu","uganda","ukraine","united","arab", "emirates",
               "kingdom","states","uruguay","uzbekistan","vanuatu","venezuela","vietnam","yemen","zambia","zimbabwe")


# min fecha
reddit %>%
  select(date_created) %>%
  summarise(min(date_created))

# max fecha
reddit %>%
  select(date_created) %>%
  summarise(max(date_created))

mas18 <- TRUE

#  PALABRAS
reddit %>% 
  filter(date_created >= as_date("2010-2-20")) %>%
  filter(date_created <= as_date("2011-2-25")) %>%
  filter(over_18 == ifelse(mas18 == "True", 
                           mas18, 
                           "False") ) %>%
  unnest_tokens(output = word, input = title) %>% 
  count(word, sort = TRUE) %>%
  mutate(isCommon = word %in% commonWords) %>%
  filter(isCommon == FALSE) %>%
  mutate(word = reorder(word,n)) %>%
  filter(n > 1) %>%
  ggplot(aes(n,word))+
  geom_col(fill = "green") +
  labs(x = "Palabra", y = "Cant. veces", title = "--- Palabras que mas aparecen ---") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")

reddit %>% 
  select(date_created, title, over_18)%>%
  filter(date_created >= as_date("2008-2-20") & 
           date_created <= as_date("2016-2-25")) %>%
  filter(over_18 == ifelse(mas18 == TRUE, "True", "False")) %>%
  unnest_tokens(output = word, input = title) %>% 
  count(word, sort = TRUE) %>%
  mutate(isCommon = word %in% commonWords) %>%
  filter(isCommon == FALSE) %>%
  mutate(word = reorder(word,n)) %>%
  filter(n > ifelse(mas18 == TRUE, 1, 15000)) %>%
  ggplot(aes(n,word))+
  geom_col(fill = "green") +
  labs(x = "Palabra", y = "Cant. veces", title = "--- Palabras que mas aparecen ---") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")


# PAISES
paises18 <- FALSE
reddit %>% 
  filter(over_18 == ifelse(paises18 == TRUE, 
                           "True", 
                           "False")) %>%
  unnest_tokens(output = word, input = title) %>% 
  count(word, sort = TRUE, over_18) %>%
  mutate(isCommon = word %in% countries) %>%
  filter(isCommon == TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  filter(n > ifelse(over_18 == "True", 
                    2, 
                    10000)) %>%
  ggplot(aes(n,word))+
  geom_col(fill = "purple") +
  labs(x = "Cant. veces", y = "País", title = "--- Países que mas aparecen ---") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")

# tabla  
selectCountry <- "Pakistan"
reddit %>%
  select(date_created,up_votes,title,author) %>%
  mutate(hasCountry = selectCountry %in% reddit$title) %>%
  #filter(hasCountry == TRUE) %>%
  View()

reddit %>% 
  select(date_created,up_votes,title,author) %>%
  mutate(hasCountry = str_extract_all(title, selectCountry))%>%
  filter(hasCountry == selectCountry) %>%
  View()

input$mas18 = FALSE

reddit %>% 
  select(date_created, title, over_18, up_votes, down_votes)%>%
  filter(date_created >= as_date(input$dateRange[1]) & 
           date_created <= as_date(input$dateRange[2])) %>%
  filter(over_18 == ifelse(FALSE == TRUE, "True", "False")) %>%
  unnest_tokens(output = word, input = title) %>% 
  count(word, sort = TRUE) %>%
  mutate(isCommon = word %in% commonWords) %>%
  filter(isCommon == FALSE) %>%
  mutate(word = reorder(word,n)) %>%
  #filter(n > 8) %>%
  filter(n > ifelse(FALSE == TRUE, 8, 15000)) %>%
  DT::datatable()

library(dplyr)
library(tidytext)
d = data_frame(reviewText = c('1 2 3 4 5 able', '1 2\n3 4 5\n6\n7\n8\n9 10 above', '1!2', '1',
                              '!', '', '\n', '1', 'able able', 'above above', 'able', 'above'),
               asin = rep(letters, each = 2, length.out = length(reviewText)))
?rep

reddit("crude")
myTdm <- as.matrix(TermDocumentMatrix(crude))
unnest_tokens()

library(dplyr)
library(janeaustenr)

d <- tibble(txt = prideprejudice)
d

d %>%
  unnest_tokens(word, txt)














