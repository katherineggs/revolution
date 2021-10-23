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

#Base de datos
reddit <- read.csv("/Users/andreareyes/Desktop/Data Product/parcial2/revolution/reddit_worldnews_start_to_2016-11-22.csv")

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


















