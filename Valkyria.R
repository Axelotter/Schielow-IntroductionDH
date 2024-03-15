library(maps)
library(ggplot2)

#Europakarte, schneidet Russland östlich nach Asien ab und Norwegen und Russland nach Norden einige Inseln ab um Karte kompakter zu halten
europmap <- map_data("world")
europmap <- europmap[europmap$region %in% c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "UK", "Vatican City"), ]
europmap <- europmap[!(europmap$region == "Russia" & europmap$long > 50), ]
europmap <- europmap[!(europmap$region == "Russia" & europmap$lat > 74), ]
europmap <- europmap[!(europmap$region == "Norway" & europmap$lat > 71), ]
europmap <- europmap[!(europmap$region == "Norway" & europmap$long < 0), ]




#Entnahme der Daten aus csv Liste für Länder und Amtssprachen
acountrylanguages <- read.csv("C:/Users/alexs/Desktop/Hausarbeit/world-data-2023.csv")
#verschiedene Länderbezeichnungen werden austauschbar verwendet
acountrylanguages$Country[acountrylanguages$Country == "United Kingdom"] <- "UK"
acountrylanguages$Country[acountrylanguages$Country == "Republic of Ireland"] <- "Ireland"
acountrylanguages$is_denmark <- ifelse(acountrylanguages$Country %in% c("Denmark", "Faroe Islands"), TRUE, FALSE)

#Sprachen von Valkyria Chronicles
gamelanguages <- c("English")

##############################Sprachen vom SPiel werden mit Sprachen aus csv Liste verglichen und das entsprechende Land gewählt
languagecountrycomparison <- data.frame(language = character(), country = character())

for (language in gamelanguages) {
  countryindices <- which(grepl(pattern = tolower(language), x = tolower(acountrylanguages$"Official.language")))
  countries <- acountrylanguages$Country[countryindices]
  languagecountrycomparison <- rbind(languagecountrycomparison, 
                                     data.frame(language = rep(language, length(countries)), 
                                                country = countries))
}

languageforcountry <- europmap[europmap$region %in% unique(languagecountrycomparison$country), ]

##################################################################################################################################


#Audio des Spiels
gameaudio <- c("English")

##############################Audio vom SPiel werden mit Sprachen aus csv Liste verglichen und das entsprechende Land gewählt
audiocountrycomparison <- data.frame(language = character(), country = character())

for (language in gameaudio) {
  countryindices <- which(grepl(pattern = tolower(language), x = tolower(acountrylanguages$"Official.language")))
  countries <- acountrylanguages$Country[countryindices]
  audiocountrycomparison <- rbind(audiocountrycomparison, 
                                  data.frame(language = rep(language, length(countries)), 
                                             country = countries))
}

audioforcountry <- europmap[europmap$region %in% unique(audiocountrycomparison$country), ]

##################################################################################################################################


#Ausgabe der Karte; hell für Text, dunkel für Ton
ggplot() +
  geom_map(data = europmap, map = europmap,
           aes(x = long, y = lat, map_id = region),
           fill = "lightblue", color = "black", size = 0.15) +
  geom_map(data = languageforcountry, map = europmap,
           aes(x = long, y = lat, map_id = region),
           fill = "orchid", color = "black", size = 0.3) +
  geom_map(data = audioforcountry, map = europmap,
           aes(x = long, y = lat, map_id = region),
           fill = "darkorchid4", color = "black", size = 0.3) +
  coord_fixed(1.3) +
  theme_void()
