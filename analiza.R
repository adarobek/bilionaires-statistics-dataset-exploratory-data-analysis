#WCZYTANIE ZBIORU DANYCH
data<-read.csv("Billionaires Statistics Dataset.csv")
#sprawdzenie klasy wczytanych danych
class(data)
#podsumowanie i struktura zbioru danych
summary(data)
str(data)
# CZYSZCZENIE DANYCH I KORYGOWANIE FORMATÓW ZMIENNYCH
# sprawdzenie, czy występują obserwacje brakujące
data[!complete.cases(data),]
# sprawdzenie kolumny rank
summary(data$rank)
data$rank[is.na(data$rank)] #brak obserwacji brakujących
# sprawdzenie kolumny finalWorth
summary(data$finalWorth)
data$finalWorth[is.na(data$finalWorth)]
#przeliczenie kolumny finalWorth z milionów dolarów na miliardy dolarów
data$finalWorth<-data$finalWorth/1000
summary(data$finalWorth)
# sprawdzenie kolumny category
summary(data$category)
head(data$category)
install.packages("dplyr")
library(dplyr)
filter(data,category=="") #brak obserwacji brakujących
# przekształcenie kolumny category w zmienną typu factor
data$category<-factor(data$category)
summary(data$category)
str(data$category)
# sprawdzenie kolumny personName
filter(data,personName=="") #brak obserwacji brakujących
# sprawdzenie kolumny age
summary(data$age)
data$age[is.na(data$age)]
medianAge<-median(data$age,na.rm=TRUE)
data$age[is.na(data$age)]<-medianAge #uzupełnienie wartości brakujących medianą
summary(data$age) #brak obserwacji brakujących
# sprawdzenie kolumny country
summary(data$country)
#przekształcenie kolumny country w zmienną typu factor
data$country<-factor(data$country)
summary(data$country)
#uzupełnienie wierszy z brakującymi danymi odnośnie kraju
levels(data$country)[1]<-"Unknown"
summary(data$country)
# sprawdzenie kolumny city
summary(data$city)
data$city[data$city==""]
#uzupełnienie wierszy z brakującymi danymi odnośnie miasta
data$city[data$city==""]<-"Unknown"
# sprawdzenie kolumny source
summary(data$source)
data$source[data$source==""]
# sprawdzenie kolumny industries
data$industries[data$industries==""]
data$industries[is.na(data$industries)]
#przekształcenie kolumny industries w zmimanną typu factor oraz przetłumaczenie
#etykiet na język polski
data$industries<-factor(data$industries,levels=c("Automotive","Construction & Engineering",
                                                 "Diversified","Energy","Fashion & Retail",
                                                 "Finance & Investments","Food & Beverage",
                                                 "Gambling & Casinos","Healthcare",
                                                 "Logistics","Manufacturing","Media & Entertainment",
                                                 "Metals & Mining","Real Estate","Service","Sports",
                                                 "Technology","Telecom"),
                        labels=c("Motoryzacja","Budownictwo i inżynieria","Zdywersyfikowane",
                                 "Energia","Moda i handel detaliczny","Finanse i inwestycje",
                                 "Żywność i napoje","Hazard i kasyna","Opieka zdrowotna",
                                 "Logistyka","Produkcja","Media i rozrywka","Metale i górnictwo",
                                 "Nieruchomości","Usługi","Sport","Technologia","Telekomunikacja"))
summary(data$industries)
levels(data$industries)
# sprawdzenie kolumny countryOfCitizenship
data$countryOfCitizenship[data$countryOfCitizenship==""]
# sprawdzenie kolumny organization
data$organization[data$organization==""]
# uzupełnienie wierszy z brakującymi danymi odnośnie organizacji
data$organization[data$organization==""]<-"No Data"
data$organization[data$organization==""]
# sprawdzenie kolumny selfMade
summary(data$selfMade)
head(data$selfMade)
# przekształcenie kolumny selfMade w zmienną typu factor
data$selfMade<-factor(data$selfMade,levels=c(FALSE,TRUE),
                      labels=c("Majątek uzyskany niesamodzielnie",
                               "Majątek uzyskany samodzielnie"))
str(data$selfMade)
summary(data$selfMade)
# sprawdzenie kolumny status
summary(data$status)
data$status
# przekształcenie danych, na temat których autor nie zamieścił informacji w "słowniczku"
data$status[data$status=="E"]<-"No Data"
data$status[data$status=="R"]<-"No Data"
data$status[data$status=="N"]<-"No Data"
# przekształcenie kolumny status w zmienną typu factor
data$status<-factor(data$status,levels=c("D","No Data",
                                         "Split Family Fortune","U"),
                    labels=c("Założyciel","Brak danych","Podzielony rodzinny\n majątek",
                             "Odziedziczone/\nniezarobione"))
summary(data$status)
# sprawdzenie kolumny gender
summary(data$gender)
head(data$gender)
# przekształcenie kolumny gender w zmienną typu factor
data$gender<-factor(data$gender,levels=c("F","M"),labels=c("Kobieta","Mężczyzna"))
summary(data$gender)
str(data)
# sprawdzenie kolumny birthDate
head(data$birthDate)
#decyzja o nieuwzględnieniu kolumny birthDate 
#w późniejszej analizie

#sprawdzenie kolumny lastName
head(data$lastName)
data$lastName[data$lastName==""] #brak obserwacji brakujących
data$lastName[is.na(data$lastName)]
#sprawdzenie kolumny firstName
data$firstName[data$firstName==""] #brak obserwacji brakujących
# sprawdzenie kolumny title
data$title
#decyzja o nieuwzględnieniu kolumny title 
#w późniejszej analizie

str(data)
#sprawdzenie kolumny date
data$date
#decyzja o nieuwzględnieniu kolumny date 
#w późniejszej analizie

#sprawdzenie kolumny state
data$state
#uzupełnienie wierszy z brakującymi danymi odnośnie state
data$state[data$state==""]<-"Not applicable/No data"
data$state
str(data)
# sprawdzenie kolumny residenceStateRegion
data$residenceStateRegion
#uzupełnienie wierszy z brakującymi danymi odnośnie residenceStateRegion
data$residenceStateRegion[data$residenceStateRegion==""]<-"No Data"
data$residenceStateRegion
str(data)
# sprawdzenie kolumny birthYear
data$birthYear[is.na(data$birthYear)]
summary(data$birthYear)
medianBirthYear<-median(data$birthYear,na.rm=TRUE)
medianBirthYear
#uzupełnienie brakujących danych w kolumnie birthYear medianą
data$birthYear[is.na(data$birthYear)]<-medianBirthYear
summary(data$birthYear)
# sprawdzenie kolumny birthMonth
summary(data$birthMonth)
#uzupełnienie brakujących danych w kolumnie birthMonth wartością 0
data$birthMonth[is.na(data$birthMonth)]<-0
summary(data$birthMonth)

#sprawdzenie kolumny birthDay
summary(data$birthDay)
#uzupełnienie brakujących danych w kolumnie birthDay wartością 0
data$birthDay[is.na(data$birthDay)]<-0
summary(data$birthDay)

#sprawdzenie kolumny cpi_country
summary(data$cpi_country)
data[is.na(data$cpi_country),]
#uzupełnienie brakujących danych wartościami z internetu
data[data$country=="Hong Kong"&is.na(data$cpi_country),"cpi_country"]<-106.7
data[is.na(data$cpi_country),]
data[data$country=="Taiwan"&is.na(data$cpi_country),"cpi_country"]<-106.6
data[is.na(data$cpi_country),]
summary(data$cpi_country)
#uzupełnienie pozostałych brakujących danych za pomocą mediany
medianCPICountry<-median(data$cpi_country,na.rm=TRUE)
str(data$cpi_country)
data$cpi_country[is.na(data$cpi_country)]<-medianCPICountry
summary(data$cpi_country)

#sprawdzenie kolumny cpi_change_country
summary(data$cpi_change_country)
#decyzja o nieuwzględnianiu kolumny cpi_change_country
#w późniejszej analizie


# sprawdzenie kolumny gdp_country
head(data$gdp_country)

#przekształcenie wartości z kolumny gdp_country z wartości tekstowych na liczbowe
data$gdp_country<-substr(data$gdp_country,2,100)
head(data$gdp_country)
data$gdp_country<-gsub(',','',data$gdp_country)
data$gdp_country<-gsub(' ','',data$gdp_country)
data$gdp_country<-as.numeric(data$gdp_country)
summary(data$gdp_country)
#sprawdzenie, czy w kolumnie gdp_country występują obserwacje brakujące
missing_data_gdp<-factor(data$country[is.na(data$gdp_country)])
summary(missing_data_gdp)
#uzupełnienie obserwacji brakujących danymi z internetu
data[is.na(data$gdp_country)&data$country=="Hong Kong","gdp_country"]<-369200000000
data[is.na(data$gdp_country)&data$country=="Taiwan","gdp_country"]<-790700000000
data[is.na(data$gdp_country)&data$country=="Ireland","gdp_country"]<-504200000000
summary(data$gdp_country)
#uzupełnienie pozostałych obserwacji brakujących przy pomocy mediany
median_gdp_country<-median(data$gdp_country,na.rm=TRUE)
data$gdp_country[is.na(data$gdp_country)]<-median_gdp_country
summary(data$gdp_country)

#sprawdzenie kolumny gross_terirary_education_enrollment
data[is.na(data$gross_tertiary_education_enrollment),]
summary(data$gross_tertiary_education_enrollment)
#uzupełnienie obserwacji brakujących danymi z internetu
data[is.na(data$gross_tertiary_education_enrollment)
     &data$country=="Hong Kong","gross_tertiary_education_enrollment"]<-104
summary(data$gross_tertiary_education_enrollment)
data[is.na(data$gross_tertiary_education_enrollment)
     &data$country=="Taiwan","gross_tertiary_education_enrollment"]<-48.8
summary(data$gross_tertiary_education_enrollment)
#uzupełnienie pozostałych brakujących danych przy pomocy mediany
median_gross_tertiary_edu<-median(data$gross_tertiary_education_enrollment,na.rm=TRUE)
data$gross_tertiary_education_enrollment[is.na
                                         (data$gross_tertiary_education_enrollment)]<-
  median_gross_tertiary_edu
summary(data$gross_tertiary_education_enrollment)


#sprawdzenie kolumny gross_primary_education_enrollment_country
summary(data$gross_primary_education_enrollment_country)
data[is.na(data$gross_primary_education_enrollment_country),]
#uzupełnienie brakujących danych danymi z internetu
data[is.na(data$gross_primary_education_enrollment_country)&
       data$country=="Hong Kong","gross_primary_education_enrollment_country"]<-103.64
data[is.na(data$gross_primary_education_enrollment_country),]
#uzupełnienie pozostałych brakujących danych przy pomocy mediany
median_primary_edu<-median(data$gross_primary_education_enrollment_country,na.rm=TRUE)
data$gross_primary_education_enrollment_country[is.na(data$gross_primary_education_enrollment_country)]<-median_primary_edu
summary(data$gross_primary_education_enrollment_country)

#sprawdzenie kolumny life_expectancy
summary(data$life_expectancy_country)
#sprawdzenie dla których krajów brakuje danych o life expectancy
missing_data_life_expectancy<-factor(data$country[is.na(data$life_expectancy_country)])
summary(missing_data_life_expectancy)
#uzupełnienie obserwacji brakujących przy pomocy danych z internetu
data[is.na(data$life_expectancy_country)&data$country=="Hong Kong",
     "life_expectancy_country"]<-85.4 #dane z internetu
data[is.na(data$life_expectancy_country)&data$country=="Taiwan",
     "life_expectancy_country"]<-81.2 #dane z internetu
#uzupełnienie pozostałych obserwacji brakujących wartością średnią
mean_life_expectancy<-mean(data$life_expectancy_country,na.rm=TRUE)
mean_life_expectancy
data$life_expectancy_country[is.na(data$life_expectancy_country)]<-mean_life_expectancy
summary(data$life_expectancy_country)
str(data)

#sprawdzenie kolumny tax revenue country
summary(data$tax_revenue_country_country)
#uzupelnienie brakujących danych medianą
median_tax_revenue<-median(data$tax_revenue_country_country,na.rm=TRUE)
data$tax_revenue_country_country[is.na
                                 (data$tax_revenue_country_country)]<-median_tax_revenue
summary(data$tax_revenue_country_country)

#sprawdzenie kolumny total tax rate country
summary(data$total_tax_rate_country)
#uzupełnienie brakujących danych medianą
median_total_tax<-median(data$total_tax_rate_country,na.rm=TRUE)
data$total_tax_rate_country[is.na(data$total_tax_rate_country)]<-median_total_tax
summary(data$total_tax_rate_country)

str(data)
#sprawdzenie kolumny population country
summary(data$population_country)
#sprawdzenie dla których krajów brakuje danych
missing_data_population<-factor(data$country[is.na(data$population_country)])
summary(missing_data_population)
#uzupełnienie obserwacji brakujących przy pomocy danych z internetu
data[is.na(data$population_country)&data$country==
       "Hong Kong","population_country"]<-7413000 #dane z internetu
data[is.na(data$population_country)&data$country==
       "Taiwan","population_country"]<-23570000 #dane z internetu
data[is.na(data$population_country)&data$country==
       "British Virgin Islands","population_country"]<-31122 #dane z internetu
data[is.na(data$population_country)&data$country==
       "Guernsey","population_country"]<-63950 #dane z internetu
data[is.na(data$population_country)&data$country==
       "Bahamas","population_country"]<-407906 #dane z internetu
data[is.na(data$population_country)&data$country==
       "Cayman Islands","population_country"]<-68136 #dane z internetu
data[is.na(data$population_country)&data$country==
       "Turks and Caicos Islands","population_country"]<-45114 #dane z internetu
data[is.na(data$population_country)&data$country==
       "Eswatini (Swaziland)","population_country"]<-1192000 #dane z internetu
data[is.na(data$population_country)&data$country==
       "Ireland","population_country"]<-5033000 #dane z internetu
#uzupełnienie pozostałych danych medianą
median_population_country<-median(data$population_country,na.rm=TRUE)
median_population_country
data$population_country[is.na(data$population_country)]<-median_population_country
summary(data$population_country)


#Sprawdzenie, czy uzupełniono już wszystkie dane
str(data)
data[!complete.cases(data),]
#Wybór danych do analizy statystycznej - rezygnacja z kolumn birthDate, tile, date, 
#cpi_change_country,latitude_country,longitude_country, category
data_to_analysis<-subset(data,select=-c(birthDate,title,date,cpi_change_country,
                                        latitude_country,longitude_country,category))
str(data_to_analysis)

#Sprawdzenie, czy w kolumnach danych wybranych do analizy są jeszcze dane brakujące
data_to_analysis[!complete.cases(data_to_analysis),]

###ANALIZA STATYSTYCZNA#######

#Posumowanie i sprawdzenie struktury danych
str(data_to_analysis)
summary(data_to_analysis)

#Wnioski:
#1 Maksymalna wartość majątku spośród wszystkich miliarderów w znaczącym stopniu 
#odbiega od wartości średniej i mediany
#2 Średni wiek miliardera to 65 lat
#3 Najwięcej miliarderów mieszka w USA, Chinach oraz Indiach
#4 Najwięcej miliarderów uzyskało swój majątek w branży "Finanse i inwestycje"
#5 Większość miliarderów uzyskała swój majątek samodzielnie
#6 Większość miliarderów to mężczyźni


median(data_to_analysis$finalWorth)

#10 krajów o największej liczbie miliarderów
table(data_to_analysis$country)[order(table(data_to_analysis$country),
                                      decreasing = TRUE)][1:10]

#Porównanie statystyk wartości majątku netto miliarderów z kilku krajów
max(data_to_analysis$finalWorth[data_to_analysis$country=="United States"])
max(data_to_analysis$finalWorth[data_to_analysis$country=="China"])
max(data_to_analysis$finalWorth[data_to_analysis$country=="India"])
max(data_to_analysis$finalWorth[data_to_analysis$country=="Germany"])
max(data_to_analysis$finalWorth[data_to_analysis$country=="Poland"])

#W USA, Chinach, Indiach oraz Niemczech maksymalna wartość majątku miliarderów 
#jest wielokrotnie większa, niż wartość majątku jaką posiada 
#najbogatsza osoba w Polsce


sd(data_to_analysis$finalWorth[data_to_analysis$country=="United States"])
sd(data_to_analysis$finalWorth[data_to_analysis$country=="China"])
sd(data_to_analysis$finalWorth[data_to_analysis$country=="India"])
sd(data_to_analysis$finalWorth[data_to_analysis$country=="Germany"])
sd(data_to_analysis$finalWorth[data_to_analysis$country=="Poland"])
sd(data_to_analysis$finalWorth)

#Ochylenie standardowe wartości netto miliardera w USA jest większe od 
#odchylenia standardowego wartości netto miliardera dla całego zbioru danych. 
#Majątek poszczególnych miliarderów w USA charakteryzuje się większym rozrzutem, 
#niż przeciętnie dla całego zbioru danych

#Wartość odchylenia standardowego dla wartości netto miliardera w Polsce jest
#mniejsze od odchylenia standardowego wartości netto miliardera dla całego zbioru danych.
#Majątek poszczególnych miliarderów w Polsce charakteryzuje się mniejszym rozrzutem
#niż przeciętnie dla całego zbioru danych

mean(data_to_analysis$finalWorth[data_to_analysis$country=="United States"])
mean(data_to_analysis$finalWorth[data_to_analysis$country=="China"])
mean(data_to_analysis$finalWorth[data_to_analysis$country=="India"])
mean(data_to_analysis$finalWorth[data_to_analysis$country=="Germany"])
mean(data_to_analysis$finalWorth[data_to_analysis$country=="Poland"])
mean(data_to_analysis$finalWorth)

#Średnie wartości majątku netto miliarderów w poszczególnych analizowanych krajach
#są zbliżone do wartości średniej dla całego zbioru danych. Dla USA, Chin, Indii oraz Niemiec
#wartości maksymalne majątku netto miliarderów znacznie odbiegają od wartości średnich.

#Poszczególne branże uporządkowane wg ilości miliarderów
table(data_to_analysis$industries)[order(table(data_to_analysis$industries),decreasing=TRUE)]

#Porównanie statystyk wartości majątku netto miliarderów wg branż
max(data_to_analysis$finalWorth[data_to_analysis$industries=="Finanse i inwestycje"])
max(data_to_analysis$finalWorth[data_to_analysis$industries=="Produkcja"])
max(data_to_analysis$finalWorth[data_to_analysis$industries=="Technologia"])
max(data_to_analysis$finalWorth[data_to_analysis$industries=="Energia"])
max(data_to_analysis$finalWorth[data_to_analysis$industries=="Opieka zdrowotna"])

#Wartości majątku netto miliarderów dla branż Finanse i inwestycje oraz Technologia są
#kilkukrotnie wyższe, niż dla branż takich jak Produkcja, Energia czy Opieka zdrowotna

sd(data_to_analysis$finalWorth[data_to_analysis$industries=="Finanse i inwestycje"])
sd(data_to_analysis$finalWorth[data_to_analysis$industries=="Produkcja"])
sd(data_to_analysis$finalWorth[data_to_analysis$industries=="Technologia"])
sd(data_to_analysis$finalWorth[data_to_analysis$industries=="Energia"])
sd(data_to_analysis$finalWorth[data_to_analysis$industries=="Opieka zdrowotna"])
sd(data_to_analysis$finalWorth)

#Branża technologiczna charakteryzuje się wyższą wartością odchylenia standardowego wartości
#majątku netto miliarderów niż pozostałe analizowane branże. Branża produkcyjna oraz energetyczna
#charakterysują się niższą wartością odchylenia standardowego wartości netto miliarderów
#niż przeciętna dla całego zbioru danych.

mean(data_to_analysis$finalWorth[data_to_analysis$industries=="Finanse i inwestycje"])
mean(data_to_analysis$finalWorth[data_to_analysis$industries=="Produkcja"])
mean(data_to_analysis$finalWorth[data_to_analysis$industries=="Technologia"])
mean(data_to_analysis$finalWorth[data_to_analysis$industries=="Energia"])
mean(data_to_analysis$finalWorth[data_to_analysis$industries=="Opieka zdrowotna"])
mean(data_to_analysis$finalWorth)

#Wartość średnia majątku netto miliarderów w branży technologicznej jest znacznie wyższa
#niż wartość średnia tego parametru dla całego zbioru danych. Dla pozostałych analizowanych
#branż, wartość średnia jest mniejsza od wartości średniej dla całego zbioru danych - 
#najniższą średnią w analizowanych pięciu branżach (Finanse i inwestycje, Produkcja, Technologia,
#Energia,Opieka zdrowotna) charakteryzuje się Produkcja, następnie Opieka zdrowotna. 

median(data_to_analysis$finalWorth[data_to_analysis$selfMade=="Majątek uzyskany samodzielnie"])
median(data_to_analysis$finalWorth[data_to_analysis$selfMade=="Majątek uzyskany niesamodzielnie"])

#Mediana wartości majątku netto miliarderów w przypadku majątku uzyskanego niesamodzielnie jest 
#wyższa (o 0.4 mld USD), niż dla majątku uzyskanego samodzielnie

mean(data_to_analysis$finalWorth[data_to_analysis$selfMade=="Majątek uzyskany samodzielnie"])
mean(data_to_analysis$finalWorth[data_to_analysis$selfMade=="Majątek uzyskany niesamodzielnie"])

#Średnia wartości majątku netto miliarderów dla majątku uzyskanego niesamodzielnie jest wyższa
#(o ok. 0.5 mld USD) niż dla majątku uzyskanego samodzielnie

str(data_to_analysis)
cor(data_to_analysis$cpi_country,data_to_analysis$finalWorth)
cor(data_to_analysis$gdp_country,data_to_analysis$finalWorth)
cor(data_to_analysis$gross_tertiary_education_enrollment,data_to_analysis$finalWorth)
cor(data_to_analysis$gross_primary_education_enrollment_country,data_to_analysis$finalWorth)
cor(data_to_analysis$total_tax_rate_country,data_to_analysis$finalWorth)
cor(data_to_analysis$population_country,data_to_analysis$finalWorth)

#Obliczono korelację pomiędzy finalną wartością majątku netto miliarderów w poszczególnych krajach,
#a parametrami: Wskaźnikiem cen towarów i usług konsumpcyjnych,
#produktem krajowym brutto, skolaryzacją brutto w szkolnictwie 
#podstawowym i wyższym, całkowitą stawką podatku oraz populacją w danym kraju.
#Na podstawie przeprowadzonych obliczeń można wywniosokować, że pomiędzy wymienionymi parametrami
#a wartością majątku netto miliarderów w poszczególnych krajach, występuje słaba korelacja.

####WYKRESY#######

install.packages("viridis")
library("viridis")

dev.off()

boxplot(data_to_analysis$age~data_to_analysis$gender,
        ylab="Wiek podany w latach",xlab="Płeć",
        col=c("deeppink2","deepskyblue4"))
title("Rozkład wieku wśród miliarderów w podziale na płeć")

#Na podstawie wykresu rozkładu wieku wśród miliaredrów w podziale na płeć
# można wywnioskować, że większość miliarderów na świecie
#to osoby po 50 a przed 80 rokiem życia - dotyczy to zarówno mężczyzn, 
#jak i kobiet

boxplot(data_to_analysis$finalWorth~data_to_analysis$gender,log="y",
        col=c("deeppink2","deepskyblue4"),xlab="Płeć",
        ylab="Wartość netto miliarderów  w mld USD")
title("Rozkład majątku wśród miliarderów w podziale na płeć")

#Na podstawie wykresu rozkładu majątku wśród miliarderów w podziale na płeć
#można wywnioskować, że większość miliarderów na świecie
#dysponuje majątkiem pomiędzy około 1,5 a 5 mld USD. Pojedyncze osoby
#dysponują wielokrotnie większym majątkiem od pozostałych. 
#Mediana, dolny kwartyl i górny kwartyl zarówno dla mężczyzn jak i kobiet
#są na podobnym poziomie.

boxplot(data_to_analysis$finalWorth~data_to_analysis$selfMade,log="y",col=viridis(3),ylab="Wartość netto miliarderów  w mld USD",xlab="Sposób uzyskania majątku")
title("Rozkład majątku wśród miliarderów w podziale \nna sposób uzyskania majątku")
par(cex.axis=1)

#Na podstawie wykresu rozkładu majątku wśród miliarderów w podziale na sposób
#uzyskania majątku można wywnioskować, że osoby które uzyskały swój majątek
#niesamodzielnie dysponują na ogół nieco większym majątkiem, niż osoby
#które uzyskały pieniądze samodzielnie

boxplot(data_to_analysis$finalWorth~data_to_analysis$status,log="y",las=1,
        ylab="Wartość netto miliarderów  w mld USD",xlab="Status majątku",col=terrain.colors(7))
title("Rozkład majątku wśród miliarderów\n w podziale na status")
par(cex.axis=0.5)

#Na podstawie wykresu rozkładu majątku wśród miliarderów w podziale na 
#status można wywnioskować, że osoby które są założycielami firm, dzięki
#którym uzyskały majątek dysponują średnio nieco mniejszą ilością pieniędzy
#niż osoby, które odziedziczyły lub w inny sposób uzyskały swoje bogactwo

dev.off()

mosaicplot(table(data_to_analysis$industries,data_to_analysis$selfMade),
           col=c("darkseagreen4","gold"), 
           main="Miliarderzy w podziale na branże oraz \nsposób uzyskania majątku",
           las=1,dir=c("h","v"))

#Na podstawie mozaiki przedstawiającej miliarderów w podziale na branże oraz
#sposób uzyskania majątku można powiedzieć, że największy udział osób,
#którym samodzielnie udało się uzyskać majątek, występuje w branżach:
#technologicznej, telekomunikacyjnej, hazardzie i kasynach.
#W branżach motoryzacyjnej oraz w budownictwie i inżynierii podział jest
#w przybliżeniu równy.
#Największy udział osób, które uzyskały swój majątek niesamodzielnie, 
#występuje wśród osób o zdywersyfikowanych źródłach przychodu, a następnie w 
#branżach: budownictwo i inżynieria, motoryzacja, żywność i napoje.


mosaicplot(table(data_to_analysis$industries,data_to_analysis$gender),
           col=c("deeppink2","deepskyblue4"), main="Miliarderzy w podziale na branże oraz płeć",
           las=1,dir=c("h","v"))

#Na podstawie mozaiki przedstawiającej miliarderów w podziale na branże oraz
#płeć można powiedzieć, że największy udział kobiet wśród miliaredrów 
#występuje w branży spożywczej, następnie w usługach, sporcie, hazardzie i kasynach.
#Branże charakteryzujące się najmniejszym udziałem kobiet wśród miliarderów
#to: telekomunikacja, finanse i inwestycje, motoryzacja oraz energia.
