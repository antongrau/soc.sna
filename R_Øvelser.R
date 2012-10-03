##################################################################
############### Øvelser ##########################################
# Øvelserne her er opdelt efter tema og sværhedsgrad, de sidste i
# hvert tema er de sværeste. Forsøg at klare så mange som muligt,
# men gå videre til et andet tema hvis opgaven er for svær.
# Husk at du kan finde hjælp på r-help og på stackoverflow

# Skriv jeres bud ind under "Svar" og når I er færdige så send
# løsningen til os.

# Vi er ikke interesserede i det konkrete svar, 
# men hvordan man når dertil med kode. 

###################################################
################### Indledning ####################

# Set working directory:
# Indsæt her din egen path
setwd("~/My Dropbox/R/interlocks/")

# Indlæs data om virksomhederne

# Windows
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top_U.csv", destfile="Organisation_BIQ_top_U.csv")
# Mac/Linux
download.file("http://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top_U.csv", destfile="Organisation_BIQ_top_U.csv", method="curl")
virk <- read.csv(file="Organisation_BIQ_top_U.csv", sep="|", fileEncoding="UTF-8", dec = ",") 

# Her kan du se hvilke variable der er i datasættet
str(virk)

###################################################
#################### Vectorer #####################
# De her øvelser er på enkelte vectorer
# I kan tænke det som manipulationer af en enkelt variabel

############### Vector øvelse 1 ###################
# Den her øvelse går ud på at forstå:
# Hvordan NA opfører sig
# Hvordan man finder simple mål for en vector
# Hvordan man tilfører ekstra argumenter til en funktion
# Hvordan man læser en hjælp fil og finder nye funktioner ved at gætte

# Spørgsmål:
# Hvor stor er den største virksomheds omsætning?
# Hvad er median omsætningen?
# Hvor mange gange størrere er den største virksomhed i forhold til den mindste?

# Tip: max(x, na.rm=TRUE), ? 

omsætning <- virk$OMSÆTNING.10 # Omsætning i millioner kr. i 2010

#### Svar 


############### Vector øvelse 2 ###################
# Den her øvelse går ud på at forstå:
# Hvordan sætter vi navne på en vector
# Hvordan laver vi en logisk sammenligning mellem to objekter
# Hvordan subsetter vi en vector

navn <- virk$ORG_NAVN # Virksomhedens navn

# Spørgsmål:
# Find navnene på virksomhederne med størst, mindst og median omsætning

# Tip: names(), [], which(), ==

#### Svar


############### Vector øvelse 3 ###################
# Den her øvelse går ud på at forstå:
# Hvordan vi subsetter en vector med data hentet fra en anden

ansatte <- virk$ANSATTE.10 # Antal ansatte i 2010

# Spørgsmål:
# Hvor mange ansatte har den største, mindste og median virksomheden?
# Hvor meget omsætning har virksomheden med flest og færrest ansatte?
# Hvad hedder virksomhederne med færrest og flest ansatte?

# Tip: which(), [], ==, names()

#### Svar


############### Vector øvelse 4 ###################
# Den her øvelse går ud på at forstå:
# Hvordan vi udregner nye værdier på baggrund af to vectorer
# Hvordan man afrunder tal


ansatte.9.til.10    <- virk$ÆNDRING.ANSATTE.9.10 # Ændringen i antal ansatte fra 2009 til 2010 i procent

# Spørgsmål:
# Hvor meget omsætter hver medarbejder for i hver virksomhed?
# Hvor mange ansatte havde virksomheden i 2009?
# Hvilken virksomhed fyrede flest? Hvilken hyrede flest?
# Præsenter tallene med 2 decimaler.

# Tip: round()

#### Svar

############### Vector øvelse 5 ###################
# Den her øvelse går ud på at forstå:
# Hvordan ordner vi en variabel efter størrelse?
# Hvordan vi laver en variabel udfra placering i vector
# Hvordan vi subsetter et interval

# Spørgsmål:
# Ranger virksomederne efter størrelse så den største står på 1. pladsen og nedefter
# Lav en vector med virksomhedens rang
# Vis de tyve største og mindste virksomheder

# Tip: order(), sort(), head(), tail()
# Den her øvelse kan være en smule svær, særligt i forskellen på sort og order - hold tungen lige munden!
# Order giver ikke værdierne, altså omsætning, men den placering virksomheden har i rangeringen
# Sort giver derimod værdierne, men ikke placeringen og den fjerner missing (NA)
# Order bruges også til at ordne data.frames, så vi kommer til at støde på den senere.

### Svar


############### Vector øvelse 6 ###################
# Den her øvelse går ud på at forstå:
# Omkodning af kontinuerte variable til kategorielle variable
# Krydstabeller af kategorielle variable

# Spørgsmål:
# Omkod omsætning til en variabel med 4 udfald, med cirka 20% i hver kategori.

#Tip: quantile(), cut(), table(), prop.table()
# Brug quantile som breaks i cut
# brug prop.table på et table objekt og angiv en margin med et tal fx. 

### Svar


###################################################
#################### Matricer #####################

# De her øvelser er for matricer, men med fokus på netværksmatricer

# Indlæs data:

# Windows
download.file("https://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv")
# Mac/Linux
download.file("http://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv", method="curl")
rel    <- read.csv("Relation_BIQ_top.csv", sep="|", encoding="UTF-8", strip.white=TRUE) 

edges <- data.frame(as.character(rel$NAVN), as.character(rel$ORG_NAVN))
edge.table    <- table(edges)
tabnet        <- as.matrix(edge.table)
adj           <- t(tabnet)%*%tabnet

# adj er en adjacency matrice af virksomheder og det er den vi skal arbejde med i de næste øvelser


############### Matrix øvelse 1 ###################
# Den her øvelse går ud på at forstå:
# Hvordan vi laver en variabel udfra en diagonal
# Hvordan vi fjerner en diagonal
# Hvordan vi bruger funktioner på en matrice

# Spørgsmål:
# Lav en vector på baggrund af diagonalen
# Sæt 0 i diagonalen i matricen adj
# Hvilken virksomhed har flest bestyrelsesmedlemmer?

#Tip: diag()
# Diagonalen er hvor mange bestyrelsesmedlemmer en virksomhed har.

### Svar


############### Matrix øvelse 2 ###################
# Den her øvelse går ud på at forstå:
# Hvordan vi subsetter en matrice
# Hvordan vi bruger funktioner på en matrice
# Hvordan vi identificerer og fjerner isolates

# Spørgsmål:
# Lav en ny matrice uden isolates
# Hvor mange isolates er der?
# Hvilken virksomhed har flest forbindelser?


# Tip: colSums(), rowSums() [,]
# En isolate er en virksomhed uden nogen ties, altså med en kolonne eller række sum på 0
# Husk at have fjernet diagonalen!
# Husk at matricer skal subsettes i to dimensioner [,]


### Svar





###################################################
#################### Data.frames ##################








