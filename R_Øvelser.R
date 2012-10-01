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

download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top_U.csv", destfile="Organisation_BIQ_top_U.csv")
virk <- read.csv(file="Organisation_BIQ_top_U.csv", sep="|", fileEncoding="UTF-8", dec = ",") 


# Her kan du se hvilke variable der er i datasættet
str(virk)

###################################################
#################### Vectorer #####################
# De her øvelser er på enkelte vectorer

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

# A:
ma    <- max(omsætning, na.rm=TRUE)
mi    <- min(omsætning, na.rm=TRUE)
med   <- median(omsætning, na.rm=TRUE)
ma/mi

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

## A
names(omsætning) <- navn
omsætning[which(omsætning==ma)]
omsætning[which(omsætning==mi)]
omsætning[which(omsætning==med)]


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

## A
names(ansatte) <- navn
ansatte[which(omsætning == ma)]
ansatte[which(omsætning == mi)]
ansatte[which(omsætning == med)]

omsætning[which(ansatte == max(ansatte, na.rm=TRUE))]
omsætning[which(ansatte == min(ansatte, na.rm=TRUE))]

############### Vector øvelse 4 ###################
# Den her øvelse går ud på at forstå:
# Hvordan vi udregner nye værdier på baggrund af to vectorer
# Hvordan man afrunder tal
# 

ansatte.9.til.10    <- virk$ÆNDRING.ANSATTE.9.10 # Ændringen i antal ansatte fra 2009 til 2010 i procent

# Spørgsmål:
# Hvor meget omsætter hver medarbejder for i hver virksomhed?
# Hvor mange ansatte havde virksomheden i 2009?
# Hvilken virksomhed fyrede flest? Hvilken hyrede flest?
# Præsenter tallene med 2 decimaler.

# Tip: round()

#### Svar


## A
omsætning.pr.ansat <- omsætning/ansatte
summary(omsætning.pr.ansat)
round(omsætning.pr.ansat, 2)

hyr.fyr <- round(ansatte - ansatte/(1+ansatte.9.til.10/100))
hyr.fyr




###################################################
#################### Matricer #####################



###################################################
#################### Data.frames ##################








