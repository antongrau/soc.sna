#####################################
#### R - 2. undervisningsgang #######
# Denne her gang skal vi lære om lists og lidt mere avanceret om matricer

#### Først det praktiske

# Sæt jeres working directory:
setwd("~/My Dropbox/R/interlocks/")

# Download data om virksomhederne
# Windows
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top_U.csv", destfile="Organisation_BIQ_top_U.csv")
# Mac/Linux
# Hvis du har en gammel mac så må du hente det ned manuelt og ligge filerne direkte i dit working directory
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top_U.csv", destfile="Organisation_BIQ_top_U.csv", method="curl")

# Indlæs data
virk   <- read.csv(file="Organisation_BIQ_top_U.csv", sep="|", fileEncoding="UTF-8", dec = ",")
# File angiver stien til filen. Når vi bare skriver navnet på filen antages det at filen ligger i working directory
# sep angiver hvilket tegn der angiver adskillelsen af cellerne i filen
# fileEncoding er hvilken encoding vores fil er i
# dec er hvilket tegn der bruges som decimal i filen. Det er vigtigt hvis vi importerer en dansk fil!



###########################################
######### At ordne sine data.frames #######

an     <- data.frame(virk$ORG_NAVN, virk$ANSATTE.10, virk$EGENKAPITAL.10)
order(an$virk.ANSATTE.10, decreasing=TRUE)
ano    <- an[order(an$virk.ANSATTE.10, decreasing=TRUE),]
ano
ano[1:20,]


######################################################
################# Lists ##############################
# Lists er det mest abstrakte objekt - Det består af elementer, der kan bestå af hvad som helst
# 

abe     <- c("A", "B", "E")
snegl   <- matrix(1:10, ncol=2)
listen  <- list(abe, snegl)
listen
str(listen)
listen[[1]]   # Rigtigt!
listen[1]     # Forkert!  Fordi den henter det første element ud som list
listen  <- list(abe=abe, snegl=snegl)
str(listen)
length(listen)
listen$snegl

### Nu til et eksempel fra den "virkelige" verden!
## Factor analyse

eksempel <- data.frame(virk$OMSÆTNING.10, virk$ANSATTE.10, virk$EGENKAPITAL.10, virk$BALANCE.10)
eksempel <- eksempel[complete.cases(eksempel),]
str(eksempel)

fact <- factanal(eksempel, 1)
fact
str(fact)
fact$loadings
fact$criteria
fact$uniquenesses
fact$listen <- listen  # Vi kan assigne ind i noget der ikke findes!


### Øvelse
# Lav en klyngeanalyse og undersøg resultat objektet
cluster <- hclust(dist(eksempel))







