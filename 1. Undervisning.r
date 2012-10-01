#####################################
#### R - Undervisning ###############

# Hjælpe funktioner
# Find hjælp på:
# R-help
# Stackoverflow.com brug tag [R]
# R-bloggers.com
?max
example(max)

# Øvelse 1:
# Find hjælp til funktionen min()
# Prøv et eksempel fra funktionen min()


##################################################
############# Vectorer ###########################

# Numerical/Integer
tal         <- 1:4
tal
tal * 5
str(tal)
summary(tal)
edit(tal)
str(1.5)              # Numerical
plot(tal)
# Øvelse 2:
# Lav en numerisk vector med 5 udfald
# Lav et summary af din vector

# Character
tekst       <- c("abe", "gris", "hest", "måge")
tekst
str(tekst)
summary(tekst)
edit(tekst)
# Øvelse 3:
# Lav en character vector med 5 udfald

# Logical
logik       <- c(TRUE, TRUE, TRUE, FALSE)
logik
str(logik)
summary(logik)
edit(logik)
as.numeric(logik)
1:10 %in% tal
tekst == "abe"


# Øvelse 4:
# Lav en logical vector med 5 udfald
# Lav en logisk test af hvor mange af din numeric vector der ligger indenfor et bestemt interval 

# Factors
tekst2            <- c(tekst, tekst)
tekst2
faktor            <- as.factor(tekst2)
faktor
str(faktor)
summary(faktor)
edit(faktor)
levels(faktor)
levels(faktor)[1]
levels(faktor)[1] <- "vandmand"
summary(faktor)
levels(faktor)[1] <- "gris"
faktor
levels(faktor)    <- c("arme", "næse", "ben")
faktor
plot(faktor)

# Øvelse 5:
# Lav din character vector om til factor
# Hvor mange levels har din factor?
# Omkod et eller flere af dine factorer

# Length
# Længden af et objekt
length(faktor)
vector(mode="logical", length=10)

# Øvelse 6:
# Hvad er længden af jeres factor?
# Lav en numerisk vector med length 10

########################################
############ Matricer ##################

matrice           <- matrix(1:8, ncol=2)
matrice
str(matrice)
length(matrice)
nrow(matrice)
ncol(matrice)
summary(matrice)
View(matrice)
plot(matrice)
rownames(matrice)
rownames(matrice) <- tekst                   # Rækkenavne
rownames(matrice)
colnames(matrice) <- c("Antal", "Ton foder") # Kolonnenavne
matrice
matrice * 2
log(matrice)
char.mat    <- cbind(matrice, tekst)
str(char.mat)

rbind(tekst, tal, logik)                     # Forbundet via rækker
cbind(tekst, tal, logik)                     # Forbundet via kolonner

# Øvelse 7:
# Lav en matrice af jeres vectorer
# Hvor mange rækker og kolonner har jeres matrice?
# Hvad er række- og kolonne-navnene på jeres matrice?
# Hvilket slags mode har jeres matrice, er den character, logical, numeric eller factor?
# Giv jeres matrice række og kolonne navne

# Data.frames
df          <- data.frame(matrice, logik)
df
str(df)
summary(df)

# Øvelse 8:
# Lav en data.frame af jeres vectorer med data.frame() funktionen
# Hvilket slags mode har jeres data.frame?

# Funktioner
antal.celler  <- function(x, plus=0){
nrow(x)*ncol(x)+plus        # Her ganger vi antallet af rækker med antallet af kolonner og lægger plus til
  }
antal.celler
antal.celler(df)
antal.celler(df, plus=10)   # Her angiver jeg hvad plus skal være med et =
antal.celler(df, 10)        # Her sætter jeg plus værdien på den rigtige plads, altså som det andet objekt i funktionen

# Øvelse 9:
# Udregn antallet af celler med antal.celler funktionen
# Læg 20 til med funktionen

######################################################
################ Logik ###############################
     1 == 1            # Lig med
   "A" == "A"          # Lig med character
faktor == "arme"       # Sammenlign en vector med et udfald
faktor != "arme"       # Ikke lig med
tal
     3 <  tal          # Større end
     3 >  tal          # Mindre end  
     3 <= tal          # Større end eller lig med
     3 >= tal          # Mindre end eller lig med
matrice
matrice %in% c(1,3,6)  # Fællesmængden
            

######################################################
############## Subsetting ############################

### Subsetting med placering
tekst[1]
tekst[1:2]
tekst[c(1,3,4)]
tekst[-c(1,3,4)]
tekst[5]

# Øvelse 10:
# Subset en af jeres vectorer
# Vend jeres subset om med -

### Subsetting med logik
tekst == "abe"
tekst[tekst == "abe"]
tekst[tekst != "abe"]
a <- tekst == "abe" | tekst == "gris"
tekst[a]
tekst[a==FALSE]

# Øvelse 11:
# Subset en vector med et logisk udtryk
# Vend jeres subset om med FALSE

### Subsetting med which()
tekst
which(tekst=="måge")
b <- which(tekst=="måge")
b
tekst[which(tekst=="måge")]
tekst[b]
d <- which(a)
tekst[d]
d <- which(a==FALSE)
tekst[d]

# Øvelse 12:
# Subset en vector med which()
# Assign et logisk udtryk til en selvstændig vector og brug det i which()

### Subsetting i matricer
matrice
matrice[2,2]
matrice[1:2,]
matrice[,2]
foder <- matrice[,2]
foder

# Øvelse 13:
# Udvælg den anden kolonne i jeres matrice
# Udvælg den fjerde række i jeres matrice
# Udvælg den anden række i tredje kolonne i jeres matrice

### Subsetting i dataframes
df
str(df)
df[,1]
df$Antal

# Øvelse 14:
# Subset jeres dataframe med $

