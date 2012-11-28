### Køns matching

# Først renser vi vores navne tabel.

setwd("~/Dropbox/R/soc.sna/")
kvinder <- read.csv(file="fornavne 2012 - kvinder.txt", sep="\t", skip=1, fileEncoding="latin1", stringsAsFactors=FALSE)
str(kvinder)
kvinder[, -1] <- apply(kvinder[, -1], 2, as.numeric)
kvinder[is.na(kvinder)] <- 0

mænd <- read.csv(file="fornavne 2012 - mænd.txt", sep="\t", skip=1, fileEncoding="latin1", stringsAsFactors=FALSE)
str(mænd)
mænd[, -1] <- apply(mænd[, -1], 2, as.numeric)
mænd[is.na(mænd)] <- 0

mænd.navn     <- mænd$Drengenavn 
kvinder.navn  <- kvinder$Pigenavn

#### Scenariet hvor det er alle generationer af mænd og kvinder
mænd.antal <- mænd$I.alt
kvinder.antal <- kvinder$I.alt

bi.navne.ind.mænd <- mænd.navn %in% kvinder.navn
summary(bi.navne.ind.mænd)
bi.navne.ind.kvinder <- kvinder.navn %in% mænd.navn
summary(bi.navne.ind.kvinder)
bi.navne.mænd      <- mænd.navn[bi.navne.ind.mænd] 
bi.antal.mænd      <- mænd.antal[bi.navne.ind.mænd]
bi.navne.kvinder   <- kvinder.navn[bi.navne.ind.kvinder]
bi.antal.kvinder   <- kvinder.antal[bi.navne.ind.kvinder] 

bi.kvinder         <- data.frame(bi.navne.kvinder, bi.antal.kvinder)
bi.mænd            <- data.frame(bi.navne.mænd, bi.antal.mænd) 
bi.kvinder         <- bi.kvinder[order(bi.navne.kvinder),]
bi.mænd            <- bi.mænd[order(bi.navne.mænd),]
identical(bi.mænd[,1], bi.kvinder[,1])
# 
# begge.antal        <-  bi.mænd$bi.antal.mænd + bi.kvinder$bi.antal.kvinder
# andel.mænd         <-  round(bi.mænd$bi.antal.mænd / begge.antal * 100, 2)

bi <- data.frame(bi.kvinder, bi.mænd$bi.antal.mænd)

### Her bygger vi den endelige matrice

antal.bi           <- sum(bi.navne.ind.mænd)
mat.length         <- length(mænd.navn) + length(kvinder.navn) - antal.bi
navne.mat          <- as.data.frame(matrix(nrow=mat.length, ncol=5))
colnames(navne.mat) <- c("Navn", "Kvinder", "Mænd", "Samlet", "Andel.mænd")
 
mænd.ind           <- 1:(length(mænd.navn)-antal.bi)
kvinder.ind        <- 1:(length(kvinder.navn)-antal.bi) + max(mænd.ind)
bi.ind             <- 1:length(bi.navne.mænd) + max(kvinder.ind)
navne.mat$Navn[mænd.ind]         <- mænd.navn[bi.navne.ind.mænd==FALSE]
navne.mat$Mænd[mænd.ind]         <- mænd.antal[bi.navne.ind.mænd==FALSE] 
navne.mat$Navn[kvinder.ind]      <- kvinder.navn[bi.navne.ind.kvinder==FALSE]
navne.mat$Kvinder[kvinder.ind]   <- kvinder.antal[bi.navne.ind.kvinder==FALSE]
navne.mat$Navn[bi.ind]           <- as.character(bi[,1])
navne.mat$Kvinder[bi.ind]        <- bi[,2]
navne.mat$Mænd[bi.ind]           <- bi[,3]
navne.mat[is.na(navne.mat)]      <- 0
navne.mat$Samlet                 <- navne.mat$Kvinder + navne.mat$Mænd 
navne.mat$Andel.mænd             <- navne.mat$Mænd / navne.mat$Samlet
b <- c(0, 0.3, 0.7, 1)
kategori <- cut(navne.mat$Andel.mænd, b, include.lowest=TRUE, labels=c("Kvinde", "Binominal", "Mand"))
navne.mat$Kategori               <- kategori
names.gender                     <- navne.mat
save(names.gender, file="names_gender")
write.table(names.gender, file="names_gender.csv", sep="|")

