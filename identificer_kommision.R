################################
### Identificer kommission

setwd("~/My Dropbox/R/soc.sna/")
library(igraph)

ind  <- read.csv("indsamling_kommission.csv", sep="|", fileEncoding="UTF-8")
data <- read.csv("Data_inderkreds_individer.csv", sep="|", fileEncoding="UTF-8")

dn <- data$Navn

indo <- ind[ind$BIQ_ID != 0,]
indo <- indo[indo$Data_kilde != "Bestyrelse",]
bid  <- indo$BIQ_ID
bestyrelse <- ind[ind$BIQ_ID %in% bid & ind$Data_kilde == "Bestyrelse",]
# bestyrelse er det objekt der indeholder alle inderkredsmedlemmer med kommissionsposter - med navne og rækker fra bestyrelsesdatasættet.

kommission <- ind[ind$Data_kilde == "Kommission",]
bestyrelsesmedlem <- as.factor(kommission$BIQ_ID != 0)
levels(bestyrelsesmedlem) <- c("Ingen bestyrelsesposter", "Bestyrelsespost")
kommission$bestyrelsesmedlem <- bestyrelsesmedlem
# kommission er det objekt der indeholder alle kommissionsmedlemmer med bestyrelsesposter - med navne og rækker fra kommissionsdatasættet.

# I har nu også en række med BIQ_ID som I kan subsette jeres relationsmatrice med.


write.table(kommission, file="kommissioner_med_bestyrelsesposter", sep="|", fileEncoding="UTF-8")


