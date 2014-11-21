# Function fuer Datenlesen
# Yuan.Yao 2014-11-18
# Start--------------------------------------------

# Konstant 
kFlaechenZone <- c(410.5, 410.5, 410.5, 80.75, 6.12, 80.75, 6.12, 80.75, 6.12, 11.88, 14.38, 11.88, 14.38,
                   11.38, 14.38, 25, 42.62, 25, 42.62, 25, 42.62, 37.62, 175, 7.35)  # Flaechen jeder Zone
kFlaechenNP <- c(410.5*3,(80.75+6.12)*3,(11.88+14.38)*3,(25+42.62)*3+37.62,175+7.35)  #F laechen Nutzprofil
kNameZone <- c("0_P03_01" ,"1_P03_01", "2_P03_01", "0_P04_01", "0_P04_02", "1_P04_01" ,"1_P04_02", "2_P04_01", "2_P04_02" ,"0_P16_01" 
               ,"0_P16_02", "1_P16_01" ,"1_P16_02", "2_P16_01", "2_P16_02", "0_P19_01",
               "0_P19_02" ,"1_P19_01" ,"1_P19_02", "2_P19_01", "2_P19_02", "3_P19_01", "3_P20_01" ,"3_P20_02")
kNameNP <- c("P03","P04","P16","P19","P20")
kAlteName <- c("QHEAT.", "QCOOL.", "QINF.", "QVENT.", "QTRANS.", "QGINT.", "QSOL." )
kNeuName <- c("Heiz", "Kuhl", "Infil", "Vent", "Tran", "InLast","Solar")
kWaste <- c("QBAL", "DQAIR", "QWGAIN","QCOUP","SOLAIR")

names(kFlaechenZone) <- kNameZone
names(kFlaechenNP) <- kNameNP
# Datenlesen----------------------------------------------------------------
FunctionDatenL <- function(v.AngabeOrdner, v.Type){  # Ordner nur Ordnername, Type ist c("D","V","R")
  # Funtionen
  FunctionTrennH <- function(x){
    v.Heiz <- x
    v.Heiz[which(v.Heiz>0)] <- 0
    v.Heiz
  }  # Trennung Heiz- und Kuhlenergiebedarf
  FunctionTrennK <- function(x){
    v.Kuhl <- x
    v.Kuhl[which(v.Kuhl<0)] <- 0
    v.Kuhl
  }  # Trennung Heiz- und Kuhlenergiebedarf
  # Daten lesen
  if (v.Type =="D"){  # Daten für Detaliertes Modell
    v.Ordner <- paste("Z:/ESM/Masterarbeit/", v.AngabeOrdner, "/D.out",sep="") 
    v.Daten <- read.delim(v.Ordner)  # ".out" lesen
  }
  else if (v.Type == "V"){ # Daten für Vereinfachtes Modell
    v.Ordner <- paste("Z:/ESM/Masterarbeit/", v.AngabeOrdner, "/Vereinfachte.out",sep="") 
    v.Daten <- read.delim(v.Ordner)  # ".out" lesen
  }
  else if (v.Type == "R"){ # Daten für Vereinfachtes-Real Modell
    v.Ordner <- paste("Z:/ESM/Masterarbeit/", v.AngabeOrdner, "/R.out",sep="") 
    v.Daten <- read.delim(v.Ordner)  # ".out" lesen
  }  
  else {
    print ("Falsche Type")
  }  
  # Daten waschen
  v.Daten <- v.Daten[which(v.Daten$TIME>168),]  # betrachte Zeitraum (169 - 8929)
  v.Daten <- v.Daten[, -length(v.Daten)]  # letzte leer Columns löschen
  # fuer D
  if (v.Type == "D") {
    # Energiebedarf und Temperatur aufteilen
    v.EnergieZone <- v.Daten[, grep("^Q",names(v.Daten))]/3600  # kJ zu kWh/m2
    v.TempZone <- v.Daten[, grep("^TA", names(v.Daten))]
    # Bearbeiten Energiebedarf
    v.HeizZone <- as.data.frame(apply(v.EnergieZone, 2, FunctionTrennH))
    v.KuhlZone <- as.data.frame(apply(v.EnergieZone, 2, FunctionTrennK))
    names(v.EnergieZone) <- kNameZone
    names(v.HeizZone) <- kNameZone
    names(v.KuhlZone) <- kNameZone
    v.HeizNP <- matrix(0,nrow(v.EnergieZone), 5)
    v.HeizNP <- as.data.frame(v.HeizNP)
    names(v.HeizNP) <- kNameNP
    v.KuhlNP <- v.HeizNP  # Formatierung
    for (i in kNameNP){
    v.HeizNP[i] <- rowSums(v.HeizZone[, grep(i, kNameZone)])  # Heizenergiebedarf für jeder NP
    v.KuhlNP[i] <- rowSums(v.KuhlZone[, grep(i, kNameZone)])  # Kuhlenergiebedarf für jeder NP
    }
    # Berechnung mittel Temperatur
    names(v.TempZone) <- kNameZone
    v.TempNP <- matrix(0,nrow(v.EnergieZone), 5)
    v.TempNP <- as.data.frame(v.TempNP)
    names(v.TempNP) <- kNameNP
    for (i in kNameNP){
      v.TempNP[i] <- apply(v.TempZone[, grep(i, kNameZone)], 1, weighted.mean, w=kFlaechenZone[grep(i, kNameZone)])
    }  
  }  # Ende Daten der Zonen in Detailliertes Modell
  else if (v.Type == "V" | v.Type == "R"){
    v.EnergieNP <- v.Daten[, grep("^Q",names(v.Daten))]/3600
    v.TempNP <- v.Daten[, grep("^TA", names(v.Daten))]
    v.HeizNP <- as.data.frame(apply(v.EnergieNP,2, FunctionTrennH))  # Heizenergiebedarf für jeder NP
    v.KuhlNP <- as.data.frame(apply(v.EnergieNP,2, FunctionTrennK))  # Kuhlenergiebedarf für jeder NP
    names(v.HeizNP) <- kNameNP
    names(v.KuhlNP) <- kNameNP
    names(v.TempNP) <- kNameNP
  }
  # Spezifische Wert berechnen
  if (v.Type == "D") {
    v.HeizSpeZone <- v.HeizZone
    v.KuhlSpeZone <- v.KuhlZone
    for (i in kNameZone){
      v.HeizSpeZone[, i] <-v.HeizSpeZone[, i] / kFlaechenZone[i]
      v.KuhlSpeZone[, i] <-v.KuhlSpeZone[, i] / kFlaechenZone[i] 
    }
  }
  v.HeizSpeNP <- v.HeizNP  
  v.KuhlSpeNP <- v.KuhlNP
  for (i in kNameNP){
    v.HeizSpeNP[, i] <- v.HeizSpeNP[, i] / kFlaechenNP[i] 
    v.KuhlSpeNP[, i] <- v.KuhlSpeNP[, i] / kFlaechenNP[i] 
  }
  # mittlere Temperatur des Modells
  v.TempMittel <- apply(v.TempNP,1,weighted.mean,w=kFlaechenNP)
  # Output
  if (v.Type == "D") {
    v.Ergebniss <- list("HeizSpeZone" = v.HeizSpeZone, "KuhlSpeZone"= v.KuhlSpeZone, "HeizSpeNP" = v.HeizSpeNP,"KuhlSpeNP" = v.KuhlSpeNP
                        ,"TempZone" = v.TempZone, "TempNP" = v.TempNP, "TempMittel" = v.TempMittel)
  }
  else if (v.Type == "V"| v.Type == "R") {
  v.Ergebniss <- list("HeizSpeNP" = v.HeizSpeNP,"KuhlSpeNP" = v.KuhlSpeNP,"TempNP" = v.TempNP, "TempMittel" = v.TempMittel)
  }
  return(v.Ergebniss)  
} 
# Bilanzlesen---------------------------------------------------------------
FunctionBilanzlesen <- function (v.AngabeOrdner,v.Type){
  # Daten lesen
  v.Ordner <- paste("Z:/ESM/Masterarbeit/", v.AngabeOrdner, "/Energy_zone.BAL",sep="") 
  v.Bilanz <- read.table(v.Ordner, header=TRUE, quote="\"", stringsAsFactors=FALSE)  # ".BAL" lesen
  v.LeerSpalt <- grep("^X\\.", names(v.Bilanz))  # leere Spalte finden
  v.Bilanz <- v.Bilanz[-1, c(-2,-v.LeerSpalt)]  # leere Spalte loeschen
  v.Bilanz <- apply(v.Bilanz,2,as.numeric)  # von Text nach numeric wechseln
  v.Bilanz <- as.data.frame(v.Bilanz)  # zu data.frame
  v.Bilanz <- v.Bilanz[-which(v.Bilanz$TIME<169),]  # betrachte Zeitraum (169 - 8929)
  v.Bilanz[,2:length(v.Bilanz)] <- v.Bilanz[,2:length(v.Bilanz)] / 3600  # kJ zu kWh
  # Daten fuer D
  if (v.Type == "D"){  # fuer detailliertes Modell
    v.BilanzZone <- v.Bilanz  # loeschen Spalt TIME
    v.BilanzNP <- as.data.frame(matrix(0,nrow(v.Bilanz),35))  #  später 8760  
    for (i in 1:length(kNameZone)){
      names(v.BilanzZone) <- sub(paste("X", i, ".?B4",sep = ""), kNameZone[i], names(v.BilanzZone))
    }  # Zone Namen geben 
    for (i in kWaste){
      v.Waste <- grep(i,names(v.BilanzZone))
      v.BilanzZone <- v.BilanzZone[, -v.Waste]
    }  # Sinnlos Spalten loeschen
    for (i in 1:length(kAlteName)){
      names(v.BilanzZone) <- sub(kAlteName[i], kNeuName[i], names(v.BilanzZone))
    }
    I <- rep(kNeuName,5)  # Quellen auswaehlen 
    B <- rep(kNameNP, each = 7)  #NP auswaehlen
    for (i in 1:35) {
      v.AuswahlQuellen <- grep(B[i], names(v.BilanzZone))
      v.AuswahlNP <- grep(I[i], names(v.BilanzZone))
      v.BilanzNP[,i] <- rowSums(v.BilanzZone[, v.AuswahlQuellen[v.AuswahlQuellen %in% v.AuswahlNP ]])
      names(v.BilanzNP)[i] <- paste(B[i], I[i], sep="_")
    }
  }
  # Daten fuer V und R
  else if (v.Type == "V"| v.Type == "R"){
    v.BilanzNP <- v.Bilanz[,-1]  # loeschen Spalt TIME 
    for (i in kWaste){
      v.Waste <- grep(i,names(v.BilanzNP),ignore.case = T)
      v.BilanzNP <- v.BilanzNP[, -v.Waste]
    } 
    for (i in 1:length(kAlteName)){
      names(v.BilanzNP) <- sub(kAlteName[i], kNeuName[i], names(v.BilanzNP))
    }
    for (i in 1:length(kNameNP)){
      names(v.BilanzNP) <- sub(paste("X", i, ".?B4",sep = ""), kNameNP[i], names(v.BilanzNP))
    }  
    
  }
  # Speizifische Werten
  v.BilSpeNP <- v.BilanzNP
  for (i in kNameNP){
    v.AuswahlNP2 <- grep(i, names(v.BilanzNP))
    v.BilSpeNP[, v.AuswahlNP2] <- v.BilanzNP[, v.AuswahlNP2] / kFlaechenNP[grep(i, names(kFlaechenNP))] 
  }
  #if (v.Type == "D") {
  #  return (list(v.BilanzZone,v.BilanzNP))
  #}                    
  #else {
  #  return (v.BilanzNP)
  #}
  return (v.BilSpeNP)
}
#End---------------------------------------------------------------------
#Output--------------------------------------------------------------------
#HeizSpeSum <- sum(v.HeizNP) / sum(kFlaechenNP)
#HeizSpeNP <- c(sapply(v.HeizSpeNP, sum), "Sum" = HeizSpeSum)
#KuhlSpeSum <- sum(v.KuhlNP) / sum(kFlaechenNP)
#KuhlSpeNP <- c(sapply(v.KuhlSpeNP, sum), "Sum" = KuhlSpeSum)
#EnergieSpeNP <- rbind("HeizSpeNP" = HeizSpeNP, "KuhlSpeNP"= KuhlSpeNP)
#v.Ergebniss <- list("Spezifische Energiebedarf"= EnergieSpeNP, )
#plot(x <- 1:8760, y = b[["TempMittel"]],type="l",col="blue")
