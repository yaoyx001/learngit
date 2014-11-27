FunctionFlaechen <- function(v.AngabeOrdner){
  v.Ordner <- paste("Z:/ESM/Masterarbeit/", v.AngabeOrdner, "/20141126_Modell_Gross - Kopie.b17",sep="") 
  v.Daten <- readLines(v.Ordner)  # ".out" lesen

  daten.Flaechen <- data.frame()
  for (i in v.Daten){
    if (grepl("^ZONE",i)){
      Name.Zone <- substr(i,6,nchar(i))
    }
    else if (grepl("^WALL  =EXT_WALL",i)){
      Type.Flaechen <- "AW"
      a <- unlist(gregexpr(":",i)) 
      A <- as.numeric(substr(i,a[2]+8,a[3]-1))
      Ori.Flaechen <- substr(i,a[4]+6,a[4]+6)
      list.Flaechen <- data.frame(A, Name.Zone, Type.Flaechen, Ori.Flaechen)
      daten.Flaechen <- rbind(daten.Flaechen, list.Flaechen)
    }
    
    else if (grepl("^WINDOW=EXT_WINDOW1",i)){
      Type.Flaechen <- "AF"
      a <- unlist(gregexpr(":",i)) 
      A <- as.numeric(substr(i,a[2]+8,a[3]-1))
      Ori.Flaechen <- substr(i,a[4]+6,a[4]+6)
      list.Flaechen <- data.frame(A, Name.Zone, Type.Flaechen, Ori.Flaechen)
      daten.Flaechen <- rbind(daten.Flaechen, list.Flaechen)
    }
    
    else if (grepl("^WALL  =GROUND_FLOOR",i)){
      Type.Flaechen <- "BO"
      a <- unlist(gregexpr(":",i)) 
      A <- as.numeric(substr(i,a[2]+8,a[3]-1))
      Ori.Flaechen <- "H"
      list.Flaechen <- data.frame(A, Name.Zone, Type.Flaechen, Ori.Flaechen)
      daten.Flaechen <- rbind(daten.Flaechen, list.Flaechen)
    }
    
    else if (grepl("^WALL  =EXT_ROOF",i)){
      Type.Flaechen <- "DA"
      a <- unlist(gregexpr(":",i)) 
      A <- as.numeric(substr(i,a[2]+8,a[3]-1))
      Ori.Flaechen <- substr(i,a[4]+6,a[4]+6)
      list.Flaechen <- data.frame(A, Name.Zone, Type.Flaechen, Ori.Flaechen)
      daten.Flaechen <- rbind(daten.Flaechen, list.Flaechen)
    }
    else if (grepl("VOLUME",i)){
      Type.Flaechen <- "NutzF"
      a <- unlist(gregexpr(":",i)) 
      A <- as.numeric(substr(i,a[1]+9,a[2]-1))/3.35  # Volumen/ Raumhöhe
      Ori.Flaechen <- "H"
      list.Flaechen <- data.frame(A, Name.Zone, Type.Flaechen, Ori.Flaechen)
      daten.Flaechen <- rbind(daten.Flaechen, list.Flaechen)
    }
  }
  return(daten.Flaechen)
}
