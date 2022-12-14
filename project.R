#Androni Stefano Mat.845811

library(psych)
library(naniar) 
library(Amelia)
library(ggplot2)
library(car)
library(visdat)
library(tidyverse) 
library(lattice)
library(plyr) 
library(ggpubr)
library(dplyr) 
library(corrplot)
library(RColorBrewer)
library(GGally) 

#1a-DATA ACQUISITION_____________________________________________________________________________________________________________________________________________________

  #Leggo il file (formato: csv) -> viene fornito già diviso in train e test 
    f_train = "C:/Users/Stefano/Desktop/Progetto/train.csv"
    f_test = "C:/Users/Stefano/Desktop/Progetto/test.csv"

    d_train = read.csv(f_train, stringsAsFactors=TRUE) 
    d_test = read.csv(f_test, stringsAsFactors=TRUE)
  
  #Creo un unico dataframe /Data Integration/    
    df = rbind(d_train, d_test) #rbind -> unione di due dataframe (per righe) 
    
  #Primo sguardo
    head(df, 10) #prime 10 righe
    tail(df, 10) #ultime 10 righe
    some(df, 10) #random 10 righe
    
    
#1b-DATA PRE PROCESSING___________________________________________________________________________________________________________________________________________________
  
  dim(df) #dimensione dataframe
  str(df)
    #NOTA
      #- Numero righe[istanze]: 129880, Numero colonne[attributi/features]: 25 (si potevano utilizzare anche nrow(df) e ncol(df))
      #- Gli attributi 'X' e 'id' sono inutili ai fini di analisi ('X' è inutile in ogni caso) -> Id utile solo per verificare coerenza dei dati
      #- Meglio rinominare le colonne (gli attributi che valutano la soddisfazione le identifico facendole iniziare con 'Sat')
      #- Gli attributi che valutano la soddisfazione nei vari campi è utile definirli come fattori
      #- Sarebbe utile anche avere un 'punteggio totale di soddisfazione' come colonna
    
  #Elimino la prima colonna ('X') /Data Reduction/
    df = df[2:ncol(df)]
    
  #Rinomino le colonne con un nome più significativo
    NewNames <- c('Id', 'Gender', 'CustomerType', 'Age', 'TypeOfTravel', 'Class', 'FlightDistance', 'Sat.InflightWifiService',
                  'Sat.FlightSchedule', 'Sat.EaseOnlineBooking', 'Sat.GateLocation', 'Sat.FoodDrink', 'Sat.OnlineBoarding', 
                  'Sat.SeatComfort', 'Sat.InflightEntertainment','Sat.OnBoardService', 'Sat.Legroom', 'Sat.BaggageHandling',
                  'Sat.CheckinService', 'Sat.InflightService', 'Sat.Cleanliness','DepartureDelayMinutes', 'ArrivalDelayMinutes', 
                  'Satisfaction')
    names(df) <- NewNames
    
  #Rendo le variabili che calcolano la soddisfazione dei fattori (levels da 0 a 5)
    df$Sat.InflightWifiService = as.factor(df$Sat.InflightWifiService)   
    df$Sat.FlightSchedule = as.factor(df$Sat.FlightSchedule) 
    df$Sat.EaseOnlineBooking = as.factor(df$Sat.EaseOnlineBooking) 
    df$Sat.GateLocation = as.factor(df$Sat.GateLocation) 
    df$Sat.FoodDrink = as.factor(df$Sat.FoodDrink) 
    df$Sat.OnlineBoarding = as.factor(df$Sat.OnlineBoarding) 
    df$Sat.SeatComfort = as.factor(df$Sat.SeatComfort) 
    df$Sat.InflightEntertainment = as.factor(df$Sat.InflightEntertainment) 
    df$Sat.OnBoardService = as.factor(df$Sat.OnBoardService) 
    df$Sat.Legroom = as.factor(df$Sat.Legroom) 
    df$Sat.BaggageHandling = as.factor(df$Sat.BaggageHandling) 
    df$Sat.CheckinService = as.factor(df$Sat.CheckinService) 
    df$Sat.InflightService = as.factor(df$Sat.InflightService) 
    df$Sat.Cleanliness = as.factor(df$Sat.Cleanliness) 

    str(df) #check modifiche

    
#DATA CLEANING

 #<<<<<<MISSING VALUES 1 >>>>>>
  
  #Conto i missing values per ogni colonna
     
    #NOTA
      #L'attributo 'ArrivalDelayMinutes' ha 393 Missing Values 
      sum(is.na(df$ArrivalDelayMinutes)==TRUE) #393
      #I missing values dell'attributo sono 0.302587% (in rapporto all'attributo)
      sum(is.na(df$ArrivalDelayMinutes)==TRUE) / (nrow(df)) * 100
#VISUALIZZAZIONE
  #Visualizzazione con 'NANIAR' 
    #vis_miss(df, sort_miss = TRUE, warn_large_data=FALSE)
       
  #Visualizzazione con 'AMELIIA'      
    #require(Amelia)
    #missmap(df, main="MissingMap", legend=TRUE, x.labels=(sum(is.na(df$ArrivalDelayMinutes)==TRUE)))    
    #AmeliaView()
    #Amelia indica lo 0% perchè non ha precisione nei decimali
            
  #Visualizzazione con 'GGPLOT'      
    #gg_miss_var(df)  + labs(y = "# of Missing Values")   + theme_bw() # buna 
        
         #ggplot(df, 
              #       aes(x = ArrivalDelayMinutes, 
              #           y = DepartureDelayMinutes)) + 
              #  geom_miss_point()
#STRATEGIA
  #i Missing Values sono davvero pochi in percentuale(non utilizzo nessuno algorito ML per calcolarli), quindi:
  #1- li sostituisco con la media e Arrotondo all'intero perchè indicano i minuti.
      #df$ArrivalDelayMinutes[is.na(df$ArrivalDelayMinutes)] = round(mean(df$ArrivalDelayMinutes, na.rm = TRUE))  
  #2- li sostituisco con i minuti di ritardo alla partenza (fortemente correlati)   
      df$ArrivalDelayMinutes <- ifelse(is.na(df$ArrivalDelayMinutes), df$DepartureDelayMinutes, df$ArrivalDelayMinutes)      
  #Scelgo la 2a opzione per mantenere la correlazione tra i due atttributi
      #Check modifiche
          sapply(df, function(x) sum(is.na(x)==TRUE))
        #Visualizzazione con 'GGPLOT'      
          #gg_miss_var(df)  + labs(y = "# of Missing Values")   + theme_bw() # buna 
        #Visualizzazione con 'NANIAR' 
          #require(naniar)
          #vis_miss(df, warn_large_data=FALSE) 
        #Altro check(non grafico)
          df[(!complete.cases(df)),] #0 righe -> tutte le istanze sono complete
 
          
          
          
           
  #Dopo aver sistemato i missing values, dalle indicazini che abbiamo sul dataset, se nelle variabili, che calcolano
  #la soddisfazione da 1 a 5, il valore è 0, questo è un NA (Not Applicable), cioè il cliente non ha espresso la sua 
  #preferenza (potrebbe anche non aver usufruito del servizio?). Per ricordarcelo trasformo tutti i '0' in NA 
  # [si potrebbero stimare]
        
        
    #Sat.FlightSchedule    
        levels(df$Sat.FlightSchedule) = c(levels(df$Sat.FlightSchedule),NA) #aggiungo il levels "NA" 
        #levels(df$Sat.FlightSchedule) #check livelli
        df$Sat.FlightSchedule[df$Sat.FlightSchedule==0] = NA #sostituisco il livello '0' con il livello 'NA'
        df$Sat.FlightSchedule = factor(df$Sat.FlightSchedule) #questo elimina i livelli non più utilizzati --> '0'
    
    #Sat.InflightWifiService         
        levels(df$Sat.InflightWifiService) = c(levels(df$Sat.InflightWifiService),NA) 
        df$Sat.InflightWifiService[df$Sat.InflightWifiService==0] = NA 
        df$Sat.InflightWifiService = factor(df$Sat.InflightWifiService) 
        
    #Sat.EaseOnlineBooking         
        levels(df$Sat.EaseOnlineBooking) = c(levels(df$Sat.EaseOnlineBooking),NA) 
        df$Sat.EaseOnlineBooking[df$Sat.EaseOnlineBooking==0] = NA 
        df$Sat.EaseOnlineBooking = factor(df$Sat.EaseOnlineBooking)         
        
    #Sat.GateLocation         
        levels(df$Sat.GateLocation) = c(levels(df$Sat.GateLocation),NA) 
        df$Sat.GateLocation[df$Sat.GateLocation==0] = NA 
        df$Sat.GateLocation = factor(df$Sat.GateLocation)   
        
    #Sat.FoodDrink         
        levels(df$Sat.FoodDrink) = c(levels(df$Sat.FoodDrink),NA) 
        df$Sat.FoodDrink[df$Sat.FoodDrink==0] = NA 
        df$Sat.FoodDrink = factor(df$Sat.FoodDrink)   
    
    #Sat.OnlineBoarding         
        levels(df$Sat.OnlineBoarding) = c(levels(df$Sat.OnlineBoarding),NA) 
        df$Sat.OnlineBoarding[df$Sat.OnlineBoarding==0] = NA 
        df$Sat.OnlineBoarding = factor(df$Sat.OnlineBoarding)   
    
    #Sat.SeatComfort         
        levels(df$Sat.SeatComfort) = c(levels(df$Sat.SeatComfort),NA) 
        df$Sat.SeatComfort[df$Sat.SeatComfort==0] = NA 
        df$Sat.SeatComfort = factor(df$Sat.SeatComfort)   
    
    #Sat.InflightEntertainment         
        levels(df$Sat.InflightEntertainment) = c(levels(df$Sat.InflightEntertainment),NA) 
        df$Sat.InflightEntertainment[df$Sat.InflightEntertainment==0] = NA 
        df$Sat.InflightEntertainment = factor(df$Sat.InflightEntertainment)   
    
    #Sat.OnBoardService         
        levels(df$Sat.OnBoardService) = c(levels(df$Sat.OnBoardService),NA) 
        df$Sat.OnBoardService[df$Sat.OnBoardService==0] = NA 
        df$Sat.OnBoardService = factor(df$Sat.OnBoardService)   
    
    #Sat.Legroom         
        levels(df$Sat.Legroom) = c(levels(df$Sat.Legroom),NA) 
        df$Sat.Legroom[df$Sat.Legroom==0] = NA 
        df$Sat.Legroom = factor(df$Sat.Legroom)   
    
    #Sat.BaggageHandling         
        levels(df$Sat.BaggageHandling) = c(levels(df$Sat.BaggageHandling),NA) 
        df$Sat.BaggageHandling[df$Sat.BaggageHandling==0] = NA 
        df$Sat.BaggageHandling = factor(df$Sat.BaggageHandling)   
    
    #Sat.CheckinService         
        levels(df$Sat.CheckinService) = c(levels(df$Sat.CheckinService),NA) 
        df$Sat.CheckinService[df$Sat.CheckinService==0] = NA 
        df$Sat.CheckinService = factor(df$Sat.CheckinService)   
    
    #Sat.InflightService         
        levels(df$Sat.InflightService) = c(levels(df$Sat.InflightService),NA) 
        df$Sat.InflightService[df$Sat.InflightService==0] = NA 
        df$Sat.InflightService = factor(df$Sat.InflightService)   
    
    #Sat.Cleanliness         
        levels(df$Sat.Cleanliness) = c(levels(df$Sat.Cleanliness),NA) 
        df$Sat.Cleanliness[df$Sat.Cleanliness==0] = NA 
        df$Sat.Cleanliness = factor(df$Sat.Cleanliness)   
        
        #Aggiungere colonna utile (DA METTERE ALLA FINE -- FUNZIONA MALE QUI)
        
        df[,"TotalScore"] =  as.integer(df[,"Sat.InflightWifiService"]) +
          as.integer(df[,"Sat.FlightSchedule"]) +
          as.integer(df[,"Sat.EaseOnlineBooking"]) +
          as.integer(df[,"Sat.GateLocation"]) +
          as.integer(df[,"Sat.FoodDrink"] ) +
          as.integer(df[,"Sat.OnlineBoarding"] ) +
          as.integer(df[,"Sat.SeatComfort"] ) +
          as.integer(df[,"Sat.InflightEntertainment"]) +
          as.integer(df[,"Sat.OnBoardService"] ) +
          as.integer(df[,"Sat.Legroom"] ) +
          as.integer(df[,"Sat.BaggageHandling"]) +
          as.integer(df[,"Sat.CheckinService"]) +
          as.integer(df[,"Sat.Cleanliness"] ) +
          as.integer(df[,"Sat.InflightService"]) 
        
##<<<<<<MISSING VALUES 2 >>>>>>    
       
  
   #Visualizzazione con 'GGPLOT'      
    #gg_miss_var(df, show_pct=TRUE) + labs(y = "% of Missing Values")   + theme_light() # percentuale
    
    ################################<Graf.Relazione>  
    gg_miss_var(df[, c("Sat.FlightSchedule","Sat.EaseOnlineBooking", "Sat.InflightWifiService","Sat.OnlineBoarding", 
                       "Sat.Legroom", "Sat.FoodDrink", "Sat.InflightEntertainment")], show_pct=FALSE,) +  
      ggtitle("Number of Missing Values per features") +
      labs(y = "Number of Missing Values", x = "Features")  + 
      theme_light()  # assoluto

    
   #Conto per ogni attributo
    sapply(df, function(x) sum(is.na(x)==TRUE)) 
  
   #Conto celle totali NA 
    table(is.na(df))
    sum(is.na(df)) / (nrow(df)*ncol(df)) * 100 #in percentuale
    
    
   #Quante righe non sono 'complete'?
    df[!complete.cases(df), ] 
    (nrow(df[(!complete.cases(df)),])) / (nrow(df)) *100  #in percentuale
    
    #sono il 7,9%, decido di eliminarli (anche se potevo sostiturli) --> con la media avrei distorto troppo i risultati, applicare ML non era lo scopo attuale
    
#DISTRIBUZIONE  MISSING VALUES 2  
    
  #Visualizzazione con 'VISDATA'  - NON INTERESSANTE
    #vis_dat(df,warn_large_data=FALSE)
  
  #Visualizzazione con 'AMELIIA'  - LENTA     
    #missmap(df, main="MissingMap", legend=TRUE, col=c("firebrick2", "peachpuff"), mar = c(10, 10), rank.order=TRUE)    
    #AmeliaView()
    
    
  #Visualizzazione con 'NANIAR'  - INTERESSANTE
    ################################<Graf.Relazione>  
    vis_miss(df, sort_miss = TRUE, warn_large_data=FALSE)
    
  #COMBINAZIONE/PATTERN NA  
    ################################<Graf.Relazione>  
    gg_miss_upset(df) #interessante
    #gg_miss_upset(df, nsets = n_var_miss(df), nintersects = NA) 
    
    
  #STRATEGIA: elimino le istanze incompelte
    summary(df) 
    dim(df) 
    
    df <- na.omit(df) #decido di eliminre le istanze non complete
    
    summary(df) 
    dim(df)
 
    
#Noto che ci sono missing values anche per campi che sarebbero dovuti essere obbligatori (qualunque passeggero per esempio avrebbe potuto dare un voto sulla soddisfazione sull'orario del volo)       
#Questo ci indica che gli NA non sono solo di chi non ha usufruito di un servizio, ma in alcuni casi sono proprio dei valori mancanti per qualche altro motivo
#(salta l'idea) Poteva essere interessante studiare gli NA per vedere quante persone usufruivano o meno di un certo servizio e se esistesse una certa correlazione tra questi servizi 'opzionali' 

    
    
 #<<<<<<CONSISTENCE>>>>>> (Noisy Data)
   #Stabilire obiettivi e vincoli
    #VINCOLI
     #Gender ??? {M, F}
     #CustomerType ??? {disloyal Customer, Loyal Customer}
     #Age > 0, Age < 120
     #TypeOfTravel ??? {Business travel, Personal travel}
     #Class ??? {Business, Eco, Eco Plus} 
     #FlightDistance > 0
     #Sat.* ??? {1,2,3,4,5,NA}
     #DepartureDelayMinutes >= 0
     #ArrivalDelayMinutes >= 0
     #Satisfaction ??? {neutral or dissatisfied, satasfied}
    #Prima verifica: con summary(df), andano a vedere max e min (i livelli per i fattori)
        
  #Verifica presenza di valori duplicati (posso verificarli solo per Id)     
    
      #df[df$Id==3, "Id"] = 1 #df[df$Id==4, "Id"] = 5 #df[df$Id==2, "Id"] = 1
      x <- df[,1] #prendo prima colonna 'Id' come vettore
      x[duplicated(x)] #mi restituisce gli Id duplicati
      #Nessun Id duplicato
      

        
 #OUTLAIERS --> hanno senso in questo caso??--> No, li analizzo dopo
      #boxplot(df$ArrivalDelayMinutes, color="red", xlab="Km", ylab="", main="FlightDistance", horizontal=TRUE)
      
      #ggplot(df) + ggtitle("BoxPlot - FlightDistance") +
      #  aes(x = " ", y = FlightDistance) +
      #  geom_boxplot(fill = "#73b5f5") +
      #  theme_classic() + coord_flip()
 
      #ggplot(df) + ggtitle("BoxPlot - ArrivalDelayMinutes") +
      #  aes(x = "", y = ArrivalDelayMinutes) +
      #  geom_boxplot(fill = "#73b5f5") +
      #  theme_classic() + coord_flip()
      
      #ggplot(df) + ggtitle("BoxPlot - DepartureDelayMinutes") +
      #  aes(x = "", y = DepartureDelayMinutes) +
      #  geom_boxplot(fill = "#73b5f5") +
      #  theme_classic() + coord_flip()
      
      
#2-EXPLORATORY ANALYSIS ______________________________________________________________________________________________________________________________________________
        
      #OBIETTIVI:
      # A) CARATTERISTICHE DEI PASSEGGERI
         
      # B) SODDISFAZIONE DEL CLIENTE 
      
      # C) VALUTAZIONE DEI SERVIZI 
      
 
#A) CARATTERISTICHE DEI PASSEGGERI (Graf.A) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        
      #GENDER
        ################################<Graf.A1 Relazione>  
        ggplot(df) + 
          geom_bar(mapping = aes(x = Gender), fill = c("#FFBCFB", "#2ACBFF")) + 
          theme_get() + 
          ggtitle("Number of passengers by gender") + 
          labs(y="Number of passengers")
        #Numero passeggeri per genere
        table(df$Gender)
  
      #AGE
        mean(df$Age)
        max(df$Age)
        min(df$Age)
        getmode(df$Age)#39 anni
        nrow(df[df$Age==39,]) #3487
        
        
        max(df$Age[df$Gender=="Male"]) #85
        max(df$Age[df$Gender=="Female"]) #85
        min(df$Age[df$Gender=="Male"]) #7
        min(df$Age[df$Gender=="Female"]) #7
        
        #Cacolo la media per genere
        mu <- ddply(df, "Gender", summarise, grp.mean=mean(Age))
      
        #Distribuzione Age totale
         #ggplot(df, aes(x = Age)) + geom_area( stat = "bin", bins = 40, color = "black", fill = "#00AFBB")+ theme_get() + ggtitle("Age distribution") + labs(y="Number of passengers")
        
        #Distribuzione Age per Gender [ggplot]
         #ggplot(df, aes(x=Age, fill=Gender)) + geom_area(bins = 40, stat ="bin")  + facet_wrap(~Gender) #affiancate
        ################################<Graf.A2 Relazione>  
         ggplot(df, aes(Age, fill = Gender)) +  
           geom_density(bins = 40, stat = "bin", alpha = 0.6)+ 
           ggtitle("Age distribution") + 
           labs(y="Number of passengers")+ 
           geom_vline(data=mu, aes(xintercept=grp.mean, color=Gender), linetype="dashed", size=1) #sovrapposte
        #Conferma conclusioni del grafico precedente
         #ggplot(df) + ggtitle("BoxPlot - Age distribution") + aes(x = Gender, y = Age) + geom_boxplot(fill = "#73b5f5") + theme_classic() + coord_flip()
        #Distribuzione Age per Gender [bwplot]
         #bwplot(df$Age ~ df$Gender,  data = df, panel = panel.violin, xlab = "Gender", ylab = "Age", main="Age distribution")
        
        #Controllo normalità
         #ggqqplot(df$Age) 
         #qqplot(df$Age)
        

#B) SODDISFAZIONE DEL CLIENTE  (Graf.B) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         
#Satisfaction
  ################################<Graf.BB1 Relazione>
  ggplot(df,  aes(Satisfaction, fill=Satisfaction)) + 
              geom_bar(width = 0.8, fill = c("#003f5c", "#bc5090")) +
              theme_get() + ggtitle("No. of satisfied passengers") + 
              labs(y="No. of passengers") +
              theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) + 
              geom_text(aes(label=paste0((format(round((after_stat(prop*100) ), 2), nsmall = 2)),"%"), group=1),
                        stat='count', colour = 'white',  size=8, vjust = +1.5)



#Distribuzione TotalScore
  ################################<Graf.BB2 Relazione>
  ggplot(df, aes(x = TotalScore)) +
       geom_area( stat = "bin", bins = 10, color = "#003f5c", fill = "#003f5c") +
       theme_get() +
       ggtitle("TotalScore distribution") + labs(y="Number of passengers") +
       geom_vline(xintercept = 35, color = "#bc5090", size=1) +
       theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) 
    
  #TotalScore>35
  nrow(df[df$TotalScore>=35,]) #105469 - 88%
  nrow(df) #119567 - 100%


#Satisfaction X Gender
  ################################<Graf.BB3 Relazione>
  ggplot(df,  aes(x=Gender, fill=Satisfaction)) +
            geom_bar(position = position_dodge(0.9)) + 
            theme_get() + ggtitle("Satisfaction X Gender") +
            labs(y="No. of passengers") +
            theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
            scale_fill_manual(values=c("#003f5c", "#bc5090"))


#Satisfaction X Type Of Travel
  ################################<Graf.B4 Relazione>
  ggplot(df,  aes(x=TypeOfTravel, fill=Satisfaction)) +
    geom_bar(position = position_dodge(0.9)) + 
    theme_get() + ggtitle("Satisfaction X TypeOfTravel") +
    labs(y="No. of passengers") +
    theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
    scale_fill_manual(values=c("#003f5c", "#bc5090"))


  nrow(df[df$TypeOfTravel=="Business travel" & df$Satisfaction=="satisfied",]) #47940
  nrow(df[df$TypeOfTravel=="Business travel",]) #82676

  nrow(df[df$TypeOfTravel=="Personal Travel" & df$Satisfaction=="satisfied",]) #3090
  nrow(df[df$TypeOfTravel=="Personal Travel",]) #36891


#Satisfaction X CustomerType
  ################################<Graf.B5 Relazione>
  ggplot(df,  aes(x=CustomerType, fill=Satisfaction)) +
    geom_bar(position = position_dodge(0.9)) + 
    theme_get() + 
    ggtitle("Satisfaction X CustomerType") +
    labs(y="No. of passengers") +
    theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
    scale_fill_manual(values=c("#003f5c", "#bc5090"))

  nrow(df[df$CustomerType=="disloyal Customer" & df$Satisfaction=="satisfied",]) #3505
  nrow(df[df$CustomerType=="disloyal Customer",]) #19234

  #3505*100/19234 18%

#Satisfaction X Age
  ################################<Graf.B6 Relazione>
  ggplot(df,  aes(x=Age, fill=Satisfaction)) +
    geom_bar(position = position_dodge(1)) + 
   theme_get() + ggtitle("Satisfaction X Age") +
   labs(y="No. of passengers") +
   theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
   scale_fill_manual(values=c("#003f5c", "#bc5090")) 


  ggplot(df, aes(Age, fill = Satisfaction)) +  
    geom_density(bins = 20, stat = "bin", alpha = 0.6)  + 
    ggtitle("Age distribution") + labs(y="Number of passengers")  + 
    geom_vline(data=mu, aes(xintercept=grp.mean, color=Gender), linetype="dashed", size=1) #sovrapposte



#Satisfaction X FlightDistance
  #ggplot(df,  aes(x=FlightDistance, fill=Satisfaction)) +
  #  geom_bar(position = position_dodge(1)) + 
  #  theme_get() + ggtitle("Satisfaction X FlightDistance") +
  #  labs(y="No. of passengers") +
  #  theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
  #  scale_fill_manual(values=c("#003f5c", "#bc5090")) 

  ################################<Graf.B7 Relazione>
  ggplot(df, aes(FlightDistance, fill = Satisfaction)) +  
    geom_density(bins = 80, stat = "bin", alpha = 0.8)+ 
    ggtitle("Satisfaction X FlightDistance") + 
    labs(y="Number of passengers")+
    scale_fill_manual(values=c("#003f5c", "#bc5090"))  #sovrapposte



#Satisfaction X Class
  ################################<Graf.B8 Relazione>
  ggplot(df,  aes(x=Satisfaction, fill=Satisfaction)) + 
    facet_grid(Class ~ .) +
    geom_bar(position = position_dodge(1)) + 
    theme_get() + ggtitle("Satisfaction X Class") +
    labs(y="No. of passengers") +
    theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
    scale_fill_manual(values=c("#003f5c", "#bc5090")) 


#Satisfaction X Arrival/Departure delay miuntes
  #ggplot(df, aes(DepartureDelayMinutes, fill = Satisfaction)) +  geom_density(bins = 80, stat = "bin", alpha = 0.8)+ ggtitle("Satisfaction X DepartureDelayMinutes") + labs(y="Number of passengers")+scale_fill_manual(values=c("#003f5c", "#bc5090"))  #sovrapposte
  #ggplot(df, aes(ArrivalDelayMinutes, fill = Satisfaction)) +  geom_density(bins = 80, stat = "bin", alpha = 0.8)+ ggtitle("Satisfaction X ArrivalDelayMinutes") + labs(y="Number of passengers")+scale_fill_manual(values=c("#003f5c", "#bc5090"))  #sovrapposte

  #bwplot(df$ArrivalDelayMinutes ??? df$Satisfaction,  data = df, panel = panel.violin, xlab = "Gender", ylab = "Age", main="Age distribution")
  #boxplot(df$ArrivalDelayMinutes ??? df$Satisfaction, horizontal=TRUE, ylab="_", xlab="_", las=1, main="_")

  #ggplot(df,  aes(x=ArrivalDelayMinutes, fill=Satisfaction)) +
  #  geom_bar(position = position_dodge(1)) + 
  #  theme_get() + ggtitle(" Satisfaction X ArrivalDlayMinutes") +
  #  labs(y="No. of passengers") +
  #  theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
  #  scale_fill_manual(values=c("#003f5c", "#bc5090")) 

  #ggplot(df, aes(x=TotalScore, y=ArrivalDelayMinutes, color=Satisfaction)) + geom_point()
  #ggplot(df, aes(x=TotalScore, y=DepartureDelayMinutes, color=Satisfaction)) + geom_point()

  #ggplot(df, aes(x=Satisfaction, y=ArrivalDelayMinutes, color=Satisfaction)) + geom_point() +
  #  theme_get() + ggtitle("Satisfaction X ArrivalDelayMinutes") +
  #  labs(y="Delay minutes") +
  #  theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
  #  scale_color_manual(values=c("#003f5c", "#bc5090")) 

  #ggplot(df, aes(x=Satisfaction, y=DepartureDelayMinutes, color=Satisfaction)) + geom_point() +
  #  theme_get() + ggtitle("Satisfaction X DepartureDelayMinutes") +
  #  labs(y="Delay minutess") +
  #  theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
  #  scale_color_manual(values=c("#003f5c", "#bc5090")) 

  ################################<Graf.B9 Relazione>
  ggplot(df, aes(x=ArrivalDelayMinutes, y=DepartureDelayMinutes, color=Satisfaction)) + geom_point() +
    theme_get() + ggtitle("ArrivalDelayMinutes X DepartureDelayMinutes | Satisfaction") +
    labs(y="DepartureDelayMinutes", x="ArrivalDelayMinutes") +
    theme(text = element_text(size=17,colour="#614949"), axis.text = element_text(size=16, colour="#614949")) +
   scale_color_manual(values=c("#003f5c", "#bc5090")) 


#TotalScore e Satisfaction
  ################################<Graf.B10 Relazione>
  ggplot(df, aes(x=Id, y=TotalScore, color=Satisfaction)) + geom_point() +
   ggtitle("Satisfaction X TotalScore")+
   scale_color_manual(values=c("#003f5c", "#bc5090")) 

  #ci sono outliers????? --> Studio con BoxPlot
  ################################<Graf.BB11 Relazione>
  ggplot(df, aes(x=Satisfaction, y=TotalScore, fill=Satisfaction)) + 
   geom_boxplot() +
   stat_summary(fun=mean, geom="point", shape=23, size=4) +
    scale_fill_manual(values=c("#003f5c", "#bc5090"))+
    ggtitle("Satisfaction X TotalScore (outliers)")

  #Media Total Score per SOddisfatti e Non Soddisfatti
  mean(df[df$Satisfaction=="satisfied","TotalScore"])
  mean(df[df$Satisfaction=="neutral or dissatisfied","TotalScore"])




#C) VALUTAZIONE DEI SERVIZI (Graf.C) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#Media dei servizi______________________________________________
  
  #Creo un dataframe con la media di ogni feauter del tipo Sat.*
  value=c(round(mean(as.numeric(as.character(df$Sat.BaggageHandling))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.CheckinService))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.Cleanliness))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.InflightWifiService))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.EaseOnlineBooking))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.FlightSchedule))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.FoodDrink))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.GateLocation))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.InflightEntertainment))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.InflightService))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.Legroom))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.OnBoardService))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.OnlineBoarding))), digits=2),
          round(mean(as.numeric(as.character(df$Sat.SeatComfort))), digits=2))

  name=c("BaggageHandling","CheckinService","Cleanliness","InflightWifiService","EaseOnlineBooking","FlightSchedule","FoodDrink",
         "GateLocation","InflightEntertainment","InflightService","Legroom","OnBoardService","OnlineBoarding","SeatComfort")

  data <- data.frame(
    name,  
   value
  )


  #Plotto il nuovo dataframe 'data' (in ordine per media)
  ################################<Graf.C1 Relazione>
  ggplot(data, aes(x=reorder(name, -value), y=value, fill=value)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle("Average rating of services") +
    labs(y="Average", x = "Services") +
    theme(title=element_text(size=14), axis.text=element_text(size=12), axis.title=element_text(size=13,face="bold")) +
    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=+1.25, color='white', size=6)+
    scale_fill_gradient(low = "#FF4D26",  high = "#61E341") 
   #+   geom_hline(yintercept = mean(data$value),color = "blue", size=0.7)


  #Media dei servizi 
  mean(data$value) #3.28



#Matrice di correlazione______________________________________________

  d = df

  d$Sat.OnBoardService <- as.numeric(as.character(d$Sat.OnBoardService))
  d$Sat.OnlineBoarding <- as.numeric(as.character(d$Sat.OnlineBoarding))
  d$Sat.BaggageHandling <- as.numeric(as.character(d$Sat.BaggageHandling))
  d$Sat.CheckinService <- as.numeric(as.character(d$Sat.CheckinService))
  d$Sat.InflightWifiService <- as.numeric(as.character(d$Sat.InflightWifiService))
  d$Sat.EaseOnlineBooking <- as.numeric(as.character(d$Sat.EaseOnlineBooking))
  d$Sat.FoodDrink <- as.numeric(as.character(d$Sat.FoodDrink))
  d$Sat.SeatComfort <- as.numeric(as.character(d$Sat.SeatComfort))
  d$Sat.Cleanliness <- as.numeric(as.character(d$Sat.Cleanliness))
  d$Sat.Legroom <- as.numeric(as.character(d$Sat.Legroom))
  d$Sat.GateLocation <- as.numeric(as.character(d$Sat.GateLocation))
  d$Sat.InflightService <- as.numeric(as.character(d$Sat.InflightService))
  d$Sat.InflightEntertainment <- as.numeric(as.character(d$Sat.InflightEntertainment))
  d$Sat.FlightSchedule <- as.numeric(as.character(d$Sat.FlightSchedule))

  #ccd e cc dataframe con solo le colonne tra parentesi selezionate
  cc=select(d, Sat.OnBoardService, Sat.OnlineBoarding, Sat.Cleanliness,Sat.BaggageHandling,Sat.CheckinService,
          Sat.InflightWifiService,Sat.EaseOnlineBooking, Sat.FoodDrink,Sat.SeatComfort,Sat.Legroom,Sat.GateLocation,
          Sat.InflightService,Sat.InflightEntertainment, Sat.FlightSchedule, FlightDistance, Age, DepartureDelayMinutes, 
          ArrivalDelayMinutes)
  ccd=select(d,Class,Gender, CustomerType, TypeOfTravel, Age, Sat.OnBoardService, Sat.OnlineBoarding, Sat.Cleanliness,
           Sat.BaggageHandling,Sat.CheckinService,Sat.InflightWifiService,Sat.EaseOnlineBooking, Sat.FoodDrink,
           Sat.SeatComfort,Sat.Legroom,Sat.GateLocation, Sat.InflightService,Sat.InflightEntertainment, Sat.FlightSchedule,
           FlightDistance, Age, DepartureDelayMinutes, ArrivalDelayMinutes)

  cor(cc)


  #Visualizzazione 1
   #corrplot.mixed(corr=cor(cc),upper="ellipse", tl.pos="lt",
   #               lower.col = colorpanel(50, "red", "gray60", "blue4"),
   #               upper.col = colorpanel(50, "red", "gray60", "blue4")) 


  #Visualizzazione 2
  #corrplot(cor(cc), type="upper", order="hclust",
  #         col=brewer.pal(n=8, name="RdYlBu"))
  

  #Visualizzazione 3
   ################################<Graf.C2 Relazione>
   ggcorr(cc, nbreaks=8, label=TRUE, label_size=5, label_color='white', 
          label_round = 2, hjust = 0.9, size = 5, color = "#2F1919",layout.exp = 5, palette="BrBG")+
          ggtitle("Correlation Matrix")

  #Visualizzazione 4
  #scatterplotMatrix(cc, diagonal="histogram")




#ESEMPIO GRAFICO DI VARAIBILI CORRELATE_________________________________________

  ################################<Graf.C3 Relazione>
  ggplot(cc, aes(x=Sat.Cleanliness, y=Sat.InflightEntertainment)) +
   geom_jitter(width = 0.5, height = 0.5, shape=1, size=1, colour = "#35978F")+ 
   ggtitle("Correlation Cleanliness - InflightEntertainment") +
   geom_smooth(method='lm', color = "#F7B125", size=1)

  #plot(jitter(cc$Sat.Cleanliness, factor=4, amount=0),jitter(cc$Sat.InflightEntertainment, factor=4, amount=0), pch = 0.4, cex=1,col=rgb(red=(53/255), green=(151/255), blue=(143/255), alpha=0.2),
  #     xlab="Sat.Cleanliness",
  #     ylab="Sat.InflightEntertainment",
  #     main="Correlation between Cleanliness and InflightEntertainment") +
  #abline(lm(cc$Sat.InflightEntertainment ~ cc$Sat.Cleanliness), col = "#F7B125", lwd = 2, lty=1)




#ANALISI DEGLI ATTRIBUTI PEGGIORI
  #INFLIGHT WIFI SERVICES
  #EASE ONLINE BOOKING
  #GATE LOCATION i

#BARPLOT 3 ATTRIBUTI PEGGIORI_________________________________________________
  
  mean=mean(as.numeric(df$Sat.InflightWifiService))

  a=ggplot(df, aes(x=Sat.InflightWifiService, fill=Sat.InflightWifiService)) +
   geom_bar(stat = "count", show.legend = FALSE) +
   ggtitle("InflightWifiService")+
    scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F","#F5AF25","#35978F")) +
    labs(x = "Sat.InflightWifiService", y = "No. of passengers") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18)) +
    geom_vline(xintercept = mean, color="#2FF329",size=2)+
    geom_text(aes(y=20000,label="MEAN",x=mean), colour="#2FF329", size=6, angle=90, vjust = 1.2)

  mean=mean(as.numeric(df$Sat.EaseOnlineBooking))             
  b=ggplot(df, aes(x=Sat.EaseOnlineBooking, fill=Sat.EaseOnlineBooking)) +
   geom_bar(stat = "count", show.legend = FALSE) +
    ggtitle("EaseOnlineBooking")+
    scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F","#F5AF25","#35978F")) +
    labs(x = "Sat.EaseOnlineBooking", y = "No. of passengers") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18)) +
    geom_vline(xintercept = mean, color="#2FF329",size=2)+
    geom_text(aes(y=20000,label="MEAN",x=mean), colour="#2FF329", size=6, angle=90, vjust = 1.2)

  mean=mean(as.numeric(df$Sat.GateLocation))
  c=ggplot(df, aes(x=Sat.GateLocation, fill=Sat.GateLocation)) +
   geom_bar(stat = "count", show.legend = FALSE) +
    ggtitle("GateLocation")+
    scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F","#F5AF25","#35978F")) +
    labs(x = "Sat.GateLocation", y = "No. of passengers") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18)) +
    geom_vline(xintercept = mean, color="#2FF329",size=2)+
    geom_text(aes(y=20000,label="MEAN",x=mean), colour="#2FF329", size=6, angle=90, vjust = 1.2)

  ################################<Graf.C4 Relazione>
  ggarrange(a, b,c,
            labels = c("A", "B", "C"),
             ncol = 3, nrow = 1)

  
#DISTRIBUZIONE InflightWifiService e Sat.EaseOnlineBooking IN BASE A AGE_________________________________________________
  

  #Sat.InflightWifiService - Age
  a=ggplot(df, aes(x=Sat.InflightWifiService, y=Age, fill=Sat.InflightWifiService)) + 
    geom_violin(show.legend = FALSE) +
    ggtitle("Sat.InflightWifiService ~ Age") +
    labs(x = "Sat.InflightWifiService", y = "Age")+ 
    scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F","#F5AF25","#35978F"))+
    geom_boxplot(width=0.1, color="#E6E6E6",show.legend = FALSE) + theme_minimal()
  
  
  #Sat.GateLocation - Age
  #ggplot(df, aes(x=Sat.GateLocation, y=Age, fill=Sat.GateLocation)) + 
   # geom_violin() +
    #ggtitle("Sat.GateLocation ~ Age") +
    #labs(x = "Sat.GateLocation", y = "Age")+ 
    #scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F","#F5AF25","#35978F"))+
    #geom_boxplot(width=0.1, color="#E6E6E6") + theme_minimal()
  
  #Sat.EaseOnlineBooking - Age
  b=ggplot(df, aes(x=Sat.EaseOnlineBooking, y=Age, fill=Sat.EaseOnlineBooking)) + 
    geom_violin(show.legend = FALSE) +
    ggtitle("Sat.EaseOnlineBooking ~ Age") +
    labs(x = "Sat.EaseOnlineBooking", y = "Age")+ 
    scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F","#F5AF25","#35978F"))+
    geom_boxplot(width=0.1, color="#E6E6E6",show.legend = FALSE) + theme_minimal()
  
  ################################<Graf.C5 Relazione>
  ggarrange(a, b,
            labels = c("A", "B"),
            ncol = 2, nrow = 1)



#DISTRIBUZIONE GateLocation IN BASE A AGEGROUP_________________________________________________

  #Divido passeggeri per Age 0-25 26-50 51-75 76-100 (creando colonna temporanea-->non soluzione migliore)
  df$AgeGroup[df$Age >0 & df$Age <=33 ] = "1-33"
  df$AgeGroup[df$Age >33 & df$Age <=66]  = "34-66"
  df$AgeGroup[df$Age >66 & df$Age <=100 ] = "67-100"
  
  ################################<Graf.C6 Relazione>
  ggplot(df,  aes(x=Sat.GateLocation, fill=AgeGroup)) +
    geom_bar(position = position_dodge(0.9),show.legend = FALSE) + 
    theme_get() + ggtitle("Sat.GateLocation - AgeGroup") +
    labs(y="Sat.GateLocation") +
    theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949"))+
    facet_wrap(~ AgeGroup)+ 
    scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F"))


  
  

#ATTRIBUTI NEGATIVI X TypeOfTravel_______________________________________________________________

  #b=ggplot(df,  aes( x=Sat.EaseOnlineBooking, fill=Sat.EaseOnlineBooking)) +
  #  geom_bar(position = position_dodge(0.9), show.legend = FALSE) + 
  #  theme_get() + ggtitle("Sat.EaseOnlineBooking X TypeOfTravel") +
  #  labs(y="EaseOnlineBooking") +
  #  theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
  #  facet_wrap(~TypeOfTravel)+
  #  scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F", "#F5AF25","#35978F"))+ 
  #  geom_vline(data = df, aes(xintercept = mean),size=1, linetype = "dashed", colour = "#2FF329")
  
  #a=ggplot(df,  aes( x=Sat.InflightWifiService, fill=Sat.InflightWifiService)) +
  #  geom_bar(position = position_dodge(0.9), show.legend = FALSE) + 
  #  theme_get() + ggtitle("Sat.InflightWifiService X TypeOfTravel") +
  #  labs(y="InflightWifiService") +
  #  theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
  #  facet_wrap(~TypeOfTravel)+
  #  scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F", "#F5AF25","#35978F"))+ 
  #  geom_vline(data = df, aes(xintercept = mean),size=1, linetype = "dashed", colour = "#2FF329")
  
  #c=ggplot(df,  aes( x=Sat.GateLocation, fill=Sat.GateLocation)) +
  # geom_bar(position = position_dodge(0.9), show.legend = FALSE) + 
  #  theme_get() + ggtitle("Sat.GateLocation X TypeOfTravel") +
  #  labs(y="GateLocation") +
  #  theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
  #  facet_wrap(~TypeOfTravel)+
  #  scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F", "#F5AF25","#35978F"))+ 
  #  geom_vline(data = df, aes(xintercept = mean),size=1, linetype = "dashed", colour = "#2FF329")
  
  #ggarrange(a, b,c,
  #          labels = c("A", "B","C"),#
  #          ncol = 1, nrow = 3)




#ATTRIBUTI NEGATIVI X CustomerType_______________________________________________________________

  #b=ggplot(df,  aes( x=Sat.EaseOnlineBooking, fill=Sat.EaseOnlineBooking)) +
  #  geom_bar(position = position_dodge(0.9), show.legend = FALSE) + 
  #  theme_get() + ggtitle("Sat.EaseOnlineBooking X CustomerType") +
  #  labs(y="EaseOnlineBooking") +
  #  theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
  #  facet_wrap(~CustomerType)+
  #  scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F", "#F5AF25","#35978F"))+ 
  #  geom_vline(data = df, aes(xintercept = mean),size=1, linetype = "dashed", colour = "#2FF329")#
  
  #a=ggplot(df,  aes( x=Sat.InflightWifiService, fill=Sat.InflightWifiService)) +
   # geom_bar(position = position_dodge(0.9), show.legend = FALSE) + 
    #theme_get() + ggtitle("Sat.InflightWifiService X CustomerType") +
    #labs(y="InflightWifiService") +
    #theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
    #facet_wrap(~CustomerType)+
    #scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F", "#F5AF25","#35978F"))+ 
    #geom_vline(data = df, aes(xintercept = mean),size=1, linetype = "dashed", colour = "#2FF329")
  
  
  #c=ggplot(df,  aes( x=Sat.GateLocation, fill=Sat.GateLocation)) +
    #geom_bar(position = position_dodge(0.9), show.legend = FALSE) + 
    #theme_get() + ggtitle("Sat.GateLocation X CustomerType") +
    #labs(y="GateLocation") +
    #theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
    #facet_wrap(~CustomerType)+
    #scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F", "#F5AF25","#35978F"))+ 
    #geom_vline(data = df, aes(xintercept = mean),size=1, linetype = "dashed", colour = "#2FF329")
  
  #ggarrange(a, b,c,
  #          labels = c("A", "B","C"),
  #          ncol = 1, nrow = 3)
  
  
  
  
  #ggplot(df,  aes( x=as.numeric(Sat.GateLocation)))+
    #geom_boxplot(width=0.6) + theme_minimal() +
   # theme_get() + ggtitle("Satisfaction X EaseOnlineBooking") +
    #labs(y="EaseOnlineBooking") +
    #theme(text = element_text(size=14,colour="#614949"), axis.text = element_text(size=12, colour="#614949")) +
    #facet_wrap(~AgeGroup, ncol=1)




#DISTRIBUZIONE EaseOnlineBooking IN BASE A FlightDistance_________________________________________________
  
  ################################<Graf.C7 Relazione>
  ggplot(df, aes(x=Sat.EaseOnlineBooking, y=FlightDistance, fill=Sat.EaseOnlineBooking)) + 
    geom_violin(show.legend = FALSE) +
    ggtitle("Sat.EaseOnlineBooking ~ FlightDistance") +
    labs(x = "Sat.EaseOnlineBooking", y = "FlightDistance")+ 
    scale_fill_manual(values = c("#35978F", "#F5AF25","#35978F","#F5AF25","#35978F"))+
    geom_boxplot(width=0.1, color="#E6E6E6", show.legend = FALSE) + theme_minimal()
  
  

#3-MACHINE LEARNING[1] - DECISION TREE______________________________________________________________________________________________________________________________________________
  
#ML
library(tree)
library(ctree) #non va
library(rpart)
#View
library(rattle)
library(rpart.plot)
library(RColorBrewer)
#ConfusionMatrix  
library(e1071)
library(caret)
#ROC curve
library(ROCR)

#Data preparing  
  dfML = df[2:(ncol(df)-1)] #escludo Id (non utile alla classificazione) e TotalScore (ridondante per la classificazione) -->Departure Arrival li escludo??
  
  #Trasforma fattori che Sat.* in numderi (solo per migliore visualizzazione con rattle)
  dfML$Sat.OnBoardService <- as.numeric(as.character(dfML$Sat.OnBoardService))
  dfML$Sat.OnlineBoarding <- as.numeric(as.character(dfML$Sat.OnlineBoarding))
  dfML$Sat.BaggageHandling <- as.numeric(as.character(dfML$Sat.BaggageHandling))
  dfML$Sat.CheckinService <- as.numeric(as.character(dfML$Sat.CheckinService))
  dfML$Sat.InflightWifiService <- as.numeric(as.character(dfML$Sat.InflightWifiService))
  dfML$Sat.EaseOnlineBooking <- as.numeric(as.character(dfML$Sat.EaseOnlineBooking))
  dfML$Sat.FoodDrink <- as.numeric(as.character(dfML$Sat.FoodDrink))
  dfML$Sat.SeatComfort <- as.numeric(as.character(dfML$Sat.SeatComfort))
  dfML$Sat.Cleanliness <- as.numeric(as.character(dfML$Sat.Cleanliness))
  dfML$Sat.Legroom <- as.numeric(as.character(dfML$Sat.Legroom))
  dfML$Sat.GateLocation <- as.numeric(as.character(dfML$Sat.GateLocation))
  dfML$Sat.InflightService <- as.numeric(as.character(dfML$Sat.InflightService))
  dfML$Sat.InflightEntertainment <- as.numeric(as.character(dfML$Sat.InflightEntertainment))
  dfML$Sat.FlightSchedule <- as.numeric(as.character(dfML$Sat.FlightSchedule)) 



#Data Partition
  split.data = function(data, p = 0.7, s = 1){
    set.seed(s)
    index = sample(1:dim(data)[1])
    train = data[index[1:floor(dim(data)[1] * p)], ]
    test = data[index[((ceiling(dim(data)[1] * p))+1):dim(data)[1]], ]
    return(list(train=train, test=test))
  }
  
  allset = split.data(dfML) #0.7 train, 0.3 test 
  trainset = allset$train
  testset = allset$test  
  
  #L'attributo Satisfaction è già ben distribuito?
  #Si

#Machine Learning Algorithms
  
  #MODELLO 1 - CREO 'MANUALMENTE' UN MODELLO DI BASE E STIMO LE PERFORMANCE
    #Modello di Base [0-rules] --> predirre valore classe che ha più osservazioni -->'neutral or dissatisfied'
    testset$Prediction = rep("neutral or dissatisfied", nrow(testset))
    testset$Prediction = factor(testset$Prediction)
    
    #Quanto è buono il nostro modello di base? - ACCURACY
    confusion.matrix = table(testset$Satisfaction, testset$Prediction)
    sum(diag(confusion.matrix))/sum(confusion.matrix) #0.57 - Estimate accuracy
    
  #MODELLO 2 - DECISION TREE
#___#1-CREAZIONE  
    #Passo tutte le caratteristiche significative (ho già escluso Id(non significativo) e TotalScore(ridondante))
    decisionTree = rpart(Satisfaction ~ ., data=trainset, method="class") #class --> Satisfaction è categorica 
    #Di default rpart utilizza l'indice di Gini 
    
    #Primo plot semplice
      #plot(decisionTree)
      #text(decisionTree)
    
    #Stampo testualmente l'albero
      print(decisionTree) #num istanze #Gini    #Categiria più frequente
      summary(decisionTree) #INTERESSANTE
    #Stampo con rattle 
      ################################<Graf.D1 Relazione>  
      fancyRpartPlot(decisionTree, main="", sub="", palettes="BuGn", type=1,yesno=1) #The default is of cp is 0.01
     #Altra visualizzazione  
      #rpart.plot(decisionTree, main="", sub="", type=1,cex=1,trace = 1,yesno=1) 
     #Altra visualizzazione 
      #multi.class.model <- rpart(Satisfaction ~ ., data = dfML)
      #rpart.plot( multi.class.model) 
    
#___#2-VARIABILI PIU IMPORTANTI DEL MODELLO
      summary(decisionTree)
      
      #Creo un dataframe con le variabili più importanti e lo plotto
      name=c("Sat.OnlineBoarding","Sat.InflightWifiService", "Sat.SeatComfort","Sat.EaseOnlineBooking",
             "TypeOfTravel", "Class", "Sat.InflightEntertainment","Age")
      value=c(28,18,13,10,10,10,9,1)
      
      data <- data.frame(
        name,  
        value
      )
      
      ################################<Graf.D2 Relazione>  
      ggplot(data,  aes(x=value,y=name,fill=value)) + 
        geom_bar(stat="identity", width = 0.8) +
        theme_get() + ggtitle("Features importance") + 
        labs(y="", x="") +
        theme(text = element_text(size=14,colour="#614949"), 
              axis.text = element_text(size=12, colour="#614949"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        scale_fill_gradient(low = "#FF4D26",  high = "#61E341")+
        geom_text(aes(label=value), position=position_dodge(width=0.9), hjust=1.25, color='black', size=6)
      
          
#___#3-VALUTAZIONE DEL MODELLO
      #Predizione nel test set
        testset$Prediction <- predict(decisionTree, testset, type = "class")  
       
    #Quanto è buono il nostro modello di base? - ACCURACY
        confusion.matrix = table(testset$Satisfaction, testset$Prediction)
        #sum(diag(confusion.matrix))/sum(confusion.matrix) #88%
        
        a=confusionMatrix(confusion.matrix) # generating the confusion matrix     
        a$byClass #Evalueting Stats
        
    #ROC Curve 
        pred <- prediction(predict(decisionTree, type = "prob")[, 2], trainset$Satisfaction)
        
        ################################<Graf.D3 Relazione>
        plot(performance(pred, "tpr", "fpr"),
             main="ROC Curve",
             ylab="Sensitivity",
             xlab="1-Specificity",
             col="Red",
             lwd=2)
        
        abline(a=0, b=1, lty = 3, lwd=2, col="#0027FF")
        
    #AUC Area Under Curve     
        auc = performance(pred, "auc")
        auc = unlist(slot(auc,"y.values"))
        auc=round(auc,3)
        a= paste("AUC: ",auc)
        #Aggiungo il valore di AUC al plot come legend
        legend(0.6, 0.2 , a, cex=1.5,bg='#A10000',box.col="#A10000",text.col="white", xjust = -0.4, yjust= +1.5)
  
#___#4-PRUNING
      #plotto parametro di complessità  
      ################################<Graf.D4 Relazione>  
      plotcp(decisionTree, col="Red", minline=TRUE) # show you the optimal place to prune the tree.
      #min a 0.016 --albero attuale(prendo l'albero che minimizza l'errore)
        
      prunedTree = prune(decisionTree, cp=0.0016)
      fancyRpartPlot(prunedTree, main="", sub="", palettes="BuGn", type=1,yesno=1) 
      #L'albero rimane lo stesso infatti
          
      printcp(prunedTree)
      plotcp(prunedTree)
          
     
      
    
    
    
#3-MACHINE LEARNING[2] - K-NN ______________________________________________________________________________________________________________________________________________
library(class)
library(caret)
library(mlbench)
library(pROC)    

    
data = dfML

#Data Normalization

  #Attributi da Fattori(di Stringhe) a Nuemerici
    data$Gender <- ifelse(data$Gender=="Male", 0, 1)    #0-Male 1-Female  
    data$CustomerType <- ifelse(data$CustomerType=="Loyal Customer",1,0)   #0-disloyal 1-loyal
    data$TypeOfTravel <- ifelse(data$TypeOfTravel=="Personal Travel", 0, 1)    #0-personal 1-business  
    data$Class <- ifelse(data$Class=="Eco",0,ifelse(data$Class=="Eco Plus",1,2)) #Eco-0 Eco-Plus-1 ..
    data$Satisfaction <- ifelse(data$Satisfaction=="neutral or dissatisfied",0,1) #Satisfied-1 ..
    str(data)
  
  #Funzione:  NORMALIZE - normalizzazione (variabili numeriche) 
    normalize = function(x) {
                             return ((x - min(x)) / (max(x) - min(x))) }
  #Normalizzo ogni colonna del dataset  
    data_norm = as.data.frame(lapply(data, normalize))
  

#Data Partition
  allset = split.data(data_norm)   #0.7 train, 0.3 test  --> da funzione
  
  #Colonna attributo da predire (Satisfaction)
  trainset_labels =  (allset$train)$Satisfaction
  testset_labels =  (allset$test)$Satisfaction
  
  tr = allset$train
  te = allset$test
  
  #Escludo la feature da predire
  trainset = tr[,1:22]
  testset = te[,1:22]

  
#Predizione 
  #k=289  
    knn_a = knn(train=trainset, test=testset, cl =  trainset_labels, k=289, prob = TRUE) #k dispari
    a=confusionMatrix( table(knn_a, testset_labels)) # generating the confusion matrix     
    a$byClass 
  
    
  #K=290    
    knn_b = knn(train=trainset, test=testset, cl =  trainset_labels, k=290)
    
    b=confusionMatrix( table(knn_b, testset_labels)) # generating the confusion matrix     
    b$byClass
    #risultati peggiori rispertto a k=289
    
    
#Valutazione 
  a$byClass 
  
  #ROC curve
    prob <- attr(knn_a, "prob")
    
    prob <- 2*ifelse(knn_a == "0", 1-prob, prob) - 1 #sistemo le probabilità (da documentazione) per prediction() --> ROC
    
    pred_knn <- prediction(prob, te[,23]) 
    pred_knn <- performance(pred_knn, "tpr", "fpr") #TPR: true positive rate, FPR: false positive rate
    
  
    ################################<Graf.E1 Relazione>
    plot(pred_knn,
         main="ROC Curve",
         ylab="Sensitivity",
         xlab="1-Specificity",
         col="Red",
         lwd=2,print.auc=T, print.thres = T
         )
    
    abline(a=0, b=1, lty = 3, lwd=2, col="#0027FF") #bisettrice
    
    #AUC Area Under Curve
      pred_knn = prediction(prob, te[,23])
      auc_ROCR = performance(pred_knn, measure = "auc") #Prelevo auc dalle performance
      auc = auc_ROCR@y.values[[1]] #Prelevo il valore di auc dall'oggetto di tipo 'performance'
      auc = round(auc,3) #Arrotondo
      a = paste("AUC: ", auc)
    
    #Aggiungo il valore di AUC al plot come legend
    legend(0.6, 0.2 , a, cex=1.5, bg='#A10000', box.col="#A10000", text.col="white", xjust = -0.45, yjust= +1.5)
    
  
  
#ALTRO METODO (con Cross Validation- troppo lento, si ferma)
#  library(pROC)
#  library(caret)
#  library(mlbench)
  
  
#  data = dfML
  
#  data$Gender <- ifelse(data$Gender=="Male", 0, 1)    #0-Male 1-Female  
#  data$CustomerType <- ifelse(data$CustomerType=="Loyal Customer",1,0)   #0-disloyal 1-loyal
#  data$TypeOfTravel <- ifelse(data$TypeOfTravel=="Personal Travel", 0, 1)    #0-personal 1-business  
#  data$Class <- ifelse(data$Class=="Eco",0,ifelse(data$Class=="Eco Plus",1,2)) #Eco-0 Eco-Plus-1 ..

#  str(data)
  
  #Data Partition
 #   allset = split.data(data)
    
    #0.7 train, 0.3 test 
  #  trainset = allset$train
  # testset = allset$test
  
  #Modello  
#  cv = trainControl(method = "repeatedcv", #repeat cross validation
#                    number = 10, #numero iterazioni
#                    repeats = 3) #ripeti cv 3 volte

#  set.seed(2222)
  
#  knn = train(Satisfaction~.,
#              data = trainset,
#              method = 'knn',
#              tuneLenght=20,
#              trControl = cv,
#              preProc = c("center","scale"))
  
  

