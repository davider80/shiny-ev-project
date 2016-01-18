library(maptools)
library(sp)
library(raster)
library(pxR)
library(XML)
library(ggplot2)

#-------------------------------------------------------------------------------
# Funzioni caricamento dati
#-------------------------------------------------------------------------------

readOldDataYear <- function(year){
        
        #Lista simboli cantoni
        
        
        df <- readHTMLTable(paste0("data/old/pub1_",year,".html"))[[2]]
        
        #Se 2012 aggiungiamo riga confederazione vuota
        if(year == 2012){
                df$CH <- 0 
        }
        
        #Togliamo colonna total
        #df <- df[,c(1,3:29)]
        
        df[,2:29] <- as.numeric(as.matrix(df[,2:29]))
        
        
        if(year == 2005){
                index = 27
                length = 10
        }
        else if(year == 2006){
                index = 27
                length = 11
        }
        else if(year == 2007 | year == 2008){
                index = 27
                length = 12
        }
        else if(year == 2009 | year == 2010 | year == 2011){
                index = 28
                length = 15
        }
        else if(year == 2012){
                index = 28
                length = 16
        }
        
        
        region.names <- c("GE","VS","VD","BE","FR","JU","NE","SO","AG","BL","BS","ZH",
                          "AR","AI","GL","GR","SG","SH","TG","LU","NW","OW","SZ","UR",
                          "ZG","TI","CH")
        
        #Teniamo solo fuel e riallineiamo etichette con valori
        df <- cbind(df[seq(index, length.out=length),1], year, df[c(index-1,seq(index+1, length.out=length-1)),2:ncol(df)])
        
        #Teniamo solo le righe fuel
        colnames(df) <- c("Fuel","Year","Total",region.names)
        
        
        df
        
}

readOldData <- function(){
        df <- rbind(readOldDataYear(2005),
                    readOldDataYear(2006),
                    readOldDataYear(2007),
                    readOldDataYear(2008),
                    readOldDataYear(2009))
        
        df <- droplevels(df)
        
        df$Year <- factor(df$Year)
        
        #Simplify fuel categories
        
        levels(df$Fuel) <- c("Hybrid",
                             "Gasoline",
                             "Diesel",
                             "Electric",
                             "Other",
                             "Hybrid",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "Hybrid",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "Other")
        
        
        # Melts the tidy dataset by subject and activity
        df.melted <- melt(df, id=c("Fuel", "Year"))
        
        # Casts the melted dataset by calculating the sum of each variable
        dcast(df.melted , Fuel + Year ~ variable, sum)      
}

readNewData <- function(){
        
        df <- as.data.frame(read.px("data/px-x-1103020100_103.px"))
        df <- droplevels(df)
        
        #Simplify fuel categories
        
        levels(df$Treibstoff) <- c("Gasoline",
                                   "Diesel",
                                   "Hybrid",
                                   "Hybrid",
                                   "Electric",
                                   "Other",
                                   "Other")
        
        
        
        
        # Casts the melted dataset by calculating the sum of each variable
        df <- dcast(df , Treibstoff + Jahr ~ Kanton, sum) 
        
        df$Total <- rowSums(df[,3:29] )
        
        df <- df[,c(1:2,30,3:29)]
        
        
        
        colnames(df) <- c("Fuel","Year","Total", "ZH","BE","LU","UR","SZ","OW","NW","GL",
                          "ZG","FR","SO","BS","BL","SH","AR","AI","SG","GR","AG","TG","TI","VD","VS","NE","GE","JU","CH")
        
        df
}


#-------------------------------------------------------------------------------
# Caricamento dati auto
#-------------------------------------------------------------------------------

cars <- rbind(readOldData(), readNewData())



gadm <- getData('GADM', country='CHE', level=1)

plate <- c("AG","AR","AI","BL","BS","BE","FR","GE","GL","GR","JU","LU","NE","NW",
           "OW","SG","SH","SZ","SO","TG","TI","UR","VS","VD","ZG","ZH")

jet.colors <- 
        colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", 
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")) 

#Melt e calcoliamo percentuale per categoria di carburante
cars.melted <- melt(cars, id=c("Fuel", "Year"))
cars.melted$Pct <- 100*cars.melted$value/with(cars.melted, ave(value, list(Year, variable), FUN = sum))

#Filtriamo auto elettriche
ev.melted <- cars.melted[cars.melted$Fuel== "Electric",]

shinyServer(function(input, output, session) {
        
        # Combine the selected variables into a new data frame
        ev <- reactive({
                if(input$type == "Plot: total vehicles")
                {
                        ev.melted.lastyear <- ev.melted[ev.melted$Year==max(as.numeric(as.character(ev.melted$Year))),]
                        max.ev.cantons <- as.character(ev.melted.lastyear[order(ev.melted.lastyear$value, decreasing=TRUE)[2:11], 3]) #Togliamo Total
                        
                        # Filtriamo i dati per quei cantoni
                        #ev.melted[ev.melted$variable %in% max.ev.cantons,]
                        
                        ev.melted
                        
                }
                else
                {
                        ev.melted[ev.melted$Year==input$year,]
                }
               
        })
        
        
        output$plotEV <- renderPlot({

                if(input$type == "Map: percentage by cantons")
                {
                        
                       
                        #Ordiniamo e selezioniamo il numero di auto in base alla lista di cantoni della mappa (funzione match)
                        gadm$registered.ev <- ev()[match(plate, ev()$variable),"Pct"]
                        
                        scale <- seq(0, max(gadm$registered.ev)*1.05, by = max(gadm$registered.ev)*1.05/30)
                        cols <- jet.colors(length(scale))
                        
                        spplot(gadm, "registered.ev", col.regions=cols, at=scale, main=paste("Percentage of EV for year", input$year))
                
                }               
                else if(input$type == "Map: total by cantons")
                {
                        
                    
                        gadm$registered.ev <- ev()[match(plate, ev()$variable),"value"]
                        
                        scale <- seq(0, max(gadm$registered.ev)*1.05, by = max(gadm$registered.ev)*1.05/30)
                        cols <- jet.colors(length(scale))
                        
                        spplot(gadm, "registered.ev", col.regions=cols, at=scale, main=paste("Total registered EV for year", input$year))
                }
                else if(input$type == "Plot: total vehicles")
                {
                        ggplot(ev(), aes(x = Year, y = value, color = variable)) +
                                geom_line(aes(group = variable)) +
                                ylab("Total Registered EV") +
                                xlab("Year") +
                                ggtitle("Total registered EV in top 10 cantons")+
                                geom_point()
                }
        })
        
})