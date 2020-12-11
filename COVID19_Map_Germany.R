# In this script we make a nice Covid-Graph

##### Used Packages #####
library(readr)    # Read CSV from URL
library(plyr)     # ddply()
library(ggplot2)  # Nice plots
library(readxl)   # import excel data
library(rgdal)    # readOGR()
library(maptools) # fortify
library(rgeos)    # fortify regions
library(scales)   # 
library(ggpubr)   # ggarrange -> arrange plots in nice way

#### Prepare your folder structure ####
# Create a folder where data will be stored (only needed for GIS-Data).
MyFolder <- "F:/CoronaPlot"
dir.create(MyFolder)
# Then create a folder within this folder with all Plots.
dir.create(paste0(MyFolder, "/Plots"))
# Set the working directory to match the folder we just created.
setwd(MyFolder)

#### Load data ####
# Load the Covid-19 data from RKI (German data)
Corona <- read_csv("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv")
# And change the column names for easier Indexing.
colnames(Corona) <- c("ObjectID", "BundeslandID", "Bundesland", "Landkreis", "Altersgruppe",
                      "Geschlecht", "AnzahlFall", "AnzahlTodesfall", "Meldedatum", "LandkreisID",
                      "Datenstand", "NeuerFall", "NeuerTodesfall", "Refdatum", "NeuGenesen", "AnzahlGenesen",
                      "IstErkrankungsbeginn", "Altersgruppe2")

# Load GIS data.
# You should change your working directory before downloading the zip-file.
download.file("https://opendata.arcgis.com/datasets/b2e6d8854d9744ca88144d30bef06a76_1.zip" , destfile="LandkreisMap.zip")
# Unzip the file with R.
system("unzip LandkreisMap.zip")
# Now load the GIS-data.
LK_GIS <- readOGR(paste0(getwd(), "/Kreisgrenzen_2017_mit_Einwohnerzahl.shp"))

# Load Landkreis-names (and in which Bundesland they are).
LK_Namen <- read_csv("https://raw.githubusercontent.com/JHeinermann/GermanCovidMap/main/LK_Namen.csv", 
                     locale = locale(encoding = "ISO-8859-1"))

# Define colors for the Bundeslaender.
ColorsBund <- list("Schleswig-Holstein" = "#0597e0",
                   "Mecklenburg-Vorpommern" = "#fb800b",
                   "Niedersachsen" = "#4ab78d",
                   "Nordrhein-Westfalen" = "#f91438",
                   "Bremen" = "#ebe157",
                   "Hamburg" = "#19459c",
                   "Berlin" = "#223d9e",
                   "Sachsen-Anhalt" = "#d53c9e",
                   "Hessen" = "#fbad03",
                   "Thüringen" = "#0e4aa2",
                   "Saarland" = "#be3788",
                   "Rheinland-Pfalz" = "#12b3a4",
                   "Baden-Württemberg" = "#ffe208",
                   "Bayern" = "#02b7e6",
                   "Sachsen"= "#fbdf11",
                   "Brandenburg" = "#f00f31")

#### Set the framerate ####
FramesPerDay <- 10

#### Prepare data ####
## Spatial data
# So to make sure to keep data from GIS data (such as LandkreisID and stuff),
# create an ID column in the GIS data set.
LK_GIS@data$id <- rownames(LK_GIS@data)
# Save the GIS data in a data.frame.
LK_GIS_Data <- LK_GIS@data
# Convert GIS-data (large spatial polygon) into data.frame
LK_GIS <- fortify(LK_GIS, by = "id")
# And then merge the previously saved GIS data with the just created spatial data.frame.
LK_GIS <- merge(LK_GIS, LK_GIS_Data[, c("Kennziffer", "id")])  # Kennziffer = LandkreisID

## Covid data
# Turn the LandkreisID into a numeric
Corona$LandkreisID <- as.numeric(Corona$LandkreisID)
# Berlin is devided into different regions for Covid-tracking. We don't need this,
# so set all Berlin IDs to the default Berlin LandkreisID.
Corona$LandkreisID <- ifelse(Corona$BundeslandID == 11, 
                             11000,
                             Corona$LandkreisID)
# Transform date into date format.
Corona$Refdatum <- as.Date(Corona$Refdatum, "%Y/%m/%d")

#### Now we create a data frame with 7-day-incidences ####
# First create a data frame with the summed up numbers per day and Landkreis.
CoronaLK <- ddply(Corona, ~ Refdatum + LandkreisID, summarise, 
                  Faelle = sum(AnzahlFall))
# Now add all missing dates.
CoronaLK <- merge(CoronaLK, expand.grid(LandkreisID = levels(factor(CoronaLK$LandkreisID)), 
                                        Refdatum = levels(factor(CoronaLK$Refdatum))),
                  by = c("LandkreisID", "Refdatum"), all = TRUE)
# Set the cases of all missing dates to 0.
CoronaLK[is.na(CoronaLK$Faelle), ]$Faelle <- 0
# Also include the number of inhabitants per Landkreis. 
# They are included in our GIS-data.
LK_GIS_Data <- LK_GIS_Data[, c("Kennziffer", "EWZ")]
# Change column names for merging.
colnames(LK_GIS_Data) <- c("LandkreisID", "Einwohner")
# Now merge the data.
CoronaLK <- merge(CoronaLK, LK_GIS_Data, by = c("LandkreisID"))
# Now calculate the 7-day-incidence per 100.000 inhabitants.
CoronaLK$proT <- (CoronaLK$Faelle * 100000) / CoronaLK$Einwohner
# Create a dummy data.frame that should later include the LandkreisID, a date and the 7-day-incidence per 100k.
SiebentageLK <- data.frame(Landkreis = 1,
                           Datum = as.Date("2000-01-01"),
                           Inzidenz = 1)
# Delete dummy data, only keeping the frame.
SiebentageLK <- SiebentageLK[0, ]

# Now fill ou the dummy data with a for-loop.
for(i in 1:length(levels(factor(CoronaLK$LandkreisID)))){
  # First save the LandkreisID.
  LandName <- as.character(levels(factor(CoronaLK$LandkreisID))[i])
  # Then create a temporary data.frame with only a chosen Landkreis.
  XLand <- CoronaLK[CoronaLK$LandkreisID == LandName, ]
  # Because there is also dates without data, that are not present in the Covid data.frame,
  # insert this data.
  XLand <- merge(XLand, data.frame(Refdatum = as.Date(as.Date("2020-01-01"):as.Date("2020-12-03"), origin = "1970-01-01")),
                 by = c("Refdatum"), all = TRUE)
  # Sort the data by date
  XLand <- XLand[order(XLand$Refdatum), ]
  # Set the Incidence to NA
  Inz <- NA
  # And iterate through another for-loop, that calculates the number of cases for the 7 previous days.
  for(j in 7:length(XLand$proT)){
    Inz[j] <- sum(XLand$proT[(j - 6):j])
  }
  # Take the dummy data.frame and add the data obtained from this loop.
  SiebentageLK <- rbind(SiebentageLK, data.frame(Landkreis = LandName,
                                                 Datum = XLand$Refdatum,
                                                 Inzidenz = Inz))
}
# Set all NA data to 0 (this is only the first 6 data points of each Landkreis).
SiebentageLK[is.na(SiebentageLK$Inzidenz), ]$Inzidenz <- 0
# No calculate the colors for the Landkreise in our map.
SiebentageLK$col <- ifelse(SiebentageLK$Inzidenz == 0, "#ffffff",
                           ifelse(SiebentageLK$Inzidenz <= 5, "#d7d4b0",
                                  ifelse(SiebentageLK$Inzidenz <= 25, "#d7d289", 
                                         ifelse(SiebentageLK$Inzidenz <= 50, "#d29a0b", 
                                                ifelse(SiebentageLK$Inzidenz <= 100, "#d63523",
                                                       ifelse(SiebentageLK$Inzidenz <= 250, "#931214",
                                                              ifelse(SiebentageLK$Inzidenz <= 500, "#651212","#ea0085")))))))


#### Calculate Deaths ####
# Deaths are summed through all dates.
Deaths <- ddply(Corona, ~ Refdatum, summarise, Cases = sum(AnzahlTodesfall))
# Also include days without deaths. 
Deaths <- merge(Deaths, data.frame(Refdatum = as.Date(as.Date("2020-01-01"):as.Date("2020-12-03"), origin = "1970-01-01")),
                by = c("Refdatum"), all = TRUE)
# Set all NA data 0.
Deaths[is.na(Deaths$Cases), ]$Cases <- 0

# Use Moving Average because it's easier to look at
WindowSize <- 7           # Must be an unequal number
# Create a variable where the moving average is stored.
Deaths$MovingAverage <- NA
# And use a for-loop to calculate the moving average.
for(i in ((WindowSize + 1) / 2):(length(Deaths$Cases) - (WindowSize + 1) / 2)){
  WStart <- i - ((WindowSize - 1) / 2)
  WEnd   <- i + ((WindowSize - 1) / 2)
  Deaths$MovingAverage[i] <- mean(Deaths$Cases[WStart:WEnd])
}


#### Calculate cases per age class ####
# Create data.frame with all age groups and dates.
IncidenceA <- ddply(Corona, ~Altersgruppe + Refdatum, summarise, 
                    Cases = sum(AnzahlFall), .drop = FALSE)
# Set the date as.Date.
IncidenceA$Refdatum <- as.Date(IncidenceA$Refdatum)
# Create a new variable calculating the cases per age class during the previous 7 days
# This could also be a moving average, might alter this in the future.
IncidenceA$SevenDays <- NA

# So set a start and an end value defining the insertion positions in the data.
iStart <- 1
iEnd <- 1
# Then iterate through all age classes.
for(i in 1:length(levels(factor(IncidenceA$Altersgruppe)))){
  # First create a temporaty data.frame with data of only a chosen age class.
  iData <- IncidenceA[IncidenceA$Altersgruppe == as.character(levels(factor(IncidenceA$Altersgruppe)))[i], ]
  # Set the incidence NA by default.
  iIncidence <- NA
  # Set the end of the insertion position the length of the data.
  iEnd <- iStart + nrow(iData) - 1
  # And now iterate through each day to calculate the 7-day sum.
  for(j in 7:nrow(iData)){
    WStart <- j - ((WindowSize - 1) / 2)
    WEnd   <- j + ((WindowSize - 1) / 2)
    iIncidence[j] <- sum(iData$Cases[(j - 6):j])
    iIncidence[j] <- mean(iData$Cases[WStart:WEnd])
  }
  # Insert it to the data.frame.
  IncidenceA$SevenDays[iStart:iEnd] <- iIncidence
  # Set the new start the old end (+1).
  iStart <- iEnd + 1
}

# Reorder factor levels (in our plot, old people should be on the top, you people at the bottom).
IncidenceA$Altersgruppe <- factor(IncidenceA$Altersgruppe, 
                                  levels = sort(levels(factor(IncidenceA$Altersgruppe)), 
                                                decreasing = TRUE))

# So there is something special with this. We want not only the data of this day and maybe the previous 
# one, but also the data of all previous dates. This is why we have to make a data.frame now,
# that includes already the data of all frames. In our plot, each day will be represented by 
# multiply frames to make things smooth. Differences between to dates will be linearly interpolated.
# The start date will be the first date; the laste date will be the final date.
dStart <- as.numeric(as.POSIXct(IncidenceA$Refdatum[1]))
dEnd   <- as.numeric(as.POSIXct(IncidenceA$Refdatum[nrow(IncidenceA)]))
# We also create a new data.frame including rows for this inbetween data.
CasesPlot <- data.frame(Altersgruppe = rep(levels(IncidenceA$Altersgruppe), 
                                           each = FramesPerDay * nrow(IncidenceA) / 7 + 1),
                        Refdatum = rep(
                          as.POSIXct(
                            seq(dStart, dEnd, by = (60 * 60 * 24 / FramesPerDay)), 
                            origin = "1970-01-01 00:00:00"),
                          length(levels(IncidenceA$Altersgruppe))))
# We set all data NA first.
CasesPlot$SevenDays <- NA
CasesPlot$Deaths <- NA
CasesPlot$Altersgruppe <- factor(CasesPlot$Altersgruppe,
                                 levels = sort(levels(factor(CasesPlot$Altersgruppe)), 
                                               decreasing = TRUE))
# We create a new column so that we can easily just choose all data "below" a certain data.
CasesPlot$CRow <- rep(1:length(levels(factor(CasesPlot$Refdatum))), 7)
# Order the data.frame so that the for loop can insert at the right spot.
CasesPlot <- CasesPlot[order(CasesPlot$Altersgruppe, CasesPlot$Refdatum), ]
# Sort the Incidence numbers by age class and then by date.
IncidenceA <- IncidenceA[order(IncidenceA$Altersgruppe, IncidenceA$Refdatum), ]

# We now iterate through all data and add the previously calculated incidences to the newly created data.frame.
iStart <- 1
iEnd <- FramesPerDay
for(i in 1:(nrow(IncidenceA) - 1)){
  # For linear interpolation we use a slope and a intercept parameter of a linear function.
  lmB <- (IncidenceA$SevenDays[i + 1] - IncidenceA$SevenDays[i]) / FramesPerDay
  lmA <- IncidenceA$SevenDays[i]
  # Then we add these values to the new data.frame.
  CasesPlot$SevenDays[iStart:iEnd] <- lmA + lmB * 0:(FramesPerDay - 1)
  
  # We do the same with death data.
  lmBd <- (Deaths$MovingAverage[i + 1] - Deaths$MovingAverage[i]) / FramesPerDay
  lmAd <- Deaths$MovingAverage[i]
  CasesPlot$Deaths[iStart:iEnd] <- lmAd + lmBd * 0:(FramesPerDay - 1)
  iStart <- iEnd + 1
  iEnd <- iStart + FramesPerDay - 1
}

# We then create a column storing the total amount of infections during the last 7 days.
CasesPlot <- ddply(CasesPlot, ~ Refdatum, transform, SMax = sum(SevenDays, na.rm = T))
# We now set all NAs to 0
CasesPlot[is.na(CasesPlot$SevenDays), ]$SevenDays <- 0
# We order everything by age class and date.
CasesPlot <- CasesPlot[order(CasesPlot$Altersgruppe, CasesPlot$Refdatum), ]



##### Now the for-Loop to create a video #####
# First find out the last date of our data set so we know the limits of our plotting.
max(as.numeric(format(Corona$Refdatum, "%j")))
# Create a variable, numbering the png output.
npic <- 1
# Set the First and Last day for plotting. 1 = 2020-01-01
FirstDay <- 1
LastDay <- 340
# For the second plot, we have a make another variable because we have data inbetween single dates (eg. 2020-01-01 03:36:00)
CurrentRow <- FirstDay * FramesPerDay

# Now we make a for-loop from the First to the Last date and save the plots in breviously created folders.
for(i in FirstDay:LastDay){
  # First, select the i'th date and the date after this. 
  iDate1 <- as.Date(levels(factor(SiebentageLK$Datum))[i])
  iDate2 <- as.Date(levels(factor(SiebentageLK$Datum))[i + 1])
  # Plot 1 Stuff
  # Get the colors (based on the colors of the Corona-Map of the RKI) for each Landkreis.
  Inzidenz1 <- SiebentageLK[SiebentageLK$Datum == iDate1, ]$col
  Inzidenz2 <- SiebentageLK[SiebentageLK$Datum == iDate2, ]$col
  CL <- list()
  # Interpolate the colors from one date to another.
  for(k in 1:length(Inzidenz1)){
    CL[[k]] <- colorRampPalette(c(Inzidenz1[k], Inzidenz2[k]))(FramesPerDay + 1)
  }
  names(CL) <- SiebentageLK[SiebentageLK$Datum == iDate1, ]$Landkreis
  
  # Plot 3 stuff
  # Select the top 5 Landkreise (7 Day Incidence per 100.000)
  Top5LK_1 <- SiebentageLK[SiebentageLK$Datum == iDate1, ]
  Top5LK_1 <- Top5LK_1[order(-Top5LK_1$Inzidenz), ][1:5, ]
  # Set the X-Values of the bars in the following bar chart.
  Top5LK_1$x <- 5:1
  # Also do this for the top 5 of the next day.
  Top5LK_2 <- SiebentageLK[SiebentageLK$Datum == iDate2, ]
  Top5LK_2 <- Top5LK_2[order(-Top5LK_2$Inzidenz), ][1:5, ]
  Top5LK_2$x <- 5:1
  # Merge both top 5's
  Top5LK <- rbind(Top5LK_1, Top5LK_2)
  # Add the missing dates.
  Top5LK <- merge(Top5LK, SiebentageLK[SiebentageLK$Landkreis %in% c(Top5LK$Landkreis) &
                                         SiebentageLK$Datum %in% c(Top5LK$Datum), ],
                  by = c("Landkreis", "Datum", "Inzidenz", "col"), all = TRUE)
  colnames(Top5LK) <- c("LandkreisID", "Refdatum", "SevenDays", "col", "x")
  # And if there is not a single Landkreis with at least 1 case, set all bar positions to 0 so they're not displayed in the plot.
  if(sum(is.na(Top5LK$x)) > 0){
    Top5LK[is.na(Top5LK$x), ]$x <- 0
  }
  # End of Plot 3 stuff
 
  # Now we have all data of two dates, make FramesPerDay plots per day (data is interpolated).
  for(p in 1:FramesPerDay){
    # This is the first plot, the Map on the left side of the plot.
    p1 <- ggplot(LK_GIS, aes(x = long, y = lat, fill = factor(Kennziffer), group = group))+
      geom_polygon(color = "grey40")+
      annotate("text", 
               x = (max(LK_GIS$long) - min(LK_GIS$long)) * 0.01 + min(LK_GIS$long), 
               y = (max(LK_GIS$lat)  - min(LK_GIS$lat))  * 0.95 + min(LK_GIS$lat), 
               label = format(iDate1, "%d.%m.%Y"), 
               hjust = 0,
               size = 8)+
      scale_fill_manual(values = sapply(CL, "[[", p))+
      theme_void()+
      theme(legend.position = "none")
    
    # The second plot is the Cases / Deaths in time. If there are no cases, set the y limits to 0 an 1,
    # if there are cases, set them to 1.1 * the maximum value.
    p2Lim <- ifelse(max(CasesPlot[CasesPlot$CRow <= CurrentRow, ]$SMax, na.rm = T) == 0, 1,
                    max(CasesPlot[CasesPlot$CRow <= CurrentRow, ]$SMax, na.rm = T) * 1.1)
    
    # Plot second plot
    p2 <- ggplot(CasesPlot[CasesPlot$CRow <= CurrentRow, ])+
      geom_area(aes(x = Refdatum, y = SevenDays, fill = Altersgruppe))+
      geom_line(data = CasesPlot[CasesPlot$Altersgruppe == "unbekannt" &
                                   CasesPlot$CRow <= CurrentRow, ], 
                aes(x = Refdatum, y = Deaths * 1), size = 1.2)+
      scale_y_continuous(name = "Infektionen / Tote",
                         labels = comma,
                         limits = c(0, p2Lim),
                         expand = c(0, 0))+
      scale_x_datetime(name = "Datum", 
                       limits = c(min(CasesPlot$Refdatum, na.rm = T),
                                  max(CasesPlot[CasesPlot$CRow <= CurrentRow, ]$Refdatum, na.rm = T)),
                       expand = c(0, 0))+
      scale_fill_manual(values = c("#404040", 
                                   "#bba807", 
                                   "#3da5a7", 
                                   "#246e89", 
                                   "#6d6fbb", 
                                   "#bc68bd", 
                                   "#e1a0a4"),
                        labels = c("unbekannt",
                                   "80 +",
                                   "60 - 79",
                                   "35 - 59",
                                   "15 - 34",
                                   "  5 - 14",
                                   "0 - 5"))+
      theme_classic()+
      theme(legend.text.align = 0.5,
            axis.title.x = element_blank(),
            legend.position = c(0, 1),
            legend.justification = c(-0.1, 1))
    
    # The third plot is the plot with the top 5 Landkreise. 
    # First, Interpolate the coordinates of the bars between two dates.
    # They are not suddenly changing positions but rather floating from one place to another.
    iy <- ((Top5LK[Top5LK$Refdatum == levels(factor(Top5LK$Refdatum))[2], ]$x - 
              Top5LK[Top5LK$Refdatum == levels(factor(Top5LK$Refdatum))[1], ]$x) / FramesPerDay) * 
      p + Top5LK[Top5LK$Refdatum == levels(factor(Top5LK$Refdatum))[1], ]$x
    ix <- ((Top5LK[Top5LK$Refdatum == levels(factor(Top5LK$Refdatum))[2], ]$SevenDays - 
              Top5LK[Top5LK$Refdatum == levels(factor(Top5LK$Refdatum))[1], ]$SevenDays) / FramesPerDay) * 
      p + Top5LK[Top5LK$Refdatum == levels(factor(Top5LK$Refdatum))[1], ]$SevenDays
    # Create a vecor with the Landkreis names.
    iKreis <- Top5LK[Top5LK$Refdatum == levels(factor(Top5LK$Refdatum))[2], ]$LandkreisID
    # Create a vector specifying the colors of the bars, depending on the Bundesland (state).
    iColors <- LK_Namen[match(iKreis, LK_Namen$LandkreisID), ]$Bundesland
    # Because the Landkreis names have only been ID's up to this moment, now replace them with the real names.
    iKreis <- LK_Namen[match(iKreis, LK_Namen$LandkreisID), ]$LandkreisName
    # Set coordinates of the bars to 6, if there are no cases and the width of the bars to 0.1 to have a limit 
    # and not a squished plot.
    iy <- ifelse(ix == 0, 6, iy)
    ix <- ifelse(ix == 0, 0.1, ix)
    
    # Now plot the third plot.
    p3 <- ggplot()+
      geom_rect(aes(xmin = 0, 
                    xmax = ix, 
                    ymin = ifelse(iy < 0.9 & iy > 0.1, 0.5, iy - 0.4), 
                    ymax = iy + 0.4,
                    fill = iColors),
                alpha = 0.5)+
      geom_vline(xintercept = 0)+
      geom_text(aes(x = ix - (max(ix) *0.02), y = iy, label = round(ix, digits = 1)), hjust = 1)+
      geom_text(aes(x = max(ix) * 0.01, y = iy, label = iKreis, hjust = 0))+
      scale_fill_manual(values = ColorsBund)+
      scale_y_continuous(limits = c(0.5, 5.5))+
      scale_x_continuous(limits = c(0, max(ix)))+
      theme_classic()+
      theme(axis.text.y = element_blank(),
            axis.title = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.x = element_blank(),
            legend.position = "none")
    
    # Arrange the three plots and save them in the previously created folder.
    px <- ggarrange(p1, ggarrange(p3, p2, nrow = 2))
    ggsave(px, file = paste0(getwd(), "/Plots/", npic, ".png"),
           width = 16, height = 9, dpi = 120)
    
    # Now print the percentage of plots already saved to see how long it still takes
    print(paste0(npic, " of ", ((LastDay - FirstDay + 1) * FramesPerDay), ".   ",
                 round(npic / ((LastDay - FirstDay + 1) * FramesPerDay) * 100, digits = 1), " % done."))
    # Go to the next row of the data and add 1 to the save plot location.
    npic <- npic + 1
    CurrentRow <- CurrentRow + 1
  }
}













