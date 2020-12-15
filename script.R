library(ggplot2)
library(plyr)
library(ggpubr)
library(tidyverse)

#Dataset from middle of 2016, 2016 is not complete, so we exclude it.
vgsales <- vgsales[which(vgsales$Year < 2016),]
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

#Top10 sales per decade (bar chart)

vgsalesDecade <- vgsales 
vgsalesDecade$Decade <- as.numeric(vgsalesDecade$Year) - as.numeric(vgsalesDecade$Year) %% 10 #Adding decade column
vgsalesDecade <- na.omit(vgsalesDecade)

gameRanked <- ddply(vgsalesDecade,"Decade",summarize, #Getting the rank of each games, per decade
              name = Name, 
              sales = Global_Sales, 
              platform = Platform,
              rank=rank(-Global_Sales, ties.method="first"))

Top10 <- gameRanked[which(gameRanked$rank<=10),] #top10 of games for each decade
Top10

plots <- list() #list to store plots and show them at the same time
cnt <- 1
minDecade <- min(vgsalesDecade$Decade)
maxDecade <- max(vgsalesDecade$Decade)
for (dec in seq(from=minDecade, to=maxDecade, by=10)) {
    max_sales = Top10[Top10$Decade == dec & Top10$rank == "1",]$sales #rank1 for scaling
    title <- paste("Top 10 game sales for decade",toString(dec))
    dataDecade <- Top10[Top10$Decade==dec,]
    plots[[cnt]] <- ggplot(data = dataDecade,aes(x = rank, y = sales), label=sales) + #plotting
      geom_bar(position="dodge",stat="identity") +
      geom_text(aes(label = sales),size=3,vjust=2,hjust=0.5) + 
      geom_text(aes(label = paste(name,"(",platform,")")),angle=20,size=3,vjust=-2,hjust=0.05) + 
      ggtitle(title) +
      labs(x="Rank", y="Sales number (millions)") +
      ylim(0,max_sales+3) + 
      scale_x_continuous(breaks=dataDecade$rank, limits=c(0,15))
    cnt <- cnt + 1
  
}
figure <- ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]],
                    ncol = 4, nrow = 1)
figure #showing every plots, for each decade

# Sales per contient and per year

salesLocationYear <- ddply(vgsales,"Year",summarize, #Total sales per location for each years
             NAsales = sum(NA_Sales),
             EUsales = sum(EU_Sales),
             JPsales = sum(JP_Sales),
             OtherSales=sum(Other_Sales))

salesLocationYear$Year <- as.numeric(as.character(salesLocationYear$Year)) 

ggplot(salesLocationYear, aes(x=Year)) + 
  geom_line(aes(y=NAsales, colour="NA")) +
  geom_line(aes(y=EUsales, colour="EU")) +
  geom_line(aes(y=JPsales, colour="JP")) +   
  geom_line(aes(y=OtherSales, colour="Other")) + 
  scale_color_discrete(name = "Location") +
  labs(x = "Year", y = "Sales (in millions)", title = "Sales per continent, per year")

# Sales of the 5 most sold genre, per year

vy <- ddply(vgsales,"Genre",summarise, #Getting top5 genre
            vente = sum(Global_Sales))
vy <- vy[order(-vy$vente),]
vy <- vy %>% slice(1:5)
vec_vy <- vy$Genre 

vgsalesTop5genre <- vgsales[vgsales$Genre %in% vec_vy,] #Removing every games not in top5 genre
vgsalesTop5genre

salesGenreYear <- ddply(vgsalesTop5genre,c("Year","Genre"), summarize,  #Getting sales per genre, per year
                      sales = sum(vgsalesTop5genre[which(vgsalesTop5genre$Genre == Genre & vgsalesTop5genre$Year == Year),11]))

ggplot(salesGenreYear, aes(x=Year, y = sales, group = Genre, colour = Genre)) + 
  geom_line(size=1) + 
  labs(x = "Year", y = "Sales (in millions)", title = "Sales per genre, per year") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))


# sales per top3 console/Decade per year

vgsalesDecade$Decade <- as.numeric(vgsalesDecade$Year) - #decade column
  as.numeric(vgsalesDecade$Year) %% 10 
vgsalesDecade <- na.omit(vgsalesDecade)


salesPerDecade <- ddply(vgsalesDecade,c("Decade","Platform"),summarize, #Sales per decade, per platform
                    sales = sum(Global_Sales))
salesPerDecade <- salesPerDecade[order(salesPerDecade$Decade,-salesPerDecade$sales),]

minDecade <- 1980
maxDecade <- 2010
vecPlatform <- c()
for (dec in seq(from=minDecade, to=maxDecade, by=10)) {
  tempDF <- salesPerDecade[which(salesPerDecade$Decade == dec),] #Sales per decade for top3 platform
  tempDF <- tempDF %>% slice(1:3)
  vecPlatform <- c(vecPlatform,c(tempDF$Platform))
}

vgsalesFiltered = filter(vgsales,Platform %in% vecPlatform) #Removing non-top platform

salesYearPlatform <- ddply(vgsalesFiltered,c("Platform","Year"),summarize, #Getting sales per platform, per year
                                  sales=sum(Global_Sales))

ggplot(data = salesYearPlatform, aes( x = Year, y = sales, fill = Platform)) +
  geom_bar(aes(x=Year, y =sales),stat="identity", position = position_fill()) +
  theme(legend.position='bottom') + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) + 
  labs(title="Top3 platform per decade, share of sales") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

# most sold genre per console

topGenreConsole <- ddply(vgsales,c("Platform","Genre"),summarize, #Sales per genre
                         sales = sum(vgsales[which(vgsales$Genre == Genre & vgsales$Platform == Platform),11]))

topGenreConsole <- ddply(topGenreConsole,"Platform",summarize, #Ranking of sales per genre for each platform 
                         genre = Genre,
                         Vente= sales,
                         rank=rank(-sales, ties.method="first"))

topGenreConsole <- topGenreConsole[which(topGenreConsole$rank==1),] #Keeping top1

VenteParConsole <- ddply(vgsales,"Platform",summarize, #Sales per platform, number of games sold
                         VT = sum(vgsales[which(vgsales$Platform == Platform),11]))

VenteParConsolePg <- ddply(topGenreConsole,"Platform",summarize, #Sales per platform with percentage
              Genre = genre,
              vente = ((Vente/VenteParConsole[which(VenteParConsole$Platform == Platform),2] )*100))


specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

ggplot(data = VenteParConsolePg) +
  geom_bar(aes(x = Platform, y = vente, fill=Genre),alpha=0.5,stat="identity") +
  geom_text(aes(x = Platform, y = vente,label = specify_decimal(vente,1)),size=3,vjust=-1) + 
  labs(x="Platform", y="Share of sales for the top-selling genre", title="Share of sales for the top-selling genre per platform") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# sales for top20 publiser

salesPerPublisher <- ddply(vgsales,"Publisher",summarize,
                           sales=sum(Global_Sales)) #Sales per publisher

salesPerPublisher <- salesPerPublisher[order(-salesPerPublisher$sales),] #Ordering

Top20Publishers <- c(salesPerPublisher[1:20,1]) #keeping top20 publisher

salesPerPublisher <- salesPerPublisher[which(salesPerPublisher$Publisher #keeping only top20 publiser in data set
                                             %in% Top20Publishers),] 

ggplot(data = salesPerPublisher) +
  geom_bar(aes(x = Publisher, y = sales, fill = Publisher),alpha=0.5,stat="identity") +
  geom_text(aes(x = Publisher, y = sales,label = specify_decimal(sales,0)),size=3,vjust=-1) + 
  labs(x="Publisher", y="Sales (in millions)", title="Sales per publisher, between 1980 and 2015") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# percentage of genre for top8 publisher

salesPerPublisher <- ddply(vgsales,"Publisher",summarize,
                           sales=sum(Global_Sales)) #Sales per publisher

salesPerPublisher <- salesPerPublisher[order(-salesPerPublisher$sales),] #Ordering

Top8Publishers <- c(salesPerPublisher[1:8,1]) #keeping top8 publisher

topGenrePublisher <- ddply(vgsales,c("Genre","Publisher"),summarize, #Sales per genre for publisher
                         sales = sum(vgsales[which(vgsales$Genre == Genre & vgsales$Publisher == Publisher),11]))

venteParPublisher <- ddply(vgsales,"Publisher",summarize, #Sales for each publisher
                           VT = sum(vgsales[which(vgsales$Publisher == Publisher),11]))

topGenrePublisher <- topGenrePublisher[which(topGenrePublisher$Publisher #removing non-top8 publisher
                                             %in% Top8Publishers),] 

topGenrePublisher <- merge(topGenrePublisher, #merging top genre and sales per publisher 
                           venteParPublisher) #to get 2 informations in the same dataset

topGenrePublisher$pg <- (topGenrePublisher$sales/topGenrePublisher$VT)*100 #percentage

ggplot(data = topGenrePublisher, aes( x = Publisher, y = pg, fill = Genre)) +
  geom_bar(aes(x=Publisher, y =pg),stat="identity", position = position_fill()) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  theme(legend.position='bottom') + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) + 
  labs(x="Publisher", y="Share of sales for each genre", title="Share of sales for each genre per publisher") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3))




