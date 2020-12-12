library(ggplot2)
library(plyr)
library(ggpubr)
library(tidyverse)

#Dataset from middle of 2016, 2016 is not complete, so we exclude it.
vgsales <- vgsales[which(vgsales$Year < 2016),]
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

#Top10 sales per decade (bar chart)

vgsalesDecade <- vgsales #na.omit not working ?
vgsalesDecade$Decade <- as.numeric(vgsalesDecade$Year) - as.numeric(vgsalesDecade$Year) %% 10
vgsalesDecade <- na.omit(vgsalesDecade)

gameRanked <- ddply(vgsalesDecade,"Decade",summarize,
              name = Name, 
              sales = Global_Sales, 
              platform = Platform,
              rank=rank(-Global_Sales, ties.method="first"))

Top10 <- gameRanked[which(gameRanked$rank<=10),]
Top10

plots <- list()
cnt <- 1
minDecade <- min(vgsalesDecade$Decade)
maxDecade <- max(vgsalesDecade$Decade)
for (dec in seq(from=minDecade, to=maxDecade, by=10)) {
    max_sales = Top10[Top10$Decade == dec & Top10$rank == "1",]$sales
    title <- paste("Top 10 game sales for decade",toString(dec))
    dataDecade <- Top10[Top10$Decade==dec,]
    plots[[cnt]] <- ggplot(data = dataDecade,aes(x = rank, y = sales), label=sales) +
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
figure

# Sales per contient and per year

salesLocationYear <- ddply(vgsales,"Year",summarize,
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

vy <- ddply(vgsales,"Genre",summarise,
            vente = sum(Global_Sales))
vy <- vy[order(-vy$vente),]
vy <- vy %>% slice(1:5)
vec_vy <- vy$Genre

vgsalesTop5genre <- vgsales[vgsales$Genre %in% vec_vy,]
vgsalesTop5genre

salesGenreYear <- ddply(vgsalesTop5genre,c("Year","Genre"), summarize, 
                      sales = sum(vgsalesTop5genre[which(vgsalesTop5genre$Genre == Genre & vgsalesTop5genre$Year == Year),11]))

ggplot(salesGenreYear, aes(x=Year, y = sales, group = Genre, colour = Genre)) + 
  geom_line(size=1) + 
  labs(x = "Year", y = "Sales (in millions)", title = "Sales per genre, per year") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))


# sales per console during their lifetime ---> a filtrer pour clarifier le plot

salesYearPlatform <- ddply(vgsales,c("Platform","Year"),summarize,
                                  sales=sum(Global_Sales))

ggplot(salesYearPlatform, aes(x=Year, y = sales, group = Platform, colour = Platform)) + 
  geom_line(size=1) + 
  labs(x = "Year", y = "Sales (in millions)", title = "Sales per platform, per year") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

# most sold genre per console (bars)

topGenreConsole <- ddply(vgsales,c("Platform","Genre"),summarize, 
                         sales = sum(vgsales[which(vgsales$Genre == Genre & vgsales$Platform == Platform),11]))

topGenreConsole <- ddply(topGenreConsole,"Platform",summarize, 
                         genre = Genre,
                         Vente= sales,
                         rank=rank(-sales, ties.method="first"))

topGenreConsole <- topGenreConsole[which(topGenreConsole$rank==1),]

VenteParConsole <- ddply(vgsales,"Platform",summarize,
                         VT = sum(vgsales[which(vgsales$Platform == Platform),11]))

VenteParConsolePg <- ddply(topGenreConsole,"Platform",summarize,
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
                           sales=sum(Global_Sales))

salesPerPublisher <- salesPerPublisher[order(-salesPerPublisher$sales),]

Top20Publishers <- c(salesPerPublisher[1:20,1])

salesPerPublisher <- salesPerPublisher[which(salesPerPublisher$Publisher %in% Top20Publishers),]

ggplot(data = salesPerPublisher) +
  geom_bar(aes(x = Publisher, y = sales, fill = Publisher),alpha=0.5,stat="identity") +
  geom_text(aes(x = Publisher, y = sales,label = specify_decimal(sales,0)),size=3,vjust=-1) + 
  labs(x="Publisher", y="Sales (in millions)", title="Sales per publisher, between 1980 and 2015") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# percentage of genre for top8 publisher (piechart?)

salesPerPublisher <- ddply(vgsales,"Publisher",summarize,
                           sales=sum(Global_Sales))

salesPerPublisher <- salesPerPublisher[order(-salesPerPublisher$sales),]

Top8Publishers <- c(salesPerPublisher[1:8,1])

topGenrePublisher <- ddply(vgsales,c("Genre","Publisher"),summarize, 
                         sales = sum(vgsales[which(vgsales$Genre == Genre & vgsales$Publisher == Publisher),11]))

venteParPublisher <- ddply(vgsales,"Publisher",summarize,
                           VT = sum(vgsales[which(vgsales$Publisher == Publisher),11]))

topGenrePublisher <- topGenrePublisher[which(topGenrePublisher$Publisher %in% Top8Publishers),]

topGenrePublisher <- merge(topGenrePublisher,venteParPublisher)

topGenrePublisher$pg <- (topGenrePublisher$sales/topGenrePublisher$VT)*100

ggplot(data = topGenrePublisher, aes( x = Publisher, y = pg, fill = Genre)) +
  geom_bar(aes(x=Publisher, y =pg),stat="identity", position = position_fill()) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  theme(legend.position='bottom') + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) + 
  labs(x="Publisher", y="Share of sales for each genre", title="Share of sales for each genre per publisher") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3))




