library(ggplot2)
library(plyr)
library(ggpubr)
library(tidyverse)

#Dataset from middle of 2016, 2016 is not complete, so we exclude it.
vgsales <- vgsales[which(vgsales$Year < 2016),]

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
                      sales = sum(vgsales[which(vgsalesTop5genre$Genre == Genre & vgsalesTop5genre$Year == Year),11]))

ggplot(salesGenreYear, aes(x=Year, y = sales, group = Genre, colour = Genre)) + 
  geom_line(size=1) + 
  labs(x = "Year", y = "Sales (in millions)", title = "Sales per genre, per year") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

# evolution of sales per console



