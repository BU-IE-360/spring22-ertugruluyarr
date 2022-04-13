install.packages("readxl")
install.packages("lubridate")
install.packages("zoo")
library(readxl)
library(lubridate)
library(zoo)
library(ggplot2)

gold_usd_debt <- read_excel("Downloads/gold-usd-debt.xlsx")
gold_usd_debt <- gold_usd_debt[1:59,]
gold_usd_debt$Tarih <- as.yearmon(gold_usd_debt$Tarih)
gold_usd_debt$`TP MK KUL YTL` <- as.numeric(gold_usd_debt$`TP MK KUL YTL`)
gold_usd_debt$`TP DK USD S YTL` <- as.numeric(gold_usd_debt$`TP DK USD S YTL`)
str(gold_usd_debt)
gold_usd_debt$Year <- factor(format(x = gold_usd_debt$Tarih, format = "%Y"), ordered = TRUE)


ggplot(data=gold_usd_debt)+
  labs(title = "Bullion Gold Selling Price (TRY/Gr)", x = "Time", y = "Levels" ) +
  geom_line(group=1,size=1,color='blue', aes(x=Tarih,y=`TP MK KUL YTL`)) +
  geom_smooth(fill = NA, color="green",linetype = "twodash", size = 0.7, aes(x=Tarih,y=`TP MK KUL YTL`))

ggplot(data = gold_usd_debt,aes(x = `TP MK KUL YTL` )) +
  geom_histogram(bins=15,alpha=0.6,aes(color=Year,fill=Year))+
  facet_wrap(facets= .~Year)+
  scale_color_brewer(palette = "Paired",aesthetics = c("color", "fill")) +
  labs(title = "Histograms of Gold Selling Price",
       x = "Gold Selling Price",
       y = "Frequency")

ggplot(data = gold_usd_debt,aes(x=factor(Year),y=`TP MK KUL YTL`))+ 
  geom_boxplot(aes(fill=factor(Year)))+ 
  xlab("Years") + ylab("Gold Selling Price")+ 
  ggtitle("Boxplot for Gold Selling Price")+
  theme(legend.position = "none")

gram_altin <- read_excel("Downloads/gram-altin.xlsx")
gram_altin$tariha <- as.yearmon(x = gram_altin$tariha)
ggplot(data = gram_altin)+
  geom_point(group =1,size = 1, color ='purple', aes(x=tariha, y= counta))+
  geom_smooth(fill = NA, color="orange",linetype = "twodash", size = 0.7, aes(x=tariha,y= counta))

ggplot(data=gold_usd_debt)+
  labs(title = "(USD) US Dollar (Selling)", x = "Time", y = "Levels" ) +
  geom_line(group=1,size=1,color='red', aes(x=Tarih,y=`TP DK USD S YTL`)) +
  geom_smooth(fill = NA, color="yellow",linetype = "twodash", size = 0.7, aes(x=Tarih,y=`TP DK USD S YTL`))

ggplot(data = gold_usd_debt,aes(x =`TP DK USD S YTL` )) +
  geom_histogram(bins=15,alpha=0.6,aes(color=Year,fill=Year))+
  facet_wrap(facets= .~Year)+
  scale_color_brewer(palette = "Paired",aesthetics = c("color", "fill")) +
  labs(title = "Histograms of US Dollar-TRY",
       x = "USD-TRY",
       y = "Frequency")

ggplot(data = gold_usd_debt,aes(x=factor(Year),y=`TP DK USD S YTL`))+ 
  geom_boxplot(aes(fill=factor(Year)))+ 
  xlab("Years") + ylab("US Dollar-TRY")+ 
  ggtitle("Boxplot for US Dollar-TRY")+
  theme(legend.position = "none")

doviz_kuru <- read_excel("Downloads/doviz-kuru.xlsx")
View(doviz_kuru)
doviz_kuru$tarihd <- as.yearmon(x = doviz_kuru$tarihd)
ggplot(data = doviz_kuru)+
  geom_point(group =1,size = 1, color ='magenta', aes(x=tarihd, y= countd))+
  geom_smooth(fill = NA, color="blue",linetype = "twodash", size = 0.7, aes(x=tarihd,y= countd))

ggplot(data=gold_usd_debt)+
  labs(title = "Domestic Debt Position (Treasury)(TRY Thousand)(Monthly)", x = "Time", y = "Levels" ) +
  geom_line(group=1,size=1,color='purple', aes(x=Tarih,y=`TP KB A09`))+
  geom_smooth(fill = NA, color="red",linetype = "twodash", size = 0.7, aes(x=Tarih,y=`TP KB A09`))

ggplot(data = gold_usd_debt,aes(x =`TP KB A09` )) +
  geom_histogram(bins=15,alpha=0.6,aes(color=Year,fill=Year))+
  facet_wrap(facets= .~Year)+
  scale_color_brewer(palette = "Paired",aesthetics = c("color", "fill")) +
  labs(title = "Histograms of Domestic Debt Position",
       x = "Domestic Debt Position",
       y = "Frequency")

ggplot(data = gold_usd_debt,aes(x=factor(Year),y=`TP KB A09`))+ 
  geom_boxplot(aes(fill=factor(Year)))+ 
  xlab("Years") + ylab("Domestic Debt Position")+ 
  ggtitle("Boxplot for Domestic Debt Position")+
  theme(legend.position = "none")

borc <- read_excel("Downloads/borc.xlsx")
borc$tarih <- as.yearmon(x = borc$tarih)
ggplot(data = borc)+
  geom_point(group =1,size = 1, color ='red', aes(x=tarih, y= count))+
  geom_smooth(fill = NA, color="yellow",linetype = "twodash", size = 0.7, aes(x=tarih,y= count))

#pairs gibi
plot(gold_usd_debt)
