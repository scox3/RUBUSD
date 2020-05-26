####get data from Web

#{r Get data from CBR: exchange rate}

source( "code/CBR_web.R")

get_cbr_data_ws_daily <- function() { 
  
  df1 <- GetCursDynamicXML( as.Date('1992-01-01'), Sys.Date(), "R01235")
  df1["/1997-12-29"] <- df1["/1997-12-29"]/1000
  
  df2 <- GetCursDynamicXML( as.Date('1992-01-01'), Sys.Date(), "R01239")
  
  df_merged <- merge(df1, df2, suffixes=c("RUBUSD", "RUBEUR"), join="outer")
  
  df1 <- MosPrimeXML( as.Date('1992-01-01'), Sys.Date() )
  colnames(df1) <- paste( "MosPrime_", colnames(df1), sep="")
  df_merged <- merge(df_merged, df1, suffixes=c("", ""), join="outer")
  
  #	df1 <- MKRXML(as.Date('1992-01-01'), Sys.Date() ) 
  # colnames(df1) <- paste( "MBK_", colnames(df1), sep="")
  # head(df_merged)
  
  return( df_merged) 
}


crb_d <- get_cbr_data_ws_daily()


#{r Get Data from FRED: Brent price, CPI RUS, CPI US}
#Consumer Price Index for All Urban Consumers: All Items (CPIAUCNS)
# Consumer Price Index: All Items for Russian Federation (RUSCPIALLMINMEI),  Units: Index 2010=100, Not Seasonally Adjusted Frequency: Monthly

library(quantmod)

getSymbols( c("CPIAUCNS", "RUSCPIALLMINMEI", "DCOILBRENTEU") , src='FRED' )


# ```{r Read Minfin currency purchases dataset }
# library(readxl)
# Minfin_currency_purchases <- read_excel("Minfin currency purchases.xlsx")
# View(Minfin_currency_purchases)
# ```
# 
# 
# ```{r Read Non-res OFZ  purchases dataset}
# library(readxl)
# table_ofz <- read_excel("table_ofz.xlsx", 
#                         col_types = c("date", "numeric", "numeric", 
#                                       "text", "numeric", "numeric"), skip = 1)
# View(table_ofz)
# ```
# 
# 
# ```{r Read HSE dataset on export}
# HSE_EX_T_M  <- sophisthse("EX_T_M", output="ts")	
# HSE_EX_T_M <- as.xts( HSE_EX_T_M )
# index(HSE_EX_T_M) <- as.Date( index( as.xts(HSE_EX_T_M) ) )
# # plot(HSE_EX_T_M )
# ```


#{r Daily data series setup}
leny <- 2021-1992
leny <- leny %/%4 + leny*365
dates <- seq(as.Date("1992-01-01"),length=len,by="days")

ddatD <- xts( x=dates, order.by=dates)
tail(ddatD)

##ADD excahnge rate RUBUSD
ddatD <- merge(ddatD, DCOILBRENTEU, join="left")
ddatD <- merge(ddatD, crb_d[, "RUBUSD"], join="left")

rm(leny)
rm(dates)


#{r Monthly data series setup}
##Month endpoints
ep2 <- endpoints(ddatD,on="months")

ddatM <- ddatD[ ep2, 1 ]
index(ddatM) <- index(ddatM) + 1

ddatM <- merge(ddatM, CPIAUCNS, join="left")
ddatM <- merge(ddatM, RUSCPIALLMINMEI, join="left")


##ADD Minfin purchases 
# Minfin_currency_purchases[is.na(Minfin_currency_purchases)] <- 0 
# 
# d4b <- xts( x=Minfin_currency_purchases$Minfin_Purchases, order.by=Minfin_currency_purchases$Date)
# index(d4b) <- as.Date( index(d4b))+1
# colnames(d4b) <- "MinfinPRUB"
# 
# ddatM <- merge( ddatM,  d4b, join="left" )
# rm(d4b)

##ADD OFZ Non residents - monthly
# table_ofz <- table_ofz[ !is.na(table_ofz[,1]), ]
# d2 <- xts( table_ofz$`Номинальный объем ОФЗ, принадлежащих нерезидентам,             млрд руб.1)`, order.by = table_ofz$`Дата` )
# colnames(d2)[1] <- "VolOFZNonRez"
# 
# d2$OFZNonResSales <- - diff(d2$VolOFZNonRez)
# #set NA to 0
# table_ofz[ is.na(table_ofz)] <- 0 
# 
# ddatM <- merge(ddatM, d2, join="left")
# class(index(ddatM))

###Calc monthly averages from days data
# ep2 <- endpoints(ddatD,on="months")
# #fill NA with last observation and calculate monthly means
# d4 <- period.apply( na.locf(ddatD),INDEX=ep2,FUN=mean)
# index(d4)<- index(d4)+1
# 
# ddatM <- merge(ddatM, d4, join="left")
# ddatM$ddatD <- NULL
# 
# ddatM$OFZNonResSales[ is.na( ddatM$OFZNonResSales ) ] <- 0
# ddatM$MinfinPRUB[ is.na( ddatM$MinfinPRUB ) ] <- 0
# 
# ddatM$MinFin_and_NonrezOFZ_OP <- ddatM$MinfinPRUB + ddatM$OFZNonResSales 


## Add Export}
# ddatM <- merge(ddatM, HSE_EX_T_M[,1], join="left")
# 
# rm(d2)




ddatM_save <- ddatM
ddatD_save <- ddatD

write.zoo( ddatD, "ddatD.zoo", sep=",")
write.zoo( ddatM, "ddatM.zoo", sep=",")


