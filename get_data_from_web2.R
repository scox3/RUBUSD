#{r Get data from CBR: exchange rate}
source( "CBR_web.R")

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
  
  t1<-FlikvidXML(as.Date('1992-01-01'), Sys.Date())
  t2 <- apply( t1[,-1], c(2), function(x) {return(as.numeric(as.character(x)))})
  t3 <- data.frame( date=as.Date(t1$date), t2)
  #wrong date in dataset? Различие с таблицей на сайте +1
  t3$date <- t3$date+1
  names(t3)[names(t3)=="liq"] <- "bank.debt.on.fed.deposits" 
  names(t3)[names(t3)=="reserve.req"] <- "int.gov.debt" 
  names(t3)[8:11] <- c("Minfin", "reserve.req", "NettoLiq", "FLQUnknown")
  #tail(t3)
  
  t4 <- xts(t3[,-1], order.by=t3$date)
  df_merged <- merge(df_merged, t4,  join="left")	
  
  return( df_merged) 
}

cbr_d <- get_cbr_data_ws_daily()



#{r Get Data from FRED: Brent price, CPI RUS, CPI US}
#Consumer Price Index for All Urban Consumers: All Items (CPIAUCNS)
# Consumer Price Index: All Items for Russian Federation (RUSCPIALLMINMEI),  Units: Index 2010=100, Not Seasonally Adjusted Frequency: Monthly

require(quantmod)

fred_list_D <-  c("CPIAUCNS", "RUSCPIALLMINMEI", "DCOILBRENTEU", 'DEXUSEU', 'WPRIME', "USDONTD156N", "USD1MTD156N", "USD3MTD156N", 'USD3MTD156N',  'USD6MTD156N', 'EUR1MTD156N', 'EUR3MTD156N', 
                  'EUR6MTD156N', 'TEDRATE', 'DTB3', 'DTB6', 'TB1YR', 'SP500')
getSymbols(  fred_list_D , src='FRED' )

# 3-Month London Interbank Offered Rate (LIBOR), based on U.S. Dollar (USD3MTD156N) 	
# 6-Month London Interbank Offered Rate (LIBOR), based on U.S. Dollar (USD6MTD156N), Daily
# Gold Fixing Price 10:30 A.M. (London time) in London Bullion Market, based in U.S. Dollars (GOLDAMGBD228NLBM) 
# U.S. Dollars per Troy Ounce, Not Seasonally Adjusted Frequency: Daily 	
#TED Spread (TEDRATE) - Series is calculated as the spread between 3-Month LIBOR based on US dollars (https://fred.stlouisfed.org/series/USD3MTD156N) and 3-Month Treasury Bill

# ICE BofAML US Corporate AAA Effective Yield (BAMLC0A1CAAAEY) 
# 6-Month Treasury Bill: Secondary Market Rate (DTB6) 
#1-Year Treasury Bill: Secondary Market Rate (TB1YR)

#S&P 500


#shift to end of month
index(CPIAUCNS) <- index(CPIAUCNS) - 1 
index(RUSCPIALLMINMEI) <- index(RUSCPIALLMINMEI) - 1 


#{r Daily data series setup}
leny <- 2021-1992
len <- leny %/%4 + leny*365
dates <- seq(as.Date("1992-01-01"),length=len,by="days")

ddatD <- xts( x=dates, order.by=dates)

##ADD excahnge rate RUBUSD
x1 <- do.call(merge.xts, mget(fred_list_D)) #by default merge keeps all data
ddatD <- merge(ddatD, x1, join="left")  
ddatD <- merge(ddatD, cbr_d, join="left")

rm(leny)
rm(dates)

#{r Monthly data series setup}
##Month endpoints
ep2 <- endpoints(ddatD,on="months")

ddatM <- ddatD[ ep2, 1 ]

ddatM <- merge(ddatM, CPIAUCNS, join="left")
ddatM <- merge(ddatM, RUSCPIALLMINMEI, join="left")

###Calc monthly averages from days data
ep2 <- endpoints(ddatD,on="months")
#fill NA with last observation and calculate monthly means
d4 <- period.apply( na.locf(ddatD),INDEX=ep2,FUN=mean)

ddatM <- merge(ddatM, d4, join="left")
ddatM$ddatD.1 <- NULL


ddatM_save <- ddatM
ddatD_save <- ddatD

write.zoo( ddatD, "ddatD.zoo", sep=",")
write.zoo( ddatM, "ddatM.zoo", sep=",")
