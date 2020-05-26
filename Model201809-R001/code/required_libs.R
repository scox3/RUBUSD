#Data manipulations


#"SSOAP",

lib_list <- c( "lubridate", "stringi", "stringr", "plyr", "data.table", "xts", 
		"xlsx", "curl", "XML", "RCurl", 
		"tseries", "forecast", "seasonal",
		"ggplot2", "ggthemes", "plotly", 
		"quantmod", "Quandl" )

lapply( lib_list, function( s ) { 
			if(!require(s, character.only = TRUE)){
				install.packages(s)
				library(s, character.only = TRUE)
			}})

# "sophisthse"