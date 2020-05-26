
make_rus_colnames <- function( v1 ) { 
  v2 <- stri_trans_general(v1, "Russian-Latin/BGN")
  v2 <- str_replace_all( v2, "[[:cntrl:]]", " ")
  v2 <- str_replace_all(v2, "[^[:alnum:]]", " ")
  v2 <- iconv(v2, "latin1", "ASCII", sub="_")	
  return(v2)
}

data_folder = "Data/"

get_cbr_CR_table <- function() {
  ###Grab CR HTML data

  #names( icr_dataset1)[5]<-"CR_DIFF_OP"
  
  #2003-2007 data get HTMLs saved from CBR site
  

  options(stringsAsFactors= FALSE)
  df4 <- NULL
  
  for( iy in  c(3:7) ){
    print (iy)
    fn <- paste0(data_folder, "iip_ira_0",iy,".htm")
    print( fn)
    df1 <- readHTMLTable(fn, elFun=function(node){
      return( as.character( xmlValue(node))) })
    
    n<-length(df1)
    print(n)
    
    for( i in 1:length(df1) ) {
      print( paste0( "table#", i ))
      
      df2 <- df1[[i]]
      
      #			s1<- colnames(df2)			
      s1 <- make_rus_colnames( trimws(colnames(df2)) )
      
      ir1 <- c(1:dim(df2)[1])
      ic1 <- c(2:7)
      
      #remove white spaces and convert to numeric
      df2[ir1,ic1] <- apply( df2[ir1,ic1], c(1,2), function( x) { as.numeric(str_replace_all(x, "\\s","")) } )
      
      #remove dates from column names so that rbind works
      colnames(df2) <- c( s1[1], paste( str_sub(s1[2],end=-9), "DateSt", sep=""), s1[3:6],
                          paste( str_sub(s1[2],end=-9), "DateFn", sep=""))
      
      #set dates columns
      df2$DateSt <- as.Date(str_sub(s1[2],start=-8), format="%d %m %y")
      df2$DateFn <- as.Date(str_sub(s1[7],start=-8), format="%d %m %y")
      
      #merge with growing frame
      if( is.null(df4) ) { df4 <- df2 } else {df4 <- rbind(df4, df2) }
    }
  }
  
  #insert middle column to comply with latest CBR format
  require(tibble)
  df4 <- add_column(df4, X6 = 0, .before = 5) 
  
  df4$Keys <- make_rus_colnames(trimws(df4[,1]))
  
  icr_dataset1 <- df4
  #	View( icr_dataset1)
  
  
  #Collect data from files for 2008-2017
  df5 <- NULL 
  ny2 <- last_year %% 100
  for( i in 8:ny2) {
    fn  <- paste0( data_folder, "iip_ira_", formatC(i,width=2,flag="0",zero.print = TRUE), ".xlsx")
    print(fn)
    df1 <- read.xlsx(fn, stringsAsFactors = FALSE, header=FALSE,encoding = "UTF-8",sheetIndex=1, colIndex=c(1:8), startRow = 4)
    if( is.null(df5) ) { df5 <- df1 } else { df5 <- rbind(df5, df1) }
  }
  #	View(df5)
  
  #setup colums
  df5$DateSt <- as.Date("1970-01-01")
  df5$DateFn <- as.Date("1970-01-01")
  
  #Make keys column
  df5$Keys <- make_rus_colnames(trimws(df5[,1]))
  
  #find rows in which dates are present
  i1 <- grep( "^Ostatok", make_rus_colnames(df5[,2]), perl=TRUE)
  
  #	i1<-c(i1, dim(df5)[1])
  #i2 <- do.call( c, sapply( seq(1:(length(i1)-1)), function(i) { return( i1[i]:i1[i+1])}) )
  
  for( i in 1:length(i1)  ) {
    #		print( paste0(str_sub(df5[i1[i],2],start=-8), "/", str_sub(df5[i1[i],8],start=-8)))
    n <- nchar(df5[i1[i],2])
    d1 <-  as.Date(str_sub(df5[i1[i],2],start=-8), format="%d.%m.%y")
    #  print(d1)
    d2 <-  as.Date(str_sub(df5[i1[i],8],start=-8), format="%d.%m.%y")
    
    print( paste0(d1, "-", d2))
    df5$DateSt[(i1[i]):(i1[i]+7)]<- d1		
    df5$DateFn[(i1[i]):(i1[i]+7)]<- d2
  }
  
  #Find end row for each data block
  i1 <- grep( "^Mezhdunar", df5$Keys, perl=TRUE)
  i2 <- grep( "^Proch", df5$Keys, perl=TRUE)
  
  #Form vector of row indexes for extraction
  ii <- unlist( sapply( c(1:length(i1)), function(i) { return( c(i1[i]:i2[i]))} , simplify=TRUE)	)
  
  #Select rows
  df6 <- df5[ii, ]
  
  #	View(df6)
  
  #проблема df4 и df6 имеют разное число колонок, так как есть колка Keys
  colnames(df6)<- colnames(icr_dataset1)
  
  #Join datasets
  icr_dataset2 <- rbind(icr_dataset1, df6)
  
  # set duration column
  icr_dataset2$Duration <- icr_dataset2$DateFn -  icr_dataset2$DateSt
  
  #	View(icr_dataset2)
  
  
  #Rename Keys
  spatterns <- data.frame( pattern= c("(^Mezhdunar|Rezervnyye)", "^Proch", "SDR", "MVF", "zoloto"), 
                           replacement= c("Total", "Proch", "SDR", "MVF", "Zoloto"), 
                           stringsAsFactors=FALSE)
  
  icr_dataset2$K <- ""
  
  for( i in c(1:dim(spatterns)[1]) ) {
    
    ii1 <-  grep( spatterns[i,1], icr_dataset2$Keys, perl=TRUE)
    print( length(ii1))
    icr_dataset2[ii1, "K"] <- spatterns[i,2]
  }
  
  icr_dataset2$Keys <- icr_dataset2$K
  
  icr_dataset2$K <- NULL
  
  return( icr_dataset2 )
}


select_cbr_icr_type <- function( crtable, cr_component, periodicity) {
  #periodicity in months
  ii <- grep(paste0("^", cr_component), crtable$Keys, perl="TRUE" )
  cr1 <- crtable[ii,]
  
  #id cr1 <=0 return all
  if( periodicity > 0 ) { 
    cr1 <-  cr1[ ( cr1[ , "Duration" ] <= periodicity* 31 ) & ( cr1[ , "Duration" ] > (periodicity-1)* 31 ), ]
  } 
  
  return( cr1[ order( cr1[,"DateSt"]), ])
}

get_cbr_icr_component <- function(crtable, cr_component, periodicity ) {
  
  cr1 <- select_cbr_icr_type( crtable, cr_component , 1 ) 
  if( periodicity == 1 ) {
    return( cr1 ) 
  } else if( periodicity == 3 ) {
    cr2 <- select_cbr_icr_type( crtable, cr_component , 3 )
    
    n1 = dim( cr1 )[1]
    icnt = dim( cr2 )[1]+1
    for( i in c(1: (n1 %/% 3) ) ) { 
      print(i)
      i1 <- (i-1)*3 + 1
      i2 <- i1 + 2
      cr2[icnt, ] <- NA
      cr2[icnt, 1:2] <- cr1[i1,1:2]
      cr2[icnt, 3:7] <- unlist( sapply( c(3:7), function(j) { return( sum(as.numeric(cr1[i1:i2, j])))} ) )
      cr2[icnt, 8] <-  cr1[i2,8]
      cr2[icnt, 9] <-  cr1[i1,9]
      cr2[icnt, 10:11] <-  cr1[i2,10:11]			
      cr2[icnt, 12] <- cr1[i2,10]-cr1[i1,9]
      icnt <- icnt+1
    }
    return(cr2)
  } else if( periodicity == 12 ) {
    cr2 <- select_cbr_icr_type( crtable, cr_component , 12 )
    n1 = dim( cr1 )[1]
    icnt = dim( cr2 )[1]+1
    for( i in c(1: (n1 %/% 12) ) ) { 
      print(i)
      i1 <- (i-1)*12 + 1
      i2 <- i1 + 11
      cr2[icnt, ] <- NA
      cr2[icnt, 1:2] <- cr1[i1,1:2]
      cr2[icnt, 3:7] <- unlist( sapply( c(3:7), function(j) { return( sum(as.numeric(cr1[i1:i2, j])))} ) )
      cr2[icnt, 8] <-  cr1[i2,8]
      cr2[icnt, 9] <-  cr1[i1,9]
      cr2[icnt, 10:11] <-  cr1[i2,10:11]			
      cr2[icnt, 12] <- cr1[i2,10]-cr1[i1,9]
      icnt <- icnt+1	
    }	
    return(cr2)
  }
}

select_icr_Proch <- function( cr1, period ) { 
  df3 <- select_cbr_icr_type( cr1, "Proch", period )[, c("DateFn", "Ostatok na DateFn", "Izmeneniya v rezul__tate operatsiy" )]
  cr_names <-  c("Date", "CR", "dCR")
  
  names(df3) <- cr_names 		
  df3$Date <- df3$Date -1
  df3[,(2:3)] <- apply( df3[,(2:3)], 2, as.numeric )
  return(df3)
}

last_year <- 2018
ICR_table <- get_cbr_CR_table() 
ICR_M <- select_icr_Proch(ICR_table, 1)
ICR_Q <- select_icr_Proch(ICR_table, 3)

ICR_Q_xts <- xts( ICR_Q[,-1], order.by = ICR_Q[,1])
ICR_M_xts <- xts( ICR_M[,-1], order.by = ICR_M[,1])


d1 <- rbind( ICR_M_xts[,"dCR"], ICR_Q_xts[paste0("/",index(first(ICR_M_xts))), "dCR"]/3 )
d2 <- ddatM$ddatD["2003/"] 
d2$dCR <- d1$dCR
d2 <- na.locf(d2, fromLast=TRUE)

ddatM$dICR_CB <- NULL
ddatM$dICR_CB <- d2$dCR

