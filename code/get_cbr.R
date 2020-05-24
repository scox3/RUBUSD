


# Получение динамики ежедневных курсов валюты (как XMLDocument)
GetCursDynamicXML <- function(FromDate, ToDate, ValutaCode){
  
  FromDate <- "2020-01-01"
  ToDate <- "2020-04-01"
  ValutaCode <- "R01235"
    
  if(!FromDate<ToDate){stop('Dates sequence error')}
  h <- basicTextGatherer()
  req_url <- 'http://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx'
  req_body <-paste( 
    '<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <GetCursDynamicXML xmlns="http://web.cbr.ru/">
      <FromDate>', FromDate,'</FromDate>
      <ToDate>', ToDate, '</ToDate>
      <ValutaCode>', ValutaCode,'</ValutaCode>
    </GetCursDynamicXML>
  </soap:Body>
</soap:Envelope>', sep = '')
  HeaderFields=c(Accept="text/xml", Accept="multipart/*", SOAPAction='"http://web.cbr.ru/GetCursDynamicXML"',
                 'Content-Type' = "text/xml; charset=utf-8")
  curlPerform(url = req_url, httpheader = HeaderFields, 
              postfields = req_body, writefunction = h$update)
  response <- h$value()     
  doc <- xmlInternalTreeParse(response)
  df <- Doc2Df(doc, 'ValuteCursDynamic')
  cleanup <- getNodeSet(doc, '//ValuteCursDynamic')
  
  df[, 'CursDate']<- as.Date(as.POSIXct(df[, 'CursDate']))
  df <- df[, c(1,4)]
  df <- xts(as.numeric(as.character(df[,2])), order.by = df[,1])
  return(df)
}
