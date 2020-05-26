## load required package `RWordPress` and `XMLRPC`
## as the interface and transfer protocol to WordPress
if (!require('RWordPress')) {
  devtools::install_github(c("duncantl/XMLRPC", "duncantl/RWordPress"))
}


library(RWordPress)
library(XMLRPC)
##
## ------------------------------------------------------------------------
##
## load `knitr`: A general-purpose tool for dynamic report generation in R
if (!require("knitr")) {
  install.packages("knitr", repos = 'http://cran.wu.ac.at/')
}


library(knitr)
##
## ------------------------------------------------------------------------
##
## load `reshape2`: Flexible restructuring and aggregating of data
if (!require("reshape2")) {
  install.packages("reshape2", repos = 'http://cran.wu.ac.at/')
}


library(reshape2)
##
## ------------------------------------------------------------------------
##
## load `RCurl`: Provides functions to compose general HTTP requests
if (!require("RCurl")) {
  install.packages("RCurl", repos = 'http://cran.wu.ac.at/')
}

library(RCurl)



options(WordpressLogin = c(yourUserName = 'vladborkus'),
        WordpressURL = 'yourWordPressURL')


opts_knit$set(upload.fun = imgur_upload, base.url = NULL)  # upload all images to imgur.com
opts_chunk$set(fig.width = 5, fig.height = 5, cache = TRUE)


postTitle = "Testpage from R resp. knitr"
fileName = "main.Rmd"
postID = 13409 # for my test post


postID <- knit2wp(
  input = fileName, 
  title = postTitle, 
  publish = FALSE,
  action = "newPost"
)




