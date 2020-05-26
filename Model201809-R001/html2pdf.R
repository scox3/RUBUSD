
library(purrr)
library(stringr)

doc_list <-list.files(pattern=".*\\.html")

convert_html2pdf <-function(x) {
  system(paste0("\"C:/Program Files/wkhtmltopdf/bin/wkhtmltopdf.exe\" --javascript-delay 1 ", x, " ",
                           str_remove(x, "\\.html"), ".pdf"))
}

doc_list %>% convert_html2pdf
