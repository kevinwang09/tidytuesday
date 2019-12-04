# years = 2007:2018
# urls = paste0(
#   "https://www.uac.edu.au/assets/documents/scaling-reports/Scaling-Report-",
#   years, "-NSW-HSC.pdf")
# 
# purrr::map2(.x = years,
#             .y = urls,
#             .f = ~ download.file(url = .y, destfile = paste0("data/", .x, ".pdf")))



# library(pdftools)
# txt <- pdf_text("data/2007.pdf")
# pdf_data(txt[44:48])
