
wd = "/home/sitian/Dropbox/Agroimpacts/BULC_LULC/figures/"
fp <- paste0(wd,'Table2.pdf')
# This is the validation points stats
pdftools::pdf_convert(fp, format = "png", pages = NULL, 
                      filenames = paste0(wd,"Table2.png"), 
                      dpi = 600, 
            opw = "", upw = "", verbose = TRUE)

# This is the slope acc.
fp <- paste0(wd,'Table3.pdf')
pdftools::pdf_convert(fp, format = "png", pages = NULL, 
                      filenames = paste0(wd,"Table3.png"), 
                      dpi = 600, 
                      opw = "", upw = "", verbose = TRUE)
