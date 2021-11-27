library(rmarkdown)
output_file <- gsub(" ","_",gsub(':|- ', '_', Sys.time()),fixed=TRUE)
output_file <- paste0("data/report-",output_file)
rmarkdown::render(
    input         = "R/stock_summary.Rmd",
    output_format = "pdf_document",
    output_file   = output_file,
    output_dir    = "reports/",
    params        = list(
        symbols        = 'AAPL',
        show_code      = FALSE
    )
)
