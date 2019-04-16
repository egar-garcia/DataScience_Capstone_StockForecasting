require(rmarkdown)

render('Report.Rmd',
       pdf_document(toc = TRUE,
                    toc_depth= 3,
                    number_sections = TRUE,
                    fig_width = 7,
                    fig_height = 4.33))
