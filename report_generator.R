require(rmarkdown)

render('Report.Rmd',
       pdf_document(toc = TRUE,
                    toc_depth= 3,
                    number_sections = TRUE,
                    fig_width = 7,
                    fig_height = 4.33,
                    #citation_package = 'biblatex', 
                    pandoc_args = c('--bibliography', 'bibliography.bib',
                                    '--csl', 'ieee-with-url.csl')))
