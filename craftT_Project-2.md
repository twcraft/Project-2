Project 2
================
Tiffany Craft
2023-10-07

# Code to Create this Document

<!-- adding the code chunk to render the document but set it to not evaluate, and with all of the options set to create the table of contents, numbered sections, and data frames printed to match the original PDF -->

``` r
rmarkdown::render("craftt_R Markdown PDF.Rmd", 
              output_format = "pdf_document",
              output_options = list(
                toc = TRUE,
                toc_depth = 2,
                number_sections = TRUE,
                df_print = "tibble"
                )
              )
```
