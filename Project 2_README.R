# Code to Create README.md file

rmarkdown::render("craftt_Project2.Rmd", 
                   output_format = "github_document",
                   output_file = "README.md",
                   output_options = list(
                     toc = TRUE,
                     toc_depth = 2,
                     df_print = "tibble")
 )

