# Code to Create README.md file

rmarkdown::render("craftt_Project 2.Rmd", 
                   output_format = "github_document",
                   output_file = "README.md",
                   output_options = list(
                     toc = TRUE)
 )
