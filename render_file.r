rmarkdown::render("D:/Fall2023/ST558/ST558Proj2/RC ST558 Project 2.rmd", 
                  output_file = "README.md",
                  output_format = "github_document",
                  output_options = list(
                    toc = TRUE, toc_depth = 2, number_sections = TRUE, df_print = "tibble")
      )
