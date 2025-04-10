rmarkdown::render("baseball_win_percentage.Rmd", 
                  output_file = "README.md", 
                  output_format = "github_document",
                  output_options = list(toc=TRUE, toc_depth=1,
                                        number_sections=TRUE, df_print="default"))
