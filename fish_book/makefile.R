#' File containing the code to creat html and pdf versions
#' of the book.

#' Gitbook for html viewing

#' May want to rm prior html files
#' Can first look at what will be deleted (with FALSE, below)
bookdown::clean_book(clean = getOption("bookdown.clean_book", FALSE))

#' And to delete these files 
bookdown::clean_book(clean = getOption("bookdown.clean_book", TRUE))

#' This will generate the book
bookdown::render_book("index.Rmd", "bookdown::gitbook")

#' To open the html (gitbook) version of the book.
browseURL("_book/index.html")

#' rename previous tex file (so can diff the two to see recent 
#' changes)

#'rmarkdown::render("Subchapters/_StatisticalPerspective.Rmd", "html_document")
#'rmarkdown::render("Subchapters/_MovementstoDistributions.Rmd", "html_document")
#'rmarkdown::render("02-SettingTheStage.Rmd", "pdf_document")
#'rmarkdown::render("02-SettingTheStage.Rmd", "html_document")

#' pdf
bookdown::render_book("index.Rmd", bookdown::pdf_book())


#' Eventually, may want to look at ways to keep the tex file to do 
#' a word-like track changes.  The code below may help.
bookdown::render_book("index.Rmd", bookdown::pdf_book(keep_tex=TRUE))


#' to word - note: this could also be used with "compare documents"
bookdown::render_book("index.Rmd", bookdown::word_document2())


