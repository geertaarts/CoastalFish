#' File containing the code to creat html and pdf versions
#' of the book.

#' To be able to track changes, use this workflow using 
#' library(latexdiffr) (you will need latexdiff installed, not
#' sure if that comes w/ Miketex by default).
#' 
#' 1. Copy the current version of the chapter (.Rmd file) to OldFilesForDiff folder
#' 2. Make changes to the .Rmd file of interest 
#' 3. Run the code below (modifying the names of the .Rmd files
#' as appropriate):

# Using latexdiffr
# Change file name, below, as appropriate (note, no .Rmd)
#chapter_filenm <- "02-SettingTheStage" # NOTE NO .RMD
chapter_filenm <- "07-LinkingMovementHabitatSelection" # NOTE NO .RMD
modified_filenm<- paste0(chapter_filenm, "OLD")
file.copy(from=here("OldFilesForDiff", paste0(chapter_filenm, ".Rmd")), to=here("",paste0(modified_filenm, ".Rmd")))
latexdiff(paste0(modified_filenm, ".Rmd"),paste0(chapter_filenm, ".Rmd")) 

#' Note, tabular output seems to break latexdiff.  One PAINFUL
#' way forward is to keep the diff.tex file (below) 
#latexdiff(paste0(modified_filenm, ".Rmd"),paste0(chapter_filenm, ".Rmd"), clean=FALSE) 
#' Then, open it and the log and try to get something to work
#Delete unnecessaryfiles
unlink(paste0(chapter_filenm,"_files"), recursive = TRUE)
unlink(paste0(modified_filenm,"_files"), recursive = TRUE)
file.remove(paste0(modified_filenm, ".Rmd"))


#' Gitbook for html viewing

#' May want to rm prior html files
#' Can first look at what will be deleted (with FALSE, below)
clean_book(clean = getOption("bookdown.clean_book", FALSE))

#' And to delete these files 
bookdown::clean_book(clean = getOption("bookdown.clean_book", TRUE))

#' This will generate the book
bookdown::render_book("index.Rmd", "bookdown::gitbook")

#' To open the html (gitbook) version of the book.
browseURL("_book/index.html")

#' rename previous tex file (so can diff the two to see recent 
#' changes)

rmarkdown::render("Subchapters/_StatisticalPerspective.Rmd", "html_document")
rmarkdown::render("Subchapters/_MovementstoDistributions.Rmd", "html_document")
rmarkdown::render("02-SettingTheStage.Rmd", "pdf_document")
rmarkdown::render("02-SettingTheStage.Rmd", "html_document")

#' pdf
bookdown::render_book("index.Rmd", bookdown::pdf_book())


#' Eventually, may want to look at ways to keep the tex file to do 
#' a word-like track changes.  The code below may help.
bookdown::render_book("index.Rmd", bookdown::pdf_book(keep_tex=TRUE))


#' to word - note: this could also be used with "compare documents"
bookdown::render_book("index.Rmd", bookdown::word_document2())

# Now, latexdiff
library(latexdiffr)
library(here)

#file.copy( from=here("OldFilesForDiff","HSF-Book.tex"), to=here("OldFilesForDiff","HSF-BookOLD.tex"))
#file.copy( from=here("_book", "HSF-Book.tex"), to=here("OldFilesForDiff","HSF-Book.tex"))
#latexdiff("OldFilesForDiff/HSF-Book.tex", "OldFilesForDiff/HSF-Book.tex")

# Using latexdiffr
file.copy(from=here("OldFilesForDiff", "02-SettingTheStage.Rmd"), to=here("","02-SettingTheStageOLD.Rmd"))
modified_file <- "02-SettingTheStage.Rmd"
reference_file <- "02-SettingTheStageOld.Rmd"
latexdiff(reference_file, modified_file)

#Now delete unnecessary files
unlink("02-SettingTheStageOld_files", recursive = TRUE)
unlink("02-SettingTheStage_files", recursive = TRUE)

# Using reviewer
library(reviewer)
result <- diff_rmd(modified_file, reference_file, show="rendered")
browseURL("C:/Users/jfieberg/AppData/Local/Temp/RtmpYRvOiF/file30d029f5f24.html")

# This doesn't work for some reason....would be really nice if it did!
git_latexdiff("02-SettingTheStage.Rmd", "HEAD~1") 
