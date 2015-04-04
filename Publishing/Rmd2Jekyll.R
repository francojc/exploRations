#!/usr/bin/env Rscript

# ABOUT -------------------------------------------------------------------
# This script uses the `knitr`, `markdown` and `RWordPress` packages to
# prepare and upload an Rmd file to Wordpress.
# Based on: http://gtog.github.io/workflow/2013/06/12/rmarkdown-to-rbloggers/?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+FromGuinnessToGARCH+%28From+Guinness+to+GARCH%29
# JCF Dec 6, 2014

# USAGE -------------------------------------------------------------------
# Rmd2Jekyll.R <filename> "title"

# _Libraries --------------------------------------------------------------
suppressPackageStartupMessages(library("knitr"))

# _ Read in arguments -----------------------------------------------------
args <- commandArgs(TRUE) # retrieve command-line arguments '<filename> "title"'
names(args) <- c("document") # label the argument(s)
setwd(system("pwd", intern = TRUE)) # set to the current working directory
cat("Preparing to `knit` <", args["document"], 
    "> for Jekyll \n")

# _ knit function ---------------------------------------------------------

KnitPost <- function(input, base.url = my.jekyll.site) {
  opts_knit$set(base.url = base.url)
  fig.path <- paste0("images/", sub(".Rmd$", "", basename(input)), "/")
  opts_chunk$set(fig.path = fig.path)
  opts_chunk$set(fig.cap = "center")
  render_jekyll()
  knit(input, envir = parent.frame())
}

# KNIT --------------------------------------------------------------------

my.jekyll.site = c("http://francojc.github.io/")
KnitPost(args["document"])

cat(args["document"], "has been `knitted`! \n")