#!/usr/bin/env Rscript

# ABOUT -------------------------------------------------------------------
# This script uses the `knitr`, `markdown` and `RWordPress` packages to
# prepare and upload an Rmd file to Wordpress.
# JCF Dec 6, 2014

# USAGE -------------------------------------------------------------------
# Rscript Rmd2wp.R <filename.Rmd> "title"

rm(list = ls())

# _Libraries --------------------------------------------------------------
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("markdown"))
suppressPackageStartupMessages(library("RWordPress"))

# _ Read in arguments -----------------------------------------------------
args <- commandArgs(TRUE) # retrieve command-line arguments <filename> "title"'
names(args) <- c("document", "title") # label the arguments
setwd(system("pwd", intern = TRUE)) # set to the current working directory
cat("Preparing to send <", args["document"], 
    "> to Wordpress with the title <", 
    args["title"], "> \n")

# _knitr ------------------------------------------------------------------
cat("---- Setting `knitr` options \n")
opts_knit$set(upload.fun = imgur_upload, base.url = NULL) # upload all images to imgur.com
opts_chunk$set(fig.width=5, 
               fig.height=5, 
               cache=TRUE)

cat("---- knitting:", args["document"], "\n")
cat(getwd(), "\n")
knit(args["document"], quiet = TRUE)
markdown.file <- gsub(pattern = "Rmd$", 
                      replacement = "md", 
                      x = args["document"])
html.file <- gsub(pattern = "md$", 
                  replacement = "_pub.html", 
                  x = markdown.file)

# _ get HTML fragment, create MD file -------------------------------------
markdownToHTML(file = markdown.file, 
               output = html.file, 
               fragment.only = TRUE) # removes 'yaml' information

# _upload to Wordpress ----------------------------------------------------
options(WordpressLogin = c(your.username = 'your.password'), 
        WordpressURL = 'https://your-wordpress-site.com/xmlrpc.php')

cat("---- Composing document to publish \n")
text = paste(readLines(html.file),
             collapse = "\n")
newPost(list(description = text, 
             title = args["title"]), 
        publish = FALSE)

cat("---- Published: ", args['title'], "! \n", sep = "")
