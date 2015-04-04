
# ABOUT -------------------------------------------------------------------
# This script processes State of the Union speeches (1947-present) providing
# a word frequency and keyword analysis
# Includes visualizations
# JCF February 28, 2015

# SETUP -------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Libraries
library("stringr")
library("tm")
library("dplyr")
library("ggplot2")
library("wordcloud")

# Options
theme_set(theme_light())

# READ --------------------------------------------------------------------

# _ Read ------------------------------------------------------------------

read.corpus <- function(directory, pattern = "", to.lower = TRUE) {
  corpus <- DirSource(directory = directory, pattern = pattern) %>%
    VCorpus
  if(to.lower == TRUE) corpus <- tm_map(corpus, content_transformer(tolower))
  return(corpus)
}

rep.corpus.raw <- read.corpus(directory = "sotu/texts/", 
                          pattern = "republican", 
                          to.lower = TRUE)

dem.corpus.raw <- read.corpus(directory = "sotu/texts/", 
                          pattern = "democrat", 
                          to.lower = TRUE)

# _ Explore ---------------------------------------------------------------

# _ Token frequency -------------------------------------------------------

create.wordlist <- function(corpus, create_df = FALSE) {
  wordlist <- corpus %>% 
    DocumentTermMatrix() %>% 
    as.matrix() %>% 
    colSums() %>% 
    sort(decreasing = TRUE)
  if(create_df == TRUE) wordlist <- wordlist %>%
    data.frame(words = names(.), freq = ., row.names = NULL)
  return(wordlist)
}

rep.wordlist <- create.wordlist(corpus = rep.corpus.raw, create_df = TRUE)
dem.wordlist <- create.wordlist(corpus = dem.corpus.raw, create_df = TRUE)

# Get a quick view of the top 50 words
wordcloud(words = rep.wordlist$words, freq = rep.wordlist$freq, min.freq = 20, max.words = 25, scale = c(3, .1))
wordcloud(words = dem.wordlist$words, freq = dem.wordlist$freq, min.freq = 20, max.words = 25, scale = c(3,.1))

rep.wordlist$words %>%
  grep(pattern = "[\\(\\[\\]+", x = ., value = TRUE)

# _ Clean -----------------------------------------------------------------

clean.corpus <- function(corpus, my.stopwords = "") {
  # Clean a `tm` corpus #
  # Substitution function for to simplify `tm_map`
  my.sub <- content_transformer( 
    function(x, pattern, replacement) 
      gsub(pattern, replacement, x))
  
  corpus %>%
    tm_map(content_transformer(tolower)) %>% # lowercase
    tm_map(my.sub, "\\[\\w+\\]", "") %>% # ex.`[applause]` 
    tm_map(my.sub, "\\(\\w+\\)", "") %>% # ex. `(tip)`
    tm_map(removeWords, stopwords("english")) %>% # if file specified
    tm_map(removePunctuation, preserve_intra_word_dashes = TRUE) %>% # keep `low-income`
    tm_map(removeNumbers) %>% # numerals
    tm_map(stripWhitespace) # clean up any trailing whitespace
}

rep.corpus <- clean.corpus(corpus = rep.corpus.raw)
dem.corpus <- clean.corpus(corpus = dem.corpus.raw)

# _ Create word frequency lists -------------------------------------------

rep.wordlist_df <- create.wordlist(corpus = rep.corpus, create.df = TRUE)
dem.wordlist_df <- create.wordlist(corpus = dem.corpus, create.df = TRUE)

# _ Visualize word frequencies --------------------------------------------

# Get a quick view of the top 50 words
wordcloud(words = rep.wordlist_df$words, freq = rep.wordlist_df$freq, min.freq = 20, max.words = 50, scale = c(3, .1))
wordcloud(words = dem.wordlist_df$words, freq = dem.wordlist_df$freq, min.freq = 20, max.words = 50, scale = c(3, .1))

p <- ggplot(data = rep.wordlist_df[1:25, ], aes(x = reorder(words, freq), y = freq))
p1 <- p + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  ggtitle("Republican most frequent words") + ylab("Token frequency") + xlab("Words")
p1

p <- ggplot(data = dem.wordlist_df[1:25, ], aes(x = reorder(words, freq), y = freq))
p2 <- p + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  ggtitle("Democrat most frequent words") + ylab("Token frequency") + xlab("Words")
p2

# KEYWORD ANALYSIS --------------------------------------------------------

# Relative Frequency Ratio (RFR) ------------------------------------------

rfr.calc <- function(wordlist.a, wordlist.b, create_df = FALSE) {
  # Calculate Relative Frequency Ratio #
  # Words in b, not in a
  wordlist.a[setdiff(names(wordlist.b), names(wordlist.a))] <- 0 # Fill missing words
  # Words in a, not in b
  wordlist.b[setdiff(names(wordlist.a), names(wordlist.b))] <- 0 # 
  
  vocabulary <- wordlist.a %>% names # get complete vocabulary 
  
  a.ratios <- wordlist.a/sum(wordlist.a) # ratios for wordlist a
  b.ratios <- wordlist.b/sum(wordlist.b) # ratios for wordlist b
  
  # wordlist a ratio/ wordlist b ratio (smoothing for 0 values)
  rfr <- log((a.ratios[vocabulary] + 1) / (b.ratios[vocabulary] + 1))
  if(create_df == TRUE) rfr <- rfr %>%
    data.frame(Words = names(.), 
               Scores = ., 
               row.names = NULL, 
               stringsAsFactors = FALSE) %>%
    arrange(Scores)
  return(rfr)
}

rep.wordlist <- create.wordlist(corpus = rep.corpus)
dem.wordlist <- create.wordlist(corpus = dem.corpus)

rfr_df <- rfr.calc(wordlist.a = rep.wordlist, 
                   wordlist.b = dem.wordlist, 
                   create_df = TRUE)

# Get highest and lowest scores (i.e. Republican/ Democrat)
rep.dem_df <- rbind(head(rfr_df, 50), tail(rfr_df, 50))
rep.dem_df$Party <- ifelse(test = (rep.dem_df$Scores > 0), 
                           yes = "Republicans", 
                           no = "Democrats")

# dem.rep.df$rep.freq <- rep.wordlist[dem.rep.df$words]
# dem.rep.df$dem.freq <- dem.wordlist[dem.rep.df$words]
# dem.rep.df$diff <- abs((dem.rep.df$rep.freq - dem.rep.df$dem.freq)/(dem.rep.df$rep.freq + dem.rep.df$dem.freq))*100


# _ Visualize RFR scores --------------------------------------------------

ggplot(rep.dem_df, aes(x = reorder(Words, Scores), y = Scores, color = Party)) + 
  geom_point() +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab("Terms") + ylab("Relative Frequency Ratio") + ggtitle("Top 50 most indicative terms")

# __ Concordances ---------------------------------------------------------

concordance <- function(texts, pattern, margin = 30, n = 0) {
  # A script to create a concordance listing for character vectors #
  pattern <- tolower(pattern) # lowercase
  concordance <- lapply(tolower(texts), function(i) { # loop over lowercased texts
    str_locate_all(string = i, # find all indices for pattern matches
                   pattern = pattern) %>%
      sapply(function(j) { # loop over index pairs (start/ end)
        str_sub(string = i, 
                start = as.integer(j[, 'start'])-margin, # extract the pattern
                end = as.integer(j[, 'end'])+margin) %>% # with margins
          str_replace_all(pattern = pattern, # replace pattern with padded pattern
                          replacement = str_pad(string = pattern, 
                                                width = nchar(pattern)+4, 
                                                side = "both",
                                                pad = " "))
      }) %>%
      unlist # clean up output
  }) %>% 
    unlist %>% # clean up output
    as.vector  # clean up output
  if(n > 0) concordance <- concordance[1:n]
  return(concordance)
}

concordance(texts = content(rep.corpus.raw), pattern = "federal", margin = 35, n = 5)
concordance(texts = content(dem.corpus.raw), pattern = "year", margin = 35, n = 100)


# EXTRA -------------------------------------------------------------------

# Read texts ------------------------------------------------------------

read.text <- function(directory, pattern, to.lower = TRUE) {
  text <- list.files(path = directory, 
                     full.names = TRUE, 
                     pattern = pattern) %>%
    lapply(function(x) scan(x, what = character(), 
                            sep = "\n", 
                            quiet = TRUE)) %>%
    do.call(c, args = .)
  if(to.lower == TRUE) text <- tolower(text)
  return(text)
}

rep.text <- read.text(directory = "sotu/texts/", pattern = "republican")
dem.text <- read.text(directory = "sotu/texts/", pattern = "democrat")


# Tokens ------------------------------------------------------------------

whitespace.tokens <- function(texts) {
  strsplit(texts, split = " ") %>% 
    unlist
}

rep.token.freq <- whitespace.tokens(rep.text) %>%
  table %>%
  sort
dem.token.freq <- whitespace.tokens(dem.text) %>%
  table %>%
  sort

dem.token.freq[dem.token.freq > 20] %>% head(100)


# Log-likelihood scores ---------------------------------------------------

# Expected frequency values (ExpFreq): predicted distribution of the null hypothesis
# c.f. Rayson & Garside (2000) Comparing Corpora using Frequency Profiling
# formula: ExpFreq1 = freq_total1*(word_freq1+word_freq2)/(freq_total1+freq_total2)
# ExpFreq2 = freq_total2*(word_freq2+word_freq1)/(freq_total2+freq_total1)
# R: same as above
# 
# 
# Log-likelihood score (LL): measure of difference between two collections (based on chi-square, expected values needed (ExpFreq) for each word, from each collection)
# c.f. Rayson & Garside (2000) Comparing Corpora using Frequency Profiling
# formula: 2*((word_freq1*log(word_freq1/ExpFreq1))+(word_freq2*log(word_freq2/ExpFreq2)))
# R: (same as above)

word <- "terrorist"
corpus.a <- rep.wordlist
corpus.b <- dem.wordlist

ll.calc <- function(corpus.a, corpus.b, vocabulary) {
  freq.total.a <- sum(corpus.a + length(corpus.a)) # corpus.a total w/ smoothing
  freq.total.b <- sum(corpus.b + length(corpus.a)) # corpus.b total w/ smoothing
  # Fill missing words
  corpus.a[setdiff(names(corpus.b), names(corpus.a))] <- 0 # words in b, not in a
  corpus.b[setdiff(names(corpus.a), names(corpus.b))] <- 0 # words in a, not in b
  # Expected frequencies
  exp.freq.a = freq.total.a * 
    (corpus.a[vocabulary] + corpus.b[vocabulary]) / 
    (freq.total.a + freq.total.b)
  exp.freq.b = freq.total.b * 
    (corpus.b[vocabulary] + corpus.a[vocabulary]) /
    (freq.total.b + freq.total.a)
  # Log-likelihood
  (score <- 2 * 
     ((corpus.a[vocabulary] * log(corpus.a[vocabulary] + 1 / exp.freq.a)) + 
        (corpus.b[vocabulary] * log(corpus.b[vocabulary] + 1 / exp.freq.b))
     )
  )
}

vocabulary <- unique(c(names(rep.wordlist), names(dem.wordlist)))

ll <- ll.calc(corpus.a = rep.wordlist,
              corpus.b = dem.wordlist,
              vocabulary = vocabulary)

ll.df <- ll %>%
  data.frame(words = names(.), scores = ., row.names = NULL, stringsAsFactors = FALSE) %>%
  arrange(scores)

