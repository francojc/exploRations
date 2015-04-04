
# ABOUT -------------------------------------------------------------------
# This script will authenticate your Twitter developer API application, 
# download tweets during a specific time period, write the data to disk,
# read the downloaded data, download a spatial object and use this object 
# as a mask for the tweet geo-coordinates, resulting in region-specific tweets
# December 2014
# Jerid Francom

# SETUP -------------------------------------------------------------------

# Clean workspace
rm(list = ls())
# Load and/or install packages
packages <- c("ROAuth", "twitteR", "streamR", "plyr", "data.table", "plyr")
packages <- lapply(packages, function(x) {
  if(!require(package = x, character.only=TRUE, quietly = TRUE)) {
    install.packages(pkgs = x, dep = TRUE, repos="http://cran.rstudio.com", quiet = TRUE)
    if(!library(package = x, character.only=TRUE, quietly=TRUE)) stop("Package not found!")
  }
})
# Set working directory
setwd("./")

# API AUTH ----------------------------------------------------------------

# _ authentication atributes ----------------------------------------------

api.key <- `your.api.key` # your consumer key
api.secret <- `your.api.secret` # your consumer secret
request.url <- "https://api.twitter.com/oauth/request_token"
access.url <- "https://api.twitter.com/oauth/access_token"
authorize.url <- "https://api.twitter.com/oauth/authorize"
oauth.file <- "myoauth.RData"

# _ authentication --------------------------------------------------------

if (!file.exists(oauth.file)) {
  # RCurl options
  options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", 
                                                   package = "RCurl"), 
                              ssl.verifypeer = TRUE))
  # Authentication frame
  my.oauth <- OAuthFactory$new(consumerKey = api.key, 
                               consumerSecret = api.secret, 
                               requestURL = request.url, 
                               accessURL= access.url,
                               authURL = authorize.url)
  # Handshake
  my.oauth$handshake()
  # Save credentials to disk
  save(my.oauth, file = oauth.file) 
}
# Load credentials file from disk
load(file = oauth.file)

# STREAM TWEETS -----------------------------------------------------------

# Check auth status
registerTwitterOAuth(my.oauth)
# Stream tweets
world.tweets <- filterStream(file="", # redirect to the console
                             locations = c(-180,-90,180,90), # geo-tweets
                             timeout = 60, # open stream for '60' secs
                             oauth = my.oauth) # use my credentials

# WRITE TO DISK -----------------------------------------------------------

# Convert to data.frame
world.tweets <- parseTweets(world.tweets)
# Select relevant columns
world.tweets <- world.tweets[, c("lang", "lat", "lon", "text")]
# Remove extra line breaks
world.tweets$text <- gsub("\\n+", "", world.tweets$text) 
# Remove spurious coordinates
world.tweets <- subset(world.tweets, 
                       (lat <= 90  & lon <= 180  & lat >= -90 & lon >= -180)) 
# Write data to disk
write.table(x = world.tweets, file = "worldtweets.tsv", 
            sep = ",", row.names = FALSE, fileEncoding = "utf8", 
            quote = TRUE, na = "NA")
# Clean up workspace
rm(list = ls())

# Clip the coordinates ----------------------------------------------------

# _ load tweet data -------------------------------------------------------

world.tweets <- fread(input = "worldtweets.tsv", sep = ",", 
                      header = TRUE, data.table = FALSE)

# _ Visualize tweet distribution ------------------------------------------

world.map <- map_data(map = "world") # get the world map
world.map <- subset(world.map, subset = region != "Antarctica") # remove this region
p <- ggplot(world.map, aes(x = long, y = lat, group = group)) + 
  geom_path() # base plot

p + geom_point(data = world.tweets, # plot tweet origin points
               aes(x = lon, y = lat, color = lang, group = 1), 
               alpha = 1/2) + theme(legend.position = "none")

# _ Load/ download spatial object -----------------------------------------

url <- "http://biogeo.ucdavis.edu/data/gadm2/R/USA_adm0.RData"
file <- basename(url) # gets the file's name
if (!file.exists(file)) { # If the `file` hasn't been downloaded, do so now
  download.file(url, file)
}
load(file = file) # Now load the `file` from disk


# _ project spatial coords to tweets --------------------------------------

coords <- world.tweets[, c("lon", "lat")] # extract/ reorder `lon/lat`
library(sp)
coordinates(coords) <- c("lon", "lat") # create a SpatialPoints object
proj4string(coords) <- proj4string(gadm) # add `gadm` projection to `coords`


# _ Clip ------------------------------------------------------------------

system.time(usa.coords <- coords[gadm, ]) # filter tweets
usa.tweets <- as.data.frame(usa.coords@coords) # extract coordinates

# _ Finalize clipped tweets -----------------------------------------------

usa.tweets <- join(usa.tweets, world.tweets)

# _ Visualize the new tweets ----------------------------------------------

p + geom_point(data = usa.tweets, # plot tweet origin points
               aes(x = lon, y = lat, color = lang, group = 1), 
               alpha = 1/2) + theme(legend.position = "none")
