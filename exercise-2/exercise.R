### Exercise 2 ###

# Load the httr and jsonlite libraries for accessing data
#install.packages('httr')
#install.packages('jsonlite')

library(httr)
library(jsonlite)

# Create a `base.uri` variable that holds the base uri. You wil then paste endpoints to this base.
base.uri <- 'https://api.spotify.com'


## As you may have noticed, it often takes multiple queries to retrieve the desired information.
## This is a perfect situation in which writing a function will allow you to better structure your
## code, and give a name to a repeated task!


# Define a function `TopTrackSearch` that takes in an artist name as an argument,
# and returns the top 10 tracks (in the US) by that artist
TopTrackSearch <- function(my.name) {
  resource <- "/v1/search"
  uri <- paste0(base.uri, resource)
  query.params <- list(q = my.name, type = "artist")
  response <- GET(uri, query = query.params)
  body <- content(response, "text")
  artist.info <- fromJSON(body)
  
  artist.id <- artist.info$artists$items$id[1]
  resource <- paste0('/v1/artists/', artist.id, '/top-tracks')
  uri <- paste0(base.uri, resource)
  query.params <- list(country = 'US')
  response <- GET(uri, query = query.params)
  body <- content(response, "text")
  top.tracks <- fromJSON(body)
  
  return(top.tracks$tracks)
}



# What are the top 10 tracks by Beyonce?
top.tracks <- TopTrackSearch('Beyonce')

# Use the `flatten` function to flatten the data.frame -- note what differs!
top.tracks <- flatten(top.tracks)

# Use the `save()` function to save the flattened data frame to a file `beyonce.Rdata`
save(top.tracks, file="beyonce.Rdata")

# Use your `dplyr` functions to get the number of the songs that appear on each album
install.packages('dplyr')
library(dplyr)
num.album <- top.tracks %>% group_by(album.name) %>% summarise(n = n()) %>% arrange(-n)


### Bonus ###
# Write a function that allows you to specify a search type (artist, album, etc.), and a string,
# that returns the album/artist/etc. page of interest
SpotifySearch <- function(my.type, s) {
  resource <- '/v1/search'
  uri <- paste0(base.uri, resource)
  query.params <- list(q = s, type = my.type)
  response <- GET(uri, query = query.params)
  return(fromJSON(content(response, "text"))$albums)
}

# Search albums with the word "Sermon"
SpotifySearch('album', 'Sermon')
