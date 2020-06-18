# libraries
library(rvest)
library(jsonlite)
library(httr)
library(magrittr)
library(tidyverse)
library(dplyr)
library(plyr)
library(stringr)
library(data.table)
library(purrr)
library(httr)
library(assertthat)
library(shiny)
library(markdown)
library(DT)
library(shinythemes)

# new
library(StatMatch)

#######################

# get access token
# code taken from https://rayheberer.ai/archive/spotifyapi/
get_tokenv <- function(){
  clientID = "3a9bb802ae9d464a8eccacbfa65e7dd8"
  secret = "75abf174c2f44280b067403e885dff5f"
  
  response = POST(
    'https://accounts.spotify.com/api/token',
    accept_json(),
    authenticate(clientID, secret),
    body = list(grant_type = 'client_credentials'),
    encode = 'form'
  )
  
  token = content(response)$access_token
  authorization.header = paste0("Bearer ", token)
}

authorization.header = get_tokenv()

#######################

# load data
load("data/tracks.Rdata")

# variable we'll be working with
var <- c("popularity", "danceability", "energy", "acousticness", "instrumentalness", "liveness", 
         "valence", "tempo", "tempo_lvl")

# categories we'll be using
select_categories <- c("Pop", "Hip-Hop", "Country", "Rock", "Latin", "Jazz", "Dance/Electronic", "R&B",
                       "Indie", "Folk & Acoustic", "Party", "Chill", "Classical", "Soul", "Metal", "Reggae",
                       "Blues", "Punk", "Funk", "Anime", "Christian", "Romance", "K-Pop", "Arab", "Desi", "Afro")

#######################

# takes list input and returns vals and weights
input_creator <- function(vals){
  
  # input checks
  assert_that(length(vals) == 8)
  
  # removing NULLs and putting weights 0 for NULLs
  tmp <- unlist( map(vals, is.null) )
  wt <- rep(1, 8)
  wt[tmp] <- 0
  vals <- as.numeric( unlist( map(vals, function(v){
    if(is.null(v))
      return(0)
    else
      return(v)
  }) ) )
  
  # tempo mapping
  if(vals[8] != 0){
    tmp_matches <- c(60, 100, 140, 180)
    tempo <- tmp_matches[vals[8]]  # vals[8]
    vals <- c(vals[1:7], tempo, vals[8])
  }
  else{
    vals <- c(vals[1:7], 0, 1)
  }
  
  ti <- as.data.frame( as.list(vals) )
  colnames(ti) <- var
  ti$tempo_lvl <- factor(ti$tempo_lvl, levels = c("1", "2", "3", "4"))
  list(input_val = ti, weights = wt)
}

#######################

# distance calculator(and returns top 10 results)
tracks_find <- function(inp, wt, pop_flag = T, inds = 1:nrow(tracks_all)){   # wt is 8 dimensional
  
  # weight adjustment
  # weights (0 or 1), 8 dimensional - tempo uses two vars but has same weight for each
  if(pop_flag)                # scale popularity weight wrt to other features
    wt[1] <- sum(wt[-1])/6  
  if(sum(wt) == 0) wt[1] <- 0.0001    # if no weights give results by popularity
  wt <- wt/sum(wt)            # normalize
  tempo_w <- wt[8]            # tempo weight
  wts <- wt[-8]/sum(wt[-8])   # adjust weight without tempo
  if(any(is.na(wts))) wts <- rep(0, 7)    # if all 0 weights(except tempo)
  
  # remove duplicate tracks
  inds <- inds[!duplicated(tracks_all$id[inds])]
  
  # tmp df
  df2 <- tracks_all[inds,var]
  
  # calculate tempo distance
  tempo_cont <- gower.dist(inp$tempo, df2$tempo)
  tempo_fct <- gower.dist(inp$tempo_lvl, df2$tempo_lvl)
  tempo_dist <- tempo_cont*tempo_fct
  
  # calculate rest of the distances
  test_res <- gower.dist(inp[1:7], df2[1:7], rngs = c(100,1,1,1,1,1,1), var.weights = wts)
  test_res <- c(test_res)
  
  # calculate total dist
  test_res <- tempo_w*tempo_dist + (1 - tempo_w)*test_res
  
  # display result, top 5
  r_inds <- inds[ order(test_res)[1:10] ]
}

#######################

# get song details(album, artist), can scrape earlier, but no point in saving everything
results_final <- function(results){
  
  df_final <- map(results$id, function(ids){
    
    # get track data
    track_dets <- content(GET(url = sprintf("https://api.spotify.com/v1/tracks/%s", ids), 
                              config = add_headers(authorization = get_tokenv())))
    
    # required results
    name <- track_dets$name
    album <- track_dets$album$name
    artists <- paste( unlist( map(track_dets$artists, function(art) art$name ) ), collapse = ", ")
    ext_url <- track_dets$external_urls$spotify
    
    # as dataframe
    tdf <- data.frame(list(name, album, artists, ext_url), stringsAsFactors = F)
    colnames(tdf) <- c("Song", "Album", "Artists", "Link")
    tdf
  })
  
  df_final <- data.table::rbindlist(df_final)   # join the dataframes
  df_final <- as.data.frame(df_final, stringsAsFactors = F)
}

#######################

# call the distance function for each cat and create dataframe of results
# default popularity is used and all categories will be used
get_results <- function(inp, wt, pop_flag = T, categories = NULL){
  
  ti <- inp
  
  if(is.null(categories)){
    # call for all categories
    inds_result <- tracks_find(ti, wt, pop_flag)
  }
  else{
    # indices for required categories
    i <- which(tracks_all$category %in% categories)
    
    # call for each category
    inds_result <- tracks_find(ti, wt, pop_flag, inds = i) 
  }
  
  # result in dataframe
  a <- results_final(tracks_all[inds_result, c("id", "category")])  # may use category later?
}

