get_rec = function(genre){
  recommendation = GET(paste0('https://api.spotify.com/v1/recommendations?limit=3&seed_genres=',genre),
                       query = list(access_token = token)) %>% content %>% .$tracks
  # track
  track = map_df(recommendation, `[`, c('name','uri')) %>% 
    `colnames<-`(c('track','track.uri'))
  # album
  album = map(recommendation,`[[`,'album')
  album.name = album %>% map_df(.,`[`,c('name','uri')) %>% 
    `colnames<-`(c('album','album.uri'))
  album.image = album %>% map(.,`[[`,'images') %>% map_df(.,`[[`,2) %>% 
    `colnames<-`(c('image.height','image.url','image.width'))
  # artist
  artist = map(recommendation,`[[`,'artists') %>% 
    map(.,`[[`,1) %>%
    map_df(.,`[`,c('name','uri')) %>% 
    `colnames<-`(c('artist','artist.uri'))
  rec.df = cbind(track, album.name, album.image, artist)
}
