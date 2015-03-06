\donttest{
gmap(title = "NYC taxi pickups January 2013",
  lat = 40.74, lng = -73.95, zoom = 11,
  map_type = "roadmap", width = 1000, height = 800) %>%
  ly_hexbin(nyctaxihex, alpha = 0.5,
    palette = "Spectral10", trans = log, inv = exp)
}
