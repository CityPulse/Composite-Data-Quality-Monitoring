

leaflet()%>%
addProviderTiles("Stamen.TonerLite",
                 options = providerTileOptions(noWrap = TRUE)
) %>%
  addMarkers(data=simple_datasets[[1]])
