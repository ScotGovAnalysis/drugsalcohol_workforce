library(leaflet)
library(rgdal)
library(tmaptools)

spatial_data <- bind_rows(surveyrecipients, Fig1) %>% 
  pivot_wider(names_from = cat, values_from = total) %>% 
  replace(is.na(.), 0) %>% 
  mutate(prop = round(RESPONDENTS/OVERALL*100,1),
         HBName = str_replace(`Health Board`, "&", "and")) %>% 
  ungroup() %>% 
  select(HBName, prop)

###################################################################################
healthboards <- readOGR("C:/Users/u449921/Documents/Workforce/Survey work/NHS_healthboards_2019/SG_NHS_HealthBoards_2019.shp",
                        layer= 'SG_NHS_HealthBoards_2019',
                        GDAL1_integer64_policy = T) %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84"))

simplified_healthboards <- rmapshaper::ms_simplify(healthboards)

#joining
simplified_healthboards$prop <- spatial_data$prop[match(simplified_healthboards$HBName, spatial_data$HBName)]

#colour palette
pal <- colorNumeric(
  palette = "Blues",
  domain = simplified_healthboards$prop
)

export <- spatial_data %>% 
  leaflet() %>% 
  addProviderTiles('Esri.WorldShadedRelief') %>% 
  addPolygons(
    color = "#444444",
    data = simplified_healthboards,
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1.0,
    fillOpacity = 100,
    fillColor = ~pal(simplified_healthboards$prop),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = T)) %>% 
  addLegend("bottomright", 
            pal = pal,
            values = ~simplified_healthboards$prop,
            title = "Figure 1: Survey response <br/>rates by NHS Health Board",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1) 


mapview::mapshot(export, url = "ex1.html")
