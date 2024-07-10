#### geospatial ###
##### IMPORT DONNEES ####
library(leaflet)
library(ggmap)
library(devtools)
library(sf)
library(htmlwidgets)

# IMPORT
dial<-read.csv2("dialyses.csv",numerals = c("lat","lon"))
str(dial)
acc<-read.csv2("accnt_oct_2019.csv",colClasses = c(finess="character",site="character",sejours="integer"))
str(acc)
sites<-read.csv("site_geoloc_finess.csv",colClasses = c(finess="character",site="character",lon="numeric",lat="numeric"))
str(sites) 
geocod<-fread("finess_idf_adress.geocoded.csv")

# sites de dialyse IDF avec marqueurs ####
m <- leaflet(dial) %>% addTiles() %>% 
  setView(lng =2.3, lat = 48.9, zoom = 12) %>% 
  addMarkers(lng = ~lon, lat = ~lat,label=~site)
m
# sauvergarde carte html
saveWidget(m, file="sites_dialyse_idf.html", selfcontained = TRUE)
#
# Centres de dialyse (public/privé) avec volume annuel d'activité en IDF ####
#
pub<-subset(dial,type=="prv")
prv<-subset(dial,type=="aphp")
#
n <- leaflet(dial) %>% addTiles() %>% 
  setView(lng =2.3, lat = 48.9, zoom = 12) %>%
  addCircles(lng = ~pub$lon, lat = ~pub$lat, weight = 1,
             radius = ~sqrt(pub$dial19)*5, popup = ~paste(pub$site, ":", pub$dial19,"seances"),
             color = "#0063AF", fillOpacity = 0.8)%>%
  addCircles(lng = ~prv$lon, lat = ~prv$lat, weight = 1,
             radius = ~sqrt(prv$dial19)*5, popup = ~paste(prv$site, ":", prv$dial19,"seances"),
             color = "#E5006D", fillOpacity = 0.8)%>%
  addLegend(
    position = "bottomleft",
    colors = c("#0063AF","#E5006D"),
    labels = c("privé","aphp"), opacity = 1,
    title = "Type centre")
n
saveWidget(n, file="activite_dialyse_pub_prv_idf.html", selfcontained = TRUE)
#
acnt<-merge(acc,geocod,by = "finess",all.x = T)
write.csv2(acnt,file = "acnt.csv")
acnt<-fread("acnt.csv")# reimport apres ajout de finess manquant
acnt$sejours<-as.numeric(acnt$sejours)
dial$dial18<-as.numeric(dial$dial18)
dial$dial19<-as.numeric(dial$dial19)
str(acnt)
#
#
save(acnt,file = "acnt.rda")
#
##### ajouter legende aux marqueurs, a la ligne ###
acnt$hover <- with(acnt, paste(site, '<br>', sejours, "accnt 2019oct"))
unique(acnt$site[1:29])
#
list_prive<-c("INSTFBKLEBER","CHRIVESDESEINE","FOCH","CASH","MATSTEFELICITE","CLSTETHERESE","CLLAMBERT","STJOSEPH","HPROUQUESLESBLUETS","CLDELAMUETTE","IMM","HAMERICAIN","CLCBERNARD","CLDELESTREE","MATDESLILAS")
#
# public - prive - aphp  ####
acnt$htype<-ifelse(acnt$site %in%list_prive,"prive","public")# attention, valable pour les >5 séjours !!!
ac2<-subset(acnt,htype=="prive")
ac1<-subset(acnt,htype=="public")
pub<-subset(dial,type=="prv")
prv<-subset(dial,type=="aphp")
#
#


# hopitaux - sejours, public vs prive couleurs differentes ####
#
# arrondissements de Paris avec limites
arp<-st_read(dsn = "arrondissements.shp", stringsAsFactors = F)
#
n <- leaflet(dial) %>% addTiles() %>% 
  setView(lng =2.3, lat = 48.9, zoom = 11) %>%
  addCircles(lng = ~pub$lon, lat = ~pub$lat, weight = 1,
             radius = ~sqrt(pub$dial19)*5, popup = ~paste(pub$site, ":", pub$dial19,"seances"),
             color = "#920000", fillOpacity = 0.8)%>%
  addCircles(lng = ~prv$lon, lat = ~prv$lat, weight = 1,
             radius = ~sqrt(prv$dial19)*5, popup = ~paste(prv$site, ":", prv$dial19,"seances"),
             color = "#a500a5", fillOpacity = 0.8)%>%
  addLegend(
               position = "bottomleft",
               colors = c("#920000","#a500a5"),
               labels = c("privé","aphp"), opacity = 1,
               title = "Type centre")

n
?addPolygons
#
pal<-colorFactor(c("navy","red","brown"),domain = c("ap","pb","pr"))
leaflet(hpt) %>% addTiles() %>%
  setView(lng =2.3, lat = 48.9, zoom = 11)%>% 
  addProviderTiles("Stamen.Toner", group = "Toner by Stamen")%>%
  addCircleMarkers(
    radius = ~sqrt(vol1718)/7,popup = ~paste(nomcourt, ":", vol1718,"sejours"),
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 0.9
  )%>% addLegend(
    position = "bottomleft",
    colors = c("navy","red","brown"),
    labels = c("aphp","public","privé"), opacity = 1,
    title = "Type hôpital"
  )
# fond google simple
m<-leaflet(hpt) %>% addTiles() %>%
  setView(lng =2.3, lat = 48.9, zoom = 11)%>% 
  addCircleMarkers(
    radius = ~sqrt(vol1718)/7,popup = ~paste(nomcourt, ":", vol1718,"sejours"),
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 0.9
  )%>% addLegend(
    position = "bottomleft",
    colors = c("navy","red","brown"),
    labels = c("aphp","public","privé"), opacity = 1,
    title = "Type hôpital"
  )
## LEGENDE ####
# a manual legend
leaflet(df) %>% addTiles() %>%
  setView(lng =39.12, lat = 51.5, zoom = 11)%>%
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(~lat, ~lng, color = ~size, group = "circles") %>%
  addLegend(
    position = "bottomleft",
    colors = c("#920000","#a500a5","#03F"),
    labels = c("public","privé","aphp"), opacity = 1,
    title = "Type hôpital"
  )
#
str(df)

##### COULEURS #####
modifier les couleurs selon public-prive-APHP? "#03F"-couleur bleue "#a500a5"-violette "#FAFCFA"-gris clair
# "#B3C4B3"-vert de gris "#920000"-rouge brique
#
# palettes de couleurs utilisables
pal <- colorBin("Greens", domain = 0:100)
pal(runif(10, 60, 100))
pal <- colorBin("Reds", domain = 0:100)
pal(runif(10, 60, 100))
pal <- colorBin("Blues", domain = 0:100)
pal(runif(10, 60, 100))
pal <- colorQuantile("YlOrRd", quakes$mag)
#
###### PUBLICATION DE CARTE #####
library(shiny)

app <- shinyApp(
  ui = fluidPage(leafletOutput('n'),height=1000),
  server = function(input, output) {
    map = leaflet(dial)%>%
      addCircles(lng = ~lon, lat = ~lat, weight = 1,
                 radius = ~sqrt(dial19) * 9, popup = ~paste(site, ":", dial19),
                 color = "#920000", fillOpacity = 0.8)
    output$n = renderLeaflet(map)
  }
)
if (interactive()) app
#
########### MINICHARTS avec exemple####
library(leaflet.minicharts)
data("eco2mixBalance")
bal <- eco2mixBalance
str(bal)
m <- leaflet(bal) %>% addTiles() %>% 
  setView(lng =2.3, lat = 48.9, zoom = 12)
m
m %>%
  addFlows(
    bal$lng0, bal$lat0, bal$lng1, bal$lat1,
    flow = bal$balance,
    time = bal$month
  )
# exemple simule avec idf ap-public-prive
m
# selection de flux>30
flux30<-subset(fluxsim,flux>30)
m %>%
  addFlows(
    fluxsim$lng0, fluxsim$lat0, fluxsim$lon, fluxsim$lat,
    flow = fluxsim$flux,
    time = fluxsim$annee,popup = popupArgs(labels ="Flux annuel"),maxThickness = 6
  )
# trop chargee, laisser > 30
m %>%
  addFlows(
    flux30$lng0, flux30$lat0, flux30$lon, flux30$lat,
    flow = flux30$flux,opacity = 0.6,color = "blue",
    time = flux30$annee,popup = popupArgs(labels ="Flux annuel"),maxThickness = 6
  )
# essayer couleurs differentes en fonction de publ/priv/ap?