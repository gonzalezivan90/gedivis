library(bit)
library(digest)
library(dplyr)
library(ggplot2)
library(highcharter)
library(htmlwidgets)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(knitr)
library(rgl)
library(magrittr)
library(mongolite)
library(raster)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(rmarkdown)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)


# setwd('C:/GoogleDrive/IG/server_IG/gedivis')
# setwd('N:/Mi unidad/IG/server_IG/gedivis')
# print(paste('WD: ', getwd()))

if ( identical ( unname(Sys.info()[c("sysname", 'nodename')]), c("Windows", 'HP-Z400')) ){
  setwd('N:/Mi unidad/IG/server_IG/gedivis')
}

# Loads  ---------------
outDir <- ifelse( Sys.info()["sysname"] == "Linux",  '/home/shiny/tmpR/', 'C:/temp/Rtmp/'); dir.create(outDir)
rdatPath <- ifelse( Sys.info()["sysname"] == "Linux",  '/home/vmuser/gedivis/', getwd())

load(paste0(rdatPath, '/uEcoDf.RData')) # uEcoDf # 40872 bytes
load(paste0(rdatPath, '/regionsSimpl.RData')) # regSim # 3724896 bytes


highNull <- NULL
highEmpty <- highchart() %>%  
  hc_title(text = "Empty ecosystem. Please select another one"
           #, margin = 20, align = "left", style = list(color = "black", useHTML = TRUE)
  ) %>% hc_exporting(enabled = TRUE) 

ecoID <<- 0
suppN11 <- (0:10)*10
suppN21 <- c((0:9)*10, 90+(1:10))
regID <<- 0
shapesPt <<- c(16, 17) # base R plotting symbols (http://www.statmethods.net/advgraphs/parameters.html)


bins <- c('100', '200', '500', '1000', '2000', '5000', '10000', '20000', '50000', '100000', '200000', '500000')
bins <- c('100', '200', '500', '1000', '2000', '5000', '10000', '20000', '50000', '100000')
bin_selected <<- '1000'
bin_selected_start <<- '1000'
gediType <<- '2a'

load(paste0(rdatPath, '/umap_', gediType, '/umap_', bin_selected,'.RData')) # "xyDf" "st"
colnames(xyDf) <- gsub('_a1', '', colnames(xyDf))
xyDf$sortID <- 1:nrow(xyDf)
xyDfEco <- strsplit(xyDf$ecoID, ' - ')

xyDfEco2 <- do.call(rbind, lapply(xyDfEco, function(x){
  if(length(x) == 3){
    c(paste0(x[1], '--', x[2]),  x[3])
  } else if (length(x) == 2){
    x
  }
}))

xyDf$biom <- xyDfEco2[, 1]
xyDf$eco <- xyDfEco2[, 2]

xyDf <<- xyDf
xyDfC <<- colnames(xyDf)
xyDfClass <<- sapply(xyDf, class)
xyDfCN <<- xyDfC[xyDfClass == 'numeric'][-1]
xyDfCNr <<- c(xyDfCN, paste0(xyDfCN, ' - rev'))

## 3d RGL
hxy <- data.frame(x = 1, y = 1, h = 1)

## Raster
plotParams <- ''
fagua30m <- 'Fagua_et_al_2019_Byte_compDEF.tif'
fagua1km <- 'Fagua_et_al_2019_Byte_1km.tif'
gediRDatPath <- 'gedi2019_id_5km'
gediRid5KM <- 'ID_sampled_5km_gedi.tif'


# ## Mongo
# mongoUrl <- "mongodb://gedidb:gedidb123@localhost:27017/gedidb" #<-admin here is the mongodb database that stores the authentication info
# 
# if(Sys.info()["sysname"] == "Linux") {
#   gedidb <<- mongo(db = "gedidb", collection = "gedicol", mongoUrl)
# }


## funcs
# shp points shapes in png
pchIconsPng <<- function(pch = 1, width = 30, height = 30, bg = "transparent", col = "black", ...) {
  n <- length(pch)
  files <- character(n)
  
  # Create a sequence of png images
  for (i in seq_len(n)) { # i = 1
    # f = tempfile(fileext = '.png')
    f = paste0(outDir, '/pch', pch[i], '_colhh', gsub('#', '', col[i]),
               '_cex', gsub('\\.', 'p', min(width, height) / 8), '.png')
    if(!file.exists(f)){
      #print(paste('saving png;', outDir,  f))
      png(f, width = width, height = height, bg = bg)
      par(mar = c(0, 0, 0, 0))
      plot.new()
      points(.5, .5, pch = pch[i], col = col[i], cex = min(width, height) / 8, ...)
      dev.off()
    }
    files[i] = f
  }
  files
}
pchIconsPng(pch=shapesPt, width=40,height=40,col=c('blue','blue'),lwd=4)



# Preload Leaflets profiles  ---------------

if(file.exists(paste0(outDir, '/xleafSim.RData'))){
  # R> exists("somethingUnknown"); exists("regSim")
  print('Loading RDATA')
  load(paste0(outDir, '/leafSim.RData'))
  print('Loaded RDATA')
} else {
  
  print('Saving RDATA')
  
  load(paste0(rdatPath, '/orbits_col_crop_892.RData'))
  
  fagua1kmR <- raster(paste0(rdatPath, '/', fagua1km))
  fagua1kmR[which(fagua1kmR[] == 255)] <- NA
  fagua1kmVals <- na.omit(values(fagua1kmR))
  fagua1kmVrange <- range(fagua1kmVals, na.rm = TRUE)
  gedi_samp_id5km <- raster(gediRid5KM)
  
  # heigthPal <<-  colorNumeric( #reverse = TRUE,
  #   (c('yellow', "gold",'green', "darkgreen")) ,
  #   #c("#0C2C84", "#41B6C4", "#FFFFCC"), 
  #   domain = fagua1kmVals,
  #   na.color = "transparent")
  
  heigthPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                               domain = fagua1kmVals, na.color = "transparent")

  leafFagua <<- leaflet() %>% addTiles() %>% 
    addRasterImage(fagua1kmR, colors = heigthPal, opacity = .7, group = "Veg. Heigth") %>%
    addRasterImage(gedi_samp_id5km, colors = 'blue', opacity = .5, group = "GEDI data") %>% 
    addLegend(pal =  heigthPal, values = values(fagua1kmR),
              position = 'topleft',
              title= "Meters"#, opacity = .3
              #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    ) %>%
    addPolygons(data = orbits, color = 'red', weight = 0, 
                opacity = .9,fillOpacity = .2, group = 'GEDI Orbits',
                label = ~htmlEscape(paste0('Orbit ID: ', StrtOrN))) %>%
    addLayersControl(position = 'topleft',
                     overlayGroups = c('Veg. Heigth', 'GEDI data', 'GEDI Orbits'),
                     options = layersControlOptions(collapsed = FALSE),
                     baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>%
    addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>% 
    hideGroup(c('GEDI Orbits'))
  
  
  
  shapesPt <<- c(16, 17) # base R plotting symbols (http://www.statmethods.net/advgraphs/parameters.html)
  # A function to create file names 
  filenameCol <- function(pch,col) paste0(pch, '_', col, '.png')
  
  ## Make icons
  #print('Making plots?')
  #pchIconsPng(pch=shapesPt, width=40,height=40,col=c('blue','blue'),lwd=4)
  
  # A function to build the legend
  #https://stackoverflow.com/questions/41512908/leaflet-legend-in-r-based-on-color-and-shape
  
  ## Regions 
  pal <- colorNumeric( palette = "YlGnBu", domain = regSim$regionF)
  palCom <- colorNumeric( palette = "YlGnBu", domain = regSim$idrast)
  
  
  regSimLeaf <- leaflet() %>% addTiles() %>%
    addPolygons(data = regSim,
                label = ~htmlEscape(regSim$region),
                popup = paste0("<strong>", regSim$region,"</strong>"),
                group = "Region",
                fillOpacity = .9, 
                color = ~pal(regSim$regionF), 
                layerId = ~regionF)
  
  ## Ecosystems
  uEcoSin <- (c("Bosque", "Arbustal",  "Paramo", "Herbazal","Subxerofitia", "Xerofitia",
                "Zona pantanosa", "Turbera"))
  uEcoSinPal <- c("#31a354", "#74c476",  "#016c59", "#c2e699","#feb24c", "#de2d26",
                  "#1c9099", "#253494")
  # barplot(rep(1, length(uEcoSin)), names.arg=uEcoSin, col = uEcoSinPal)
  
  ecoPal <- colorFactor(palette = uEcoSinPal, domain = uEcoSin,  ordered = FALSE)
  
  load(paste0(rdatPath, '/rdat_eco/regEcoShp_44.RData')) # reg_and_ecos.i
  load(paste0(rdatPath, '/rdat_eco/regShp_44.RData')) # reg.i
  matchCols <- match(reg_and_ecos.i$ECOS_SINTE, uEcoSin)
  colVec <- uEcoSinPal[matchCols]
  
  regEcoLeaf <- leaflet() %>% addTiles() %>%
    addPolygons(data = reg.i, popup = paste0("<strong>Region</strong>: ", reg.i$region,""),
                group = "Region", fillOpacity = .2,
                color = 'grey60',
                layerId = ~reg.i$regionF) %>%
    addPolygons(data = reg_and_ecos.i,
                label = ~htmlEscape(reg_and_ecos.i$ECOS_SINTE),
                # popup = paste0("<strong>", reg_and_ecos.i$ECOS_SINTE,"</strong><br>",
                #                reg_and_ecos.i$ECOS_SINTE, "</br>"),
                group = "Ecosystems",
                fillOpacity = .9,
                color = colVec, #~ecoPal(factor(reg_and_ecos.i$ECOS_SINTE)),
                layerId = ~ecos_regF) %>%
    addLegend(position = "topright", #pal = ecoPal, values = uEcoSin, 
              colors = uEcoSinPal, labels= uEcoSin,
              title = "Ecosystems") %>%
    addLayersControl(overlayGroups = c('Ecosystems', 'Region'),
                     #position = "topleft",
                     baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )
  
  
  ##### GEDI
  
  load(paste0(rdatPath, '/rdat_2a/md_curve_602.RData')) # me
  load(paste0(rdatPath, '/rdat_2a/shp_602.RData')) # shp_rh_small" "mat_rh_small
  eco.i <- reg_and_ecos.i[reg_and_ecos.i$ecos_regF == 602, ]
  shp_rh_small$mx_h <- shp_rh_small$idsmall
  
  ### 2A
  palGEDI <- colorNumeric( palette = "viridis", domain = shp_rh_small$mx_h, reverse = TRUE)
  gediLabel <- paste0('<strong>ShotNum: ', shp_rh_small$shotNum,'</strong>',
                      '<br>Max. height: ', round(shp_rh_small$mx_h, 3),
                      '<br>Beam: ', shp_rh_small$beam,
                      '<br>Date: ', shp_rh_small$date,
                      '<br>Day of year: ', shp_rh_small$DOY,
                      '<br>Time: ', shp_rh_small$time,
                      '<br>solElev: ', round(shp_rh_small$solElev, 3),
                      '<br>degFlag: ', shp_rh_small$degFlag)
  
  
  
  ecoLeaf <- leaflet(shp_rh_small) %>% addTiles() %>%
    addPolygons(data = reg.i, popup = paste0("<strong>Region</strong>: ", reg.i$region,""),
                group = "Region", fillOpacity = .2,
                color = 'grey60',
                layerId = ~reg.i$regionF) %>%
    
    addPolygons(data = eco.i, popup = paste0("<strong>Ecosystem</strong>: ", eco.i$ECOS_SINTE,""),
                group = "Ecosystems", fillOpacity = .6, 
                stroke = TRUE,  opacity = 0, # amke invisible borders
                color = uEcoSinPal[uEcoSin %in% eco.i$ECOS_SINTE[1]],
                layerId = ~eco.i$ecos_regF) %>%
    
    addCircleMarkers(popup = (gediLabel), 
                     # https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines
                     label = ~htmlEscape(shp_rh_small$shotNum),
                     radius = .2, group = 'GEDI', layerId = ~shp_rh_small$idsmall,
                     color = ~palGEDI(mx_h)) %>% # stroke = FALSE, fillOpacity = 0.5
    
    addLegend("bottomright", pal = palGEDI, values = ~mx_h,
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
              title = "Max. height", #labFormat = labelFormat(prefix = "$"),
              opacity = 1
              
    ) %>%
    addLegend("bottomright", pal = ecoPal, values = eco.i$ECOS_SINTE[1],
              title = "Ecosystem", #labFormat = labelFormat(prefix = "$"),
              opacity = 1
              
    ) %>%
    addLayersControl(overlayGroups = c('GEDI', 'Ecosystems', 'Region'),
                     #position = "topleft",
                     baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )
  
  
  save(pal, regSimLeaf, 
       uEcoSin, uEcoSinPal, ecoPal, regEcoLeaf,
       ecoLeaf, mat_rh_small, shp_rh_small,
       leafFagua, fagua1kmR, fagua1kmVals, fagua1kmVrange, heigthPal,
       orbits,
       
       file = paste0(outDir, '/leafSim.RData'))
  print('Saved RDATA')
  
}




# UI  ---------------
ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader( 
    ## UI Tags ------------------------------
    tags$li(a(href = 'https://goetzlab.rc.nau.edu/', target="_blank",
              img(src = 'geodelab.png',
                  title = "TNC Colombia", height = "50px"),
              style = "padding-top:0px; padding-bottom:0px;padding-left:0px;padding-right:0px;"),
            class = "dropdown"),
    title = "GEDI Visualization tool for Colombia",  titleWidth = 400
    ## UI sidebar ------------------------------
  ), ## End class
  
  # UI Panel  ---------------
  dashboardSidebar(disable = FALSE,
                   sidebarMenu(
                     
                     menuItem("Profiles", tabName = "tab_profile", startExpanded = TRUE,
                              menuSubItem("How does it work??", tabName = "tab_workgedi"),
                              menuSubItem("Profiles\ntool", tabName = "tab_gedi")
                     ),
                     
                     menuItem("Profiles L2b", tabName = "tab_profile2", startExpanded = TRUE,
                              #  menuSubItem("How does it work??", tabName = "tab_workgedi2"),
                              menuSubItem("Profiles\ntool", tabName = "tab_gedi2"),
                              menuSubItem("Compare", tabName = "tab_compare")
                     ),
                     
                     menuItem("Similarity", tabName = "tab_similarity", startExpanded = TRUE,
                              menuSubItem("How does it work??", tabName = "tab_worksimi"),
                              menuSubItem("Similarity\ntool", tabName = "tab_similarity")),
                     
                     menuItem("Height map", tabName = "tab_height", startExpanded = TRUE,
                              menuSubItem("Height map", tabName = "tab_heightmap"))
                   )
  ),                 
  
  dashboardBody( 
    tags$head(tags$style(
      HTML('.skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }.skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }.nav>li>a {
        padding: 1vh 8vw;
        }'))),
    
    tabItems(
      ######## UI GEDI viz ------------
      
      # tab_workgedi, tab_gedi, tab_worksimi, tab_similarity
      tabItem('tab_workgedi', includeMarkdown("md_intro.md")),
      tabItem('tab_gedi', 
              tags$style(type = "text/css", "#mapleaf {height: calc(100vh - 80px) !important;}"),
              tags$style(type = "text/css", "#plotcurve {height: calc(90vh - 90px) !important;}"),
              # tabBox(  width = NULL,
              #   tabsetPanel(  type = 'pills',
              #     tabPanel( id = 'tab_intros', 
              #       title = 'Intro', ## Tomar índices calculados y ponerlos en mapas
              #       includeMarkdown("md_intro.md") ),
              #     
              #     tabPanel(
              #id = 'tab_gedi', 
              title = 'GEDI Vis', ## Consultar especies basado en un mapa
              
              
              fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                
                column( width = 6,
                        h6(),
                        leafletOutput("mapleaf", #height = "600px"
                        ) %>% withSpinner(color="#0dc5c1")
                ),
                column(width = 6, 
                       fluidRow( 
                         #h5('Click the biotic regions map >> click an ecosystem >> click up to 3 points >> Clear'),
                         column( width = 4, h5('1. Select an ecorregion in the map'), actionButton("clearMap", "Clear ecorregion") ),
                         column( width = 4, h5('2. Select an ecosystem'), actionButton("clearEco", "Clear ecosystem") ),
                         column( width = 4, h5('3. Select GEDI points'), actionButton("clearCur", "Clear curves") )
                       ),
                       br(),
                       highchartOutput("plotcurve", height = "600px") %>% withSpinner(color="#0dc5c1") )
              )
              # ) # close tab
              #### close tabs
              #     
              #   ) # tabsetPanel
              # ) # tabbox
      ),
      
      ######## UI GEDI viz2 ------------
      
      # tab_workgedi, tab_gedi, tab_worksimi, tab_similarity
      #tabItem('tab_workgedi2', includeMarkdown("md_intro.md")),
      tabItem('tab_gedi2', 
              tags$style(type = "text/css", "#mapleaf2 {height: calc(90vh - 90px) !important;}"),
              tags$style(type = "text/css", "#plotcurve2 {height: calc(90vh - 90px) !important;}"),
              title = 'GEDI Vis', ## Consultar especies basado en un mapa
              
              fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                
                column( width = 6,
                        fluidRow( 
                          #h5('Click the biotic regions map >> click an ecosystem >> click up to 3 points >> Clear'),
                          column( width = 1),
                          column( width = 4, h5('1. Select an ecorregion in the map'), 
                                  actionButton("clearMap2", "Clear ecorregion") ),
                          column( width = 4, h5('2. Select an ecosystem'), 
                                  actionButton("clearEco2", "Clear ecosystem") ),
                          column( width = 3, h5('3. Select GEDI points'), 
                                  actionButton("clearCur2", "Clear curves") )
                        ),
                        h6(),
                        leafletOutput("mapleaf2", #height = "800px"
                        ) %>% withSpinner(color="#0dc5c1")
                ),
                column(width = 6, 
                       
                       fluidRow( 
                         column( width = 3,
                                 selectInput(inputId = "varSelected", label = "Variable:",
                                             choices =  c( 'PAI', 'PAVD', 'COV'), # 'RH',
                                             selected = 'RH')),
                         column( width = 3,
                                 selectInput(inputId = "eneSelected", label = "Energy:",
                                             choices =  c('Absolute', 'Proportional'), 
                                             selected = 'RH')),
                         column( width = 3, 
                                 sliderInput(inputId = "xAxisSlider", label = "x-range:", 
                                             min = 0, max = 100, value = 100, step = 1)),
                         column( width = 3, 
                                 sliderInput(inputId = "yAxisSlider", label = "Y-range:", 
                                             min = 0, max = 100, value = 100, step = 1))
                       ),
                       
                       highchartOutput("plotcurve2"
                                       , height = "800px"
                       ) %>% withSpinner(color="#0dc5c1") )
              )
              # ) # close tab
              #### close tabs
              #     
              #   ) # tabsetPanel
              # ) # tabbox
      ),
      
      ######## UI GEDI compare ------------
      
      tabItem('tab_compare', 
              tags$style(type = "text/css", "#mapleaf3 {height: calc(90vh - 90px) !important;}"),
              tags$style(type = "text/css", "#plotcurve3 {height: calc(90vh - 90px) !important;}"),
              
              fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                
                column( width = 6,
                        fluidRow( 
                          #h5('Click the biotic regions map >> click an ecosystem >> click up to 3 points >> Clear'),
                          column( width = 1),
                          column( width = 4, #h5('1. Select an ecosystem type:'),
                                  selectInput('selEco', label = '1. Select an ecosystem type:', 
                                              choices = uEcoSin, selected = 'Bosque')),
                          column( width = 4, h5('2. Click regions in the map'), 
                                  actionButton("clearEco3", "Clear ecosystem") ),
                          column( width = 3, h5('3. Clear curves'),
                                  actionButton("clearCur3", "Clear curves") )
                        ),
                        h6(),
                        leafletOutput("mapleaf3", height = "800px"
                        ) %>% withSpinner(color="#0dc5c1")
                ),
                column(width = 6, 
                       
                       fluidRow( 
                         column( width = 3,
                                 selectInput(inputId = "varSelected3", label = "Variable:",
                                             choices =  c( 'PAI', 'PAVD', 'COV'), # 'RH',
                                             selected = 'RH')),
                         column( width = 3,
                                 selectInput(inputId = "eneSelected3", label = "Energy:",
                                             choices =  c('Absolute', 'Proportional'), 
                                             selected = 'RH')),
                         column( width = 3, 
                                 sliderInput(inputId = "xAxisSlider3", label = "x-range:", 
                                             min = 0, max = 100, value = 100, step = 1)),
                         column( width = 3, 
                                 sliderInput(inputId = "yAxisSlider3", label = "Y-range:", 
                                             min = 0, max = 100, value = 100, step = 1))
                       ),
                       
                       highchartOutput("plotcurve3"
                                       , height = "800px"
                       ) %>% withSpinner(color="#0dc5c1") 
                )
              )
              # ) # close tab
              #### close tabs
              #     
              #   ) # tabsetPanel
              # ) # tabbox
      ),
      
      
      
      ######## UI GEDI cluster ------------
      
      tabItem('tab_worksimi', includeMarkdown("md_introsimi.md")),
      
      tabItem("tab_similarity", 
              fluidRow(
                column(width = 2, selectInput("in_gedi", "GEDI type", c('2a', '2b'), selected = '2a')), #2a_2b
                column(width = 2, selectInput("in_bin", "Samples", bins, selected = bin_selected_start)),
                column(width = 2, fluidRow(br(), actionButton("go_showscatter", "Show in plot"))),
                column(width = 1, fluidRow(br(), actionButton("go_showmap", "Show in map"))),
                column(width = 1, fluidRow(br(), actionButton("go_reset", "Reset"))),
                column(width = 2, selectInput("in_x1", "Plot-X-axis", xyDfCN, selected = 'u1')),
                column(width = 2, selectInput("in_y2", "Plot-Y-axis", xyDfCN, selected = 'u2'))
              ),
              fluidRow(
                
                column(width = 6, 
                       fluidRow(column(width = 1),
                                column(width = 3, selectInput("in_r", "Pts color: Red", xyDfCNr, selected = 'u1')),
                                column(width = 4, selectInput("in_g", "Pts color: Green", xyDfCNr, selected = 'u2')),
                                column(width = 4, selectInput("in_b", "Pts color: Blue", xyDfCNr, selected = 'rh_100')))
                       , 
                       leafletOutput("leafPts", height = "600px") %>% withSpinner(color="#0dc5c1")
                ),
                
                column(width = 6, 
                       fluidRow(plotlyOutput("plot", height = '80%') %>% withSpinner(color="#0dc5c1"),
                                plotlyOutput("barplot", height = '80%') %>% withSpinner(color="#0dc5c1"))
                       
                )
              )
      ) ,
      
      
      ######## UI heigth ------------
      
      tabItem(tabName = "tab_heightmap", 
              
              #bootstrapPage(
              #fluidPage(
              #fluidRow(
              fillPage(
                #column(width = 12,
                #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                # leafletOutput("mapx",# width = "100%", height = "100%")
                #               height = 1000)
                
                #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                #tags$style(type = "text/css", "#plotcurve2 {height: calc(90vh - 90px) !important;}"),
                tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
                leafletOutput("mapx", width="100%",height="1000px")#, width="100%", height = "100%")
                ,
                absolutePanel(top = 50, right = 20,
                              fluidRow(
                                column(2, br(), actionButton("go_height", "Clip map")),
                                column(3, selectInput("map_eco", "Eco. domain", c('All', 'Forest'), 
                                                      selected = 'All')),
                                column(3, selectInput("map_var", "Veg. variable", 
                                                      c('Canopy heigth',
                                                        'Canopy cover',
                                                        'fhdPai',
                                                        'PAI',
                                                        'RH50'), 
                                                      selected = 'Canopy heigth'))
                                ),
                              
                              
                              verbatimTextOutput('rglText') %>% withSpinner(color="#0dc5c1"),
                              #checkboxInput("scale_rgl", "Show legend", TRUE),
                              rglwidgetOutput("rglPlot")
                )
              )
      ) # close tab
      
      ######## UI close ------------
      
    ) # close tabItems
  ) # dashboardbody
)


# SERVER  ---------------
server <- function(input, output, session) {
  
  # pchIconsPng <- function(pch = 1, width = 30, height = 30, bg = "transparent", col = "black", ...) {
  #   n = length(pch)
  #   files = character(n)
  #   # create a sequence of png images
  #   for (i in seq_len(n)) { # i = 1
  #     #f = tempfile(fileext = '.png')
  #     f = paste0(outDir, '/pch', pch[i], '_colhh', gsub('#', '', col[i]),
  #                '_cex', gsub('\\.', 'p', min(width, height) / 8), '.png')
  #     if(!file.exists(f)){
  #       #print(paste('saving png;', outDir,  f))
  #       png(f, width = width, height = height, bg = bg)
  #       par(mar = c(0, 0, 0, 0))
  #       plot.new()
  #       points(.5, .5, pch = pch[i], col = col[i], cex = min(width, height) / 8, ...)
  #       dev.off()
  #     }
  #     files[i] = f
  #   }
  #   files
  # }
  ###### Hight map ------------------
  
  output$mapx <- renderLeaflet({
    leafFagua
  })
  
  output$rglPlot <- renderRglwidget({
    NULL
  })
  
  output$rglText <- renderText({ NULL })
  
  observeEvent(input$mapx_click, {

    click3 <- input$mapx_click
    
    # click3 <- list(lat = 5.747174, lng = -74.86084)
    # save(click3, file = 'mapxClick.RData')
    # load('mapxClick.RData') # click3
    buffPt <- rgeos::gBuffer(sp::SpatialPoints(cbind(click3$lng, click3$lat)),
                             width = 0.0083/1) # 1km
    buffPt <<- SpatialPolygonsDataFrame(buffPt, 
                                        data.frame(a = 1,
                                                           x = click3$lng, 
                                                           y = click3$lat
    ), match.ID = FALSE)
    #text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
    
    proxy <- leafletProxy("mapx")
    # print(click)
    # # proxy <- leafFagua
    proxy %>% removeShape(c("studyCirc")) %>% 
      addPolygons(data = buffPt, color = 'blue', label = 'Study area', layerId = 'studyCirc')
    
  })
  
  ### Go Height raster ---------
  observeEvent(input$go_height, {
    if(exists("tempRaster")){
      file.remove(tempRaster) # Remove previous raster clip
    }
    
    ## isolate to make the space next to click buttom load
    if(exists('buffPt')){
    output$rglText <- renderText({

      
      output$rglPlot <- renderRglwidget({
        
        timeMark <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
        tempRaster <- paste0(outDir, '/tempR_', timeMark, 
                             '_r1', round(runif(1, 0, 100)), 
                             '_r2', round(runif(1, 0, 100)), 
                             '_r3', basename(tempfile()), '.tif')
        
        (rast <- gdalUtilities::gdalwarp(srcfile = paste0(rdatPath, '/', fagua30m),
                                         dstfile = tempRaster,
                                         csql = paste0("select ST_GeomFromText('", 
                                                       rgeos::writeWKT(buffPt, byid = F), "')"),
                                         cutline = paste0(rdatPath,'/tempSQlite.sqlite'),
                                         dstnodata = 999,
                                         crop_to_cutline = TRUE,
                                         overwrite = TRUE))
        
        hmap <- raster(rast)
        #plot(hmap)
        #if(hmap@data@inmemory)
        
        if(!exists('gedi_samp_id5km')){
          #fagua1kmR <- raster(fagua1km)
          gedi_samp_id5km <- raster(gediRid5KM)
        }
        
        
        # idGed <- na.omit(idGedi5m[])
        # mbt <- microbenchmark::microbenchmark(times = 5,a = {raster::extract(gedi_samp_id5km, cbind(click3$lng, click3$lat))},
        #                                       b = {idGedi5m <- crop(gedi_samp_id5km, buffPt);idGed <- na.omit(idGedi5m[])})
        
        # idGedi5m <- raster::extract(gedi_samp_id5km, cbind(click3$lng, click3$lat)) # slighly slower
        idGedi5m <- crop(gedi_samp_id5km, buffPt)
        idGed <- na.omit(idGedi5m[])
        #print(paste('ID GEDI: ', paste(idGed), paste0('; click3 <- list(lng=',click3$lng,', lat=', click3$lat, ')') ))
        
        #rm(gediMat)
        
        #Pre-start gediDF of class logical
        gediDf <- NA
        gediPts <- NA
        if (length(idGed) != 0){
          #plot(idGedi5m)
          #plot(buffPt, add = TRUE)
          gediRdats <- paste0(gediRDatPath, '/', idGed,'.RData')
          gediRdatOk <- gediRdats[which(file.exists(gediRdats))]
          #gediRdatOk <- c('gedi2019_id_5km/25.RData', 'gedi2019_id_5km/24.RData')
          
          if(length(gediRdatOk) != 0){
            
            gediMat <- (lapply(gediRdatOk, function(z0){
              # z0 <- gediRdatOk[1]
              #print(z0)
              load(z0) # mat
              #str(mat)
              mat$gediid1km <- z0
              row.names(mat) <- NULL
              mat[, c("lon_lm_a0", "lat_lm_a0", 
                      'shot_num', 'orbit', 'beam', 
                      'delta_time', 'date', 'time', 'rh_100_a0')]
            }))
            
            gediDf <- do.call(rbind, gediMat)
            
            wkt_ext <- raster::extent(buffPt)
            spatialQuery <- raster::extract(buffPt, gediDf[, c("lon_lm_a0", "lat_lm_a0")],
                                            method = 'simple')
            
            # head(gediDf[, c("lon_lm_a0", "lat_lm_a0")])
            
            selPosGedi <- which(!is.na(spatialQuery$poly.ID))
            
            gediPts <- NA
            if(length(selPosGedi) != 0){
              gediPts <- gediDf[!is.na(spatialQuery$poly.ID), ]
            }
            
            
            # plot(hmap)
            # plot(buffPt, add = TRUE)
            # points(gediDf[, c("lon_lm_a0", "lat_lm_a0")], pch = 21)
            # points(gediPts[, c("lon_lm_a0", "lat_lm_a0")], pch = 20, col = 2)
            
            #if(nrow(mongoResult) > 0){ }
            # hxy2 <- hxy
            # hxy2$h <- 10
            #mongoResult <- data.frame(lon_lm_a0 = 0, lat_lm_a0 = 0)
          } else {
            gediDf <- NA
          }
        } else {
          gediDf <- NA
        }
        
        
        proxy <- leafletProxy("mapx")
        # proxy <- leafFagua
        
        # if(Sys.info()["sysname"] == "Linux") {
        #   #gedidb <<- mongo(db = "gedidb", collection = "gedicol", mongoUrl)
        #   wkt_ext <- raster::extent(buffPt)
        #   mongoQuery <- paste0('{"lon_lm_a0": {"$gte": ', wkt_ext@xmin, ', "$lte": ', wkt_ext@xmax,
        #                        '}, "lat_lm_a0": {"$gte": ', wkt_ext@ymin, ', "$lte": ', wkt_ext@ymax, '}}')
        #   mongoResult <- gedidb$find(mongoQuery)
        #   # load('mongoResult.RData') # mongoResult
        #   ## Closing connection 
        #   #rm(gedidb); gc()
        #   ## Select records inside the geometry
        #   spatialQuery <- raster::extract(buffPt, mongoResult[, c("lon_lm_a0", "lat_lm_a0")])
        #   mongoResult <- mongoResult[!is.na(spatialQuery$poly.ID), ]
        #   
        # } else {
        #   load('mongoResult.RData') # mongoResult
        # }
        # 
        # if(nrow(mongoResult) == 0){
        #   proxy %>% 
        #     addRasterImage(hmap, group = 'Crop',
        #                    colors = heigthPal, opacity = 1)  %>%
        #     addMarkers(lng = buffPt$x, lat = buffPt$y, group = 'Point')   %>%
        #     setView(lng = buffPt$x, lat = buffPt$y, zoom = 16) %>%
        #     addPolygons(data = buffPt, color = 'blue', fill = NA,
        #                 label = ~htmlEscape('Study area'), group = 'StudyArea' ) %>% 
        #     addLayersControl(position = 'topleft',
        #                      overlayGroups = c('Heigth', 'Orbits', 'StudyArea', 'Crop'),
        #                      options = layersControlOptions(collapsed = FALSE),
        #                      baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>% 
        #     hideGroup(c('Heigth', 'Orbits')) %>% hideGroup(c('Orbits'))     
        #   print('4c1')
        # } else {
        
        
        if( class(gediDf) == 'data.frame'){
          # c('Veg. Heigth', 'GEDI data', 'GEDI Orbits')
          
          proxy %>% removeShape(c("studyCirc")) %>% 
            addRasterImage(hmap, group = 'Crop',
                           colors = heigthPal, opacity = 1)  %>%
            addCircleMarkers(data = gediDf, lng = ~lon_lm_a0, lat = ~lat_lm_a0,
                             color = 'red', 
                             fillColor = 'red',
                             # https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines
                             label = ~htmlEscape(paste0('GEDI shot:', gediDf$shot_num)), # shotNum
                             radius = 5, group = 'GEDI points'
                             # , 
                             # layerId = ~shp_rh_small$idsmall,
                             # color = ~palGEDI(mx_h)
            ) %>% # stroke = FALSE, fillOpacity = 0.5
            
            addMarkers(lng = buffPt$x, lat = buffPt$y, group = 'Point')   %>%
            setView(lng = buffPt$x, lat = buffPt$y, zoom = 16) %>%
            addPolygons(data = buffPt, color = 'blue', fill = NA,
                        label = ~htmlEscape('Study area'), group = 'StudyArea' ) %>% 
            addLayersControl(position = 'topleft',
                             overlayGroups = c('Veg. Heigth', 'GEDI data', 'GEDI Orbits',
                                               'StudyArea', 'Crop', 'GEDI points'),
                             options = layersControlOptions(collapsed = FALSE),
                             baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>% 
            hideGroup(c('Veg. Heigth', 'GEDI data', 'GEDI Orbits'))
          
        } else if (class(gediDf) == 'logical'){
          
          proxy %>% 
            addRasterImage(hmap, group = 'Crop',
                           colors = heigthPal, opacity = 1)  %>%
            addMarkers(lng = buffPt$x, lat = buffPt$y, group = 'Point')   %>%
            setView(lng = buffPt$x, lat = buffPt$y, zoom = 16) %>%
            addPolygons(data = buffPt, color = 'blue', fill = NA,
                        label = ~htmlEscape('Study area'), group = 'StudyArea' ) %>% 
            addLayersControl(position = 'topleft',
                             overlayGroups = c('Veg. Heigth', 'GEDI data', 'GEDI Orbits',
                                               'StudyArea', 'Crop'),
                             options = layersControlOptions(collapsed = FALSE),
                             baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>% 
            hideGroup(c('Veg. Heigth', 'GEDI data', 'GEDI Orbits'))
          
          print('4c2')
        }
        
        
        
        
        # output$wdg <- renderRglwidget({
        #   open3d(useNULL = TRUE)
        #   ids <- plot3d(hxy$x, hxy$y, hxy$h)[1]
        #   scene <- scene3d()
        #   rgl.close()
        #   rglwidget(scene, controllers = c("control"))
        # })
        # 
        
        # Raster surface
        hxy <- data.frame(xyFromCell(hmap, cell = 1:ncell(hmap)), h = hmap[])
        hxy <- subset(hxy, !is.na(h) & h != 256)
        #dim(hxy)
        
        try(rgl.close(), silent = TRUE)
        try(rgl.close(), silent = TRUE)
        #if (input$rescale) aspect3d(1,1,10) else aspect3d(1,1,1)
        
        ## color pal col pal
        palGEDIcrop <- colorNumeric( palette = "viridis", reverse = TRUE,
                                     domain = hxy$h)
        points3d(zoom = 1.2,
                 hxy$x, hxy$y, hxy$h, 
                 col = palGEDIcrop(hxy$h) 
                 #col = '#E0E318',radius = 0.1, lit = TRUE
        )
        
        if( class(gediPts) == 'data.frame'){
          
          points3d(#mongoResult$lon_lm_a0,
            gediPts$lon_lm_a0,
            gediPts$lat_lm_a0,
            gediPts$rh_100_a0, 
            col = 2
            #col = '#E0E318',radius = 0.1, lit = TRUE
          )
        }
        
        aspect3d(1,1,1)
        axes3d(labels = TRUE, xlab = 'Lat', ylab = 'Lon', zlab = 'Heigth')
        rglwidget()
      }) # close renderRglwidget
      
      NULL# 'Done' return VText
    })
    }
  })
  
  ###### GO RGL -------------
  # options(rgl.useNULL = TRUE)
  # save <- options(rgl.inShiny = TRUE)
  # on.exit(options(save))
  # 
  # output$wdg <- renderRglwidget({
  #   rglwidget(scene, controllers = c("control"))
  # })
  # 
  # output$control <- renderPlaywidget({
  #   toggleWidget("wdg", respondTo = "chk", ids = ids)
  # })
  # 
  ###### end heigth example ------------------
  
  output$barplot <- renderPlotly({ NULL }) 
  
  pchIconsPng(pch = shapesPt, width=40,height=40,col=c('blue','blue'),lwd=4)
  
  build_legend <<- function(){
    paste(
      sapply(
        strsplit(
          unique(paste(shapesPt, 'blue')), " "),
        #paste(sapply(strsplit(unique(paste(data_all$Group, data_all$condition)), " "), 
        function(x){
          #x <- list(16, 'blue')
          xyz <- paste0("<img src='data:image/png;base64,",
                        base64enc::base64encode(
                          paste0(#outDir, '/', 
                            filenameCol(x[[1]], x[[2]]))), 
                        "' width='16'; height='16'> ",
                        "",ifelse(x[[1]] == 16, 'All', 'Protected area'), 
                        "<br/>" )
          
          #print(xyz)
          xyz
        }), collapse = " ")
  };
  
  ## Profile study --------------
  
  output$mapleaf <- renderLeaflet({ # studymap: mapleaf
    regSimLeaf # studyleaflet
    #regEcoLeaf # 
    #ecoLeaf
  })
  
  output$mapleaf2 <- renderLeaflet({ # studymap: mapleaf
    regSimLeaf # studyleaflet
    #regEcoLeaf # 
    #ecoLeaf
  })
  
  output$mapleaf3 <- renderLeaflet({ # studymap: mapleaf
    regSimLeaf # studyleaflet
    #regEcoLeaf # 
    #ecoLeaf
  })
  
  
  ## Profile ecosystems --------------
  
  data_of_curve <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$mapleaf_shape_click,{
    myClickPol <- data_of_curve$clickedMarker <- input$mapleaf_shape_click
    # print(data_of_curve$clickedMarker)
    # print(str(data_of_curve$clickedMarker))
    # print(class(input$mapleaf_shape_click))
    #print(str(myClickPol))
    # myClickPol <- list(id = 44, group = "Region")
    
    if(is.numeric(myClickPol$id) & is.character(myClickPol$group)){
      #print('Valid click')
      
      ## First click selecting region ----
      if(myClickPol$group == 'Region'){
        regID <<- myClickPol$id
        
        # print('Zoom to region')
        # print(str(myClickPol))
        # myClickPol$id <- 50
        output$mapleaf <- renderLeaflet({ 
          load(paste0(rdatPath, '/rdat_eco/regEcoShp_', regID,'.RData')) # reg_and_ecos.i
          load(paste0(rdatPath, '/rdat_eco/regShp_', regID,'.RData')) # reg.i
          
          reg.i <<- reg.i
          reg_and_ecos.i <<- reg_and_ecos.i
          
          matchCols <- match(reg_and_ecos.i$ECOS_SINTE, uEcoSin)
          colVec <- uEcoSinPal[matchCols]
          
          regEcoLeaf <<- leaflet() %>% addTiles() %>%
            addPolygons(data = reg.i, popup = paste0("<strong>Region</strong>: ", reg.i$region,""),
                        group = "Region", fillOpacity = .2,
                        color = 'grey60') %>%
            
            addPolygons(data = reg_and_ecos.i,
                        label = ~htmlEscape(reg_and_ecos.i$ECOS_SINTE),
                        stroke = TRUE,  opacity = 0, # make invisible borders
                        group = "Ecosystems",
                        fillOpacity = .9,
                        color = colVec, #~ecoPal(reg_and_ecos.i$ECOS_SINTE),
                        layerId = ~ecos_regF) %>%
            addLegend(position = "topright", #pal = ecoPal, values = uEcoSin, 
                      colors = uEcoSinPal, labels= uEcoSin,
                      title = "Ecosystems") %>%
            
            addLayersControl(overlayGroups = c('Ecosystems', 'Region'),
                             #position = "topleft",
                             baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )
          
          regEcoLeaf
        })
        
      }
      
      ## Second click selecting ecosystem ----
      if(myClickPol$group == 'Ecosystems' & myClickPol$id != ecoID){
        # print('Zoom to ecosystems')
        # print(str(myClickPol))
        ecoID <<- myClickPol$id; # ecoID <- 621; regID <- 45
        
        output$mapleaf <- renderLeaflet({ 
          load(paste0(rdatPath, '/rdat_2a/shp_', ecoID,'.RData')) # "shp_rh_small" "mat_rh_small"
          
          if(!exists('reg_and_ecos.i')){
            load(paste0(rdatPath, '/rdat_eco/regEcoShp_', regID,'.RData')) # reg_and_ecos.i
          }
          
          if(!exists('reg.i')){
            load(paste0(rdatPath, '/rdat_eco/regShp_', regID,'.RData')) # reg_and_ecos.i
          }
          
          matchEcoRegPols <- which(reg_and_ecos.i$ecos_regF == ecoID)
          if(length(matchEcoRegPols) == 0){
            load(paste0(rdatPath, '/rdat_eco/regEcoShp_', regID,'.RData')) # reg_and_ecos.i
            load(paste0(rdatPath, '/rdat_eco/regShp_', regID,'.RData')) # reg.i
          }
          
          # print(paste('EcoID: ', ecoID, ' - regID:', regID))
          # print(paste('Ecos orig shp', nrow(reg_and_ecos.i)))
          # print(paste('N pols reg & Eco', length(matchEcoRegPols)))
          eco.i <- reg_and_ecos.i[matchEcoRegPols, ]
          # print(paste('Ecos shp', nrow(eco.i)))
          
          shp_rh_small$mx_h[is.infinite(shp_rh_small$mx_h)] <- NA
          palGEDI <- colorNumeric( palette = "viridis", reverse = TRUE,
                                   domain = shp_rh_small$mx_h)
          gediLabel <- paste0('<strong>Point ID:', shp_rh_small$idsmall,'</strong>',
                              '<br>Ecosys. ID:', ecoID,'</strong>',
                              '<br><strong>ShotNum: ', shp_rh_small$shotNum,'</strong>',
                              '<br>Max. height: ', round(shp_rh_small$mx_h, 3),
                              '<br>Beam: ', shp_rh_small$beam,
                              '<br>Date: ', shp_rh_small$date,
                              '<br>Day of year: ', shp_rh_small$DOY,
                              '<br>Time: ', shp_rh_small$time,
                              '<br>solElev: ', round(shp_rh_small$solElev, 3),
                              '<br>degFlag: ', shp_rh_small$degFlag)
          
          # print(paste('Prev leaf: ', nrow(eco.i)))
          
          ecoLeaf <- leaflet(shp_rh_small) %>% addTiles() %>%
            addPolygons(data = reg.i, popup = paste0("<strong>Region</strong>: ", reg.i$region,""),
                        group = "Region", fillOpacity = .2,
                        color = 'grey60',
                        layerId = ~reg.i$regionF) %>%
            
            addPolygons(data = eco.i, popup = paste0("<strong>Ecosystem</strong>: ", eco.i$ECOS_SINTE,""),
                        group = "Ecosystems", fillOpacity = .6, 
                        stroke = TRUE,  opacity = 0, # amke invisible borders
                        color = uEcoSinPal[uEcoSin %in% eco.i$ECOS_SINTE[1]],
                        layerId = ~eco.i$ecos_regF) %>%
            
            addCircleMarkers(popup = (gediLabel), 
                             # https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines
                             label = ~htmlEscape(shp_rh_small$idsmall), # shotNum
                             radius = .2, group = 'GEDI', 
                             layerId = ~shp_rh_small$idsmall,
                             color = ~palGEDI(mx_h)) %>% # stroke = FALSE, fillOpacity = 0.5
            
            addLegend("bottomright", pal = palGEDI, values = ~mx_h,
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)),
                      title = "Max. height", #labFormat = labelFormat(prefix = "$"),
                      opacity = 1
                      
            ) %>%
            addLegend("bottomright", # pal = ecoPal, values = eco.i$ECOS_SINTE[1],
                      colors = uEcoSinPal[uEcoSin %in% eco.i$ECOS_SINTE[1]], 
                      labels = eco.i$ECOS_SINTE[1],
                      title = "Ecosystem", #labFormat = labelFormat(prefix = "$"),
                      opacity = 1
                      
            ) %>%
            addLayersControl(overlayGroups = c('GEDI', 'Ecosystems', 'Region'),
                             #position = "topleft",
                             baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )
          # print(paste('Post leaf: ', nrow(eco.i)))
          ecoLeaf
        })
        
        
        output$plotcurve <- renderHighchart({
          load(paste0(rdatPath, '/rdat_2a/hc_', ecoID,'.RData')) # hc
          hc0 <<- hc %>% hc_exporting(enabled = TRUE) 
          hc <<- hc 
          hc %>% hc_exporting(enabled = TRUE) 
        })
      } 
    }
  })
  
  
  ### 
  observeEvent(input$mapleaf2_shape_click,{
    myClickPol2 <<- data_of_curve$clickedMarker <- input$mapleaf2_shape_click
    # print(data_of_curve$clickedMarker)
    # print(str(data_of_curve$clickedMarker))
    # print(class(input$mapleaf_shape_click))
    # print(str(myClickPol))
    # myClickPol <- list(id = 44, group = "Region")
    
    if(is.numeric(myClickPol2$id) & is.character(myClickPol2$group)){
      #print('Valid click')
      
      ## Region is selected
      if(myClickPol2$group == 'Region'){
        regID <<- myClickPol2$id
        
        # print('Zoom to region')
        # print(str(myClickPol))
        # myClickPol$id <- 50
        output$mapleaf2 <- renderLeaflet({ 
          load(paste0(rdatPath, '/rdat_eco/regEcoShp_', regID,'.RData')) # reg_and_ecos.i
          load(paste0(rdatPath, '/rdat_eco/regShp_', regID,'.RData')) # reg.i
          
          reg.i <<- reg.i
          reg_and_ecos.i <<- reg_and_ecos.i
          
          matchCols <- match(reg_and_ecos.i$ECOS_SINTE, uEcoSin)
          colVec <- uEcoSinPal[matchCols]
          
          regEcoLeaf <<- leaflet() %>% addTiles() %>%
            addPolygons(data = reg.i, popup = paste0("<strong>Region</strong>: ", reg.i$region,""),
                        group = "Region", fillOpacity = .2,
                        color = 'grey60') %>%
            
            addPolygons(data = reg_and_ecos.i,
                        label = ~htmlEscape(reg_and_ecos.i$ECOS_SINTE),
                        stroke = TRUE,  opacity = 0, # make invisible borders
                        group = "Ecosystems",
                        fillOpacity = .9,
                        color = colVec, #~ecoPal(reg_and_ecos.i$ECOS_SINTE),
                        layerId = ~ecos_regF) %>%
            addLegend(position = "topright", #pal = ecoPal, values = uEcoSin, 
                      colors = uEcoSinPal, labels= uEcoSin,
                      title = "Ecosystems") %>%
            
            addLayersControl(overlayGroups = c('Ecosystems', 'Region'),
                             #position = "topleft",
                             baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )
          
          regEcoLeaf
        })
        
      }
      
      
      ## Ecosystem is selected
      if(myClickPol2$group == 'Ecosystems' & myClickPol2$id != ecoID){
        # print('Zoom to ecosystems')
        # print(str(myClickPol))
        
        ecoID <<- myClickPol2$id; # ecoID <- 453; regID <- 45
        print(paste0('ecoID:', ecoID))
        output$mapleaf2 <- renderLeaflet({ 
          load(paste0(rdatPath, '/rdat_2b/shp_', ecoID,'.RData')) # "shp_rh_small"
          load(paste0(rdatPath, '/rdat_2b/mat_', ecoID,'.RData')) # "mat" "mat_rh_small"  "mat_pai_small" "mat_pav_small" "mat_cov_small"
          mat_rh_small <<- mat_rh_small
          mat_pai_small <<- mat_pai_small
          mat_pav_small <<- mat_pav_small
          mat_cov_small <<- mat_cov_small
          
          if(!exists('reg_and_ecos.i')){
            load(paste0(rdatPath, '/rdat_eco/regEcoShp_', regID,'.RData')) # reg_and_ecos.i
          }
          
          if(!exists('reg.i')){
            load(paste0(rdatPath, '/rdat_eco/regShp_', regID,'.RData')) # reg_and_ecos.i
          }
          
          matchEcoRegPols <- which(reg_and_ecos.i$ecos_regF == ecoID)
          if(length(matchEcoRegPols) == 0){
            load(paste0(rdatPath, '/rdat_eco/regEcoShp_', regID,'.RData')) # reg_and_ecos.i
            load(paste0(rdatPath, '/rdat_eco/regShp_', regID,'.RData')) # reg.i
          }
          
          # print(paste('EcoID: ', ecoID, ' - regID:', regID))
          # print(paste('Ecos orig shp', nrow(reg_and_ecos.i)))
          # print(paste('N pols reg & Eco', length(matchEcoRegPols)))
          eco.i <- reg_and_ecos.i[matchEcoRegPols, ]
          # print(paste('Ecos shp', nrow(eco.i)))
          
          shp_rh_small$mx_h[is.infinite(shp_rh_small$mx_h)] <- NA
          
          palGEDI <- colorNumeric( palette = "viridis", reverse = TRUE,
                                   domain = (shp_rh_small$mx_h))
          
          gediLabel <- paste0('<strong>Point ID:', shp_rh_small$idsmall,'</strong>',
                              '<br>Ecosys. ID:', ecoID,'</strong>',
                              '<br><strong>ShotNum: ', shp_rh_small$shot_num,'</strong>',
                              '<br>Max. height: ', round(shp_rh_small$mx_h, 3),
                              '<br>Beam: ', shp_rh_small$beam,
                              '<br>Date: ', shp_rh_small$date,
                              '<br>Day of year: ', shp_rh_small$doy,
                              '<br>Time: ', shp_rh_small$time,
                              '<br>solElev: ', round(shp_rh_small$sol_elev, 3),
                              '<br>degFlag: ', shp_rh_small$deg_flag)
          
          # print(paste('Prev leaf: ', nrow(eco.i)))
          
          ecoLeaf <- leaflet(shp_rh_small) %>% addTiles() %>%
            addPolygons(data = reg.i, popup = paste0("<strong>Region</strong>: ", reg.i$region,""),
                        group = "Region", fillOpacity = .2,
                        color = 'grey60',
                        layerId = ~reg.i$regionF) %>%
            
            addPolygons(data = eco.i, popup = paste0("<strong>Ecosystem</strong>: ", eco.i$ECOS_SINTE,""),
                        group = "Ecosystems", fillOpacity = .6, 
                        stroke = TRUE,  opacity = 0, # amke invisible borders
                        color = uEcoSinPal[uEcoSin %in% eco.i$ECOS_SINTE[1]],
                        layerId = ~eco.i$ecos_regF) %>%
            
            addCircleMarkers(popup = (gediLabel), 
                             # https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines
                             label = ~htmlEscape(shp_rh_small$idsmall), # shotNum
                             radius = .2, group = 'GEDI', layerId = ~shp_rh_small$idsmall,
                             color = ~palGEDI(mx_h)) %>% # stroke = FALSE, fillOpacity = 0.5
            
            addLegend("bottomright", pal = palGEDI, values = ~mx_h,
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)),
                      title = "Max. height", #labFormat = labelFormat(prefix = "$"),
                      opacity = 1
                      
            ) %>%
            addLegend("bottomright", # pal = ecoPal, values = eco.i$ECOS_SINTE[1],
                      colors = uEcoSinPal[uEcoSin %in% eco.i$ECOS_SINTE[1]], 
                      labels = eco.i$ECOS_SINTE[1],
                      title = "Ecosystem", #labFormat = labelFormat(prefix = "$"),
                      opacity = 1
                      
            ) %>%
            addLayersControl(overlayGroups = c('GEDI', 'Ecosystems', 'Region'),
                             #position = "topleft",
                             baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )
          # print(paste('Post leaf: ', nrow(eco.i)))
          ecoLeaf
        })
        
        
        output$plotcurve2 <- renderHighchart({
          #print(paste('Plot curve EcoID:', ecoID))
          # ecoID = 924
          if (!file.exists(paste0(rdatPath, '/rdat_2b/md_curve_', ecoID,'.RData'))){
            highEmpty
          } else {
            load(paste0(rdatPath, '/rdat_2b/md_curve_', ecoID,'.RData')) # me_pavd; me_cov; me_pai; me
            
            if( input$varSelected == 'RH'){
              me_rh$me <- me_rh$me3
              me_rh$av <- me_rh$av3
              me_curve <<- me_rh
            } else if (input$varSelected == 'PAI'){
              me_curve <<- subset(me_pai, rh != 0)
            } else if (input$varSelected == 'PAVD'){
              me_curve <<- subset(me_pavd, rh != 0)
            } else if (input$varSelected == 'COV'){
              me_curve <<- subset(me_cov, rh != 0)
            }
            
            
            if( input$eneSelected == 'Proportional'){
              me_curve$xAxis <- me_curve$me/sum(me_curve$me)
              me_curve$xAxisAv <- me_curve$av/sum(me_curve$av)
            } else if (input$eneSelected == 'Absolute'){
              me_curve$xAxis <- me_curve$me
              me_curve$xAxisAv <- me_curve$av
            }
            
    
            
            
            #             fntlt <- JS("function(){
            #   return 'RH: ' + Highcharts.numberFormat(this.point.x, 3) + '<br> H(m): ' + this.point.y + '' + '<br>Count:' +
            #   Highcharts.numberFormat(this.point.value, 0);
            # }")
            
            input <- list(xAxisSlider = 100, yAxisSlider = 100, eneSelected = 'Absolute', varSelected = 'PAI')
            
            hc2 <<- hc <<- highchart() %>%
              hc_add_series(me_curve, type = "line",  color = 'blue',
                            hcaes(x = xAxis, y = rh), #value = count, color = count, size = count),
                            #tooltip = list(pointFormat = '{point.x}, <br>{point.y}: {point.value}'),
                            maxSize = "5%", name = "Median", showInLegend = TRUE) %>%
              hc_add_series( me_curve, type = "line", 
                             hcaes(x = (xAxisAv), y = rh), color = 'green',
                             #tooltip = list(pointFormat = '{point.x},<br> {point.y}: <br>{point.value}'),
                             name = "Average", showInLegend = TRUE) %>%
              hc_title(text = paste(input$varSelected, "profile curve")) %>%
              #hc_tooltip(formatter = fntlt, snap = 0, stickyTracking = FALSE) %>%
              hc_yAxis(max = max(me_curve$rh, na.rm = TRUE), min = 0, 
                       title = list(text = input$varSelected)) %>%
              hc_xAxis(max = max(c(me_curve$xAxis,me_curve$xAxisAv), na.rm = TRUE), min = 0, 
                       title = list(text = input$eneSelected)) %>% 
              hc_exporting(enabled = TRUE) 
            hc
          }
        })
        
      } 
    }
  }) #  observeEvent(input$mapleaf2_shape_click
  
  ## Change axis 
  
  observe({
    c(input$yAxisSlider, input$xAxisSlider)
    if(exists('myClickPol2')){
      output$plotcurve2 <- renderHighchart({
        hc <<- hc %>% hc_yAxis(max = input$yAxisSlider, min = 0,
                               title = list(text = input$varSelected))  %>% 
          hc_xAxis(max = input$xAxisSlider, min = 0,
                   title = list(text = paste(input$eneSelected, "energy"))) %>% hc_exporting(enabled = TRUE) 
      })
    }
  })
  
  ## v3 compare
  
  observe({
    c(input$yAxisSlider3, input$xAxisSlider3)
    if(exists('myClickPol3')){
      output$plotcurve3 <- renderHighchart({
        hc3 <<- hc3 %>% hc_yAxis(max = input$yAxisSlider3, min = 0, 
                                 title = list(text = input$varSelected3))  %>% 
          hc_xAxis(max = input$xAxisSlider3, min = 0, 
                   title = list(text = paste(input$eneSelected3, "energy"))) %>% 
          hc_exporting(enabled = TRUE) 
      })
    }
  })
  
  ## ---EneSelected
  
  
  observe({
    c(input$varSelected, input$eneSelected)
    # ecoID <- 139
    if (!file.exists(paste0(rdatPath, '/rdat_2b/md_curve_', ecoID,'.RData'))){
      highEmpty
    } else {
      load(paste0(rdatPath, '/rdat_2b/md_curve_', ecoID,'.RData')) # me_pavd; me_cov; me_pai; me
      
      if( input$varSelected == 'RH'){
        me_rh$me <- me_rh$me3
        me_rh$av <- me_rh$av3
        me_curve <<- me_rh
      } else if (input$varSelected == 'PAI'){
        me_curve <<- subset(me_pai, rh != 0)
      } else if (input$varSelected == 'PAVD'){
        me_curve <<- subset(me_pavd, rh != 0)
      } else if (input$varSelected == 'COV'){
        me_curve <<- subset(me_cov, rh != 0)
      }
      
      
      if( input$eneSelected == 'Proportional'){
        me_curve$xAxis <- me_curve$me/sum(me_curve$me)
        me_curve$xAxisAv <- me_curve$av/sum(me_curve$av)
      } else if (input$eneSelected == 'Absolute'){
        me_curve$xAxis <- me_curve$me
        me_curve$xAxisAv <- me_curve$av
      }
      
      updateSliderInput(session, inputId = "xAxisSlider", 
                        value = round(max(me_curve$xAxis, na.rm = TRUE), 3),
                        min = 0, max = round(max(me_curve$xAxis, na.rm = TRUE), 3), step = .01)
      
      updateSliderInput(session, inputId = "yAxisSlider", 
                        value = max(me_curve$rh, na.rm = TRUE),
                        min = 0, max = round(max(me_curve$rh, na.rm = TRUE), 3), step = 1)
      
      hc2 <<- hc <<- highchart() %>%
        hc_add_series(me_curve, type = "line",  color = 'blue',
                      hcaes(x = xAxis, y = rh), #value = count, color = count, size = count),
                      #tooltip = list(pointFormat = '{point.x}, <br>{point.y}: {point.value}'),
                      maxSize = "5%", name = "Median", showInLegend = TRUE) %>%
        hc_add_series( me_curve, type = "line", 
                       hcaes(x = xAxisAv, y = rh), color = 'green',
                       #tooltip = list(pointFormat = '{point.x},<br> {point.y}: <br>{point.value}'),
                       name = "Mean", showInLegend = TRUE) %>%
        hc_title(text = "Median profile curve") %>%
        #hc_tooltip(formatter = fntlt, snap = 0, stickyTracking = FALSE) %>%
        hc_yAxis(max = max(me_curve$rh, na.rm = TRUE), min = 0, 
                 title = list(text = input$varSelected)) %>%
        hc_xAxis(max = max(c(me_curve$xAxis,me_curve$xAxisAv), na.rm = TRUE), min = 0, 
                 title = list(text = paste(input$eneSelected, "energy"))) %>% 
        hc_exporting(enabled = TRUE) 
      
      hc
    }
  })
  
  
  ## plot 3 compare ----
  
  observe({
    c(input$varSelected3, input$varSelected3)
    # ecoID <- 139
    if (!file.exists(paste0(rdatPath, '/rdat_2b/md_curve_', ecoID,'.RData'))){
      highEmpty
    } else {
      load(paste0(rdatPath, '/rdat_2b/md_curve_', ecoID,'.RData')) # me_pavd; me_cov; me_pai; me
      
      if( input$varSelected3 == 'RH'){
        me_rh$me <- me_rh$me3
        me_rh$av <- me_rh$av3
        me_curve <<- me_rh
      } else if (input$varSelected3 == 'PAI'){
        me_curve <<- subset(me_pai, rh != 0)
        varUnits <<- 'm2/m2'
      } else if (input$varSelected3 == 'PAVD'){
        me_curve <<- subset(me_pavd, rh != 0)
        varUnits <<- 'PAVD units'
      } else if (input$varSelected3 == 'COV'){
        me_curve <<- subset(me_cov, rh != 0)
        varUnits <<- 'COV units'
      }
      
      
      if( input$eneSelected3 == 'Proportional'){
        me_curve$xAxis <- me_curve$me/sum(me_curve$me)
        #me_curve$me/sum(me_curve$me)
        me_curve$xAxisAv <- me_curve$av/sum(me_curve$av)
      } else if (input$eneSelected3 == 'Absolute'){
        me_curve$xAxis <- me_curve$me
        me_curve$xAxisAv <- me_curve$av
      }
      
      updateSliderInput(session, inputId = "yAxisSlider3", 
                        value = max(me_curve$rh, na.rm = TRUE),
                        min = 0, max = max(me_curve$rh, na.rm = TRUE), step = 1)
      
      updateSliderInput(session, inputId = "xAxisSlider3", 
                        value = round(max(me_curve$xAxis, na.rm = TRUE), 3),
                        min = 0, max = round(max(me_curve$xAxis, na.rm = TRUE), 3), 
                        step = .01)
      
      hc2 <<- hc <<- highchart() %>%
        hc_add_series(me_curve, type = "line",  color = 'blue',
                      hcaes(x = xAxis, y = rh), #value = count, color = count, size = count),
                      #tooltip = list(pointFormat = '{point.x}, <br>{point.y}: {point.value}'),
                      maxSize = "5%", name = "Median", showInLegend = TRUE) %>%
        hc_add_series( me_curve, type = "line", 
                       hcaes(x = xAxisAv, y = rh), color = 'green',
                       #tooltip = list(pointFormat = '{point.x},<br> {point.y}: <br>{point.value}'),
                       name = "Mean", showInLegend = TRUE) %>%
        hc_title(text = "Median profile curve") %>%
        #hc_tooltip(formatter = fntlt, snap = 0, stickyTracking = FALSE) %>%
        hc_yAxis(max = max(me_curve$rh, na.rm = TRUE), min = 0, 
                 title = list(text = input$varSelected)) %>%
        hc_xAxis(max = max(c(me_curve$xAxis,me_curve$xAxisAv), na.rm = TRUE), min = 0, 
                 title = list(text = paste(input$eneSelected, varUnits))) %>% hc_exporting(enabled = TRUE) 
      
      hc
    }
  })
  
  
  # Profile curves ---------------
  data_of_gedi <- reactiveValues(clickedMarker=NULL)
  
  # Profile curves v1 ---------------
  
  observeEvent(input$mapleaf_marker_click,{
    myClickPts <- data_of_gedi$clickedMarker <- input$mapleaf_marker_click
    # print(class(myClickPts))
    #  print(str(myClickPts))
    
    if(!is.null(myClickPts$id)){
      output$plotcurve <- renderHighchart({
        
        # myClickPts <- list(id = 125)
        #    print('gedi point')
        #    print(myClickPts$id)
        
        rowID <- myClickPts$id
        # dim(mat_rh_small)
        # newPt <- data.frame( rh = suppN21, hm = unlist(as.numeric(mat_rh_small[myClickPts$id, -1])))
        # 
        # newPt$dif <- c(0, diff(newPt$hm)) # perfil
        # newPt$rh01 <-  c(x1/sum(xdif),0) # densidad energia
        # 
        x <- unlist(as.numeric(mat_rh_small[rowID, -1]))
        
        x12 <- c(x, max(x)+1)
        xdif <- c(0, diff(x), 0) # perfil
        x01 <-  c(xdif/sum(xdif)) # densidad energia
        x12 <- c(x, max(x)+1)
        newPt <- cbind.data.frame(x12, xdif, x01)
        #plot(x01, x12, type = 'l')
        
        hc <<- hc %>% hc_add_series(newPt, type = "line",
                                    hcaes(x = x01, y = x12 ),
                                    #tooltip = list(pointFormat = '{point.x}, <br>{point.y}: {point.value}'),
                                    maxSize = "5%", name = rowID, showInLegend = TRUE) %>% 
          hc_exporting(enabled = TRUE) 
        hc
      })
    }
  })
  
  # Profile curves v2 ---------------
  
  #observeEvent(input$varSelected, {
  
  #})
  
  observeEvent(input$mapleaf2_marker_click,{
    myClickPts <- input$mapleaf2_marker_click
    
    #print('Click 2') 
    if(!is.null(myClickPts$id)){
      #print(rowID)
      output$plotcurve2 <- renderHighchart({
        
        rowID <- myClickPts$id # rowID <- 8
        print(rowID)
        # dim(mat_rh_small)
        # newPt <- data.frame( rh = suppN21, hm = unlist(as.numeric(mat_rh_small[myClickPts$id, -1])))
        # 
        # newPt$dif <- c(0, diff(newPt$hm)) # perfil
        # newPt$rh01 <-  c(x1/sum(xdif),0) # densidad energia
        # 
        if( input$varSelected == 'RH'){
          x <- unlist(as.numeric(mat_rh_small[rowID, -1])) #skip ID
          hcol <- as.numeric(gsub(x = colnames(mat_rh_small)[-1], pattern = '_a0|[a-zA-Z]|_', ''))
          
        } else if (input$varSelected == 'PAI'){
          x <- unlist(as.numeric(mat_pai_small[rowID, -1]))[-1] #skip ID
          hcol <- as.numeric(gsub(x = colnames(mat_pai_small)[-1], pattern = '[a-zA-Z]|_', ''))[-1]
          
        } else if (input$varSelected == 'PAVD'){
          x <- unlist(as.numeric(mat_pav_small[rowID, -1])) #skip ID
          hcol <- as.numeric(gsub(x = colnames(mat_pav_small)[-1], pattern = '[a-zA-Z]|_', ''))
          
        } else if (input$varSelected == 'COV'){
          x <- unlist(as.numeric(mat_cov_small[rowID, -1])) #skip ID
          hcol <- as.numeric(gsub(x = colnames(mat_cov_small)[-1], pattern = '[a-zA-Z]|_', ''))
        }
        
        
        if( input$varSelected != 'RH' ){
          hcol <- c(hcol, tail(hcol, 1))
          x12 <- c(x, tail(x, 1))
          xdif <- c(0, diff(x), 0) # perfil
          x01 <-  c(xdif/sum(xdif)) # densidad energia
          xp <-  x12/sum(x12) # densidad energia
          
          # str(hcol)
          # str(x12)
          # str(xdif)
          # str(x01)
          # str(xp)
          
          newPt <- cbind.data.frame(hcol, x12, xdif, x01, prp = xp, abs = x12)
          # plot(x12, hcol, type = 'b')
          # plot(xp, hcol, type = 'b')
          # plot(xdif, hcol, type = 'b')
          # plot(x01, hcol, type = 'b')
        } else {
          
          x12 <- c(x, max(x)+1)
          xdif <- c(0, diff(x), 0) # perfil
          x01 <-  c(xdif/sum(xdif)) # densidad energia
          x12 <- c(x, max(x)+1)
          newPt <- cbind.data.frame(hcol = x12, x12, xdif, x01, prp = x01, abs = xdif)
          # plot(x01, x12, type = 'l')
          # plot(xdif, x12, type = 'l')
          # sum(x01)
        }
        #pairs(newPt)
        
        if( input$eneSelected == 'Proportional'){
          newPt$x <- newPt$prp
        } else if (input$eneSelected == 'Absolute'){
          newPt$x <- newPt$abs
        }
        
        updateSliderInput(session, inputId = "yAxisSlider3", 
                          value = round(max(me_curve$rh, na.rm = TRUE), 3),
                          min = 0, max = round(max(me_curve$rh, na.rm = TRUE), 3), step = 1)
        
        updateSliderInput(session, inputId = "xAxisSlider3", 
                          value = max(me_curve$xAxis, na.rm = TRUE),
                          min = 0, max = max(me_curve$xAxis, na.rm = TRUE), step = .01)
        
        
        xmx <- max(c(max(newPt$x, na.rm = TRUE), max(hc$x$hc_opts$xAxis$max, na.rm = TRUE)))
        hc <<- hc %>% 
          hc_add_series(newPt, type = "line",
                        hcaes(x = x, y = hcol ),
                        #tooltip = list(pointFormat = '{point.x}, <br>{point.y}: {point.value}'),
                        maxSize = "5%", name = rowID, showInLegend = TRUE) %>%
          hc_xAxis(max = xmx, min = 0, 
                   title = list(text = input$eneSelected)) %>%
          hc_exporting(enabled = TRUE) 
        
        hc
      })
    }
  })
  
  
  # Profile curves v3 ---------------
  
  
  observeEvent(input$selEco, {
    hc3 <<- highchart()
    output$mapleaf3 <- renderLeaflet({ 
      #input$selEco <- 'Zona pantanosa' # uEcoSin
      keepEcos <- na.omit(unique(subset(uEcoDf, ECOS_SINTE == input$selEco)$regionF))
      # delEcos <- na.omit(unique(subset(uEcoDf, ECOS_SINTE != input$selEco)$regionF))
      # delEcos2 <- setdiff(delEcos, keepEcos)
      # 
      # unique(regSim$regionF) %in% keepEcos
      # regSimLeaf %>%  removeShape(delEcos2)
      
      regSimLeaf3 <<- leaflet() %>% addTiles() %>%
        addPolygons(data = regSim[regSim$regionF %in% keepEcos, ],
                    label = ~htmlEscape(regSim$region),
                    popup = paste0("<strong>", regSim$region,"</strong>"),
                    group = input$selEco,
                    fillOpacity = .9, 
                    color = ~pal(regSim$regionF), 
                    layerId = ~regionF) %>%
        addLayersControl(overlayGroups = c( input$selEco),
                         #position = "topleft",
                         baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )
      # print(paste('Post leaf: ', nrow(eco.i)))
      
      regSimLeaf3
      
    })
  })
  
  observeEvent(input$mapleaf3_shape_click,{
    myClickPol3 <<- input$mapleaf3_shape_click
    #print(myClickPts)
    
    print('Click 3') 
    if(!is.null(myClickPol3$id)){
      
      output$plotcurve3 <- renderHighchart({
        
        regID <<- myClickPol3$id; # ecoID <- 453; regID <- 45
        #input <- list(selEco = 'Bosque')
        selEcoID <- subset(uEcoDf, ECOS_SINTE == input$selEco & regionF == regID)[1, ]
        ecoID3 <<- selEcoID$ecos_regF
        selReg <<- selEcoID$region
        print('Click 3A') 
        if (!file.exists(paste0(rdatPath, '/rdat_2b/md_curve_', ecoID3,'.RData'))){
          highEmpty
        } else {
          load(paste0(rdatPath, '/rdat_2b/md_curve_', ecoID3,'.RData')) # me_pavd; me_cov; me_pai; me
          print('Click 3B') 
          if( input$varSelected3 == 'RH'){
            me_rh$me <- me_rh$me3
            me_rh$av <- me_rh$av3
            me_curve <<- me_rh
          } else if (input$varSelected3 == 'PAI'){
            me_curve <<- subset(me_pai, rh != 0)
          } else if (input$varSelected3 == 'PAVD'){
            me_curve <<- subset(me_pavd, rh != 0)
          } else if (input$varSelected3 == 'COV'){
            me_curve <<- subset(me_cov, rh != 0)
          }
          
          print('Click 3C') 
          if( input$eneSelected3 == 'Proportional'){
            me_curve$xAxis <- me_curve$me/sum(me_curve$me)
            me_curve$xAxisAv <- me_curve$av/sum(me_curve$av)
          } else if (input$eneSelected3 == 'Absolute'){
            me_curve$xAxis <- me_curve$me
            me_curve$xAxisAv <- me_curve$av
          }
          print('Click 3D') 
          updateSliderInput(session, inputId = "yAxisSlider3", 
                            value = round(max(me_curve$rh, na.rm = TRUE), 3),
                            min = 0, max = round(max(me_curve$rh, na.rm = TRUE), 3), step = 1)
          
          updateSliderInput(session, inputId = "xAxisSlider3", 
                            value = max(me_curve$xAxis, na.rm = TRUE),
                            min = 0, max = max(me_curve$xAxis, na.rm = TRUE), step = .01)
          
          if(!exists('hc3')) {
            hc3 <<- highchart() %>% hc_exporting(enabled = TRUE) 
          }
          
          xmx <- round(max(c(max(c(me_curve$xAxisAv, me_curve$xAxis),
                                 na.rm = TRUE), max(hc3$x$hc_opts$xAxis$max, na.rm = TRUE))), 3)
          
          print('Click 3E') 
          hc3 <<- hc3 %>%
            hc_add_series(me_curve, type = "line",  
                          hcaes(x = xAxis, y = rh), #value = count, color = count, size = count),
                          #tooltip = list(pointFormat = '{point.x}, <br>{point.y}: {point.value}'),
                          maxSize = "5%", name = paste("Mdn ", selReg), 
                          showInLegend = TRUE) %>%
            hc_add_series( me_curve, type = "line", dashStyle = "DashDot",
                           hcaes(x = (xAxisAv), y = rh), #color = 'green',
                           #tooltip = list(pointFormat = '{point.x},<br> {point.y}: <br>{point.value}'),
                           name = paste("Avg ", selReg), 
                           showInLegend = TRUE) %>%
            hc_title(text = paste0( input$varSelected3, " profile curve")) %>%
            #hc_tooltip(formatter = fntlt, snap = 0, stickyTracking = FALSE) %>%
            hc_yAxis(max = max(me_curve$rh, na.rm = TRUE), min = 0, 
                     title = list(text = input$varSelected3)) %>%
            hc_xAxis(max = xmx, min = 0, 
                     title = list(text = paste(input$eneSelected3, "energy"))) %>% hc_exporting(enabled = TRUE) 
          hc3
        }
        
      }) ## chart
    }
  })
  
  
  
  
  # Profile reset  ---------------
  output$plotcurve <- renderHighchart({
    highNull
  })
  
  output$plotcurve2 <- renderHighchart({
    highNull
  })
  
  output$plotcurve3 <- renderHighchart({
    highNull
  })
  
  # Profile reset clear map  ---------------
  isolate(observeEvent(input$clearMap,{ # Return to simple region map
    ecoID <<-0   
    output$mapleaf <- renderLeaflet({
      regSimLeaf 
    })
    output$plotcurve <- renderHighchart({
      highNull
    })
  }))
  
  isolate(observeEvent(input$clearMap2,{ # Return to simple region map
    ecoID <<-0   
    output$mapleaf2 <- renderLeaflet({
      regSimLeaf 
    })
    output$plotcurve2 <- renderHighchart({
      highNull
    })
  }))
  
  # Profile reset clear eco  ---------------
  
  isolate(observeEvent(input$clearEco,{ # Return to ecosystems in the region
    if(regID != 0){
      ecoID <<-0   
      output$mapleaf <- renderLeaflet({ # studymap: mapleaf
        regEcoLeaf
      })
    }
  }))
  
  isolate(observeEvent(input$clearEco2,{ # Return to ecosystems in the region
    if(regID != 0){
      ecoID <<-0   
      output$mapleaf2 <- renderLeaflet({ # studymap: mapleaf
        regEcoLeaf
      })
    }
  }))
  
  # Profile reset clear curve  ---------------
  
  isolate(observeEvent(input$clearCur,{
    output$plotcurve <- renderHighchart({
      hc <<- hc0
      hc
    })
  }))
  
  isolate(observeEvent(input$clearCur2,{
    output$plotcurve2 <- renderHighchart({
      hc <<- hc2
      hc
    })
  }))
  
  isolate(observeEvent(input$clearCur3,{
    output$plotcurve3 <- renderHighchart({
      hc <<- highNull
      hc
    })
  }))
  
  
  
  
  ## SERVER Similarity click biom -------------------------
  
  #output$leafPts <- renderLeaflet({
  observeEvent(input$leafPts_shape_click, {
    
    clickReg <- input$leafPts_shape_click
    #save(clickReg, xyDf, file = 'clickReg.RData')
    #load('clickReg.RData')
    #print(clickReg)
    selBiom <- regSim@data[which(regSim$regionF == clickReg$id)[1], ]
    #str(xyDf)
    xyBiom <-  xyDf[grep(selBiom$region, xyDf$biom), c('X1', 'Y1','sortID','isPnn')]
    # print(nrow(xyBiom))
    if(nrow(xyBiom)>= 3){
      output$plot <- renderPlotly({
        
        ch_bio <- chull(xyBiom[, c('X1', 'Y1')])
        coord_ch <- xyBiom[c(ch_bio, ch_bio[1]), 1:2]
        
        colBiom <- palCom(selBiom$idrast)
        
        biomName <- iconv(selBiom$name, from = 'utf8')
        
        
        PL %>% add_trace(data = xyBiom, type = "scatter", mode = "markers",
                         name = biomName, text = ~sortID,
                         symbol = ~isPnn+0, symbols = c('circle', 'triangle-up'),
                         x = ~X1, y = ~Y1,
                         marker = list(color = 'purple'#colBiom
                         )) %>% 
          add_polygons(x=coord_ch[, 1], y=coord_ch[, 2],
                       line=list(width=2,color="black"),name = paste0('Biom-', biomName),
                       fillcolor='purple',#colBiom,
                       opacity = .5, inherit = FALSE)
      })
    }
    
  })
  
  ## SERVER Similarity Scatter -------------------------
  output$plot <- renderPlotly({
    if (input$in_bin != bin_selected){
      bin_selected <<- input$in_bin
      load(paste0(rdatPath, '/umap_', gediType, '/umap_', bin_selected,'.RData')) # "xyDf" "st"
      xyDf$sortID <- 1:nrow(xyDf)
      colnames(xyDf) <- gsub('_a1', '', colnames(xyDf))
      #head(xyDf)
      #xyDfEco <- strsplit(sub(' - ','xxxyyy', xyDf$ecoID), 'xxxyyy')
      xyDfEco <- strsplit(xyDf$ecoID, ' - ')
      xyDfEco2 <- do.call(rbind, lapply(xyDfEco, function(x){
        if(length(x) == 3){
          c(paste0(x[1], '--', x[2]),  x[3])
        } else if (length(x) == 2){
          x
        }
      }))
      
      xyDf$biom <- xyDfEco2[, 1]
      xyDf$eco <- xyDfEco2[, 2]
      xyDf <<- xyDf
      #apply(xyDfEco2, 2, function(x) length(unique(x)))
    }
    
    # input <- list(in_bin = 100, in_x1 = 'u1', in_y2 = 'u2', in_r = 'u1', in_g = 'u2', in_b = 'rh_50')
    
    if(!identical(plotParams, 
                  c(input$in_b, input$in_r, input$in_g, input$in_x1,
                    input$in_y2, input$in_bin))){
      plotParams <<- c(input$in_b, input$in_r, input$in_g, input$in_x1, input$in_y2, input$in_bin)
    }
    
    
    #input <- list(in_x1 = 'u1', in_y2 = 'u2', in_r = 'u1', in_g = 'u2', in_b = 'rh_100')
    
    xg <<- input$in_x1; # xg = 'u1'
    yg <<- input$in_y2 # yg = 'u2'
    sortID <<- 'sortID'
    rgb_r <<- xyDf[, gsub(' - rev', '', input$in_r)]
    rgb_g <<- xyDf[, gsub(' - rev', '', input$in_g)]
    rgb_b <<- xyDf[, gsub(' - rev', '', input$in_b)]
    
    rgb_r <<- (rgb_r - min(rgb_r, na.rm = TRUE))/(max(rgb_r, na.rm = TRUE) - min(rgb_r, na.rm = TRUE))
    rgb_g <<- (rgb_g - min(rgb_g, na.rm = TRUE))/(max(rgb_g, na.rm = TRUE) - min(rgb_g, na.rm = TRUE))
    rgb_b <<- (rgb_b - min(rgb_b, na.rm = TRUE))/(max(rgb_b, na.rm = TRUE) - min(rgb_b, na.rm = TRUE))
    
    if(any(grep(' - rev', input$in_r))){rgb_r <- 1 - rgb_r}
    if(any(grep(' - rev', input$in_g))){rgb_g <- 1 - rgb_g}
    if(any(grep(' - rev', input$in_b))){rgb_b <- 1 - rgb_b}
    
    # xyDf$colcol <- rgb(red = rgb_r, green = rgb_g, blue = rgb_b)
    xyDf$colcol <<- rgb(red = rgb_r, green = rgb_g, blue = rgb_b)
    
    # #print(head(xyDf))
    # p <- ggplot(data = xyDf, 
    #             aes_string(x = xg, y = yg
    #                        , key = sortID
    #                        )) + theme(legend.position = "none") +
    #   geom_point(size = .5) + 
    #   scale_color_manual(values = xyDf$colcol) +
    #   labs(x = xg, y = yg)
    # 
    # plot(xyDf[, c(xg, yg)], col = xyDf$colcol, pch = 20)
    # output$leafPts <- renderLeaflet({
    # ggplotly(p) %>% layout(dragmode = "lasso")
    
    ## SERVER Similarity Map -------------------------
    output$leafPts <- renderLeaflet({
      
      ptsLLCol <- rgb(red = rgb_r, green = rgb_g, blue = rgb_b)
      iconFiles <- pchIconsPng(pch = 16 + (xyDf$isPnn), width = 10, height = 10, col = (ptsLLCol))
      
      
      LL <<- leaflet() %>% addTiles() %>%
        addPolygons(data = regSim,
                    label = ~htmlEscape(regSim$region),
                    popup = paste0("<strong>", regSim$region,"</strong>"),
                    group = "Region",
                    fillOpacity = .9, 
                    color = ~pal(regSim$idrast), 
                    layerId = ~idrast) %>% 
        
        addMarkers(data = xyDf, lng = ~X, lat = ~Y, label = ~sortID,
                   icon = ~ icons(iconUrl = iconFiles),
                   group = 'Points'
        ) %>% addControl(html = build_legend(), position = "bottomright") %>% 
        addLayersControl(baseGroups = c("Esri.WorldImagery", "OpenStreetMap"),
                         overlayGroups = c("Region"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        
        hideGroup("Region") %>% addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
        setView(lng = -74, lat = 4.6, zoom = 6) %>%
        leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                       rectangleOptions = FALSE, circleOptions = FALSE,
                                       markerOptions = FALSE, circleMarkerOptions = FALSE,
                                       editOptions = leaflet.extras::editToolbarOptions())
      
      # LL <<- leaflet() %>% addTiles() %>% 
      #   addCircleMarkers(data = xyDf, lng = ~X, lat = ~Y, #popup = popupPts,#,
      #                    radius = ~ifelse(isPnn, 4, 2),
      #                    fillColor = rgb(red = rgb_r, green = rgb_g, blue = rgb_b),
      #                    fillOpacity = 0.6, weight=0,
      #                    color="white", opacity = .6, group = 'Points',
      #                    #color = colcol# stroke = FALSE, fillOpacity = 0.5
      #   ) %>% addLayersControl(baseGroups = c("Esri.WorldImagery", "OpenStreetMap"),
      #                          options = layersControlOptions(collapsed = FALSE)) %>%
      #   addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
      #   setView(lng = -74, lat = 4.6, zoom = 6) %>%
      #   leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
      #                                  rectangleOptions = FALSE, circleOptions = FALSE,
      #                                  markerOptions = FALSE, circleMarkerOptions = FALSE,
      #                                  editOptions = leaflet.extras::editToolbarOptions())
      # # print(c(input$in_b, input$in_r, input$in_g, input$in_x1, input$in_y2, input$in_bin))
      # # https://stackoverflow.com/questions/41512908/leaflet-legend-in-r-based-on-color-and-shape
      
      LL
      
      
    })
    
    
    #####
    xyDf$X1 <- xyDf[, input$in_x1] # xg
    xyDf$Y1 <- xyDf[, input$in_y2] # xg
    
    xyDf <<- xyDf
    
    PL <<- plot_ly(xyDf[, c('X1','Y1', 'sortID','isPnn', 'sortID')]) %>% 
      layout(dragmode = "lasso",
             xaxis = list(title = xg),
             yaxis = list(title = yg))
    
    # PL2 <<- PL %>% add_trace(type = "scatter", mode = "markers",
    #                       name = 'Points', text = ~sortID,
    #                       symbol = ~isPnn+0, symbols = c('circle', 'triangle-up'),
    #                       x = ~X1, y = ~Y1,
    #                       marker = list(color = rgb(red = rgb_r, green = rgb_g, blue = rgb_b))) 
    #   
    
    colCat <- 'eco'#  input$in_cat
    #print(colCat %in% colnames(xyDf))
    uCats <- unique(xyDf[, 'eco']) 
    
    for(l in uCats) {
      sub_xy <-  xyDf[xyDf[, colCat] == l, c('X1', 'Y1','sortID','isPnn')]
      ch_l2 <- chull(sub_xy[, c('X1', 'Y1')])
      coord_ch <- sub_xy[c(ch_l2, ch_l2[1]), 1:2]
      
      # PL2 <- PL2 %>%  add_polygons(x=coord_ch[, 1], y=coord_ch[, 2],
      #                      line=list(width=2,color="black"),name = l,
      #                        fillcolor='TTT', inherit = FALSE)
      colEco <- uEcoSinPal[uEcoSin %in% l]
      
      PL <- PL %>% add_trace(data = sub_xy, type = "scatter", mode = "markers",
                             name = l, text = ~sortID,
                             symbol = ~isPnn+0, symbols = c('circle', 'triangle-up'),
                             x = ~X1, y = ~Y1,
                             marker = list(color =
                                             rgb(
                                               red = rgb_r[xyDf[, colCat] == l],
                                               green = rgb_g[xyDf[, colCat] == l],
                                               blue = rgb_b[xyDf[, colCat] == l])
                             )) %>% 
        add_polygons(x=coord_ch[, 1], y=coord_ch[, 2],
                     line=list(width=2,color="black"),name = paste0(l, '-pol'),
                     fillcolor = colEco, opacity = .2, inherit = FALSE,
                     visible = "legendonly")
    }
    
    PL <<- PL
    PL
    
    
  })
  
  isolate(observeEvent(input$go_reset, { # go_showscatter
    
    output$plot <- renderPlotly({ PL })
    output$leafPts <- renderLeaflet({ LL })
    
  }))
  
  ###### GO map -------------
  isolate(observeEvent(input$go_showmap, { # go_showscatter
    d <- event_data("plotly_selected")
    if (!is.null(d)){
      if ( Sys.info()["sysname"] == "Windows"){
        save(xyDf, d, file = 'drawinscat.RData'); 
        #ss <- load('drawinscat.RData') # 18
      }
      #print(head(xyDf))
      #print(paste0(nrow(d)))
      d2 <- xyDf[d$pointNumber+1, c('X', 'Y', 'colcol', 'isPnn', 'rh_100')]
      #print(head(d))
      ch <- chull(d2[, c('X', 'Y')])
      coords <- d2[c(ch, ch[1]), c('X', 'Y')]
      # plot(coords, col = 2)
      
      sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
      sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
      
      
      output$barplot <- renderPlotly({ 
        fig <- plot_ly(d2, y = ~rh_100, color = ~isPnn, type = "box") %>% 
          layout(xaxis = list(
            title = 'is ProtArea?'
          ))
        fig
      })
      
      #proxy <- leafletProxy("leafPts"); # proxy <- LL
      
      leafletProxy("leafPts") %>% addPolygons(data = sp_poly_df, fillColor = 'purple',
                                              group = "Selected", opacity = .1, stroke = FALSE) %>% 
        addCircleMarkers(data = d2, lng = ~X, lat = ~Y, #popup = popupPts,#,
                         radius = ~ifelse(isPnn, 4, 2),
                         group = 'Selected', 
                         fillColor = d2$colcol,
                         fillOpacity = 0.6,
                         weight=0, color="white",
                         opacity = .6 #color = colcol# stroke = FALSE, fillOpacity = 0.5
        )  %>% 
        addLayersControl(overlayGroups = c('Region','Points', 'Selected'),
                         baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                         options = layersControlOptions(collapsed = FALSE))
      
    } 
  }))
  
  ###### GO scater -------------
  observeEvent(input$go_showscatter,{
    polDraw <- input$leafPts_draw_new_feature
    if( (!is.null(polDraw))) {
      if ( Sys.info()["sysname"] == "Windows"){
        save(polDraw, xyDf, file = 'drawinmap.RData'); 
        #ss <- load('drawinmap.RData') # 4
        #load('temp.RData')
      }
      coordMat <- as.data.frame(do.call(rbind, polDraw$geometry$coordinates[[1]]))
      coordMat <- data.frame(X = unlist(coordMat$V1), Y = unlist(coordMat$V2))
      
      posCoord <- which(
        xyDf$X <= max(coordMat$X) & # west
          xyDf$X >= min(coordMat$X) & # east
          xyDf$Y <= max(coordMat$Y) & # north
          xyDf$Y >= min(coordMat$Y) )# south
      
      xySel <- xyDf[posCoord, ]
      
      spDf <<- SpatialPolygonsDataFrame(
        SpatialPolygons(list(Polygons(list(Polygon(coordMat)), 1) # polgons
        )), data = data.frame(ID = 1), match.ID = FALSE)
      
      posSel <- sp::over(SpatialPoints(xySel[, c('X', 'Y')]), spDf)
      
      
      # plot(coordMat[, c('X', 'Y')], type = 'b')
      # points(xyDf[, c('X', 'Y')], col = 2, pch = 20)
      # points(xyDf[posCoord, c('X', 'Y')], col = 4, pch = 2)
      # points(xySel[which(!is.na(posSel$ID)), c('X', 'Y')], col = 1, pch = 2, cex = 2)
      
      xySel <<- xySel[which(!is.na(posSel$ID)), c(xg, yg, 'isPnn', 'rh_100')]
      ch2 <- chull(xySel[, c('X1', 'Y1')])
      coords2 <<- xySel[c(ch2, ch2[1]), c('X1', 'Y1')]
      
      # print(xg)
      # print(yg)
      # print(nrow(coords2))
      # print(nrow(xySel))
      # print(head(coords2))
      # print(head(xySel))
      
      PL_sel <<- PL %>% 
        #add_polygons(x=xySel[, xg], y=xySel[, yg], line=list(width=2,color="black"), fillcolor='purple', inherit = FALSE) %>% 
        add_polygons(x = coords2[, 1], y = coords2[, 2], 
                     line=list(width=2,color="purple"),
                     name = 'Selected',
                     fillcolor='transparent', inherit = FALSE) %>% 
        add_trace(data = xySel, type = "scatter", mode = "markers",
                  x = ~X1, y = ~Y1,
                  symbol = ~isPnn+0, symbols = c('circle', 'triangle-up'),
                  marker = list(color = 'purple', size = 10), name = 'Selected') 
      
      output$plot <- renderPlotly({ 
        if ( Sys.info()["sysname"] == "Windows"){
          save(coords2, xySel, file = 'selPolPlotly.RData')
          ss <- load(file = 'selPolPlotly.RData')
        }
        # print('--------------')
        # print(nrow(coords2))
        # print(nrow(xySel))
        # print(head(coords2))
        # print(head(xySel))
        
        PL_sel
        
      })
      
      output$barplot <- renderPlotly({ 
        fig <- plot_ly(xySel, y = ~rh_100, color = ~isPnn, type = "box") %>% 
          layout(xaxis = list(
            title = 'is ProtArea?'
          ))
        fig
      })
    }
  })
  
  
  
  
} # close


shinyApp(ui = ui, server = server)


###### LINUX SERVER COPY -------------

# sudo cp /home/vmuser/gedivis /srv/shiny-server/gedivis -R
# sudo cp /home/vmuser/gedivis/app.R /srv/shiny-server/gedivis/app.R


# sudo rm /home/shiny/tmpR/leafSim.RDatasudo cp /home/vmuser/gedivis /srv/shiny-server/gedivis -R
# sudo rm /var/log/shiny-server/gedivis/*
# #sudo rm /srv/shiny-server/gedivis/*
# sudo su - -c "R -e \"shinyParallel::installShinyParallel('/home/vmuser/gedivis/', max.sessions = 25)\"" # home/shinyusername/
# #sudo rm /srv/shiny-server/gedivis2 -R
# sudo cp /home/vmuser/gedivis /srv/shiny-server/gedivis2 -R
# sudo cat /var/log/shiny-server/gedivis_

# "ecoID:924" forest Zulia