library(tidyverse)
library(here)
library(sf)
library(DT)
library(shiny)
library(shinydashboard)
library(mapdeck)

vessels <- read_rds(here("data/vessels.rds"))

ui <-
  dashboardPage(

    dashboardHeader(title = "Flotinn ósigrandi"),
    dashboardSidebar(
      radioButtons("trips", "Túrar: ",  choices = c("Síðasti" = 0,
                                                    "Síðustu tveir" = 1,
                                                    "Síðustu þrír" = 2,
                                                    "Síðustu fjórir" = 3)),
      selectInput("vids", "Skip á sjó", selectize = FALSE,
                  choices = vessels,
                  selected = vessels,
                  multiple = TRUE,
                  size = 30)
    ),
    dashboardBody(
      mapdeckOutput(outputId = "map", height = "900px")
    )
  )

server <- function(input, output, session) {


  vids_r <- reactive({
    input$vids
  })

  trips_r <- reactive({
    input$trips
  })

  trail.live <-
    reactiveFileReader(intervalMillis = 100,
                       session = session,
                       filePath = here("data/flotinnosigrandi.rds"),
                       readFunc = read_rds)


  selected <- reactive({
    trail.live() %>%
      filter(mid %in% vids_r()) %>%
      filter(trip <= trips_r())
  })

  output$map <- mapdeck::renderMapdeck({
    mapdeck::mapdeck(location = c(-20, 65), zoom = 4)
  })

  observeEvent(
    selected(),
    if(length(vids_r()) == 0) {
      mapdeck_update(map_id = "map") %>%
        mapdeck::clear_path(layer_id = "trail") %>%
        mapdeck::clear_scatterplot(layer_id = "trail") %>%
        mapdeck::clear_scatterplot(layer_id = "position")
    } else {
      mapdeck::mapdeck_update(map_id = "map") %>%
        mapdeck::add_path(data = selected() %>%
                            group_by(vessel) %>%
                            summarise(do_union = FALSE) %>%
                            st_cast("LINESTRING"),
                          layer_id = "trail",
                          stroke_width = 300,
                          width_min_pixels = 1,
                          width_max_pixels = 5,
                          tooltip = "vessel",
                          auto_highlight = TRUE,
                          highlight_colour = "#00000095",
                          update_view = FALSE,
                          stroke_colour = "#E0FFFF80") %>%
        # speed
        mapdeck::add_scatterplot(data =
                                   selected() %>%
                                   #arrange(speed) %>%
                                   mutate(pu = leafpop::popupTable(.,
                                                                   zcol = c("vessel", "time"),
                                                                   feature.id = FALSE,
                                                                   row.numbers = FALSE)),
                                 fill_colour = "speed",
                                 legend = TRUE,
                                 tooltip = "pu",
                                 layer_id = "trail",
                                 radius = 400,
                                 radius_min_pixels = 2,
                                 radius_max_pixels = 6,
                                 update_view = FALSE,
                                 stroke_opacity = 1,
                                 palette = "inferno") %>%
        # last position
        mapdeck::add_scatterplot(data =
                                   selected() %>%
                                   filter(time == max.time) %>%
                                   mutate(lon = round(lon, 2),
                                          lat = round(lat, 2)) %>%
                                   select(Skip = vessel,
                                          Nr = vid,
                                          Tími = time,
                                          Hraði = speed, Stefna = heading, lon, lat) %>%
                                   mutate(pu = leafpop::popupTable(.,
                                                                   zcol = c("Skip", "Nr", "Tími", "Hraði", "Stefna", "lon", "lat"),
                                                                   feature.id = FALSE,
                                                                   row.numbers = FALSE)),
                                 tooltip = "pu",
                                 legend = FALSE,
                                 layer_id = "position",
                                 radius = 3000,
                                 radius_min_pixels = 4,
                                 radius_max_pixels = 20,
                                 update_view = FALSE,
                                 fill_colour = "#ff00f080")    # pinkish
    }
  )
}

shinyApp(ui, server)
