
## prototype shiny app for intervention impact

library(shiny)
library(data.table)
library(rsconnect)
library(ggplot2)

library(raster)
library(rasterVis)
library(gridExtra)
library(MapSuite)
library(PNWColors)

theme_set(theme_minimal(base_size = 24))

shape_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/general/shapefiles/"
main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20201001_new_2020_dists/04_predictions/rasters/"
all_rasts <- list.files(main_dir)
exceed_rasts <- all_rasts[all_rasts %like% "exceed"]

# shapefile
Africa <- readOGR(file.path(shape_dir, "Africa_simplified.shp"))
Africa_dt <- data.table(fortify(Africa, region = "COUNTRY_ID"))
Africa_dt[, modeled:= ifelse(id %in% unique(cube_nat_level$iso3), "Yes", "No")]

load(file.path(main_dir, "../../final_plots/background_mask_dt.Rdata"))

find_pal <- function(variable, metric){
  
  if (metric=="mean"){
    if (variable %in% c("access", "use")){
      pal <- wpal("seaside", noblack = T)
    }else if (variable=="use_rate"){
      pal <- rev(pnw_palette("Sailboat", 30))
    }else if (variable == "percapita_nets"){
      pal <- rev(pnw_palette("Mushroom", 30))
    }
  }else if (metric=="neg_exceed"){
    pal <-brewer.pal(4, "YlGnBu")
  }else if (metric=="pos_exceed"){
    pal <- brewer.pal(4, "YlOrBr")
  }
  
  return(pal)
}

plot_raster <- function(year, variable, metric, cutoff=NULL){
  
  this_title <- ifelse(metric=="mean", "Mean Values", ifelse(metric=="neg_exceed", "Negative Exceedance", "Positive Exceedance"))
  
  cutoff_str <- ifelse(is.null(cutoff), "", paste0("_", cutoff))
  this_raster <- raster(file.path(main_dir, paste0("ITN_", year, "_", variable, "_", metric, cutoff_str, ".tif")))
  pal <- find_pal(variable, metric)
  
  this_dt <- data.table(rasterToPoints(this_raster))
  names(this_dt) <- c("long", "lat", "value")
  
  this_plot <- ggplot() +
    geom_raster(data = this_dt, aes(fill = value, y = lat, x = long)) +
    annotate(geom = "raster", x = background_mask_dt$long, y = background_mask_dt$lat, fill = "gray80") +
    geom_path(data = Africa_dt, aes(x = long, y = lat, group = group), color = "black", size = 0.3) + 
    scale_fill_gradientn(colors= pal, limits=c(0, 1)) +
    coord_equal(xlim = c(-18, 52), ylim = c(-35, 38)) +
    labs(x = NULL, y = NULL, title = this_title) +
    theme_classic(base_size = 12) +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "in"), legend.title=element_blank())
  
  return(this_plot)
  
}


years_available <- unique(as.integer(gsub("ITN_([0-9]{4})_.*", "\\1", exceed_rasts)))


ui <- fluidPage(
  
  plotOutput("maps", height="900px", width="1600px"),
  
  hr(),
  
  fluidRow(
    column(3, 
           sliderInput("year", h3("Year"),
                       min = 2000, max = max(years_available), value = max(years_available), step=1, sep="")
    ),
    column(2,
           radioButtons("variable", h3("Variable"),
                        choices = list("Access" = "access", 
                                       "Use" = "use",
                                       "Nets-Per-Capita" = "percapita_nets",
                                       "Use Rate" = "use_rate"),selected = "access")
    ),
    
    column(3, 
           sliderInput("negval", h3("Negative Exceedance Cutoff"),
                       min = 0.1, max = 0.9, value = 0.5, step=0.1)
    ),
    
    column(3, 
           sliderInput("posval", h3("Positive Exceedance Cutoff"),
                       min = 0.1, max = 0.9, value = 0.5, step=0.1)
    )
    
  )
  
  
)

server <- function(input, output){
  
  output$maps <- renderPlot({
    
    mean_plot <- reactive({
      plot_raster(input$year, input$variable, "mean")
    })
    
    neg_plot <- reactive({
      plot_raster(input$year, input$variable, "neg_exceed", input$negval)
    })
    
    pos_plot <- reactive({
      plot_raster(input$year, input$variable, "pos_exceed", input$posval)
    })
    
    do.call("grid.arrange", c(list(neg_plot(), mean_plot(), pos_plot()), ncol=3))

  })
  
}

shinyApp(ui, server)