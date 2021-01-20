
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

theme_set(theme_minimal(base_size = 18))

main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20201001_new_2020_dists/04_predictions/rasters/"
all_rasts <- list.files(main_dir)
exceed_rasts <- all_rasts[all_rasts %like% "exceed"]


find_pal <- function(variable){
    if (variable %in% c("access", "use")){
      pal <- wpal("seaside", noblack = T)
    }else if (variable=="use_rate"){
      pal <- c("#722503", "#AB0002", "#F2A378", "#F4CA7D", "#C8D79E", "#70A800")
    }else if (variable == "percapita_nets"){
      pal <- rev(pnw_palette("Mushroom", 30))
    }
  return(pal)
}


years_available <- unique(as.integer(gsub("ITN_([0-9]{4})_.*", "\\1", exceed_rasts)))


ui <- fluidPage(
  
  plotOutput("maps", height="600px"),
  
  hr(),
  
  fluidRow(
    column(3, 
           sliderInput("year", h3("Year"),
                       min = 2000, max = max(years_available), value = max(years_available))
    ),
    column(2,
           radioButtons("metric", h3("Metric"),
                        choices = list("Access" = "access", 
                                       "Use" = "use",
                                       "Nets-Per-Capita" = "percapita_nets",
                                       "Use Rate" = "use_rate"),selected = "access")
    ),
    
    column(3, 
           sliderInput("negval", h3("Negative Exceedance Cutoff"),
                       min = 0.1, max = 0.9, value = 0.5)
    ),
    
    column(3, 
           sliderInput("posval", h3("Positive Exceedance Cutoff"),
                       min = 0.1, max = 0.9, value = 0.5)
    )
    
  )
  
  
)

server <- function(input, output){
  
  output$maps <- renderPlot({
    
    mean_raster <- raster(file.path(main_dir, paste0("ITN_", input$year, "_", input$metric, "_mean.tif")))
    pal <- find_pal(input$metric)
    
    mean_dt <- data.table(rasterToPoints(mean_raster))
    names(mean_dt) <- c("long", "lat", "value")
    
    mean_plot <- ggplot() +
      geom_raster(data = mean_dt, aes(fill = value, y = lat, x = long)) +
      # annotate(geom = "raster", x = background_mask_dt$long, y = background_mask_dt$lat, fill = "gray80") +
      # geom_path(data = Africa_dt, aes(x = long, y = lat, group = group), color = "black", size = 0.3) + 
      scale_fill_gradientn(colors= pal, limits=c(0, 1)) +
      coord_equal(xlim = c(-18, 52), ylim = c(-35, 38)) +
      labs(x = NULL, y = NULL, title = "Mean Values:") +
      theme_classic(base_size = 12) +
      theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "in"), legend.title=element_blank())
    
    
    
    
    neg_raster <- raster(file.path(main_dir, paste0("ITN_", input$year, "_", input$metric, "_neg_exceed_", input$negval, ".tif")))
    neg_dt <- data.table(rasterToPoints(neg_raster))
    names(neg_dt) <- c("long", "lat", "value")
    
    neg_plot <- ggplot() +
      geom_raster(data = neg_dt, aes(fill = value, y = lat, x = long)) +
      # annotate(geom = "raster", x = background_mask_dt$long, y = background_mask_dt$lat, fill = "gray80") +
      # geom_path(data = Africa_dt, aes(x = long, y = lat, group = group), color = "black", size = 0.3) + 
      scale_fill_gradientn(colors= brewer.pal(4, "YlGnBu"), limits=c(0, 1)) +
      coord_equal(xlim = c(-18, 52), ylim = c(-35, 38)) +
      labs(x = NULL, y = NULL, title = "Negative Exceed:") +
      theme_classic(base_size = 12) +
      theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "in"), legend.title=element_blank())
    
    
    
    pos_raster <- raster(file.path(main_dir, paste0("ITN_", input$year, "_", input$metric, "_pos_exceed_", input$posval, ".tif")))
    pos_dt <- data.table(rasterToPoints(pos_raster))
    names(pos_dt) <- c("long", "lat", "value")
    
    pos_plot <- ggplot() +
      geom_raster(data = pos_dt, aes(fill = value, y = lat, x = long)) +
      # annotate(geom = "raster", x = background_mask_dt$long, y = background_mask_dt$lat, fill = "gray80") +
      # geom_path(data = Africa_dt, aes(x = long, y = lat, group = group), color = "black", size = 0.3) + 
      scale_fill_gradientn(colors= brewer.pal(4, "YlOrBr"), limits=c(0, 1)) +
      coord_equal(xlim = c(-18, 52), ylim = c(-35, 38)) +
      labs(x = NULL, y = NULL, title = "Positive Exceed:") +
      theme_classic(base_size = 12) +
      theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "in"), legend.title=element_blank())
    
    do.call("grid.arrange", c(list(neg_plot, mean_plot, pos_plot), ncol=3))

  })
  
}

shinyApp(ui, server)