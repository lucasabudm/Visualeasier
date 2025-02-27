#Required libraries are requested and installed if necessary
if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readr")) install.packages("readr")
if (!require("reshape2")) install.packages("reshape2")
if (!require("raster")) install.packages("raster")
if (!require("data.table")) install.packages("data.table")
if (!require("patchwork")) install.packages("patchwork")

#Libraries are declared
library(shiny)
library(shinythemes)
library(tidyverse)
library(readr)
library(reshape2)
library(raster)
library(data.table)
library(patchwork)

# User Interface is defined
ui <- fluidPage(theme = shinytheme("yeti"), # Themes
                navbarPage("Visualeasier", # Project Title
                           tabPanel("Upload Dataset", # Initialization tab, responsible for receiving user data
                                    sidebarPanel( h2("Input data", align = "center"),# Side Panel, requesting elements and datasets
                                                  selectInput("element", "Elements to map:", selected = NULL, multiple = TRUE, selectize = TRUE,choices = c("Al", "Ca", "Fe", "K", "Mg", "Mn", "S", "Si", "Ti")),
                                                  uiOutput("file_inputs"),
                                                  uiOutput("element_inputs"),
                                                  uiOutput("dynamic_inputs")
                                    ),
                                    mainPanel(h3(HTML("<b>Preview</b>"), align = "center"), # Main panel, where plots will be displayed
                                              mainPanel(
                                                uiOutput("graph_outputs") # Plot output where first results are previewed
                                              )
                                    )
                           ),
                           
                           tabPanel("Download", # Download tab, a panel for downloading the plots
                                    mainPanel(h3(HTML("<b>Visualisation</b>"), align = "center"),
                                              sidebarPanel(
                                                h2("Download plots", align = "center"),
                                                h5("(Before attempting to download, please go to the Upload Dataset tab)", align = "center"),
                                                hr(),
                                                downloadButton("download_plot", "Download Combined Plot") # Action button allowing user to download final plot
                                              ),
                                              mainPanel(
                                                plotOutput("combined_plot", width = "100%", height = "800px") # Final plot output
                                              )
                                    )
                           ),
                           # Help tab, providing general information about the project and user guide
                           tabPanel("Help", "This application is designed to help users visualize Geochemical analysis results.",
                                    hr(),
                                    "The first step is to insert required data at the Upload Dataset panel, confirm the preview and then go to the Download panel to see the compiled plot and download it.",
                                    "It's recommended to wait a few seconds before the plot appears on the screen.")
                )
)

# Server is defined
server <- function(input, output, session) {
  
  stored_plots <- reactiveVal(list()) # Reactive variable to store plots
  
  output$file_inputs <- renderUI({ # Upload gadget responsible for collecting user input data
    num <- length(input$element) # Number of elements selected for the mapping
    
    file_inputs <- lapply(1:num, function(i) {
      fileInput(paste0("file_", i), paste("Upload file to map", i, ": ", input$element[i]), accept = c(".csv", ".txt"))
    })
    
    do.call(tagList, file_inputs)
  })
  
  
  datasets <- reactive({ # Reactive variable to handle the input format
    num <- length(input$element)
    lapply(1:num, function(i) {
      file_input <- input[[paste0("file_", i)]]
      req(file_input)
      file_path <- file_input$datapath 
      mtx <- as.matrix(fread(file_path))
      return(mtx)
    })
  })
  
  output$graph_outputs <- renderUI({ # Plot output
    req(datasets())
    num <- length(input$element)
    
    graph_list <- lapply(1:num, function(i) { # First input transformation to raster data
        data <- datasets()[[i]]
        rst <- raster(data)
        df <- as.data.frame(rst, xy = TRUE) # Second transformation to data frame
        
        ggplot(df, aes(x,y, fill = layer)) + # Plotting
          geom_tile() +
          coord_equal() +
          scale_fill_gradientn(colours = pals::turbo()) +
          ggpubr::theme_pubr()+
          labs(title = paste0("Map ", i, " (", input$element[i], ")"), fill = input$element[i])+
          theme(plot.title = element_text(hjust = 0.5))
        
      })
      stored_plots(graph_list) # Plot reactive storage
      do.call(tagList, lapply(graph_list, function(g) {
        renderPlot({ g })
      }))
    })
  
  output$combined_plot <- renderPlot({ # Rendering combined plots
    req(stored_plots())  # Receiving reactive variable
    num <- length(input$element)
    col <- floor(num / 2)
    
    plot_list <- stored_plots()  # Storing reactive plots
    wrap_plots(plot_list, ncol = col) +  # Combining plots
      plot_layout(widths = unit(c(2, 2), "null"))+ # Adjusting width
      theme(
        axis.title.x = element_text(margin = margin(t = 0.01)),  # Adjusting plot spacing and margin
        plot.margin = margin(0.01, 0.01, 0.01, 0.01)
      )

  })
  
  output$download_plot <- downloadHandler( # Download button
    plot_name <- paste(input$element, collapse = "_"), # Predefined file name based on previous user selection
    filename = function() {
      paste("plot_", plot_name, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) { # GGSAVE
      ggsave(file, plot = last_plot(), width = 5 * 2, height = 5 * 2, dpi = 300, bg = "white")
      
    }
  )
  
  
}
shinyApp(ui = ui, server = server) # Shiny App structure
