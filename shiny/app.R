library(ggplot2)
library(tidyverse) 
library(sp)
library(mapproj)

options(shiny.sanitize.errors = TRUE)

total <- read.csv(
  "data/total_final.csv",sep = ",",
  colClasses=c("group"="factor"))




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(
    "Relative Frequency Distribution of Company Sales by Categories"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("categories",
                  "categories",
                  choices = c("ALL","BODY","FACE","JEWELRY","FRAGRANCE",
                              "TOILETRIES","COLOR"))

    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$mapPlot <- renderPlot({

    if(input$categories == "ALL") {
      ggplot(total)+geom_polygon( aes(x = long, y = lat, group = group, 
                                          fill = total),
                                      color = "grey") +
        coord_map() +theme_void() + labs(
          title = "Relative Frequency Distribution of ALL Sales") +
        scale_fill_distiller(name = "Frequency Table",palette = "Spectral", 
                             limits = c(0,0.065),
                             na.value = "red") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else if (input$categories == "BODY") {
      ggplot(total)+geom_polygon( aes(x = long, y = lat, group = group, 
                                               fill = body),
                                           color = "grey") +
        coord_map() +theme_void() + labs(
          title = "Relative Frequency Distribution of BODY Sales") +
        scale_fill_distiller(name = "Frequency Table",palette = "Spectral", 
                             limits = c(0,0.08),
                             na.value = "red") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else if (input$categories == "FACE") {
      ggplot(total)+geom_polygon( aes(x = long, y = lat, group = group, 
                                     fill = face),
                                 color = "grey") +
        coord_map() +theme_void() + labs(
          title = "Relative Frequency Distribution of FACE Sales") +
        scale_fill_distiller(name = "Frequency Table",palette = "Spectral", 
                             limits = c(0,0.06),
                             na.value = "red") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else if (input$categories == "JEWELRY") {
      ggplot(total)+geom_polygon( aes(x = long, y = lat, group = group, 
                                     fill = jewelry),
                                 color = "grey") +
        coord_map() +theme_void() + labs(
          title = "Relative Frequency Distribution of JEWELRY Sales") +
        scale_fill_distiller(name = "Frequency Table",palette = "Spectral", 
                             limits = c(0,0.07),
                             na.value = "red") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else if (input$categories == "FRAGRANCE") {
      ggplot(total)+geom_polygon( aes(x = long, y = lat, group = group, 
                                        fill = fragrance),
                                    color = "grey") +
        coord_map() +theme_void() + labs(
          title = "Relative Frequency Distribution of FRAGRANCE Sales") +
        scale_fill_distiller(name = "Frequency Table",palette = "Spectral", 
                             limits = c(0,0.06),
                             na.value = "red") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else if (input$categories == "TOILETRIES") {
      ggplot(total)+geom_polygon( aes(x = long, y = lat, group = group, 
                                          fill = toiletries),
                                      color = "grey") +
        coord_map() +theme_void() + labs(
          title = "Relative Frequency Distribution of TOILETRIES Sales") +
        scale_fill_distiller(name = "Frequency Table",palette = "Spectral", 
                             limits = c(0,0.065),
                             na.value = "red") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    else if (input$categories == "COLOR") {
      ggplot(total)+geom_polygon( aes(x = long, y = lat, group = group, 
                                           fill = color),
                                       color = "grey") +
        coord_map() +theme_void() + labs(
          title = "Relative Frequency Distribution of COLOR Sales") +
        scale_fill_distiller(name = "Frequency Table",palette = "Spectral", 
                             limits = c(0,0.05),
                             na.value = "red") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


