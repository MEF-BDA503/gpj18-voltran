library(ggplot2)
library(tidyverse) 
library(sp)
library(mapproj)



total <- read.csv(
  "C:/Users/kerim.acar/Desktop/BDA/BDA503/Project/final_map.csv",sep = ",",
  colClasses=c("group"="factor"))

body <- read.csv(
  "C:/Users/kerim.acar/Desktop/BDA/BDA503/Project/body_final_map.csv",sep = ",",
  colClasses=c("group"="factor"))

face <- read.csv(
  "C:/Users/kerim.acar/Desktop/BDA/BDA503/Project/face_final_map.csv",sep = ",",
  colClasses=c("group"="factor"))

jewelry <- read.csv(
  "C:/Users/kerim.acar/Desktop/BDA/BDA503/Project/jewelry_final_map.csv",sep = ",",
  colClasses=c("group"="factor"))

fragrance <- read.csv(
  "C:/Users/kerim.acar/Desktop/BDA/BDA503/Project/fragrance_final_map.csv",sep = ",",
  colClasses=c("group"="factor"))

toiletries <- read.csv(
  "C:/Users/kerim.acar/Desktop/BDA/BDA503/Project/toiletries_final_map.csv",sep = ",",
  colClasses=c("group"="factor"))

color <- read.csv(
  "C:/Users/kerim.acar/Desktop/BDA/BDA503/Project/color_final_map.csv",sep = ",",
  colClasses=c("group"="factor"))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(
    "Company Sales Relative Frequency Distribution of Turkey by Categories"),
  
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
                                          fill = rel_freqs),
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
      ggplot(body)+geom_polygon( aes(x = long, y = lat, group = group, 
                                               fill = rel_freqs),
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
      ggplot(face)+geom_polygon( aes(x = long, y = lat, group = group, 
                                     fill = rel_freqs),
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
      ggplot(jewelry)+geom_polygon( aes(x = long, y = lat, group = group, 
                                     fill = rel_freqs),
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
      ggplot(fragrance)+geom_polygon( aes(x = long, y = lat, group = group, 
                                        fill = rel_freqs),
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
      ggplot(toiletries)+geom_polygon( aes(x = long, y = lat, group = group, 
                                          fill = rel_freqs),
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
      ggplot(color)+geom_polygon( aes(x = long, y = lat, group = group, 
                                           fill = rel_freqs),
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


