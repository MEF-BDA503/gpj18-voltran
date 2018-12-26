### project BDA 503
library(ggplot2)
library(tidyverse) 
library(sp)
library(mapproj)
library(GGally)

### 1) data reading sales & map
sales_data<-readxl::read_excel(
  "C:/Users/kerim.acar/Desktop/BDA/BDA503/Project/sales_data_v10.xlsx",
                              col_names=TRUE)

TUR <- readRDS("C:/Users/kerim.acar/Desktop/BDA/BDA503/gadm36_TUR_1_sp.rds")

### 2) data cleaning
# sales to numeric
sales_data$SALES <- as.numeric(gsub(",", ".", gsub("\\.", "", sales_data$SALES)))
# map data as tibble
TUR@data %>% as_tibble() %>% head(10)
# fortify TUR data
TUR_for <- fortify(TUR)
# character function
turkceden_ingilizceye <- function(dataset){
  turkce_harfler<- c("Ç","Þ","Ð","Ý","Ü","Ö","ç","þ","ð","ý","ü","ö")
  ingilizce_harfler<- c("C","S","G","I","U","O","c","s","g","i","u","o")
  dataset=mgsub(turkce_harfler,ingilizce_harfler,dataset)
  return(dataset)
}
# replacement function
mgsub <- function(pattern, replacement, x, ...) {
  n = length(pattern)
  if (n != length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result = x
  for (i in 1:n) {
    result <- gsub(pattern[i],replacement[i],result)
  }
  return(result)
}
# string operations
TUR@data$NAME_1 <- turkceden_ingilizceye(TUR@data$NAME_1 )
TUR@data$NAME_1 <- gsub("K. Maras", "Kahramanmaras",TUR@data$NAME_1 )
TUR@data$NAME_1 <- gsub("Kinkkale","Kirikkale",TUR@data$NAME_1 )
TUR@data$NAME_1 <- gsub("Zinguldak", "Zonguldak", TUR@data$NAME_1 )
TUR@data$NAME_1 <- gsub("Afyon","Afyonkarahisar", TUR@data$NAME_1 )
sales_data$sehir <- turkceden_ingilizceye(sales_data$CITY)
sales_data %>% as_tibble

### 3) data manipulation part I: total sales by cities (rel_freqs)
# get total sales & rel_freqs
total_sales <- sales_data %>% 
  group_by(sehir) %>% 
  summarise(SALES = sum(SALES)) %>%
  mutate(rel_freqs = SALES / sum(SALES)) 
# TUR & total sales data joint
id_and_cities <- data_frame(id = rownames(TUR@data), sehir = TUR@data$NAME_1) %>% 
  left_join(total_sales, by = "sehir")
# last join (TUR data & id_cities)
final_map <- left_join(TUR_for, id_and_cities, by = "id")
# save final map data as .csv
write.csv(final_map, file = "final_map.csv")

### 4) mapPlot I: total sales as rel_freqs by cities
ggplot(final_map)+geom_polygon( aes(x = long, y = lat, group = group, 
                                    fill = rel_freqs),
                                color = "grey") +
  coord_map() +theme_void() + labs(title = "Total sales by cities") +
  scale_fill_distiller(name = "Total Sales",palette = "Spectral", 
                       limits = c(0,0.07),
                       na.value = "red") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

### 5) data manipulation part II: total sales by categories rel_freqs
# BODY total sales
body <- sales_data %>%
  filter(CATEGORY_ == "BODY") %>%
  group_by(sehir) %>%
  summarise(SALES = sum(SALES)) %>%
  mutate(rel_freqs = SALES / sum(SALES)) %>%
  arrange(desc(rel_freqs))
# TUR & total sales merger
body_id_and_cities <- data_frame(id = rownames(TUR@data), sehir = TUR@data$NAME_1) %>% 
  left_join(body, by = "sehir")
# last join (TUR data & id_cities)
body_final_map <- left_join(body_id_and_cities, TUR_for, by = "id")
# save final map data as .csv
write.csv(body_final_map, file = "body_final_map.csv")
# mapPlot
ggplot(body_final_map)+geom_polygon( aes(x = long, y = lat, group = group, 
                                    fill = rel_freqs),
                                color = "grey") +
  coord_map() +theme_void() + labs(title = "Total sales by cities") +
  scale_fill_distiller(name = "Total Sales",palette = "Spectral", 
                       limits = c(0,0.08),
                       na.value = "red") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


### 6) data manipulation part II: total sales by categories rel_freqs
# FACE total sales
face <- sales_data %>%
  filter(CATEGORY_ == "FACE") %>%
  group_by(sehir) %>%
  summarise(SALES = sum(SALES)) %>%
  mutate(rel_freqs = SALES / sum(SALES)) %>%
  arrange(desc(rel_freqs))
# TUR & total sales merger
face_id_and_cities <- data_frame(id = rownames(TUR@data), sehir = TUR@data$NAME_1) %>% 
  left_join(face, by = "sehir")
# last join (TUR data & id_cities)
face_final_map <- left_join(face_id_and_cities, TUR_for, by = "id")
# save final map data as .csv
write.csv(face_final_map, file = "face_final_map.csv")
# mapPlot
ggplot(face_final_map)+geom_polygon( aes(x = long, y = lat, group = group, 
                                         fill = rel_freqs),
                                     color = "grey") +
  coord_map() +theme_void() + labs(title = "Total sales by cities") +
  scale_fill_distiller(name = "Total Sales",palette = "Spectral", 
                       limits = c(0,0.06),
                       na.value = "red") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


### 7) data manipulation part II: total sales by categories rel_freqs
# JEWELRY total sales
jewelry <- sales_data %>%
  filter(CATEGORY_ == "JEWELRY") %>%
  group_by(sehir) %>%
  summarise(SALES = sum(SALES)) %>%
  mutate(rel_freqs = SALES / sum(SALES)) %>%
  arrange(desc(rel_freqs))
# TUR & total sales merger
jewelry_id_and_cities <- 
  data_frame(id = rownames(TUR@data), sehir = TUR@data$NAME_1) %>% 
  left_join(jewelry, by = "sehir")
# last join (TUR data & id_cities)
jewelry_final_map <- left_join(jewelry_id_and_cities, TUR_for, by = "id")
# save final map data as .csv
write.csv(jewelry_final_map, file = "jewelry_final_map.csv")
# mapPlot
ggplot(jewelry_final_map)+geom_polygon( aes(x = long, y = lat, group = group, 
                                         fill = rel_freqs),
                                     color = "grey") +
  coord_map() +theme_void() + labs(title = "Total sales by cities") +
  scale_fill_distiller(name = "Total Sales",palette = "Spectral", 
                       limits = c(0,0.07),
                       na.value = "red") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


### 8) data manipulation part II: total sales by categories rel_freqs
# FRAGRANCE total sales
fragrance <- sales_data %>%
  filter(CATEGORY_ == "FRAGRANCE") %>%
  group_by(sehir) %>%
  summarise(SALES = sum(SALES)) %>%
  mutate(rel_freqs = SALES / sum(SALES)) %>%
  arrange(desc(rel_freqs))
# TUR & sales merger
fragrance_id_and_cities <- 
  data_frame(id = rownames(TUR@data), sehir = TUR@data$NAME_1) %>% 
  left_join(fragrance, by = "sehir")
# last join (TUR data & id_cities)
fragrance_final_map <- left_join(fragrance_id_and_cities, TUR_for, by = "id")
# save final map data as .csv
write.csv(fragrance_final_map, file = "fragrance_final_map.csv")
# mapPlot
ggplot(fragrance_final_map)+geom_polygon( aes(x = long, y = lat, group = group, 
                                            fill = rel_freqs),
                                        color = "grey") +
  coord_map() +theme_void() + labs(title = "Total sales by cities") +
  scale_fill_distiller(name = "Total Sales",palette = "Spectral", 
                       limits = c(0,0.06),
                       na.value = "red") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


### 9) data manipulation part II: total sales by categories rel_freqs
# TOILETRIES total sales
toiletries <- sales_data %>%
  filter(CATEGORY_ == "TOILETRIES") %>%
  group_by(sehir) %>%
  summarise(SALES = sum(SALES)) %>%
  mutate(rel_freqs = SALES / sum(SALES)) %>%
  arrange(desc(rel_freqs))
# TUR & sales merger
toiletries_id_and_cities <- 
  data_frame(id = rownames(TUR@data), sehir = TUR@data$NAME_1) %>% 
  left_join(toiletries, by = "sehir")
# last join (TUR data & id_cities)
toiletries_final_map <- left_join(toiletries_id_and_cities, TUR_for, by = "id")
# save final map data as .csv
write.csv(toiletries_final_map, file = "toiletries_final_map.csv")
# mapPlot
ggplot(toiletries_final_map)+geom_polygon( aes(x = long, y = lat, group = group, 
                                              fill = rel_freqs),
                                          color = "grey") +
  coord_map() +theme_void() + labs(title = "Total sales by cities") +
  scale_fill_distiller(name = "Total Sales",palette = "Spectral", 
                       limits = c(0,0.065),
                       na.value = "red") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

### 10) data manipulation part II: total sales by categories rel_freqs
# COLOR total sales
color <- sales_data %>%
  filter(CATEGORY_ == "COLOR") %>%
  group_by(sehir) %>%
  summarise(SALES = sum(SALES)) %>%
  mutate(rel_freqs = SALES / sum(SALES)) %>%
  arrange(desc(rel_freqs))
# TUR & sales merger
color_id_and_cities <- 
  data_frame(id = rownames(TUR@data), sehir = TUR@data$NAME_1) %>% 
  left_join(color, by = "sehir")
# last join (TUR data & id_cities)
color_final_map <- left_join(color_id_and_cities, TUR_for, by = "id")
# save final map data as .csv
write.csv(color_final_map, file = "color_final_map.csv")
# mapPlot
ggplot(color_final_map)+geom_polygon( aes(x = long, y = lat, group = group, 
                                               fill = rel_freqs),
                                           color = "grey") +
  coord_map() +theme_void() + labs(title = "Total sales by cities") +
  scale_fill_distiller(name = "Total Sales",palette = "Spectral", 
                       limits = c(0,0.05),
                       na.value = "red") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))






