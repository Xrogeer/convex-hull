# CARGA DE LIBRERÍAS Y ARCHIVO DE TRABAJO
library(readr)
library(ggplot2)
library(gridExtra)
library(ggforce)
library(ggExtra)
library(tidyverse)

setwd("/Users/rogersans/Desktop/Màster Big Data Deportivo/Máster Big Data Deportivo/M4_ANÁLISIS DE DATOS DEPORTIVOS CON R Y PHYTON/Examen_R/Ejercicios_resueltos_R/data")

full_data <- read_delim("full_data.txt", ";", 
                        locale = locale(decimal_mark = ","),
                        trim_ws = TRUE)

# EJERCICIO 1. Identificar todos los pases realizados por Sergio Busquets
jugador_elegido <- c("Busquets")
evento_elegido <- c("Passes_All")

data_player1 <- subset(full_data, Jugador == jugador_elegido & 
                        Sección == evento_elegido)

# EJERCICIO 2. Representar un campo de fútbol, donde además se vean las porterias, los arcos correspondientes a los saques de córner y los arcos correspondientes al área
pitch_plot <- ggplot() +
 
  geom_rect(mapping = aes(xmin = 0.0, xmax = 596.97, 
                          ymin = 0.0, ymax = 396),
            color ="#00529f", fill = NA, alpha = 0.1) +
  geom_rect(mapping = aes(xmin = 0.0, xmax = 91.935, 
                          ymin = 78.475, ymax = 317.525), 
            color ="#00529f", fill = NA, alpha = 0.1) +
  geom_rect(mapping = aes(xmin = 505.035, xmax = 596.97, 
                          ymin = 78.475, ymax = 317.525), 
            color ="#00529f", fill = NA, alpha = 0.1) +
  geom_rect(mapping = aes(xmin = 0.0, xmax = 29.858, 
                          ymin = 144.5, ymax = 251.50), 
            color ="#00529f", fill = NA, alpha = 0.1) +
  geom_rect(mapping = aes(xmin = 567.112, xmax = 596.97, 
                          ymin = 145.5, ymax = 251.50), 
            color ="#00529f", fill = NA, alpha = 0.1) +
  

  geom_linerange(aes(x = 298.485, ymin = 0, 
                     ymax = 396), 
                 color = "#00529f") +
  geom_circle(mapping = aes(x0 = 66.33, y0 = 198.0, r = 1), color = "#00529f") +
  geom_circle(mapping = aes(x0 = 530.64, y0 = 198.0, r = 1), color = "#00529f") +
  geom_circle(mapping = aes(x0 = 298.485, y0 = 198.0, r = 1), color = "#00529f") +
  geom_circle(mapping = aes(x0 = 298.485, y0 = 198.0, r = 52), color = "#00529f") +
  coord_fixed() +

  geom_arc(mapping = aes(x0 = 0.0, y0 = 0, r=52/9.15, start=0, end=pi/2),
           color = "#00529f") +
  geom_arc(mapping = aes(x0 = 596.97, y0 = 0, r=52/9.15, start=0, end=-pi/2),
           color = "#00529f") +
  geom_arc(mapping = aes(x0 = 596.97, y0 = 396, r=52/9.15, start=-pi/2, end=-pi),
           color = "#00529f") +
  geom_arc(mapping = aes(x0 = 0.0, y0 = 396, r=52/9.15, start=pi/2, end=pi),
           color = "#00529f") +
 
  geom_arc(mapping = aes(x0 = 66.33, y0 = 198, r=52, start=pi/6, end=5/6*pi),
           color = "#00529f") +
  geom_arc(mapping = aes(x0 = 530.64, y0 = 198, r=52, start=-pi/6, end=-5/6*pi),
           color = "#00529f") +
  
  geom_linerange(aes(x = 0.0, ymin = 198 - ((52/9.15) * (7.32/2)), ymax = 198 + ((52/9.15) * (7.32/2))), 
                 color = "#00529f", size = 2) +
  geom_linerange(aes(x = 596.97, ymin = 198 - ((52/9.15) * (7.32/2)), ymax = 198 + ((52/9.15) * (7.32/2))), 
                 color = "#00529f", size = 2) +
  
   theme_no_axes(base.theme = theme_bw()) +
  theme(legend.position = c(0.5, 1),
        legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.box.background = element_rect(fill = "transparent",
                                             colour = "transparent"),
        legend.text = element_text(size = 10),
        panel.border = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0),"mm"))+
  scale_x_continuous(limits = c(0,605), expand = c(0.01,0)) +
  scale_y_continuous(limits = c(0,425), expand = c(0,0))


pitch_plot

# EJERCICIO 3. Representar en el campo los pases realizados, mediante puntos de sus orígenes, creando un área convexa (investigar sobre Convex Hull en R) que englobe todos los puntos correspondientes a dichos eventos
plot_origenpases <- pitch_plot + 
  geom_point(data_player1, mapping = aes(x= x1-58.015, y= 396 - (y1 - 50.5), color= factor(Jugador),shape= factor(Resultado)), 
             size = 5) +
  guides(color = FALSE) +
  theme(legend.title = element_blank())+
  scale_shape_manual(values = c(3, 6, 10, 1))

plot_origenpases

indices_vertices_poligono <- chull(data_player1$x1,data_player1$y1)
vertices_poligono1 <- data_player1[indices_vertices_poligono,]

plot_convexhull <- plot_origenpases +
  geom_polygon(vertices_poligono1, mapping = aes(x = x1-58.015, y = 396 - (y1 - 50.5), color= Jugador, fill = Jugador), alpha= 0.2, show.legend= FALSE) +
  theme(legend.box = "vertical",
        legend.position = "bottom") +
    guides(color = guide_legend(order = 1),
           shape = guide_legend(order = 2))
plot_convexhull

# EJERCICIO 4. Representar como diagrama marginal un diagrama de violín (investigar Violin plot en R y, en particular, en ggMarginal) que muestre la distribución espacial en x e y de dichos eventos
ggExtra::ggMarginal(plot_origenpases,
                    type = "violin")

# EJERCICIO EXTRA 1. Generar un nuevo gráfico donde se muestren los vectores de pases realizados, visualizando así tanto origen como destino
plot_vectores <- pitch_plot +
  geom_segment(data_player1, mapping = aes(x = x1 - 58.015, y = 396 - (y1 - 50.5), xend = x2 - 58.015, yend = 396 - (y2 - 50.5), color = factor(Resultado)), arrow = arrow(length = unit(0.012,"npc"))) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("steelblue", "red", "green"))

plot_vectores

# EJERCICIO EXTRA 2. Completar la información del gráfico (cualquiera de los dos propuestos) con el nombre del jugador, del club y rival que se esté mostrando
plot_vectores <- pitch_plot +
  labs(title = "Sergio Busquest (FC Barcelona) vs Real Madrid") +
  geom_segment(data_player1, mapping = aes(x = x1 - 58.015, y = 396 - (y1 - 50.5), xend = x2 - 58.015, yend = 396 - (y2 - 50.5), color = factor(Resultado)), arrow = arrow(length = unit(0.012,"npc"))) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("steelblue", "red", "green"))

plot_vectores
# EJERCICIO EXTRA 3. Extraer de la información recogida en la tabla el resultado del partido y mostrarlo en el gráfico  (cualquiera de los dos propuestos) como complemento informativo
plot_vectores <- pitch_plot +
  labs(title = "Sergio Busquest (FC Barcelona) vs Real Madrid",
       subtitle = "RMA 2-3 FCB") +
  geom_segment(data_player1, mapping = aes(x = x1 - 58.015, y = 396 - (y1 - 50.5), xend = x2 - 58.015, yend = 396 - (y2 - 50.5), color = factor(Resultado)), arrow = arrow(length = unit(0.012,"npc"))) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("steelblue", "red", "green"))

plot_vectores
# EJERCICIO EXTRA 4. Mostrar en el primer gráfico las áreas convexas de pases realizados de todos los defensas y Sergio Busquets del FC Barcelona en el partido contra el Real Madrid. Mostrar cada una con un color diferente
lst_jugadores <- c("Busquets", "Alba", "Piqué", "Umtiti", "Roberto")
linea_defensiva <- full_data %>%
  filter(Equipo %in% "Barcelona", Rival %in% "Real Madrid", Sección %in% "Passes_All", Jugador %in% lst_jugadores)

indices_vertices_poligono5 <- chull(linea_defensiva$x1, linea_defensiva$y1)
vertices_poligono5 <- linea_defensiva %>%
  group_by (Jugador)%>%
  slice(chull(x1,y1))

plot_convexhull5 <- pitch_plot +
  labs(title = "Línea defensiva FCB",
       subtitle = "RMA 2-3 FCB") +
  geom_polygon(vertices_poligono5, mapping = aes(x = x1-58.015, y = 396 - (y1 - 50.5), color= Jugador, fill = Jugador), alpha= 0.2, linewidth = 0.7, show.legend= TRUE) +
  theme(legend.box = "vertical",
        legend.position = "bottom") +
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2))
plot_convexhull5
#EJERCICIO EXTRA 5. Comentar, tácticamente, los resultados obtenidos