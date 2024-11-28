# Cargar las librerías necesarias
set.seed(123)  # Para reproducibilidad
library(dplyr)

# Definir las carreras, tutores y otros valores
carreras <- c("Ingeniería Civil", "Medicina", "Psicología", "Arquitectura", "Derecho", "Ingeniería Informática", "Biología", "Química", "Ciencias Sociales", "Economía")
tutores <- c("Dr. Pérez", "Dra. Gómez", "Lic. López", "Dr. Fernández", "Lic. Martínez", "Lic. Rodríguez", "Dr. Sánchez", "Dra. Jiménez")

# Número de estudiantes
n_estudiantes <- 1000

# Generar los datos
encuesta_completa <- data.frame(
  ID = 1:n_estudiantes,
  Carrera = sample(carreras, n_estudiantes, replace = TRUE),
  Tutor = sample(tutores, n_estudiantes, replace = TRUE),
  Desempeño_academico = sample(1:5, n_estudiantes, replace = TRUE),
  Desempeño_profesional = sample(1:5, n_estudiantes, replace = TRUE),
  Tiempo_dedicado = sample(10:40, n_estudiantes, replace = TRUE),  # Tiempo dedicado en horas por semana
  Satisfaccion_practica = sample(1:5, n_estudiantes, replace = TRUE),
  Compromiso_estudio = sample(1:5, n_estudiantes, replace = TRUE),
  Apoyo_tutor = sample(1:5, n_estudiantes, replace = TRUE),
  Retroalimentacion = sample(1:5, n_estudiantes, replace = TRUE),
  Seguimiento = sample(1:5, n_estudiantes, replace = TRUE),
  Reuniones = sample(0:10, n_estudiantes, replace = TRUE),  # Número de reuniones con el tutor
  Motivacion_estudio = sample(1:5, n_estudiantes, replace = TRUE),
  Redes_apoyo = sample(1:5, n_estudiantes, replace = TRUE),
  Estres = sample(1:5, n_estudiantes, replace = TRUE),
  Expectativas_carrera = sample(1:5, n_estudiantes, replace = TRUE)
)

# Verificar las primeras filas del dataset
head(encuesta_completa)

# Guardar la base de datos como un archivo CSV
write.csv(encuesta_completa, "encuesta_estudiantes_completa.csv", row.names = FALSE)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# Cargar los datos
encuesta_completa <- read.csv("encuesta_estudiantes_completa.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Panorama de Estudiantes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen General", tabName = "resumen", icon = icon("chart-bar")),
      menuItem("Análisis Detallado", tabName = "analisis", icon = icon("search")),
      menuItem("Tabla de Datos", tabName = "tabla", icon = icon("table"))
    ),
    selectInput("filtro_carrera", "Seleccionar Carrera:",
                choices = c("Todas", unique(encuesta_completa$Carrera)), selected = "Todas"),
    sliderInput("filtro_desempeno", "Filtrar Desempeño Académico:",
                min = 1, max = 5, value = c(1, 5))
  ),
  dashboardBody(
    tabItems(
      # Resumen General
      tabItem(tabName = "resumen",
              fluidRow(
                box(title = "Distribución por Carrera", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("grafico_carrera")),
                box(title = "Promedio de Variables Clave", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("grafico_promedios"))
              )
      ),
      # Análisis Detallado
      tabItem(tabName = "analisis",
              fluidRow(
                box(title = "Relación entre Reuniones y Satisfacción Práctica", status = "info", solidHeader = TRUE,
                    plotOutput("grafico_relacion")),
                box(title = "Filtrar por Carrera", status = "info", solidHeader = TRUE,
                    selectInput("filtro_carrera_2", "Seleccionar Carrera:", 
                                choices = c("Todas", unique(encuesta_completa$Carrera)), selected = "Todas"),
                    plotOutput("grafico_carrera_detalle"))
              )
      ),
      # Tabla de Datos
      tabItem(tabName = "tabla",
              fluidRow(
                box(title = "Datos Completos", status = "warning", solidHeader = TRUE, width = 12,
                    DTOutput("tabla_datos"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Filtrar datos según inputs
  datos_filtrados <- reactive({
    data <- encuesta_completa
    if (input$filtro_carrera != "Todas") {
      data <- data %>% filter(Carrera == input$filtro_carrera)
    }
    data <- data %>% filter(Desempeño_academico >= input$filtro_desempeno[1],
                            Desempeño_academico <= input$filtro_desempeno[2])
    return(data)
  })
  
  # Gráficos
  output$grafico_carrera <- renderPlot({
    ggplot(datos_filtrados(), aes(x = Carrera)) +
      geom_bar(fill = "steelblue") +
      theme_minimal() +
      labs(title = "Distribución de Estudiantes por Carrera", x = "Carrera", y = "Cantidad")
  })
  
  output$grafico_promedios <- renderPlot({
    datos_promedios <- datos_filtrados() %>%
      summarise(across(c(Desempeño_academico, Satisfaccion_practica, Motivacion_estudio, Estres), mean))
    barplot(as.numeric(datos_promedios), names.arg = colnames(datos_promedios), col = "coral",
            main = "Promedio de Variables Clave", ylab = "Promedio")
  })
  
  output$grafico_relacion <- renderPlot({
    ggplot(datos_filtrados(), aes(x = Reuniones, y = Satisfaccion_practica)) +
      geom_point(alpha = 0.5, color = "purple") +
      geom_smooth(method = "lm", color = "blue") +
      theme_minimal() +
      labs(title = "Reuniones vs Satisfacción Práctica", x = "Número de Reuniones", y = "Satisfacción Práctica")
  })
  
  output$grafico_carrera_detalle <- renderPlot({
    data <- datos_filtrados()
    if (input$filtro_carrera_2 != "Todas") {
      data <- data %>% filter(Carrera == input$filtro_carrera_2)
    }
    ggplot(data, aes(x = Desempeño_academico)) +
      geom_histogram(fill = "lightgreen", bins = 5) +
      theme_minimal() +
      labs(title = paste("Distribución del Desempeño en", input$filtro_carrera_2),
           x = "Desempeño Académico", y = "Frecuencia")
  })
  
  # Tabla de datos
  output$tabla_datos <- renderDT({
    datatable(datos_filtrados(), options = list(scrollX = TRUE))
  })
}

# Run App
shinyApp(ui = ui, server = server)



