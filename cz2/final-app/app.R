library(shiny)
library(ranger)
library(ggplot2)
library(plotly)
library(DT)

load("models.RData")

server <- function(input, output) {
  prediction <- reactive({
    dat <- data.frame(expand.grid(rok = seq(input[["rok"]][1], input[["rok"]][2], by = 5),
                                  dzielnica = input[["dzielnica"]]),
                      n_pokoj = 2, 
                      metraz = 50, 
                      pietro = 2, 
                      pietro_maks = 2)
    
    data.frame(dat, cena_m2 = predict(model_best, data = dat)[["predictions"]])
  })
  
  output[["pred"]] <- renderDataTable({
    datatable(prediction(), extensions = 'Buttons', 
              filter = "top", rownames = FALSE, style = "bootstrap",
              options = list(dom = "Brtip",
                             buttons = c("copy", "csv", "excel", "print")))

  })
  
  output[["pred_plot"]] <- renderPlotly({
    ggplotly(ggplot(prediction(), aes(x = rok, y = cena_m2, color = dzielnica)) +
      geom_point(size = 4) +
      theme_bw())
  })
  
}

ui <- fluidPage(
  titlePanel("Przewidywanie cen mieszkań"),
  theme = shinythemes::shinytheme("journal"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("rok", "Rok:", min = 1950, max = 2010, value = c(1990, 2000), step = 5, sep = ""),
      checkboxGroupInput("dzielnica", "Dzielnica:", choices = c("Brak", "Fabryczna", "Krzyki", "Psie Pole", "Stare Miasto", "Śródmieście"),
                         selected = "Brak")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Table", dataTableOutput("pred")),
        tabPanel(title = "Charts", plotlyOutput("pred_plot"))))
  )
)

shinyApp(ui = ui, server = server)
