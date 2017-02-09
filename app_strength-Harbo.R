# ui
library(shinythemes)

ui <- fluidPage(
  titlePanel(
    'Predictive values for maximal voluntary isokinetic and isometric voluntary strength in major muscle groups'
  ),
  theme = shinytheme("spacelab"),
  sidebarLayout(
    
    sidebarPanel(
    selectInput(
      inputId = 'muscle',
      label = 'Function Tested',
      choices = c(
        'Knee extension isokinetic',
                  'Knee flexion isokinetic',
                  'Knee extension isometric',
                  'Ankle extension isokinetic',
                  'Ankle flexion isokinetic',
                  'Ankle extension Isometric',
                  'Hip extension isokinetic', 
                  'Hip flexion isokinetic',
                  'Hip flexion isometric',
                  'Shoulder abduction isokinetic',
                  'Shoulder adduction isokinetic',
                  'Shoulder abduction isometric',
                  'Elbow extension isokinetic',
                  'Elbow flexion isokinetic',
                  'Elbow flexion isometric',
                  'Wrist extension isokinetic',
                  'Wrist flexion isokinetic',
                  'Wrist flexion isometric')
    ),
    
    selectInput(
      inputId = 'sex',
      label = 'Sex',
      choices = c('Male', 'Female')
    ),
    
    sliderInput(
      inputId = 'age',
      label = 'Age (yr)',
      min = 20,
      max = 80,
      step = 1,
      value = 30,
      post = ' years'
    ),
    
    sliderInput(
      inputId = 'height',
      label = 'Height (cm)',
      min = 140,
      max = 220,
      step = 1,
      value = 180
    ),
    
    
    sliderInput(
      inputId = 'weight',
      label = 'Weight (kg)',
      min = 0,
      max = 200,
      step = 1,
      value = 80
    ),
    
    sliderInput(
      inputId = 'strength',
      label = 'Strength (Nm)',
      min = 0,
      max = 400,
      step = 1,
      value = 60
    ),
    p("By Damien Bachasson."),
    p("Predictive equations are from:"),
    a(p('Harbo, T., Brincks, J. & Andersen, H. Maximal isokinetic and isometric muscle strength of major muscle groups related to age, body mass, height, and sex in 178 healthy subjects. European journal of applied physiology 112, 267-275.'),
      href = 'http://link.springer.com/article/10.1007%2Fs00421-011-1975-3'),
    p('Refer to the article for more details.')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Results",plotOutput('pl'))
    )
  )
))

# server

server <- function(input, output) {
  output$pl <- renderPlot({
    if (input$sex == 'Male')
      filename <- "coeff_men.csv"
    else
      filename <- "coeff_women.csv"
    
    coeff <-
      read.csv(
        filename,
        header = TRUE,
        sep = ";",
        dec = ".",
        row.names = 1,
        fill = TRUE,
        na.strings = c(""),
        comment.char = ""
      )
    
    coeff.muscle <- coeff[input$muscle,]
    
    pred <-
      coeff.muscle$I +
      (coeff.muscle$A * input$age) +
      (coeff.muscle$H * input$height / 100) +
      (coeff.muscle$W * input$weight)
    
    pc = input$strength / pred * 100

    
    library(ggplot2)
    library(ggthemes)
    pc = as.data.frame(pc)
    ggplot(pc, aes(x = "", y = pc)) +
      geom_bar(
        stat = "identity",
        colour = "black",
        fill = "#2980b9",
        width = .8
      ) +
      xlab("") +
      ylab("Muscle Strength (% Predicted)") +
      scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 20)) +
      theme_few(base_size = 12) +
      theme(plot.title = element_text(face = "bold")) +
      geom_rect(
        ymin = 110,
        ymax = 200,
        xmin = -Inf,
        xmax = Inf,
        fill = '#3498db',
        alpha = 0.2
      ) +
      geom_rect(
        ymin = 90,
        ymax = 110,
        xmin = -Inf,
        xmax = Inf,
        fill = '#2ecc71',
        alpha = 0.2
      ) +
      geom_rect(
        ymin = 0,
        ymax = 90,
        xmin = -Inf,
        xmax = Inf,
        fill = '#e74c3c',
        alpha = 0.2
      ) +
      geom_text(
        aes(label = paste0(
          formatC(pc, format = "f", digits = 0), ' %'
        )),
        fontface = "bold",
        size = 5,
        hjust = 0.5,
        vjust = -1,
        position = "stack"
      )
  })
}

shinyApp(ui = ui, server = server)

