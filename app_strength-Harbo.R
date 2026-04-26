# ui
library(shinythemes)
library(ggplot2)
library(ggthemes)

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
      min = 30,
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
    
    coeff <- tryCatch(
      read.csv(
        filename,
        header = TRUE,
        sep = ";",
        dec = ".",
        row.names = 1,
        fill = TRUE,
        na.strings = c(""),
        comment.char = ""
      ),
      error = function(e) validate(need(FALSE, paste("Failed to load coefficient file:", e$message)))
    )
    
    coeff.muscle <- coeff[trimws(rownames(coeff)) == input$muscle, ]
    validate(need(nrow(coeff.muscle) == 1, "Coefficients not found for selected muscle."))
    
    pred <-
      coeff.muscle$I +
      (coeff.muscle$A * input$age) +
      (coeff.muscle$H * input$height / 100) +
      (coeff.muscle$W * input$weight)
    
    pc = input$strength / pred * 100

    
    pc = as.data.frame(pc)

    pc_val    <- pc$pc[1]
    bar_fill  <- ifelse(pc_val < 90, "#e74c3c", ifelse(pc_val <= 110, "#2ecc71", "#2980b9"))
    lbl_color <- ifelse(pc_val < 90, "#c0392b", ifelse(pc_val <= 110, "#1e8449", "#1a5276"))

    ggplot(pc, aes(x = "", y = pc)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 110, ymax = 200, fill = "#2980b9", alpha = 0.15) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 90,  ymax = 110, fill = "#2ecc71", alpha = 0.15) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,   ymax = 90,  fill = "#e74c3c", alpha = 0.15) +
      geom_hline(yintercept = 90,  linetype = "dashed", color = "#7f8c8d", linewidth = 0.5) +
      geom_hline(yintercept = 110, linetype = "dashed", color = "#7f8c8d", linewidth = 0.5) +
      geom_bar(stat = "identity", fill = bar_fill, width = 0.45, alpha = 0.9) +
      annotate("text", x = Inf, y = 155, label = "Above normal", hjust = 1.1, vjust = 0.5, size = 3.2, color = "#2471a3", fontface = "italic") +
      annotate("text", x = Inf, y = 100, label = "Normal",       hjust = 1.1, vjust = 0.5, size = 3.2, color = "#1e8449", fontface = "italic") +
      annotate("text", x = Inf, y = 45,  label = "Below normal", hjust = 1.1, vjust = 0.5, size = 3.2, color = "#c0392b", fontface = "italic") +
      geom_text(
        aes(label = paste0(formatC(pc, format = "f", digits = 0), "%")),
        vjust = -0.7, fontface = "bold", size = 11, color = lbl_color
      ) +
      scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 20), expand = expansion(mult = 0, add = 0)) +
      labs(title = input$muscle, x = NULL, y = "Muscle Strength (% Predicted)") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title         = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 10)),
        axis.text.x        = element_blank(),
        axis.ticks.x       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y        = element_text(color = "#2c3e50", size = 11),
        axis.title.y       = element_text(color = "#2c3e50", size = 12, margin = margin(r = 10)),
        panel.grid.major.y = element_line(color = "#ecf0f1", linewidth = 0.4),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background   = element_rect(fill = "white", color = NA),
        plot.background    = element_rect(fill = "white", color = NA),
        plot.margin        = margin(t = 14, r = 20, b = 10, l = 10)
      )
  })
}

shinyApp(ui = ui, server = server)

