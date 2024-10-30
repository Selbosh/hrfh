library(shiny)
library(ggplot2)
library(patchwork)

# Define latent_process() function:
source('simulator.R')

# Define UI
ui <- fluidPage(
  titlePanel('Patient Simulator'),
  sidebarLayout(
    sidebarPanel(
      sliderInput('num_timepoints', 'Number of Timepoints', min = 50, max = 365, value = 180),
      numericInput('baseline_variation', 'Baseline Variation', value = 0.2),
      fluidRow(
        column(4, numericInput('flare_rate', 'Rate', value = 0.01)),
        column(4, numericInput('flare_magnitude', 'Magnitude', value = 2)),
        column(4, numericInput('flare_variation', 'Variation', value = 0.25))
      ),
      numericInput('treat_magnitude', 'Treatment Magnitude', value = 0.1),
      sliderInput('treat_duration', 'Treatment Duration', min = 1, max = 90, value = 30),
      textInput('cutpoints', 'Cutpoints (comma-separated)', value = paste((-5:4)/2, collapse = ',')),
      fluidRow(
        column(6, actionButton('randomizeSeed', 'Randomize seed')),
        column(6, numericInput('seed', 'Random Seed', value = sample(1:9999, 1), label = NULL)),
      )
    ),
    mainPanel(
      plotOutput('trajectoryPlot'),
      HTML("<p><u>Key</u>: <span style='color:tomato;'>Red lines indicate <strong>flares</strong></span> and <span style='color:steelblue;'>blue lines indicate <em>effective</em> <strong>treatments</strong></span>.</p>"),
      HTML("<p>The latent pain trajectory can go beyond the range of detection, meaning a flare might not bring pain above 0 or treatment bring pain below 10!</p>")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$randomizeSeed, {
    updateNumericInput(session, 'seed', value = sample(1:9999, 1))
  })

  output$trajectoryPlot <- renderPlot({
    cutpoints <- as.numeric(unlist(strsplit(input$cutpoints, ',')))

    set.seed(input$seed)

    patient <- simulate_patient(
      num_timepoints = input$num_timepoints,
      baseline_variation = input$baseline_variation,
      flare_rate = input$flare_rate,
      flare_magnitude = input$flare_magnitude,
      flare_variation = input$flare_variation,
      treat_magnitude = input$treat_magnitude,
      treat_duration = input$treat_duration,
      alpha = input$alpha,
      cutpoints = cutpoints,
      verbose = TRUE
    )

    trajectory_plot <- ggplot(patient) +
      aes(x = time, y = pain) +
      geom_vline(aes(xintercept = time), data = subset(patient, treat), colour = 'steelblue') +
      geom_vline(aes(xintercept = time), data = subset(patient, flare), colour = 'tomato2') +
      geom_line() +
      geom_point() +
      labs(x = NULL, y = 'Pain Level') +
      scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10))

    latent_plot <- ggplot(patient) +
      aes(x = time, y = latent) +
      geom_vline(aes(xintercept = time), data = subset(patient, treat), colour = 'steelblue') +
      geom_vline(aes(xintercept = time), data = subset(patient, flare), colour = 'tomato2') +
      geom_line() +
      geom_point() +
      labs(x = 'Day of follow-up', y = 'Latent disease') +
      scale_y_continuous(breaks = cutpoints, minor_breaks = NULL)


    trajectory_plot + latent_plot + plot_layout(ncol = 1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)