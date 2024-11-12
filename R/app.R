library(shiny)
library(ggplot2)
library(patchwork)

# Define latent_process() function:
source('trajectory.R')

# Define UI
ui <- fluidPage(
  titlePanel('Patient Simulator'),
  sidebarLayout(
    sidebarPanel(
      sliderInput('days', 'Number of Timepoints', min = 50, max = 365, value = 180),
      numericInput('sigma', 'Baseline Variation', value = 0.2),
      fluidRow(
        column(4, numericInput('flare_rate', 'Rate', value = 0.01, min = 0, max = 1)),
        column(4, numericInput('flare_mean', 'Mean', value = 2, min = 0)),
        column(4, numericInput('flare_sd', 'SD', value = 0.25, min = 0))
      ),
      numericInput('treat_magnitude', 'Treatment Magnitude', value = 0.1),
      numericInput('treat_effect_prob', 'Treatment Success Probability', value = 0.5, min = 0, max = 1),
      sliderInput('treat_duration', 'Treatment Duration', value = 14, min = 1, max = 14),
      sliderInput('treat_interval', 'Treatment Interval', min = 1, max = 90, value = 30),
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

    patient <- simulate_pain_trajectory(
      days = input$days,
      sigma = input$sigma,
      flare_rate = input$flare_rate,
      flare_intensity = input$flare_mean,
      flare_temp_prob = input$flare_sd,
      treat_intensity = input$treat_magnitude,
      treat_duration = input$treat_duration,
      treat_interval = input$treat_interval,
      treat_effect_prob = input$treat_effect_prob,
      cutpoints = cutpoints
    )

    print(patient)

    trajectory_plot <- ggplot(patient) +
      aes(x = day, y = measurement) +
      geom_vline(aes(xintercept = day), data = subset(patient, on_treatment), colour = 'steelblue',
                 alpha = .5) +
      geom_vline(aes(xintercept = day), data = subset(patient, flares > 0), colour = 'tomato2',
                 alpha = 0.5) +
      geom_line() +
      geom_point() +
      labs(x = NULL, y = 'Pain Level') +
      scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10))

    latent_plot <- ggplot(patient) +
      aes(x = day, y = trajectory) +
      geom_vline(aes(xintercept = day), data = subset(patient, on_treatment), colour = 'steelblue',
                 alpha = 0.5) +
      geom_vline(aes(xintercept = day), data = subset(patient, flares > 0), colour = 'tomato2',
                 alpha = 0.5) +
      geom_line() +
      geom_point() +
      labs(x = 'Day of follow-up', y = 'Latent disease') +
      scale_y_continuous(breaks = cutpoints, minor_breaks = NULL)


    trajectory_plot + latent_plot + plot_layout(ncol = 1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)