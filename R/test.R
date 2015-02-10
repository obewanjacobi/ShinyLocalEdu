#' @title Test

#' @description To show difference in screen width when function is loaded and called vs.
#'  called from an attached package.
#'
#' @rdname test
#' @usage test()
#' @import shiny
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' test()
#' }
test <-
  function ()
  {



# Define server logic for test
server <- shinyServer(function(input, output) {
  output$main_plot <- renderPlot({

    hist(faithful$eruptions,
         probability = TRUE,
         breaks = as.numeric(input$n_breaks),
         xlab = "Duration (minutes)",
         main = "Geyser eruption duration")

    if (input$individual_obs) {
      rug(faithful$eruptions)
    }

    if (input$density) {
      dens <- density(faithful$eruptions,
                      adjust = input$bw_adjust)
      lines(dens, col = "blue")
    }

  })
})



# Define ui for test
ui<- shinyUI(bootstrapPage(

    selectInput(inputId = "n_breaks",
                label = "Number of bins in histogram (approximate):",
                choices = c(10, 20, 35, 50),
                selected = 20),

    checkboxInput(inputId = "individual_obs",
                  label = strong("Show individual observations"),
                  value = FALSE),

    checkboxInput(inputId = "density",
                  label = strong("Show density estimate"),
                  value = FALSE),

    plotOutput(outputId = "main_plot", height = "300px"),

    # Display this only if the density is shown
    conditionalPanel(condition = "input.density == true",
                     sliderInput(inputId = "bw_adjust",
                                 label = "Bandwidth adjustment:",
                                 min = 0.2, max = 2, value = 1, step = 0.2)
    )

  ))


shiny::shinyApp(ui = ui, server = server,
                options=list(height=600,width=1200))
# as expected, setting options does not help increase screen width when package is attached


  }#end test

