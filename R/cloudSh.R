#' @title 3-D Scatter Plots

#' @description Uses the \code{cloud} function from the \code{lattice} package.
#'
#' @rdname cloudSh
#' @usage cloudSh(x, data = parent.frame())
#' @param x Could be a formula.  If so, it should be of the form z~x*y|g1*g2, as in the
#' \code{lattice} function \code{cloud}.
#' @param data data frame supplying variables for formula x.  If variables in x are not found in the data,
#' then they will be searched for in the parent environment.
#' @return side effects
#' @note This is a locally-run Shiny app.  It may not work properly on some R Studio Server set-ups,
#' especially on the CentOS operating system.  For best views, open the app in the browser.
#' @import shiny
#' @import shinythemes
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' }
cloudSh <-
  function (x,data=parent.frame())
  {

 ##################################################################
 ####
 ####
 #### Process Input
 ####
 ####
 #################################################################



# Define server logic for cloudSh app
server <- shinyServer(function(input, output,session) {

  output$cloudplot <- renderPlot({
    cloud(x,data=data)
  })


})


# Define ui for cloudSh app
ui <- shinyUI(fluidPage(shinythemes::shinytheme("cerulean"),

  #  Application title
  titlePanel("3-D Scatter Plot"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      helpText("One simulation means the machine will produce one table of",
             "counts, using the Null probabilities.  How many simulations do",
             "you want the machine to perform at once?  (Limit is 10000.)"),
      numericInput("sims","Number of Simulations at Once",1,min=0,step=1),
      br(),
      actionButton("resample","Simulate Now"),
      conditionalPanel(
        condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
        actionButton("reset","Start Over")
      )
    ), #end sidebarPanel

  # Here comes the main panel
  mainPanel(

    plotOutput("cloudplot")

  ) #end mainPanel

  ) #end SidebarLayout

))  #end fluidPage and and shinyUI



shiny::shinyApp(ui = ui, server = server)


  }#end cloudSh

