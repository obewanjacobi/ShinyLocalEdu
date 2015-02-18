#' @title 3-D Scatter Plots

#' @description Uses the \code{cloud} function from the \code{lattice} package.
#'
#' @rdname cloudSh
#' @usage cloudSh(x, data = parent.frame(),group=NULL)
#' @param x Could be a formula.  If so, it should be of the form z~x*y|g1*g2, as in the
#' \code{lattice} function \code{cloud}.
#' @param data data frame supplying variables for formula x.  If variables in x are not found in the data,
#' then they will be searched for in the parent environment.
#' @param group A factor variable by which to groups the points of the plot.
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
  function (x,data=parent.frame(),group=NULL)
  {
  groupName <- as.character(substitute(group))
  if (length(groupName) > 0) {
    group <- simpleFind(varName=groupName,data=data)
    haveGroup <- TRUE
  } else haveGroup <- FALSE

# Define server logic for cloudSh app
server <- shinyServer(function(input, output,session) {

  output$zControl <- renderUI({
    speed <- 1000-input$speed
    sliderInput("zScreen","z",0,360,value=0,step=1,
                animate=animationOptions(interval=speed,loop=TRUE))

  })

  output$xControl <- renderUI({
    speed <- 1000-input$speed
    sliderInput("xScreen","x",0,360,value=90,step=1,
                animate=animationOptions(interval=speed,loop=TRUE))

  })

  output$yControl <- renderUI({
    speed <- 1000-input$speed
    sliderInput("yScreen","y",0,360,value=40,step=1,
                animate=animationOptions(interval=speed,loop=TRUE))

  })

  output$cloudplot <- renderPlot({
    if (!is.null(input$xScreen)) {
      cloud(x,data=data,group=NULL,
            screen=list(x=-input$xScreen,y=input$yScreen,z=input$zScreen),
            auto.key=haveGroup)
    }
  })


})


# Define ui for cloudSh app
ui <- shinyUI(fluidPage(shinythemes::shinytheme("cerulean"),

  #  Application title
  titlePanel("3-D Scatter Plot"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
        numericInput("speed","Animation Speed",0,900,step=100,value=450),
        uiOutput("zControl"),
        uiOutput("xControl"),
        uiOutput("yControl")
    ), #end sidebarPanel

  # Here comes the main panel
  mainPanel(
    wellPanel(
      plotOutput("cloudplot")
    )

  ) #end mainPanel

  ) #end SidebarLayout

))  #end fluidPage and and shinyUI



shiny::shinyApp(ui = ui, server = server)


  }#end cloudSh

