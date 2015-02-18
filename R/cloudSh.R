#' @title 3-D Scatter Plots

#' @description Uses the \code{cloud} function from the \code{lattice} package.
#'
#' @rdname cloudSh
#' @usage cloudSh(x, data = parent.frame(),...)
#' @param x Could be a formula.  If so, it should be of the form z~x*y|g1*g2, as in the
#' \code{lattice} function \code{cloud}.
#' @param data data frame supplying variables for formula x.  If variables in x are not found in the data,
#' then they will be searched for in the parent environment.
#' @param ... Other arguments passed to \code{cloud}.
#' @return side effects
#' @note This is a locally-run Shiny app.  It may not work properly on some R Studio Server set-ups,
#' especially on the CentOS operating system.  For best views, open the app in the browser.
#' @import shiny
#' @importFrom lattice cloud
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' cloudSh(Petal.Length ~ Sepal.Length * Sepal.Width,data=iris,
#'      group=Species,auto.key=TRUE)
#' }
cloudSh <-
  function (x,data=parent.frame(),...)
  {

  argList <- as.list(match.call(expand.dots = TRUE)[-1])

  haveGroup <- (length(as.character(substitute(group)))>0)

# Define server logic for cloudSh app
  server1 <- shinyServer(function(input, output,session) {
    #This option uses slider to set step on controls.  Not properly responsive yet.

    step <- reactive({
      10*input$speed
    })

    output$zControl <- renderUI({
      step <- step()
      print(step)
      sliderInput("zScreen","z",0,360,value=0,step=step,
                  animate=animationOptions(interval=100,loop=TRUE))

    })

    output$xControl <- renderUI({
      step <- step()
      sliderInput("xScreen","x",0,360,value=90,step=step,
                  animate=animationOptions(interval=100,loop=TRUE))

    })

    output$yControl <- renderUI({
      step <- step()
      sliderInput("yScreen","y",0,360,value=40,step=step,
                  animate=animationOptions(interval=100,loop=TRUE))

    })

    output$cloudplot <- renderPlot({
      if (!is.null(input$xScreen)) {
        allArgs <- c(list(screen=list(x=-input$xScreen,y=input$yScreen,z=input$zScreen)),
                     argList)
        do.call(cloud,allArgs)
        #       cloud(eval(ll$x),data=eval(ll$data),group=eval(ll$group),
        #             screen=list(x=-input$xScreen,y=input$yScreen,z=input$zScreen),
        #             auto.key=haveGroup,pch=19)
      }
    })


  })
server2 <- shinyServer(function(input, output,session) {
  #This option permits three different step sizes.  Properly responsive.

  step <- reactive({
    switch(input$speed,
      "slow"=1,
      "medium"=10,
      "fast"=30
    )
  })

  output$zControl <- renderUI({
    step <- step()
    sliderInput("zScreen","z",0,360,value=0,step=step,
                animate=animationOptions(interval=200,loop=TRUE))

  })

  output$xControl <- renderUI({
    step <- step()
    sliderInput("xScreen","x",0,360,value=90,step=step,
                animate=animationOptions(interval=200,loop=TRUE))

  })

  output$yControl <- renderUI({
    step <- step()
    sliderInput("yScreen","y",0,360,value=40,step=step,
                animate=animationOptions(interval=200,loop=TRUE))

  })

  output$cloudplot <- renderPlot({
    if (!is.null(input$xScreen)) {
      allArgs <- c(list(screen=list(x=-input$xScreen,y=input$yScreen,z=input$zScreen)),
                   argList)
      do.call(cloud,allArgs)
#       cloud(eval(ll$x),data=eval(ll$data),group=eval(ll$group),
#             screen=list(x=-input$xScreen,y=input$yScreen,z=input$zScreen),
#             auto.key=haveGroup,pch=19)
    }
  })


})


# Define ui for cloudSh app
ui1 <- shinyUI(fluidPage(

  #  Application title
  titlePanel("3-D Scatter Plot"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
        sliderInput("speed","Animation Speed (degrees/sec)",0,360,step=10,value=90),
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

ui2 <- shinyUI(fluidPage(

                        #  Application title
                        title="3-D Scatter Plot",
                        titlePanel("3-D Scatter Plot"),

                        fluidRow(
                          column(3,
                                 helpText("Use these sliders to rotate the plot."),
                                 uiOutput("zControl"),
                                 uiOutput("xControl"),
                                 uiOutput("yControl"),
                                 hr(),
                                 helpText("Press a Play button to animate the rotation.",
                                          "For best results, animate only",
                                          "one direction at a time."),
                                 hr(),
                                 selectInput("speed","Animation Speed",
                                             c("Slow"="slow",
                                               "Medium"="medium",
                                               "Fast"="fast"))
                          ),
                          column(9,
                                 plotOutput("cloudplot",width="700px",height="700px")
                                 )
                        )

#                         hr(),
#
#                         fluidRow(
#                           column(3,uiOutput("zControl")),
#                           column(3,offset=1,uiOutput("xControl")),
#                           column(3,offset=1,uiOutput("yControl"))
#                         )

))  #end fluidPage and and shinyUI

shiny::shinyApp(ui = ui2, server = server2)


  }#end cloudSh

