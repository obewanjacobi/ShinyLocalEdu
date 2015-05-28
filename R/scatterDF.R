#' @title Dynamic Faceting in Scatter Plots

#' @description Interactively facet a scatterplot by a third variable.
#'
#' @rdname scatterDF
#' @usage scatterDF(form, data = parent.frame())
#' @param form a formula, of the form y~x|z, where z is the facetting variable.
#' @param data dataframe supplying variables for formula.  If variables in the formula are not found in the data,
#' then they will be searched for in the parent environment.
#' @return side effects
#' @note This is a locally-run Shiny app.  It may not work properly on some R Studio Server set-ups,
#' especially on the CentOS operating system.
#' @import shiny ggplot2
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' scatterDF(Petal.Length ~ Petal.Width | Species, data = iris)
#' if (require(mosaicData)) scatterDF(sat ~ salary | frac, data = SAT)
#' }



scatterDF <-   function (form,data=parent.frame()) {

 prsd <- ParseFormula(form)
 yname <- as.character(prsd$lhs)
 xname <- as.character(prsd$rhs)
 zname <- as.character(prsd$cond)

 y <- simpleFind(yname,data)
 x <- simpleFind(xname,data)
 z <- simpleFind(zname,data)

 df <- data.frame(x,y,z)

 if (is.numeric(z)) {

   lowValue <- with(df,min(z))
   highValue <- with(df,max(z))
   initialValues <- c(lowValue,highValue)
   middleValue <- median(z)
 } else { # to satisfy shiny
   lowValue <- 0
   highValue <- 100
   initialValues <- c(0,100)
   middleValue <- 50
 }

 otherColour <- "blue"
 selectColour <- "red"
 myColours <- c(otherColour,selectColour)

 ui.numerical <- shinyUI(fluidPage(

   #  Application title
   title="Scatterplot with Dynamic Facetting",
   titlePanel("Scatterplot with Dynamic Facetting"),

   fluidRow(
     column(3,
            checkboxInput(inputId = "nearest",
                          label = paste("Work with nearest neighbors in",zname),
                          value = FALSE),
            conditionalPanel(
              condition="input.nearest == false",
              sliderInput(inputId="desired",
                        label=paste("Range for", zname),
                        min=lowValue,max=highValue,value=initialValues)
            ),
            conditionalPanel(
              condition="input.nearest == true",
              numericInput(inputId="percent",label="Percentage to Select",
                           min=5,max=40,value=10,step=1),
              sliderInput(inputId="center",
                          label=paste("Central Value of", zname),
                          min=lowValue,max=highValue,value=middleValue,
                          animate=animationOptions(interval=1000,loop=TRUE))
            ),
            selectInput(
              inputId="smoother",
              label="Smoothing Method",
              choices=c("line"="lm","loess curve"="loess","gam curve"="gam","none"="none"),
              selected="lm")

            ),

     column(9,
            plotOutput("plot")
     )
   )

 ))  #end fluidPage and and shinyUI

 ui.factor <- shinyUI(fluidPage(

   #  Application title
   title="Scatterplot with Dynamic Facetting",
   titlePanel("Scatterplot with Dynamic Facetting"),

   fluidRow(
     column(3,
            selectInput(
              inputId="level",
              label=paste("Value of",zname),
              choices=levels(z)
              ),
            selectInput(
              inputId="smoother",
              label="Smoothing Method",
              choices=c("line"="lm","loess curve"="loess","gam curve"="gam","none"="none"),
              selected="lm")

     ),

     column(9,
            plotOutput("plot")
     )
   )

 ))

 server.numerical <- shinyServer(function(input, output,session) {

   make_plot <- reactive({

     if (input$nearest == TRUE) {

       currentNum <- input$center
       percentage <- input$percent
       prop <- percentage/100
       distances <- with(df,abs(currentNum-z))
       sorted <- df[order(distances),]
       lastPick <- floor(nrow(sorted)*prop)
       subFrame <- sorted[1:lastPick,]
       selected  <- c(rep("selected",lastPick),rep("other",nrow(sorted)-lastPick))
       sorted$selected <- factor(selected)
       df <- sorted

     } else {

      desiredRows <- with(df, z >= input$desired[1] & z <= input$desired[2])
      subFrame <- subset(df,subset=desiredRows)
      selected  <- ifelse(desiredRows,"selected","other")
      df$selected <- factor(selected)
     }

     if (input$smoother != "none") {

      ggplot(df,aes(x=x,y=y)) + geom_point(aes(colour=selected,size=selected,fill=selected)) +
        geom_smooth(method="lm",se=FALSE,colour=otherColour) +
        scale_fill_manual(values=myColours) +
        scale_colour_manual(values=myColours) +
        scale_size_manual(values=c(2,3)) +
        geom_smooth(data=subFrame,method=input$smoother,se=FALSE,colour=selectColour) +
        labs(x=xname,y=yname)

     } else {

       ggplot(df,aes(x=x,y=y)) + geom_point(aes(colour=selected,size=selected,fill=selected)) +
         scale_fill_manual(values=myColours) +
         scale_colour_manual(values=myColours) +
         scale_size_manual(values=c(2,3)) +
         labs(x=xname,y=yname)

     }

   })

   output$plot <- renderPlot({
     make_plot()
   })

 })

 server.factor <- shinyServer(function(input, output,session) {

   make_plot <- reactive({
    desiredLevel <- input$level
    subFrame <- subset(df,z == desiredLevel)
    selected  <- ifelse(z == desiredLevel,"selected","other")
    df$selected <- factor(selected)

    if (input$smoother != "none") {

      ggplot(df,aes(x=x,y=y)) + geom_point(aes(colour=selected,size=selected,fill=selected)) +
        geom_smooth(method="lm",se=FALSE,colour=otherColour) +
        scale_fill_manual(values=myColours) +
        scale_colour_manual(values=myColours) +
        scale_size_manual(values=c(2,3)) +
        geom_smooth(data=subFrame,method=input$smoother,se=FALSE,colour=selectColour) +
        labs(x=xname,y=yname)

    } else {

      ggplot(df,aes(x=x,y=y)) + geom_point(aes(colour=selected,size=selected,fill=selected)) +
        scale_fill_manual(values=myColours) +
        scale_colour_manual(values=myColours) +
        scale_size_manual(values=c(2,3)) +
        labs(x=xname,y=yname)
    }

    }) # end make_plot

   output$plot <- renderPlot({
     make_plot()
   })

 })

 if (is.numeric(z)) {
  shiny::shinyApp(ui = ui.numerical, server = server.numerical)
 } else {
   shiny::shinyApp(ui = ui.factor, server = server.factor)
 }

} #end scatterDF
