#' @title Chi-Square Simulation (Contingency Table)

#' @description Perform chi-square test for association, by simulation.  Enter either
#' formula-data input or a summary table.
#'
#' @rdname chisqSimShiny
#' @usage chisqSimShiny(x, data = parent.frame())
#' @param x Could be a formula.  If so, it should be of the form ~var1+var2.
#' Otherwise either a table or matrix of summary data.
#' @param data dataframe supplying variables for formula x.  If variables in x are not found in the data,
#' then they will be searched for in the parent environment.
#' @return side effects
#' @note This is a locally-run Shiny app.  It may not work properly on some R Studio Server set-ups,
#' especially on the CentOS operating system.
#' @import shiny
#' @export
#' @author Homer White \email{hwhite0@@georgetowncollege.edu}
#' @examples
#' \dontrun{
#' # from a data frame:
#' chisqSimShiny(~am+cyl,data=mtcars)
#'
#' # from a summary table:
#' DoesNotSmoke <- c(NeitherSmokes=1168,OneSmokes=1823,BothSmoke=1380)
#' Smokes <- c(188,416,400)
#' ChildParents <- rbind(DoesNotSmoke,Smokes)
#' chisqSimShiny(ChildParents)
#' }



xyplotDyn <-   function (form,data=parent.frame()) {

 prsd <- ParseFormula(form)
 yname <- as.character(prsd$lhs)
 xname <- as.character(prsd$rhs)
 zname <- as.character(prsd$cond)

 y <- simpleFind(yname,data)
 x <- simpleFind(xname,data)
 z <- simpleFind(zname,data)

 ui <- pageWithSidebar(

   #  Application title
   headerPanel("Scatterplot with Conditioning"),

   # Sidebar
   sidebarPanel(
     sliderInput()
   ), #end sidebarPanel


   # Here comes the main panel

   mainPanel(

     conditionalPanel(
       condition="input.resample == 0 || output.totalPrev == output.total",
       tabsetPanel(selected="Set-Up",
                   tabPanel("Set-Up",
                            plotOutput("mosaicInitial"),
                            fluidRow(
                              column(3,
                                     h5("Observed"),
                                     tableOutput("obsTable")
                              ),
                              column(3,
                                     h5("Expected by Null"),
                                     tableOutput("expTable")
                              ),
                              column(3,offset=1,
                                     h5("Contributions"),
                                     tableOutput("contrTable")
                              )
                            ),
                            hr(),
                            h5(textOutput("remarksInitial"))
                   ),
                   tabPanel("App Help",
                            includeHTML(system.file("doc/chisqSimShiny.html",
                                                    package="ShinyLocalEdu"))
                   )
       ) # end tabset panel
     ),

     conditionalPanel(
       condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
       tabsetPanel(selected="Latest Simulation",
                   tabPanel("Latest Simulation",
                            plotOutput("mosaicLatest"),
                            fluidRow(
                              column(4,
                                     h5("Simulated Table"),
                                     tableOutput("latestTable")
                              ),
                              column(4,offset=2,
                                     h5("Expected Table"),
                                     tableOutput("latestExpTable")
                              )
                            ),
                            p(textOutput("remarksLatest1")),
                            tableOutput("summary1"),
                            p(textOutput("remarksProbBar"))),
                   tabPanel("Density Plot of Simulations",
                            plotOutput("densityplot"),
                            p(textOutput("remarksLatest2")),
                            tableOutput("summary2"),
                            p(textOutput("remarksProbDensity"))),
                   tabPanel("Probability Distribution",
                            plotOutput("chisqCurve"),
                            br(),
                            checkboxInput("compareDen","Compare with simulated chi-square distribution"),
                            p(textOutput("remarksProb"))
                   ),
                   id="MyPanel"
       )
     ),
     width = 9
   )# end mainPanel

 ) #end ui


 shiny::shinyApp(ui = ui, server = server)
