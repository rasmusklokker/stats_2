#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load fits.df

library(shiny)

library(ggplot2)

fits.df <- read.csv("resi_loop.csv")


ui <- fluidPage(
  sliderInput("rank", "Rank:",
              min = 1, max = max(fits.df$rank),
              value = c(1,max(fits.df$rank)),
              step=5, width = "800px"),
  fluidRow(
    column(width = 12, class = "well",
           h4("The slider selects observations with a given rank of their RSS. As such moving the slider to e.g.
              1500 would select the 1500 lowest/best RSS values
              You can zoom on the upper plot by drawing a box with the mouse. The zoomed area is shown in the bottom plot
              The lines indicate the actual values of the intercept(20) and beta_1(-0,2)"),
           column(width = 12,
                  plotOutput("plot2", height = 500,
                             brush = brushOpts(
                               id = "plot2_brush",
                               resetOnNew = TRUE
                             )
                  )
           ),
           column(width = 12,
                  plotOutput("plot3", height = 500)
           )
    )
  )

)

server <- function(input, output) {


  # -------------------------------------------------------------------
  # Linked plots (middle and right)
  ranges2 <- reactiveValues(x = NULL, y = NULL)

  output$plot2 <- renderPlot({
    p <- ggplot(data=fits.df[fits.df$rank<=input$rank,], aes(intercept, beta_1, color=rss))
    p <- p+geom_point()
    p <- p+ylim(0, -10)+xlim(0,40)
    p <- p+geom_hline(yintercept = -.2)
    p <- p+geom_vline(xintercept=20)
    p
  })

  output$plot3 <- renderPlot({
    p <- ggplot(data=fits.df[fits.df$rank<=input$rank,], aes(intercept, beta_1, color=rss))
    p <- p+geom_point()
    p <- p+ylim(0, -10)+xlim(0,40)
    p <- p+geom_hline(yintercept = -.2)
    p <- p+geom_vline(xintercept=20)
    p <- p+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = TRUE)
    p
  })

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })

}


shinyApp(ui, server)
