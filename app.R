#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(imager)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mandelbrot & Julia Viewer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
            selectInput("plot_type","Select plot type:",choices=c("Mandelbrot","Julia"), selected = "Mandelbrot")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot")
            plotOutput("mbrotPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #  plot_type <- reactive(input$plot_type)
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    
    output$mbrotPlot <- renderPlot({
        
        
        num_rows <- 1000   # how many rows/columns in our image?
        cutoff <- 5       # how big is "infinity?" doesn't have to be big
        n_iter <- 20      # how many iterations? more take longer but give more detail
        
        
        
        # set the boundaries of our plot
        x_min <- -2
        x_max <- 2
        y_min <- -2
        y_max <- 2
        
        # are we doing mbrot or julia?
        #mbrot_flag <- TRUE
        mbrot_flag <- case_when(
            input$plot_type == "Mandelbrot" ~ TRUE,
            input$plot_type == "Julia" ~ FALSE
            )
        julia_x <- -0.5
        julia_y <- 0.5
        
        # set up our row and column indices
        v <- vector(mode="double")
        for (ind in 1:num_rows){
            y <- y_min + ((y_max-y_min)/(num_rows-1))*(ind-1)
            v <- c(v, rep(y, num_rows))
        }
        
        x <- rep(seq(x_min,x_max,length.out=num_rows), num_rows)
        
        # set up our tibble
        mbrot <- tibble(x,y=v, c=0) %>%
            mutate(y=complex(imaginary=y), z = (x+y))
        
        # set up a progress bar
       # pb <- progress_bar$new(total = n_iter)
        
        # do our number of iterations. could this be done completely tidily with a map or a recursive function?
       
        if (mbrot_flag){
            for (i in 1:n_iter){
                mbrot <- mbrot %>%
                    mutate(z = z^2 + (x+y)) %>%
                    mutate(c = if_else( abs(mbrot$z) > cutoff & c == 0 , as.double(i), c))
               # pb$tick()
            }
        } else {
            for (i in 1:n_iter){
                mbrot <- mbrot %>%
                    mutate(z = z^2 + complex(real=julia_x, imaginary=julia_y)) %>%    # this one does a classic julia
                    mutate(c = if_else( abs(mbrot$z) > cutoff & c == 0 , as.double(i), c))
              # pb$tick()
            }
        }

        
        
        ### try it with imager: based on toc/toc test, imager is about 30 times faster
        # https://rdrr.io/cran/imager/man/as.cimg.data.frame.html
        #tic()
        q <- expand.grid(x=1:num_rows, y=1:num_rows) %>%
            mutate(value = mbrot$c)
        
        img <-  as.cimg(q,dims=c(num_rows,num_rows,1))#
        
        img %>%
            plot(axes=FALSE)
        
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
