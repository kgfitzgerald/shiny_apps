#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(infer)
library(plotly)


# Define UI for application that diplays two density plots and the datatable
ui <- fluidPage(
  
  # Application title
  titlePanel("Z-Scores"),
  
  # Sidebar with a slider inputs
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu",
                  "Mean SAT score:",
                  min = 1000,
                  max = 2000,
                  value = 1500,
                  step = 10),
      sliderInput("sd",
                  "Standard Deviation of SAT scores:",
                  min = 100,
                  max = 300,
                  value = 100,
                  step = 10),
      sliderInput("z_star",
                  "Z* cutoff value",
                  min = 0,
                  max = 3,
                  value = 1,
                  step = 0.01),
      actionButton("generate", "Generate sample"),
      textOutput("Instructions"))
,

    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Interface", plotOutput("plots"),
                           textOutput("SAT_lb"),
                           textOutput("SAT_ub"),
                           textOutput("percent_within_range"),
                           textOutput("z_score_statement"),
                           hr(),
                           DT::dataTableOutput("table")),
                  tabPanel("Info", p(strong("WHAT IS IT?")),
                           p("This app develops intuition about the statistical measure 
                             of a z-score and the standard normal distribution, which is a special case of the normal distribution that has mean 0 
                             and standard deviation 1. As discovered in the standard deviations app, standard deviations have consistent properties 
                             that are useful in describing what percentage of values you expect to fall within a certain range around the mean. 
                             This app explores the z-score as a way to put data on a standard scale that is in terms of standard deviation units. 
                             This app uses SAT scores as its context, which we know in practice to roughly follow a normal distribution. 
                             This app allows you to explore how z-scores behave under many different mean and standard deviation combinations and 
                             what information they tell you about the distribution of SAT scores."), 
                           p(strong("HOW IT WORKS")),
                           p("There are two sliders to specify the mean and standard deviation of SAT scores for the population. 
                             Each time the sliders are adjusted, 10,000 random SAT scores from a normal distribution with the specified mean and standard deviation are generated, 
                             and a corresponding z-score is calculated for each SAT score. The data is visualized in two density plots (i.e. smoothed histograms) and is presented in a table. 
                             There is an additional slider to specify a Z* cutoff value. Observations that fall within Z* standard deviations of the mean are shaded in blue. The exact percentage that fall within the range is calculated and displayed. 
                             Lower and upper bounds of the resulting range are also displayed below the plots."),
                           p(strong("THINGS TO NOTICE")),
                           p("Pay attention to the distance_from_mean and z_score columns in the data table, and try to discern the relationship between the two.  
                             What makes some z-scores negative? Do larger distances correspond to smaller or larger z-scores? 
                             What are these two numbers telling you about the SAT score of the student represented by that row?"),
                           p("Pay attention to how the lower and upper bounds change as you change Z*."),
                           p(strong("THINGS TO TRY")),
                           p("Try various mean and standard deviation combinations using the sliders and observe the resulting density plots. 
                             Do the two density plots have the same shape? What is the range of values in each density plot? What is the center value for each? Try changing the sd slider and generate a new sample. 
                             Do the two density plots have the same shape now? Are the density plots always symmetric? 
                             Did the range of the SAT density plot change? What about the Z density plot? Why might this be the case? 
                             Try to discern how the range of the SAT density plot relates to the values you set for the mean and standard deviation."),
                           p("Explore the relationship between Z* and the upper and lower bounds by moving the Z* slider. 
                             Does this relationship change if you change the mean and standard deviation? 
                             Try to find the Z* values that cause approximately 68.5%, 95%, and 99.7% of the SAT scores to be red. 
                             How do your findings compare to the rule you established in the standard deviations app?"))
                             ))
      
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  scores <- reactive({
    ###generates 10000 SAT scores from normal distribution with specified mean and sd
    ###calculates Z-score for each SAT score
    ###determines if each datapint falls within the range specified by the Z* cutoff value
    input$generate
    isolate(data.frame(SAT = signif(rnorm(10000, mean = input$mu, sd = input$sd), digits = 3)) %>% 
                mutate(distance_from_mean = (SAT - input$mu),
                       z_score = (SAT - input$mu)/(input$sd),
                       within_range = as.factor(case_when(abs(z_score) <= input$z_star ~ "yes",
                                              abs(z_score) > input$z_star ~ "no"))))
  })
    
  results <- reactive({
    ###computes lower and upper bounds for the range according to the specified the Z* cutoff value
    ###calculates the proportion of observations that fall within the computed range
    input$generate
    isolate({SAT_lb <- input$mu - input$z_star * input$sd 
    SAT_ub <- input$z_star * input$sd + input$mu
    percent_within_range <- sum(scores()$within_range == "yes") / 10000
    list(SAT_lb = SAT_lb, SAT_ub = SAT_ub, percent_within_range = percent_within_range)})
  })
    
  output$plots <- renderPlot({
    ###creates two density plots, one for the SAT scores and one for the Z-scores
    input$generate
    isolate({
      g1 <- ggplot(scores()) +
        geom_density(aes(x = SAT), fill = "grey") +
        xlim(c(min(scores()$SAT) - 10, max(scores()$SAT) + 10)) +
        scale_y_continuous(NULL, breaks = NULL) +
        theme(legend.position = "none",
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 14),
              plot.title = element_text(hjust = 0.5)) +
        shade_ci(endpoints = c(results()$SAT_lb, results()$SAT_ub)) +
        ggtitle(paste(results()$percent_within_range*100, "% within range"))
      
      g2 <-  ggplot(scores()) +
        geom_density(aes(x = z_score), fill = "grey") +
        xlim(c(min(scores()$z_score) - .1, max(scores()$z_score) + .1)) +
        scale_y_continuous(NULL, breaks = NULL) +
        theme(legend.position = "none",
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 14),
              plot.title = element_text(hjust = 0.5)) +
        shade_ci(endpoints = c(input$z_star*-1, input$z_star)) +
        ggtitle(paste(results()$percent_within_range*100, "% within range"))
     grid.arrange(g1,g2, nrow = 2)
    })
  })
    
  output$table <- DT::renderDataTable({
    ###displays table of generated data
    input$generate
    isolate(scores() %>% mutate(z_score = round(z_score, 2)))
  }) 

  output$percent_within_range <- renderText({
    input$generate
    isolate(paste(results()$percent_within_range*100,"% of SAT scores fall within", 
                      input$z_star, "standard deviations (i.e. within", input$z_star*input$sd, 
                      "points) of the mean of", input$mu))
  })
  
  output$SAT_lb <- renderText({
    input$generate
    isolate(paste("Lower bound:", results()$SAT_lb))
  })
  
  output$SAT_ub <- renderText({
    input$generate
    isolate(paste("Upper bound:", results()$SAT_ub))
  })
  
  output$z_score_statement <- renderText({
    input$generate
    isolate(paste(results()$percent_within_range*100,
                      "% of corresponding Z-scores fall between", 
                      -input$z_star, "and", input$z_star))
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

