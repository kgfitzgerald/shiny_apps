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
library(infer)
library(gridExtra)

# Define UI for application that draws histograms for various SD values
ui <- fluidPage(
   
   # Application title
   titlePanel("Standard Deviation"),
   
   # Sidebar with a slider input for standard deviations for each group and range to shade
   sidebarLayout(
     sidebarPanel(
         sliderInput("sd_1",
                     "Standard Deviation for Group 1 (sd_1):",
                     min = 0.25,
                     max = 2,
                     value = 1),
         sliderInput("sd_2",
                     "Standard Deviation for Group 2 (sd_2):",
                     min = 0.25,
                     max = 2,
                     value = 1),
         sliderInput("sd_range",
                     "Standard deviation range to shade",
                     min = 0.5,
                     max = 3,
                     value = 1),
         actionButton("generate", "Generate samples"),
         br(),
         textOutput("lb_1"),
         textOutput("ub_1"),
         br(),
         textOutput("lb_2"),
         textOutput("ub_2"))
     ,
         
      # Create Interface tab (to show histograms of the generated distributions) and Info tab
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Interface", plotOutput("hist_1", height = "600px", width = "100%"),
                             plotOutput("hist_2", height = "300px"),
                             textOutput("percent_within_range"),
                             hr(),
                             DT::dataTableOutput("table")),
                    tabPanel("Info", p(strong("WHAT IS IT?")),
                             p("This app explores the statistical concept of standard deviation. When we observe data on lots 
of individuals, we often want to consider all of the data points in relation to one another. We might want to know which values 
occur most often or what the range of observed values is. Histograms are a convenient way to visualize this. Most students are 
already familiar with with the concept of a", strong("mean"), "(i.e. average), which describes the", em("center"), "of the data. Another important
feature for summarizing data is,", strong("standard deviation"), "which is a way to quantify how", 
em("spread out"), "the data is. Together, mean and standard deviation are two numbers in statistics that we 
use to summarize the", strong("distribution"), "of the data. This app develops intuition about the concept of standard 
deviation as a measure of spread as well as its usefulness in determining how much of the data you should expect to fall 
within a certain range. This app uses average hours of sleep among US college students as an example. Perhaps we are 
interested in differences between athletes vs. non-athletes. It is difficult to survey 
all college students in the US about their sleep habits, but we could survey a random sample from 
each sub-population (athlete vs non-athlete). This app mimics what we might observe if we did just that. We set the mean 
hours of sleep to be 7 in both populations, but the app allows you to observe what happens as the standard deviations vary."), 
                             p(strong("HOW IT WORKS")),
                             p("We generate reported number of hours of sleep by drawing random x1 and x2 values from normal 
distributions with mean 7 and standard deviations equal to the specified values for Groups 1 and 2, for athletes and non-athletes 
respectively. Two histograms are created to display the distribution of the sample data drawn from the two populations. The third slider 
allows you to shade the students in the histogram that fall within a certain distance from the mean. 
For example, if you set the third slider to 2 the students that fall within 2 standard deviations of 
the mean (above or below) will be shaded in blue. Text will also display numerically the proportion of students that fall 
within that range for each sample."),
                             p(strong("THINGS TO NOTICE")),
                             p("Pay attention the Group ranges displayed at the bottom of the left sidebar panel.
Notice that the third slider controls the range for both samples. That is, if the slider is set to 2, then the students
that fall within 2*sd_1 hours above or below the mean are shaded in 
sample 1, and the students that fall within 2*sd_2 hours above or below the mean are shaded in sample 2. In general, 
take notice of similarities and differences between sample 1 and sample 2 in  each feature of the app (histograms and 
text output) and of how all of the features relate to sd_1 and sd_2."),
                             p(strong("THINGS TO TRY")),
                             p("Try changing the values of the two group standard deviation sliders. What happens to the 
histograms when you change sd_1 and sd_2? How does the distribution of a sample with a larger standard deviation compare to 
one with a smaller standard deviation? What percentage of students fall within the range in each sample? Are these two numbers similar? Try this for several values of 
the third slider, and pay attention to any patterns. Do you expect the percentages to be similar if sd_1 and sd_2 are very different? 
Try this with several different datasets using various sd_1 and sd_2 values to find out. 
Is the pattern you're seeing surprising? Try making statements about what you're visualizing in the context of this data: \"___% 
of the athletes get between ___ and ___ hours of sleep per night\" and \"___% of non-athletes get between ___ and ___ 
hours of sleep per night.\" Why are the ranges different even if the percentages are similar? Which group has a wider range? 
How do these ranges change as you change sd_1 and sd_2? Try coming up with a \"rule\" for what percentage of observations 
you should expect to fall within 1 standard deviation of the mean by setting the third slider to 1 and observing the percentages 
for several datasets, with various combinations of sd_1 and sd_2. Come up with a similar rule for a standard deviation range of 2 and 3."))
        )
      )
   )
)

# Define server logic required to draw histograms
server <- function(input, output) {
  
  #function to compute upper and lower bounds and proporitons that fall within those bounds
  within_range <- function(sd_range, vector){
    lb <- mean(vector) - sd_range*sd(vector)
    ub <- mean(vector) + sd_range*sd(vector)
    prop <- sum(vector > lb & vector < ub)/length(vector)
    return(c(lb,ub,prop))
  }
  
  #generates 100000 data points from distributions with mean 7 and the specified standard deviation for group 1
  #and determines whether each datapoint falls within the range specified by the Z* cutoff value
  x1_data <- reactive({
    input$generate
    isolate(data.frame(x1 = rnorm(100000, mean = 7, sd = input$sd_1)) %>% 
               mutate(within_range = as.factor(case_when(x1 >= 7 - input$sd_range * input$sd_1 & x1 <= 7 + input$sd_range * input$sd_1 ~ 1,
                                        x1 < 7 - input$sd_range * input$sd_1 | x1 > 7 + input$sd_range * input$sd_1 ~ 0))))
  })
  
  #generates 100000 data points from distributions with mean 7 and the specified standard deviation for group 2
  #and determines whether each datapoint falls within the range specified by the Z* cutoff value
  x2_data <- reactive({
    input$generate
    isolate(data.frame(x2 = rnorm(100000, mean = 7, sd = input$sd_2)) %>% 
               mutate(within_range = as.factor(case_when(x2 >= 7 - input$sd_range * input$sd_2 & x2 <= 7 + input$sd_range * input$sd_2 ~ 1,
                                        x2 < 7 - input$sd_range * input$sd_2 | x2 > 7 + input$sd_range * input$sd_2 ~ 0))))
  })

  
  percent_within_range <- reactive({
    #computes the proportion of observations within each group that fall within the specified range
    #computes lower and upper bounds of range for each group
    input$generate
    isolate({
      x1_prop <- sum(x1_data()$within_range == 1) / 100000
      x2_prop <- sum(x2_data()$within_range == 1) / 100000
      lb_1 <- 7 - input$sd_range * input$sd_1
      lb_2 <- 7 - input$sd_range * input$sd_2
      ub_1 <- 7 + input$sd_range * input$sd_1
      ub_2 <- 7 + input$sd_range * input$sd_2
      list(x1_prop = x1_prop, x2_prop = x2_prop, lb_1 = lb_1, lb_2 = lb_2, ub_1 = ub_1, ub_2 = ub_2)      
    })
  })
  
   output$hist_1 <- renderPlot({
    #creates histograms for each group's generated data
    input$generate
     isolate({
       g1 <- ggplot(x1_data()) +
         geom_histogram(aes(x = x1), binwidth = 0.25, color = "black", alpha = 0.8)+
         xlim(c(0,7 + 4*max(input$sd_1,input$sd_2))) +
         xlab("Group 1: Hours of Sleep (x1)") +
         theme(legend.position = "none",
               axis.text = element_text(size = 14),
               axis.title = element_text(size = 14)) +
         shade_ci(endpoints = within_range(input$sd_range, x1_data()$x1)[1:2]) 

       g2 <- ggplot(x2_data()) +
         geom_histogram(aes(x = x2), binwidth = 0.25, color = "black", alpha = 0.8)+
         xlim(c(0,7 + 4*max(input$sd_1,input$sd_2))) +
         xlab("Group 2: Hours of Sleep (x2)") +
         theme(legend.position = "none",
               axis.text = element_text(size = 14),
               axis.title = element_text(size = 14)) +
         shade_ci(endpoints = within_range(input$sd_range, x2_data()$x2)[1:2])
       g1.2 <- g1 + scale_y_continuous(expand = c(0,0),
                                       limits=c(0,max(max(ggplot_build(g1)$data[[1]]$count)*1.1, max(ggplot_build(g2)$data[[1]]$count)*1.1)))   
       g2.2 <- g2 + scale_y_continuous(expand = c(0,0),
                                       limits=c(0,max(max(ggplot_build(g1)$data[[1]]$count)*1.1, max(ggplot_build(g2)$data[[1]]$count)*1.1)))   
       grid.arrange(g1.2, g2.2, nrow = 2)      
     })

    })
   
   output$lb_1 <- renderText({
     input$generate
     isolate(paste(sep = "","Group 1 Range: [", percent_within_range()$lb_1, " , ", percent_within_range()$ub_1, "]"))
    })
   
   output$ub_1 <- renderText({
     input$generate
     isolate(paste(round(percent_within_range()$x1_prop*100, 1), "% fall within", input$sd_range, 
                   " standard deviations (i.e. within",input$sd_1*input$sd_range,
                   "hours) of the mean (7 hours)"))
   })
   output$lb_2 <- renderText({
     input$generate
     isolate(paste(sep = "","Group 2 Range: [", percent_within_range()$lb_2, " , ", percent_within_range()$ub_2, "]"))
   })
   output$ub_2 <- renderText({
     input$generate
     isolate(paste(round(percent_within_range()$x2_prop*100, 1), "% fall within", 
                   input$sd_range, " standard deviations (i.e. within", 
                   input$sd_2*input$sd_range,"hours) of the mean (7 hours)"))
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

