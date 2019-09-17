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
library(gridExtra)
library(ggpubr)
library(tidyverse)


# Define UI for application to plot confidence intervals and display table
ui <- fluidPage(
  withMathJax(),
  # Application title
  titlePanel("Confidence Intervals"),
  
  # Sidebar with a slider input for sample size and Z* value
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Sample size",
                  min = 1,
                  max = 500,
                  value = 250),
      sliderInput("z_star",
                  "Z*",
                  min = 0,
                  max = 3,
                  value = 1, 
                  step = 0.01))
    ,
    
    # create Interface and Info tabs
    mainPanel(
      tabsetPanel(type = "tabs",
                    tabPanel("Interface", plotOutput("forest", height = "600px"),
                             textOutput("success_rate"),
                             textOutput("percent_within_range"),
                            DT::dataTableOutput("table")),
                    tabPanel("Info", p(strong("WHAT IS IT?")),
                  p("This app explores the statistical concept of confidence intervals. In statistics, we often do not observe the whole population, 
                    but rather we rely on random samples to infer something about the population. For example, if we have a random sample of 200 students 
                    and their SAT scores, we can use the average SAT score in our sample as an estimate of the true average SAT score in the population, 
                    but our estimate contains some uncertainty. If we were to draw a different random sample of students, it's likely that we would observe 
                    a slightly different average SAT score. Confidence Intervals are one way to address this uncertainty; instead of reporting our one 
                    sample average as the true average, we report a range around our estimate that we think the true mean falls in, with varying degrees
                    of confidence. This app develops intuition about how we decide how wide of a range is appropriate and how confident we should be 
                    that our range contains the true population mean."),
                  p(strong("HOW IT WORKS")),
                  p("This app again uses SAT scores as an example and fixes the true population mean (\\(\\mu\\)) and standard deviation (\\(\\sigma\\)) to be 1500 and 
300 respectively. There are two sliders for sample size (n) and Z*. Ten thousand sample means are drawn from a normal distribution with mean 1500 and 
standard deviation \\(\\frac{300}{\\sqrt{n}}\\) (due to the Central Limit Theorem). For each sample mean, a confidence interval is computed. The
table displays the sample mean, the lower and upper bounds and width of the confidence interval, and an indicator variable for whether 
the interval contains the true mean of 1500. A random sample of 100 confidence intervals are plotted. This subset is sampled via proportional allocation to maintain the success rate of the full dataset of 10,000 sample means.
If the confidence interval contains the mean (success = 1), it is shaded blue, and it is shaded red if 1500 falls outside the interval. 
The success rate emerges as an estimate of the level of confidence of the interval. That is, it is a representation of how often 
this procedure - of drawing a random sample and calculating a range around the sample mean using the specified sample size and Z* value - was successful at containing the true population mean"),
                  p(strong("THINGS TO NOTICE")),
                  p("Pay attention to both the color and the width of the intervals and notice which values of \\(\\bar{x}\\)
                    give blue confidence intervals and which ones give red confidence intervals. Which ones are farther away from the mean of 1500? 
                    Remember that all of these \\(\\bar{x}\\) values are generated in the same way from the normal distribution (check out the code to see the specifics), 
                    so why are some of the values farther away from 1500 than others?"),
                  p(strong("THINGS TO TRY")),
                  p("Pay attention to both the success rate and the width of the shaded intervals. Try using different Z* values, 
                    but keep sample size the same. What happens to the success rate and width of the confidence intervals when you change Z*? 
                    Are larger Z* values associated with a narrower or wider interval? Are wider intervals associated with a lower or higher 
                    success rate? Try keeping Z* the same and try various values for n. What happens to the width of the interval when you 
                    increase the sample size? Why might this be the case? Would you expect to have a better estimate of the true average SAT score 
                    if you took a sample of 20 people or of 200 people? Does anything happen to the success rate when you change n? 
                    What role does n play in how \\(\\bar{x}\\) is generated? Would a larger n cause the distribution of \\(\\bar{x}\\) to be more spread out or 
                    more concentrated around \\(\\mu = 1500\\)? Try reasoning about why nothing happens to the success rate when you change n, 
                    and talk with your classmates and professor about it. Try to determine which Z* values lead to a success rate of 
                    approximately 90%, 95%, and 99%. How do these Z* values compare to the ones you found in the Z-scores app and to the rule 
                    you developed in the Standard Deviations app? Talk with a friend and/ or your professor about the relationships between these three concepts.")
                  ) 
    )
  )
  )
)
  


# Define server logic required to compute and plot CIs
server <- function(input, output) {
  
  I <- reactive(10000)
  CIs <- reactive({data.frame(xbar = rnorm(I(), mean = 1500, sd = 100 / sqrt(input$n))) %>% 
      mutate(lb = xbar - input$z_star * 100 / sqrt(input$n),
             ub = xbar + input$z_star * 100 / sqrt(input$n),
             width = ub - lb,
             success = as.factor(case_when(1500 >= lb & 1500 <=ub ~ 1,
                                           1500 < lb | 1500 > ub ~ 0)),
             label = paste0("X", 1:I()))})
  
  success_rate <- reactive(sum(CIs()$success == 1) / I())
    
  output$forest <- renderPlot({
    
    CIs_subset <- reactive({
      CIs() %>%
        group_by(success) %>%
        mutate(num_rows=n()) %>%
        sample_frac(100/I(), weight=num_rows) %>%
        ungroup
    })
    
    ggplot(CIs_subset()) +
      geom_pointrange(aes(x = label, y = xbar, ymin = lb, ymax = ub, color = success)) +
      coord_flip() +
      geom_hline(yintercept= 1500, lty=2) +
      ylab("Confidence Intervals") +
      xlab(" ") +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.title.x = element_text(size = 14)) +
      ylim(1400, 1600)
    
  })
  
  output$success_rate <- renderText(paste(round(success_rate()*100, 1), "% of the confidence intervals contain the true mean", 1500))
  
  output$table <- DT::renderDataTable(select(CIs(), -label) %>% mutate_if(is.numeric, round, 0) %>% 
                                        rename(`Lower Bound` = lb, `Upper Bound` = ub, 
                                               `Width of interval` = width))
}

# Run the application 
shinyApp(ui = ui, server = server)
