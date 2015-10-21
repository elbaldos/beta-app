# Define UI for application that draws all beta distribuitions
shinyUI( fluidPage(
    # Application title
    titlePanel("Beta distribuition in Bayesian Statistics"),
        # Sidebar with a numeric input for the parameters od distribuition
    sidebarLayout(    
        sidebarPanel(

          # Initial paragraph
            h3("Beta distribuition"),
            h4("The", a("beta distribution", href = "https://en.wikipedia.org/wiki/Beta_distribution"), 
              "is a family of continuous probability distributions defined on the interval \\( [0, 1] \\) 
              parametrized by two positive shape parameters, denoted by \\(\\alpha\\) and \\(\\beta\\), 
              that appear as exponents of the probability density function and control the shape of the distribution." , 
              style = "font-family: 'times'; font-si18pt" ), 

            # to write formulas like in LaTEX
            h5(withMathJax("$$ \ff(x)  = \\frac{x^{ \\alpha -1}(1-x)^{ \\beta - 1}}{B(\\alpha ,\\beta )} $$")),
            h5(withMathJax("$$  B(\\alpha ,\\beta ) = \\int_{0}^{1}{x^{ \\alpha -1}(1-x)^{ \\beta - 1}} dx $$")),
            
            # Image at the bottom
            h1(img(src = "beta_distribution_info.jpg", height = 199, width = 199), align = "center"),
            
            # Second paragraph
            h3("Prior and Posterior Density Function"),
            h5(withMathJax("Let's say that we observe data that are the number \\( x \\)
               of successes out of \\( n \\) independent trials, that is",  
               span("$$x \\sim \\mbox{Bi} (n, p), $$", style = "color:green"),
               "so that the associated probability mass function is", 
               span("$$\\Pr(x | p) =  \\binom{n}{x} p^x(1-p)^{n-x} . $$", style = "color:green"),
               strong("This is the data model.", style = "color:black"),
               "We want to estimate \\(p\\), the probability of success.
               One possible estimate could be",
               span("$$\\hat{p} =  \\frac{x}{n}.$$", style = "color:green"), 
               "This is the maximum likelihood estimate.")),
            h5(withMathJax("In", a("Bayesian statistics", href = "https://en.wikipedia.org/wiki/Bayesian_statistics"), 
               "we put a", strong("prior density function", style = "color:black" ), 
               "on p", span("$$p \\sim \\mbox{Beta} (\\alpha, \\beta), $$", style = "color:orange"),
                "so that the associated probability density function is",
               span("$$\\pi(p) = C \\, p^{ \\alpha -1}(1-p)^{\\beta -1},$$", style = "color:orange"),
                "in which \\( C \\) is a constant.  
                This prior distribution espress our knowledge about \\(p\\)",
               em("before seeing the data."),
               "Some expert specify the parameters
                \\(\\alpha \\) and \\(\\beta\\) so that the prior distribution of \\(p\\)
                has user specified values of the mean and standard deviation.")
               ),
            h5(withMathJax("We can now ask: ", 
               em("what do we know about \\( p \\space \\underline{\\mbox{after}}\\) seeing the data"),
               "?")),
            h5(withMathJax("We answer this question using the", 
                strong("posterior density function.", style = "color:black"),
                "By Bayes Theorem, the posterior density function is proportional to the 
                 product of the data model and the prior:",
                span("\\begin{eqnarray} \\pi(p | x) & \\propto & \\Pr(x | p) \\, \\pi(p) \\\\
                  & \\propto &  p^x (1-p)^{n-x} \\\\
                & & \\mbox{} \\times p^{ \\alpha -1}(1-p)^{\\beta -1}\\\\ 
                & = & p^{x + \\alpha - 1} (1 - p)^{n - x + \\beta - 1}, \\end{eqnarray}", style = "color:red"), # \\times is the X (multiplicator)
                "so that the posterior distribution of the parameter \\(p\\) given the data \\(x\\) is",
                span("$$\\mbox{Beta} (x + \\alpha, n - x + \\beta) . $$", style = "color:red"))),
            h4("The follow plots allow you to investigate the effect that the prior parameters
                and the data have on the posterior density in an interactive and easy way.
                It is also possible turning the point of view from the value p on the probability of prediction of
                the number of successes x, in both the cases before and after the observation of a champion of extractions.",style = "font-family: 'times'; font-si18pt")
            ), #sidebarPanel
        # Show a plot of the generated distribution
        mainPanel( 
              fluidRow(
                       # Sliders
                       column(6, sliderInput("alpha",
                                   label = h3("Value of \\(\\alpha\\) parameter:"),
                                   min = 0.01,  # 0 is not a good value in the distribution
                                   max = 10,
                                   value = 1)),                     
                       column(6, sliderInput("beta",
                                   label = h3("Value of \\(\\beta\\) parameter:"),
                                   min = 0.01,  
                                   max = 10,
                                   value = 1)),
                       column(6, numericInput("x", 
                                              label = h3("Number of success x:"), 
                                              value = 3)   ),
                       column(6, numericInput("n", 
                                              label = h3("Number of trial n:"), 
                                              value = 10)   ),
                       
                       column(9, h4("Maximum likelihood estimate \\( \\hat{p} = \\frac{x}{n}: \\)", 
                                    textOutput("pEstimationText", inline = TRUE) ) ),
                       ## PLOTS
                       # Prior Plot
                       plotOutput("betaPlot", width = "100%", height = "700px"),
                       
                       # Carriage return
                       br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                       
                       # helpText("The", span("green", style = "color:green"), "line is the value of ", withMathJax("$$\\frac{x}{n}$$"), ", the", strong("black", style = "color:black"), "line the mean of the new posterior distribution"),
                       h5(helpText("The", span("red curve", style = "color:red"), " is the posterior density function, with its mean shown by the dashed vertical line")),
                       h5(helpText("The", span("orange curve", style = "color:orange"), " is the prior density function, with its mean shown by the dashed vertical line")),
                       h5(helpText("The", span("green line", style = "color:green"), " is the value of maximum likelihood estimate \\(\\hat{p} = \\frac{x}{n}\\) ") ),
                       h5(helpText("The", span("orange points", style = "color:orange"), " are the probabilities of seeing \\(x^{new}\\) successes in n trials, before having observed the data x")),
                       h5(helpText("The", span("red points", style = "color:red"), " are the probabilities of seeing \\(x^{new}\\) successes in n trials, after having observed the data x")),
                       h5(helpText("The", span("cyan line", style = "color:skyblue"), " is the observed value of x, number of success.")),
                       # Mean and Standard Deviation
                       br(),
                       h4(strong("Others info:",style = "font-family: 'times'; font-si18pt")),
                       column(5.5, h4(textOutput("meanText"))),
                       column(5.5, h4(textOutput("sdText"))),
                       br(),
                       column(5.5, h4(textOutput("PosteriorMeanText"))),
                       column(5.5, h4(textOutput("PosteriorsdText"))),
                       br(), br(), br(),br(), br(), br(),
                       # A brief recap
                       h4(strong("A brief recap:",style = "font-family: 'times'; font-si18pt")),
                       h5("Before seeing the data we assume", 
                          span("\\(p \\sim \\mbox{Beta}(\\alpha, \\beta) \\)", style = "color:orange"),"."),
                       h5("The observed data comprise \\( x \\) successes out of \\( n \\) independent trials."),
                       h5("After seeing the data we have that", 
                          span("\\(p | x \\sim \\mbox{Beta}(x + \\alpha, n - x + \\beta)  \\)", style = "color:red"),".")
              ) # fluidRow
    
        ) #mainPanel
        
    ) # sidebarLayout
    
) #fluidPage
) # shinyUI
