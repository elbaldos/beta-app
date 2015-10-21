library(shiny)
library(ggplot2)
require(latex2exp) 
# It converts a LaTeX string to a plotmath expression,
# So we can use LaTeX code.

# Define server logic required to draw all beta distribuitions
shinyServer(function(input, output) {

 
  # Expression that generates a plot. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$pEstimationText <- renderText({ 
    pEstimation <- input$x / input$n
    # paste("Estimation of the probability of success \\(\\frac{x}{n} \\): ", round(pEstimation, 3) )
    # paste("Estimation of \\(\\hat{p} = \\frac{x}{n}\\): ", round(pEstimation, 3) ) 
    round(pEstimation, 3) 
  })
  
  output$betaPlot <- renderPlot({
    
    # CHECK
    validate( 
      need(input$x <= input$n, "The number of successes x must be less than the number of trial n!"),
      need(input$x >= 0, "The number of successes x must be at least 0!") )
    
    # PARAMETERS and INPUTS
    
     prior_beta_mean <- input$alpha / (input$alpha + input$beta)
     pEstimation <- input$x / input$n
     new_alpha <- input$alpha + input$x
     new_beta <- input$beta + input$n - input$x
     posterior_beta_mean <- new_alpha / (new_alpha + new_beta)
     
     # Rappresentazione grafica
     # About MAR =
     # A numerical vector of the form c(bottom, left, top, right) which gives the
     # number of lines of margin to be specified on the four sides of the plot.
     # The default is c(5, 4, 4, 2) + 0.1.
     p <- par(mar = c(5, 4, 4, 2) + 0.9, mfrow= c(2,2))

     #Grafici
      
      # 1
    # Prior Probability Density Function 
     curve(dbeta(x, input$alpha, input$beta), from = 0, to = 1, 
           col = "orange", lwd = 3, xlab = "p" , ylab = latex2exp("$\\pi(p)$"), 
           main = latex2exp("Prior Probability Density $\\pi(p)$"), 
           cex.main = 1.75, cex.lab = 1.8, cex.axis = 1.6, n = 250)
     abline(v = prior_beta_mean, lty = 2, lwd = 1.5, col = "orange") # lty = 1 is "solid" (Line TYpe), lwd = Line WiDth
      
      # 2
     # Prior Predictive Probability Mass Function
     # Parameters of the new plot
     n <- input$n
     alpha.prior <- input$alpha
     beta.prior <- input$beta
     
     x.seq <- 0:n
    
     # Calculate on a log scale
     log.y.seq <- lchoose(n, x.seq) + lbeta(x.seq + alpha.prior, n - x.seq + beta.prior) - lbeta(alpha.prior, beta.prior)
     y.seq <- exp(log.y.seq)
     
     plot(x.seq, y.seq, xlab = latex2exp("x^{new}"), ylab = latex2exp("$\\Pr(X^{new} = x^{new})$"),
           main = latex2exp("Prior Pred. Prob. Mass $\\Pr(X^{new} = x^{new})$"), 
          ylim = c(0,max(y.seq)), # this solve the problem in prior predictive probability mass function due to rounding
          type = "n", col = "orange",  # type = "n" put invisible point on the plot
          cex.main = 1.75, cex.lab = 1.8, cex.axis = 1.6) 
     segments(x.seq, 0, x.seq, y.seq, col = "orange", lwd = 0.5, lty = 2) # to make visible the segments
     points(x.seq, y.seq, col = "orange", pch = 16)  # to fill the point
 
      # 3
     # Posterior Probability Density Function
     curve(dbeta(x, new_alpha , new_beta ), from = 0, to = 1, 
           col = "red", lwd = 3, xlab = "p" , ylab = latex2exp("$\\pi(p | x)$"), 
           main = latex2exp("Posterior Probability Density $\\pi(p | x)$"),
           cex.main = 1.75, cex.lab = 1.8, cex.axis = 1.6, n = 250)
     abline(v = posterior_beta_mean, lty = 2, lwd = 1.2, col = "red") # lty = 1 is "solid" (Line TYpe), lwd = Line WiDth
     abline(v = pEstimation, lty = 1, lwd = 2, col = "green2")
     # to add a curve in a plotted plot ---> "add = TRUE"
     curve(dbeta(x, input$alpha , input$beta), from = 0, to = 1, lty = 1, lwd = 2, col = "orange", add = TRUE, n = 250) # the old probability: lty = 2 is "dashed" (Line TYpe), lwd = Line WiDth
     abline(v = prior_beta_mean, lty = 2, lwd = 1.5, col = "orange")
     # beta_mean <- input$alpha / (input$alpha + input$beta)
     # abline(v = beta_mean, lty = 2, lwd = 1)
     legend("topleft", legend = c("Prior density with mean", 
                                  "Posterior density with mean", 
                                  latex2exp("$\\hat{p} = \\frac{x}{n}$")), 
            col = c("orange", "red", "green2"), lty = 1, lwd = 2,
            bty = "n")  # bty is the tipe of the box of the legend: "n" means neutral
     
     # 4
     # Posterior Predictive Probability Mass Function
     
     # Parameters of the new plot
     x <- input$x # Observed data
     
     log.y.seq2 <- lchoose(n, x.seq) + lbeta(x.seq + x + alpha.prior, n - x.seq + n - x + beta.prior) - lbeta(x + alpha.prior, n - x + beta.prior)
     y.seq2 <- exp(log.y.seq2)
     
     plot(x.seq, y.seq2, xlab = latex2exp("x^{new}"), ylab = latex2exp("$\\Pr(X^{new} = x^{new}|X = x)$"),
          main = latex2exp("Post. Pred. Prob. Mass $\\Pr(X^{new} = x^{new}|X = x)$"),
          type = "n", col = "red",  # type = "n" put invisible point on the plot
          cex.main = 1.45, cex.lab = 1.65, cex.axis = 1.6)  
     segments(x.seq, 0, x.seq, y.seq2, col = "red", lwd = 0.5, lty = 2) # to make visible the segments
     abline(v = input$x, lty = 1, lwd = 2.5, col = "cadetblue1")
     points(x.seq, y.seq2, col = "red", pch = 16)  # to fill the point
     legend("topleft", legend = c("Number of success"), 
            col = c("cadetblue1"), lty = 1, lwd = 2,
            bty = "n")  # bty is the tipe of the box of the legend: "n" means neutral
     par(p)

 }) 

  output$meanText <- renderText({ 
    beta_mean <- input$alpha / (input$alpha + input$beta)
    paste("Prior Mean of p: ", round(beta_mean, 3) ) # To three significant figures
  }) 
  
  output$sdText <- renderText({ 
    beta_var <- (input$alpha * input$beta) / ( ((input$alpha + input$beta)^2)*(input$alpha + input$beta + 1) )
    paste("Prior Standard Dev of p:", round(sqrt(beta_var), 3)) # To three significant figures
  })
  
  output$PosteriorMeanText <- renderText({ 
    new_alpha <- input$alpha + input$x
    new_beta <- input$beta + input$n - input$x
    posterior_beta_mean <- new_alpha / (new_alpha + new_beta)
    paste("Posterior Mean of p:", round(posterior_beta_mean, 3) ) # To three significant figures
  }) 
  
  output$PosteriorsdText <- renderText({
    new_alpha <- input$alpha + input$x
    new_beta <- input$beta + input$n - input$x
    posterior_beta_var <- (new_alpha * new_beta) / ( ((new_alpha + new_beta)^2)*(new_alpha + new_beta + 1) )
    paste("Posterior Standard Dev of p: ", round(sqrt(posterior_beta_var), 3)) # To three significant figures
  })
    
}) #function
