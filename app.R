#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load R packages
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "Non-Inferiority Testing",
                  tabPanel("Difference in Proportions (Binary Outcome)",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("outcome_nature", "Nature of outcome ('good' or 'bad')", "good"),
                             numericInput("oT", "Number of outcomes in treatment group:", 59, min = 1, max = 1000),
                             numericInput("oC", "Number of outcomes in control group:", 59, min = 1, max = 1000),
                             numericInput("nT", "Number of samples in treatment group:", 132, min = 1, max = 1000),
                             numericInput("nC", "Number of samples in control group:", 135, min = 1, max = 1000),
                             numericInput("delta", "Margin of error:", 0.1, min = 0, max = 1),
                             numericInput("alpha", "Level of Significance", 0.05, min = 0, max = 1),

                           ),
                           mainPanel(
                             h1("Results on testing the non-inferiority"),
                             h3("P-value:"),
                             verbatimTextOutput("txtout"),
                             h3("One-sided CI:"),
                             verbatimTextOutput("txtout3"),
                             h3("Conclusion and Interpretation"),
                             verbatimTextOutput("txtout2"),
                             #textOutput("txtout4"),
                             verbatimTextOutput("txtout4"),
                             h3("Standard Normal Variate Curve"),
                             plotOutput("P1"),
                             plotOutput("P2"),
                           )
                  ),
                  tabPanel("Odds Ratio (Binary outcome)", "This panel is intentionally left blank"),
                  tabPanel("Hazard Ratio (Time-dependent binary outcome)", "This panel is intentionally left blank")
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  reactpT <- reactive({ round(input$oT / input$nT, 3) })
  reactpC <- reactive({ round(input$oC / input$nC, 3) })

  Lower <- reactive({
    one_minus_alpha <- 1-input$alpha
    if (input$outcome_nature == "good") {
      ci_good <- prop.test(c(input$oT, input$oC), c(input$nT, input$nC), correct = FALSE, alternative = "greater",conf.level = one_minus_alpha)
      LB_alter_reactive <- ci_good$conf.int[1]
      LB_alter_reactive
    } else if (input$outcome_nature == "bad") {
      ci_good <- prop.test(c(input$oC, input$oT), c(input$nC, input$nT), correct = FALSE, alternative = "less",conf.level = one_minus_alpha)
      LB_alter_reactive <- ci_good$conf.int[1]
      LB_alter_reactive
    }
  })

  Upper <- reactive({
    one_minus_alpha <- 1-input$alpha
    if (input$outcome_nature == "good") {
      ci_good <- prop.test(c(input$oT, input$oC), c(input$nT, input$nC), correct = FALSE, alternative = "greater",conf.level = one_minus_alpha)
      UB_alter_reactive <- ci_good$conf.int[2]
      UB_alter_reactive
    } else if (input$outcome_nature == "bad") {
      ci_good <- prop.test(c(input$oC, input$oT), c(input$nC, input$nT), correct = FALSE, alternative = "less",conf.level = one_minus_alpha)
      UB_alter_reactive <- ci_good$conf.int[2]
      UB_alter_reactive
    }
  })

  Lower_decision <- reactive({
    if (Lower() > -input$delta) {
      paste("This lower bound", Lower(), "is above the predefined non-inferiority margin of", -input$delta, "with p-value of", output$txtout(), ". Thus indicating non-inferiority achieved and hence we conclude that the new treatment \n is non-inferior to the control group treatment.")
    } else if (Lower() <= -input$delta) {
      paste("This lower bound", Lower(), "is below the predefined non-inferiority margin of", -input$delta, "with p-value of", output$txtout(), ". Thus indicating non-inferiority not achieved and hence we conclude that the new treatment is inferior to the control group treatment.")
    }
  })

  Upper_decision <- reactive({
    if (Upper() < input$delta) {
      paste("This upper bound", Upper(), "is below the predefined non-inferiority margin of", input$delta, "with p-value of", output$txtout(), ". Thus indicating non-inferiority achieved and hence we conclude that the new treatment is non-inferior to the control group treatment.")
    } else if (Upper() >= input$delta) {
      paste("This upper bound", Upper(), "is above the predefined non-inferiority margin of", input$delta, "with p-value of", output$txtout(), ". Thus indicating non-inferiority not achieved and hence we conclude that the new treatment is inferior to the control group treatment.")
    }
  })


  output$txtout <- renderPrint({
    if (input$outcome_nature == "good") {
      diffT_C <- reactpT() - reactpC()
      z_numerator <- diffT_C + input$delta
      z_denominator <- round(sqrt((reactpT() * (1 - reactpT()) / input$nT) + (reactpC() * (1 - reactpC()) / input$nC)), 4)
      z <- z_numerator / z_denominator

      # Pvalue for testing the non-inferiority over the lower bound
      phi_z <- pnorm(z)

      # Probability for testing non-inferiority at the lower bound for good outcome
      PNI <- 1 - phi_z
      PNI
    } else if (input$outcome_nature == "bad") {
      diffT_C <- reactpC() - reactpT()
      z_numerator <- diffT_C - input$delta
      z_denominator <- round(sqrt((reactpT() * (1 - reactpT()) / input$nT) + (reactpC() * (1 - reactpC()) / input$nC)), 4)
      z <- z_numerator / z_denominator

      phi_z <- pnorm(z)

      PNI <- phi_z
      PNI
    }
  })

  pval <- reactive({output$txtout})

  output$txtout3 <- renderText({
    paste(Lower(), Upper(), sep = " to ")
  })

  output$txtout2 <- renderPrint({
    # alternate for limits
    if (input$outcome_nature == "good") {
      cat("We here have considered the nature of outcome as", input$outcome_nature, "\n")
      cat("The hypothesis or testing the non-inferiority is given as follows ", "\n")
      cat("H0: D=pT-pC <= -delta    VS    H1: D=pT-pC > -delta","\n\n")

      cat("Proportion of outcome in treatment group (p_hat_T):", reactpT(), "\n")
      cat("Proportion of outcome in control group (p_hat_C): ", reactpC(), "\n\n")
      cat("One-sided", (1 - input$alpha)*100, "% CI is", Lower(), "to ", Upper(), " \n\n")
      cat("The lower bound of one-sided", (1 - input$alpha)*100, "% CI for the difference in proportions of good outcome \n between the treatment and the control (pT-pC) is ", Lower(), "\n\n")

    } else if (input$outcome_nature == "bad") {
      cat("We here have considered the nature of outcome as", input$outcome_nature, "\n")
      cat("The hypothesis of non-inferiority is given as follows ", "\n")
      cat("H0: D=pT-pC >= delta    VS   H1: D=pT-pC < delta")

      cat("Proportion of outcome in treatment group (p_hat_T):", reactpT(), "\n")
      cat("Proportion of outcome in control group (p_hat_C): ", reactpC(), "\n\n")
      cat("One sided", (1 - input$alpha)*100, "% CI is", Lower(), "to ", Upper(), " \n\n")
      cat("This upper bound of one-sided", (1 - input$alpha)*100, "% CI for the difference in proportions of bad outcome \n between the treatment and the control (pT-pC) is ", Upper(), "\n\n")
    }
  })

  output$txtout4 <- renderPrint({
    #alternate for limits
    if (input$outcome_nature == "good") {
      #Lower_decision()
      diffT_C <- reactpT() - reactpC()
      z_numerator <- diffT_C + input$delta
      z_denominator <- round(sqrt((reactpT() * (1 - reactpT()) / input$nT) + (reactpC() * (1 - reactpC()) / input$nC)), 4)
      z <- z_numerator / z_denominator
      # Pvalue for testing the non-inferiority over the lower bound
      phi_z <- pnorm(z)

      # Probability for testing non-inferiority at the lower bound for good outcome
      PNI <- 1 - phi_z
      if (Lower()> -input$delta){

        cat("This lower bound", Lower(), "is above the predefined non-inferiority margin of", -input$delta, "\n with p-value", PNI,". Thus indicating non-inferiority achieved and hence \n we conclude that the new treatment is non-inferior to \n the control group treatment.")
      } else if (Lower()<= -input$delta){
        cat("This lower bound", Lower(), "is below the predefined non-inferiority margin of", -input$delta, "\n with p-value", PNI,". Thus indicating non-inferiority not achieved and \n hence we conclude that the new treatment is inferior to \n the control group treatment.")
      }
    } else if (input$outcome_nature == "bad") {
      #Upper_decision()
      diffT_C <- reactpC() - reactpT()
      z_numerator <- diffT_C - input$delta
      z_denominator <- round(sqrt((reactpT() * (1 - reactpT()) / input$nT) + (reactpC() * (1 - reactpC()) / input$nC)), 4)
      z <- z_numerator / z_denominator
      # Pvalue for testing the non-inferiority over the lower bound
      phi_z <- pnorm(z)

      # Probability for testing non-inferiority at the lower bound for bad outcome
      PNI <- phi_z
      if (Upper() < input$delta) {
        cat("This upper bound", Upper(), "is below the predefined non-inferiority margin of", input$delta, "\n with p-value", PNI,". Thus indicating non-inferiority achieved and hence \n we conclude that the new treatment is non-inferior to \n the control group treatment.")
      } else if (Upper() >= input$delta) {
        cat("This upper bound", Upper(), "is above the predefined non-inferiority margin of", input$delta, "\n with p-value", PNI,". Thus indicating non-inferiority not achieved and hence \n we conclude that the new treatment is inferior to \n the control group treatment.")
      }
    }
  })


  output$P1 <- renderPlot({
    if (input$outcome_nature == "good") {
      # Calculate the z-value for the given cumulative probability
      diffT_C <- reactpT() - reactpC()
      z_numerator <- diffT_C + input$delta
      z_denominator <- round(sqrt((reactpT() * (1 - reactpT()) / input$nT) + (reactpC() * (1 - reactpC()) / input$nC)), 4)
      z <- z_numerator / z_denominator
      # Pvalue for testing the non-inferiority over the lower bound
      phi_z <- pnorm(z)

      # Probability for testing non-inferiority at the lower bound for good outcome
      PNI <- 1 - phi_z
      z_value <- qnorm(PNI)

      # Create a standard normal distribution curve
      x <- seq(-3, 3, length.out = 1000)
      y <- dnorm(x)

      # Plot the standard normal distribution curve
      plot(x, y, type = "l", ylab = "Density", xlab = "z",
           main = "Standard Normal Distribution Curve with Quantile",
           col = "blue", lwd = 2)

      # Highlight the area under the curve for P(Z <= z)
      polygon(c(-3, x, z_value), c(0, y, 0), col = "skyblue", border = NA)

      # Add a vertical line at the calculated z-value
      abline(v = z_value, col = "red", lty = 2)

      # Add text labels
      text(z_value, 0.1, labels = paste("z =", round(z_value, 2)), pos = 4, col = "red")
      text(z_value, 0.2, labels = paste("p =", round(PNI, 4)), pos = 4, col = "red")
    } else if (input$outcome_nature == "bad") {
      # Calculate the z-value for the given cumulative probability
      diffT_C <- reactpC() - reactpT()
      z_numerator <- diffT_C - input$delta
      z_denominator <- round(sqrt((reactpT() * (1 - reactpT()) / input$nT) + (reactpC() * (1 - reactpC()) / input$nC)), 4)
      z <- z_numerator / z_denominator
      # Pvalue for testing the non-inferiority over the lower bound
      phi_z <- pnorm(z)

      # Probability for testing non-inferiority at the lower bound for bad outcome
      PNI <- phi_z
      z_value <- qnorm(1 - PNI)

      # Create a standard normal distribution curve
      x <- seq(-3, 3, length.out = 1000)
      y <- dnorm(x)

      # Plot the standard normal distribution curve
      plot(x, y, type = "l", ylab = "Density", xlab = "z",
           main = "Standard Normal Distribution Curve with Quantile",
           col = "blue", lwd = 2)

      # Highlight the area under the curve for P(Z >= z)
      polygon(c(z_value, x, 3), c(0, y, 0), col = "skyblue", border = NA)

      # Add a vertical line at the calculated z-value
      abline(v = z_value, col = "red", lty = 2)

      # Add text labels
      text(z_value, 0.1, labels = paste("z =", round(z_value, 2)), pos = 2, col = "red")
      text(z_value, 0.2, labels = paste("p =", round(PNI, 4)), pos = 2, col = "red")
    }
  })

  output$P2 <- renderPlot({
    if (input$outcome_nature == "good") {
      diff <- as.numeric(reactpT())-as.numeric(reactpC())
      ci <- c(Lower(),Upper())

      # plot
      plot(0, xlim = c(-100, 100), ylim = c(0, 0.5), type = "n",
           bty = "n", xlab = "Difference in proportions of patients with desired outcome in treatment and control group" , ylab = "",
           axes = FALSE)
      points(x = diff*100, y = 0.3,cex=0.9)
      #points(x = 9.01, y = 0.3, pch = 30)
      #points(x = -10.9, y = 0.3, pch = 15)
      segments(x0 = +ci[1]*100, x1 = ci[2]*100, y0 = 0.3)
      segments(x0 = +ci[1]*100,
               x1 = +ci[1]*100,
               y0 = 0.25,
               y1 = 0.35,
               lwd = 2,
               col = "darkblue")
      segments(x0 = +ci[2]*100,
               x1 = +ci[2]*100,
               y0 = 0.25,
               y1 = 0.35,
               lwd = 2,
               col = "darkblue")
      abline(v = 0, lty = 2)
      abline(v = -input$delta*100, lty = 1, col = "darkblue")
      axis(1, at = seq(-100,100,10), labels = seq(-1, 1, 0.1), col = "darkblue")
      #text(-30, 0.45, "Difference in proportions \n (1-sided 95% C.I.) \n ",diff ,"(",Lower()," to ",Upper(),")",cex=1)
      #text(25, 0.47, expression("NI Margin"),cex=1)
      text(-50, 0.4, "Control better",cex=1)
      text(50, 0.4, "Treatment better",cex=1)
    } else if (input$outcome_nature == "bad") {
      diff <- as.numeric(reactpT())-as.numeric(reactpC())
      ci <- c(Lower(),Upper())

      # plot
      plot(0, xlim = c(-100, 100), ylim = c(0, 0.5), type = "n",
           bty = "n", xlab = "Difference in proportions of patients with desired outcome in treatment and control group" , ylab = "",
           axes = FALSE)
      points(x = diff*100, y = 0.3,cex=0.9)
      #points(x = 9.01, y = 0.3, pch = 30)
      #points(x = -10.9, y = 0.3, pch = 15)
      segments(x0 = +ci[2]*100, x1 = ci[1]*100, y0 = 0.3)
      segments(x0 = +ci[2]*100,
               x1 = +ci[2]*100,
               y0 = 0.25,
               y1 = 0.35,
               lwd = 2,
               col = "darkblue")
      segments(x0 = +ci[1]*100,
               x1 = +ci[1]*100,
               y0 = 0.25,
               y1 = 0.35,
               lwd = 2,
               col = "darkblue")
      abline(v = 0, lty = 2)
      abline(v = input$delta*100, lty = 1, col = "darkblue")
      axis(1, at = seq(-100,100,10), labels = seq(-1, 1, 0.1), col = "darkblue")
      #text(-30, 0.45, "Difference in proportions \n (1-sided 95% C.I.) \n ",diff ,"(",Lower()," to ",Upper(),")",cex=1)
      #text(25, 0.47, expression("NI Margin"),cex=1)
      text(-50, 0.4, "Treatment better",cex=1)
      text(50, 0.4, "Control better",cex=1)

    }
  })



}

# Create Shiny object
shinyApp(ui = ui, server = server)


# Run the application
shinyApp(ui = ui, server = server)
