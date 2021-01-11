###########################################################################
##### THIS APPLICATION IS FOR EDUCATIONAL PURPOSES AND CAN BE USED TO #####
##### PRACTICE COMPUTING THE STATISTICAL POWER USING A Z-TEST         #####
##### Author: Robbie C.M. van Aert                                    #####
##### License: MIT License (Expat)                                    #####
###########################################################################

server <- function(input, output) 
{
  
  ### Function to generate data
  gen_dat <- function()
  {
    alpha <- sample(c(0.01, 0.05, 0.1), size = 1) # Sample alpha level
    direc <- sample(c("\\neq", "<", ">"), size = 1) # Sample direction of the test
    
    ### Compute zcv for a given direction of the test
    if (direc == "<")
    {
      zcv <- round(qnorm(alpha), 3)
    } else if (direc == ">")
    {
      zcv <- round(qnorm(alpha, lower.tail = FALSE), 3)
    } else if (direc == "\\neq")
    {
      ### Sample zcv such that it is either positive or negative
      zcv <- round(sample(c(qnorm(alpha/2), qnorm(alpha/2, lower.tail = FALSE)), 
                          size = 1), 3)
    }
    
    ### z-values that can easily be looked up in tables
    zh1s <- c(seq(-3.8, -3.5, 0.1), seq(-3.45, -3.25, 0.05), seq(-3.24, -0.2, 0.01),
              seq(3.5, 3.8, 0.1), seq(3.25, 3.45, 0.05), seq(0.2, 3.24, 0.01))
    
    ### Generate data
    stay <- TRUE
    
    while(stay)
    {
      zh1 <- sample(zh1s, size = 1) # z-value under H1
      N <- round(runif(1, min = 10, max = 100)) # Sample size 
      sigma <- round(runif(1, min = 1, max = 20), 1) # Standard deviation in population
      se <- round(sigma/sqrt(N), 3) # Standard error
      mu_h0 <- round(runif(1, min = 0.1, max = 100), 1)
      xcv <- mu_h0+zcv*se # Critical value on the scale of means
      mu_h1 <- round(xcv-zh1*se, 1)
      
      ### Make sure the direction of the test and mu_h0 and mu_h1 are in line
      if (direc == ">" | direc == "\\neq" & zcv > 0)
      {
        stay <- ifelse(mu_h1 > mu_h0, FALSE, TRUE)
      } else if (direc == "<" | direc == "\\neq" & zcv < 0)
      {
        stay <- ifelse(mu_h1 < mu_h0, FALSE, TRUE)
      }
      
      ### Compute z-value under H1
      if (stay == FALSE)
      {
        zh1 <- round((xcv-mu_h1)/se, 2)
      }
      
    }
    
    ### Store some objects that will be just later on for generating the excercise
    if (direc == "\\neq")
    {
      text_direc <- "tweezijdige"
      tail <- "Proportion in Two Tails Combined"
      nota4 <- ifelse(mu_h1 > mu_h0, "\\(P(Z \\geq Z_{H_1} | H_1)\\)", 
                      "\\(P(Z \\leq Z_{H_1} | H_1)\\)")
      nota4_eq <- ifelse(mu_h1 > mu_h0, "P(Z \\geq Z_{H_1} | H_1)", 
                         "P(Z \\leq Z_{H_1} | H_1)")
      two_direc <- ifelse(mu_h1 > mu_h0, "rechterstaart", "linkerstaart")
      
      power <- ifelse(mu_h1 > mu_h0, pnorm(zh1, lower.tail = FALSE), pnorm(zh1))
      
    } else if (direc == "<")
    {
      text_direc <- "linkseenzijdige"
      tail <- "Proportion in One Tail"
      nota4 <- "\\(P(Z \\leq Z_{H_1} | H_1)\\)"
      nota4_eq <- "P(Z \\leq Z_{H_1} | H_1)"
      two_direc <- "one"
      
      power <- pnorm(zh1)
    } else if (direc == ">")
    {
      text_direc <- "rechtseenzijdige"
      tail <- "Proportion in One Tail"
      nota4 <- "\\(P(Z \\geq Z_{H_1} | H_1)\\)"
      nota4_eq <- "P(Z \\geq Z_{H_1} | H_1)"
      two_direc <- "one"
      
      power <- pnorm(zh1, lower.tail = FALSE)
    }
    
    power <- round(power, 4) # Round power to match the table
    
    h0 <- paste0("\\(H_0: \\mu = ", mu_h0, "\\)")
    h1 <- paste0("\\(H_1: \\mu ", direc, " ", mu_h0, "\\)")
    
    ### Create a list with bullet points for the conditions
    output$ex <- renderUI({
      list(structure(list(name = "li", attribs = structure(list(), .Names = character(0)),
                          children = list(c(item = withMathJax(
                            paste0("Hypotheses ", h0, " en ", h1))))),
                     class = "shiny.tag"),
           
           structure(list(name = "li", attribs = structure(list(), .Names = character(0)),
                          children = list(c(item = withMathJax(
                            paste0("Verwacht steekproefgemiddelde \\(\\mu_{H_1}\\) = ",
                                   mu_h1))))),
                     class = "shiny.tag"),
           
           structure(list(name = "li", attribs = structure(list(), .Names = character(0)),
                          children = list(c(item = withMathJax(
                            paste0("Standaarddeviatie in de populatie \\(\\sigma\\) = ",
                                   sigma))))),
                     class = "shiny.tag"),
           
           structure(list(name = "li", attribs = structure(list(), .Names = character(0)),
                          children = list(c(item = withMathJax(
                            paste0("Steekproefgrootte \\(\\textit{N}\\) = ", N))))),
                     class = "shiny.tag"),
           
           structure(list(name = "li", attribs = structure(list(), .Names = character(0)),
                          children = list(c(item = "Significantieniveau van hypothesetoets "),
                                          withMathJax("\\(\\alpha\\) = "), c(num = alpha))),
                     class = "shiny.tag"))
      
    })
    
    ### The question depends on whether a one or two-tailed test was used
    if (direc != "\\neq")
    {
      output$ques <- renderUI({
        
        helpText("Wat is het onderscheidend vermogen/power van deze hypothesetoets?")
        
      })
    } else if (direc == "\\neq")
    {
      output$ques <- renderUI({
        
        helpText("Wat is het onderscheidend vermogen/power van deze hypothesetoets 
                 voor het vinden van een effect in de juiste richting?")
        
      })
    }
    
    return(data.frame(mu_h0, h0, alpha, sigma, mu_h1, N, direc, h1, text_direc, 
                      tail, zcv, nota4, nota4_eq, xcv, zh1, power, two_direc))
    
  }
  
  ### Function for getting the solution
  get_sol <- function()
  {
    
    observeEvent(input$sol, {
      
      ### Step 1: Extra information about the direction of the test is added for 
      # two-tailed tests
      if (direc != "\\neq")
      {
        output$sol1 <- renderUI({
          withMathJax(
            helpText(paste0("\\(\\textbf{Stap 1: Bepaal de $Z_{cv}$ onder $H_0$ (bij
                        gegeven $\\alpha$ en richting van de toets)}\\)")),
            helpText(paste0("Er is een ", text_direc, " toets uitgevoerd met
                        \\(\\alpha\\) = ", alpha, ", dus \\(\\textit{$Z_{cv}$}\\)
                         = ", zcv, ". Deze \\(\\textit{$Z_{cv}$}\\)
                        kunnen we vinden in Tabel B.2 als we kijken bij het
                        \\(\\textit{$\\infty$}\\)-teken in combinatie met '", tail,
                            "' en \\(\\alpha\\) = ", alpha, ". Hieronder wordt dit
                        weergegeven in een figuur o.b.v. \\(\\textit{Z}\\)-scores 
                        waarbij het oranje gebied het verwerpingsgebied is.")))
          
        })
        
      } else if (direc == "\\neq")
      {
        output$sol1 <- renderUI({
          withMathJax(
            helpText(paste0("\\(\\textbf{Stap 1: Bepaal de $Z_{cv}$ onder $H_0$ (bij
                        gegeven $\\alpha$ en richting van de toets)}\\)")),
            helpText(paste0("Er is een ", text_direc, " toets uitgevoerd met
                        \\(\\alpha\\) = ", alpha, ", dus \\(\\textit{$Z_{cv}$}\\)
                         = ", zcv, ". Deze \\(\\textit{$Z_{cv}$}\\)
                        kunnen we vinden in Tabel B.2 als we kijken bij het
                        \\(\\textit{$\\infty$}\\)-teken in combinatie met '", tail,
                            "' en \\(\\alpha\\) = ", alpha, ". Hieronder wordt dit
                        weergegeven in een figuur o.b.v. \\(\\textit{Z}\\)-scores 
                        waarbij het oranje gebied het verwerpingsgebied is. Dit 
                            verwerpingsgebied ligt in de ", two_direc, " van de 
                            verdeling, want dit is de juiste richting volgens de 
                            \\(\\textit{$\\mu_{H_1}$}\\).")))
          
        })
      }
      
      ### Generate plot step 1
      output$plot1 <- renderPlot({
        par(mar = c(5, 5, 2, 2), xaxs = "i", yaxs = "i")
        
        x <- seq(from = -3, to = 3, length.out = 100)
        plot(x, dnorm(x), type = "l", yaxt = "n", bty = "n", ylab = "",
             cex.axis = 1.5, cex.lab  = 1.5, xlab = "", xaxt = "n")
        
        if (text_direc == "linkseenzijdige" | two_direc == "linkerstaart")
        {
          x <- seq(-5, -abs(zcv), length = 100)
          dvals <- dnorm(x)
          polygon(c(x, rev(x)), c(rep(0, 100), rev(dvals)), col = "orange")
          
          mtext(expression(italic(Z)[cv]), side = 1, at = -abs(zcv),
                line = 1, cex = 1.5)
        } else if (text_direc == "rechtseenzijdige" | two_direc == "rechterstaart")
        {
          x <- seq(abs(zcv), 5, length = 100)
          dvals <- dnorm(x)
          polygon(c(x, rev(x)), c(rep(0,100), rev(dvals)), col = "orange")
          
          mtext(expression(italic(Z)[cv]), side = 1, at = abs(zcv), line = 1, cex = 1.5)
        }
        
        abline(v = 0, lty = 2)
        mtext("0", side = 1, at = 0, line = 1, cex = 1.5)
        
      })
      
      
      ### Step 2
      output$sol2 <- renderUI({
        withMathJax(
          helpText(paste0("\\(\\textbf{Stap 2: Bepaal het steekproefgemiddelde
        $\\bar{X}_{cv}$ dat bij $Z_{cv}$ hoort onder $H_0$}\\)")),
          helpText(paste0("Standaardfout van het gemiddelde is: $$\\sigma_\\bar{X} =
        \\frac{\\sigma}{\\sqrt{N}} = \\frac{", sigma, "}{\\sqrt{", N, "}} = ",
                          round(sigma/sqrt(N), 3), "$$")),
          helpText(paste0("Bepaal \\(\\textit{$\\bar{X}_{cv}$}\\): $$\\bar{X}_{cv} =
        \\mu_{H_0} + Z_{cv} \\times \\sigma_\\bar{X} = ", mu_h0, " + ", zcv, "\\times",
                          round(sigma/sqrt(N), 3), " = ", round(xcv, 3), "$$")),
          helpText("Hieronder wordt dit weergegeven in een figuur o.b.v. de 
                 ongestandaardiseerde scores waarbij het oranje gebied het
                 verwerpingsgebied is.")
          
        )
        
      })
      
      
      ### Generate plot step 2
      output$plot2 <- renderPlot({
        par(mar = c(5, 5, 2, 2), xaxs = "i", yaxs = "i")
        
        xcv_l <- mu_h0 - abs(zcv) * sigma/sqrt(N)
        xcv_r <- mu_h0 + abs(zcv) * sigma/sqrt(N)
        
        lb <- mu_h0 - 5 * sigma/sqrt(N)
        ub <- mu_h0 + 5 * sigma/sqrt(N)
        
        x <- seq(from = mu_h0-3*sigma/sqrt(N), to = mu_h0+3*sigma/sqrt(N), length.out = 100)
        plot(x, dnorm(x, mean = mu_h0, sd = sigma/sqrt(N)), type = "l", yaxt = "n",
             bty = "n", ylab = "", cex.axis = 1.5, cex.lab  = 1.5, xlab = "", xaxt = "n")
        
        if (text_direc == "linkseenzijdige" | two_direc == "linkerstaart")
        {
          x <- seq(lb, xcv_l, length = 100)
          dvals <- dnorm(x, mean = mu_h0, sd = sigma/sqrt(N))
          polygon(c(x, rev(x)), c(rep(0, 100), rev(dvals)), col = "orange")
          
          mtext(expression(italic(bar(X))[cv]), side = 1, at = xcv_l, line = 1, 
                cex = 1.5)
        } else if (text_direc == "rechtseenzijdige" | two_direc == "rechterstaart")
        {
          x <- seq(xcv_r, ub, length = 100)
          dvals <- dnorm(x, mean = mu_h0, sd = sigma/sqrt(N))
          polygon(c(x, rev(x)), c(rep(0,100), rev(dvals)) ,col = "orange")
          
          mtext(expression(italic(bar(X))[cv]), side = 1, at = xcv_r, line = 1,
                cex = 1.5)
        }
        
        abline(v = mu_h0, lty = 2)
        mtext(mu_h0, side = 1, at = mu_h0, line = 1, cex = 1.5)
        
      })
      
      
      ### Step 3
      output$sol3 <- renderUI({
        withMathJax(
          helpText(paste0("\\(\\textbf{Stap 3: Reken de kritieke grenswaarde 
                        $\\bar{X}_{cv}$ om naar de $Z_{H_1}$-waarde onder de $H_1$}\\)")),
          helpText(paste0("$$Z_{H_1} = \\frac{\\bar{X}_{cv}-\\mu_{H_1}}{\\sigma_\\bar{X}} 
                        = \\frac{", round(xcv, 3), "-", mu_h1, "}{", 
                          round(sigma/sqrt(N), 3), "} = 
                        ", zh1, "$$")),
          helpText("Hieronder wordt dit weergegeven in een figuur o.b.v. 
        \\(\\textit{Z}\\)-scores waarbij het oranje gebied het verwerpingsgebied is.")
        )
      })
      
      
      ### Generate plot step 3
      output$plot3 <- renderPlot({
        par(mar = c(5, 5, 2, 2), xaxs = "i", yaxs = "i")
        
        x <- seq(from = -4, to = 4, length.out = 100)
        plot(x, dnorm(x), type = "l", yaxt = "n", bty = "n", ylab = "",
             cex.axis = 1.5, cex.lab  = 1.5, xlab = "", xaxt = "n")
        
        if (text_direc == "linkseenzijdige" | two_direc == "linkerstaart")
        {
          x <- seq(-5, zh1, length = 100)
          dvals <- dnorm(x)
          polygon(c(x, rev(x)), c(rep(0, 100), rev(dvals)), col = "orange")
        } else if (text_direc == "rechtseenzijdige" | two_direc == "rechterstaart")
        {
          x <- seq(zh1, 5, length = 100)
          dvals <- dnorm(x)
          polygon(c(x, rev(x)), c(rep(0,100), rev(dvals)) ,col = "orange")
        }
        
        abline(v = 0, lty = 2)
        mtext("0", side = 1, at = 0, line = 1, cex = 1.5)
        
        mtext(zh1, side = 1, at = zh1, line = 1, cex = 1.5)
        
      })
      
      
      ### Step 4
      output$sol4 <- renderUI({
        withMathJax(
          helpText(paste0("\\(\\textbf{Stap 4: Bepaal het onderscheidend vermogen/power}\\)")),
          helpText(paste0("Het onderscheidend vermogen/power is gelijk aan de 
                        oppervlakte van het oranje gebied in de verdeling in stap 3.
                        Dat is ", nota4, " en kan gevonden worden met behulp van 
                        Tabel B.1.")),
          helpText(paste0("$$", nota4_eq, " = ", power, "$$."))
        )
      })
      
    })
    
  }
  
  ### Generate new data and attach these objects to global environment
  attach(gen_dat(), warn.conflicts = FALSE)
  
  ### Get the solution to the exercise
  get_sol()
  
  ### Generate new data if the button "new" is clicked
  observeEvent(input$new, {
    attach(gen_dat(), warn.conflicts = FALSE)
    
    ### Omit the solution to the previous exercise
    output$sol1 <- output$sol2 <- output$sol3 <- output$sol4 <- NULL
    output$plot1 <- output$plot2 <- output$plot3 <- NULL
  })
  
}