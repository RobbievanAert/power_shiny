###########################################################################
##### THIS APPLICATION IS FOR EDUCATIONAL PURPOSES AND CAN BE USED TO #####
##### PRACTICE COMPUTING THE STATISTICAL POWER USING A Z-TEST         #####
##### Author: Robbie C.M. van Aert                                    #####
##### License: MIT License (Expat)
###########################################################################

################
### PACKAGES ###
################

library(shiny)

################################################################################

ui <- fluidPage(
  
  ### App title
  titlePanel("Onderscheidend vermogen/power berekenen"),
  
  ### Main panel for displaying outputs
  mainPanel(
    
    tags$em("Ontwikkeld door: Tilburg University, MTO departement, Robbie C.M. 
             van Aert"),
    
    tags$br(),
    tags$br(),
    
    tags$em("Deze applicatie kan gebruikt worden door studenten om te oefenen 
    met het berekenen van het onderscheidend vermogen/power. Opgaven en 
             bijbehorende oplossingen worden automatisch gegenereerd."),
    
    tags$br(),
    tags$br(),
    
    withMathJax(helpText("Een onderzoeker is van plan een \\(\\textit{Z}\\)-toets uit 
      te voeren en wil weten wat het onderscheidend vermogen/power is van deze 
      hypothesetoets. Onderstaande gegevens zijn bekend:")),
    
    uiOutput("ex"), # List of conditions
    
    uiOutput("ques"), # Question depending on one or two-tailed test
    
    ### Layout of buttons
    tags$head(
      tags$style(HTML('#new{background-color:#555555}')),
      tags$style(HTML('#new{color:white}')),
      tags$style(HTML('#sol{background-color:#555555}')),
      tags$style(HTML('#sol{color:white}'))
    ),
    
    tags$br(),
    
    actionButton("new", "Volgende vraag"),
    
    actionButton("sol", "Oplossing"),
    
    tags$br(),
    tags$br(),
    
    ### Conditional panel where solutions are not shown at launch of the application
    # (input.sol > 0)
    conditionalPanel("input.sol > 0",
                     uiOutput("sol1"), # Solution step 1

                     plotOutput("plot1", width = "70%", height = "220px"), # Plot step 1

                     uiOutput("sol2"), # Solution step 2

                     plotOutput("plot2", width = "70%", height = "220px"), # Plot step 2

                     uiOutput("sol3"), # Solution step 3

                     plotOutput("plot3", width = "70%", height = "220px"), # Plot step 3

                     uiOutput("sol4") # Solution step 4
    ),

  )
)