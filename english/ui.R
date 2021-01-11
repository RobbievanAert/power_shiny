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
  titlePanel("Calculating the Power of a Statistical Test"),
  
  ### Main panel for displaying outputs
  mainPanel(
    
    tags$em("Developed by: Tilburg University, MTO department, Robbie C.M. 
             van Aert, Leone Verweij"),
    
    tags$br(),
    tags$br(),
    
    tags$em("This application can be used by students to practise with calculating 
            the power of a test. Assignments and their solutions are automatically 
            generated."),
    
    tags$br(),
    tags$br(),
    
    helpText("A researcher is planning to carry out a \\(\\textit{Z}\\)-test and 
             wants to know the power of this hypothesis test. The following 
             information is known:"),
    
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
    
    actionButton("new", "Next Question"),
    
    actionButton("sol", "Solution"),
    
    tags$br(),
    tags$br(),
    
    uiOutput("sol1"), # Solution step 1
    
    plotOutput("plot1", width = "70%", height = "220px"), # Plot step 1
    
    uiOutput("sol2"), # Solution step 2
    
    plotOutput("plot2", width = "70%", height = "220px"), # Plot step 2
    
    uiOutput("sol3"), # Solution step 3
    
    plotOutput("plot3", width = "70%", height = "220px"), # Plot step 3
    
    uiOutput("sol4") # Solution step 4
    
  )
)