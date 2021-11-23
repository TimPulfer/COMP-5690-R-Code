library(shiny)
library(shiny)
library(shinyjs)
library(shinyhelper)
library(shinyWidgets)
library(deSolve)
library(ggplot2)
library(tidyverse)
maxPopulation = 100000
maxTimeSteps = 1000
fluidPage(navbarPage(
  title = span("Compartmental Models of Epidemiology", style = "color:#000000; font-weight:bold; font-size:15pt"),
  tabPanel(title = "Model",
           sidebarLayout(
             sidebarPanel((""),
                          div(
                            radioButtons(
                              inputId = "modelSelect",
                              label = ("Epidemic Model"),
                              choiceValues = list("SIR","SEIR"),
                              choiceNames = list("SIR","SEIR"),
                              inline = TRUE,
                              width = "1000px"
                            ),
                            
                            radioButtons(
                              inputId = "qValue",
                              label = ("Model Formulation"),
                              choiceValues = list(1, 0),
                              choiceNames = list("True-Mass Action", "Pseudo-Mass Action"),
                              inline = TRUE,
                              width = "1000px",
                              selected = "0"
                            ),
                            
                            checkboxInput(
                              "muValue",
                              label = "Vitial Dynamics",
                              value = FALSE
                            ),
                            
                            withMathJax(),
                            conditionalPanel(
                              condition = "input.muValue == '1'",
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "muBirth",
                                label = "Birth Rate  (\\( \\mu_B\\))",
                                min = 0,
                                max = 0.1,
                                step = 0.0001,
                                value = 0.00
                              ),
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "muDeath",
                                label = "Death Rate  (\\( \\mu_D\\))",
                                min = 0,
                                max = 0.1,
                                step = 0.0001,
                                value = 0.00
                              )
                            ),
                            
                            radioButtons(
                              inputId = "distributSelect",
                              label = ("Distribution Method"),
                              choiceValues = list("Deterministic", "Stochastic"),
                              choiceNames = list("Deterministic", "Stochastic"),
                              inline = TRUE,
                              width = "1000px"
                            ),
                            
                            conditionalPanel(
                              condition = "input.modelSelect == 'SIR'",
                              withMathJax(),
                              sliderInput(
                                inputId = "betaSIR",
                                label = "Transmission Rate (\\( \\beta\\))",
                                min = 0,
                                max = 0.5,
                                step = 0.00001,
                                value = 0.00003,
                                animate = animationOptions(interval = 100, pauseButton = NULL)
                              ),
                              sliderInput(
                                inputId = "gammaSIR",
                                label = "Removal Rate  (\\( \\gamma\\))",
                                min = 0,
                                max = 0.5,
                                step = 0.00001,
                                value = 0.12
                              ),
                              numericInput(
                                inputId = "populationSIR",
                                label = "Total Population (N)",
                                value = 20000,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "susceptibleSIR",
                                label = "Susceptible (S)",
                                value = 19999,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "infectedSIR",
                                label = "Infected (I)",
                                value = 1,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "recoveredSIR",
                                label = "Recovered (R)",
                                value = 0,
                                min = 0,
                                max = maxPopulation,
                                step = 1,
                              ),
                            ),
                            
                            conditionalPanel(
                              condition = "input.modelSelect == 'SIRD'",
                              # withMathJax(),
                              # sliderInput(
                              #     #TODO: Adjust min/ Max
                              #     inputId = "beta",
                              #     label = "Transmission Rate (\\( \\beta\\))",
                              #     min = 0,
                              #     max = 0.02,
                              #     step = 0.000001,
                              #     value = 0.0015,
                              #     animate = animationOptions(interval = 100, pauseButton = NULL)
                              # ),
                              # 
                              # sliderInput(
                              #     #TODO: Adjust min/ Max
                              #     inputId = "gamma",
                              #     label = "Removal Rate  (\\( \\gamma\\))",
                              #     min = 0,
                              #     max = 0.1,
                              #     step = 0.00001,
                              #     value = 0.005
                              # ),
                              # numericInput(
                              #     inputId = "population",
                              #     label = "Total Population (N)",
                              #     value = 500,
                              #     min = 1,
                              #     max = maxPopulation,
                              #     step = 1,
                              # ),
                              # numericInput(
                              #     inputId = "susceptible",
                              #     label = "Susceptible (S)",
                              #     value = 499,
                              #     min = 1,
                              #     max = maxPopulation,
                              #     step = 1,
                              # ),
                              # numericInput(
                              #     inputId = "infected",
                              #     label = "Infected (I)",
                              #     value = 1,
                              #     min = 1,
                              #     max = maxPopulation,
                              #     step = 1,
                              # ),
                              # numericInput(
                              #     inputId = "recovered",
                              #     label = "Recovered (R)",
                              #     value = 0,
                              #     min = 0,
                              #     max = maxPopulation,
                              #     step = 1,
                              # ),
                            ),
                            
                            conditionalPanel(
                              condition = "input.modelSelect == 'SEIR'",
                              withMathJax(),
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "beta",
                                label = "Transmission Rate (\\( \\beta\\))",
                                min = 0,
                                max = 1,
                                step = 0.01,
                                value = 0.5,
                                animate = animationOptions(interval = 10, pauseButton = NULL)
                              ),
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "gamma",
                                label = "Removal Rate  (\\( \\gamma\\))",
                                min = 0,
                                max = 3,
                                step = 0.00001,
                                value = 0.005
                              ),
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "sigma",
                                label = "Removal Rate  (\\( \\sigma\\))",
                                min = 0,
                                max = 0.1,
                                step = 0.00001,
                                value = 0.005
                              ),
                              numericInput(
                                inputId = "population",
                                label = "Total Population (N)",
                                value = 500,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "susceptible",
                                label = "Susceptible (S)",
                                value = 499,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "exposed",
                                label = "Exposed (E)",
                                value = 0,
                                min = 0,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "infected",
                                label = "Infected (I)",
                                value = 1,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "recovered",
                                label = "Recovered (R)",
                                value = 1,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                            ),
                            
                            conditionalPanel(
                              condition = "input.modelSelect == 'SEIRD'",
                              # withMathJax(),
                              # sliderInput(
                              #     #TODO: Adjust min/ Max
                              #     inputId = "beta",
                              #     label = "Transmission Rate (\\( \\beta\\))",
                              #     min = 0,
                              #     max = 0.02,
                              #     step = 0.000001,
                              #     value = 0.0015,
                              #     animate = animationOptions(interval = 100, pauseButton = NULL)
                              # ),
                              # 
                              # sliderInput(
                              #     #TODO: Adjust min/ Max
                              #     inputId = "gamma",
                              #     label = "Removal Rate  (\\( \\gamma\\))",
                              #     min = 0,
                              #     max = 0.1,
                              #     step = 0.00001,
                              #     value = 0.005
                              # ),
                              # numericInput(
                              #     inputId = "population",
                              #     label = "Total Population (N)",
                              #     value = 500,
                              #     min = 1,
                              #     max = maxPopulation,
                              #     step = 1,
                              # ),
                              # numericInput(
                              #     inputId = "susceptible",
                              #     label = "Susceptible (S)",
                              #     value = 499,
                              #     min = 1,
                              #     max = maxPopulation,
                              #     step = 1,
                              # ),
                              # numericInput(
                              #     inputId = "exposed",
                              #     label = "Exposed (E)",
                              #     value = 0,
                              #     min = 0,
                              #     max = maxPopulation,
                              #     step = 1,
                              # ),
                              # numericInput(
                              #     inputId = "infected",
                              #     label = "Infected (I)",
                              #     value = 1,
                              #     min = 1,
                              #     max = maxPopulation,
                              #     step = 1,
                              # ),
                              # numericInput(
                              #     inputId = "recovered",
                              #     label = "Recovered (R)",
                              #     value = 0,
                              #     min = 0,
                              #     max = maxPopulation,
                              #     step = 1,
                              # ),
                              # numericInput(
                              #     inputId = "dead",
                              #     label = "Dead (D)",
                              #     value = 0,
                              #     min = 0,
                              #     max = maxPopulation,
                              #     step = 1,
                              # ),
                            ),
                          ),
                          
                          numericInput(
                            inputId = "timesteps",
                            label = "Number of Timesteps (m)",
                            value = 50,
                            min = 1,
                            max = maxTimeSteps,
                            step = 1,
                          ),
             ),
             
             ##########################
             # End of Side Panel UI...#
             ##########################
             
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   title = "Plot",
                   downloadButton(outputId = "downloadPlot", label = "Save PNG/JPEG"),
                   
                   conditionalPanel(condition = "input.modelSelect == 'SIR'",
                                    plotOutput("plotSIR")),
                   conditionalPanel(condition = "input.modelSelect == 'SEIR'",
                                    plotOutput("plotSEIR"))
                 ),
                 
                 tabPanel(
                   title = "Phase Plane",
                   downloadButton(outputId = "downloadPlane", label = "Save PNG/JPEG"),
                   
                   conditionalPanel(condition = "input.modelSelect == 'SIR'",
                                    plotOutput("SIRPhasePlane")),
                   conditionalPanel(condition = "input.modelSelect == 'SEIR'",
                                    plotOutput("SEIRPhasePlane"))
                   
                 ),
                 
                 tabPanel(
                   title = "Simulation Summary",
                   conditionalPanel(
                     condition = "input.modelSelect == 'SIR'",
                     tags$iframe(style = "height:600px; width:100%;scrolling=yes", src =
                                   "SIR.pdf")
                   ),
                   conditionalPanel(
                     condition = "input.modelSelect == 'SIRS'",
                     tags$iframe(style = "height:600px; width:100%;scrolling=yes", src =
                                   "SIRS.pdf")
                   ),
                   conditionalPanel(
                     condition = "input.modelSelect == 'SEIR'",
                     tags$iframe(style = "height:600px; width:100%;scrolling=yes", src =
                                   "SEIR.pdf")
                   ),
                   conditionalPanel(
                     condition = "input.modelSelect == 'SEIRD'",
                     tags$iframe(style = "height:600px; width:100%;scrolling=yes", src =
                                   "SEIRD.pdf")
                   ),
                   conditionalPanel(
                     condition = "input.modelSelect == 'SVEIRD'",
                     tags$iframe(style = "height:600px; width:100%;scrolling=yes", src =
                                   "SVEIRD.pdf")
                   )
                 ),
                 
                 tabPanel(
                   title = "Mathematical Model",
                   
                   conditionalPanel(
                     condition = "input.modelSelect == 'SIR'",
                     conditionalPanel(
                       condition = "input.muValue == '0'",
                       withMathJax(
                         helpText("Susceptible $$\\frac{dS}{dt} = - \\beta \\frac{ S I}{N^q}$$"),
                         helpText(
                           "Infectious $$\\frac{dI}{dt} = \\frac{\\beta S I}{N^q} - \\gamma I $$"
                         ),
                         helpText("Recovered $$\\frac{dR}{dt} = \\gamma I $$"),
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$")
                       )
                     ),
                     conditionalPanel(
                       condition = "input.muValue == '1'",
                       withMathJax(
                         helpText(
                           "Susceptible $$\\frac{dS}{dt} =\\mu_B N - \\mu_D S - \\beta \\frac{ S I}{N^q}$$"
                         ),
                         helpText(
                           "Infectious $$\\frac{dI}{dt} = \\frac{\\beta S I}{N^q} - \\gamma I - \\mu_D I$$"
                         ),
                         helpText("Recovered $$\\frac{dR}{dt} = \\gamma I - \\mu_D R $$"),
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$")
                       )
                     )
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.modelSelect == 'SEIR'",
                     conditionalPanel(
                       condition = "input.muValue == '0'",
                       withMathJax(
                         helpText("Susceptible $$\\frac{dS}{dt} = - \\beta \\frac{ S I}{N^q}$$"),
                         helpText("Exposed $$\\frac{dE}{dt} = \\beta \\frac{ S I}{N^q} - \\gamma I $$"),
                         helpText("Infectious $$\\frac{dI}{dt} = \\gamma I - \\sigma R $$"),
                         helpText("Recovered $$\\frac{dR}{dt} = \\sigma R $$"),
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$")
                       )
                     ),
                     conditionalPanel(
                       condition = "input.muValue == '1'",
                       withMathJax(
                         helpText(
                           "Susceptible $$\\frac{dS}{dt} =\\mu_B N - \\mu_D S - \\beta \\frac{ S I}{N^q}$$"
                         ),
                         helpText("Exposed $$\\frac{dE}{dt} = \\beta \\frac{ S I}{N^q} - \\gamma I - \\mu_D E$$"),
                         helpText("Infectious $$\\frac{dI}{dt} = \\gamma I - \\sigma R - \\mu_D I $$"),
                         helpText("Recovered $$\\frac{dR}{dt} = \\sigma R - \\mu_D R $$"),
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$")
                       )
                     )
                   ),
                 ),
                 
                 tabPanel(
                   title = "Output Summary",
                   
                   conditionalPanel(condition = "input.modelSelect == 'SIR'",
                                    tableOutput("tableSIR")),
                   conditionalPanel(condition = "input.modelSelect == 'SEIR'",
                                    tableOutput("tableSEIR"))
                 )
               )
             )
           )),
  tabPanel(
    title = "Authors",
    
    h3("Development Team", style = "font-weight:bold"),
    br(),
    p(span("Timothy Pulfer", style = "font-weight:bold")),
    p("Lead Developer"),
    p("Mount Royal University,"),
    p("Calgary, AB, Canada"),
    br(),
    
    p(span("Ashok Krishnamurthy, PhD", style = "font-weight:bold")),
    p("Project Supervisor,"),
    p("Mount Royal University"),
    br(),
    
    p(
      "This app was written as Timothys senior undergraduate project for the COMP 5690 course."
    )
  )
))