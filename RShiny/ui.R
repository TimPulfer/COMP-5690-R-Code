library(shiny)
library(shinyjs)
library(shinyhelper)
library(shinyWidgets)
library(deSolve)
library(ggplot2)
library(tidyverse)

maxPopulation = 900000000

fluidPage(
  
  div(
    titlePanel("Compartmental Models of Epidemiology")
  ),
  navbarPage(
  title = span(),
  tabPanel(title = "Model",
           sidebarLayout(
             sidebarPanel((""),
                          div(
                            radioButtons(
                              inputId = "modelSelect",
                              label = ("Epidemic Model"),
                              choiceValues = list("SIR", "SIRD","SEIR","SEIRD", "SIR-Stochastic"),
                              choiceNames = list("SIR", "SIRD", "SEIR", "SEIRD", "SIR-Stochastic"),
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
                              label = "Vital Dynamics",
                              value = FALSE
                            ),
                            
                            withMathJax(),
                            conditionalPanel(
                              condition = "input.muValue == '1'",
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "muBirth",
                                label = "Birth Rate (\\( \\mu_B\\))",
                                min = 0,
                                max = 0.1,
                                step = 0.0001,
                                value = 0.00
                              ),
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "muDeath",
                                label = "Death Rate due to Natural Causes (\\( \\mu_D\\))",
                                min = 0,
                                max = 0.1,
                                step = 0.0001,
                                value = 0.00
                              )
                            ),
                            
                            conditionalPanel(
                              condition = "input.modelSelect == 'SIR-Stochastic'",
                              withMathJax(),
                              sliderInput(
                                inputId = "stochasticSIR",
                                label = "Number of Simulations",
                                min = 1,
                                max = 100,
                                step = 1,
                                value = 50
                              ),
                              sliderInput(
                                inputId = "betaSIR_Stoc",
                                label = "Transmission Rate (\\( \\beta\\))",
                                min = 0,
                                max = 0.5,
                                step = 0.00001,
                                value = 0.00003,
                              ),
                              sliderInput(
                                inputId = "gammaSIR_Stoc",
                                label = "Removal Rate  (\\( \\gamma\\))",
                                min = 0,
                                max = 0.5,
                                step = 0.00001,
                                value = 0.12
                              ),
                              numericInput(
                                inputId = "populationSIR_Stoc",
                                label = "Total Population (N)",
                                value = 1000,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "susceptibleSIR_Stoc",
                                label = "Susceptible (S)",
                                value = 990,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "infectedSIR_Stoc",
                                label = "Infected (I)",
                                value = 10,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "recoveredSIR_Stoc",
                                label = "Recovered (R)",
                                value = 0,
                                min = 0,
                                max = maxPopulation,
                                step = 1,
                              )
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
                              withMathJax(),
                              sliderInput(
                                inputId = "betaSIRD",
                                label = "Transmission Rate (\\( \\beta\\))",
                                min = 0,
                                max = 0.5,
                                step = 0.00001,
                                value = 0.00003,
                              ),
                              sliderInput(
                                inputId = "gammaSIRD",
                                label = "Removal Rate  (\\( \\gamma\\))",
                                min = 0,
                                max = 0.5,
                                step = 0.00001,
                                value = 0.015
                              ),
                              sliderInput(
                                inputId = "deltaSIRD",
                                label = "Death Rate (\\( \\delta\\))",
                                min = 0,
                                max = 0.5,
                                step = 0.00001,
                                value = 0.08
                              ),
                              numericInput(
                                inputId = "populationSIRD",
                                label = "Total Population (N)",
                                value = 20000,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "susceptibleSIRD",
                                label = "Susceptible (S)",
                                value = 19999,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "infectedSIRD",
                                label = "Infected (I)",
                                value = 1,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "recoveredSIRD",
                                label = "Recovered (R)",
                                value = 0,
                                min = 0,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "deadSIRD",
                                label = "Dead (R)",
                                value = 0,
                                min = 0,
                                max = maxPopulation,
                                step = 1,
                              ),
                            ),
                            
                            conditionalPanel(
                              condition = "input.modelSelect == 'SEIR'",
                              withMathJax(),
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "beta",
                                label = "Exposure Rate (\\( \\beta\\))",
                                min = 0,
                                max = 1,
                                step = 0.01,
                                value = 0.16,
                              ),
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "gamma",
                                label = "Infectiousness (\\( \\gamma\\))",
                                min = 0,
                                max = 3,
                                step = 0.00001,
                                value = 0.045
                              ),
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "sigma",
                                label = "Removal Rate  (\\( \\sigma\\))",
                                min = 0,
                                max = 0.5,
                                step = 0.00001,
                                value = 0.11
                              ),
                              numericInput(
                                inputId = "population",
                                label = "Total Population (N)",
                                value = 53,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "susceptible",
                                label = "Susceptible (S)",
                                value = 50,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "exposed",
                                label = "Exposed (E)",
                                value = 3,
                                min = 0,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "infected",
                                label = "Infected (I)",
                                value = 0,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "recovered",
                                label = "Recovered (R)",
                                value = 0,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                            ),
                            
                            conditionalPanel(
                              condition = "input.modelSelect == 'SEIRD'",
                              withMathJax(),
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "betaSEIRD",
                                label = "Exposure Rate (\\( \\beta\\))",
                                min = 0,
                                max = 1,
                                step = 0.01,
                                value = 0.16,
                              ),
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "gammaSEIRD",
                                label = "Infectiousness (\\( \\gamma\\))",
                                min = 0,
                                max = 3,
                                step = 0.00001,
                                value = 0.045
                              ),
                              sliderInput(
                                #TODO: Adjust min/ Max
                                inputId = "sigmaSEIRD",
                                label = "Removal Rate  (\\( \\sigma\\))",
                                min = 0,
                                max = 0.5,
                                step = 0.00001,
                                value = 0.11
                              ),
                              sliderInput(
                                inputId = "deltaSEIRD",
                                label = "Death Rate (\\( \\delta\\))",
                                min = 0,
                                max = 0.5,
                                step = 0.00001,
                                value = 0.08
                              ),
                              numericInput(
                                inputId = "populationSEIRD",
                                label = "Total Population (N)",
                                value = 53,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "susceptibleSEIRD",
                                label = "Susceptible (S)",
                                value = 50,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "exposedSEIRD",
                                label = "Exposed (E)",
                                value = 3,
                                min = 0,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "infectedSEIRD",
                                label = "Infected (I)",
                                value = 0,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "recoveredSEIRD",
                                label = "Recovered (R)",
                                value = 0,
                                min = 1,
                                max = maxPopulation,
                                step = 1,
                              ),
                              numericInput(
                                inputId = "deadSEIRD",
                                label = "Dead (R)",
                                value = 0,
                                min = 0,
                                max = maxPopulation,
                                step = 1,
                              ),
                            ),
                          ),
                          
                          numericInput(
                            inputId = "timesteps",
                            label = "Number of Timesteps (m)",
                            value = 50,
                            min = 1,
                            max = 1000,
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
                   #downloadButton(outputId = "downloadPlot", label = "Save PNG/JPEG"),
                   conditionalPanel(condition = "input.modelSelect == 'SIR'",
                                    plotOutput("plotSIR"),
                                    img(src='SIR.jpg', height = '100px')),
                   conditionalPanel(condition = "input.modelSelect == 'SIR-Stochastic'",
                                    plotOutput("plotSIR_Stoc")),
                   conditionalPanel(condition = "input.modelSelect == 'SIRD'",
                                    plotOutput("plotSIRD"),
                                    img(src='SIRD.jpg', height = '200px')),
                   conditionalPanel(condition = "input.modelSelect == 'SEIR'",
                                    plotOutput("plotSEIR"),
                                    img(src='SEIR.jpg', height = '100px')),
                   conditionalPanel(condition = "input.modelSelect == 'SEIRD'",
                                    plotOutput("plotSEIRD"),
                                    img(src='SEIRD.jpg', height = '200px'))
                 ),
                 
                 tabPanel(
                   title = "Phase Plane",
                   #downloadButton(outputId = "downloadPlane", label = "Save PNG/JPEG"),
                   conditionalPanel(condition = "input.modelSelect == 'SIR'",
                                    plotOutput("SIRPhasePlane")),
                   conditionalPanel(condition = "input.modelSelect == 'SIRD'",
                                    plotOutput("SIRDPhasePlane")),
                   conditionalPanel(condition = "input.modelSelect == 'SEIR'",
                                    plotOutput("SEIRPhasePlane")),
                   conditionalPanel(condition = "input.modelSelect == 'SEIRD'",
                                    plotOutput("SEIRDPhasePlane"))
                 ),
                 
                 tabPanel(
                   title = "Output Summary",
                   conditionalPanel(condition = "input.modelSelect == 'SIR'",
                                    tableOutput("tableSIR")),
                   conditionalPanel(condition = "input.modelSelect == 'SIRD'",
                                    tableOutput("tableSIRD")),
                   conditionalPanel(condition = "input.modelSelect == 'SEIR'",
                                    tableOutput("tableSEIR")),
                   conditionalPanel(condition = "input.modelSelect == 'SEIRD'",
                                    tableOutput("tableSEIRD"))
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
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma} S(0)^q$$"),
                         helpText("q-Value", br(), "$$ 1, frequency-dependent $$",br(),"$$ 0, density-dependent $$")
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
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma} S(0)^q$$"),
                         helpText("q-Value", br(), "$$ 1, frequency-dependent $$",br(),"$$ 0, density-dependent $$")
                       )
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "input.modelSelect == 'SIRD'",
                     conditionalPanel(
                       condition = "input.muValue == '0'",
                       withMathJax(
                         helpText("Susceptible $$\\frac{dS}{dt} = - \\beta \\frac{ S I}{N^q}$$"),
                         helpText(
                           "Infectious $$\\frac{dI}{dt} = \\frac{\\beta S I}{N^q} - \\gamma I - \\delta I $$"
                         ),
                         helpText("Recovered $$\\frac{dR}{dt} = \\gamma I $$"),
                         helpText("Dead $$\\frac{dD}{dt} = \\delta I $$"),
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma + \\delta} S(0)^q$$"),
                         helpText("q-Value", br(), "$$ 1, frequency-dependent $$",br(),"$$ 0, density-dependent $$")
                       )
                     ),
                     conditionalPanel(
                       condition = "input.muValue == '1'",
                       withMathJax(
                         helpText(
                           "Susceptible $$\\frac{dS}{dt} =\\mu_B N - \\mu_D S - \\beta \\frac{ S I}{N^q}$$"
                         ),
                         helpText(
                           "Infectious $$\\frac{dI}{dt} = \\frac{\\beta S I}{N^q} - \\gamma I - \\delta I - \\mu_D I$$"
                         ),
                         helpText("Recovered $$\\frac{dR}{dt} = \\gamma I - \\mu_D R $$"),
                         helpText("Dead $$\\frac{dD}{dt} = \\delta I $$"),
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma + \\delta} S(0)^q$$"),
                         helpText("q-Value", br(), "$$ 1, frequency-dependent $$",br(),"$$ 0, density-dependent $$")
                       )
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "input.modelSelect == 'SEIR'",
                     conditionalPanel(
                       condition = "input.muValue == '0'",
                       withMathJax(
                         helpText("Susceptible $$\\frac{dS}{dt} = - \\beta \\frac{ S I}{N^q}$$"),
                         helpText("Exposed $$\\frac{dE}{dt} = \\beta \\frac{ S I}{N^q} - \\gamma E $$"),
                         helpText("Infectious $$\\frac{dI}{dt} = \\gamma E - \\sigma I $$"),
                         helpText("Recovered $$\\frac{dR}{dt} = \\sigma I $$"),
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$"),
                         helpText("q-Value", br(), "$$ 1, frequency-dependent $$",br(),"$$ 0, density-dependent $$")
                       )
                     ),
                     conditionalPanel(
                       condition = "input.muValue == '1'",
                       withMathJax(
                         helpText(
                           "Susceptible $$\\frac{dS}{dt} =\\mu_B N - \\mu_D S - \\beta \\frac{ S I}{N^q}$$"
                         ),
                         helpText("Exposed $$\\frac{dE}{dt} = \\beta \\frac{ S I}{N^q} - \\gamma E - \\mu_D E$$"),
                         helpText("Infectious $$\\frac{dI}{dt} = \\gamma E - \\sigma I - \\mu_D I $$"),
                         helpText("Recovered $$\\frac{dR}{dt} = \\sigma I - \\mu_D R $$"),
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$"),
                         helpText("q-Value", br(), "$$ 1, frequency-dependent $$",br(),"$$ 0, density-dependent $$")
                       )
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "input.modelSelect == 'SEIRD'",
                     conditionalPanel(
                       condition = "input.muValue == '0'",
                       withMathJax(
                         helpText("Susceptible $$\\frac{dS}{dt} = - \\beta \\frac{ S I}{N^q}$$"),
                         helpText("Exposed $$\\frac{dE}{dt} = \\beta \\frac{ S I}{N^q} - \\gamma E $$"),
                         helpText("Infectious $$\\frac{dI}{dt} = \\gamma E - \\sigma I - \\delta I $$"),
                         helpText("Recovered $$\\frac{dR}{dt} = \\sigma I $$"),
                         helpText("Dead $$ \\frac{dD}{dt} = \\delta I $$"),
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$"),
                         helpText("q-Value", br(), "$$ 1, frequency-dependent $$",br(),"$$ 0, density-dependent $$")
                       )
                     ),
                     conditionalPanel(
                       condition = "input.muValue == '1'",
                       withMathJax(
                         helpText(
                           "Susceptible $$\\frac{dS}{dt} =\\mu_B N - \\mu_D S - \\beta \\frac{ S I}{N^q}$$"
                         ),
                         helpText("Exposed $$\\frac{dE}{dt} = \\beta \\frac{ S I}{N^q} - \\gamma E - \\mu_D E$$"),
                         helpText("Infectious $$\\frac{dI}{dt} = \\gamma E - \\sigma I -  \\delta I- \\mu_D I $$"),
                         helpText("Recovered $$\\frac{dR}{dt} = \\sigma I - \\mu_D R $$"),
                         helpText("Dead $$ \\frac{dD}{dt} = \\delta I $$"),
                         helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$"),
                         helpText("q-Value", br(), "$$ 1, frequency-dependent $$",br(),"$$ 0, density-dependent $$")
                       )
                     )
                   ),
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
    p("Department of Mathematics & Computing,"),
    p("Calgary, AB, Canada"),
    p("Github ",a("link", href="https://github.com/TimPulfer/COMP-5690-R-Code", target="_blank")),
    br(),

    p(span("Ashok Krishnamurthy, PhD", style = "font-weight:bold")),
    p("Project Supervisor,"),
    p("Mount Royal University"),
    p("Department of Mathematics & Computing,"),
    p("Calgary, AB, Canada"),
    br(),
    
    p(
      "This app was written by Timothy Pulfer for his senior undergraduate project for the COMP 5690 course."
    )
  )
))