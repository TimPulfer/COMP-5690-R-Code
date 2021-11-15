# Author:           Timothy Pulfer
# Instructor:       Ashok Krishnamurthy
# Course:           COMP 5690 (FALL 2021)
#
#
# Purpose:          To produce a R Shiny application that displays statistical
#                   non spatial data on different epidemiological models based on
#                   user input.

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

epidemiologyApp = shinyApp(ui <- fluidPage(navbarPage(
    title = span("Compartmental Models of Infectious Diseases", style = "color:#000000; font-weight:bold; font-size:15pt"),
    tabPanel(title = "Model",
             sidebarLayout(
                 sidebarPanel((""),
                              div(
                                  radioButtons(
                                      inputId = "modelSelect",
                                      label = ("Epidemic Model"),
                                      choiceValues = list("SIR", "SIRS", "SEIR", "SEIRD", "SVEIRD"),
                                      choiceNames = list("SIR", "SIRS", "SEIR", "SEIRD", "SVEIRD"),
                                      inline = TRUE,
                                      width = "1000px"
                                  ),
                                  
                                  radioButtons(
                                      inputId = "qValue",
                                      label = ("Model Formulation (q Value)"),
                                      choiceValues = list(1, 0),
                                      choiceNames = list("True-Mass Action", "Pseudo-Mass Action"),
                                      inline = TRUE,
                                      width = "1000px",
                                      selected = "0"
                                  ),
                                  
                                  radioButtons(
                                      inputId = "distributSelect",
                                      label = ("Distribution Method"),
                                      choiceValues = list("Deterministic", "Stochastic"),
                                      choiceNames = list("Deterministic", "Stochastic"),
                                      inline = TRUE,
                                      width = "1000px"
                                  ),
                                  
                                  radioButtons(
                                      inputId = "muValue",
                                      label = ("Population"),
                                      choiceValues = list("1", "0"),
                                      choiceNames = list("Open", "Closed"),
                                      inline = TRUE,
                                      width = "1000px",
                                      selected = "0"
                                  ),
                                  
                                  withMathJax(),
                                  conditionalPanel(
                                      condition = "input.muValue == '1'",
                                      sliderInput(
                                          #TODO: Adjust min/ Max
                                          inputId = "mu",
                                          label = "Birth and Death Rate  (\\( \\mu\\))",
                                          min = 0,
                                          max = 0.1,
                                          step = 0.0001,
                                          value = 0.00
                                      ),
                                  ),
                                  
                                  
                                  
                                  conditionalPanel(
                                      condition = "input.modelSelect == 'SIR'",
                                      withMathJax(),
                                      sliderInput(
                                          #TODO: Adjust min/ Max
                                          inputId = "beta",
                                          label = "Transmission Rate (\\( \\beta\\))",
                                          min = 0,
                                          max = 0.02,
                                          step = 0.0001,
                                          value = 0.0015,
                                          animate = animationOptions(interval = 100, pauseButton = NULL)
                                      ),
                                      
                                      sliderInput(
                                          #TODO: Adjust min/ Max
                                          inputId = "gamma",
                                          label = "Removal Rate  (\\( \\gamma\\))",
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
                                          value = 0,
                                          min = 0,
                                          max = maxPopulation,
                                          step = 1,
                                      ),
                                  ),
                                  
                                  conditionalPanel(
                                      condition = "input.modelSelect == 'SIRS'",
                                      withMathJax(),
                                      sliderInput(
                                          #TODO: Adjust min/ Max
                                          inputId = "beta",
                                          label = "Transmission Rate (\\( \\beta\\))",
                                          min = 0,
                                          max = 0.02,
                                          step = 0.000001,
                                          value = 0.0015,
                                          animate = animationOptions(interval = 100, pauseButton = NULL)
                                      ),
                                      
                                      sliderInput(
                                          #TODO: Adjust min/ Max
                                          inputId = "gamma",
                                          label = "Removal Rate  (\\( \\gamma\\))",
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
                                      withMathJax(),
                                      sliderInput(
                                          #TODO: Adjust min/ Max
                                          inputId = "beta",
                                          label = "Transmission Rate (\\( \\beta\\))",
                                          min = 0,
                                          max = 0.02,
                                          step = 0.000001,
                                          value = 0.0015,
                                          animate = animationOptions(interval = 100, pauseButton = NULL)
                                      ),
                                      
                                      sliderInput(
                                          #TODO: Adjust min/ Max
                                          inputId = "gamma",
                                          label = "Removal Rate  (\\( \\gamma\\))",
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
                                          value = 0,
                                          min = 0,
                                          max = maxPopulation,
                                          step = 1,
                                      ),
                                      numericInput(
                                          inputId = "dead",
                                          label = "Dead (D)",
                                          value = 0,
                                          min = 0,
                                          max = maxPopulation,
                                          step = 1,
                                      ),
                                  ),
                                  
                                  conditionalPanel(
                                      condition = "input.modelSelect == 'SVEIRD'",
                                      withMathJax(),
                                      sliderInput(
                                          #TODO: Adjust min/ Max
                                          inputId = "beta",
                                          label = "Transmission Rate (\\( \\beta\\))",
                                          min = 0,
                                          max = 0.02,
                                          step = 0.000001,
                                          value = 0.0015,
                                          animate = animationOptions(interval = 100, pauseButton = NULL)
                                      ),
                                      
                                      sliderInput(
                                          #TODO: Adjust min/ Max
                                          inputId = "gamma",
                                          label = "Removal Rate  (\\( \\gamma\\))",
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
                                          inputId = "vaccinated",
                                          label = "Vaccinated (V)",
                                          value = 0,
                                          min = 0,
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
                                          value = 0,
                                          min = 0,
                                          max = maxPopulation,
                                          step = 1,
                                      ),
                                      numericInput(
                                          inputId = "dead",
                                          label = "Dead (D)",
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
                                  value = 100,
                                  min = 1,
                                  max = maxTimeSteps,
                                  step = 1,
                              ),
                              
                              #actionButton("resetAll", "Clear values"),
                              
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
                                             "Susceptible $$\\frac{dS}{dt} =\\mu N - \\mu S - \\beta \\frac{ S I}{N^q}$$"
                                         ),
                                         helpText(
                                             "Infectious $$\\frac{dI}{dt} = \\frac{\\beta S I}{N^q} - \\gamma I - \\mu I$$"
                                         ),
                                         helpText("Recovered $$\\frac{dR}{dt} = \\gamma I - \\mu R $$"),
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
                                             "Susceptible $$\\frac{dS}{dt} =\\mu N - \\mu S - \\beta \\frac{ S I}{N^q}$$"
                                         ),
                                         helpText("Exposed $$\\frac{dE}{dt} = \\beta \\frac{ S I}{N^q} - \\gamma I - \\mu E$$"),
                                         helpText("Infectious $$\\frac{dI}{dt} = \\gamma I - \\sigma R - \\mu I $$"),
                                         helpText("Recovered $$\\frac{dR}{dt} = \\sigma R - \\mu R $$"),
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
)),


##########################################################################################
#############################----SERVER SIDE----##########################################
##########################################################################################


server <- function(input, output) {
    #############################################
    #####            PLOT 1 - SIR           #####
    #############################################
    sir_equations <- function(time, variables, parameters) {
        S <- variables[1]
        I <- variables[2]
        R <- variables[3]
        N <- variables[4]
        q <- variables[5]
        
        dS <-  (input$mu * N) - (input$mu * S) - (input$beta * ((S * I) / (N ^ q)))
        dI <-  ((input$beta * S * I) / (N ^ q)) - (input$gamma * I) - (input$mu * I)
        dR <-  (input$gamma * I) - (input$mu * R)
        
        dN <- dS + dI + dR
        
        list(c(dS, dI, dR, dN, q))
        
    }
    
    sir_values <- reactive({
        req(input$timesteps, input$beta, input$gamma, input$mu)
        ode(
            y = c(
                S = input$susceptible,
                I = input$infected,
                R = input$recovered,
                N = input$population,
                q = as.integer(input$qValue)
            ),
            times = seq(0, input$timesteps, by = 1),
            func = sir_equations,
            parms = c(
                beta = input$beta,
                gamma = input$gamma,
                mu = input$mu
            )
            
        )
    })
    
    output$plotSIR <- renderPlot({
        val <- as.data.frame(sir_values())
        
        ggplot(val, aes(x = time)) +
            theme(axis.line = element_line(color="black")) +
            ggtitle("SIR Epidemic Model") +
            theme(plot.title = element_text(size = 22)) +
            ylab("Number of People") +
            xlab("Time") +
            geom_line(aes(y = S), color = "Blue") +
            geom_line(aes(y = I), color = "Red") +
            geom_line(aes(y = R), color = "Green")
    })
    
    output$SIRPhasePlane <- renderPlot({
        val <- as.data.frame(sir_values())
        
        ggplot(val, aes(x = S)) +
            geom_line(aes(y = I), color = "Blue") +
            theme(axis.line = element_line(color="black")) +
            ggtitle("SIR Phase Plane") +
            theme(plot.title = element_text(size = 22)) +
            ylab("Infected (I)") +
            xlab("Susceptible (S)")
    })
    
    
    output$tableSIR <- renderTable({
        val <- as.data.frame(sir_values())
        val <- val[-c(6)]
        return(val)
    })
    
    
    #############################################
    #####            PLOT 1 - SEIR          #####
    #############################################
    seir_equations <- function(time, variables, parameters) {
        S <- variables[1]
        E <- variables[2]
        I <- variables[3]
        R <- variables[4]
        N <- variables[5]
        q <- variables[6]
        
        dS <-  (input$mu * N) - (input$mu * S) - (input$beta * ((S * I) / (N ^ q)))
        dE <-  (input$beta * ((S * I) / (N ^ q))) - (input$gamma * I) -(input$mu * E)
        dI <-  (input$gamma * I) - (R * input$sigma) - (input$mu * I)
        dR <-  (R * input$sigma) - (input$mu * R)
        
        dN <- dS + dE + dI + dR
        
        list(c(dS, dE, dI, dR, dN, q))
        
    }
    
    
    seir_values <- reactive({
        req(input$timesteps, input$beta, input$gamma, input$mu)
        ode(
            y = c(
                S = input$susceptible,
                E = input$exposed,
                I = input$infected,
                R = input$recovered,
                N = input$population,
                q = as.integer(input$qValue)
            ),
            times = seq(0, input$timesteps, by = 1),
            func = seir_equations,
            parms = c(
                beta = input$beta,
                gamma = input$gamma,
                mu = input$mu
            )
            
        )
    })
    
    output$plotSEIR <- renderPlot({
        val <- as.data.frame(seir_values())
        
        ggplot(val, aes(x = time)) +
            ggtitle("SIR Epidemic Model") +
            theme(axis.line = element_line(color="black")) +
            theme(plot.title = element_text(size = 22)) +
            ylab("Number of People") +
            xlab("Time") +
            geom_line(aes(x = time, y = S), color = "Blue") +
            geom_line(aes(x = time, y = E), color = "Brown") +
            geom_line(aes(x = time, y = I), color = "Red") +
            geom_line(aes(x = time, y = R), color = "Green")
        
    })
    
    output$SEIRPhasePlane <- renderPlot({
        val <- as.data.frame(sir_values())
        
        ggplot(val, aes(x = S)) +
            geom_line(aes(y = I), color = "Blue") +
            theme(axis.line = element_line(color="black")) +
            ggtitle("SEIR Phase Plane") +
            theme(plot.title = element_text(size = 22)) +
            ylab("Infected (I)") +
            xlab("Susceptible (S)")
    })
    
    output$tableSEIR <- renderTable({
        valSEIR <- as.data.frame(seir_values())
        valSEIR <- valSEIR[-c(7)]
        return(valSEIR)
    })
    
})
