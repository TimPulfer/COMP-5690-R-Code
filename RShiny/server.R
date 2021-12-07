library(shiny)
library(shiny)
library(shinyjs)
library(shinyhelper)
library(shinyWidgets)
library(deSolve)
library(ggplot2)
library(tidyverse)
server <- function(input, output) {
  #############################################
  #####            PLOT - SIR             #####
  #############################################
  sir_equations <- function(time, variables, parameters) {
    S <- variables[1]
    I <- variables[2]
    R <- variables[3]
    N <- variables[4]
    q <- variables[5]
    dS <-  (input$muBirth * N) - (input$muDeath * S) - (input$betaSIR * ((S * I) / (N ^ q)))
    dI <-  (input$betaSIR *((S * I) / (N ^ q))) - (input$gammaSIR * I) - (input$muDeath * I)
    dR <-  (input$gammaSIR * I) - (input$muDeath * R)
    dN <- dS + dI + dR
    list(c(dS, dI, dR, dN, q))
  }
  
  sir_values <- reactive({
    req(input$timesteps, input$betaSIR, input$gammaSIR, input$muBirth, input$muDeath)
    ode(
      y = c(
        S = input$susceptibleSIR,
        I = input$infectedSIR,
        R = input$recoveredSIR,
        N = input$populationSIR,
        q = as.integer(input$qValue)
      ),
      times = seq(0, input$timesteps, by = 1),
      func = sir_equations,
      parms = c(
        beta = input$betaSIR,
        gamma = input$gammaSIR,
        muB = input$muBirth,
        muD = input$muDeath
      )
      
    )
  })
  
  output$plotSIR <- renderPlot({
    val <- as.data.frame(sir_values())
    ggplot(val, aes(x = time)) +
      theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
            axis.title.x = element_text(size=16,face="bold"), 
            axis.title.y = element_text(size=16,face="bold")) +
      ggtitle("SIR Epidemic Model") +
      theme(plot.title = element_text(size = 22, face="bold")) +
      ylab("Number of People") +
      xlab("Time") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) +
      geom_line(aes(y = S, color = "Blue"), size = 1.5) +
      geom_line(aes(y = I, color = "Red"), size = 1.5) +
      geom_line(aes(y = R, color = "Green"), size = 1.5) +
      scale_color_identity(name= "SIR", breaks = c("Blue", "Red", "Green"), 
                           labels = c("Susceptible", "Infected", "Recovered"), guide = "legend")
      
  })
  
  output$SIRPhasePlane <- renderPlot({
    val <- as.data.frame(sir_values())
    ggplot(val, aes(x = S)) +
      geom_line(aes(y = I, color = "Blue"), size = 1.5) +
      theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
            axis.title.x = element_text(size=16,face="bold"), 
            axis.title.y = element_text(size=16,face="bold")) +
      ggtitle("SIR Phase Plane") +
      theme(plot.title = element_text(size = 22,face="bold")) +
      ylab("Infected (I)") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) +
      xlab("Susceptible (S)")+
      scale_color_identity(breaks = "Blue", 
                           labels = "Susceptible")
  })
  
  
  output$tableSIR <- renderTable({
    val <- as.data.frame(sir_values())
    val <- val[-c(6)]
    return(val)
  })
  
  #############################################
  #####            PLOT - SIRD            #####
  #############################################
  sird_equations <- function(time, variables, parameters) {
    S <- variables[1]
    I <- variables[2]
    R <- variables[3]
    D <- variables[4]
    N <- variables[5]
    q <- variables[6]
    dS <- (input$muBirth * N) - (input$muDeath * S) - (input$betaSIRD * ((S * I) / (N ^ q)))
    dI <- (input$betaSIRD *((S * I) / (N ^ q))) - (input$gammaSIRD * I) - (input$deltaSIRD * I) - (input$muDeath * I)
    dR <- (input$gammaSIRD * I) - (input$muDeath * R)
    dD <- (input$deltaSIRD * I)
    dN <- dS + dI + dR + dD
    list(c(dS, dI, dR, dD, dN, q))
  }
  
  sird_values <- reactive({
    req(input$timesteps, input$betaSIRD, input$gammaSIRD, input$deltaSIRD,input$muBirth, input$muDeath)
    ode(
      y = c(
        S = input$susceptibleSIRD,
        I = input$infectedSIRD,
        R = input$recoveredSIRD,
        D = input$deadSIRD,
        N = input$populationSIRD,
        q = as.integer(input$qValue)
      ),
      times = seq(0, input$timesteps, by = 1),
      func = sird_equations,
      parms = c(
        beta = input$betaSIRD,
        gamma = input$gammaSIRD,
        delta = input$deltaSIRD,
        muB = input$muBirth,
        muD = input$muDeath
      )
    )
  })
  
  output$plotSIRD <- renderPlot({
    val <- as.data.frame(sird_values())
    ggplot(val, aes(x = time)) +
      theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
            axis.title.x = element_text(size=16,face="bold"), 
            axis.title.y = element_text(size=16,face="bold")) +
      ggtitle("SIRD Epidemic Model") +
      theme(plot.title = element_text(size = 22,face="bold")) +
      ylab("Number of People") +
      xlab("Time") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) +
      geom_line(aes(y = S, color = "Blue"), size = 1.5) +
      geom_line(aes(y = I, color = "Red"), size = 1.5) +
      geom_line(aes(y = R, color = "Green"), size = 1.5) +
      geom_line(aes(y = D, color = "Orange"), size = 1.5) +
      scale_color_identity(name= "SIRD", breaks = c("Blue", "Red", "Green", "Orange"), 
                           labels = c("Susceptible", "Infected", "Recovered", "Dead"), guide = "legend")
    
  })
  
  output$SIRDPhasePlane <- renderPlot({
    val <- as.data.frame(sird_values())
    ggplot(val, aes(x = S)) +
      geom_line(aes(y = I, color = "Blue"), size = 1.5) +
      theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
            axis.title.x = element_text(size=16,face="bold"), 
            axis.title.y = element_text(size=16,face="bold")) +
      ggtitle("SIRD Phase Plane") +
      theme(plot.title = element_text(size = 22,face = "bold")) +
      ylab("Infected (I)") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) +
      xlab("Susceptible (S)")+
      scale_color_identity(breaks = "Blue", 
                           labels = "Susceptible")
  })
  
  output$tableSIRD <- renderTable({
    val <- as.data.frame(sird_values())
    val <- val[-c(7)]
    return(val)
  })
  
  #############################################
  #####            PLOT - SEIR            #####
  #############################################
  seir_equations <- function(time, variables, parameters) {
    S <- variables[1]
    E <- variables[2]
    I <- variables[3]
    R <- variables[4]
    N <- variables[5]
    q <- variables[6]
    dS <-  (input$muBirth * N) - (input$muDeath * S) - (input$beta * ((S * I) / (N ^ q)))
    dE <-  (input$beta * ((S * I) / (N ^ q))) - (input$gamma * E) -(input$muDeath * E)
    dI <-  (input$gamma * E) - (I * input$sigma) - (input$muDeath * I)
    dR <-  (I * input$sigma) - (input$muDeath * R)
    dN <- dS + dE + dI + dR
    list(c(dS, dE, dI, dR, dN, q))
  }
  
  seir_values <- reactive({
    req(input$timesteps, input$beta, input$gamma,input$muBirth, input$muDeath)
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
        muB = input$muBirth,
        muD = input$muDeath
      )
      
    )
  })
  
  output$plotSEIR <- renderPlot({
    val <- as.data.frame(seir_values())
    
    ggplot(val, aes(x = time)) +
      ggtitle("SEIR Epidemic Model") +
      theme(axis.line = element_line(color="black"),
            axis.title.x = element_text(size=16,face="bold"), 
            axis.title.y = element_text(size=16,face="bold")) +
      theme(plot.title = element_text(size = 22,face = "bold"), axis.text=element_text(size=14)) +
      ylab("Number of People") +
      xlab("Time") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) +
      geom_line(aes(x = time, y = S, color = "Blue"), size = 1.5) +
      geom_line(aes(x = time, y = E, color = "Brown"), size = 1.5) +
      geom_line(aes(x = time, y = I, color = "Red"), size = 1.5) +
      geom_line(aes(x = time, y = R, color = "Green"), size = 1.5)+
      scale_color_identity(name= "SEIR", breaks = c("Blue", "Brown","Red", "Green"), 
                           labels = c("Susceptible", "Exposed","Infected", "Recovered"), guide = "legend")
    
  })
  
  output$SEIRPhasePlane <- renderPlot({
    val <- as.data.frame(seir_values())
    ggplot(val, aes(x = S)) +
      geom_line(aes(y = I, color = "Blue"), size = 1.5) +
      theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
            axis.title.x = element_text(size=16,face="bold"), 
            axis.title.y = element_text(size=16,face="bold")) +
      ggtitle("SEIR Phase Plane") +
      theme(plot.title = element_text(size = 22, face="bold")) +
      ylab("Infected (I)") +
      xlab("Susceptible (S)")+
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) +
      scale_color_identity(breaks = "Blue", 
                           labels = "Susceptible")
  })
  
  output$tableSEIR <- renderTable({
    valSEIR <- as.data.frame(seir_values())
    valSEIR <- valSEIR[-c(7)]
    return(valSEIR)
  })
  
  #############################################
  #####            PLOT - SEIRD          #####
  #############################################
  seird_equations <- function(time, variables, parameters) {
    S <- variables[1]
    E <- variables[2]
    I <- variables[3]
    R <- variables[4]
    D <- variables[5]
    N <- variables[6]
    q <- variables[7]
    dS <- (input$muBirth * N) - (input$muDeath * S) - (input$betaSEIRD * ((S * I) / (N ^ q)))
    dE <- (input$beta * ((S * I) / (N ^ q))) - (input$gammaSEIRD * E) -(input$muDeath * E)
    dI <- (input$gammaSEIRD * E) - (I * input$sigmaSEIRD) - (input$deltaSEIRD) - (input$muDeath * I)
    dR <- (I * input$sigma) - (input$muDeath * R)
    dD <- (input$deltaSEIRD * I)
    dN <- dS + dE + dI + dR + dD
    list(c(dS, dE, dI, dR, dD, dN, q))
  }
  
  seird_values <- reactive({
    req(input$timesteps, input$betaSEIRD, input$gammaSEIRD,input$muBirth, input$muDeath)
    ode(
      y = c(
        S = input$susceptibleSEIRD,
        E = input$exposedSEIRD,
        I = input$infectedSEIRD,
        R = input$recoveredSEIRD,
        D = input$deadSEIRD,
        N = input$populationSEIRD,
        q = as.integer(input$qValue)
      ),
      times = seq(0, input$timesteps, by = 1),
      func = seird_equations,
      parms = c(
        beta = input$betaSEIRD,
        gamma = input$gammaSEIRD,
        sigma = input$sigmaSEIRD,
        delta = input$deltaSEIRD,
        muB = input$muBirth,
        muD = input$muDeath
      )
    )
  })
  
  output$plotSEIRD <- renderPlot({
    val <- as.data.frame(seird_values())
    ggplot(val, aes(x = time)) +
      ggtitle("SEIRD Epidemic Model") +
      theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
            axis.title.x = element_text(size=16,face="bold"), 
            axis.title.y = element_text(size=16,face="bold")) +
      theme(plot.title = element_text(size = 22, face= "bold")) +
      ylab("Number of People") +
      xlab("Time") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) +
      geom_line(aes(x = time, y = S, color = "Blue"), size = 1.5) +
      geom_line(aes(x = time, y = E, color = "Brown"), size = 1.5) +
      geom_line(aes(x = time, y = I, color = "Red"), size = 1.5) +
      geom_line(aes(x = time, y = R, color = "Green"), size = 1.5)+
      geom_line(aes(x = time, y = D, color = "Orange"), size = 1.5)+
      scale_color_identity(name= "SEIRD", breaks = c("Blue", "Brown","Red", "Green", "Orange"), 
                           labels = c("Susceptible", "Exposed","Infected", "Recovered", "Dead"), guide = "legend")
    
  })
  
  output$SEIRDPhasePlane <- renderPlot({
    val <- as.data.frame(seird_values())
    ggplot(val, aes(x = S)) +
      geom_line(aes(y = I, color = "Blue"), size = 1.5) +
      theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
            axis.title.x = element_text(size=16,face="bold"), 
            axis.title.y = element_text(size=16,face="bold")) +
      ggtitle("SEIRD Phase Plane") +
      theme(plot.title = element_text(size = 22, face="bold")) +
      ylab("Infected (I)") +
      xlab("Susceptible (S)")+
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) +
      scale_color_identity(breaks = "Blue", 
                           labels = "Susceptible")
  })
  
  output$tableSEIRD <- renderTable({
    valSEIRD <- as.data.frame(seird_values())
    valSEIRD <- valSEIRD[-c(8)]
    return(valSEIRD)
  })
}