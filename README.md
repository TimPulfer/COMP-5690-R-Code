# Compartmental Models of Epidemiology

## Shiny app authors

* **Timothy Pulfer** | *Lead Developer*
* **Ashok Krishnamurthy** | *Template design and tips*
 
## Overview

Run the app by loading `app.R` and clicking `Run App`.

## Features
In the R Shiny app's current state, it can run SIR, SIRD, SEIR and SEIRD models. Each of these four models can be ran in either true-mass action (frequency-dependent) or pseudo-mass action (density-dependent) for a given population. Additionally, the option to run these models with vital dynamics with independent death (due to natural causes) and birth rates is also available. Plans to incorporate a stochastic version of each model are in the works and will be included in the next major version of the app.

Once appropriate parameters are selected by the user, the app will generate a time-series plot, phase plane plot and output summary for the selected model. These graphs are reactive to user input and will dynamically change as the user changes their parameters. The mathematical model tab will also react to user input to show the correct mathematical model equations being used to produce the output. On the ‘Plot’ tab, a flow chart depicting how an individual may move through the different epidemic compartments is displayed, this is also reactive to user input.

## References

## Feedback

[Timothy Pulfer](mailto:tpulf154@mtroyal.ca)  

This interactive R Shiny app is maintained by Timothy Pulfer. We welcome questions, insights, and feedback.
