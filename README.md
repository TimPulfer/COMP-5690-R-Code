# Compartmental Models of Epidemiology
 
## Overview

Run the app by loading `app.R` and clicking `Run App`.

## Shiny app authors

* **Timothy Pulfer** | *Lead Developer*
* **Ashok Krishnamurthy** | *Template design and tips*

## Features
In the apps current state, it can run SIR, SIRD, SEIR and SEIRD models. Each of these four models can be ran in either true-mass or pseudo-mass action for the given population. Additionally, the option to run these models with vital dynamics with independent death and birth rates is also available. Plans to incorporate a stochastic version of each model are in the works and will be included in the next major version of the app.

Once appropriate parameters are selected by the user, the app will generate a timeseries plot, phase plane and output summary for the selected model. These graphs are reactive to user input and will dynamically change as the user changes their parameters. The mathematical model tap will also react to user input to show the correct mathematical equations being used to produce the output. On the ‘Plot’ tab, a flow chart depicting how an individual may move through the different compartments is displayed, this is also reactive to user input.



## References

## Feedback

[Timothy Pulfer](mailto:tpulf154@mtroyal.ca)  

This interactive R Shiny app is maintained by Timothy Pulfer. We welcome questions, insights, and feedback.
