# The "shiny" and "MASS" packages need to be installed. The MASS package is
# loaded automatically during the App generation.

# install.packages("shiny")
# install.packages("MASS")
library(shiny)

# Run app in "showcase" mode. This should open it in a separate window
# or your browser, and you should be able to see the ui.R and server.R
# files next to the app.
runApp("../mc_power_med", display.mode = "normal")
