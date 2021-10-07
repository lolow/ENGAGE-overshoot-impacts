# Drake master script
# r_make()
# Info about drake: https://books.ropensci.org/drake

source("R/packages.R")  # Load your packages.
source("R/functions.R") # Functions definition.
source("R/plan.R")      # Drake plan definition.

config <- drake_config(plan)
