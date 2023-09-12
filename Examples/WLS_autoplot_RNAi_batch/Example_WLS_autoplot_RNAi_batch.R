# this is an example of WLS autoplotting lifespans in multiple subdirectories
# all questions regarding this script, or the WLSplot library in general should be directed toward Blaise at blaisemariner17@gmail.com

rm(list = ls())

# this sets the working directory to this script's path
if (isRStudio <- Sys.getenv("RSTUDIO") == "1"){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }
# libraries required for WLSplot
require(scales)
require(WLSplot)

# Running autoplot, which will automatically go into the the files within this directory to plot lifespans
WLSplot::WLS_autoplot(lifespan_type = "RNAi", empty_vector = "PAD12")
