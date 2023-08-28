# this is an example of WLS autoplotting lifespans from an experiment with multiple concentration variables of a drug
# all questions regarding this script, or the WLSplot library in general should be directed toward Blaise at blaisemariner17@gmail.com

rm(list = ls())

# this sets the working directory to this script's path
if (isRStudio <- Sys.getenv("RSTUDIO") == "1"){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }
# libraries required for WLSplot
require(scales)
require(WLSplot)

#Run the autoplot function, which will recognize the .ods file in the current working directory and output svg file(s) into the working directory
WLSplot::WLS_autoplot(lifespan_type = "Drug", return_data = FALSE)
