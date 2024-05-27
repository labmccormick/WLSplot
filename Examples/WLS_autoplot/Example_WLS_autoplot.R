# this is an example of WLS autoplotting a lifespan where different worm Genotypes were tested.
# all questions regarding this script, or the WLSplot library in general should be directed toward Blaise at blaisemariner17@gmail.com

rm(list = ls())

# this sets the working directory to this script's path
if (isRStudio <- Sys.getenv("RSTUDIO") == "1"){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }

# libraries required for WLSplot
require(scales)
require(WLSplot)
require(survival)

#run the autoplot function, which will recognize automatically the .ods files in the current working directory.
WLSplot::WLS_autoplot(lifespan_type = "Genotype", return_data = FALSE)
