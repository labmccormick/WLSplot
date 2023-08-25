# this is an example of WLS autoplotting lifespans in multiple subdirectories
# all questions regarding this script, or the WLSplot library in general should be directed toward Blaise at blaisemariner17@gmail.com

rm(list = ls())

# this sets the working directory to this script's path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# libraries required for WLSplot
require(scales)
require(WLSplot)

#convert the poorly-formatted spreadsheet to a usable matrix for WLS_autoplot.
# this function contains an option to control where the processed file is written.
#Automatically, it will create a "results_counts_converted" directory and save the processed file there.
WLSplot::WLS_convert_counts()

#From the convert_counts() file, you can pass it in to the WLS_autoplot function.
WLS_autoplot(lifespan_type = "Genotype",
             processed_file = "results_counts_converted/Lifespan_LS1.csv")

