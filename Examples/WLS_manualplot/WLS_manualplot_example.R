# this is an example of WLS autoplotting lifespans in multiple subdirectories
# all questions regarding this script, or the WLSplot library in general should be directed toward Blaise at blaisemariner17@gmail.com

rm(list = ls())

# this sets the working directory to this script's path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

require(scales)
require(WLSplot)

#The following example shows that autoplot can be used to output a matrix that is used for the Kaplan-Meir plotting.
lifespan_data <- WLSplot::WLS_autoplot(lifespan_type = "Genotype", return_data = TRUE)
head(lifespan_data)

#This lifespan_data can then be passed into the WLS_manualpot function for more control of the axis labels, researchers, limits, etc.
WLS_manualplot(lifespan_data = lifespan_data, col_list = c("black", "blue", "red", "darkgreen"),
               control = "N2", plot_title = "Test", lifespan_type = "Genotype", researchers = "BLM",
               x_upper_bound = 35, plot_individual_lifespans_to_control = FALSE, stats = TRUE, temperature = "25°C"
                )
