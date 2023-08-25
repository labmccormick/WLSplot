# WLSplot

### Description 

WLSplot is used as a tool that currently supports plotting one variable worm lifespan experiments for RNAi, Treatment/Drug, or Genotype.

The functions plot worm lifespans based on the experimental control and outputs as an svg. There is also the option to output the worm data calculated for the Kap Meier survival curve. 

#### More specific description of WLSplot functions:

There is one function used to graph the three types of experiments: WLS_autoplot(), where you pass in the "lifespan_type" keyword argument as "Treatment", "RNAi", or "Genotype" experiment. The negative controls, used for stats and plotting, can be passed into the function as keyword arguments for each type of experiment, too. Make sure to check defaults and keyword argument descriptions.

The alternative is to pass in WLS_manualplot(), which is a function that returns a lifespan plot with correctly formatted data for Kaplan-Meier survival curves. With WLS_manualplot, you have the ability to plot multiple variables at a time.

# Setting up WLSplot

1. Install R and RStudio. Follow the tutorial here: https://rstudio-education.github.io/hopr/starting.html

2. Install the WLSplot dependencies. You can do this by opening RStudio/R and running the following:

```
rm(list = ls())

install.packages(c("ragg","pkgdown","scales", "systemfonts", "textshaping", "devtools", "readODS", "ggplot2", "svglite", "RColorBrewer", "ggExtra", "survival", "ggtext", "ggfortify", "plyr", "markdown"))
```

In general R will warn you if package dependencies outside of R are missing and it easiest to see these when installing the R libraries below one at a time if you run into any unforeseen snags. 

You may need to install freetype 2 and related libraries on your machine outside of R before installing the R systemfonts and pkgdown packages. E.g. if you are running Ubuntu or a deb-based linux distribution, run the following in the terminal:

```
sudo apt update && sudo apt upgrade -y

sudo apt install libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
```

3. Install WLSplot from github by running the following in RStudio/R:

```
devtools::install_github("labmccormick/WLSplot", build_vignettes = TRUE)
```


# Running WLSplot: An example provided.

### Prerequisites: 

A. Lifespan spreadsheet(s) equal to or very similar to the blank WLS lifespan .ods file given in the Examples directory. If you are unfamiliar with .ods files, this is the filetype used for "excel" sheets in LibreOffice, which is free and open-source. To install libreoffice, visit here: https://www.libreoffice.org/get-help/install-howto/. 

B. Installation of R, RStudio, and the WLSplot dependencies (see Setting up WLSplot above).

## Standard operating procedure

1. Create a new directory for your lifespan analysis. For the example, I name this directory Example_WLSplot. 

![plot](./pictures/Directory.jpg?raw=true)

2. Create a new directory within this one and save the .ods lifespan files of interest there. For the example, I name this new directory Lifespan_1 and then download the .ods files provided in Examples/WLS_autoplot/Lifespan_1/ and save them there, as imaged below. You can initially download these files by A) using the wget command in the terminal in the created Lifespan_1 directory (e.g. "wget https://github.com/labmccormick/WLSplot/blob/main/Examples/WLS_autoplot/Lifespan_1/N2.ods") or B) by navigating to the file you'd like to download and click the download button on the top right of the page.

![plot](./pictures/Directory_odsfiles.jpg?raw=true)
![plot](./pictures/odsfiles.jpg?raw=true)

2. Go back to the first directory you made and create an R file where we will run WLS_autoplot(). For the example, I enter the Example_WLSplot directory and download the R file Examples/WLS_autoplot/Example_WLS_autoplot.R and place it in this directory. (you can use the following command in the terminal to download the R file: wget https://github.com/labmccormick/WLSplot/blob/main/Examples/WLS_autoplot/Example_WLS_autoplot.R")

![plot](./pictures/Directory_Rfile.jpg?raw=true)

4. Open the R file with RStudio. For the example, I open Example_WLS_autoplot.R.

![plot](./pictures/RStudio_example.jpg?raw=true)

5. You can now run this script line by line by pressing Ctrl + Enter, or the entire script with Ctrl + Shift + Enter.

6. When complete, running the script should result in svg files written out to the directory with the ods files. For the example, the svgs are in the Lifespan_1 directory. 

![plot](./pictures/svgs_out.jpg?raw=true)

7. Now you can open the svg files with Inkscape and make further edits if you would like. For the example, we created three plots because two of the genotypes lived significantly differently from the control. The plot with all the lifespans should look something like this: 

![plot](./pictures/example_lifespan.jpg?raw=true)
