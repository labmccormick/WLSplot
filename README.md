# WLSplot v1.0

WLSplot v1.0 An R package that turns raw worm lifespan data into informative survival curves. 

If used, please cite:

Mariner, BL; Peterson, J; Taylor, JC; Achusim, AT; McCormick, MA (2023). Easy <i>C. elegans</i> Worm Lifespan Plotting and Statistics with WLSplot. <i>microPublication Biology</i>. <a href="https://doi.org/10.17912/micropub.biology.000991" target="_blank" rel="noopener noreferrer">10.17912/micropub.biology.000991</a>.

![plot](./pictures/figure1.jpg?raw=true)

The WLS_autoplot() function in this library recognizes, transforms, and plots raw worm lifespan data into Kaplan-Meier survival curves. This function has the flexibility to analyze RNAi, mutant/genotype, or drug/treatment experiments while integrating crucial experimental information into the output image, as seen above. 

# Setting up WLSplot

Walk-through tutorial on how to set up WLSplot: https://youtu.be/R_BCgvE1z6A

1. Install R and RStudio. Follow the tutorial here: https://rstudio-education.github.io/hopr/starting.html

2. Install the WLSplot dependencies. You can do this by opening RStudio/R and running the following in the R Console:

```
install.packages(c("xml2", "ragg", "pkgdown", "scales", "systemfonts", "textshaping", "devtools", "readODS", "ggplot2", "svglite", "RColorBrewer", "ggExtra", "survival", "ggtext", "ggfortify", "plyr", "markdown"))
```

In general, R will warn you if package dependencies outside of R are missing and it is easiest to see these when installing the R libraries below one at a time if you run into any unforeseen snags. 

You may need to install freetype 2 (currently v2.11.1) and related libraries on your machine outside of R before installing the R systemfonts and pkgdown packages. 

**e.g. if you are running Ubuntu or a deb-based linux distribution, you may have to run the following in the Terminal (open terminal with Ctrl + Alt + T):**

```
sudo apt update && sudo apt upgrade -y

sudo apt install libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev liblapack-dev libopenblas-dev libxml2-dev

```

3. Install WLSplot from GitHub by running the following in the RStudio/R Console:

```
devtools::install_github("labmccormick/WLSplot", build_vignettes = TRUE)
```

# Running WLSplot: An example

Walk-through tutorial on how to run WLSplot: https://youtu.be/xWRspLljrvE

### Prerequisites: 

#### If you are unfamiliar with the terminal, the example below shows how to download files by just using the GitHub web interface. If you'd like to learn how to use the terminal, we recommend looking at beginner tutorials like this: https://www.freecodecamp.org/news/command-line-for-beginners/

A. (OPTIONAL) Users will be able to download example files using "wget" (currently v 1.21.2) if they are on a Mac or Linux operating system. The wget commands provided in the protocol below will download the files to the current working directory, i.e. the directory in which the terminal is open (Hint: To see what current folder or directory you’re in, type "pwd" into the terminal). 

Windows (or if inexperienced with using the terminal):

If users are on a Windows operating system or inexperienced with using the terminal, we recommend downloading the files by opening them in your web browser and clicking the download button on the top right of the window:

![plot](./pictures/downloadbutton.jpg?raw=true)

If you do not have wget installed, run the following in the Terminal:

Mac OS (in Terminal): (Check to see if you have homebrew installed by typing "brew" into the terminal. If you do not have homebrew installed, see this guide on how to install: https://docs.brew.sh/Installation)
```
brew install wget
```

Apt-based Linux, e.g. Ubuntu (in Terminal):
```
sudo apt update && sudo apt upgrade -y
sudo apt install wget
```

B. Your computer will need to have LibreOffice installed. To install, please follow the instructions here: https://www.libreoffice.org/get-help/install-howto/.

C. To plot a worm lifespan, you will need worm lifespan data. The following example will walk you through step-by-step how to plot provided worm lifespan data. If you have your own data, please make sure it is in the format matching the WLS blank spreadsheet provided in the labmccormick/WLSplot/Examples directory. You can download this file by opening it on the web and clicking the download button on the top right corner of the window, or by running the following in the Terminal: 

Windows (or if inexperienced with using the terminal):

We recommend the user download files individually by opening them up on the GitHub web interface and clicking the download button on the top right of the window.

Link: https://github.com/labmccormick/WLSplot/blob/main/Examples/BLANK_WLS_file.csv

Mac OS (in Terminal):
```
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/BLANK_WLS_file.csv
```

Apt-based Linux, e.g. Ubuntu (in Terminal):
```
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/BLANK_WLS_file.csv
```

D. Your computer will need R, RStudio, and the WLSplot dependencies. Follow the instructions provided in the previous section to accomplish this prerequisite.

E. WLSplot create svg files, which can be easily opened and edited further in Inkscape. To install, see https://inkscape.org/

## WLSplot standard operating procedure

1. Create a new directory for your lifespan analysis. For the example, I name this directory Example_WLSplot. 

![plot](./pictures/Directory.jpg?raw=true)

2. Create a new daughter directory within your new directory. This daughter directory will contain the files of raw worm lifespan data. The purpose of this organization is so that you can analyze many different lifespans at once with one script. For the example, I name this daughter directory "Lifespan_1" and within, I save the csv files provided in ./Examples/WLS_autoplot/Lifespan_1/, as imaged below. These files can be individually downloaded by opening them up on GitHub and clicking the download button on the top right of the window, or you can run the following in the terminal:

Windows (or if inexperienced with using the terminal):

We recommend the user download files individually by opening them up on the GitHub web interface and clicking the download button on the top right of the window.

Links: https://github.com/labmccormick/WLSplot/blob/main/Examples/WLS_autoplot/Lifespan_1/N2.csv; https://github.com/labmccormick/WLSplot/blob/main/Examples/WLS_autoplot/Lifespan_1/yfg-1.csv; https://github.com/labmccormick/WLSplot/blob/main/Examples/WLS_autoplot/Lifespan_1/yfg-2.csv; https://github.com/labmccormick/WLSplot/blob/main/Examples/WLS_autoplot/Lifespan_1/yfg-3.csv

Mac OS (in Terminal):
```
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/WLS_autoplot/Lifespan_1/N2.csv
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/WLS_autoplot/Lifespan_1/yfg-1.csv
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/WLS_autoplot/Lifespan_1/yfg-2.csv
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/WLS_autoplot/Lifespan_1/yfg-3.csv
```

Apt-based Linux, e.g. Ubuntu (in Terminal):
```
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/WLS_autoplot/Lifespan_1/N2.csv
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/WLS_autoplot/Lifespan_1/yfg-1.csv
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/WLS_autoplot/Lifespan_1/yfg-2.csv
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/WLS_autoplot/Lifespan_1/yfg-3.csv
```
To move onto step 3, you should have these csv files in the new daughter directory created in this step as seen below. 

![plot](./pictures/Directory_odsfiles.jpg?raw=true)
![plot](./pictures/Directory_csvfiles.jpg?raw=true)

3. Navigate out of the daughter directory, and create an R file to run the WLS_autoplot() function. For the example, I downloaded the R file Examples/WLS_autoplot/Example_WLS_autoplot.R and placed it in this directory. This can be done by opening this file and clicking the download button the the top right of the screen, or by running the following command in the terminal:

Windows (or if inexperienced with using the terminal):

We recommend the user download files individually by opening them up on the GitHub web interface and clicking the download button on the top right of the window.

Link: https://github.com/labmccormick/WLSplot/blob/main/Examples/WLS_autoplot/Example_WLS_autoplot.R

Mac OS (in Terminal): 
```
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/WLS_autoplot/Example_WLS_autoplot.R
```

Apt-based Linux, e.g. Ubuntu (in Terminal):
```
wget https://github.com/labmccormick/WLSplot/raw/main/Examples/WLS_autoplot/Example_WLS_autoplot.R
```

At this point, the example looks like the following:

![plot](./pictures/Directory_Rfile.jpg?raw=true)

4. Open the R file with RStudio. For the example, I open Example_WLS_autoplot.R. This script contains only several lines and should look something like this:

![plot](./pictures/RStudio_example.jpg?raw=true)

5. You can now run this script line-by-line by pressing Ctrl + Enter, or the entire script with Ctrl + Shift + Enter when opened with RStudio. Or, you can run the following in the terminal:

Mac/Linux users (in Terminal): First, navigate to the directory with the R script using "cd". e.g. "cd ~/Example_WLS_plot/"

```
Rscript Example_WLS_autoplot.R 
```

7. When complete, the resulting lifespan svg files should be saved out into a "plots" subdirectory.

![plot](./pictures/dir_out.jpg?raw=true)
![plot](./pictures/svgplots_out.jpg?raw=true)

7. Now you can open the svg files with Inkscape and make further edits if you would like. For the example, we created three plots because two of the genotypes lived significantly differently from the control. The plot with all the lifespans should look something like this: 

![plot](./pictures/example_lifespan1.jpg?raw=true)

Other examples on the flexibility and functionality of WLS_autoplot() are provided in the Examples/ directory. There, the user will find the scripts and files to run RNAi and drug/treatment lifespan experiments. There is also an example showing how the script can batch analyze multiple lifespan experiments in one command. 
