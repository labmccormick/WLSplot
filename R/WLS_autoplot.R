#' Plot WLS or return data by worm for .ods files
#'
#' @param lifespan_type This is where you put in what the lifespan type is. Accepted inputs: "RNAi", "Genotype", "Drug", "Treatment"
#' @param plot_individual_lifespans_to_control If TRUE, the individual lifespan plots compared to the control will be made.
#' @param lifespan_directories Directory in which will have all directories that are needed to be processed.
#' @param return_data If TRUE, this will return the data used for instead of making a graph survival plotting. This function cannot be done in batch processing files and will stop at the first directory
#' @param writeout_data #If TRUE, the data will be written out as a csv.
#' @param empty_vector If RNAi experiment, this is the empty vector.
#' @param Vehicle If drug experiment, what was the control, which would be used for stats.
#' @param Wild_type If Genotype experiment, what was the control, which would be used for stats description
#' @param col_list This controls the color of the lifespan plot lines.
#' @param document_researchers If TRUE, it will put the researchers names at the bottom of the plot.
#' @param col_names The names of the columns of the summary portion of the spreadsheet.
#' @param researchers default is to start with an empty vector as researchers.
#' @param omit_file Pattern of file to omit in the analysis. The McCormick lab uses an ods file with "stack" in the filename to describe the experiment.
#' @param processed_dir_path Pass in a processed file if you'd like to skip the processing from the raw data collection spreadsheet provided
#' @param Temperature If TRUE, the temperature will be provided in the plot.
#' @param lifespanfile_convert if TRUE this will convert the ods files to csv files for WLS_autoplot()
#' @param censored_plate What the censored plate code is. 'x' by default. 
#' @return Function can return the data that is used for lifespan plotting, or it saves lifespan plots out to the working directory.
#' @export WLS_autoplot
#'
#' @import readODS
#' @import ggplot2
#' @import svglite
#' @import RColorBrewer
#' @import survival
#' @import ggExtra
#' @import survival
#' @import ggtext
#' @import ggfortify
#' @import plyr
#' @import markdown
#' @import readxl
#'
#
# This function was written by Blaise Mariner to graph C. elegans lifespans with automated labeling
# for questions regarding the script or recommended improvements, please email Blaise at blaisemariner17@gmail.com
#'
#' 
WLS_autoplot <- function (lifespan_type = "RNAi", #this is where you put in what the lifespan type is. Accepted inputs: "RNAi", "Genotype", "Drug"
                          plot_individual_lifespans_to_control = TRUE, #if you do not want the individual lifespans plotted, pass in FALSE
                          lifespan_directories = list.dirs()[list.dirs()!="."], #This gets all the directories in which you want to plot the spreadsheets
                          return_data = FALSE, #If TRUE, return the data used for plotting. This function cannot be done in batch processing files and will stop at the first directory
                          writeout_data = FALSE, #If TRUE, the data will be written out as a csv.
                          empty_vector = c("L4440", "PAD12"), #If RNAi experiment, this is the empty vector
                          Vehicle = c("DMSO", "Vehicle"), #If drug experiment, what was the control, which would be used for stats
                          Wild_type = c("Wild type", "Wild-type", "Wildtype", "wild type", "wildtype", "wild-type", "N2"), #If Genotype experiment, what was the control, which would be used for stats
                          col_list = append(c("black", "red"),brewer.pal(8,"Set1")[3:8]), #This controls the color of the lifespan plot lines
                          document_researchers = TRUE, #If TRUE, it will put the researchers names at the bottom of the plot.
                          col_names = c("Dead", "M-E", "M-B", "M", "M_all", "Day"), #The names of the columns of the summary portion of the spreadsheet.
                          omit_file = "stack", #Pattern of file to omit in the analysis. The McCormick lab uses an ods file with "stack" in the filename to describe the experiment.
                          processed_dir_path = FALSE, #pass in a processed file if you'd like to skip the processing from the raw data collection spreadsheet provided
                          researchers = as.character(c()), #default is to start with an empty vector as researchers.
                          Temperature = TRUE, #do you want to include the temperature in the plot?
                          lifespanfile_convert = FALSE, #if true, this will convert the ods files to csv files for WLS_autoplot
                          censored_plate = 'x' #what is written on the plate if there is a censored plate 
){

  if (processed_dir_path == FALSE){
    starting_dir <- getwd()

    if (lifespan_directories[1] %in% list.dirs()[list.dirs()!="."] == FALSE){
      return("Did you put in the correct lifespan_directories?")
    }

    for (directory in lifespan_directories){
      browser()
      #make sure we are starting fresh.
      lifespan_data <- data.frame("Day" = c(), "status" = c(), "Genotype" = c(),
                                  "Treatment"=c(), "Bacteria"=c(), "MeanWLS" = c(), "N_worms" = c())
      
      directory <- sub("\\.", "", directory)

      # change the working directory to the directory within the lifespan_directories directory
      setwd(file.path(starting_dir, directory))
      
      #convert the ods or xlsx files to csvs for the WLS_autoplot
      if (lifespanfile_convert) {
        if (length(list.files(pattern="\\.ods")) > 0) {
          for (file in list.files(pattern = "\\.ods")){
            for (sheet_ in 1:length(readODS::list_ods_sheets(file))){
              new_lifespan_df <- readODS::read_ods(file, sheet = sheet_, col_names = FALSE)
              new_lifespan_df <- as.data.frame(new_lifespan_df)
              write.csv(x = new_lifespan_df, file = paste0(gsub(pattern = "\\.ods",replacement = "",x = file), "_", sheet_, ".csv"), row.names = FALSE)
            }            
          }
        } else if (length(list.files(pattern="\\.xlsx")) > 0){
          for (file in list.files(pattern="\\.xlsx")){
            for (sheet_ in (readxl::excel_sheets(file))){
              new_lifespan_df <- readxl::read_excel(file, sheet = sheet_)
              new_lifespan_df <- as.data.frame(new_lifespan_df)
              write.csv(x = new_lifespan_df, file = paste0(gsub(pattern = "\\.xlsx",replacement = "",x = file),"_", sheet_, ".csv"))              
            }
          }
        }
      }
      
      #for each lifespan file, build the results table that is going to be plotted.
      lifespan_files <- list.files(pattern="\\.csv")[grepl(paste0(omit_file), list.files(pattern="\\.csv"))==FALSE]
      plot_title <- sub("/", "", directory) #Title of the plot. Default is to grab the last directory in the working directory.
      data_summary <- data.frame("Day" = c(), "Dead" = c(), "Censored" = c(), 
                                 "Bacteria" = c(), "Strain" = c(), "Treatment" = c(),
                                 "Temperature" = c())
      for (file in lifespan_files) {
        new_lifespan_ods <- as.data.frame(read.csv(file, header = F))
        # get the experimental info (worm strain, bacteria, treatment, researchers running the experiment)
        for (row_ in 1:nrow(new_lifespan_ods)){
          if (sum(grepl("Strain", new_lifespan_ods[row_,1:10]))){
            strain <- new_lifespan_ods[row_, (grep("Strain", new_lifespan_ods[row_,1:10])+1)] #[row,col] in R
            
            i=1
            while (is.na(strain)) {
              strain <- new_lifespan_ods[row_, (grep("Strain", new_lifespan_ods[row_,1:10])+1+i)]
              i <- i + 1
              if (i > 50) {
                strain <- "No strain input"
              }
            }
            
            #if the strain name has a gene name in it, italicize it.
            if (grepl("[a-z]{3}-[0-9]{1}", strain) |
                grepl("[a-z]{4}-[0-9]{1}", strain) |
                grepl("[a-z]{3}-[0-9]{2}", strain) |
                grepl("[a-z]{4}-[0-9]{2}", strain)) {
              strain <- paste0("<i>", strain, "</i>") # Markdown formatting for italics
            }
            break
          }
        }
        for (row_ in 1:nrow(new_lifespan_ods)){
          if (sum(grepl("Bacteria", new_lifespan_ods[row_,]))){
            bacteria <- new_lifespan_ods[row_, (grep("Bacteria", new_lifespan_ods[row_,])+1)] #[row,col] in R
            i=1
            while (is.na(bacteria)) {
              bacteria <- new_lifespan_ods[row_, (grep("Bacteria", new_lifespan_ods[row_,])+1+i)]
              i <- i + 1
              if (i > 50) {
                bacteria <- "No bacteria input"
              }
            }
            break
          }
        }
        for (row_ in 1:nrow(new_lifespan_ods)){
          if (sum(grepl("Treatment", new_lifespan_ods[row_,]))){
            treatment <- new_lifespan_ods[row_, (grep("Treatment", new_lifespan_ods[row_,])+1)] #[row,col] in R
            i=1
            if (is.na(treatment)) {
              treatment <- "No treatment input"
            }
            break
          }
        }
        
        if (Temperature){
          for (row_ in 1:nrow(new_lifespan_ods)){
            if (sum(grepl("Temp", new_lifespan_ods[row_,]))){
              temperature <- new_lifespan_ods[row_, (grep("Temp", new_lifespan_ods[row_,])+1)] #[row,col] in R
              i=1
              if (is.na(temperature)) {
                temperature <- "No temp input"
              }
            }
          }
        } else {
          temperature = NA
        }
        
        if (document_researchers){
          for (row_ in 1:nrow(new_lifespan_ods)){
            if (sum(grepl("Initials", new_lifespan_ods[row_,]))){
              researchers = unique(append(researchers, unique(as.character(new_lifespan_ods[row_,
                                                                                            ((grep("Initials", new_lifespan_ods[row_,]))+1):ncol(new_lifespan_ods)]))))
              researchers <- researchers[researchers != "NA"]
              researchers <- researchers[!is.na(researchers)]
              researchers <- researchers[researchers != ""]
              break
            }
          }
        } else {
          researchers = NULL
        }
        
        for (row_ in 1:nrow(new_lifespan_ods)){
          if (sum(grepl("Plate", new_lifespan_ods[row_,]))){
            colnames(new_lifespan_ods) <- new_lifespan_ods[row_,]
            new_lifespan_ods <- new_lifespan_ods[row_+1:nrow(new_lifespan_ods),2:ncol(new_lifespan_ods)]
            break
          }
        }
        
        for (row_ in 5:nrow(new_lifespan_ods)){
          if (is.na(new_lifespan_ods[row_, 1])){
            new_lifespan_ods <- new_lifespan_ods[1:row_-1,]
            break
          }
        }

        for (row_ in rownames(new_lifespan_ods)){
          if (censored_plate %in% new_lifespan_ods[row_,]){
            new_lifespan_ods <- new_lifespan_ods[rownames(new_lifespan_ods) != row_,]
          }
        }  
        
        #now convert the inputted counts to something we can graph
        for (day in 2:ncol(new_lifespan_ods)){
          if (is.na(new_lifespan_ods[1,day]) == FALSE){
            day_ <- day - 1
            
            dead <- as.numeric(na.omit(new_lifespan_ods[new_lifespan_ods$Day == "Dead", day]))
            if (length(dead) > 0) {
              dead <- sum(dead)
            } else {
              dead <- 0
            }
            
            censored <- as.numeric(na.omit(new_lifespan_ods[grepl("Missing", new_lifespan_ods$Day), day]))
            if (length(censored) > 0) {
              censored <- sum(censored)
            } else {
              censored <- 0
            }
            
            data_summary_ <- data.frame("Day" = day_, "Dead" = dead, "Censored" = censored, 
                                        "Bacteria" = bacteria, "Strain" = strain, "Treatment" = treatment,
                                        "Temperature" = temperature)
            data_summary_ <- na.omit(data_summary_)
            data_summary <- rbind(data_summary, data_summary_)
          }
        }
      }
      
      data_summary$label_ <- paste0(data_summary$Bacteria, data_summary$Strain, data_summary$Treatment)
      
      ####
      # write.csv(file = "raw_counts_alive_censored_day.csv", x = data_summary)
      worm_res <- data.frame("Day" = c(), "status" = c(), "Strain" = c(),
                             "Treatment" = c(), "Bacteria" = c())
      
      for (condition in unique(data_summary$label_)){
        data_summary_ <- data_summary[data_summary$label_ == condition,]
        for (day_ in data_summary_$Day){
          recorded = FALSE
          censored <- data_summary_$Censored[data_summary_$Day == day_]
          strain <- data_summary_$Strain[data_summary_$Day == day_]
          treatment<- data_summary_$Treatment[data_summary_$Day == day_]
          bacteria<- data_summary_$Bacteria[data_summary_$Day == day_]
          if (censored > 0){
            worm_res_ <- data.frame("Day" = rep(day_, censored),
                                    "status" = rep(0, censored),
                                    "Strain" = rep(strain, censored),
                                    "Treatment" = rep(treatment, censored),
                                    "Bacteria" = rep(bacteria, censored))
            recorded = TRUE
          }
          dead <- data_summary_$Dead[data_summary_$Day == day_]
          if (dead > 0){
            worm_res_ <- data.frame("Day" = rep(day_, dead),
                                    "status" = rep(1, dead),
                                    "Strain" = rep(strain, dead),
                                    "Treatment" = rep(treatment, dead),
                                    "Bacteria" = rep(bacteria, dead))
            recorded = TRUE
          }
          if (recorded) {
            worm_res <- rbind(worm_res, worm_res_)
          }
        }
      }
      
      # calculating some of the stuff
      worm_res$Day <- as.numeric(worm_res$Day)
      worm_res$status <- as.numeric(worm_res$status)
      
      #calculating N and mean WLS
      worm_res$label_ <- paste0(worm_res$Strain, worm_res$Treatment, worm_res$Bacteria)
      for (condition in unique(worm_res$label_)) {
        N_worms <- nrow(worm_res[worm_res$status == 1 & worm_res$label_ == condition,])
        MeanWLS <- mean(worm_res$Day[worm_res$status == 1 & worm_res$label_ == condition])
        worm_res$N_worms[worm_res$label_ == condition] <- N_worms
        worm_res$MeanWLS[worm_res$label_ == condition] <- MeanWLS
      }
      lifespan_data <- worm_res

      # let's get the labeling for the type of experiment going
      control_found = FALSE
      if (lifespan_type == "RNAi"){
        lifespan_data$lifespan_type <- lifespan_data$Bacteria

        for (i in empty_vector){
          if (i %in% lifespan_data$Bacteria) {
            control <- i
            control_found = TRUE
          }
        }

      } else if (lifespan_type == "Genotype"| lifespan_type == "Strain") {
        lifespan_data$lifespan_type <- "" #empty bcs all this is used for is to create the label below

        for (i in Wild_type){
          if (i %in% lifespan_data$Strain) {
            control <- i
            control_found = TRUE
          }
        }


      } else if (lifespan_type == "Drug" | lifespan_type == "Treatment"){
        lifespan_data$lifespan_type <- lifespan_data$Treatment

        for (i in Vehicle){
          if (i %in% lifespan_data$Treatment) {
            control <- i
            control_found = TRUE
          }
        }

      } else {
        setwd(starting_dir)
        return("Error. Did you put in the correct lifespan_type keyword argument?")
      }

      if (control_found == FALSE){
        setwd(starting_dir)
        return("Error. Did you put in the correct empty_vector keyword argument?")
      }

      if (lifespan_type == "Genotype") {
        #create the label that is going to be used for the plots, without the astricks, which will be added in a second.
        lifespan_data$label_ <- paste0(lifespan_data$Strain, " (", round(lifespan_data$MeanWLS,1), ", ", lifespan_data$N_worms, ")")
      } else {
        #create the label that is going to be used for the plots, without the astricks, which will be added in a second.
        lifespan_data$label_ <- paste0(lifespan_data$Strain, " + ", lifespan_data$lifespan_type, " (", round(lifespan_data$MeanWLS,1), ", ", lifespan_data$N_worms, ")")
      }


      # save the data out as a csv option
      if (return_data){
        setwd(starting_dir)
        return(lifespan_data)
      }
      if (writeout_data){
        write.csv(lifespan_data, "lifespan_data.csv")
      }

    #STATS TIME

    # Conducting the statistics in comparison to the control for each sample
    lifespan_data$asterisk <- ""
    for (experimental_condition in (unique(lifespan_data$label_[grepl(paste0(control), lifespan_data$label_)==F]))){
      for_stats <- lifespan_data[lifespan_data$label_ == paste(unique(lifespan_data$label_[grepl(paste0(control), lifespan_data$label_)])) |
                         lifespan_data$label_ ==  experimental_condition,]

      # use the log rank stats test that is able to use censored data
      # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#Comparing_survival_times_between_groups
      stats_result <- survdiff(Surv(Day, status) ~ label_, data = for_stats)
      
      stats_result_out <- data.frame("label" = experimental_condition, "p-value" = stats_result$pvalue, "")
      
      if (dir.exists("stats") == FALSE) {
        dir.create("stats")
      }
      
      write.csv(x = capture.output(stats_result), paste0("stats/",gsub("<","", gsub("/", "", gsub("i>", "",experimental_condition))), ".csv"))
      
      stats_pval <- stats_result$pvalue
      if (stats_pval < 0.001){
        lifespan_data$asterisk[lifespan_data$label_ ==  experimental_condition] <- "***"
      } else if (stats_pval < 0.01){
        lifespan_data$asterisk[lifespan_data$label_ ==  experimental_condition] <- "**"
      } else if (stats_pval < 0.05) {
        lifespan_data$asterisk[lifespan_data$label_ ==  experimental_condition] <- "*"
      }
    }

    # Changing the label to represent the corresponding asterisk
    lifespan_data$label <- paste0(lifespan_data$label_, lifespan_data$asterisk)

    if (lifespan_type == "Treatment" | lifespan_type == "Drug"){
      #order the conditions to always lead with the control (and it will be black in the plot_out)
      # for treatment conditions, we want to sort by the concentration value and lead with the control
      lifespan_data$label <- factor(lifespan_data$label, levels = c(paste(unique(lifespan_data$label[grepl(paste0(control), lifespan_data$label)])),
                                                paste(unique(lifespan_data$label[grepl(paste0(control), lifespan_data$label)==F]))))

      lifespan_data <- lifespan_data[order(lifespan_data$label),]
    } else {
      #order the conditions to always lead with the control (and it will be black in the plot_out)
      # in genotype and RNAi lifespans, we will just sort by if the sample was found to have a significant difference in lifespan
      lifespan_data$label <- factor(lifespan_data$label, levels = c(paste(unique(lifespan_data$label[grepl(paste0(control), lifespan_data$label)])),
                                                paste(unique(lifespan_data$label[grepl(")\\*\\*\\*$", lifespan_data$label)])),
                                                paste(unique(lifespan_data$label[grepl(")\\*\\*$", lifespan_data$label)])),
                                                paste(unique(lifespan_data$label[grepl(")\\*$", lifespan_data$label)])),
                                                paste(unique(lifespan_data$label[grepl(paste0(control), lifespan_data$label)==F & grepl("\\*", lifespan_data$label)==F]))))

      lifespan_data <- lifespan_data[order(lifespan_data$label),]
    }

    #fitting the kap meir survival curve
    km_fit <- survfit(Surv(Day, status) ~ label, data=lifespan_data)

    if (Temperature){
      if (document_researchers){
        caption_ <- paste0("*p<0.05, **p<0.01, ***p<0.001.\n",
                           "Temperature: ", temperature, "\n",
                           "Researchers: ", paste(researchers,collapse=', '),".")
      } else {
        caption_ <- paste0("*p<0.05, **p<0.01, ***p<0.001.\n",
                           "Temperature: ", temperature,".")
      }
    } else if (document_researchers) {
      caption_ <- paste0("*p<0.05, **p<0.01, ***p<0.001.\n",
                         "Researchers: ", paste(researchers,collapse=', '),".")
    } else {
      caption_ <- "*p<0.05, **p<0.01, ***p<0.001."
    }
    
    if (dir.exists("plots") == FALSE) {
      dir.create("plots")
    }


    plot_out <- autoplot(km_fit,conf.int=FALSE,surv.geom='line',surv.size=1,censor=FALSE) +
      ggtitle(paste(plot_title)) +
      labs(caption = paste(caption_))+
      ylab("Percent alive")+
      xlab("Days of adulthood")+
      scale_color_manual("Sample (mean lifespan, N)",
                         values = col_list) +
      scale_y_continuous(expand = c(0,0), limits = c(0,1.05), labels=scales::percent)+
      scale_x_continuous(expand = c(0,0), limits = c(0,(max(round(max(lifespan_data$Day)+1, -1), 25)))) +
      theme(axis.text.x = element_text(angle=0),
            plot.title = element_markdown(family = "sans", size =30, hjust = 0.5, color="black"), # Title size and font.
            plot.caption = element_text(family = "sans", size = 18, color="black"), # Subtitle size and font.
            axis.text = element_text(family = "sans", size = 18, color="#303030", face='bold'), # Size and font of x and y values.
            axis.title = element_text(family = "sans", size = 25, color="black", face="bold"), # Size and font of x and y axes.
            panel.border = element_blank(),
            axis.line = element_line(colour = "black", linewidth = 1), #element_rect(colour = "black", fill = NA, size = 1), # Black border around the plot area.
            axis.ticks = element_line(colour = "black", linewidth = 1), # Style of x and y ticks.
            legend.key.size = unit(1, 'cm'),
            legend.key = element_rect(fill=NA),
            legend.text = element_markdown(family = "sans", size = 20),
            legend.title = element_text(family = "sans", face = "bold", size = 20),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            legend.text.align =	0,
            panel.background = element_blank(),
            panel.grid.major = element_line(colour = "lightgrey"),
            panel.grid.minor = element_line(colour = "lightgrey")#,
            # legend.position = c(0.85, 0.85)
      )

    #saving out the plots and writing it out
    svglite(paste0("plots/", plot_title, ".svg"), fix_text_size = F, width = 14)
    Sys.sleep(0.1)
    print(plot_out)
    Sys.sleep(0.1)
    dev.off()
    Sys.sleep(0.1)

    if (plot_individual_lifespans_to_control){
      #now we must plot the significant ones one by one
      result_significant <- lifespan_data[lifespan_data$asterisk %in% c("*","**","***") |
                                  grepl(paste0(control), lifespan_data$label),]
      i = 0
      for (label_significant_lifespan in unique(result_significant$label[grepl(paste0(control), result_significant$label)==F]))  {
        i <- i + 1

        #fitting the kap meir survival curve
        km_fit <- survfit(Surv(Day, status) ~ label, data=result_significant[result_significant$label == label_significant_lifespan |
                                                                               grepl(paste0(control), result_significant$label),])

        plot_out <- autoplot(km_fit,conf.int=FALSE,surv.geom='line',surv.size=1,censor=FALSE) +
          ggtitle("") +
          labs(caption = paste0(caption_))+
          ylab("Percent alive")+
          xlab("Days of adulthood")+
          scale_color_manual("Sample (mean lifespan, N)",
                             values = c(col_list[1], col_list[i+1])) +
          scale_y_continuous(expand = c(0,0), limits = c(0,1.05), labels=scales::percent)+
          scale_x_continuous(expand = c(0,0), limits = c(0,(max(round(max(lifespan_data$Day)+1, -1), 25)))) +
          theme(axis.text.x = element_text(angle=0),
                plot.title = element_markdown(family = "sans", size =30, hjust = 0.5, color="black"), # Title size and font.
                plot.caption = element_text(family = "sans", size = 18, color="black"), # Subtitle size and font.
                axis.text = element_text(family = "sans", size = 18, color="#303030", face='bold'), # Size and font of x and y values.
                axis.title = element_text(family = "sans", size = 25, color="black", face="bold"), # Size and font of x and y axes.
                panel.border = element_blank(),
                axis.line = element_line(colour = "black", linewidth = 1), #element_rect(colour = "black", fill = NA, size = 1), # Black border around the plot area.
                axis.ticks = element_line(colour = "black", linewidth = 1), # Style of x and y ticks.
                legend.key.size = unit(1, 'cm'),
                legend.key = element_rect(fill=NA),
                legend.text = element_markdown(family = "sans", size = 20),
                legend.title = element_text(family = "sans", face = "bold", size = 20),
                legend.background = element_blank(),
                legend.box.background = element_blank(),
                legend.text.align =	0,
                panel.background = element_blank(),
                panel.grid.major = element_line(colour = "lightgrey"),
                panel.grid.minor = element_line(colour = "lightgrey")#,
                # legend.position = c(0.85, 0.85)
          )

        plot_title <- paste0("SIG-", sub("/", "", directory), "-", i)

        #saving out the plots and writing it out
        svglite(paste0("plots/", plot_title,".svg"), fix_text_size = F, width = 14)
        Sys.sleep(0.1)
        print(plot_out)
        Sys.sleep(0.1)
        dev.off()
        Sys.sleep(0.1)
      }
    }
  }
  } else {
    starting_dir <- processed_dir_path

    for (processed_file in list.files(processed_dir_path, ".csv")){
      print(processed_file)

      #### skip to here if passing in a processed data with rows representing each worm in the lifespan, labeled appropriately
      lifespan_data <- read.csv(file.path(processed_dir_path, processed_file))

      plot_title = sub(".csv", "", paste0(processed_file)) #Title of the plot. Default is to grab the last directory in the working directory.

      # let's get the labeling for the type of experiment going
      # let's get the labeling for the type of experiment going
      control_found = FALSE
      if (lifespan_type == "RNAi"){
        lifespan_data$lifespan_type <- lifespan_data$Bacteria

        for (i in empty_vector){
          if (i %in% lifespan_data$Bacteria) {
            control <- i
            control_found = TRUE
          }
        }

      } else if (lifespan_type == "Genotype") {
        lifespan_data$lifespan_type <- "" #empty bcs all this is used for is to create the label below

        for (i in Wild_type){
          if (i %in% lifespan_data$Genotype) {
            control <- i
            control_found = TRUE
          }
        }

      } else if (lifespan_type == "Drug" | lifespan_type == "Treatment"){
        lifespan_data$lifespan_type <- lifespan_data$Treatment

        for (i in Vehicle){
          if (i %in% lifespan_data$Treatment) {
            control <- i
            control_found = TRUE
          }
        }

      } else {
        setwd(starting_dir)
        return("Error. Did you put in the correct lifespan_type keyword argument?")
      }

      if (control_found == FALSE){
        setwd(starting_dir)
        return("Error. Did you put in the correct empty_vector keyword argument?")
      }

      #STATS TIME

      # Conducting the statistics in comparison to the control for each sample
      lifespan_data$asterisk <- ""
      for (experimental_condition in (unique(lifespan_data$label_[grepl(paste0(control), lifespan_data$label_)==F]))){
        for_stats <- lifespan_data[lifespan_data$label_ == paste(unique(lifespan_data$label_[grepl(paste0(control), lifespan_data$label_)])) |
                                     lifespan_data$label_ ==  experimental_condition,]

        # use the log rank stats test that is able to use censored data
        # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#Comparing_survival_times_between_groups
        stats_result <- survdiff(Surv(Day, status) ~ label_, data = for_stats)
        
        write.csv(x = capture.output(stats_result), paste0(gsub("<","", gsub("/", "", gsub("i>", "",experimental_condition))), ".csv"))
        
        stats_pval <- stats_result$pvalue
        if (stats_pval < 0.001){
          lifespan_data$asterisk[lifespan_data$label_ ==  experimental_condition] <- "***"
        } else if (stats_pval < 0.01){
          lifespan_data$asterisk[lifespan_data$label_ ==  experimental_condition] <- "**"
        } else if (stats_pval < 0.05) {
          lifespan_data$asterisk[lifespan_data$label_ ==  experimental_condition] <- "*"
        }
      }

      # Changing the label to represent the corresponding asterisk
      lifespan_data$label <- paste0(lifespan_data$label_, lifespan_data$asterisk)

      if (lifespan_type == "Treatment" | lifespan_type == "Drug"){
        #order the conditions to always lead with the control (and it will be black in the plot_out)
        # for treatment conditions, we want to sort by the concentration value and lead with the control
        lifespan_data$label <- factor(lifespan_data$label, levels = c(paste(unique(lifespan_data$label[grepl(paste0(control), lifespan_data$label)])),
                                                                      paste(unique(lifespan_data$label[grepl(paste0(control), lifespan_data$label)==F]))))

        lifespan_data <- lifespan_data[order(lifespan_data$label),]
      } else {
        #order the conditions to always lead with the control (and it will be black in the plot_out)
        # in genotype and RNAi lifespans, we will just sort by if the sample was found to have a significant difference in lifespan
        lifespan_data$label <- factor(lifespan_data$label, levels = c(paste(unique(lifespan_data$label[grepl(paste0(control), lifespan_data$label)])),
                                                                      paste(unique(lifespan_data$label[grepl(")\\*\\*\\*$", lifespan_data$label)])),
                                                                      paste(unique(lifespan_data$label[grepl(")\\*\\*$", lifespan_data$label)])),
                                                                      paste(unique(lifespan_data$label[grepl(")\\*$", lifespan_data$label)])),
                                                                      paste(unique(lifespan_data$label[grepl(paste0(control), lifespan_data$label)==F & grepl("\\*", lifespan_data$label)==F]))))

        lifespan_data <- lifespan_data[order(lifespan_data$label),]
      }

      #fitting the kap meir survival curve
      km_fit <- survfit(Surv(Day, status) ~ label, data=lifespan_data)

      plot_out <- autoplot(km_fit,conf.int=FALSE,surv.geom='line',surv.size=1,censor=FALSE) +
        ggtitle(paste(plot_title)) +
        labs(caption = paste0(caption_))+
        ylab("Percent alive")+
        xlab("Days of adulthood")+
        scale_color_manual("Sample (mean lifespan, N)",
                           values = col_list) +
        # scale_y_continuous(expand = c(0,0), limits = c(0,1.05), breaks = c(0,.25,.50,.75,1))+
        scale_x_continuous(expand = c(0,0), limits = c(0,(max(round(max(lifespan_data$Day)+1, -1), 25)))) +
        theme(axis.text.x = element_text(angle=0),
              plot.title = element_markdown(family = "sans", size =30, hjust = 0.5, color="black"), # Title size and font.
              plot.caption = element_text(family = "sans", size = 18, color="black"), # Subtitle size and font.
              axis.text = element_text(family = "sans", size = 18, color="#303030", face='bold'), # Size and font of x and y values.
              axis.title = element_text(family = "sans", size = 25, color="black", face="bold"), # Size and font of x and y axes.
              panel.border = element_blank(),
              axis.line = element_line(colour = "black", linewidth = 1), #element_rect(colour = "black", fill = NA, size = 1), # Black border around the plot area.
              axis.ticks = element_line(colour = "black", linewidth = 1), # Style of x and y ticks.
              legend.key.size = unit(1, 'cm'),
              legend.key = element_rect(fill=NA),
              legend.text = element_markdown(family = "sans", size = 20),
              legend.title = element_text(family = "sans", face = "bold", size = 20),
              legend.background = element_blank(),
              legend.box.background = element_blank(),
              legend.text.align =	0,
              panel.background = element_blank(),
              panel.grid.major = element_line(colour = "lightgrey"),
              panel.grid.minor = element_line(colour = "lightgrey")#,
              # legend.position = c(0.85, 0.85)
        )

      #saving out the plots and writing it out
      svglite(file.path(processed_dir_path, paste0(plot_title, ".svg")), fix_text_size = F, width = 14)
      Sys.sleep(0.1)
      print(plot_out)
      Sys.sleep(0.1)
      dev.off()

      if (plot_individual_lifespans_to_control){
        #now we must plot the significant ones one by one
        result_significant <- lifespan_data[lifespan_data$asterisk %in% c("*","**","***") |
                                              grepl(paste0(control), lifespan_data$label),]

        i = 0
        for (label_significant_lifespan in unique(result_significant$label[grepl(paste0(control), result_significant$label)==F]))  {
          i <- i + 1
          #fitting the kap meir survival curve
          km_fit <- survfit(Surv(Day, status) ~ label, data=result_significant[result_significant$label == label_significant_lifespan |
                                                                                 grepl(paste0(control), result_significant$label),])

          plot_out <- autoplot(km_fit,conf.int=FALSE,surv.geom='line',surv.size=1,censor=FALSE) +
            ggtitle( if (lifespan_type == "Genotype") {
              paste(result_significant$Genotype[result_significant$label == label_significant_lifespan])
            } else if (lifespan_type == "RNAi") {
              paste(result_significant$Bacteria[result_significant$label == label_significant_lifespan])
            } else if (lifespan_type %in% c("Drug", "Treatment")){
              paste(result_significant$Treatment[result_significant$label == label_significant_lifespan])
            } else {
              paste0("")
            }
            ) +
            labs(caption = paste0("*p<0.05, **p<0.01, ***p<0.001.\n",
                                  "Temperature: ", temperature, "\n",
                                  "Researchers: ", paste(researchers,collapse=', '),"."))+
            ylab("Percent alive")+
            xlab("Days of adulthood")+
            scale_color_manual("Sample (mean lifespan, N)",
                               values = c(col_list[1], col_list[i+1])) +
            # scale_y_continuous(expand = c(0,0), limits = c(0,1.05), breaks = c(0,.25,.50,.75,1))+
            scale_x_continuous(expand = c(0,0), limits = c(0,(max(round(max(result_significant$Day)+1, -1), 25)))) +
            theme(axis.text.x = element_text(angle=0),
                  plot.title = element_markdown(family = "sans", size =30, hjust = 0.5, color="black"), # Title size and font.
                  plot.caption = element_text(family = "sans", size = 18, color="black"), # Subtitle size and font.
                  axis.text = element_text(family = "sans", size = 18, color="#303030", face='bold'), # Size and font of x and y values.
                  axis.title = element_text(family = "sans", size = 25, color="black", face="bold"), # Size and font of x and y axes.
                  panel.border = element_blank(),
                  axis.line = element_line(colour = "black", linewidth = 1), #element_rect(colour = "black", fill = NA, size = 1), # Black border around the plot area.
                  axis.ticks = element_line(colour = "black", linewidth = 1), # Style of x and y ticks.
                  legend.key.size = unit(1, 'cm'),
                  legend.key = element_rect(fill=NA),
                  legend.text = element_markdown(family = "sans", size = 20),
                  legend.title = element_text(family = "sans", face = "bold", size = 20),
                  legend.background = element_blank(),
                  legend.box.background = element_blank(),
                  legend.text.align =	0,
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "lightgrey"),
                  panel.grid.minor = element_line(colour = "lightgrey")#,
                  # legend.position = c(0.85, 0.85)
            )

          #saving out the plots and writing it out
          svglite(file.path(processed_dir_path, paste0(plot_title, "-SIG-", i,".svg")), fix_text_size = F, width = 14)
          Sys.sleep(0.1)
          print(plot_out)
          Sys.sleep(0.1)
          dev.off()
        }
      }
    }
  setwd(starting_dir)
  }
}

