#' Plot WLS manually
#'
#' @param lifespan_counts A matrix with the lifespan data
#' @param empty_vector If RNAi experiment, this is the empty vector.
#' @param Vehicle If drug experiment, what was the control, which would be used for stats.
#' @param Wild_type If Genotype experiment, what was the control, which would be used for stats description
#' @param rosettastone_index_colnumber column number of the plate annotation in the rosetta_stone_file
#' @param rosetta_stone_platenumber_replacement if there is a string to delete in the rosetta_stone file for plate column, put here
#' @param lifespan_counts_platenumber_replacement if there is a string to delete in the lifespan_counts file for plate column, put here
#' @param lifespancounts_index_colnumber column number of the plate annotation in the counts_file
#' @param lifespancounts_count_colnumber colnumber where the counts are recorded in the lifespan_counts, if the 'Count" column is not labeled
#' @param convert_from_epoch_time The timestamp is defaul epoch time in the counts_file
#' @param processed_dir_path path to save the processed file
#' @return Lifespan data to be plotted
#' @export WLS_convert_counts
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
#
# This function was written by Blaise Mariner to convert alive worms detected on a given day to convert to a plot-able dataframe
#

WLS_convert_counts <- function (lifespan_type = "Genotype",
                                counts_file = "counts.csv",
                                rosetta_stone_file = "rosetta_stone.csv",
                                rosettastone_index_colnumber = 1,
                                lifespancounts_index_colnumber = 1,
                                rosetta_stone_platenumber_replacement = "",
                                lifespan_counts_platenumber_replacement = "",
                                lifespancounts_count_colnumber = 6,
                                convert_from_epoch_time = TRUE,
                                processed_dir_path = FALSE, #this is where to put the path for the processed files to be saved
                                empty_vector = "L4440", #If RNAi experiment, this is the empty vector
                                Vehicle = "Vehicle", #If drug experiment, what was the control, which would be used for stats
                                Wild_type = "N2" #If Genotype experiment, what was the control, which would be used for stats
){
    # in case you want to convert from the timestamp to datatime:
    # lifespan_counts$datetime <- lubridate::as_datetime(lifespan_counts$Timestamp)
    lifespan_counts <- as.data.frame(read.csv(counts_file))
    rosetta_stone <- as.data.frame(read.csv(rosetta_stone_file))

    if ("plate" %in% colnames(lifespan_counts) == FALSE) {
      lifespan_counts$plate <- sub(pattern = lifespan_counts_platenumber_replacement, replacement = "", x = lifespan_counts[,lifespancounts_index_colnumber])
    }

    if ("plate" %in% colnames(rosetta_stone) == FALSE) {
      rosetta_stone$plate <- sub(pattern = rosetta_stone_platenumber_replacement, replacement = "", x = rosetta_stone[,rosettastone_index_colnumber])
    }

    if ("Lifespan_Number" %in% colnames(rosetta_stone) == FALSE) {
      if ("Lifespan.Number"%in% colnames(rosetta_stone)) {
        rosetta_stone$Lifespan_Number <- rosetta_stone$Lifespan.Number
      }
    }

    if ("Count" %in% colnames(lifespan_counts) == FALSE) {
      lifespan_counts$Count <- lifespan_counts[,lifespancounts_count_colnumber]
    }

    for (plate_ in lifespan_counts$plate){
      if (plate_ %in% rosetta_stone$plate == FALSE){
        return(paste0("ERROR: ", plate_, " exists in the lifespan_counts_file but not the rosetta_stone_file"))
      }
    }

    censored_plates <- rosetta_stone$Censor.plate.[rosetta_stone$plate == plate_] == "yes"
    lifespan_counts <- lifespan_counts[lifespan_counts$plate %in% censored_plates == FALSE,]

    #transfer the plate information from the rossetta_stone variable to the lifespan_counts matrix
    for (plate_ in unique(lifespan_counts$plate)) {
      lifespan_number <- rosetta_stone$Lifespan_Number[rosetta_stone$plate == plate_]
      worm_strain <- rosetta_stone$Worm.Strain[rosetta_stone$plate == plate_]

      #make the strain italic if it is a gene name
      if (grepl("[a-z]{3}-[0-9]{1}", worm_strain) |
          grepl("[a-z]{4}-[0-9]{1}", worm_strain) |
          grepl("[a-z]{3}-[0-9]{2}", worm_strain) |
          grepl("[a-z]{4}-[0-9]{2}", worm_strain)) {
        worm_strain <- paste0("<i>", worm_strain, "</i>") # Markdown formatting for italics
      }

      bacteria <- rosetta_stone$Bacteria[rosetta_stone$plate == plate_]
      treatment <- rosetta_stone$Treatment[rosetta_stone$plate == plate_]
      date_eggs_picked <- rosetta_stone$Date.Eggs.Picked[rosetta_stone$plate == plate_]
      date_fudr <- rosetta_stone$Date.FUDR.Added[rosetta_stone$plate == plate_]
      date_adulthood <- rosetta_stone$Date.of.Adulthood[rosetta_stone$plate == plate_]
      temperature <- rosetta_stone$Temp[rosetta_stone$plate == plate_]

      lifespan_counts$Lifespan_number[lifespan_counts$plate == plate_] <- lifespan_number
      lifespan_counts$Genotype[lifespan_counts$plate == plate_] <- worm_strain
      lifespan_counts$Bacteria[lifespan_counts$plate == plate_] <- bacteria
      lifespan_counts$Treatment[lifespan_counts$plate == plate_] <- treatment
      lifespan_counts$Date_eggs_picked[lifespan_counts$plate == plate_] <- date_eggs_picked
      lifespan_counts$Date_fudr[lifespan_counts$plate == plate_] <- date_fudr
      lifespan_counts$Date_adulthood[lifespan_counts$plate == plate_] <- date_adulthood
      lifespan_counts$Temperature[lifespan_counts$plate == plate_] <- temperature
    }

    for (lifespan_no in unique(lifespan_counts$Lifespan_number)){
      lifespan_counts$temp_label <- paste0(lifespan_counts$Genotype, lifespan_counts$Treatment, lifespan_counts$Bacteria, lifespan_counts$Lifespan_number)
      }

    converted <- lifespan_counts[0,]
    if (convert_from_epoch_time) {
      for (lifespan_no in unique(lifespan_counts$Lifespan_number)){
        lifespan_of_interest <- lifespan_counts[lifespan_counts$Lifespan_number == lifespan_no,]
        for (unique_condition in unique(lifespan_of_interest$temp_label)) {
          unique_condition_lifespan_of_interest <- lifespan_of_interest[lifespan_of_interest$temp_label == unique_condition,]
          start_day <- min(unique_condition_lifespan_of_interest$Timestamp)
          unique_condition_lifespan_of_interest$Start_day <- start_day
          unique_condition_lifespan_of_interest$Day[unique_condition_lifespan_of_interest$temp_label == unique_condition] <- ceiling(((unique_condition_lifespan_of_interest$Timestamp) - as.numeric(unique_condition_lifespan_of_interest$Start_day))/86400)
          converted <- rbind(converted, unique_condition_lifespan_of_interest)
        }
      }
      lifespan_counts <- converted
    }

    #the spreadsheet must provide a column that quantifies the worms missing that day (M_all)
    if ("M_all" %in% colnames(lifespan_counts) == FALSE) {
      # M_all is the column where the censored worms are documented (M = missing, but also includes explosions and other censored information)
      lifespan_counts$M_all <- 0 ### THIS IS WHERE WE MAY EDIT THE SCRIPT IN ORDER TO PASS IN CENSORED INFORMATION TO THE SCRIPT!!!! RIGHT NOW, THIS PROCESSING DOES NOT INCLUDE CENSORING
    }

    # we now can get the worm data information for which day each worm died in each lifespan
    for (lifespan_no in unique(lifespan_counts$Lifespan_number)){
      lifespan_data <- data.frame("Day" = c(), "status" = c(), "Genotype" = c(),
                             "Treatment"=c(), "Bacteria"=c(), "N_worms" = c(), "MeanWLS" = c())
      lifespan_of_interest <- lifespan_counts[lifespan_counts$Lifespan_number == lifespan_no,]

      for (unique_condition in unique(lifespan_of_interest$temp_label)){
        lifespan_of_interest_unique_condition <- lifespan_of_interest[lifespan_of_interest$temp_label == unique_condition,]

        worm_res <- data.frame("Day" = c(), "status" = c(), "Genotype" = c(),
                               "Treatment"=c(), "Bacteria"=c())

        for (day_ in sort(unique(lifespan_of_interest_unique_condition$Day))){
          if (day_ > 1) {

            change_in_worm_count <- prev_day_alive_count - max(lifespan_of_interest_unique_condition$Count[lifespan_of_interest_unique_condition$Day == day_])

            if (change_in_worm_count > 0) {
              censored <- min(lifespan_of_interest_unique_condition$M_all[lifespan_of_interest_unique_condition$Day == day_])

              strain <- unique(lifespan_of_interest_unique_condition$Genotype)
              treatment <- unique(lifespan_of_interest_unique_condition$Treatment)
              bacteria <- unique(lifespan_of_interest_unique_condition$Bacteria)

              if (censored > 0){
                worm_res_ <- data.frame("Day" = rep(day_, censored),
                                        "status" = rep(0, censored),
                                        "Genotype" = rep(strain, censored),
                                        "Treatment" = rep(treatment, censored),
                                        "Bacteria" = rep(bacteria, censored))
                worm_res <- rbind(worm_res, worm_res_)
              }

              dead <- change_in_worm_count - censored

              if (dead > 0){
                worm_res_ <- data.frame("Day" = rep(day_, dead),
                                        "status" = rep(1, dead),
                                        "Genotype" = rep(strain, dead),
                                        "Treatment" = rep(treatment, dead),
                                        "Bacteria" = rep(bacteria, dead))
                worm_res <- rbind(worm_res, worm_res_)
              }
              prev_day_alive_count <- max(lifespan_of_interest_unique_condition$Count[lifespan_of_interest_unique_condition$Day == day_])
            }
          } else if (day_ == 1) {
            prev_day_alive_count <- max(lifespan_of_interest_unique_condition$Count[lifespan_of_interest_unique_condition$Day == day_])
          }
        }

        if (nrow(worm_res) > 0){
          worm_res$Day <- as.numeric(worm_res$Day)
          worm_res$status <- as.numeric(worm_res$status)
          worm_res$N_worms <- nrow(worm_res[worm_res$status == 1,])
          worm_res$MeanWLS <- mean(worm_res$Day[worm_res$status==1])

          lifespan_data <- rbind(lifespan_data, worm_res)
        }
      }

      # let's get the labeling for the type of experiment going
      if (lifespan_type == "RNAi"){
        lifespan_data$lifespan_type <- lifespan_data$Bacteria
        control <- empty_vector
        if (sum(grepl(empty_vector, lifespan_data$Bacteria)) == 0){
          return("Error. Did you put in the correct empty_vector keyword argument?")
        }
      } else if (lifespan_type == "Genotype") {
        lifespan_data$lifespan_type <- "" #empty bcs all this is used for is to create the label below
        control <- Wild_type
        if (sum(grepl(Wild_type, lifespan_data$Genotype)) == 0){
          return("Error. Did you put in the correct Wild_type keyword argument?")
        }
      } else if (lifespan_type == "Drug" | lifespan_type == "Treatment"){
        lifespan_data$lifespan_type <- lifespan_data$Treatment
        control <- Vehicle
        if (sum(grepl(Vehicle, lifespan_data$Treatment)) == 0){
          return("Error. Did you put in the correct Vehicle keyword argument?")
        }
      } else {
        return("Error. Did you put in the correct lifespan_type keyword argument?")
      }

      if (lifespan_type == "Genotype") {
        #create the label that is going to be used for the plots, without the astricks, which will be added in a second.
        lifespan_data$label_ <- paste0(lifespan_data$Genotype, " (", round(lifespan_data$MeanWLS,1), ", ", lifespan_data$N_worms, ")")
      } else {
        #create the label that is going to be used for the plots, without the astricks, which will be added in a second.
        lifespan_data$label_ <- paste0(lifespan_data$Genotype, " + ", lifespan_data$lifespan_type, " (", round(lifespan_data$MeanWLS,1), ", ", lifespan_data$N_worms, ")")
      }

      if (processed_dir_path == FALSE) {
        processed_dir_path = file.path(getwd(), "results_counts_converted")
      }
      if (!file.exists(processed_dir_path)) {
        dir.create(processed_dir_path, showWarnings = FALSE)
      }
      write.csv(x = lifespan_data, file = file.path(processed_dir_path, paste0("Lifespan_", lifespan_no, ".csv")))
    }
  }
