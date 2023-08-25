#' Plot WLS manually
#'
#' @param lifespan_data A matrix with the lifespan data
#' @param lifespan_type The type of lifespan experiment. Accepted: "RNAi", "Treatment", "Drug", "Genotype"
#' @param stats If true, the function will plot the lifespan data with appropriate wilcoxon stats if given the correct label control in the lifespan_data
#' @param control The comparison used as a control if stats == TRUE
#' @param plot_individual_lifespans_to_control If TRUE and stats = TRUE, write-out individual lifespans in comparison to the input control
#' @param col_list color list to be used in plotting the lifespan
#' @param plot_title Title of the plot
#' @param researchers who the researchers are for the experiment. default = c()
#' @param temperature input temperature value
#' @param x_upper_bound The x upper limit of the plot
#' @return Lifespan plot with manual inputs
#' @export WLS_manualplot
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
# This function was written by Blaise Mariner to graph C. elegans lifespans with automated labeling
# for questions regarding the script or reccomended improvements, please email Blaise at blaisemariner17@gmail.com
#

WLS_manualplot <- function (lifespan_data,
                            lifespan_type,
                            col_list,
                            plot_title,
                            researchers = c(),
                            x_upper_bound = 25,
                            plot_individual_lifespans_to_control = FALSE,
                            stats = FALSE,
                            control = "",
                            temperature = NA
){
    if ("label_" %in% colnames(lifespan_data) == FALSE){
      if ("label" %in% colnames(lifespan_data)){
        lifespan_data$label_ <- lifespan_data$label
      } else {
        return("Need `label` or `label_` to be a column name of lifespan_data, or else the label will not be made correctly.")
      }
    }

    # Conducting the statistics in comparison to the control for each sample
    lifespan_data$asterisk <- ""
    if (stats == TRUE){
          for (experimental_condition in (unique(lifespan_data$label_[grepl(paste0(control), lifespan_data$label_)==F]))){
            for_stats <- lifespan_data[lifespan_data$label_ == paste(unique(lifespan_data$label_[grepl(paste0(control), lifespan_data$label_)])) |
                               lifespan_data$label_ ==  experimental_condition,]

            # use the log rank stats test that is able to use censored data
            # https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#Comparing_survival_times_between_groups
            stats_pval <- survdiff(Surv(Day, status) ~ label_, data = for_stats)$pvalue
            if (stats_pval < 0.001){
              lifespan_data$asterisk[lifespan_data$label_ ==  experimental_condition] <- "***"
            } else if (stats_pval < 0.01){
              lifespan_data$asterisk[lifespan_data$label_ ==  experimental_condition] <- "**"
            } else if (stats_pval < 0.05) {
              lifespan_data$asterisk[lifespan_data$label_ ==  experimental_condition] <- "*"
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
      }
    } else {
      # Changing the label to represent the corresponding asterisk
      lifespan_data$label <- paste0(lifespan_data$label_, lifespan_data$asterisk)
    }

    #fitting the kap meir survival curve
    km_fit <- survfit(Surv(Day, status) ~ label, data=lifespan_data)

    plot_out_return <- autoplot(km_fit,conf.int=FALSE,surv.geom='line',surv.size=1,censor=FALSE) +
      ggtitle(paste(plot_title)) +
            labs(caption = paste0("*p<0.05, **p<0.01, ***p<0.001.\n",
                                  "Temperature: ", temperature, "\n",
                                  "Researchers: ", paste(researchers,collapse=', '),"."))+
      ylab("Percent alive")+
      xlab("Days of adulthood")+
      scale_color_manual("Sample (mean lifespan, N)",
                         values = col_list) +
      scale_y_continuous(expand = c(0,0), limits = c(0,1.05), labels=scales::percent)+
      scale_x_continuous(expand = c(0,0), limits = c(0,x_upper_bound)) +
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

    if (stats & plot_individual_lifespans_to_control){
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
          scale_y_continuous(expand = c(0,0), limits = c(0,1.05), breaks =, labels=scales::percent)+
          scale_x_continuous(expand = c(0,0), limits = c(0,x_upper_bound)) +
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

        plot_title <- paste0("SIG-", sub("/", "", plot_title), "-", i)

        #saving out the plots and writing it out
        svglite(paste0(plot_title,".svg"), fix_text_size = F, width = 14)
        Sys.sleep(0.1)
        print(plot_out)
        Sys.sleep(0.1)
        dev.off()
      }
    }
    return(plot_out_return)
  }


