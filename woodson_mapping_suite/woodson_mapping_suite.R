##############################
# This function plots maps and distribution plots to a PDF.
# Author: Rebecca Woodson Stubbs
# Written 2016
##############################

library(data.table)
library(ggplot2)
library(grid)
library(ggthemes)
library(maptools)
library(rje)
library(extrafont)
library(classInt)

# For more documentation and examples, see: https://rpubs.com/BeccaStubbs/woodson_examples
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

##############################################################################
# HISTOGRAM FUNCTION
##############################################################################
# This histogram function creates and returns a ggplot() object with
# a color scale dependent on the X value (default as all colors being grey)
# as well as the possibility for vertical lines that describe different
# properties of the distribution (mean, median, etc) added on top of the graph as well.

 histogram_colorstats<-function(datavector,
                                color_ramp="grey17",
                                minimum=NULL,
                                maximum=NULL,
                                color_value_breaks=NULL,
                                dist_stats=NULL,
                                mean_color="red",
                                quantile_color="black"){
   
   # Defining data/column based variables:
     if(is.null(minimum)){minumum<-min(datavector)}
     if(is.null(maximum)){maximum<-max(datavector)}

  # Creating the basic histogram; this will be returned if dist_stats==NULL
    hist<-ggplot() + 
      geom_histogram(aes(x=datavector, fill = ..x..), bins=30, na.rm = TRUE)+
      xlim(minimum, maximum)+
      scale_fill_gradientn(colours=rev(color_ramp), limits=c(minimum, maximum), values=color_value_breaks)+
      theme_tufte()+
      guides(fill=guide_colourbar(title=" ", barheight=0, barwidth=0, label=FALSE, ticks=FALSE))+
      labs(title="Distribution",x= "",y="")+
      theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adding distribution markers, if desired

if (!is.null(dist_stats)){  
  # Looking to see what was included in the dist_stats list:
    quantile_values<-suppressWarnings(as.numeric(dist_stats)) # Getting any numeric values within the list
    other_values<-dist_stats[is.na(quantile_values)] # Getting any character strings within the list
    quantile_values<-sort(quantile_values[!is.na(quantile_values)]) # Ordering the numeric values included within the list
  
  # Adding Mean and SD, if Requested 
  #-----------------------------------------------------------------------------------------------------
  # Checking to see if any weird/unrecognized inputs were entered wihthin the dist_stats list:
    rejected_statistics<-other_values[!(other_values %in% c("mean","sd"))]                                                                    
    # Printing if unrecognized, non-numeric things were entered
    if (length(rejected_statistics)>0){
      cat("The following statistics are not supported by this function: ")
      print(rejected_statistics)
      cat(" The statistics currently supported are either values between 0 and 1 (as quantiles), \n")
      cat(" which can be entered either as numerics ex: 0.25, or charecter strings (ex:'0.9'). In addition \n")
      cat(" you may enter 'mean' and 'sd' as options. No other statistics are currently supported. \n")
      cat(" If you believe these statistics should be included in the list of commonly-used options, \n")
      cat(" please contact Rebecca Stubbs for inclusion.")
    }
  
  distribution_statistics<-data.table(type=character(0),vals=numeric(0),Statistic=character(0))
  
  # Adding valid named statistics
  #---------------------------------------------------------------------------------------------------------
  # Including a line for the mean, if it is requested:
  if("mean" %in% other_values){
    distribution_statistics<-rbind(distribution_statistics,
                                   data.table(type="Mean",
                                              vals=mean(datavector),
                                              Statistic=paste0( "Mean [", round(mean(datavector),2), "]") ))}
  
  # Including lines for +/- 1 SD, if it is requested
  if("sd" %in% other_values){
    distribution_statistics<-rbind(distribution_statistics,
                                   data.table(type="1 Std. Dev",
                                              vals=mean(datavector)+sd(datavector),
                                              Statistic=paste0( "1 Std. Dev. [", round(sd(datavector),2), "]") ))
    distribution_statistics<-rbind(distribution_statistics,
                                   data.table(type="1 Std. Dev",
                                              vals=mean(datavector)-sd(datavector),
                                              Statistic=paste0( "1 Std. Dev. [", round(sd(datavector),2), "]") ))}
  
  
  # Adding Any Quantile Values
  #-----------------------------------------------------------------------------------------------------
  if(length(quantile_values)>0){ # If there are any quantile values entered in the first place...
    
    if (max(quantile_values)>1 | min(quantile_values)<0){cat("You have included numeric values that are not between 0 and 1. These will not be included.")
                                                         quantile_values<-quantile_values[quantile_values<=1&quantile_values>=0]}
    
    if(length(quantile_values>0)){ #If there are any quantile values left after having eliminating any values between 0 and 1..
      
      # Including lines for quantile values:
      for (val in quantile_values){
        if (val==.5){distribution_statistics<-rbind(distribution_statistics,
                                                    data.table(type="Median",vals=quantile(datavector,val),Statistic="Median"))            
        }else{
          distribution_statistics<-rbind(distribution_statistics,
                                         data.table(type="Quantile",vals=quantile(datavector,val),
                                                    Statistic=paste0("q ",val,": ",round( quantile(datavector,val),2 ))))
        } # if the statistic was not median
      } # for val in quantile values
    } # If there are any valid quantile values
    
  } # Closing clause for if there were any numeric values entered in the first place...
  
  
  # Formatting the data.table and creating linetype and color themes for the different categories
  #-----------------------------------------------------------------------------------------------------
  
  distribution_statistics[,Statistic:=as.factor(type)]
  distribution_statistics[,Statistic:=as.factor(Statistic)]
  qtiles<-paste0("Qtiles: (",paste(quantile_values[quantile_values!=.5], collapse = ','),")")
  distribution_statistics[Statistic=="Quantile",Statistic:=qtiles]
  
  linetypes<-c("solid","solid","dashed","dashed")
  names(linetypes)<-c("Median","Mean","1 Std. Dev",qtiles)
  colors<-c(mean_color,quantile_color,mean_color,quantile_color)
  names(colors)<-c("Mean","Median","1 Std. Dev",qtiles)
  
  cls<-scale_color_manual(name="Statistic",values=colors)
  lines<-scale_linetype_manual(name="Statistic",values=linetypes)        
  
  hist<-hist+geom_vline(data=distribution_statistics, 
                          aes(xintercept=vals, 
                              linetype=Statistic,
                              colour = Statistic),
                          show.legend = TRUE)+cls+lines
}# if dist_stats is not null
   return(hist)
 } #Closing function
  
##############################################################################
# SERIES MAP FUNCTION
##############################################################################

wmap<-function(chloropleth_map,
               geog_id,
               variable,
               
               # Optional data/geometry
               outline_map=NULL, # 
               data=NULL,
               
               # lines around the chloropleth map layer
               chlor_lcol=NA,
               chlor_lsize=0.0,
               
               # What elements of the map do you want the function to return?
               histogram=FALSE,
                 hist_color=NULL,
                 dist_stats=NULL,
                 mean_color="red",
                 quantile_color="black",
               return_map_object_only=FALSE,
               destination_folder=NULL,
               
               # Inputs for the color scheme of the maps
               color_ramp=wpal("easter_to_earth"),
               outline_size=.1,
               outline_color="white",
               override_scale=NULL,
               color_value_breaks=NULL,
               diverging_centerpoint=NULL,
               
               # Inputs for text
               map_title=" ",
               additional_variable_name_string=NULL,
               title_font_size=NULL,
               title_font_face="plain",
               
               # Inputs for generating series-maps
               series_dimension=NULL,
               series_sequence=NULL,
               
               # Inputs for map Legend
               legend_name=NULL,
               legend_position="bottom", 
               legend_font_size=NULL,
               legend_font_face="plain",
               
               # Inputs for numeric data only
               legend_bar_width=.4,
               legend_bar_length=20,
               legend_breaks=NULL,
               legend_labels=NULL,
               
               # Inputs for categorical data only
               scramble_colors=FALSE,
               patch_width=.25,
               patch_height=.25,
               label_position="right",
               
               # Do you want print statements?
               verbose=F){      

  
  # Copying objects such that the original data sets are unaltered
    chloropleth_map<-copy(chloropleth_map)
    outline_map<-copy(outline_map)
    data<-copy(data)
  
  # Getting the data object ready to join to the fortified spatial object (either from the chloropleth map's data.table, or from an external data.table)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check to make sure the geog_id specified is within the chloropleth map's data table
          if(!(geog_id %in% names(chloropleth_map@data))) stop("That geographic ID does not appear to exist within your chloropleth map object.")
          setnames(chloropleth_map@data,geog_id,"geog_id")
      
        # Rename variables within data set
        if (!is.null(data)){ # if external data *IS* specified
            if(!(geog_id %in% names(data))) stop("That geographic ID does not appear to exist within your data set.")
            if(!(variable %in% names(data))) stop("That variable does not appear to exist within your data set.")
            setnames(data,geog_id,"geog_id")
        }else{ # If external data is NOT specified
          data<-copy(chloropleth_map@data)
            if(!(variable %in% names(data))){
              stop("That variable does not appear to exist in the data attributes of your spatial object.")
            }
          }
        setnames(data,variable,"variable")
  
      # checking whether the data is a factor/ordered: this will make it be mapped with a discrete scale.
      if (is.factor(data[["variable"]])|is.ordered(data[["variable"]])){discrete_scale<-TRUE}else{discrete_scale<-FALSE}
      if (is.character(data[["variable"]])){
        print("Character data being converted to a factor. If you'd like a specific order to your levels, please transform your variable into a factor or ordered data type with appropriate ordering. ")
        data[,variable:=as.factor(variable)]
        discrete_scale<-TRUE}
  
    # Making sure that there are the right number of colors in the color ramp, by sampling and then scrambling (optional)
    # the color pallette chosen. 
      pallette<-colorRampPalette(color_ramp) 
      color_list<-pallette(nlevels(data[["variable"]]))
      if(scramble_colors==T){
          color_list<-color_list[sample(1:length(color_list))]
      }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defining "map dims"; the levels of the series dimension you want to map

  # First, discovering or defining the "series dimension"
       if (!is.null(series_dimension)){ # If you plan to loop through miltiple dimensions...
          if(!(series_dimension %in% names(data))) stop("That series dimension (what you want to iterate through) does not appear to exist within your data set.")
          setnames(data,series_dimension,"series_dimension")
        }else{
            data[,series_dimension:="*&^! no dimensions"]
        }
  
  # Restricting the mapping to only *some* levels of that dimension, if desired
      if (is.null(series_sequence)){
        map_dims<-unique(data$series_dimension)
      }else{ 
        for (select_dimension in series_sequence){ 
          if(!(select_dimension %in% unique(data$series_dimension))) stop(paste0("The dimension ",select_dimension," does not appear to exist in the column you have specified to hold your dimensions."))
        }
        map_dims<-series_sequence
      }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fortifying the Map(s) and Joining on the Data

    # "fortifying" the Rdata shapefiles
      chloropleth_map <-data.table(suppressWarnings(fortify(chloropleth_map, region="geog_id"))) ; setnames(chloropleth_map,"id","geog_id")
      if (!is.null(outline_map)){outline_map<-data.table(suppressWarnings(fortify(outline_map)))} # If an outline map is specified, fortify the outline map as well.
      
    # creating one long, huge object that you can subset by merging together the data and the forfified geometry
      data<-data[, list(geog_id=as.character(geog_id), variable, series_dimension)] # Sub-setting the data such that only the variables that matter are kept
      chloropleth_map<-merge(data, chloropleth_map, by="geog_id", allow.cartesian=T)


###########################################
## LOOPING ACROSS DIMENSIONS
########################################### 
  # Starting a PDF, if desired
    if (!is.null(destination_folder)){pdf(paste0(destination_folder,variable,additional_variable_name_string,".pdf"))}
  # Creating a list in which to store the map and histogram
    map_and_histogram_objects<-list()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Starting the Loop
  for (select_dimension in map_dims){ #for each dimension you want to plot...
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Determining map title, and subsetting the data
      if (select_dimension=="*&^! no dimensions"){
          main_map_title<-map_title
        }else{
          main_map_title<-paste0(map_title,": ",select_dimension)}
      if(verbose) print(main_map_title)
      
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Subsetting the Data
        subset<-copy(chloropleth_map[series_dimension==select_dimension]) # Sub-setting the fortified object to map out 1 layer/dimension (ex: year) of the variable of interest  
    

    #####################
    # If Data is Numeric
    #####################
    
        if(discrete_scale==F){ # If the data is numeric... 
          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Defining Boundaries and Properties of the Color Ramp
          
          # If an override was provided, setting minimum to the first in the list, and maximum to the second in the list provided.
          if (!is.null(override_scale)){
            if(is.numeric(override_scale)){
              minimum<-override_scale[1]
              maximum<-override_scale[2]
            }else{
              if(override_scale=="each_dimension"){
                maximum<-max(subset[["variable"]])
                minimum<-min(subset[["variable"]])
              }else{stop("Any character input other than 'each_dimension', which will produce a color ramp from the min/max of each dimension, is not recognized.")}
            }
          }else{ #Otherwise, set the min/max of the scale to the min/max of ALL dimensions of the variable.
            maximum<-max(chloropleth_map[["variable"]])
            minimum<-min(chloropleth_map[["variable"]])
          }
        
        # Determining the diverging centerpoint in relation to the min/max, if desired
          if(!is.null(diverging_centerpoint)){
              if(diverging_centerpoint>maximum){stop("The diverging centerpoint provided is greater than the maximum value in the data set.")}
              if(diverging_centerpoint<minimum){stop("The diverging centerpoint provided is less than the minimum value in the data set.")}
            # Finding what where the specified break point is as a fraction of the total color range 
              value_range<-maximum-minimum
              difference_from_minimum<-diverging_centerpoint-minimum
              break_value<-difference_from_minimum/value_range
              color_value_breaks<-c(0,break_value,1)
              if(verbose) print(paste0("Centering color ramp at ",diverging_centerpoint,". Any other color breaks provided have been overridden."))
          }
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Creating the Map Plot in GGPlot2
    
        map_plot<-ggplot(subset) + 
          geom_polygon(aes(x=long, y=lat, group=group, fill=variable), color=chlor_lcol, size=chlor_lsize) +
          scale_x_continuous("", breaks=NULL) + 
          scale_y_continuous("", breaks=NULL) + 
          coord_fixed(ratio=1)+
          labs(title = main_map_title) +
          theme_tufte()
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Adding the color ramp!
        
          if(!is.null(legend_breaks)&!is.null(legend_labels)){
            map_plot<-map_plot+scale_fill_gradientn(colours=rev(color_ramp), 
                                                    limits=c(minimum, maximum),
                                                    values=color_value_breaks, 
                                                    breaks=legend_breaks, 
                                                    labels=legend_labels)
            }else{
            map_plot<-map_plot+scale_fill_gradientn(colours=rev(color_ramp), 
                                                    limits=c(minimum, maximum), 
                                                    values=color_value_breaks) 
            } # Why have this if-clause? If the value of legend_breaks is NULL, then you end up not getting a legend at all. Lame!
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Adding a legend
        if (legend_position %in% c("bottom","top")){
          map_plot<-map_plot+
                    guides(fill=guide_colourbar(title=legend_name, title.position="top", barheight=legend_bar_width, barwidth=legend_bar_length, label=TRUE, ticks=FALSE )) + 
                    theme(legend.position=legend_position,legend.title=element_text(size=legend_font_size))} 
        if (legend_position %in% c("right","left")){
          map_plot<-map_plot+
                    guides(fill=guide_colourbar(title=legend_name, title.position="top", barheight=legend_bar_length, barwidth=legend_bar_width, label=TRUE, ticks=FALSE )) +
                    theme(legend.position=legend_position,legend.title=element_text(size=legend_font_size))} 
        if (legend_position %in% c("none")){
          map_plot<-map_plot+theme(legend.position="none")
        }
        
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Making a histogram of the distribution of that dimension's values
        
        if (histogram==TRUE){ # If you have specified that you do want the histogram at the bottom:
          if(!is.null(hist_color)){hist_color_ramp<-hist_color}else{hist_color_ramp<-color_ramp}
          
          histo<-histogram_colorstats(datavector=subset$variable,
                                      color_ramp=hist_color_ramp,
                                      minimum=minimum,
                                      maximum=maximum,
                                      color_value_breaks=color_value_breaks,
                                      dist_stats=dist_stats,
                                      mean_color=mean_color,
                                      quantile_color=quantile_color)
          
        }# If histogam==T
        
        } # if data is numeric
        
    
    #################################
    # If Data is Categorical/Ordinal
    #################################
    
    if (discrete_scale==T){
        map_plot<-ggplot(subset) + 
          geom_polygon(aes(x=long, y=lat, group=group, fill=variable), color=chlor_lcol, size=chlor_lsize) +
          scale_x_continuous("", breaks=NULL) + 
          scale_y_continuous("", breaks=NULL) + 
          coord_fixed(ratio=1)+
          labs(title = main_map_title) +
          theme_tufte()
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Adding the color ramp!
    
          map_plot<-map_plot+scale_fill_manual(values=color_list,drop = FALSE)
          
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Adding a legend
          map_plot<-map_plot+guides(fill=guide_legend(title=legend_name,
                                                      keywidth=patch_width,
                                                      keyheight=patch_height,
                                                      label.position = label_position))+
            theme(legend.position=legend_position)
        
        if (legend_position %in% c("none")){
          map_plot<-map_plot+theme(legend.position="none")
        }
        
        
        # Adding a "histogram" (really, in this case, a bar chart) to the bottom of the image
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (histogram==TRUE){ # If you have specified that you do want the histogram at the bottom:          
          #histo<-qplot(variable, data=subset, geom="bar", fill=variable)+scale_fill_manual(values=color_list)
          histo<-ggplot(na.omit(subset), aes(x=variable, fill=variable)) +
            geom_bar() + 
            labs(x=NULL, y=NULL) +
            scale_fill_manual(values=color_list)+theme_tufte()+theme(legend.position="none",
                                                                     axis.ticks.x=element_blank(),
                                                                     axis.ticks.y=element_blank())
        }# If histogam==T
        
    } # if it's ordinal/categorical
    
    #######################################
    # For All Data Types
    #######################################
    
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Adding Title and Legend Formatting
          map_plot<-map_plot +  theme(plot.title = element_text(size = title_font_size,  face=title_font_face))+ #Adding custom title that might override the legend stuff
            theme(legend.text = element_text(size = legend_font_size, face=legend_font_face))
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Adding Outline Map, if desired
          if (!is.null(outline_map)){
            map_plot<-map_plot+geom_path(data = outline_map, 
                                         aes(x = long, y = lat, group = group),
                                         color = outline_color, size = outline_size)} 
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## If you just want the map plot as an object you can pass to other things...
        if (return_map_object_only==TRUE){return(map_plot)}else{
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Printing the Plot:
      if (histogram==TRUE){# Combining Histogram and Map to plot into a single image.
        grid.newpage() # Starting a new page
        pushViewport(viewport(layout = grid.layout(5, 1))) # Defining the ratio of the histogram to map to be 5 sections vertically, 1 horizontally
        vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y) # Defining a function that allows setting the layout of the viewport 
        print(map_plot, vp = vplayout(1:4, 1)) # Printing the map plot to the viewport in vertical slots 1-4, covering all the x-space
        print(histo, vp = vplayout(5, 1)) # Printing the histogram to the bottom of the map: 
      }else{print(map_plot)} #If you didn't want the histogram, just print out the map!
    } # Closing the "if return map object=TRUE" clause
  } # Closing the loop of dimensions
  if (length(destination_folder)>0){dev.off();print("PDF ready to view.")} #If you were writing this to a PDF, you can close it, and check it out!
} # Closing Function!


##############################################
## Plotting a color pallette
##############################################

# Adapted from:
#https://learnr.wordpress.com/2009/04/15/ggplot2-qualitative-colour-palettes/
plot_colors<-function(color_list,color_list_name=NULL){
  # Author: Rebecca Stubbs
  # color_list: list of colors
  # color_list_name: The title of the plot
  # requires: data.table,ggplot2,ggthemes
  
  data_table<-data.table(data.frame(x=letters[1:length(unique(color_list))]))
  data_table[, colors:=unique(color_list)]
  data_table[, bar_height:=1]
  p<-ggplot(data_table,aes(x=colors))+geom_bar(aes(fill=colors, stat="bin"))+theme_tufte()+labs(x=paste(unique(color_list), collapse=', '))+
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank())
  p<-p+ scale_fill_manual(values=color_list) + ggtitle(color_list_name)
  return(p)
}

################################################
## Generating a cubehelix intensity pallette
#################################################
intensity_pallette<-function(start,r,gamma,hue_list=c(.5,1,3)){
  # This function creates a color pallette from a cubehelix that increases in Intensity/hue as it gets darker.
  # Start: what color you want to start with
  # hue: how bright you want the colors to be (1: normal, higher: brighter, lower: more demure)
  # R: How many "rotations" through color space you want it to do (how complicated do you want your color ramp to be?)
  # gamma: How light or dark you want it to be (1: normal, higher:darker, lower: lighter)
  # requires: library(rje) for the cubehelix function. 
  # Author: Rebecca Stubbs on 2/18/2016
  #low intensity pallette
  mellow<-cubeHelix(11, start = start, r = r, hue = hue_list[1], gamma = gamma)
  #middling intensity
  mid<-cubeHelix(11, start = start, r = r, hue = hue_list[2], gamma = gamma) 
  #strong intensity
  strong<-cubeHelix(11, start = start, r = r, hue = hue_list[3], gamma = gamma) #hues higher than 2 get weird
  # binding together the colors such that they are flipped (light to dark)
  colors<-(rbind(rev(mellow[2:8]),rev(mid[2:8]),rev(strong[2:8])))
  mellow_to_strong<-append(colors[1,1:2],colors[2,3:5]) #appending together the mellow and middling colors
  mellow_to_strong<-append(mellow_to_strong,colors[3,6:7]) # appending together the strong colors onto the mellow and middling
  return(mellow_to_strong)}

################################################
## Creating a function (wpal) that 
#################################################
#http://www.mrao.cam.ac.uk/~dag/CUBEHELIX/cubewedges.html
wpal<-function(color=NULL,n=NULL,noblack=FALSE){
  wpal<-list()
  wpal[["easter_to_earth"]]<-c("#000000", "#5b2c44", "#826737", "#67af6d", "#90c6de", "#ffff4c")
  wpal[["cool_toned"]]<-c("#000000", "#3E350C", "#9D4B60" , "#AB86D0" ,"#97E4DF","#ffff4c")
  wpal[["bright_greens"]]<-(cubeHelix(8, start = -0.5, r = -.3, hue = 3, gamma = 1.5)[2:7]) #bright_greens
  wpal[["bright_fire"]]<-(cubeHelix(8, start = .5, r = .4, hue = 3, gamma = 1.3)[2:7]) # Bright fire
  wpal[["eggplant_to_teal"]]<-(cubeHelix(8, start = .95, r = -.7, hue = 3, gamma = 1.3)[2:7]) # eggplant_to_teal
  wpal[["roygbiv"]]<-c("#3F004C","#8D1026","#DC2000","#ED6500","#FFAA00","#FFD400","#FFFF00","#BDFD69","#7CFBD3","#46EFDF","#11E3EB")
    
  ###########################################
  # Small Rotation colors (for low-n data)
  ###########################################
  wpal[["cool_blue_deepindigo"]]<-cubeHelix(8, start = -2.8, r = -.15, hue = 2.5, gamma = 2)[2:7]
  wpal[["cool_blue_aqua"]]<-cubeHelix(8, start = -.35, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["cool_blue_bright"]]<-cubeHelix(8, start = 0, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["cool_blue_steel"]]<-cubeHelix(8, start = 2.1, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["cool_blue_jeans"]]<-cubeHelix(8, start = 2.45, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["cool_green_grassy"]]<-cubeHelix(8, start = -1.05, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["cool_green_happy"]]<-cubeHelix(8, start = -.7, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["cool_green_deepforest"]]<-cubeHelix(8, start = 1.4, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["cool_green_deeplake"]]<-cubeHelix(8, start = 1.75, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["warm_fire"]]<-(cubeHelix(8, start = .5, r = .3, hue = 3, gamma = 1.3)[2:7]) # Bright fire
  wpal[["warm_darkfire"]]<-(cubeHelix(8, start = .5, r = .3, hue = 3, gamma = 2)[2:7]) # Bright fire
  wpal[["warm_purple1"]]<-cubeHelix(8, start = -2.45, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["warm_purple2"]]<-cubeHelix(8, start = 0, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["warm_adultpink"]]<-cubeHelix(8, start = 0.35, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["warm_kidpink"]]<-cubeHelix(8, start = -2.1, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["warm_redpink"]]<-cubeHelix(8, start = -1.75, r = -.15, hue = 2.5, gamma = 1.3)[2:7]
  wpal[["warm_mauve"]]<-cubeHelix(8, start = 0.7, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["warm_darkpeach"]]<-cubeHelix(8, start = 0.7, r = .2, hue = 2, gamma = 1.7)[2:7]
  wpal[["warm_brown_happy"]]<-cubeHelix(8, start = 1.05, r = .15, hue = 1.5, gamma = 1.4)[2:7]
  wpal[["classy_earth"]]<-cubeHelix(11, start = .5, r = .8, hue = .5, gamma = 1)[1:10]
  wpal[["stormy_seas"]]<-cubeHelix(11, start = 1, r = .8, hue = .5, gamma = 1)[1:10]
  
  ##############################
  # Half-Rotation Colors
  ##############################
  wpal[["purple_to_sea_green"]]<-cubeHelix(11, start = 0.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["thanksgiving"]]<-cubeHelix(11, start = 0.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["purple_to_lavender"]]<-cubeHelix(11, start = 1, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["brown_to_sea_green"]]<-cubeHelix(11, start = 1, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["brown_to_pink"]]<-cubeHelix(11, start = 1.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["green_to_lavender"]]<-cubeHelix(11, start = 1.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["green_to_salmon"]]<-cubeHelix(11, start = 2, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["sea_green_to_pink"]]<-cubeHelix(11, start = 2, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["deep_blue_to_pink"]]<-cubeHelix(11, start = 2.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["deep_blue_to_pink"]]<-cubeHelix(11, start = 2.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  
  ##############  .8 rotation colors
  wpal[["brown_green_blue_pink"]]<-cubeHelix(11, start = 1, r = .8, hue = 1.5, gamma = 1)[1:10] 
  wpal[["red_green_blue"]]<-cubeHelix(11, start = .5, r = .8, hue = 1.5, gamma = 1)[1:10] 
  wpal[["purple_red_green"]]<-cubeHelix(11, start = 0, r = .8, hue = 1.5, gamma = 1)[1:10] 
  wpal[["sea_green_purple_tan"]]<-cubeHelix(11, start = 2, r = .8, hue = 1.5, gamma = 1)[1:10] 

  ## A bunch of different, single-rotation color helix patterns:
  wpal[["black_to_light_1"]]<-cubeHelix(11, start = 0, r = -1, hue = 1, gamma = 1)[1:10]
  wpal[["black_to_light_2"]]<-cubeHelix(11, start = .5, r = -1, hue = 1, gamma = 1)[1:10]
  wpal[["black_to_light_3"]]<-cubeHelix(11, start = 1, r = -1, hue = 1, gamma = 1)[1:10]
  wpal[["black_to_light_4"]]<-cubeHelix(11, start = 1.5, r = -1, hue = 1, gamma = 1)[1:10]  
  wpal[["black_to_light_5"]]<-cubeHelix(11, start = 2, r = -1, hue = 1, gamma = 1)[1:10]  
  wpal[["black_to_light_6"]]<-cubeHelix(11, start = 0, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_7"]]<-cubeHelix(11, start = .5, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_8"]]<-cubeHelix(11, start = 1, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_9"]]<-cubeHelix(11, start = 1.5, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_10"]]<-cubeHelix(11, start = 2, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_11"]]<-cubeHelix(11, start = 0, r = 1, hue = .5, gamma = 1)[1:10]  
  wpal[["black_to_light_12"]]<-cubeHelix(11, start = .5, r = 1, hue = .5, gamma = 1)[1:10]  
  wpal[["black_to_light_13"]]<-cubeHelix(11, start = 1, r = 1, hue = .5, gamma = 1)[1:10]  
  wpal[["black_to_light_14"]]<-cubeHelix(11, start = 1.5, r = 1, hue = .5, gamma = 1)[1:10]
  wpal[["black_to_light_15"]]<-cubeHelix(11, start = 2, r = 1, hue = .5, gamma = 1)[1:10]
  

  ########################
  # Diverging Schemes
  ########################
  ### Diverging Pallettes Based on Intensity Pallettes
  wpal[["purple_to_green_diverging_intensity"]]<-append(rev(intensity_pallette(start=.2,r=.4,gamma=1)), intensity_pallette(start=1.75,r=.5,gamma=1))
  wpal[["blue_to_red_intensity"]]<-append(rev(intensity_pallette(start=.5,r=-.5,gamma=1)), intensity_pallette(start=0.5,r=.5,gamma=1))
  
  ### Diverging from black
  wpal[["pink_blue_multi_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 1.5, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:10])
  wpal[["orange_blue_diverging_from_black"]]<-append(rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = .4, hue = 1.75, gamma = 1)[1:10])
  wpal[["green_purple_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 0, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = 1, r = -.4, hue = 1.75, gamma = 1)[1:10])
  wpal[["tan_green_multi_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:10])
  ### Diverging from white
  wpal[["pink_blue_multi_diverging_from_white"]]<-append(cubeHelix(11, start = 1.5, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["purple_blue_diverging_from_white"]]<-append(cubeHelix(11, start = 1, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = 0, r = -.4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["tan_green_multi_diverging_from_white"]]<-append(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]))
  ## Diverging from Colors 
  wpal[["green_pink_diverging_from_purple"]]<-append(rev(cubeHelix(11, start = 3, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = 3, r = .4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["orange_blue_diverging_from_purple"]]<-append(rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = .5, r = .4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["tan_blue_multi_diverging_from_green"]]<-append(rev(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = 2, r = .4, hue = 1.75, gamma = 1)[3:10]))
  
  
  wpal[["green_purple_diverting_from_blue"]]<-append(rev(cubeHelix(11, start = 2.5, r = -.4, hue = 1.75, gamma = 2)[3:10]), 
                          (cubeHelix(11, start = 2.5, r = .4, hue = 1.75, gamma = 2)[3:10]))
   
  wpal[["pink_green_diverging_from_brown"]]<-append(rev(cubeHelix(11, start = 1.5, r = -.2, hue = 1.75, gamma = 1)[3:10]), 
                          (cubeHelix(11, start = 1.5, r = .2, hue = 1.75, gamma = 1)[3:10]))
  
  
  wpal[["blue_pink_divering_from_green"]]<-append(rev(cubeHelix(11, start = .3, r = -.2, hue = 1.75, gamma = 1.6)[3:10]), 
                          (cubeHelix(11, start = .3, r = .2, hue = 1.75, gamma = 1.6)[3:10]))
  
  wpal[["blue_pink_diverging_from_light_purple"]]<-append((cubeHelix(11, start = .3, r = -.2, hue = 1.75, gamma = 1.6)[3:10]), 
                          rev(cubeHelix(11, start = .3, r = .2, hue = 1.75, gamma = 1.6)[3:10]))
    
  wpal[["pink_green_diverging_from_light"]]<-append((cubeHelix(11, start = 1.3, r = -.5, hue = 1.75, gamma = 1)[3:10]), 
                          rev(cubeHelix(11, start = 1.3, r = .5, hue = 1.75, gamma = 1)[3:10]))
   
  ##############################
  # Making Intensity Pallettes
  ##############################
  ## Testing out some different color pallettes  
  wpal[["tan_to_red_intensity"]]<-intensity_pallette(start=0.5,r=.5,gamma=1)
  wpal[["lavender_to_deep_green_intensity"]]<-intensity_pallette(start=1.75,r=.5,gamma=1)
  wpal[["pink_to_purple_intensity"]]<-intensity_pallette(start=3,r=.5,gamma=1)
  wpal[["mild_green_to_dark_brown_intensity"]]<-intensity_pallette(start=1,r=.5,gamma=1)
  wpal[["lavender_green_dark_brown_intensity"]]<-intensity_pallette(start=1,r=.75,gamma=1)
  wpal[["sea_green_to_purple_intensity"]]<-intensity_pallette(start=.5,r=-.5,gamma=1)
  wpal[["pastel_to_purple_intensity"]]<-intensity_pallette(start=.2,r=.4,gamma=1)
  wpal[["sea_green_to_blue_intensity"]]<-intensity_pallette(start=3,r=-.4,gamma=1)
    
  if (!is.null(color)){ # if a specific color is requested
    
    requested_ramp<-wpal[[color]]
    # Eliminating black from the color ramp, if requested.
    
    if(noblack){ # if the person has specified that they don't want pure black included in the color scheme:
      requested_ramp<-wpal[[color]]
      requested_ramp<-copy(requested_ramp[!(requested_ramp %in% c("#000000"))]) # Eliminating black from the color ramp
    }
    if (!is.null(n)){ #If a specific number of colors are requested to be sampled
      # Creating a colorRampPallette function capable of interpolating N colors from the pallette
      pallette<-colorRampPalette(requested_ramp) 
      # Sampling n numbers of colors from that pallette (default will be 11)
      color_list<-pallette(n)
      return(color_list)
    }else{return(requested_ramp)}
    
  }else{ # If a specific color is *not* requested, return the whole list.
    return(wpal)}

} # End woodson pallettes storage function.

################ 
# Writing function to view plots
view_wpal<-function(color=NULL){
  
  if (is.null(color)){
    print("No color specified; plotting all colors")
    index<-1
    for (color in names(wpal())){
      if (index==1){grid.newpage(); pushViewport(viewport(layout = grid.layout(5, 15)))}
      if (index<=5){print((plot_colors(wpal(color),color_list_name=color)), vp = vplayout(index, 1:15))}
      index <- index+1
      if (index>5){index<-1}
    }}else{
      print(paste0("Plotting wpal color scheme ",color))
      print((plot_colors(wpal(color),color_list_name=color)))}
} # Closing function

# Defining the ggplot2 theme() theme_wpal() to use with base maps-----------------------------------------
theme_wpal<- function(base_size=14, base_family="sans",map=TRUE,legend_position="bottom",legend_direction="horizontal") {
  print("If you use this for a map, you must add coord_fixed() to your plot!")
  ((theme_foundation(base_size=base_size, base_family=base_family)+theme(
    
    # Titles and text----------------------------------------------------------
    
    plot.title = element_text(face = "plain", size = rel(1.2), hjust = 0.5),
    text = element_text(),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = NA),
    # Panel outlines
    panel.grid.major = element_blank(),#element_line(colour="#f0f0f0"),
    panel.grid.minor = element_blank(),
    
    # Legends----------------------------------------------------------------
    legend.key = element_rect(colour = NA),
    legend.position = legend_position,
    legend.direction = legend_direction,
    legend.key.size= unit(0.4, "cm"),
    legend.margin = unit(0, "cm"),
    legend.title = element_text(face="plain"),
    
    # Margins----------------------------------------------------------------
    plot.margin=unit(c(10,5,5,5),"mm"),
    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
    strip.text = element_text(face="bold")))+
     
     # Adding Axis labels (map specific or no):--------------------------------
   if(map){
     theme(
       axis.title =element_blank(),
       axis.title.y = element_blank(),
       axis.title.x =element_blank(),
       axis.text = element_blank(),
       axis.line = element_blank(),
       axis.ticks = element_blank())
   }else{
     theme(
       axis.title = element_text(face = "bold",size = rel(1)),
       axis.title.y = element_text(angle=90,vjust =2),
       axis.title.x = element_text(vjust = -0.2),
       axis.text = element_text(), 
       axis.line = element_line(colour="black"),
       axis.ticks = element_line())
   }
  ) # Closing the theme() object
}# Closing theme_wmap() function

