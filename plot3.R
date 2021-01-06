################################################################################
##                                                                            ##
##   plot3.R					                              ##
##									      ##
##   Exploratory Data Analysis Course Project 2                               ##
##   2021-01                                                                  ##
##                                                                            ##
################################################################################



plot3 = function(data_file_1 ="./summarySCC_PM25.rds",
		 data_file_2 ="./Source_Classification_Code.rds",
		 file_dir = getwd(),
		 b_overwrite = TRUE){
	
################################################################################
##                                                                            ##
##                           Load libraries	                              ##
################################################################################	
	library(dplyr)
	library(ggplot2)	
	
	
################################################################################
##                                                                            ##
##                           Download data	                              ##
################################################################################	
	file_url = "https://d396qusza40orc.cloudfront.net/
	exdata%2Fdata%2FNEI_data.zip"
	file_dest = file.path(file_dir, "data.zip")
	
	# Check if one of the files contained in the zip is present,
	# else download the zip and unzip it in the current dir
	
	if (!file.exists(data_file_1)){
		print("Get data files from web.")
		
		download.file(file_url,destfile = file_dest, method="curl" )
		unzip(file_dest,exdir = file_dir ) 
	}else{
		print("Data files already available.")
	}
	
################################################################################
##                                                                            ##
##                           Load data		                              ##
################################################################################
	
	## Load the data
	NEI = readRDS(data_file_1) #("summarySCC_PM25.rds")
	SCC = readRDS(data_file_2) #("Source_Classification_Code.rds")
	
	head(NEI)
	head(SCC)
	
	# Checks
	sapply(NEI, class) # years are integers
	summary(NEI) 
	if (sum(complete.cases(NEI)) != nrow(NEI)){
		print("Incomplete data set.")
		
	}else{
		print("Complete dataset: all rows")
	}
	sum(is.na(NEI)) # no NaNs
	unique(NEI$year) # contains 1999,2002,2005 and 2008
	
	unique(NEI$Pollutant) # contains only "PM25-PRI"
	
################################################################################
##                                                                            ##
##                           Processing		                              ##
################################################################################
	
	# Assignment:
	# Of the four types of sources indicated by the type
	# (point, nonpoint, onroad, nonroad) variable, which of these four
	# sources have seen decreases in emissions from 1999–2008 
	# for Baltimore City? 
	# Which have seen increases in emissions from 1999–2008? 
	# Use the ggplot2 plotting system to make a plot answer this question.	
	#
	# Answer:
	# Decreases for NON-ROAD,NONPOINT and ON-ROAD, but not POINT.
	
	data_tmp = NEI %>%
		filter(fips == "24510") %>%
		group_by(year, type) %>%
		summarize(Total.Emissions = sum(Emissions))
	
	names(data_tmp)[names(data_tmp) == "year"] = "Year"
	names(data_tmp)[names(data_tmp) == "type"] = "Type"
	
	data_tmp$Year = factor(data_tmp$Year)
	data_tmp$Type = factor(data_tmp$Type)
	
	emissions_rounded = round(data_tmp$Total.Emissions)
	

	
	
################################################################################
##                                                                            ##
##                           Plotting		                              ##
################################################################################
	# Prepare device
	filename_plot = "plot3.png"
	filename_plot_full = file.path(file_dir,filename_plot)
	
	if (!file.exists(filename_plot_full) | b_overwrite){
		print(paste0("Prepare plot to be saved as ",filename_plot_full))
		
		if (0){	# For base plotting, open device
			png(filename = filename_plot_full,
			    width = 480, 
			    height = 480,
			    units = "px",
			    bg = "white")
		}
		# Plot
		
		ggplot(data_tmp, 
		       aes(x = Year, 
		           y = Total.Emissions,
		           fill = Type,
		           label = emissions_rounded)) + 
			geom_tile(colour = "white") + 
			geom_bar(stat="identity") +
			geom_text(size = 3,
				  position = position_stack(vjust = 0.5)) +
			labs(x = "Year",
			     y = "Total PM2.5 emissions (t)",
			     title = "Total PM2.5 emissions in Baltimore City", 
			     subtitle=paste0("by source;",
			     " decreases for NON-ROAD, NONPOINT and ON-ROAD"),
			     fill="Source")
		
		# close and save the image
		if (0){
			dev.off()
		}
		ggsave(filename_plot_full)
		
		print(paste0("Saved plot to ",filename_plot_full))
		
	}else{
		print(paste0("Old file ",filename_plot_full,
			     " was NOT overwritten." ))
	}
	
}