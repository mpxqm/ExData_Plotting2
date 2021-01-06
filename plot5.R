################################################################################
##                                                                            ##
##   plot5.R					                              ##
##									      ##
##   Exploratory Data Analysis Course Project 2                               ##
##   2021-01                                                                  ##
##                                                                            ##
################################################################################



plot5 = function(data_file_1 ="./summarySCC_PM25.rds",
		 data_file_2 ="./Source_Classification_Code.rds",
		 file_dir = getwd(),
		 b_overwrite = TRUE){
	
################################################################################
##                                                                            ##
##                           Load libraries	                              ##
################################################################################	
	library(dplyr)
	library(ggplot2)	
	library(tibble)
	library(RColorBrewer)
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
	# How have emissions from motor vehicle sources changed from 1999–2008 
	# in Baltimore City?
	#
	# Answer:
	# Reduction of emissions from 1999-2008.
	
	# The information which data is related to motor vehicles is given in 
	# SCC that is exclude other vehicles. Lets consider 
	# The key is SCC, the information is in El.Sector
	unique_Sector = unique(as_tibble(SCC$EI.Sector)) # 59
	vehicle_idx = grepl("[Mm]otor | On-road | [Vv]ehicle",SCC$EI.Sector)
	sum(vehicle_idx) # 4
	vehicle_sources = as_tibble(SCC$EI.Sector[vehicle_idx])
	vehicle_keys = unique(SCC$SCC[vehicle_idx]) # 1138

	# Get vehicle-related data
	# Filter for Baltimore then for vehicles
	data_tmp = NEI %>%
		filter(fips == "24510") %>%
		filter(SCC %in% vehicle_keys) %>% 
		group_by(year) %>%
		summarize(Total.Emissions = sum(Emissions)) #%>%
		# Add another variable: emissions in kt	(kilotonne) 10^3 t
		# numbers get too small for one city
		#mutate( Total.Emissions.kt = Total.Emissions / 10^3)
	
	names(data_tmp)[names(data_tmp) == "year"] = "Year"
	data_tmp$Year = factor(data_tmp$Year)
	
	#  Prepare for plotting
	emissions_rounded = round(data_tmp$Total.Emissions,2)
	

	
	
################################################################################
##                                                                            ##
##                           Plotting		                              ##
################################################################################
	# Prepare device
	filename_plot = "plot5.png"
	filename_plot_full = file.path(file_dir,filename_plot)
	
	if (!file.exists(filename_plot_full) | b_overwrite){
		print(paste0("Prepare plot to be saved as ",filename_plot_full))
		

		# Plot
		
		ggplot(data_tmp, 
		       aes(x = Year, 
		           y = Total.Emissions,
		           fill = Year,
		           width = 0.5,
		           label = emissions_rounded)) + 
			theme(legend.position = "right") +
			guides(fill = guide_legend(reverse = TRUE))+
			geom_tile(colour = "white") + 
			geom_bar(stat="identity") +
			coord_flip()+
			labs(x = "Year",
			     y = "Vehicle-related PM2.5 emissions (t)",
			     title = paste0("Total motor vehicle-related PM2.5",
			     " emissions in Baltimore"), 
			     subtitle = "Reduction of emissions from 1999-2008.", 
			     caption = "Data source: EPA",
			     fill="Year")+
			scale_fill_brewer(palette = "Dark2") +
			geom_label(aes(fill = Year),
				   colour = "white",
				   size = 4) +
			theme(plot.caption = element_text( face = "italic"))
		
		# close and save the image
		ggsave(filename_plot_full)
		
		print(paste0("Saved plot to ",filename_plot_full))
		
	}else{
		print(paste0("Old file ",filename_plot_full,
			     " was NOT overwritten." ))
	}
	
}