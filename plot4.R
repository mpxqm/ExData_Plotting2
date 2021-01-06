################################################################################
##                                                                            ##
##   plot4.R					                              ##
##									      ##
##   Exploratory Data Analysis Course Project 2                               ##
##   2021-01                                                                  ##
##                                                                            ##
################################################################################



plot4 = function(data_file_1 ="./summarySCC_PM25.rds",
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
	# Across the United States, how have emissions from coal 
	# combustion-related sources changed from 1999–2008?	
	# Answer:
	# Overall emissions have decreased.
	
	
	# The information which data is related to coal is given in SCC
	# The key is SCC, the information is in El.Sector
	
	coal_idx = grepl("[Cc]oal", SCC$EI.Sector)
	coal_sources = as_tibble(SCC$EI.Sector[coal_idx]) # 99 sources
	coal_keys = SCC$SCC[coal_idx]

	# Get coal-related data
	data_tmp = NEI %>%
		filter(NEI$SCC %in% coal_keys) %>%
		group_by(year) %>%
		summarize(Total.Emissions = sum(Emissions))%>%
		# Add another variable: emissions in kt	(kilotonne) 10^3 t
		mutate( Total.Emissions.kt = Total.Emissions / 10^3)
	
	names(data_tmp)[names(data_tmp) == "year"] = "Year"
	data_tmp$Year = factor(data_tmp$Year)
	
	#  Prepare for plotting
	emissions_rounded = round(data_tmp$Total.Emissions.kt,2)
	

	
	
################################################################################
##                                                                            ##
##                           Plotting		                              ##
################################################################################
	# Prepare device
	filename_plot = "plot4.png"
	filename_plot_full = file.path(file_dir,filename_plot)
	
	if (!file.exists(filename_plot_full) | b_overwrite){
		print(paste0("Prepare plot to be saved as ",filename_plot_full))
	
		
		# Plot
		
		ggplot(data_tmp, 
		       aes(x = Year, 
		           y = Total.Emissions.kt,
		           fill = Year,
		           width = 0.5)) + 
			theme(legend.position = "right") +
			geom_tile(colour = "white") + 
			geom_bar(stat="identity") +
			labs(x = "Year",
			     y = "Coal-related PM2.5 emissions (kt)",
			     title = paste0("Total coal-related PM2.5 emissions",
			     " in the US 1999-2008"), 
			     subtitle = paste0("Emissions overall have",
			     		  " decreased in the time period."), 
			     caption = "Data source: EPA",
			     fill="Year")+
			scale_fill_brewer(palette="Dark2") +
			geom_text(aes(label = emissions_rounded), 
				  vjust = 2,
				  color = "white", 
				  size = 3.5)+
			theme(plot.caption = element_text( face = "italic"))
		
		# close and save the image
		ggsave(filename_plot_full)
		
		print(paste0("Saved plot to ",filename_plot_full))
		
	}else{
		print(paste0("Old file ",filename_plot_full,
			     " was NOT overwritten." ))
	}
	
}