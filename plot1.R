################################################################################
##                                                                            ##
##   plot1.R					                              ##
##									      ##
##   Exploratory Data Analysis Course Project 2                               ##
##   2021-01                                                                  ##
##                                                                            ##
################################################################################



plot1 = function(data_file_1 ="./summarySCC_PM25.rds",
		 data_file_2 ="./Source_Classification_Code.rds",
		 file_dir = getwd(),
		 b_overwrite = TRUE){
	
################################################################################
##                                                                            ##
##                           Load libraries	                              ##
################################################################################	
	library(dplyr)
	
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
	summary(NEI) # data contains only the years necessary 
	if (sum(complete.cases(NEI)) != nrow(NEI)){
		print("Incomplete data set.")
		
	}else{
		print("Complete dataset: all rows")
	}
	sum(is.na(NEI)) # no NaNs
	
	unique(NEI$Pollutant) # contains only "PM25-PRI"
################################################################################
##                                                                            ##
##                           Processing		                              ##
################################################################################	
	# Assignment:
	# Have total emissions from PM2.5 decreased in the United States from 
	# 1999 to 2008? Using the base plotting system, make a plot showing the total 
	# PM2.5 emission from all sources for each of the years
	# 1999, 2002, 2005, and 2008.	
	#
	# Answer:
	# Total annual emissions have decreased.

	
	# Filtering by years unnecessary (see above)
	#years_str =c("1999","2002","2005","2008")
	#data_tmp = NEI %>%
	#	filter(year %in% years_str) %>%
	#	group_by(year) %>%
	#	summarize(Total.Emissions = sum(Emissions))
	
	
	data_tmp = NEI %>%
		group_by(year) %>%
		summarize(Total.Emissions = sum(Emissions))
	names(data_tmp)[names(data_tmp) == "year"] = "Year"
	

	
	
################################################################################
##                                                                            ##
##                           Plotting		                              ##
################################################################################
	# Prepare device
	filename_plot = "plot1.png"
	filename_plot_full = file.path(file_dir,filename_plot)
	
	if (!file.exists(filename_plot_full) | b_overwrite){
		print(paste0("Prepare plot to be saved as ",filename_plot_full))
		
		
		png(filename = filename_plot_full,
		    width = 480, 
		    height = 480,
		    units = "px",
		    bg = "white")
		
		# Plot 
		bp = barplot(data_tmp$Total.Emissions,
			names.arg = data_tmp$Year,
			xlab = "Year",
			ylab = "Total PM2.5 emissions (t)",
			main = "Total annual emissions of PM2.5 in the US"
		)
	
		## Add text to bottom of bars
		text(bp,
		     0,
		     round(data_tmp$Total.Emissions, 2),
		     cex = 1,
		     pos = 3) 
	
		
		if (0){ # plot with linear regression line
			plot(data_tmp$Year,data_tmp$Total.Emissions,
			     xlab = "Year",
			     ylab = "Total PM2.5 emissions (t)",
			     main = "Total annual emissions of PM2.5 in the US"
			)
			abline(lm(Total.Emissions ~ as.numeric(Year),
				  data = data_tmp))
		}
		
		# close and save the image
		dev.off()
		
		print(paste0("Saved plot to ",filename_plot_full))
		
	}else{
		print(paste0("Old file ",filename_plot_full,
			     " was NOT overwritten." ))
	}
	
}