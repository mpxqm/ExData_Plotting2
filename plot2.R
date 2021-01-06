################################################################################
##                                                                            ##
##   plot2.R					                              ##
##									      ##
##   Exploratory Data Analysis Course Project 2                               ##
##   2021-01                                                                  ##
##                                                                            ##
################################################################################



plot2 = function(data_file_1 ="./summarySCC_PM25.rds",
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
	# Have total emissions from PM2.5 decreased in the Baltimore City,
	# Maryland (fips == "24510") from 1999 to 2008?
	# Use the base plotting system to make a plot answering this question.	
	#
	# Answer:
	# Total annual emissions have decreased.
	
	data_tmp = NEI %>%
		filter(fips == "24510") %>%
		group_by(year) %>%
		summarize(Total.Emissions = sum(Emissions))
	
	names(data_tmp)[names(data_tmp) == "year"] = "Year"
	
	
	
	
################################################################################
##                                                                            ##
##                           Plotting		                              ##
################################################################################
	# Prepare device
	filename_plot = "plot2.png"
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
			main = paste0("Total annual emissions of PM2.5",
			" in Baltimore City")
		)
		## Add text to bottom of bars
		text(bp,
		     0,
		     round(data_tmp$Total.Emissions, 2),
		     cex = 1,
		     pos = 3) 
		
		
		# close and save the image
		dev.off()
		
		print(paste0("Saved plot to ",filename_plot_full))
		
	}else{
		print(paste0("Old file ",filename_plot_full,
			     " was NOT overwritten." ))
	}
	
}