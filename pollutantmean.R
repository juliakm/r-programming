pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files    
    files_list <- list.files(directory, full.names = TRUE) #creates a list of files
    dat <- data.frame() #creates an data frame
 
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    for (i in id) {
        dat <- rbind(dat, read.csv(files_list[i])) #rbind each csv file to create one monster data.frame of all 332
    }
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)  
    dat_subset <- mean(dat[, pollutant], na.rm = TRUE)
}