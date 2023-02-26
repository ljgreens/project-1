# HarvardX PH125.9x Data Science: Capstone
# Project: MovieLens
# By: Leighton Greenstein
# Last Modified: February 25, 2023

# Important Notes about Running this Script:
# 1. Define the `save_path` to a location on the machine running the script; the
#    `save_path` will be used throughout the execution of the script to read
#    and write data to prevent memory related issues
# 2. This script was developed and run on the operating system and machine:
#    MacOS Monterey Version 12.6.1
#    Mac mini (Late 2014)
#    Processor  2.6 GHz Dual-Core Intel Core i5
#    Memory  8 GB 1600 MHz DDR22
#    Graphics  Intel Iris 1536 MB

#    Running this script with hardware below the above specifications may
#    Encounter problems with memory and processing times
# 3. Sections of this code take substantial time to run, which was the 
#    motivation to save and load R-objects while completing this project
# 4. Any of the libraries listed in the `library initialization` need to be
#    installed prior to running the script
# 5. The warning message about setting a `save_path` requires the installation
#    of XQuartz from https://www.xquartz.org on Mac.  This dependency
#    may or may not be compatible with Windows or Linux operating systems.  To
#    run the code without this dependency, delete or comment out lines 364-372.
# 6. Because the deliverable requirements for the MovieLens project require a
#    single .R file, good software engineering practices such as
#    separating functions into source files, etc., have been omitted.
# 7. Whether data is downloaded, computed, or loaded from a 
#    saved location is controlled by switch variables and the save path can be 
#    set from line 362.  
# 8. Vignettes used to facilitate code development are referenced in APA
# 9. Three hash tags identifies comments that provide a high level explanation of
#    what the code is producing
# 10. One hash tag identifies comments that are specific to what a few lines
#     of code are producing

# initialize all libraries that are used throughout the project
library(caret)
library(kableExtra)
library(lubridate)
library(plot3D)
library(recosystem)
library(recommenderlab)
library(shiny)
library(stringr)
library(tcltk)
library(tidyverse)
library(threejs)

################################################################################
################################################################################
#                           FUNCTION DEFINITIONS                               #
################################################################################
################################################################################

### Function definitions and an explanation of how they work

# RMSE function (Irizarry, n.d.)
RMSE <- function(true_value, predicted_value) {
  sqrt(mean((true_value - predicted_value)^2))
}

monteCarloMeanStability <- function(x, k, rc, plot) {
  
  ### Takes a matrix `x` and a minimum number of user ratings `k` as inputs
  ### Samples a row or column (set by `rc`) of the matrix and extracts 
  ### the ratings that are not NA, when plot is set to true generates plots
  if(rc == "row") {
    
    # obtain row names and hold that information for translation into
    # the sample extraction
    rows <- nrow(x)
    xRowUserNames <- rownames(x)
    
    xRowUserIndex  <- sample(rows, 1, replace = FALSE)
    xRowUserName   <- xRowUserNames[xRowUserIndex]
    xRowUserRatings <- na.omit(x[xRowUserIndex,]) 

    # create vector n from 1:total number of ratings sampled
    n <- c(1:length(xRowUserRatings))
  
    # create data frame to hold Monte Carlo Simulation Mean values
    # computed for a user with at least `k` ratings
    monteCarloMeanValues <- data.frame(meanDifference = NULL, n = NULL)
  
    # If the number of user ratings is greater than `k`, compute the 
    # population mean since the assumption can be made that that the law of 
    # large numbers / central limit theorem has 
    # been met (Irizarry, 2022, Chapter 14); also assuming normal distribution
    if(length(xRowUserRatings) > k) {
      
      # Compute population mean for the row values
      mu <- mean(xRowUserRatings <- na.omit(x[xRowUserIndex,]) )
      
      # For the number of ratings, compute the mean by increasing the number of
      # rating samples taken until all of the rating samples have been used
      # and compute the difference between that mean and the population mean
      # to determine the number of ratings needed for the mean to approach the
      # population mean.  Store these means and the number of ratings in a
      # data frame called `monteCarloValues`.
      for(i in 1:length(n)) {
        movieMeans <- mean(sample(xRowUserRatings, n[i], replace = TRUE))
        monteCarloMeanValues[i, "meanDifference"] <- abs(movieMeans - mu)
        monteCarloMeanValues[i, "n"] <- i
      }
      
      ### Plot the meanDifference against the number of Ratings and best
      ### fit line if the plot variable is set to `TRUE`
      
      if(plot == "TRUE") {
        
        # Fit a line to the meanDifference and the number of ratings used
        # so that the relationship can be viewed on a plot 
        fit_loess <- loess(meanDifference ~ n, data = monteCarloMeanValues)
        
        # Plot the monteCarloMeanValues and the fitted line to visualize
        # the relationship between mean computed based on the number of ratings 
        # used and the population mean (entire row or column mean)
        plot(x = monteCarloMeanValues$n, y = monteCarloMeanValues$meanDifference,
             main = paste0("Monte Carlo Simulation: Movie has ", length(xRowUserRatings), " Ratings"),
             xlab = "Number of Rating Samples",
             ylab = "Est. True Mean - Sample Rating Mean")
        lines(n, predict(fit_loess, newdata = data.frame(n = n)),
              lwd = 3, lty = 2, col = "orange")
        
      }
    }
  }

  ### Takes a matrix `x` and a minimum number of user ratings `k` as inputs
  ### Samples a row or column (set by `rc`) of the matrix and extracts 
  ### the ratings that are not NA, when plot is set to true generates plots
  if(rc == "col") {
    
    # obtain column names and hold that information for translation into
    # the sample extraction
    columns <- ncol(x)
    xColMovieNames <- colnames(x)
    
    xColMovieIndex  <- sample(columns, 1, replace = FALSE)
    xColMovieName   <- xColMovieNames[xColMovieIndex]
    xColMovieRatings <- na.omit(x[,xColMovieIndex]) 
  
    # create vector n from 1:total number of ratings sampled
    n <- c(1:length(xColMovieRatings))
    
    # create data frame to hold Monte Carlo Simulation Mean values
    # computed for a movie with at least `k` ratings
    monteCarloMeanValues <- data.frame(meanDifference = NULL, n = NULL)
  
    # If the number of user ratings is greater than `k`, compute the 
    # population mean since the assumption can be made that that the law of 
    # large numbers / central limit theorem has 
    # been met (Irizarry, 2022, Chapter 14); also assuming normal distribution
    if(length(xColMovieRatings) > k) {
      
      # Compute population mean for the column values
      mu <- mean(xColMovieRatings <- na.omit(x[,xColMovieIndex]))
      
      # For the number of ratings, compute the mean by increasing the number of
      # rating samples taken until all of the rating samples have been used
      # and compute the difference between that mean and the population mean
      # to determine the number of ratings needed for the mean to approach the
      # population mean.  Store these means and the number of ratings in a
      # data frame called `monteCarloValues`.
      for(i in 1:length(n)) {
        movieMeans <- mean(sample(xColMovieRatings, n[i], replace = TRUE))
        monteCarloMeanValues[i, "meanDifference"] <- abs(movieMeans - mu)
        monteCarloMeanValues[i, "n"] <- i
      }
      
      
      ### Plot the meanDifference against the number of Ratings and best
      ### fit line if the plot variable is set to `TRUE`
      
      if(plot == TRUE) {
        
        # Fit a line to the meanDifference and the number of ratings used
        # so that the relationship can be viewed on a plot 
        fit_loess <- loess(meanDifference ~ n, data = monteCarloMeanValues)
        
        # Plot the monteCarloMeanValues and the fitted line to visualize
        # the relationship between mean computed based on the number of ratings 
        # used and the population mean (entire row or column mean)
        plot(x = monteCarloMeanValues$n, y = monteCarloMeanValues$meanDifference,
             main = paste0("Monte Carlo Simulation: Movie has ", length(xColMovieRatings), " Ratings"),
             xlab = "Number of Samples",
             ylab = "Est. True Mean - Sample Rating Mean")
        lines(n, predict(fit_loess, newdata = data.frame(n = n)),
              lwd = 3, lty = 2, col = "orange")
      }
    }
  }
  
  # Return the dataframe containing the meanDifference and the number number of
  # ratings used to obtain that difference, which can be used as input to the 
  # function `selectMeanStabilityN`
  return(monteCarloMeanValues) 
}

selectMeanStabilityN <- function(x, stop) {
  
  ### Pick N when the Standard Error (Standard Deviation) of the
  ### mean changes by less than the precision desired
  
  for(i in 2:nrow(x)) {
    # Compute the standard error of the meanDifference: difference between
    # the Monte Carlo estimated True mean of ratings for a movie and the 
    # mean determined by N ratings
    sdM <- sd(x$meanDifference[1:i])
    
    # Provide a variable to store the number of ratings required to meet
    # the precision criteria
    n_stop <- i
    
    # Determine when the precision criteria has been met and when it has,
    # exit the loop
    if(sdM < stop) {
      break
    }
  }
  
  # If all of the meanDifferences were used and the precision was not met,
  # assign n to be NA, because the precision for that rating was unattainable
  if(nrow(x) == n_stop) {
    n_stop <- NA
  }
  
  # Return the number of ratings required to meet mean that had the stability
  # matching the precision supplied. 
  return(n_stop)
  
}

# Set `save_path` to load and save R-objects
# save_path <- "//Users//leightongreenstein//Library//Mobile Documents//com~apple~CloudDocs//projects//movielens//submission//data//"
save_path <- "C:\\Users\\Leighton\\Documents\\movielens\\submission\\data\\"

# Display Message Box if `save_path` is not set and quit session
# R Core Team (2022) tcltk 
if(save_path <= "") {
  tk_messageBox(type = "ok", 
                message = "\'save_path\' must be entered to run this script!",
                title = "Script Setup Error!",
                icon = "error")
  stop("save_path must be set to a local folder")
}

# switch to determine if edx course code to download edx data is to be run
switch_downloadEdxMovieLensData <- 1

# Control whether edx MovieLens Data is to be downloaded and saved to `save_path`
# 0: do not download edx MovieLens Data and Save
# 1: download edx MovieLens Data and Save
if(switch_downloadEdxMovieLensData  == 1) {
  
################################################################################
################################################################################
#       CODE PROVIDED BY EDX TO DOWNLOAD AND FORMAT MOVIELENS DATA             #
################################################################################
################################################################################
  
  # Note: this process could take a couple of minutes
  
  if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
  
  library(tidyverse)
  library(caret)
  
  
  # MovieLens 10M dataset:
  # https://grouplens.org/datasets/movielens/10m/
  # http://files.grouplens.org/datasets/movielens/ml-10m.zip
  
  options(timeout = 120)
  
  ## Commented out code provided by edx because it did not work
  # dl <- "ml-10M100K.zip"
  # if(!file.exists(dl))
  #   download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ## Replaced the edx code with the following to download the MovieLens Data
  if(!exists("edx.RData")) {
    dl <- tempfile()
    download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  }
  
  ratings_file <- "ml-10M100K/ratings.dat"
  if(!file.exists(ratings_file))
    unzip(dl, ratings_file)
  
  movies_file <- "ml-10M100K/movies.dat"
  if(!file.exists(movies_file))
    unzip(dl, movies_file)
  
  ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                           stringsAsFactors = FALSE)
  colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
  ratings <- ratings %>%
    mutate(userId = as.integer(userId),
           movieId = as.integer(movieId),
           rating = as.numeric(rating),
           timestamp = as.integer(timestamp))
  
  movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                          stringsAsFactors = FALSE)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- movies %>%
    mutate(movieId = as.integer(movieId))
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Final hold-out test set will be 10% of MovieLens data
  set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
  # set.seed(1) # if using R 3.5 or earlier
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  # Make sure userId and movieId in final hold-out test set are also in edx set
  final_holdout_test <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from final hold-out test set back into edx set
  removed <- anti_join(temp, final_holdout_test)
  edx <- rbind(edx, removed)
  
  rm(dl, ratings, movies, test_index, temp, movielens, removed)
  
  # End of edx movielens download course code required to be used for project
  ##############################################################################
  # Beginning of movielens project code
  
  # Save files created by edx movielens course code
  
  save(edx, file = paste0(save_path, "edx.RData"))
  save(final_holdout_test, file = paste0(save_path, "final_holdout_test.RData"))
  save(movies_file, file = paste0(save_path, "movies_file.RData"))
  save(ratings_file, file = paste0(save_path, "ratings_file.RData"))
  
}

################################################################################
################################################################################
#                   START OF DATA INSPECTION AND CLEANING                      #
################################################################################
################################################################################

# Check for `edx` data within `save_path` and load if it exists
if(!exists("edx.RData")) {
  edx_path <- paste0(save_path, "edx.RData")
  load(edx_path)
}

### Initial view of HarvardX data created by taking a random sample of the 
### edx data set (training set) to see the data format

# Set seed to 1 so that the same random sample is generated each time the 
# script is run
set.seed(1, sample.kind = "Rounding")

# mirror the row index of the edx data frame for sample() to extract random 
# sample of indexes from the edx data frame
rows <- c(1:nrow(edx))

# extract the random sample of 6 indices to be applied to the edx data frame
snapshot_index <- sample(rows, 6, replace = FALSE)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(edx[snapshot_index,],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "MovieLens Data Acquired Using HarvardX Script") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# switch to determine if data tidying of raw
# edx training data code is to be run
switch_tidyEdxMovieLensData <- 1

if(switch_tidyEdxMovieLensData == 1) {
  
################################################################################
################################################################################
#                                DATA TIDYING                                  #
################################################################################
################################################################################
  
  
  ### STEP 1: Use regex to extract the year from each movie title ###
  
  # extract and convert year to numeric from string and store in new column "year"
  edx_clean <- edx %>% mutate(movie_year = as.numeric(str_match(edx$title, "\\d{4}")))
  
  # replace column title with values that no longer have "(YYYY)" 
  edx_title <- str_replace_all(edx_clean$title, "\\(\\d{4}\\)", "") %>%
    str_trim()
  
  # subset the `edx_clean` data frame by removing thte original title
  edx_clean <- subset(edx_clean, select = -c(title))
  # replace the title with the modificed title
  edx_clean <- edx_clean %>% mutate(title = edx_title)
  
  ### STEP 2: Identify all of the "genres" 
  
  # Separate each genre per observation into a list of vectors
  separate_genres <- strsplit(edx_clean$genres, "\\|")
  
  # Combine all of the split genre entries from each list element into a single vector
  total_genre_entries <- unlist(separate_genres)
  
  # Create a table from the "total_genre_entries" vector to see all movie categories and
  # store the results in a data frame
  df_genre_entries <- data.frame(table(total_genre_entries))
  
  # Here we can see that some column names have dashes and will need to be
  # replaced at some point with underscores to be compatible with
  # machine learning functions.  These are 'Film-Noir' and 'Sci-Fi'
  
  # Remove rows that have "no genre listed"
  no_genre_index <- which(str_detect(edx_clean$genres, "no genres listed"))
  edx_clean <- edx_clean[-no_genre_index,]
  
  # Test to see if the rows with "no genre listed" have been removed by repeating
  # prior determination of all genres
  separate_genres <- strsplit(edx_clean$genres, "\\|")
  total_genre_entries <- unlist(separate_genres)
  df_genre_entries <- data.frame(table(total_genre_entries))
  
  ### STEP 3: Add a new column for each genre category and populate the data frame 
  ### with 1 = TRUE and 0 = FALSE (binarization / dummy variabile / categorical)
  
  # Create vector to hold genre types to be used to binarize the genre data
  # for each movie rating
  genre_types <- as.character(df_genre_entries$total_genre_entries)
  
  # Binarize the genres by detecting the presence of the genre in the raw genre
  # catgory, and adding that genre to the edx_clean data frame by combining the
  # data frame with the new binarized vector for each genre
  for(i in 1:length(genre_types)) {
    p <- str_detect(edx_clean$genres, genre_types[i])
    v <-ifelse(p == TRUE, 1, 0)
    edx_clean <- cbind(edx_clean, v)
    edx_clean <- edx_clean %>% rename(!!genre_types[i] := v)
  }
  
  ### STEP 4: convert the "timestamp" into iso 8601 - YYYY-MM-DD
  
  # write the timestamp to a lubridate time object
  edx_clean <- edx_clean %>% mutate(date = as_datetime(timestamp))
  # take that time object and separate it into year, month, date,
  # hour, minute, second columns within the edx_clean data frame
  edx_clean <- edx_clean %>% mutate(year = year(edx_clean$date), 
                                    month = month(edx_clean$date), day = day(edx_clean$date),
                                    hour = hour(edx_clean$date), 
                                    minute = minute(edx_clean$date), 
                                    second = second(edx_clean$date))
  
  
  ### STEP 5: Convert column names with dashes to underscores
  
  colnames(edx_clean)[which(names(edx_clean) == "Film-Noir")] <- "Film_Noir"
  colnames(edx_clean)[which(names(edx_clean) == "Sci-Fi")] <- "Sci_Fi"
  
  ### STEP 6: Keep only the columns that are in tidy data format or those
  ###         that have been transformed
  
  # removal of non tidy format columns
  edx_clean <- subset(edx_clean, select = -c(timestamp, genres, date))
  
  ### STEP 7: save the cleaned edx dataset for loading and use later
  file_edx_clean <- paste0(save_path, "edx_clean.RData")
  save(edx_clean, file = file_edx_clean)
  
  ### STEP 8: clean up all of the global variables no longer needed for
  ###         memory management purposes
  
  # establish environment object list
  global_vars <- ls()
  # create list of objects to remove
  global_vars_remove <- global_vars[!(global_vars %in% c("edx", "edx_clean", "validation"))]
  # remove those objects
  rm(list = global_vars_remove)
  # remove remaining objects requried for the list removal process
  rm("global_vars_remove", "global_vars")

}

### STEP 9: display the transformed `edx` data

# Check for `edx_clean` data within `save_path` and load if it exists
if(!exists("edx_clean.RData")) {
  edx_path <- paste0(save_path, "edx_clean.RData")
  load(edx_path)
}

# mirror the row index of the `edx_clean` data frame for sample() to extract 
# random sample of indexes from the `edx_clean` data frame
rows <- c(1:nrow(edx_clean))

# extract the random sample of 6 indicies to be applied to edx_clean
snapshot_index <- sample(rows, 6, replace = FALSE)

# Because the table has many columns, display entire table in multiple chunks
# those chunks are 1:8, 9:19, 20:30

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)

# Columns 1:8
knitr::kable(edx_clean[snapshot_index,1:8],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Edx Data After Transformations (Columns 1 Through 8)") %>%
  kable_styling(latex_options = c("striped", "hold_position"), 
                position = "center",
                font_size = 8) 


# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
# Columns 9:19
knitr::kable(edx_clean[snapshot_index,9:19],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Edx Data After Transformations (Columns 9 Through 19)") %>%
  kable_styling(latex_options = c("striped", "hold_position"), 
                position = "center",
                font_size = 8) 

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
# Columns 20:30
knitr::kable(edx_clean[snapshot_index,20:30],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Edx Data After Transformations (Columns 20 Through 30)") %>%
  kable_styling(latex_options = c("striped", "hold_position"), 
                position = "center",
                font_size = 8) 


################################################################################
################################################################################
#                                DATA INSPECTION                               #
################################################################################
################################################################################

### Determine whether the data set has any "na" values or "0" and store the 
### results to a data frame called `dataInspection`

# Obtain the names of the columns in the `edx_clean` data frame
rowNames <- names(edx_clean)
# Set the column names for the `dataInspection` data frame
colNames <- c("Column", "NA_Count")

# Create an empty data frame as a matrix sized to the rowNames and colNames
# as created above and cast to data frame
dataInspection <- data.frame(matrix(nrow = length(rowNames), ncol = length(colNames)))

# Rename the rows and columns
rownames(dataInspection) <- rowNames
colnames(dataInspection) <- colNames

# Set column data types
dataInspection$NA_Count   <- as.numeric(dataInspection$NA_Count)

# Determine if any of the columns in the `edx_clean` dataframe have NA values
# and write the number of NA values to a dataframe called `dataInspection`
for(i in 1:nrow(dataInspection)) {
  dataInspection[i, "Column"] <- rowNames[i]
  dataInspection[i,"NA_Count"] <- length(which(is.na(edx_clean[,i])))
}

# Change the column names for display in the table when written to pdf
colnames(dataInspection) <- c("Column", "NA Count")

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(dataInspection,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Counts of `NA` Values Within Training Data Set") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Check if any movie ratings are not 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, or 5
# and store to a data frame with columns named `Rating` and `Frequency`
ratingSummary <- as.data.frame(table(edx_clean$rating))
colNames <- c("Rating", "Count")
colnames(ratingSummary) <- colNames

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(ratingSummary,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "MovieLens Training Data Rating Frequency Counts") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 


################################################################################
################################################################################
#                    DATA EXPLORATION AND VISUALIZATION                        #
################################################################################
################################################################################


### Memory Management: Subset edx_clean data set to reduce the size in memory 

# Create lists used to subset the data frame so R objects are smaller to
# reduce the chance of creating memory problems given the data set size
non_numeric_data <- c("timestamp", "genres", "movie_year", "title",
                      "date", "year", "month", "day", "hour", "minute",
                      "second")

only_rating_data <- c("userId", "movieId", "rating", "title")

# subset the data frame to contain basic predictors and the rating outcome
edx_basic <- dplyr::select(edx_clean, all_of(only_rating_data ))

# Create histogram to display the frequency of all possible rating values
hist(edx_clean$rating,
     main = "Histogram of All Movie Ratings by All Users",
     xlab = "Movie Rating Values",
     ylab = "Number of Ratings")


# Summary statistics for ratings provided by an individual users
userMovieStats   <- edx_basic %>% group_by(userId) %>%
  summarize(n = n(),
            userMovieMean = mean(rating),
            userMovieSd   = sd(rating))

# Create the largest and smallest userMovieStats by standard deviation
# and the opposite of smallest to largest
userMovieStats_maxSortSD <- arrange(userMovieStats, desc(userMovieSd)) 
userMovieStats_minSortSD <- arrange(userMovieStats, userMovieSd)

# Quick barplot of the standard deviation of the users ratings
# that are arranged from smallest to largest
barplot(userMovieStats_minSortSD$userMovieSd,
        main = "Standard Deviation of User Ratings",
        xlab = "User Identification Number",
        ylab = "User Movie Ratings Standard Deviation",
        names.arg = c(userMovieStats_minSortSD$userId))

# Quick barplot of the standard deviation of the users ratings of the minimum
# variability for the first 100 to get a zoomed in view of the tail ends since 
# the overall plot does not show the detail necessary

barplot(userMovieStats_minSortSD$userMovieSd[1:100],
        main = "Smallest Standard Deviation of User Ratings",
        xlab = "User Identification Number",
        ylab = "User Movie Ratings Standard Deviation",
        names.arg = c(userMovieStats_minSortSD$userId[1:100]))


# Quick barplot of the standard deviation of the users ratings of the maximum
# variability for the first 100 to get a zoomed in view of the tail ends since 
# the overall plot does not show the detail necessary

barplot(userMovieStats_maxSortSD$userMovieSd[1:100],
        main = "Largest Standard Deviation of User Ratings",
        xlab = "User Identification Number",
        ylab = "User Movie Ratings Standard Deviation",
        names.arg = c(userMovieStats_maxSortSD$userId[1:100]))

# Summary Statistics of user-mean movie ratings
summary_userMeanRatingSd <- summary(userMovieStats$userMovieSd)
# Create data frame to hold the userRatingStability summary statistics
summary_df <- data.frame(Minimum = NULL,
                         FirstQuartile = NULL,
                         Median = NULL,
                         Mean = NULL,
                         ThirdQuartile = NULL,
                         Maximum = NULL)

# Populate the summary data frame with userRatingStability summary statistics
summary_df[1, "Minimum"] <- summary_userMeanRatingSd[1]
summary_df[1, "FirstQuartile"] <- summary_userMeanRatingSd[2]
summary_df[1, "Median"] <- summary_userMeanRatingSd[3]
summary_df[1, "Mean"] <- summary_userMeanRatingSd[4]
summary_df[1, "ThirdQuartile"] <- summary_userMeanRatingSd[5]
summary_df[1, "Maximum"] <- summary_userMeanRatingSd[6]

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(summary_df,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Summary Statistics of Number of User Rating Standard Deviation") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Quick barplot of the standard deviation of the users ratings
# that are arranged from smallest to largest along with horizontal 
# lines that indicate the Global Mean standard deviation (RMSE for simple
# linear model), and the User Movie Rating Mean standard deviation 
# (RMSE for the User Movie Rating)
barplot(userMovieStats_minSortSD$userMovieSd,
        main = "Standard Deviation of User Ratings",
        xlab = "User Identification Number",
        ylab = "User Movie Ratings Standard Deviation",
        names.arg = c(userMovieStats_minSortSD$userId),
        col = "indianred3",
        border = "indianred3")
abline(a = sd(edx_basic$rating), b = 0, lwd = 2, col = "darkgreen")
abline(a = mean(userMovieStats$userMovieSd), b = 0, lwd = 2)
text(x = 30000, y = 1.15, "Global Standard Deviation of All Movie Ratings", col = "darkgreen", lwd = 4)
text(x = 30000, y = 0.85, "Mean User Movie Ratings Standard Deviation", lwd = 4)


################################################################################
################################################################################
#                  MODEL BUILDING AND MODEL DEVELOPMENT                        #
################################################################################
################################################################################

# Create a data frame to hold the RMSE results, a description of the method
# or model and an identification number
baselineRmseResults <- data.frame(model_Id = NULL, Model = NULL, RMSE = NULL)

# Populate the baselinRmseResults dataframe with the "Mean of all movie Ratings"
# model
baselineRmseResults[1, "model_Id"] <- 1
baselineRmseResults[1, "Model"]   <- "Global Mean (GM)"
baselineRmseResults[1, "RMSE"]    <- RMSE(edx_basic$rating, mean(edx_basic$rating))

### Create a new data frame called edx_working that contains computations needed
### to compute the RMSE for other models

# Create lists used to subset the dataframe so R ojects are smaller and 
# will reduce the chance of creating memory problems given the dataset size
non_numeric_data <- c("timestamp", "genres", "movie_year", "title",
                      "date", "year", "month", "day", "hour", "minute",
                      "second")

only_rating_data <- c("userId", "movieId", "rating", "title")

# Define columns for `edx_working`
colNames <- colnames(edx_clean)
working_keep <- !colNames %in% non_numeric_data
working_cols <- colNames[working_keep]

# subset the `edx_clean` data frame to contain predictors and the rating outcome
edx_working <- dplyr::select(edx_clean, all_of(working_cols))

# Add unique Id column to edx_basic and edx_working to facilitate making joins
# easier when computations used in models are added back into the original
# data frames
edx_basic <- edx_basic %>% mutate(uId = 1:nrow(edx_basic))
edx_working <- edx_working %>% mutate(uId = 1:nrow(edx_working))

# Compute mean ratings per user and store as new column within the 
# edx_working data frame
edx_working <- edx_working
userIdAvg <- edx_working %>% group_by(userId) %>%
  summarize(numUserRatings = n(), userAvgRating = mean(rating))

# Add user movie mean rating to the `edx_working` data frame
edx_working <- edx_working %>% left_join(userIdAvg, by = "userId")

# Populate the baselinRmseResults dataframe with the "User Mean Ratings"
# model
baselineRmseResults[2, "model_Id"] <- 2
baselineRmseResults[2, "Model"]   <- "User Mean Ratings (UMR)"
baselineRmseResults[2, "RMSE"]    <- RMSE(edx_working$rating, edx_working$userAvgRating)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
# Show only the model name and RMSE (not ID)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Barplot to show the variability in the number of ratings for each user
barplot(userMovieStats$n,
        main = "Number of Movie Ratings Per User",
        xlab = "User Identification Number",
        ylab = "Number of Movie Ratings",
        names.arg = c(userMovieStats_minSortSD$userId))

# Create summary statistics for user-movie ratings
numUserRatingSummary <- summary(userMovieStats$n)

# Create a data frame to holde the summary statistics so they can be 
# nicely displayed in pdf format
numUserRatingSummarydf <- data.frame(Minimum = NULL,
                                     FirstQuartile = NULL,
                                     Median = NULL,
                                     Mean = NULL,
                                     ThirdQuartile = NULL,
                                     Maximum = NULL)

# populate the summary statistics data frame 
numUserRatingSummarydf[1, "Minimum"] <- numUserRatingSummary[1]
numUserRatingSummarydf[1, "FirstQuartile"] <- numUserRatingSummary[2]
numUserRatingSummarydf[1, "Median"] <- numUserRatingSummary[3]
numUserRatingSummarydf[1, "Mean"] <- numUserRatingSummary[4]
numUserRatingSummarydf[1, "ThirdQuartile"] <- numUserRatingSummary[5]
numUserRatingSummarydf[1, "Maximum"] <- numUserRatingSummary[6]

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(numUserRatingSummarydf,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Summary Statistics of Number of Individual User Ratings") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Set options to turn off scientific notation
options(scipen = 100)

################################################################################
################################################################################
#                            MATRIX CONSTRUCTION                               #
################################################################################
################################################################################

### Transform the data frame to a matrix for the Monte Carlo Simulation

# switch to determine if code to transform the data frame into a matrix will
# be run.  This switch is necessary because populating the matrix takes
# almost 5 hours shown from the observed run time below:

# [1] "start k = 9000048  2023-01-20 20:47:57"
# [1] "stop k = 9000048  2023-01-21 01:16:31"
# Time difference of 4.47618 hours

# Control whether user-movie rating matrix is computed and saved
# 0: do not make user-movie rating matrix computation
# 1: create user-movie rating matrix and save

switch_buildUserMovieMatrix <- 1

if(switch_buildUserMovieMatrix  == 1) {
  
  # Determine the unique userIds and the unique movieIds from the edx_basic
  # dataframe.  The unique userIds and unique movieIds will be the 
  # rows and columns, respectively, of the matrix
  
  # Determine the indexes of movieIds that are unique and identify those
  # as `TRUE`
  movieId_duplicate_TF <- duplicated(edx_basic$movieId)
  unique_movie_index <- ifelse(movieId_duplicate_TF == TRUE, FALSE, TRUE)
  
  # Determine the indexes of userIds that are unique and idenitfy those
  # as 'TRUE`
  userId_duplicate_TF <- duplicated(edx_basic$userId)
  unique_user_index <- ifelse(userId_duplicate_TF == TRUE, FALSE, TRUE)
  
  # Extract the unique userIds and movieIds from the edx_basic dataframe
  unique_movies <- edx_basic[unique_movie_index,]
  unique_users <- edx_basic[unique_user_index,]
  
  # Assign the movieId, movideTitle, and userId values extracted from the
  # edx_basic dataframe into vectors to be later used in assigning names
  # to the rows and columns of the matrix
  unique_movieIds    <- unique_movies$movieId
  unique_movieTitles <- unique_movies$title 
  unique_usersIds    <- unique_users$userId 
  
  # Add an `id` column to the unique users dataframe 
  unique_users <- unique_users %>% mutate(id = seq(1, nrow(unique_users), 1))
  
  # create an n user by m movie empty matrix
  x <- matrix(nrow = nrow(unique_users), ncol = nrow(unique_movies))
  
  # Assign row names to the matrix based on the unique userId
  row_names <- as.character(unique_users$userId)
  rownames(x) <- row_names
  
  # Assign column names to the matrix based on the unique movieId
  col_names <- as.character(unique_movies$movieId)
  colnames(x) <- col_names
  
  # Populated each row and column of the matrix with the user-movie
  # rating combination.  Since the matrix was initially created based
  # on the number of unique users and movies, those values that do not
  # have a rating will keep the NA value during the matrix initialization
  k = nrow(edx_basic)
  paste0("start k = ", k, "  ", Sys.time())
  start <- Sys.time()
  for(i in 1:k) {
    row <- which(unique_users$userId %in% edx_basic$userId[i])
    col <- which(unique_movies$movieId %in% edx_basic$movieId[i])
    x[row, col] <- edx_basic$rating[i]
  }
  paste0("stop k = ", k, "  ", Sys.time())
  stop <- Sys.time()
  
  # Determine the time the matrix rating population takes
  stop - start
  
  # Because populating the matrix takes considerable time, save the matrix
  # to save runtime in the future while developing this project
  path_x <- paste0(save_path, "x.RData")
  save(x, file = path_x)
  
}


################################################################################
################################################################################
#                      MATRIX NA CONVERSION TO 0                               #
################################################################################
################################################################################

# The x matrix (n-user by m-movies) has NAs.  Some data analysis and
# and visualization is not compatible with NAs, so the NA entries are
# set to `0` using the loop below and the matrix is saved as x_0.RData

# Again, R objects are saved due to the time creating them takes

# [1] "2023-01-21 08:53:41 MST"
# [1] "2023-01-21 09:04:36 MST"
# Time difference of 10.90465 mins

# Control whether user-movie rating matrix is computed and saved
# 0: do not make user-movie rating matrix computation
# 1: create user-movie rating matrix and save

switch_zeroUserMovieMatrix <- 1

if(switch_zeroUserMovieMatrix  == 1) {
  
  # Set loop stop (originally used to predict how long setting `0` to NA takes)
  l <- ncol(x)
  
  # Initialize start time
  start <- Sys.time()
  Sys.time()
  
  # Make the substitution of `0` for NA
  for(k in 1:l) {
    row <- which(is.na(x[,k]))
    for(i in 1:length(row)) {
      x[row[i],k] <- 0
    }
  }
  
  # Record the stop time
  Sys.time()
  stop <- Sys.time()
  
  # Save `x_0.RData` by first creating a new object `x_0` from `x` 
  assign("x_0", x)
  path_x <- paste0(save_path, "x_0.RData")
  save(x, file = path_x)
  
  ### Because x is overwritten with `0s` where there are NAs and then saved as
  ### R object `x_0`, matrix `x` needs to be unloaded and reloaded
  
  # remove `x` and `x_0` because they are the same object with NAs being
  # written to `0` from the loop above
  rm(x, x_0)
  # define paths to load `x` and x_0` from their saved locations
  path_x <- paste0(save_path, "x.RData")
  path_x_0 <- paste0(save_path, "x_0.RData")
  
  # load `x_0` -- because `x_0` comes in as `x` it must be assigned `x_0`
  load(path_x_0)
  assign("x_0", x)
  # load `x`
  load(path_x)
  
}

# Check for `x_0` matrix data within `save_path` and load if it exists
# add another condition to the if statement & switch_zeroUserMovieMatrix = 0
# so the generated `x_0` matrix is not overwritten by saved data
if(!exists("x_0.RData") & switch_zeroUserMovieMatrix == 0) {
  # load `x_0` -- because `x_0` comes in as `x` it must be assigned `x_0`
  path_x_0 <- paste0(save_path, "x_0.RData")
  load(path_x_0)
  assign("x_0", x)
}

# Check for `x` matrix data within `save_path` and load if it exists
# add another condition to the if statement & switch_zeroUserMovieMatrix = 0
# so the generated `x` matrix is not overwritten by saved data
if(!exists("x_0.RData") & switch_zeroUserMovieMatrix== 0) {
  path_x <- paste0(save_path, "x.RData")
  # load `x`
  load(path_x)
}

################################################################################
################################################################################
#        USER MEAN RATING STABILITY ANALYSIS / MONTE CARLO SIMULATION          #
################################################################################
################################################################################

### User Movie Mean Rating Stability

### Monte Carlo Simulation to determine the number of observations of a movie
### Rating to obtain a stable expected value and the standard error
### Data Science: Probability: Course
### Section 3: Random Variables, Sampling Models, and the Central Limit Theorem
### 3.1 Random Variables and Sampling Models

# Individual user-movie rating mean stability (number of ratings required to
# obtain a reasonable approximation of their rating population mean)

# Again, R objects are saved due to the time creating them takes

# [1] "2023-01-22 15:57:03 MST"
# [1] "2023-01-22 16:08:18 MST"
# Time difference of 11.25729 mins

# Control whether user rating mean stability is computed and saved
# 0: do not make movie rating mean stability computation
# 1: create movie rating mean stability and save

switch_buildUserRatingMeanStability <- 1

if(switch_buildUserRatingMeanStability  == 1) {
  
  set.seed(112, sample.kind = "Rounding")
  
  # Monitor the time that the code takes to run
  start <- Sys.time()
  start
  
  # k is the minimum number of ratings required to perform the 
  # Monte Carlo Simulation
  k = 2
  
  # Set the criteria to determine if the mean has enough observations to 
  # be considered reliable
  precision = 0.1
  
  # Set the number of Monte Carlo Simulations to run; the greater the
  # number the closer the n to the true stability value
  B <- 3000
  
  # Create a vector to hold the n values that resulted in the precision
  nUserRatingStabilityValues <- vector(length = length(B))
  
  # Perform the Monte Carlo Simulation to determine the n values
  # that results in a rating mean that is at the precision level
  for(i in 1:B) {
    meanStability <- monteCarloMeanStability(x, k, rc = "row", plot = FALSE)
    if(nrow(meanStability > 0)){
      nUserRatingStabilityValues[i] <- selectMeanStabilityN(meanStability, precision)
    }
  }
  
  # Monitor the time that the code takes to run
  end <- Sys.time()
  end
  # Code runtime
  end - start
  
  # Save `nUserRatingStabilityValues` because it takes a long time to run
  nUserRatingStabilityValuesPath <- paste0(save_path, "nUserRatingStability.RData")
  save(nUserRatingStabilityValues, file = nUserRatingStabilityValuesPath)
  
}

# Check for `nUserRatingStabilityValues` within `save_path` and load if it exists
# add another condition to the if statement & switch_buildMovieRatingMeanStability = 0
# so the generated `nMovieRatingStabilityValues` are not overwritten by saved data
if(!exists("nMovieRatingStabilityValues.RData") & switch_buildUserRatingMeanStability == 0) {
  nMovieRatingStabilityValuesPath <- paste0(save_path, "nUserRatingStability.RData")
  load(nMovieRatingStabilityValuesPath)
}

# Based on the number of simulations made, the summary statistics of the
# number of observations that produce a stable mean (mean less than precision)

# Create summary statistics for the userRatingStability
summary_nUserRatingStabilityValues <- summary(nUserRatingStabilityValues)
# Create data frame to hold the userRatingStability summary statistics
summary_df <- data.frame(Minimum = NULL,
                         FirstQuartile = NULL,
                         Median = NULL,
                         Mean = NULL,
                         ThirdQuartile = NULL,
                         Maximum = NULL)
# Populate the summary data frame with userRatingStability summary statistics
summary_df[1, "Minimum"] <- summary_nUserRatingStabilityValues[1]
summary_df[1, "FirstQuartile"] <- summary_nUserRatingStabilityValues[2]
summary_df[1, "Median"] <- summary_nUserRatingStabilityValues[3]
summary_df[1, "Mean"] <- summary_nUserRatingStabilityValues[4]
summary_df[1, "ThirdQuartile"] <- summary_nUserRatingStabilityValues[5]
summary_df[1, "Maximum"] <- summary_nUserRatingStabilityValues[6]

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(summary_df,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Summary Statistics of Monte Carlo Simulation for UMR Reliability") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Histogram of User Rating Stability
hist(nUserRatingStabilityValues,
     main = "Histogram of User Rating Stability",
     xlab = "Number of Ratings Requried to Meet 0.1 Change in Mean Rating",
     ylab = "Frequency")


################################################################################
################################################################################
#         USER MEAN MONTE CARLO SIMULATION CONVERNCE PLOTS                     #
################################################################################
################################################################################

# k is the minimum number of ratings required to perform the 
# Monte Carlo Simulation; this is for making 4 convergence plots to 
# display in the report by turning the plot parameter in the 
# `mean stability` function to be TRUE
k = 2

# Set the criteria to determine if the mean has enough observations to 
# be considered reliable
precision = 0.1

# Set the number of Monte Carlo Simulations to run; the greater the
# number the closer the n to the true stability value
B <- 4

# Create a vector to hold the n values that resulted in the precision
nUserRatingStabilityValues <- vector(length = length(B))

# Perform the Monte Carlo Simulation to determine the n values
# that results in a rating mean that is at the precision level
set.seed(12, sample.kind = "Rounding")
for(i in 1:B) {
  meanStability <- monteCarloMeanStability(x, k, rc = "row", plot = TRUE)
  if(nrow(meanStability > 0)){
    nUserRatingStabilityValues[i] <- selectMeanStabilityN(meanStability, precision)
  }
}


# Add individual movie mean rating to the `edx_working` data frame
movieIdAvg <- edx_working %>% group_by(movieId) %>% summarize(numMovieRatings = n(), movieIdAvgRating = mean(rating))
edx_working <- edx_working %>% left_join(movieIdAvg, by = "movieId")
# memory cleanup
rm(movieIdAvg)

# Populate the baselineRmseResults data frame with the 
# "Mean of all movie Ratings" model
baselineRmseResults[3, "model_Id"] <- 3
baselineRmseResults[3, "Model"]   <- "Mean Movie Rating (MMR)"
baselineRmseResults[3, "RMSE"]    <- RMSE(edx_working$rating, edx_working$movieIdAvgRating)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
# Show only the model name and RMSE (not ID)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 


################################################################################
################################################################################
#         MOVIE MEAN STABILITY ANALYSIS MONTE CARLO SIMULATION                 #
################################################################################
################################################################################



# Movie rating mean stability (number of ratings required to
# obtain a reasonable approximation of the population mean for a movie)
# This block of code uses functions, see those functions for a in-depth 
# description of what the functions are computing

# Again, R objects are saved due to the time creating them takes

# [1] "2023-01-22 11:28:36 MST"
# [1] "2023-01-22 13:44:07 MST"
# Time difference of 2.25871 hours

# Control whether movie rating mean stability is computed and saved
# 0: do not make movie rating mean stability computation
# 1: create movie rating mean stability and save

switch_buildMovieRatingMeanStability <- 1

if(switch_buildMovieRatingMeanStability  == 1) {
  
  set.seed(23, sample.kind = "Rounding")
  
  # Monitor the time that the code takes to run
  start <- Sys.time()
  
  # Note that k must be set so that the number of ratings is greater than
  # the number of ratings required to obtain the mean stabilization as a
  # function of the precision.  A good rule of thumb is to have 
  # k = approximately double the stabilization n value
  k = 100
  
  # Set the criteria to determine if the mean has enough observations to 
  # be considered reliable
  precision = 0.1
  
  # Set the number of Monte Carlo Simulations to run; the greater the
  # number the closer the n to the true stability value
  B <- 3000
  
  # Create a vector to hold the n values that resulted in the precision
  nMovieRatingStabilityValues <- vector(length = length(B))
  
  # Perform the Monte Carlo Simulation to determine the n values
  # that results in a rating mean that is at the precision level
  for(i in 1:B) {
    meanStability <- monteCarloMeanStability(x, k, rc = "col", plot = FALSE)
    if(nrow(meanStability > 0)){
      nMovieRatingStabilityValues[i] <- selectMeanStabilityN(meanStability, precision)
    }
  }
  
  # Monitor the time that the code takes to run
  end <- Sys.time()
  # Code runtime
  end - start
  
  # Save the nMovieRatingStabilityValues due to the time to generate the data
  nMovieRatingStabilityValuesPath <- paste0(save_path, "nMovieRatingStability.RData")
  save(nMovieRatingStabilityValues, file = nMovieRatingStabilityValuesPath )
}

# Check for `nMovieRatingStabilityValues` within `save_path` and load if it exists
# add another condition to the if statement & switch_buildMovieRatingMeanStability = 0
# so the generated `nMovieRatingStabilityValues` are not overwritten by saved data
if(!exists("nMovieRatingStabilityValues.RData") & switch_buildMovieRatingMeanStability == 0) {
  nMovieRatingStabilityValuesPath <- paste0(save_path, "nMovieRatingStability.RData")
  load(nMovieRatingStabilityValuesPath)
}

# Summary statistics of the number of ratings required for a Movie Mean
# to stabilize
summary_nMovieRatingStabilityValues <- summary(nMovieRatingStabilityValues)
# Create data frame to hold these summary stats for nice pdf display
summary_df <- data.frame(Minimum = NULL,
                         FirstQuartile = NULL,
                         Median = NULL,
                         Mean = NULL,
                         ThirdQuartile = NULL,
                         Maximum = NULL)
# Populate the summary data frame with the number of ratings for a 
# stable movie mean statistics
summary_df[1, "Minimum"] <- summary_nMovieRatingStabilityValues [1]
summary_df[1, "FirstQuartile"] <- summary_nMovieRatingStabilityValues [2]
summary_df[1, "Median"] <- summary_nMovieRatingStabilityValues [3]
summary_df[1, "Mean"] <- summary_nMovieRatingStabilityValues [4]
summary_df[1, "ThirdQuartile"] <- summary_nMovieRatingStabilityValues [5]
summary_df[1, "Maximum"] <- summary_nMovieRatingStabilityValues [6]

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(summary_df,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Summary Statistics of Number of MMR Monte Carlo Simulation") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Histogram of Movie Rating Stability
hist(nMovieRatingStabilityValues,
     main = "Histogram of Mean Movie Rating Stability",
     xlab = "Number of Ratings Requried to Meet 0.1 Change in Mean Rating",
     ylab = "Frequency")


################################################################################
################################################################################
#                    HYBRIDIZED MODEL DEVELOPMENT                              #
################################################################################
################################################################################


# Subset the `edx_working` data frame to have only movies with greater than
# 700 ratings (to comply with 95%CI for mean reliability from Monte Carlo)
ratings700 <- edx_working %>% filter(numMovieRatings < 700)

# Reduce edx_working to contain just the needed values to predict
y_hat_700 <- edx_working %>% select(numMovieRatings, movieIdAvgRating)

# Grab an index of the applicable values for use in computing the
# hybridized mean (choice of applying GM or MMR based on the number
# of ratings that the movie recieved)
index700 <- which(y_hat_700$numMovieRatings < 700)
y_hat_700 <- y_hat_700 %>% mutate(hybridMean = movieIdAvgRating)
y_hat_700$hybridMean[index700] <- mean(edx_working$rating)

# Populate the baselinRmseResults dataframe with the "Hybrid Mean Movie Rating"
baselineRmseResults[4, "model_Id"] <- 4
baselineRmseResults[4, "Model"]   <- "Hybrid Rating Combination (MMR or GM)"
baselineRmseResults[4, "RMSE"]    <- RMSE(edx_working$rating, y_hat_700$hybridMean)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 


# Environment cleanup for memory management

# Create list of R objects from environment
objectsR <- objects()
# Create list of R object to keep in memory
keep <- c("edx_working", "baselineRmseResults",
          "factorRatings", "legitimizeRatings", "RMSE", "save_path")
removeIndex <- !objectsR %in% keep
removeObjects <- as.list(objectsR[removeIndex])
do.call(rm, removeObjects)
rm(removeObjects, keep, objectsR, removeIndex)

# Clean up memory: reduce the chance of a memory issue while running the script
gc()


################################################################################
################################################################################
#         MULTIPLE REGRESSION MODEL EXPANSION - USER AVERAGE CORRECTION        #
################################################################################
################################################################################


# Linear Model Expansion Using Dimension Reduction
# Difference between the global mean rating and individual user mean ratings 

# globalMean = global mean (all ratings by all users)
globalMean <- mean(edx_working$rating)

# userAvgRating = mean of all ratings by a single users
# userAverageCorrection = globalMean - userAvgRating
userIdAvg <- edx_working %>% group_by(userId) %>%
  summarize(numUserRatings = n(), userAvgRating = mean(rating)) %>%
  mutate(globalMean = globalMean) %>%
  mutate(userAverageCorrection = globalMean - userAvgRating)

# Join the userAverageCorrection to the edx_working dataframe
edx_working <- edx_working %>%
  left_join(userIdAvg, by = "userId")

# Compute the RMSE based on the expanded linear model

baselineRmseResults[5, "model_Id"] <- 5
baselineRmseResults[5, "Model"]   <- "MMR + UAC (Single Correction)"
baselineRmseResults[5, "RMSE"]    <- RMSE(edx_working$rating, edx_working$movieIdAvgRating - edx_working$userAverageCorrection)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

################################################################################
################################################################################
#   MULTIPLE REGRESSION MODEL EXPANSION - USER MEAN RATING BIAS CORRECTION     #
################################################################################
################################################################################

# Compute compute the user-movie rating bias correction as 
# userMNError = (single movie rating - Individual Movie Mean)
# + (global Mean - use Movie Rating Average) and generalize to each user after
edx_working <- edx_working %>%
  mutate(userMNError = rating - (movieIdAvgRating - userAverageCorrection))

# Generalize the user-movie rating bias correction to each user by grouping
# and taking the mean
edx_working_uMeanError <- edx_working %>%
  group_by(userId) %>%
  summarize(userAvgError = mean(userMNError))

# Join the user-movie rating bias correction term to the `edx_working` data
# frame so the RMSE can be computed for the multiple linear regression model
edx_working <- left_join(edx_working, edx_working_uMeanError, by = "userId")

# Compute the RMSE based on the expanded linear model

baselineRmseResults[6, "model_Id"] <- 6
baselineRmseResults[6, "Model"]    <- "MMR - UAC + UMRBC (Double Correction)"
baselineRmseResults[6, "RMSE"]     <- RMSE(edx_working$rating, edx_working$movieIdAvgRating - edx_working$userAverageCorrection
                                          + edx_working$userAvgError)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 



################################################################################
################################################################################
#              USER MEAN RATING BIAS CORRECTIOn - REGULARIZATION               #
################################################################################
################################################################################

# switch to determine if regularization tuning is run for the 
# user-movie rating bias correction
switch_regularizationTuning <- 1

# Control whether regularization tuning is run
# 0: do not regularize tune and save
# 1: complete regularization tuning and save
if(switch_regularizationTuning  == 1) {
  
  # Create data frame to hold the regularization tuning results (lambda and RMSE)
  regularizationTuning <- data.frame(lambda = NULL, RMSE = NULL, userAvgErrorRegularized = NULL)

  # Create a sequence of lambda values to test the resultant RMSE
  lambda <- seq(0, 500, 20)
  lambda[1] <- 1
  
  # Perform the regularization computation for the lambda sequence
  # including computing the RMSE for the regularization of the UMRBC
  # for each lambda applied that is base on the lambda vector values
  for(i in 1:length(lambda)) {
  
    # compute the regularization of the UMRBC for the lambda value
    # accessed by the loop parameter i
    edx_regularize <- edx_working %>%
      group_by(userId) %>%
      summarize(userAvgErrorRegularized = (1/(lambda[i] + numUserRatings))*(sum(userMNError)),
                movieIdAvgRating = movieIdAvgRating,
                userAverageCorrection = userAverageCorrection,
                rating = rating)
    
    # Populate the regularizationTuning dataframe with the result
    # achived from the lambda used out of the lambda vector
    regularizationTuning[i, "lambda"] <- lambda[i]
    regularizationTuning[i, "userAvgErrorRegularized"] <- edx_regularize %>% 
      select(userId, userAvgErrorRegularized) %>% group_by(userId) %>%
      filter(userId == "1")
    regularizationTuning[i, "RMSE"] <- RMSE(edx_regularize$rating, edx_regularize$movieIdAvgRating 
                                            - edx_regularize$userAverageCorrection + edx_regularize$userAvgErrorRegularized)
  
  }
  
  # save the regularization data frame for future use
  regularizationTuning_path <- paste0(save_path, "regularizationTuning.RData")
  save(regularizationTuning, file = regularizationTuning_path)
  
}


# Check for `regularizationTuning` data within `save_path` and load if it exists
if(!exists("regularizationTuning")) {
  regularizationTuning_path <- paste0(save_path, "regularizationTuning.RData")
  load(regularizationTuning_path)
}

# subset column results to display to the pdf table
regularizedTuningTable <- regularizationTuning %>% select(lambda, RMSE)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(regularizedTuningTable,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Lambda Tuning to Regularize UMRBC") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 


################################################################################
################################################################################
#                ADDITIONAL EXPLORATION AND VISUALIZATION                      #
################################################################################
################################################################################

### Visualize each dimension reduced predictor and see what type of model may
### fit, where the training data is subset so the plot can be interpreted

# Create a vector for createDataPartition split the data into two sets that
# have similar distributions (characteristic of createDataPartition 
# (Dalpaiz, 2020, Chapter 21))
y = edx_working$rating
sampleDataIndex <- createDataPartition(y = y, times = 10, p = 0.001, list = FALSE)

# Subset the training data set base on the index created from the 
# `createDataPartition()` above
sampleData <- edx_working[sampleDataIndex[,3],]

### Plotting the dimension reduced predictors against each other

# Generate best fit line information for the pairing of the `movieIdAvgRating`
# and the `userAverageCorrection` from the baselineModel #5
# "Mean Movie Rating + User Average Correction + user Average Error"

# General format of linear model: `response = signal + noise` (Dalpaiz, 2020)

# Designate the response variable y as movieIdAvgRating
y <- sampleData$movieIdAvgRating
# Fit the response variable to signal `userAverageCorrection`
UAC_MIAR_lm <- lm(y ~ userAverageCorrection, data = sampleData)

# Create a plot of the response variable movieIdAvgRating versus signal 
# userAverageCorrection; we do not know which is the independent or dependent
# variable
ggplot(data = sampleData) +
  geom_point(aes(x = userAverageCorrection, y = movieIdAvgRating)) +
  ggtitle("Samples of Mean Movie Rating vs. User Average Correction ") +
  xlab("User Average Correction") +
  ylab("Mean Movie Rating") +
  # Add the best fit line to the plot for the response and signal
  geom_abline(intercept = UAC_MIAR_lm$coefficients[[1]], slope = UAC_MIAR_lm$coefficients[[2]],
              lty = 2, color = "orange")


# General format of linear model: `response = signal + noise` (Dalpaiz, 2020)

# Designate the response variable y 
y <- sampleData$movieIdAvgRating
# Fit the response variable to signal `userAvgError`
UAE_MIAR_lm <- lm(y ~ userAvgError, data = sampleData)

# Create a plot of the response variable userAvgError versus signal 
# movieIdAvgRating; we do not know which is the independent or dependent
# variable
ggplot(data = sampleData) +
  geom_point(aes(x = userAvgError, y = movieIdAvgRating)) +
  ggtitle("Samples of Mean Movie Rating vs. User-Movie Rating Bias Correction") +
  xlab("User-Movie Rating Bias Correction") +
  ylab("Mean Movie Rating") +
  # Add the best fit line to the plot for the response and signal
  geom_abline(intercept = UAE_MIAR_lm$coefficients[[1]], slope = UAE_MIAR_lm$coefficients[[2]],
              lty = 2, color = "orange")

# Display the RMSE results for the Movie Rating Average versus the User Average Rating
# Correction and the Movie Average Rating versus the User Average
# Error model combinations

plot_df_1 <- baselineRmseResults[3,]
plot_df_2 <- baselineRmseResults[5,]

plot_df <- rbind(plot_df_1, plot_df_2)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(plot_df[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Designate the response variable y userAverageCorrection
y <- sampleData$userAverageCorrection
UAE_UAC_lm <- lm(y ~ userAvgError, data = sampleData)

# Create a plot of the response variable userAverageCorrection versus signal 
# userAverageError; we do not know which is the independent or dependent
# variable
ggplot(data = sampleData) +
  geom_point(aes(x = userAvgError, y = userAverageCorrection)) +
  ggtitle("Samples of User Average Correction vs. User-Movie Rating Bias Correction") +
  xlab("User-Movie Rating Bias Correction") +
  ylab("User Average Correction") +
  # Add the best fit line to the plot for the response and signal
  geom_abline(intercept = UAE_UAC_lm$coefficients[[1]], slope = UAE_UAC_lm$coefficients[[2]],
              lty = 2, color = "orange")

# Display the RMSE results for the Movie Rating Average versus the User Average Rating
# Correction and the Movie Average Rating versus the User Average
# Error model combinations

plot_df_1 <- baselineRmseResults[5,]
plot_df_2 <- baselineRmseResults[6,]

plot_df <- rbind(plot_df_1, plot_df_2)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(plot_df[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# 3D plot vignette from HTML Widgets Website: https://www.htmlwidgets.org/showcase_threejs.html
# scatterplot3js(x, y, z, color = rainbow(length(z)))

# Vingette for 3D plots obtained from (Dalpiaz)
# XQuartz is required to create a 3D plot using library(plot3D) and can be
# downloaded from the following website: https://www.xquartz.org

# Although all of the data would create a truer picture, the computing power
# required is taxing the the process of being able to zoom and rotate, so
# the smaller subset used for the linear plots was used here

# z <- edx_working$movieIdAvgRating
# x <- edx_working$userAverageCorrection
# y <- edx_working$userAvgError

z <- sampleData$movieIdAvgRating
x <- sampleData$userAverageCorrection
y <- sampleData$userAvgError

# scatterplot3js not support in pdf version, therefore, scatter3D used instead.
# However, scatterplot3js was helpful for zooming and rotating the plot to 
# better understand its shape.
# scatterplot3js(x, y, z, color = rainbow(length(z)))
scatter3D(x, y, z)

# Clean up variables
rm(x, y, z)


################################################################################
################################################################################
#                     DOUBLE CORRECTION MODEL FITTING - LM                     #
################################################################################
################################################################################

# Define response variable for fitting best performing multiple linear model
y <- edx_working$rating

# Fit the response variable to the signal (predictors) and predict ratings
fit_lm_model_1 <- lm(y ~ movieIdAvgRating + userAverageCorrection + userAvgError, data = edx_working)
y_hat_model_1 <- predict(fit_lm_model_1, newdata = edx_working)

# Compute the RMSE based on the expanded fitted linear model
baselineRmseResults[7, "model_Id"] <- 7
baselineRmseResults[7, "Model"]    <- "lm Fitted MMR + UAC + UMRBC (Double Correction)"
baselineRmseResults[7, "RMSE"]     <- RMSE(edx_working$rating, y_hat_model_1)


# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline and Fitted RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 



################################################################################
################################################################################
#              DOUBLE CORRECTION MODEL FITTING - BOOTSTRAP LOESS               #
################################################################################
################################################################################

# switch to determine if the bootstrap loess fit should be run
switch_bootstrapLoess <- 1

# Control whether the bootstrap loess is run
# 0: do not run and save the bootstrap loess
# 1: do run and save the bootstrap loess
if(switch_bootstrapLoess  == 1) {
  
  # Create sequence of samples to run in boostrap loess fit
  k <- seq(0, 500000, 100000)
  k <- k[-1]
  
  # Create data frame to hold the results of fitting loess, parameters
  # used and the time taken
  bootstrapLoess <- data.frame(Samples = NULL, RMSE = NULL, StartTime = NULL, EndTime = NULL, TimeTaken = NULL)
  
  # fit the bootstrap loess to the sample sizes created in the `k` sequence
  for(i in 1:length(k)) {
    
    # generate the sample based on the `k` samples to be taken
    sampleVector <- 1:nrow(edx_working)
    sampleIndex <- sample(sampleVector, k[i], replace = FALSE)
    
    # Create the response variable
    y <- edx_working$rating[sampleIndex]
    
    # Note the time parameters
    start <- Sys.time()
    Sys.time()
    
    # fit the response variable to the signal using loess and generate
    # the predicted values to compute the RMSE
    fit_loess_model_1 <- loess(y ~ movieIdAvgRating + userAverageCorrection + userAvgError, data = edx_working[sampleIndex,], span = 0.5)
    y_hat_loess_model_1 <- predict(fit_loess_model_1, newdata = edx_working[sampleIndex,])
    
    # Note the time parameters to monitor the computational burden
    Sys.time()
    end <- Sys.time()
    # Compute the computational burden as a time value
    end - start
    
    # Note the results of the loess fit to a data frame that will display
    # nicely in a pdf table
    bootstrapLoess[i, "Samples"]  <- k[i]
    bootstrapLoess[i, "RMSE"]     <- RMSE(edx_working$rating[sampleIndex], y_hat_loess_model_1)
    bootstrapLoess[i, "StartTime"]  <- start
    bootstrapLoess[i, "EndTime"]  <- end
    bootstrapLoess[i, "TimeTaken"]  <- end-start
    
  }
  
  # Save the loess bootstrap data frame results for future use
  bootstrapLoess_path <- paste0(save_path, "bootstrapLoess.RData")
  save(bootstrapLoess, file = bootstrapLoess_path)
  
}


# Check for `bootstrapLoess` data within `save_path` and load if it exists
if(!exists("bootstrapLoess")) {
  bootstrapLoess_path <- paste0(save_path, "bootstrapLoess.RData")
  load(bootstrapLoess_path)
}

# Round time values in the timeTaken column
bootstrapLoess$TimeTaken <- round(bootstrapLoess$TimeTaken, 0)

# Set options to turn off scientific notation
options(scipen = 100)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(bootstrapLoess,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Boostrap Loess RMSE with Increasing Random Sample Size (Without Replacement)") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Plot the Number of Samples versus the Time taken and fit a curve to the data
### to be used to predcit the time that the entire data set will take to fit

# Fit a line to the TimeTaken and the number of ratings used
# so that the relationship can be viewed on a plot 
fit_bootstraploess <- loess(duration(as.numeric(TimeTaken)) ~ Samples, data = bootstrapLoess)

# Plot the bootstrapLoess and the fitted line to visualize the relationship
plot(x = bootstrapLoess$Samples, y = bootstrapLoess$TimeTaken,
     main = "Bootstrap Loess Samples versus Time Taken", 
     xlab = "Number of Bootstrap Samples from Training Data Set",
     ylab = "Time Taken (minutes)")
lines(bootstrapLoess$Samples, predict(fit_bootstraploess, newdata = bootstrapLoess),
      lwd = 3, lty = 2, col = "orange")

# Reset scientific notation options
options(scipen = 100)

# Take the mean of the bootstrap RMSE samples (could have weighted the mean
# based on the number of samples, but did not since it will not be used becuase
# of its computational expense)
meanBootstrapLoessRMSE <- mean(bootstrapLoess$RMSE)

# Populate the baseline RMSE based on the mean of the Bootstrap loess exercise

baselineRmseResults[8, "model_Id"] <- 8
baselineRmseResults[8, "Model"]    <- "loess bootstrap Fitted MMR + UAC + UMRBC (Double Correction)"
baselineRmseResults[8, "RMSE"]     <- meanBootstrapLoessRMSE

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline and Fitted RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 



################################################################################
################################################################################
#              DOUBLE CORRECTION MODEL FITTING - GLM                           #
################################################################################
################################################################################


# switch to control running the glm
switch_runGlm <- 1

# Control whether glm results are computed and saved to `save_path`
# 0: do not run glm code and save results
# 1: run glm code and save results
if(switch_runGlm  == 1) {
  
  # Create the response variable
  y <- edx_working$rating
  
  # Fit the response variable to the signal using glm and specifying
  # the regression family: Poission, along with making the rating predictions
  fit_glm_model_1 <- glm(y ~ movieIdAvgRating + userAverageCorrection + userAvgError, data = edx_working, family = poisson())
  y_hat_glm_model_1 <- predict(fit_glm_model_1, newdata = edx_working, type = "response")
  
  # Save the glm model for future use
  y_hat_glm_model_1_path <- paste0(save_path, "y_hat_glm_model_1.RData")
  save(y_hat_glm_model_1, file = y_hat_glm_model_1_path)
}

# Check for `y_hat_glm_model_1` data within `save_path` and load if it exists
if(!exists("y_hat_glm_model_1")) {
  y_hat_glm_model_1_path <- paste0(save_path, "y_hat_glm_model_1.RData")
  load(y_hat_glm_model_1_path)
}


# Compute the RMSE based on the glm fit to the muliple linear model
baselineRmseResults[9, "model_Id"] <- 9
baselineRmseResults[9, "Model"]    <- "glm Poisson Fitted MMR + UAC + UMRBC (Double Correction)"
baselineRmseResults[9, "RMSE"]     <- RMSE(edx_working$rating, y_hat_glm_model_1)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 



################################################################################
################################################################################
#              DOUBLE CORRECTION MODEL EXPANSION - GENRE EFFECT                #
################################################################################
################################################################################

# Create the response variable to fit in the expanded genre multiple
# linear regression 
y <- edx_working$rating


fit_lm_big <- lm(y ~ movieIdAvgRating + userAverageCorrection  + userAvgError
                 + Action + Adventure + Animation
                 + Children + Comedy + Crime + Documentary + Drama + Fantasy
                 + Film_Noir + Horror + IMAX + Musical + Mystery + Romance
                 + Sci_Fi + Thriller + War + Western, data = edx_working)

# Predict ratings using the developed model that now has genre 
# predictors added
y_hat_lm_big <- predict(fit_lm_big, newdata = edx_working)

# Compute the RMSE based on the predictions of the expanded genre model
baselineRmseResults[10, "model_Id"] <- 10
baselineRmseResults[10, "Model"]    <- "lm Fitted Double Correction + Genres"
baselineRmseResults[10, "RMSE"]     <- RMSE(edx_working$rating, y_hat_lm_big)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline and Fitted RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

################################################################################
################################################################################
#          DOUBLE CORRECTION MODEL EXPANSION - KWAY INTERACTIONS               #
################################################################################
################################################################################


# reduce the size of `edx_working` to just the basics needed for the 
# k-way interaction (Dalpaiz, 2020, Chapter 4)

kBasicPredictors <- c("rating", "movieIdAvgRating", "userAverageCorrection", "userAvgError")

# Define columns for `edx_working`
colNames <- colnames(edx_working)
working_keep <- colNames %in% kBasicPredictors
working_cols <- colNames[working_keep]

# subset the `edx_clean` dataframe to contain predictors and the rating outcome
edx_kBasic <- dplyr::select(edx_working, all_of(working_cols))

# create the response variable for the interaction linear fittment
y <- edx_kBasic$rating

fit_lm_model_k <- lm(y ~ movieIdAvgRating + userAverageCorrection + userAvgError
                     + movieIdAvgRating*userAverageCorrection
                     + movieIdAvgRating*userAvgError
                     + userAverageCorrection*userAvgError
                     + movieIdAvgRating*userAverageCorrection*userAvgError,
                     data = edx_kBasic)

# create rating predictions based on the fit from the interation model
y_hat_lm_model_k <- predict(fit_lm_model_k , newdata = edx_kBasic)

# Compute the RMSE based on the predictions from the interaction model
baselineRmseResults[11, "model_Id"] <- 11
baselineRmseResults[11, "Model"]    <- "lm Fitted Double Correction Interaction"
baselineRmseResults[11, "RMSE"]     <- RMSE(edx_kBasic$rating, y_hat_lm_model_k)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 



################################################################################
################################################################################
#         DOUBLE CORRECTION MODEL FITTING - CROSS-VALIDATION - LM              #
################################################################################
################################################################################

# create the response variable
y <- edx_working$rating
# create index values to be used in cross validation
index_test <- createDataPartition(y = y, times = 1, p = 0.2, list = FALSE)

# Create the training and test sets with simple subsetting of the
# edx data set based on the index values returned from 'createDataPartition()'

test_set  <- edx_working[index_test,]
train_set <- edx_working[-index_test,]

# Vignette on cross validation (Dalpaiz, 2020, Chapter 21)
# perform the cross validation on the training set with 5 folds
cv_fit_lm_1 <- train(
  form = rating ~ movieIdAvgRating
  + userAverageCorrection
  + userAvgError,
  data = train_set,
  trControl = trainControl(method = "cv", number = 5),
  method = "lm"
)

# Make the predictions for the cross-validated model (best performing)
y_hat <- predict(cv_fit_lm_1, newdata = test_set)

# Compute the RMSE based on the best performing cv model on the test set data
# created from the original training data
baselineRmseResults[12, "model_Id"] <- 12
baselineRmseResults[12, "Model"]    <- "5-fold lm Cross-Validation Double Correction"
baselineRmseResults[12, "RMSE"]     <- RMSE(test_set$rating, y_hat)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline, Fitted, and Cross-Validated RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Cross validation results from the 5-folds computed
# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(cv_fit_lm_1$resample,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "5-Fold Cross-Validation Results of Double Correction Model") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Environment cleanup for memory management

# Create list of R objects from environment
objectsR <- objects()
# Create list of R object to keep in memory
keep <- c("edx_working", "baselineRmseResults", "RMSE", "save_path")
removeIndex <- !objectsR %in% keep
removeObjects <- as.list(objectsR[removeIndex])
do.call(rm, removeObjects)
rm(removeObjects, keep, objectsR, removeIndex)

# Clean up memory: reduce the chance of a memory issue while running the script
gc()


################################################################################
################################################################################
#            ALTERNATIVE METHODS: RECOMMENDERLAB UBCF, IBCF, SVD               #
################################################################################
################################################################################

# Due to input requirements of the recommenderlab package (Hahsler, 2022), the
# `x` matrix created prior is reloaded
path_x <- paste0(save_path, "x.RData")
load(path_x)

# switch to control runing the recommenderlab code
switch_runRecommenderlabCode <- 1

# Control whether recommenderlab results are computed and saved to `save_path`
# 0: do not run recommenderlab code and save results
# 1: run recommenderlab code and save results
if(switch_runRecommenderlabCode  == 1) {
  
  ### Vignette for the following code provided by Hahsler (2022) with
  ### modifications for data size and algorithm types and recording time, etc.
  
  # Set the number of random sample rows from the `x` matrix for a bootstrap
  # data set to run the recommenderlab UBCF, IBCF, and SVD algorithms.  Note that
  # 35000 caused the machine to run out of vector memory during the conversion of
  # the `x` matrix to the `realRatingMatrix` required as input by the 
  # recommenderlab package (Hahsler, 2022)
  k <- 30000
  sampleVector <- 1:k
  sampleIndex <- sample(sampleVector, k, replace = FALSE)
  
  # Convert the 30,000 rows of the `x` matrix to a `realRatingsMatrix`
  realRatingsMatrix <- as(x[sampleIndex,], "realRatingMatrix")
  # For memory management, remove the `x` matrix
  rm(x)
  
  # Create the `evalationScheme` using the recommenderlab helper function.  This
  # function provides the setup of a training set, method of applying the
  # algorithms, etc. (Hahsler, 2022)
  e <- evaluationScheme(realRatingsMatrix, method = "cross-validation",
                        train = 0.8, k = 3, given = -1)
  
  # initialize a method vector for the methods to be used in recommenderlab
  methodVector <- c("UBCF", "IBCF", "SVD")
  
  # create data frames to record the start and end times to evaluate
  # the speed and performance
  startTimedf <- data.frame(startTime = NULL)
  endTimedf <- data.frame(endTime = NULL)
  
  # for each recommender lab method listed in the methodVector, perform the
  # machine learning method and record the results
  for(i in 1:length(methodVector)) {
    # Store the start time for the method
    startTimedf[i,1] <- Sys.time()
    # Train the selected algorithms with the `Recommender` function
    fit <- Recommender(getData(e, "train"), methodVector[i])
    
    # Make Predictions for the test data with the predict function ()
    y_hat <- predict(fit, getData(e, "known"), type = "ratings")

    # compute the result
    result <- calcPredictionAccuracy(y_hat, getData(e, "unknown"))

    
    # Store the results from each method to a results data frame that
    # will be saved and used to display the method RMSE along with time
    # and performance indicators in a table
    if(i == 1) {
      recommenderlabRMSE <- result
    } else {
      recommenderlabRMSE <- rbind(result, recommenderlabRMSE)
    }
    endTimedf[i,1] <- Sys.time()
  }

  # Convert the results to a dataframe and manipulate the row names
  # along with computing the preformane metrics
  recommenderlabRMSE <- as.data.frame(recommenderlabRMSE)
  rownames(recommenderlabRMSE) <- methodVector 
  recommenderlabRMSE <- recommenderlabRMSE %>% 
    mutate(startTime = startTimedf[,1], endTime = endTimedf[,1])

  # record time taken parameters for analysis of model speed
  recommenderlabRMSE <- recommenderlabRMSE %>%
    mutate(timeTaken = endTime - startTime)
  
  # Save the results for future use and reporting on recommender lab
  recommenderlabRMSE_path <- paste0(save_path, "recommenderlabRMSE.RData")
  save(recommenderlabRMSE, file = recommenderlabRMSE_path)
  
}

# Check for `recommenderlabRMSE` data within `save_path` and load if it exists
if(!exists("recommenderlabRMSE")) {
  recommenderlabRMSE_path <- paste0(save_path, "recommenderlabRMSE.RData")
  load(recommenderlabRMSE_path)
}

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(recommenderlabRMSE,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = TRUE,
             caption = "Recommenderlab UBCF, IBCF, and SVD RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Environment cleanup for memory management

# Create list of R objects from environment
objectsR <- objects()
# Create list of R object to keep in memory
keep <- c("edx_working", "baselineRmseResults", "RMSE", "save_path")
removeIndex <- !objectsR %in% keep
removeObjects <- as.list(objectsR[removeIndex])
do.call(rm, removeObjects)
rm(removeObjects, keep, objectsR, removeIndex)

# Clean up memory: reduce the chance of a memory issue while running the script
gc()

################################################################################
################################################################################
#             ALTERNATIVE METHODS: RECOMMENDERLAB SVD BOOTSTRAP                #
################################################################################
################################################################################

# Due to input requirements of the recommenderlab package (Hahsler, 2022), the
# `x` matrix created prior is reloaded
path_x <- paste0(save_path, "x.RData")
load(path_x)

# switch to control running the recommenderlab SVD bootstrap code
switch_runRecommenderlabCode_SVD <- 1

# Control whether recommenderlab SVD results are computed and saved to `save_path`
# 0: do not run recommenderlab SVD code and save results
# 1: run recommenderlab SVD code and save results
if(switch_runRecommenderlabCode_SVD  == 1) {
  
  B <- 5

  startTimedf <- data.frame(startTime = NULL)
  endTimedf <- data.frame(endTime = NULL)
  methodVector <- vector(length = B)
  
  for(i in 1:B) {
    
    startTimedf[i,1] <- Sys.time()
    
    # Environment cleanup for memory management

    # Create list of R objects from environment
    objectsR <- objects()
    # Create list of R object to keep in memory
    keep <- c("edx_working", "baselineRmseResults", "RMSE", "save_path", "B",
              "startTimedf", "endTimedf", "methodVector", "i", 
              "recommenderlabRMSE_SVD")
    removeIndex <- !objectsR %in% keep
    removeObjects <- as.list(objectsR[removeIndex])
    do.call(rm, removeObjects)
    rm(removeObjects, keep, objectsR, removeIndex)
    
    # Clean up memory: reduce the chance of a memory issue while running the script
    gc()
        
    # Additional realRatingsMatrix memory cleanup
    if(exists("realRatingsMatrix")) {
      rm(realRatingsMatrix)
    }
    
    # Load x matrix if it does not exist
    if(!exists("x")) {
      # Due to input requirements of the recommenderlab package (Hahsler, 2022), the
      # `x` matrix created prior is reloaded
      path_x <- paste0(save_path, "x.RData")
      load(path_x)
    }
    
    # Initialize a method vector to contain the names of the SVD boostrap
    # iteration that will be written to the final reporting data frame
    methodVector[i] <- paste0("SVD Bootstrap ", i)
    # Set the number of bootstrap sample user rating rows to pull from the
    # x matrix
    k <- 30000
    sampleVector <- 1:k
    sampleIndex <- sample(sampleVector, k, replace = FALSE)
    # Convert the 30,000 rows of the `x` matrix to a `realRatingsMatrix`
    realRatingsMatrix <- as(x[sampleIndex,], "realRatingMatrix")
    
    # memory mangement removal of the x matrix
    rm(x)
    
    # Create the `evalationScheme` using the recommenderlab helper function.  
    # This function provides the setup of a training set, method of applying the
    # algorithms, etc. (Hahsler, 2022)
    e <- evaluationScheme(realRatingsMatrix, method = "cross-validation",
                          train = 0.8, k = 3, given = -1)
    
    # Train the selected algorithms with the `Recommender` function
    fit <- Recommender(getData(e, "train"), "SVD")
    # Make Predictions for the test data with the predict function ()
    y_hat <- predict(fit, getData(e, "known"), type = "ratings")
    result <- calcPredictionAccuracy(y_hat, getData(e, "unknown"))
    
    # Storing the results to a data frame for future reporting
    if(i == 1) {
      recommenderlabRMSE_SVD <- result
    } else {
      recommenderlabRMSE_SVD <- rbind(recommenderlabRMSE_SVD, result)
    }
    endTimedf[i,1] <- Sys.time()
    
  }
  
  # Convert the output recommenderlab results to a data frame, assign
  # rownames and performance time metrics
  recommenderlabRMSE_SVD  <- as.data.frame(recommenderlabRMSE_SVD)
  rownames(recommenderlabRMSE_SVD) <- methodVector
  recommenderlabRMSE_SVD <- recommenderlabRMSE_SVD %>% 
    mutate(startTime = startTimedf[,1], endTime = endTimedf[,1])

  # record time taken parameters for analysis of model speed
  recommenderlabRMSE_SVD <- recommenderlabRMSE_SVD %>%
    mutate(timeTaken = endTime - startTime)
  
  # Save the boostrap iterations of the recommenderlab SVD output
  recommenderlabRMSE_path_SVD <- paste0(save_path, "recommenderlabRMSE_SVD.RData")
  save(recommenderlabRMSE_SVD, file = recommenderlabRMSE_path_SVD)

}

# Check for `recommenderlabRMSE_SVD` data within `save_path` and load if it exists
if(!exists("recommenderlabRMSE_SVD")) {
  recommenderlabRMSE_path_SVD <- paste0(save_path, "recommenderlabRMSE_SVD.RData")
  load(recommenderlabRMSE_path_SVD)
}

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(recommenderlabRMSE_SVD,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = TRUE,
             caption = "Recommenderlab Bootstrap SVD RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Environment cleanup for memory management

# Create list of R objects from environment
objectsR <- objects()
# Create list of R object to keep in memory
keep <- c("baselineRmseResults", "RMSE", "save_path")
removeIndex <- !objectsR %in% keep
removeObjects <- as.list(objectsR[removeIndex])
do.call(rm, removeObjects)
rm(removeObjects, keep, objectsR, removeIndex)

# Clean up memory: reduce the chance of a memory issue while running the script
gc()


################################################################################
################################################################################
#                     ALTERNATIVE METHODS: RECOSYSTEM                          #
################################################################################
################################################################################

# Start with the clean data set by loading it from its save location
edx_clean_path <- paste0(save_path, "edx_clean.Rdata")
load(edx_clean_path)

# Create a data frame to hold the recosystem iterative results
recosystemResults <- data.frame()

# Create the data format that recosystem requires as input
edx_reco <- edx_clean %>% dplyr::select(userId, movieId, rating)

# Define the response variable for recosystem and to create training and
# test data for cross-validation while running recosystem
y <- edx_reco$rating

# Create index set for the training and test data sets for recosystem to
# user for cross validation
index_test <- createDataPartition(y = y, times = 1, p = 0.2, list = FALSE)

# Create the training and test data sets for recosystem cross-validation,
# which come from the edx training data
test_set  <- edx_reco[index_test,]
train_set <- edx_reco[-index_test,]

# convert the training and test sets into the recosystem compatible inputs
train_set_reco <- data_memory(train_set$userId, train_set$movieId, train_set$rating)
test_set_reco  <- data_memory(test_set$userId, test_set$movieId, test_set$rating)

# create the recosystem object `r`, train iteratively on the training set, 
# and produce the RMSE on the test set
r = Reco()
r$train(train_set_reco)
y_hat <- r$predict(test_set_reco, out_memory())

# Final Hold Out test using the recosystem solution
# Load the final hold out data 
final_holdout_test_path <- paste0(save_path, "final_holdout_test.RData")
load(final_holdout_test_path)

# Create the recosystem compatible data input
final_holdout_test_reco <- data_memory(final_holdout_test$userId, 
                                       final_holdout_test$movieId,
                                       final_holdout_test$rating)

# Make the predictions for the final hold out test data set
y_hat_final_holdout_test <- r$predict(final_holdout_test_reco, out_memory())

################################################################################
###                                                                          ###
###       FINAL HOLD OUT TEST SET RMSE USING RECOSYSTEM SOLUTION             ###
###                                                                          ###
################################################################################

print("***FINAL HOLD OUT RMSE - RECOSYSTEM:")
RMSE(final_holdout_test$rating, y_hat_final_holdout_test)

################################################################################
###                                                                          ###
###       OTHER RESULTS OUTPUTS THAT APPEAR IN THE RMARKDOWN REPORT          ###
###                                                                          ###
################################################################################

# Check for `baselineRMSE` data within `save_path` and load if it exists
if(!exists("baselineRmseResults")) {
  baselineRMSE_path <- paste0(save_path, "baselineRmseResults.RData")
  load(baselineRMSE_path)
}

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(baselineRmseResults[,2:3],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Baseline, Fitted, and Cross-Validated RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Check for `baselineRMSE` data within `save_path` and load if it exists
if(!exists("recommenderlabRMSE")) {
  recommenderlabRMSE_path <- paste0(save_path, "recommenderlabRMSE.RData")
  load(recommenderlabRMSE_path)
}

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(recommenderlabRMSE,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = TRUE,
             caption = "Recommenderlab RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8)

# Check for `recommenderlabRMSE_SVD` data within `save_path` and load if it exists
if(!exists("recommenderlabRMSE_SVD")) {
  recommenderlabRMSE_path_SVD <- paste0(save_path, "recommenderlabRMSE_SVD.RData")
  load(recommenderlabRMSE_path_SVD)
}

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(recommenderlabRMSE_SVD,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = TRUE,
             caption = "Recommenderlab Bootstrap SVD RMSE") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

################################################################################
################################################################################
#                              REFERENCES                                      #
################################################################################
################################################################################

# | Alamdari, P. E., Navimipour, N. J., Hosseinzadeh, M., Safaei, A. A., 
# |    Darwesh, A. (2022). Image-based product recommendation method for e-commerce 
# |    applications using convolutional neural networks. *Acta Informatica* 
# |    *Pragensia, 11(1),* 15-35. https://doi.org/10.18267/j.aip.167
# 
#     
# | Amiot, G., Rauch, S. (2018, August 14). How to split the Main title of a plot
# |    in 2 or more lines [Online forum post]. 
# |    https://stackoverflow.com/questions/8112786/how-to-split-the-main-title-
# |    of-a-plot-in-2-or-more-lines
#   
#     
# | Benoitanger, Dittenber, J., Heathen1. (2023, January). *Ideas to improve*
# |    *the model.* [Online forum post]. edx.
# |    https://discussions.edx.org/course-v1:HarvardX+PH125.9x+3T2022/posts/
# |    63ab383405bf1204de504d8d
#    
#     
# | Dalpaiz, D. (2020). *R for Statistical Learning.*
# |    https://daviddalpiaz.github.io/r4sl/
#     
#     
# | Dalpaiz, D. (2022). *Applied Statistics with R.* https://book.stat420.org
# 
# 
# | Pileggi, S. (2022, January 23). *Report Ready PDF tables with rmarkdown,*
# |    *knitr, kableExtra, and LaTeX.* Piping hot data.
# |    https://www.pipinghotdata.com/posts/2022-01-24-report-ready-pdf-tables-
# |    with-rmarkdown-knitr-kableextra-and-latex/
# 
# 
# | Eberly College of Science. (2023). *Introduction to GLMs.* STAT 504. 
# |    https://online.stat.psu.edu/stat504/lesson/6/6.1
#     
#     
# | Fitzgerald, M. (2012). *Introducing Regular Expressions.* O'REILLY. 
# |    https://www.oreilly.com/library/view/introducing-regular-expressions/
# |    9781449338879/?_gl=1*1vp58x9*_ga*MTE3NjYyNzUwMy4xNjc1MDIwMjIw*_ga_09
# |    2EL089CH*MTY3NTAyMDIyMC4xLjEuMTY3NTAyMDI3Ni40LjAuMA..
# 
# 
# | Hahsler. M. (2022). recommenderlab: An R Framework for Developing and Testing
# |    Recommendation Algorithms. arXiv:2205.12371 [cs.IR].
# |    doi:10.48550/ARXIV.2205.12371.
# 
# 
# | MovieLens (2009). *MovieLens 10M Dataset* [Data set]. grouplens.  
# |    https://files.grouplens.org/datasets/movielens/ml-10m.zip
# 
# 
# | Irizarry, R. A., (2022). *Introduction to Data Science: Data Analysis*
# |    *and Prediction Algorithms with R* 
# |    bookdown. http://rafalab.dfci.harvard.edu/dsbook/
#   
#   
# | Irizarry, R. A., (n.d.) Professional Certificate in Data Science [MOOC]. 
# |    HarvardX https://www.edx.org/professional-certificate/harvardx-data-science
# 
# 
# | The R Foundation. (n.d.) What is R? https://www.r-project.org/about.html
# 
# 
# | Qui, Y. (2021, September 19). *recosystem: Recommender System Using Parallel*
# |    *Matrix Factorization.* CRAN
# |    https://cran.r-project.org/web/packages/recosystem/vignettes/introduction.html
# 
# 
# | R Core Team (2022, October 31). *Package 'tcltk'* R Core Team
# |    https://r-universe.dev/manuals/tcltk.html
# 
# 
# | Wei, Y. (2021). *Colors in R* Department of Statistics Columbia University
# |    http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# 
# 
# | Wickham, H. (2014). Tidy Data. *Journal of Statistical Software, 59*(10),
# |    1-23. https://doi.org/10.18637/jss.v059.i10
# 
# 
# | Wikibooks. (2022). *LaTeX/Mathematics.* Wikibooks. 
# |    https://en.wikibooks.org/wiki/LaTeX/Mathematics
# 
# 
# | Xie, Y., Dervieux, C., Riederer, E. (2022). *R Markdown Cookbook.*
# |    Chapman & Hall/CRC. https://bookdown.org/yihui/rmarkdown-cookbook/
#   
#   
# | Yihui, X. (2023). *Authoring Books and Technical Documents with R Markdown*
# |    bookdown. https://bookdown.org/yihui/bookdown/
#   
#   
# | Zhu, H. (2021). *Create Awesome LaTeX Table with knitr::kable and kableExtra.*
# |    R-project. https://cran.r-project.org/web/packages/kableExtra/vignettes/
# |    awesome_table_in_pdf.pdf
