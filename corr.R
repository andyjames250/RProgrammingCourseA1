corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        result <- vector("numeric", 0)
        completecases <- complete(directory)
        completecases2 <- subset(completecases, nobs > threshold)
        if(nrow(completecases2) > 0 ) {
                id <- completecases2$id
                filenames <- paste(formatC(id, width=3, format="d", flag="0"), ".csv", sep = "")
                data <- NULL
                for(i in 1:length(filenames)) {
                        data <- read.csv(paste(directory, filenames[i], sep = "/"))
                        result <- rbind(result, cor(data$sulfate, data$nitrate, use="complete.obs", method="pearson"))
                }
                result <- as.vector(result)
        }
        corr <- result
        corr
}