complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        filenames <- paste(formatC(id, width=3, format="d", flag="0"), ".csv", sep = "")
        result <- NULL
        for(i in 1:length(filenames)) {
                data <- read.csv(paste(directory, filenames[i], sep = "/"))
                data$completecase <- complete.cases(data)
                result <- rbind(result, cbind.data.frame(id[i], sum(data$completecase)))
        }
        complete <- result
        names(complete) <- c("id", "nobs")
        complete
}

complete2 <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        filenames <- paste(formatC(id, width=3, format="d", flag="0"), ".csv", sep = "")
        data <- NULL
        for(filename in filenames) {
                data <- rbind(data, read.csv(paste(directory, filename, sep = "/")))
        }
        data$completecase <- complete.cases(data)
        complete2 <- cbind.data.frame(id, with(data, tapply(completecase, factor(ID, levels=id), sum, simplify = TRUE)), row.names=1:length(id))
        names(complete2) <- c("id", "nobs")
        complete2
}