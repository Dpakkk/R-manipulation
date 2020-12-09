
# 1. Loading the DHFR data
library(RCurl)
dhfr <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/dhfr.csv") )

idxxx <- sample(1:nrow(dhfr), 1)

View(dhfr)


# 2. Check for missing data

sum(is.na(dhfr))


# 3. If data is clean, randomly introduce NA to the dataset

na.gen <- function(data,n) {
  i <- 1
  while (i < n+1) {
    idx1 <- sample(1:nrow(data), 1)
    idx2 <- sample(1:ncol(data), 1)
    data[idx1,idx2] <- NA
    i = i+1
  }
  return(data)
}


# Before introducing NA to the dataset, leave the Y class label (output variable) out

dhfr <- dhfr[,-1]


# Choose 1 of the following to run (they'll produce the same result)

dhfr <- na.gen(dhfr,100)

# 4. Check again for missing data

sum(is.na(dhfr))

#thid is for checking the column with the missing value
colSums(is.na(dhfr))
# checking the missing value as a dataframe
View(colSums(is.na(dhfr)))

str(dhfr)

# handling the missing data

# deleting the entire row
#there were 100 na values in our dataframe
clean.data <- na.omit(dhfr)
sum(is.na(clean.data))

# copying the data into dhfr.imput variable
dhfr.imput <- dhfr
View(colSums(is.na(dhfr.imput)))
# MEAN imputation
for (i in which(sapply(dhfr.imput, is.numeric))){
  dhfr.imput[is.na(dhfr.imput[, i]), i] <- mean(dhfr.imput[, i], na.rm = TRUE)
}

View(colSums(is.na(dhfr.imput)))
# Hence we finally imputed mean to the Null column

#MEDIAN
dhfr.impute <- dhfr
for (i in which(sapply(dhfr.impute, is.numeric))) {
  dhfr.impute[is.na(dhfr.impute[, i]), i] <- median(dhfr.impute[, i], na.rm = TRUE)
}
sum(is.na(dhfr.impute))
View(colSums(is.na(dhfr.impute)))
