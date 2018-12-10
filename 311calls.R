#Code to merge frequency of 311 service calls in zipcodes where the listings exist

setwd("/Users/nik/Documents/Nik/MIM/INST737/Project/");
servicecallsData <- read.csv("/Users/nik/Documents/Nik/MIM/INST737/Project/311calls.csv", stringsAsFactors = FALSE);

install.packages("tm")
library(tm) 

corpus <-Corpus(VectorSource(servicecallsData$Incident_zip))
corpus<- tm_map(corpus, stripWhitespace);

dtm <- DocumentTermMatrix(corpus)
termFreq <- colSums(as.matrix(dtm))
freqTab <- data.frame(zipcode = names(termFreq), zipcode_freq = termFreq)

View(freqTab)

listing$zipcode_freq <- c()

for(id in 1:nrow(freqTab)){
  listing$zipcode_freq[listing$zipcode %in% freqTab$zipcode[id]] <- freqTab$zipcode_freq[id]
}

View(listing)

#Converting Blanks for zipcode frequency variable into NAs
listing$zipcode_freq[listing$zipcode_freq==""] <- NA
listing <- listing[complete.cases(listing$zipcode_freq),]


write.csv(listing, file = "listing_311freq.csv")
