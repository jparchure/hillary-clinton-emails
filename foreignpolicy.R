library(countrycode)

#for SQLite access
library(RSQLite)

#word list and Dictionaries
library(qdapDictionaries)

#word map by region with aggregation
library(rworldmap)

data(countrycode_data)
head(countrycode_data)

count.occurences <- function(needle, haystack)
{
  sapply(regmatches(haystack, gregexpr(needle, haystack, ignore.case=T, perl=T)), length)
}

string.occurence <- function(needle,haystack){
  lookaround <- 5
  pattern <- paste0("([[:print:]]+ ){0,", lookaround, "}", needle, 
                    "( [[:print:]]+){0,", lookaround, "}")
  regmatches(haystack, gregexpr(pattern, haystack))[[1]]
  
  
  #regmatches(haystack, gregexpr(needle,haystack, ignore.case=T, perl=T))
  #regmatches(haystack, gregexpr(pattern, haystack))
}
countrycode_data_without_atf <- countrycode_data[-83,]
head(countrycode_data_without_atf)

countries <- countrycode_data_without_atf[, c("country.name", "regex",
                                              "iso2c", "iso3c")]

countries$other <- NA
countries[countries$country.name=="United Kingdom",]$other <- "UK"

db <- dbConnect(dbDriver("SQLite"), "./hillary.database.sqlite")

q <- "SELECT ExtractedBodyText EmailBody FROM Emails e INNER JOIN Persons p ON e.SenderPersonId=P.Id WHERE p.Name='Hillary Clinton'  AND e.ExtractedBodyText != '' ORDER BY RANDOM()"
emailsFromHillary <- dbGetQuery(db, q)

all_hillary_emails <- paste(emailsFromHillary$EmailBody, collapse=" // ")


country_occurrences <-data.frame(country=countrycode_data_without_atf$country.name, 
                                 continent=countrycode_data_without_atf$continent, 
                                 region=countrycode_data_without_atf$region, 
                                 ISO3C=countrycode_data_without_atf$iso3c)


country_occurrences$occurences <- NA


words_to_remove <- subset(DICTIONARY, nchar(word)==3 | nchar(word)==2)


#add "RE" because the iso3 code for Reunion islands.. but it appears a lot in
#emails to indicate the RE(sponses) to previous emails.
#FM 
#TV is ISO2 code for Tuvalu but also refers to Television
#AL is a given name and also ISO2 for Albania
#BEN is a given name and also ISO3 for Benin
#LA is Los angeles and iso 2 for Lao
#AQ is abbreviation of "As Quoted" and iso 2 for Antarctica
#Jay's Note: This is something that comes up only with intense practice. On my own, I would have NEVER even thought of removing this words
#From now on I will mark these instances as MAGIC
words_to_be_removed <- toupper(c(words_to_remove$word, "RE", "FM", "TV", "LA", "AL", "BEN", "AQ"))

for(i in 1:nrow(countries)){
    n_occurences <- 0
    if(!is.na(countries[i,]$regex)){
      tmp <- count.occurences(countries[i,]$regex, all_hillary_emails)
      n_occurences <- n_occurences + tmp
      if(tmp > 0)
      print(paste(tmp, countries[i,]$regex))
    }
    
    if((!(countries[i,]$iso2c %in% words_to_be_removed)) && (!is.na(countries[i,]$iso2c)) ){

      iso_boundary <- paste0("\\s", countries[i,]$iso2c, "\\s")
      
      tmp <- count.occurences(iso_boundary, all_hillary_emails)
      
      n_occurences <- n_occurences + tmp
      
      if(tmp >0)
        print(paste(tmp, countries[i,]$iso2c))
    }
    
    #remove words that are ISO3 country codes
    if( (! (countries[i,]$iso3c %in% words_to_be_removed) ) && (!is.na(countries[i,]$iso3c))  )
    {

      iso_boundary <- paste0("\\s", countries[i,]$iso3c,"\\s")
      
      tmp <- count.occurences(iso_boundary, all_hillary_emails)
      
      n_occurences <- n_occurences + tmp
      
      if(tmp >0)
        print(paste(tmp, countries[i,]$iso3c))
    }
    
    #remove words that are other country codes
    if( (! (countries[i,]$other %in% words_to_be_removed) ) && (!is.na(countries[i,]$other))  )
    {
      iso_boundary <- paste0("\\s", countries[i,]$other,"\\s")
      
      tmp <- count.occurences(iso_boundary, all_hillary_emails)
      
      n_occurences <- n_occurences + tmp
      
      if(tmp >0)
        print(paste(tmp, countries[i,]$other))
    }
    else if(tmp <= 0) {
      #print(countries[i,1])
      tmp <- count.occurences(countries[i,1], all_hillary_emails)
      
      n_occurences <- n_occurences + tmp
      
    }
    
    country_occurrences[i,]$occurences <- n_occurences
    
    #Trying to answer some of the questions asked above

}

nrow(country_occurrences)

country_occurrences <- na.omit(country_occurrences)
country_occurrences <- country_occurrences[country_occurrences$occurences>0,]
country_occurrences <- country_occurrences[with(country_occurrences, order(-occurences)),]
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")

mapByRegion( country_occurrences
             , nameDataColumn="occurences"
             , joinCode="ISO3"
             , nameJoinColumn="ISO3C"
             , mapTitle="US Foreign Policy Country Map through Hillary Clinton's emails"
             , regionType="IMAGE24"
             , oceanCol="lightblue"
             ,missingCountryCol="white"
             , FUN="sum" )


india_occurences<- string.occurence("India", all_hillary_emails)


