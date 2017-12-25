# read data
library(tidyverse)
library(magrittr)
library("BBmisc")
dirty <- read_csv("dirty_data.csv")

##Populate the missing values in the Area variable with an appropriate values
##(Birmingham, Coventry, Dudley, Sandwell, Solihull, Walsall or Wolverhampton)
dirty %<>% fill(Area)

##Remove special characters, padding (the white space before and after the text)
##from Street 1 and Street 2 variables.
Street1<-gsub("[^0-9A-Za-z///' ]", " ", dirty$Street, ignore.case = TRUE)
Street1<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", Street1, perl=TRUE)

#explanation http://rick.measham.id.au/paste/explain.pl
Street2<-gsub("[^0-9A-Za-z///' ]", " ", dirty$`Street 2`, ignore.case = TRUE)
Street2<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", Street2, perl=TRUE)

##Make sure the first letters of street names are capitalized
#The package BBmisc now contains the function capitalizeStrings.
Street1<-capitalizeStrings(Street1, all.words = TRUE, lower.back = TRUE)
Street2<-capitalizeStrings(Street2, all.words = TRUE, lower.back = TRUE)

##the street denominations are following the same standard
##(for example, all streets are indicated as “str.”, avenues as “ave.”, etc.
format_address <- function(address_string) {
  address_string %<>%
    gsub("\\b(St)\\b|\\b(Street)\\b", "St", .) %>%
    gsub("\\b(Road)\\b", "Rd", .) %>%
    gsub("\\b(Avenue)\\b|\\b(Ave)\\b", "Ave", .) %>%
    gsub("\\b(Hospital)\\b", "Hosp", .) %>%
    gsub("\\b(Park)\\b", "Pk", .) %>%
    gsub("\\b(Circle)\\b", "Cr", .) %>%
    gsub("\\b(Drive)\\b", "Dr", .) %>%
    gsub("\\b(Green)\\b", "Gr", .) %>%
    gsub("\\b(Village)\\b", "Vil", .) %>%
    gsub("\\b(Center)\\b", "Ctr", .)
  return(address_string)
}

Street1 <- format_address(Street1)
Street2 <- format_address(Street2)

## If the value in Street 2 duplicates the value in Street 1, remove the value in Street 2
dirty <- dirty %>%
  mutate(Street = Street1, `Street 2` = Street2)

dirty %<>%
  mutate(`Street 2` = ifelse(Street1 == Street2, NA, `Street 2`))

##Remove the “Strange HTML column”
clean <- dirty %>%
  select(-`Strange HTML`)

# Save clean data
write_csv(clean, "clean.csv")
