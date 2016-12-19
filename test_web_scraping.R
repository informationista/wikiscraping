library(rvest)
library(RCurl)
library(plyr)


##create a list of URLs to retrieve
##create the list of years of interest
year_list <- as.list(c(2013:2016))

url_df <- function(year){
  url <- paste("https://en.wikipedia.org/wiki/Deaths_in_", month.name, "_", year, sep = "")
  data.frame(year, month.name, url)
}
df <- lapply(year_list, url_df) %>% rbind.fill()



##get the data
#for (i in 1:4) ##length(all_year_urls)
 #days <- read_html(urls[i]) %>% html_nodes(xpath = '//h3') %>% html_text()
#Sys.sleep(5)

get_day <- function(dataset, year, month, url){
  day <- read_html(as.character(dataset[url])) %>% html_nodes(xpath =  '//h3') %>% html_text()
  day <- gsub("\\[edit\\]", "", day) %>% as.numeric(as.character(day)) #remove the edit text and non-numerics
  day <- day[!is.na(day)] #get rid of NAs introduced in last part
  data.frame(dataset[year], dataset[month], day)
}

test <- get_day(df[1, ], "year", "month.name", "url")


test <- data.frame
for (i in 1:2){
  test1 <- get_day(df[i, 1], df[i, 2], df[i, 3])
  test <- rbind(test, test1)
}