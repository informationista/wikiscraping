library(rvest)
library(RCurl)
library(plyr)
library(tidyr)
library(ggplot2)

##create a list of URLs to retrieve
##create the list of years of interest
year_list <- as.list(c(2004:2016))

url_df <- function(year){
  url <- paste("https://en.wikipedia.org/wiki/Deaths_in_", month.name, "_", year, sep = "")
  data.frame(year, month.name, url)
}
df <- lapply(year_list, url_df) %>% rbind.fill()

##this part got the day only - this got combined with the next part where we get the day and the deaths at the same time
#get_day <- function(year, month, url){
#  day <- read_html(as.character(url)) %>% html_nodes(xpath =  '//h3') %>% html_text()
#  day <- gsub("\\[edit\\]", "", day) %>% as.numeric(as.character(day)) #remove the edit text and non-numerics
#  day <- day[!is.na(day)] #get rid of NAs introduced in last part
#  data.frame(year, month, day)
#}


##this function takes the data frame and retrieves the days and deaths on those days for each url
get_data <- function(year, month, url){
    day <- read_html(as.character(url)) %>% html_nodes(xpath =  '//h3') %>% html_text()
    day <- gsub("\\[edit\\]", "", day) %>% as.numeric(as.character(day)) #remove the edit text and non-numerics
    day <- day[!is.na(day)] #get rid of NAs introduced in last part
   df_mid <- data.frame(year, month, day, "death")
   df_mid$death <- as.character(df_mid$X.death.)
   df_mid <- df_mid[, -4] ##fix the data frame so it can take the character input from death
  death <- read_html(as.character(url)) %>%  html_nodes("div#mw-content-text.mw-content-ltr") %>% html_nodes("ul") %>% html_text()
  death <- death[c(-1, -2)] #remove the extraneous content
  for (i in 1:nrow(df_mid)){
    df_mid[i, 4] <- as.character(death[i])
  }
  return(df_mid)
}

##use the get day function to retrieve all data for all years/months in the list
df_data <- data.frame()
for (i in 1:nrow(df)){
  test1 <- get_data(df[i, 1], df[i, 2], df[i, 3])
  df_data <- rbind(df_data, test1)
}

##Now to clean up the death data
#remove citation links at end of line
df_data$death <- gsub("\\[[0-9]*\\]", "", df_data$death)

##split each person into their own line
df_data <- df_data %>% mutate(death = strsplit(as.character(death), "\n")) %>% unnest(death)

##remove any blank lines
df_data <- subset(df_data, death != "")

##split the name and age into separate columns, dump the other info
cleaned_df <- separate(df_data, col = death, into = c("name", "age"), sep = ",", remove = TRUE, extra = "drop")

##remove any stuff that got into the age column that's not an actual age
cleaned_df$age <- as.numeric(as.character(cleaned_df$age))

##put months in ordered factor for nice charts
cleaned_df$month <- factor(cleaned_df$month, levels = month.name)


##Let's make some charts!
qplot(year, data = cleaned_df, geom = "bar")


