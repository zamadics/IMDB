# IMDB Rating Scrape and Graph

# in case you cannot decide on what to scrape, here is the greatest TV show of all time:
# sopraons: "http://www.imdb.com/title/tt0141842/"

# required packages
library(rvest)
library(ggplot2)

# the base
main <- "http://www.imdb.com"

# change the address to the show you are interested in, it is the same untill what follows after "tt"
# the second greatest TV show of all time
main.page <- html(x = "http://www.imdb.com/title/tt0804503/")

# get the title of the show
title <- main.page %>% # feed `main.page` to the next step
  html_nodes("#ratingWidget strong") %>% 
  html_text()

# set up the main page
urls <- main.page %>% 
  html_nodes(".clear+ div a") %>% 
  html_attr("href") 
urls <- rev(urls)

#create an empty list to fill in with ratings
res <- list(NA)

# for loop that scans each season
for(j in 1:length(urls)){
  season.page <- paste0(main, urls[j])
  season.page <- html(x = season.page)
  
  # Get link text
  links <- season.page %>% 
    html_nodes("#episodes_content strong a") %>% 
    html_attr("href")
  
  
  results <- matrix(NA, nrow = length(links), ncol = 1)
  # now by episode
  for(i in 1:length(links)){
    episode <- paste0(main, links[i])
    
    text <- read_html(episode) %>% 
      html_nodes("strong span") %>% 
      html_text() 
    results[i,1] <- as.numeric(text)
    
  }
  
  #combine results in a list
  res[[j]] <- results
  print(j)
}

# unlist ratings
rating <- unlist(res)

# create a season tag for each episode
res.seas <- list(NA)
for(i in 1:length(res)){
  len <- length(res[[i]])
  season <- paste("Season", i, sep = " ")
  res.seas[[i]] <- rep(season, len)
}

# unlist the season tags
season <- unlist(res.seas)

# create data to be used in plot
gdat <- data.frame(rating, season)
gdat$episode <- 1:nrow(gdat)
colnames(gdat) <- c("Rating", "Season", "Episode")

# create title of plot
gtitle <- paste(title, "Ratings", sep = " ")

# plot
ggplot(gdat, aes(Episode, Rating, color = factor(Season)))+
  geom_path(aes(group = 1)) +
  geom_point() + ggtitle(gtitle) + scale_color_discrete(name = "")





