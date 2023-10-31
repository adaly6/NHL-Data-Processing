# Andrew Daly
# 10/24/2023

# Clear the environment
rm(list=ls())

# Install xml2 library for web crawling
library(xml2)

# Set up emtpy vectors
all_team <- character(0) 
all_year <- character(0) 
all_wins <- character(0)
all_losses <- character(0)

# Set start time to know how long the scraping took to run
start_time <- Sys.time()

# Web crawl the page to get the data
for (i in 1:24){
  
  # Counter to know each time the bot moves to a new page
  print(i)
  
  # Set up a user agent and the page URL that will be scrapped
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36 Edg/117.0.2045.47"
  url <- paste("https://www.scrapethissite.com/pages/forms/?page_num=", i, sep = "" )
  
  # Read the HTML on the website and use a user agent to follow best practices of web crawling
  page <- read_html(url, user_agent)
  
  # Sleep for 5 seconds to follow best practices of web crawling
  Sys.sleep(5)
  
  # Scrape the team name and append it to the all_team vector
  team <- xml_text(xml_find_all(page, "//tr/td[1]")) 
  all_team <- c(all_team, team)
  
  # Scrap the year and append it to the all_year vector
  year <- xml_text(xml_find_all(page, "//tr/td[2]")) 
  all_year <- c(all_year, year)
  
  # Scrape the total wins that year and append it to the all_wins vector
  wins <- xml_text(xml_find_all(page, "//tr/td[3]")) 
  all_wins <- c(all_wins, wins)
  
  # Scrape the total losses that year and append it to the all_losses vector
  losses <- xml_text(xml_find_all(page, "//tr/td[4]")) 
  all_losses <- c(all_losses, losses)
  
  # Wait a random amount of time before moving to the next page
  random_value <- sample(2:20, 1)
  Sys.sleep(random_value)
}

# Set end time and take the difference between start time to know how long the bot took to scape
end_time <- Sys.time()
run_time <- end_time - start_time
print(run_time)

# Create nhl data frame from scraped values and rename columns
nhl <- data.frame(all_team, all_year, all_wins, all_losses)
colnames(nhl) <- c("Team", "Year", "Wins", "Losses")

# 2 Clean and transform nhl - strip white space and convert data types
nhl$Team <- trimws(nhl$Team)
nhl$Year <- trimws(nhl$Year)
nhl$Wins <- trimws(nhl$Wins)
nhl$Losses <- trimws(nhl$Losses)

nhl$Year <- as.integer(nhl$Year)
nhl$Wins <- as.integer(nhl$Wins)
nhl$Losses <- as.integer(nhl$Losses)

# 3 Read nhl_2012-2021.xlsx to merge vertically with nhl
library(readxl)
nhl_expanded <- read_excel("nhl_2012-2021.xlsx")

# Ensure column names and data types match
names(nhl_expanded)[names(nhl_expanded) == "W"] <- "Wins"
names(nhl_expanded)[names(nhl_expanded) == "L"] <- "Losses"
names(nhl_expanded)[names(nhl_expanded) == "Season"] <- "Year"
nhl_expanded$Year <- as.integer(nhl_expanded$Year)
nhl_expanded$Wins <- as.integer(nhl_expanded$Wins)
nhl_expanded$Losses <- as.integer(nhl_expanded$Losses)

# Remove the "*" from all team names
nhl_expanded$Team <- gsub("\\*$", "", nhl_expanded$Team) 

# Remove additional columns in nhl_expanded data frame by creating another df with relevant columns
nhl_expanded2 <- data.frame(nhl_expanded$Team, nhl_expanded$Year, nhl_expanded$Wins, nhl_expanded$Losses)
colnames(nhl_expanded2) <- c("Team", "Year", "Wins", "Losses")

# Merge the two data frames vertically
nhl2 <- rbind(nhl, nhl_expanded2)

# Create a new column for win percentage
nhl2$Win_percentage <- round((nhl2$Wins / (nhl2$Wins + nhl2$Losses)), 2)

# 4 Read nhl_hockey_arenas.csv and create data frame from data
arena <- read.csv("nhl_hockey_arenas.csv")

# Rename columns to match nhl2 data frame
names(arena)[names(arena) == "Team.Name"] <- "Team"
names(arena)[names(arena) == "Arena.Name"] <- "Arena"
names(arena)[names(arena) == "Arena.Location"] <- "Location"
names(arena)[names(arena) == "Seating.Capacity"] <- "Capacity"

# Drop unnecessary columns
arena$Opening.Year <- NULL

# Check for spelling error
error <- unique(arena$Team)
error

# Correct the spelling error
arena$Team <- ifelse(arena$Team == "Seattle Kracken", "Seattle Kraken", arena$Team)

# Correct the Ducks team name
nhl2$Team <- ifelse(nhl2$Team == "Mighty Ducks of Anaheim", "Anaheim Ducks", nhl2$Team)

# Horizontally merge arena data frame and nhl2
nhl3 <- merge(nhl2, arena, by = "Team", all.x = TRUE)

# Islanders have two stadiums in arenas data frame, solve problem based on year
nhl3$Arena <- ifelse(nhl3$Team == "New York Islanders",
                     ifelse(nhl3$Year >= 2012, "Nassau Coliseum",
                            ifelse(nhl3$Year < 2012, "Barclays Center", nhl3$Arena)),
                     nhl3$Arena)

# Create new location and capacity columns for the New York Islanders based on the arena
nhl3$Location <- ifelse(nhl3$Team == "New York Islanders",
                        ifelse(nhl3$Arena == "Nassau Coliseum", "Uniondale, New York",
                               ifelse(nhl3$Arena == "Barclays Center", "Brooklyn, New York", nhl3$Location)),
                        nhl3$Location)

nhl3$Capacity <- ifelse(nhl3$Team == "New York Islanders",
                        ifelse(nhl3$Arena == "Nassau Coliseum", 13900,
                               ifelse(nhl3$Arena == "Barclays Center", 15795, nhl3$Capacity)),
                        nhl3$Capacity)

# Remove exact row duplicates
nhl3 <- nhl3[!duplicated(nhl3), ]

# Save nhl3 data frame to hockey_data.csv
write.csv(nhl3, "hockey_data.csv", row.names = FALSE)

