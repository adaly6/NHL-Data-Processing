# Install xml2 library for web crawling
library(xml2)

# Set up emtpy vectors
all_team <- character(0)
all_year <- character(0)
all_wins <- character(0)
all_losses <- character(0)

# Web crawl the page to get the data
for (i in 1:24) {
       # Counter to know each time the bot moves to a new page
       print(i)

       # Set up the page URL that will be scrapped
       url <- paste("https://www.scrapethissite.com/pages/forms/?page_num=", i, sep = "")

       # Read the HTML on the website and use a user agent to follow best practices of web crawling
       page <- read_html(url)

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

# Create nhl data frame from scraped values and rename columns
nhl0 <- data.frame(all_team, all_year, all_wins, all_losses)
colnames(nhl0) <- c("team", "year", "wins", "losses")

# 2 Clean and transform nhl - strip white space and convert data types
nhl0$team <- trimws(nhl0$team)
nhl0$year <- trimws(nhl0$year)
nhl0$wins <- trimws(nhl0$wins)
nhl0$losses <- trimws(nhl0$losses)

nhl0$year <- as.integer(nhl0$year)
nhl0$wins <- as.integer(nhl0$wins)
nhl0$losses <- as.integer(nhl0$losses)

# 3 Read nhl_2012-2021.xlsx to merge vertically with nhl
library(readxl)
nhl_expanded <- read_excel("nhl_2012-2021.xlsx")

# Ensure column names and data types match
names(nhl_expanded)[names(nhl_expanded) == "Team"] <- "team"
names(nhl_expanded)[names(nhl_expanded) == "W"] <- "wins"
names(nhl_expanded)[names(nhl_expanded) == "L"] <- "losses"
names(nhl_expanded)[names(nhl_expanded) == "Season"] <- "year"
nhl_expanded$year <- as.integer(nhl_expanded$year)
nhl_expanded$wins <- as.integer(nhl_expanded$wins)
nhl_expanded$losses <- as.integer(nhl_expanded$losses)

# Remove the "*" from all team names
nhl_expanded$team <- gsub("\\*$", "", nhl_expanded$team)

# Remove additional columns in nhl_expanded data frame by creating another df with relevant columns
nhl_expanded2 <- data.frame(nhl_expanded$team, nhl_expanded$year, nhl_expanded$wins, nhl_expanded$losses)
colnames(nhl_expanded2) <- c("team", "year", "wins", "losses")

# Merge the two data frames vertically
nhl2 <- rbind(nhl0, nhl_expanded2)

# Create a new column for win percentage
nhl2$Win_percentage <- round((nhl2$wins / (nhl2$wins + nhl2$losses)), 2)

# 4 Read nhl_hockey_arenas.csv and create data frame from data
arena <- read.csv("nhl_hockey_arenas.csv")

# Rename columns to match nhl2 data frame
names(arena)[names(arena) == "Team.Name"] <- "team"
names(arena)[names(arena) == "Arena.Name"] <- "arena"
names(arena)[names(arena) == "Arena.Location"] <- "location"
names(arena)[names(arena) == "Seating.Capacity"] <- "capacity"

# Drop unnecessary columns
arena$Opening.Year <- NULL

# Check for spelling error
error <- unique(arena$team)
error

# Correct the spelling error
arena$team <- ifelse(arena$team == "Seattle Kracken", "Seattle Kraken", arena$team)

# Correct the Ducks team name
nhl2$team <- ifelse(nhl2$team == "Mighty Ducks of Anaheim", "Anaheim Ducks", nhl2$team)

# Horizontally merge arena data frame and nhl2
nhl <- merge(nhl2, arena, by = "team", all.x = TRUE)

# Islanders have two stadiums in arenas data frame, solve problem based on year
nhl$arena <- ifelse(nhl$team == "New York Islanders",
       ifelse(nhl$year >= 2012, "Nassau Coliseum",
              ifelse(nhl$year < 2012, "Barclays Center", nhl$arena)
       ),
       nhl$arena
)

# Create new location and capacity columns for the New York Islanders based on the arena
nhl$location <- ifelse(nhl$team == "New York Islanders",
       ifelse(nhl$arena == "Nassau Coliseum", "Uniondale, New York",
              ifelse(nhl$arena == "Barclays Center", "Brooklyn, New York", nhl$location)
       ),
       nhl$location
)

nhl$capacity <- ifelse(nhl$team == "New York Islanders",
       ifelse(nhl$arena == "Nassau Coliseum", 13900,
              ifelse(nhl$arena == "Barclays Center", 15795, nhl$capacity)
       ),
       nhl$capacity
)

# Remove exact row duplicates
nhl <- nhl[!duplicated(nhl), ]

# Save nhl3 data frame to hockey_data.csv
write.csv(nhl3, "hockey_data.csv", row.names = FALSE)


library(ggplot2)

# NHL teams with over 40 wins in 1990
ggplot(nhl[which(nhl$year == 1990 & nhl$wins > 40), c("team", "wins")], aes(x = reorder(team, wins), y = wins, fill = team)) +
       geom_bar(stat = "identity", width = .75) +
       coord_flip() +
       theme(legend.position = "none") +
       ggtitle("NHL Teams > 40 Wins in 1990") +
       xlab("NHL Team") +
       ylab("Wins")

# Filter data for Philadelphia Flyers
flyers_data <- nhl[nhl$team == "Philadelphia Flyers", ]
ggplot(flyers_data, aes(x = year, y = wins, color = team)) +
       geom_line() +
       ggtitle("Philadelphia Flyers Wins Over the Years") +
       xlab("Year") +
       ylab("Wins") +
       theme_minimal()

### Distribution of Win Percentages by Team
ggplot(nhl, aes(x = team, y = Win_percentage, fill = team)) +
       geom_boxplot() +
       ggtitle("Distribution of Win Percentages by Team") +
       xlab("NHL Team") +
       ylab("Win Percentage") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))

# stacked bar chart for win percentage over the years
ggplot(nhl, aes(x = year, y = Win_percentage, fill = team)) +
       geom_bar(stat = "identity") +
       ggtitle("NHL Win Percentage Over the Years") +
       xlab("Year") +
       ylab("Win Percentage") +
       theme_minimal()

# Barchart for arena capacities
ggplot(nhl, aes(x = reorder(team, -capacity), y = capacity, fill = team)) +
       geom_bar(stat = "identity") +
       ggtitle("NHL Arena Capacities") +
       xlab("NHL Team") +
       ylab("Capacity") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
       guides(fill = FALSE)
