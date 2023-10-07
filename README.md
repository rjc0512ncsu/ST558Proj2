ST 558 Project 2
================
Ryan Craft
2023-10-7

- [1 Specific functions with various query options for the
  API](#1-specific-functions-with-various-query-options-for-the-api)

Loading necessary libraries to read data, manipulate data, and plot
data.

``` r
library(jsonlite)
library(httr)
library(tidyr)
library(tibble)
```

Functions for Querying the API: \*\`\`\`{r functions, message=FALSE} \#
Base function to make a request to the balldontlie API fetch_data \<-
function(endpoint, query_parameters=list()) { base_url \<-
“<https://www.balldontlie.io/api/v1>” url \<- paste0(base_url, “/”,
endpoint)

response \<- GET(url, query=query_parameters) parsed_response \<-
fromJSON(content(response, “text”))

return(parsed_response\$data) }

# 1 Specific functions with various query options for the API

get_players \<- function(search=““, team_ids=NULL, page=1) {
fetch_data(”players”, list(search=search, team_ids=team_ids, page=page))
}

get_teams \<- function() { fetch_data(“teams”) }

get_games \<- function(start_date=NULL, end_date=NULL, team_ids=NULL,
page=1) { fetch_data(“games”, list(start_date=start_date,
end_date=end_date, team_ids=team_ids, page=page)) }

    Question:
    Within a given date range, which teams win the most games, and do these teams also have players that are the top scorers?

    *```{r eda, message=FALSE}
    # Fetch data for games within the specified date range
    games <- get_games(start_date="2022-10-01", end_date="2023-07-31")

    # Calculate the teams with the most wins
    most_wins <- games %>%
      filter(home_team_win == TRUE) %>% 
      group_by(home_team$id) %>%
      tally() %>%
      arrange(-n)

    top_teams <- most_wins$home_team.id[1:5] # Consider top 5 teams for further analysis

    # Fetch players' stats in the same date range for the top teams
    players_stats <- lapply(top_teams, function(team_id) {
      fetch_data("stats", list(team_ids = team_id, start_date="2022-01-01", end_date="2022-12-31"))
    })

    # For simplicity, let's analyze points
    player_points <- lapply(players_stats, function(stats) {
      stats %>%
        group_by(player_id) %>%
        summarise(total_points = sum(pts, na.rm=TRUE)) %>%
        arrange(-total_points)
    })

    # Extract top 3 players from each team based on points
    top_players <- lapply(player_points, function(points) {
      head(points, 3)
    })

    # Display the results
    top_players
