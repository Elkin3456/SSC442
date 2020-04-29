library(ggplot2)
library(dplyr)
mast <- read.csv('https://raw.githubusercontent.com/Elkin3456/SSC442/master/mlb20yr_abs.csv')

al_teams <- mast %>% 
  filter(team %in% c("ANA", "BAL", "BOS", "CHA", "CLE", "DET", "HOU", "KCA", "MIN", "NYA", "OAK", "SEA",
                     "TBA", "TEX", "TOR"))

al_teams %>%
  ggplot( aes(x=year_x, y=hits_t, group=team, color=team)) +
  geom_line() + labs(title = "AL Team's Hits Over Time", x= "Year", y="Hits")

al_teams %>%
  ggplot( aes(x=year_x, y=time, group=team, color=team)) +
  geom_line() + labs(title = "AL Team's Game Length Over Time", x="Year", y="Game Length(minutes)")

al_teams %>%
  ggplot( aes(x=year_x, y=att, group=team, color=team)) +
  geom_line() + labs(title = "AL Team's Attendance Over Time", x="Year", y="Attendance")


nl_teams <- mast %>%
  filter(team %in% c("ARI", "ATL", "CHN", "CIN", "COL", "LAN", "MIA", "MIL", "NYN", "PHI", "PIT", "SDN", "SFN", 
                     "SLN", "WAS"))
nl_teams %>%
  ggplot( aes(x=year_x, y=hits_t, group=team, color=team)) +
  geom_line() + labs(title = "NL Team's Hits Over Time", x= "Year", y="Hits")

nl_teams %>%
  ggplot( aes(x=year_x, y=time, group=team, color=team)) +
  geom_line() + labs(title = "NL Team's Game Length Over Time", x="Year", y="Game Length(minutes)")

nl_teams %>%
  ggplot( aes(x=year_x, y=att, group=team, color=team)) +
  geom_line() + labs(title = "NL Team's Attendance Over Time", x="Year", y="Attendance")