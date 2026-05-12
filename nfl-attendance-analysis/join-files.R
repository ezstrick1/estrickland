library(dplyr)
library(readr)


attendance <- read_csv("~/Desktop/R Proj/data/NFL/attendance.csv")
games <- read_csv("~/Desktop/R Proj/data/NFL/games.csv")
standings <- read_csv("~/Desktop/R Proj/data/NFL/standings.csv")

left_join(attendance, standings, by = c("team", "year"))

joined <- left_join(attendance, standings, by = c("team", "year"))

#---rename and delete col names
joined <- joined %>%
  select(-team_name.y)%>%
  rename(team_name = team_name.x)
