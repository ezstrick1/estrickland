library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
#one team per season per row

season <- joined %>%
  group_by(team_name, year, playoffs)%>%
  summarise(wins = max(wins),
            home = max(home),
            playoffs = max(playoffs),
            avg_home = mean(home),
            .groups = "drop")

glimpse(season)

#---wins vs home attendance: do they correlate?
ggplot(season, aes(x=wins, y=home, color = playoffs)) +
  geom_point()+
  scale_y_continuous(labels = label_comma())+
  geom_smooth()+
  labs(title="NFL Home Attendance vs Wins (2000-2019)", y = "Total Home Attendance", x = "Total Team Wins"  )

  ggsave("attendance_vs_wins.png")

#---loyalty
loyalty <- joined %>%
  group_by(team_name)%>%
  summarise(avg_home = mean(home),
            .groups = "drop")

nfl_colors <- c(
  "49ers"      = "#AA0000",
  "Bears"      = "#00143F",
  "Bengals"    = "#FB4F14",
  "Bills"      = "#00338D",
  "Broncos"    = "#002244",
  "Browns"     = "#311D00",
  "Buccaneers" = "#D50A0A",
  "Cardinals"  = "#97233F",
  "Chargers"   = "#0080C6",
  "Chiefs"     = "#E31837",
  "Colts"      = "#003D79",
  "Cowboys"    = "#003594",
  "Dolphins"   = "#008E97",
  "Eagles"     = "#004C54",
  "Falcons"    = "#A71930",
  "Giants"     = "#0B2265",
  "Jaguars"    = "#101820",
  "Jets"       = "#004D25",
  "Lions"      = "#0076B6",
  "Packers"    = "#203731",
  "Panthers"   = "#0085CA",
  "Patriots"   = "#002244",
  "Raiders"    = "#000000",
  "Rams"       = "#003594",
  "Ravens"     = "#241773",
  "Redskins"   = "#773141",
  "Saints"     = "#D3BC8D",
  "Seahawks"   = "#002244",
  "Steelers"   = "#FFB612",
  "Texans"     = "#03202F",
  "Titans"     = "#0C2340",
  "Vikings"    = "#4F2683"
)

ggplot(loyalty, aes(x = reorder(team_name, avg_home), y = avg_home,fill = team_name )) +
  geom_col()+
  coord_flip()+
  scale_fill_manual(values = nfl_colors)+
  scale_y_continuous(labels = label_comma())+
  theme(legend.position = "none")+
  labs(title = "Team Fan Loyalty", 
       x = "Team Name", 
       y = "Average Home Attendance")


ggsave("fan_loyalty.png")


playoff_avg <- season %>%
  group_by(playoffs)%>%
  summarise(avg_home = mean(home), .groups = "drop")

ggplot(playoff_avg, aes(x=avg_home, y=playoffs, fill = playoffs))+
  geom_col()+
  coord_cartesian(xlim = c(400000, 600000)) +
  scale_x_continuous(labels = label_comma())+
  theme(legend.position = "none") +
  labs(title = "Playoff vs. Non-Playoff Attendance", 
       x= "Attendance", 
       y = "Playoffs")

