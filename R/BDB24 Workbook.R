## Big Data Bowl 2024
## Initial Data Import Workbook
## October 2023


# Load requisite packages & set global settings. --------------------------

library(tidyverse) #
library(ggrepel)
library(lubridate)
library(plotly)
library(gganimate)
library(nflverse)

## Increase number of columns / rows shown in df
# options(repr.matrix.max.cols = 500,
#         repr.matrix.max.rows = 200)

## Eliminate (mostly) unnecessary scientific notation
options(scipen = 9999)

# Import and manipulate original datasets. -------------------------------------

list.files(path = "/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024")

df_games <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/games.csv")

df_players <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/players.csv")
df_players$position <- as.factor(df_players$position)
df_players$position_group <- NA
df_players$position_unit <- NA
df_players$position_group[grepl("DT", df_players$position)] <- "DL"
df_players$position_group[grepl("C", df_players$position)] <- "OL"
df_players$position_group[grepl("CB", df_players$position)] <- "DB"
df_players$position_group[grepl("DB", df_players$position)] <- "DB"
df_players$position_group[grepl("DE", df_players$position)] <- "DL"
df_players$position_group[grepl("FB", df_players$position)] <- "RB"
df_players$position_group[grepl("FS", df_players$position)] <- "DB"
df_players$position_group[grepl("G", df_players$position)] <- "OL"
df_players$position_group[grepl("ILB", df_players$position)] <- "LB"
df_players$position_group[grepl("LS", df_players$position)] <- "ST"
df_players$position_group[grepl("MLB", df_players$position)] <- "LB"
df_players$position_group[grepl("NT", df_players$position)] <- "DL"
df_players$position_group[grepl("OLB", df_players$position)] <- "LB"
df_players$position_group[grepl("QB", df_players$position)] <- "QB"
df_players$position_group[grepl("RB", df_players$position)] <- "RB"
df_players$position_group[grepl("SS", df_players$position)] <- "DB"
df_players$position_group[grepl("T", df_players$position)] <- "OL"
df_players$position_group[grepl("TE", df_players$position)] <- "TE"
df_players$position_group[grepl("WR", df_players$position)] <- "WR"

df_players$position_unit[grepl("DL", df_players$position_group)] <- "Defense"
df_players$position_unit[grepl("DB", df_players$position_group)] <- "Defense"
df_players$position_unit[grepl("LB", df_players$position_group)] <- "Defense"
df_players$position_unit[grepl("OL", df_players$position_group)] <- "Offense"
df_players$position_unit[grepl("QB", df_players$position_group)] <- "Offense"
df_players$position_unit[grepl("RB", df_players$position_group)] <- "Offense"
df_players$position_unit[grepl("ST", df_players$position_group)] <- "ST"
df_players$position_unit[grepl("TE", df_players$position_group)] <- "Offense"
df_players$position_unit[grepl("WR", df_players$position_group)] <- "Offense"

df_players$position_group <- as.factor(df_players$position_group)
df_players$position_unit <- as.factor(df_players$position_unit)

df_plays <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/plays.csv")
df_plays$quarter <- as.factor(df_plays$quarter)
df_plays$down <- as.factor(df_plays$down)
df_plays$defendersInTheBox <- as.factor(df_plays$defendersInTheBox)
df_plays$ballCarrierId <- as.factor(df_plays$ballCarrierId)
df_plays$foulNFLId1 <- as.factor(df_plays$foulNFLId1)
df_plays$foulNFLId2 <- as.factor(df_plays$foulNFLId2)

df_tkl <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tackles.csv")

df_tkl$tackle <- as.integer(df_tkl$tackle)
df_tkl$assist <- as.integer(df_tkl$assist)

# Join Datasets to create working df --------------------------------------

## Join tackle data to player data
df_tkl_player <- left_join(df_tkl, df_players, by = 'nflId')

## Join tackles/players with play data

df <- left_join(df_tkl_player, df_plays, by = c('gameId', 'playId'))
head(df)

# df <- cbind("uniquePlayId" = as.factor(paste0(df$gameId, '-', df$playId)), df)

# unique id
# df <-
#   cbind("id" = as.factor(paste0(df$uniquePlayId, df$nflId)), df)

# head(df)
# str(df)
# 
# summary(df$position)
# summary(df$position_group)



# Tackles -----------------------------------------------------------------

## Which positions make the most tackles?

tkls_by_player <- df %>%
  group_by(nflId, displayName, position, position_group, height, weight) %>%
  summarize(tkls_made = sum(tackle, na.rm = TRUE) + sum(assist, na.rm = TRUE),
            tkls_missed = sum(pff_missedTackle, na.rm = TRUE),
            tkls_missed_pct = tkls_missed / (tkls_made + tkls_missed) * 100, .groups = "drop") %>%
  arrange(desc(tkls_made))

tkls_by_position <- tkls_by_player %>%
  group_by(position, position_group) %>%
  summarize(tkls_made = sum(tkls_made),
            tkls_missed = sum(tkls_missed))

100 - (sum(tkls_by_position$tkls_made) / (sum(tkls_by_position$tkls_made) + sum(tkls_by_position$tkls_missed)) * 100)

tkls_by_position <- tkls_by_position %>%
  filter(tkls_made > 50) %>%
  group_by(position, position_group, tkls_made, tkls_missed) %>%
  summarize(tkls_missed_pct = tkls_missed / (tkls_made + tkls_missed) * 100, .groups = "drop")

df_speedByPlayer <- df_speedByPG %>%
  group_by(nflId) %>%
  summarize(max_mph = max(s, na.rm = TRUE) / 0.488889) %>%
  arrange(desc(max_mph))

tkls_by_position <- tkls_by_player

df %>%
  group_by(nflId, displayName, position, position_group, height, weight)

tkls_by_position %>%
  ggplot(aes(x=position_group, y = tkls_missed_pct)) + 
  geom_bar(stat="identity")

tkls_by_player %>%
  ggplot(aes(x = position_group, y = tkls_missed, fill = position)) + 
  geom_bar(stat = "identity")

tkl_by_position %>%
  filter(tkls_made >= 50) %>%
  ggplot(aes(x=tkls_made, fill = position_group)) + 
  geom_bar()

tkl_by_position <- summary(tkl_by_position)




# Speed by Position Group -------------------------------------------------

## Create new df by joining tracking and player data.
df_speedByPG <- left_join(df_trk1, df_players, by = c('nflId', 'displayName')) %>%
  filter(!displayName == "football")

## Create new table with player ID and max miles per hour, converting yds/sec to mph
#### Probably a cleaner way to do this without this step but idk
df_speedByPlayer <- df_speedByPG %>%
  group_by(nflId) %>%
  summarize(max_mph = max(s, na.rm = TRUE) / 0.488889) %>%
  arrange(desc(max_mph))

## Temporary table for slide
df_speedByPG %>%
  group_by(displayName, club, position, height, weight) %>%
  summarize(max_mph = max(s, na.rm = TRUE) / 0.488889, .groups = 'drop') %>%
  arrange(desc(max_mph))

## Join table with speed by player to main table.
df_speedByPG <- left_join(df_speedByPG, df_speedByPlayer, by = 'nflId')

## Violin plot of speed by position group.
df_speedByPG %>%
  filter(!is.na(position_group)) %>%
  mutate(position_group = fct_reorder(position_group, (max_mph))) %>%
  ggplot(aes(x=position_group, y = max_mph, fill = position_unit)) + 
  geom_violin() +
  stat_summary(
    fun = "median",
    geom = "crossbar",
    linewidth = 0.25,
    width = 0.8
  ) + 
  coord_flip() + 
  labs(
    title = "NFL Players: Max Speed by Position Group",
    subtitle = "Source: Next Gen Tracking Data",
    x = "Position Group",
    y = "Max Speed (MPH)",
    fill = "Unit", 
    caption = "Crossbar indicates median \n Big Data Bowl 2024 \n Rohit Borah") + 
  theme(
    legend.position = "top"
  )
    
# Workspace ---------------------------------------------------------------

## Violin Plot for Weight by Position Group
# df_players %>%
#   filter(!position_group == "ST") %>%
#   mutate(position_group = fct_reorder(position_group, weight)) %>%
#   ggplot(aes(x=position_group, y=weight, fill=position_group)) + 
#   geom_violin() + 
#   coord_flip() + 
#   labs(
#     title = "NFL Players: Weight by Position Group",
#     subtitle = "Source: NFL Big Data Bowl 2024",
#     x = "Position Group",
#     y = "Weight (lbs)"
#   ) + 
#   theme(legend.position = "none")


# playId_ = 1162
# gameId_ = 2022092600
# 
# list.files(path = "../Data Science/BDB24")
# 
# files <-
#   system('ls -l "../Data Science/BDB24/nfl-big-data-bowl-2024"', intern = TRUE)
# print(files)

## kaggle competitions download - c nfl - big - data - bowl - 2024

## export PATH =  / Users / rohit / Library / Python / 3.11 / bin:$PATH


# Import tracking data. ---------------------------------------------------

df_trk1 <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tracking_week_1.csv")
df_trk2 <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tracking_week_2.csv")
df_trk3 <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tracking_week_3.csv")
df_trk4 <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tracking_week_4.csv")
df_trk5 <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tracking_week_5.csv")
df_trk6 <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tracking_week_6.csv")
df_trk7 <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tracking_week_7.csv")
df_trk8 <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tracking_week_8.csv")
df_trk9 <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tracking_week_9.csv")

df_trk <-
  rbind(df_trk1,
        df_trk2,
        df_trk3,
        df_trk4,
        df_trk5,
        df_trk6,
        df_trk7,
        df_trk8,
        df_trk9)

