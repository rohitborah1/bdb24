## Big Data Bowl 2024
## Play Animation Test
## October 2023

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gganimate))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(ggforce))
suppressPackageStartupMessages(library(magick))
suppressPackageStartupMessages(library(gifski))
suppressPackageStartupMessages(library(stringr))

games <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/games.csv")
players <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/players.csv")
plays <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/plays.csv")
tkl <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tackles.csv")

df <-
  read.csv("/Users/rohit/Data Science/BDB24/nfl-big-data-bowl-2024/tracking_week_1.csv")


# kBuild relevant functions  --------------------------------------------------------

fetch_team_colors <- function() {
  team_colors_ <- suppressMessages(readr::read_tsv("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/data/nfl_team_colors.tsv"))
  # add football color
  team_colors_ = rbind(team_colors_, c("football","#935e38","black","#935e38"))
  return(team_colors_)
}
team_colors_ <- fetch_team_colors()

fetch_play <- function(df,playId_,gameId_)
{
  # ' Function that returns a play dataframe with its correspondent information
  play <- df %>% 
    filter(gameId == gameId_ & playId == playId_) %>% 
    left_join(plays, by = c("playId" = "playId", "gameId" = "gameId")) %>% 
    left_join(team_colors_, by = c("club"="teams"))
  return(play)
}

# kPlot field --------------------------------------------------------------

field_height <- 160/3
field_width <- 120

plot_field <- function(field_color="#00b140", line_color = "#ffffff") {
  field_height <- 160/3
  field_width <- 120
  
  field <- ggplot() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(hjust = 1),
      legend.position = "bottom",
      # legend.title = element_text(color = "#212529", size = 12, vjust = 1),
      legend.title.align = 1,
      # legend.text = element_text(color = "#343a40", size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      # panel.background = element_blank(),
      panel.background = element_rect(fill = field_color, color = "white"),
      panel.border = element_blank(),
      aspect.ratio = field_height/field_width
    ) +
    # major lines
    annotate(
      "segment",
      x = c(0, 0, 0,field_width, seq(10, 110, by=5)),
      xend = c(field_width,field_width, 0, field_width, seq(10, 110, by=5)),
      y = c(0, field_height, 0, 0, rep(0, 21)),
      yend = c(0, field_height, field_height, field_height, rep(field_height, 21)),
      colour = line_color
    ) +
    # hashmarks
    annotate(
      "segment",
      x = rep(seq(10, 110, by=1), 4),
      xend = rep(seq(10, 110, by=1), 4),
      y = c(rep(0, 101), rep(field_height-1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101)),
      yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101)),
      colour = line_color
    ) +
    # yard numbers
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      size = 7,
      family = "mono",
      colour = line_color, # "#495057",
    ) +
    # yard numbers upside down
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(field_height-12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      angle = 180,
      size = 7,
      family = "mono",
      colour = line_color, 
    )
  
  return(field)
}

plot_field()


# kPlot frame for one play -------------------------------------------------

plot_frame <- function(onePlay_, frame_, plot_vel_ = F)
{
  #  Take one frame from one play, plot a scatter plot image.
  
  # Check for data
  if(is.null(onePlay_)) {
    print("error: need to provide play data")
    return()
  }
  if(is.null(frame_)) {
    print("error: need to provide frame of play to visualize")
    return()
  }
  # * get play metadata ----
  play_desc <- onePlay_$playDescription %>% .[1]
  play_dir <- onePlay_$playDirection %>% .[1]
  yards_togo <- onePlay_$yardsToGo %>% .[1]
  los <- onePlay_$absoluteYardlineNumber %>% .[1]
  togo_line <-if(play_dir=="left") los-yards_togo else los+yards_togo
  
  fr <- onePlay_ %>% 
    filter(frameId == frame_)
  
  colores        <- unique(fr$color2)
  names(colores) <- colores
  
  #  velocity angle in radians
  fr$dir_rad <- fr$dir * pi / 180
  
  #  velocity components
  fr$v_x <- sin(fr$dir_rad) * fr$s
  fr$v_y <- cos(fr$dir_rad) * fr$s
  
  if(plot_vel_ == T)
  {
    # one frame scatterplot
    one_frame <- plot_field() +
      # line of scrimmage
      annotate(
        "segment",
        x = los, xend = los, y = 0, yend = 160/3,
        colour = "#0d41e1"
      ) +
      # 1st down marker
      annotate(
        "segment",
        x = togo_line, xend = togo_line, y = 0, yend = 160/3,
        colour = "#f9c80e"
      )+
      geom_point(
        data = fr,
        mapping = aes(x = x, y = y, color = color2 )
      ) +
      geom_segment(
        data = fr,
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y, color =color2 ),
        linewidth = 1, arrow = arrow(length = unit(0.1, "cm"))
      ) + 
      scale_colour_manual(values = colores) +
      labs(
        title = play_desc,
        caption = "Data: Big Data Bowl 2024 \n Rohit Borah"
      ) +
      theme(legend.position="none")
  }
  else
  {
    # one frame scatterplot
    one_frame <- plot_field() +
      # line of scrimmage
      annotate(
        "segment",
        x = los, xend = los, y = 0, yend = 160/3,
        colour = "#0d41e1"
      ) +
      # 1st down marker
      annotate(
        "segment",
        x = togo_line, xend = togo_line, y = 0, yend = 160/3,
        colour = "#f9c80e"
      )+
      geom_point(
        data = fr,
        mapping = aes(x = x, y = y, color = color2 )
      ) +
      scale_colour_manual(values = colores) +
      labs(
        title = play_desc,
        caption = "Data: Big Data Bowl 2024 \n Rohit Borah"
      ) +
      theme(legend.position="none")
  }
  
  
  
  
  
  return(one_frame)
  
}

df_rushes <- df_plays %>% filter(gameId < 2022091500) %>% filter(is.na(passLength)) %>% arrange(desc(playResult))

playId_ = 1948
gameId_ = 2022091108

play <- fetch_play(df, playId_, gameId_)
head(play)
colnames(play)

print(play$playDescription %>% .[1])

plot_frame(play, 1, TRUE)


y# kAnimate play. ----------------------------------------------------------

play_animation <- function(onePlay_ , plot_vel_ = F)
{
  
  # Check for data
  if(is.null(onePlay_)) {
    print("error: need to provide play data")
    return()
  }
  # * get play metadata ----
  play_desc <- onePlay_$playDescription %>% .[1]
  play_dir <- onePlay_$playDirection %>% .[1]
  yards_togo <- onePlay_$yardsToGo %>% .[1]
  los <- onePlay_$absoluteYardlineNumber %>% .[1]
  togo_line <- if(play_dir=="left") los-yards_togo else los+yards_togo
  
  colores        <- unique(onePlay_$color2)
  names(colores) <- colores
  
  #  velocity angle in radians
  onePlay_$dir_rad <- onePlay_$dir * pi / 180
  
  #  velocity components
  onePlay_$v_x <- sin(onePlay_$dir_rad) * onePlay_$s
  onePlay_$v_y <- cos(onePlay_$dir_rad) * onePlay_$s
  
  if (plot_vel_ == T)
  {
    anim <- plot_field() +
      # line of scrimmage
      annotate(
        "segment",
        x = los, xend = los, y = 0, yend = 160/3,
        colour = "#0d41e1"
      ) +
      # 1st down marker
      annotate(
        "segment",
        x = togo_line, xend = togo_line, y = 0, yend = 160/3,
        colour = "#f9c80e"
      )+
      geom_point(
        data = onePlay_,
        mapping = aes(x = x, y = y, color = color2 ),
        size = 4
      ) +
      geom_segment(
        data = onePlay_,
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y, color =color2),
        size = 1, arrow = arrow(length = unit(0.005, "npc"))
      ) + 
      scale_colour_manual(values = colores) +
      labs(
        title = play_desc,
        caption = "Data: Big Data Bowl 2024 \n Rohit Borah"
      ) +
      theme(legend.position="none")+
      # animation stuff
      transition_time(frameId) +
      ease_aes('linear') +
      NULL  
  }
  
  else 
  {
    anim <- plot_field() +
      # line of scrimmage
      annotate(
        "segment",
        x = los, xend = los, y = 0, yend = 160/3,
        colour = "#0d41e1"
      ) +
      # 1st down marker
      annotate(
        "segment",
        x = togo_line, xend = togo_line, y = 0, yend = 160/3,
        colour = "#f9c80e"
      )+
      geom_point(
        data = onePlay_,
        mapping = aes(x = x, y = y, color = color2 ),
        size = 4
      ) +
      scale_colour_manual(values = colores) +
      labs(
        title = play_desc,
        caption = "Data: Big Data Bowl 2024 \n Rohit Borah"
      ) +
      theme(legend.position="none")+
      # animation stuff
      transition_time(frameId) +
      ease_aes('linear') +
      NULL
  }
  
  
  
  return(anim)
}

# *ensure length of play matches number of frames
play_length <- length(unique(play$frameId))

# customize duration of gif
duration_ = 7

# Animate our play! :D
p_anim <- animate(
  play_animation(play, plot_vel_ = T),
  duration = 10,
  fps = 30, 
  nframe = play_length,
  width = 850,
  height = 500,
  end_pause = 1,
  renderer = gifski_renderer()
)

print(p_anim)
