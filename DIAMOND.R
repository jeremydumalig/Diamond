library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
rm(list=ls())

raw_wnba <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/wnba_2019.csv")
raw_nba <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/nba_2019.csv")

hundred <- function(x){return(round(percent_rank(x)*100))}

standardize <- function(x){
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  return( (x - mu)/sigma )
}

comp_factors = c(
  'Player',
  'PTS',
  'FG%',
  '3P%',
  'FT%',
  'eFG%',
  'TRB',
  'AST',
  'STL',
  'TOV',
  'BLK'
)

wnba_comp <- 
  raw_wnba %>%
  select(comp_factors) %>%
  mutate_at(vars(-Player), standardize)
nba_comp <- 
  raw_nba %>%
  select(comp_factors) %>%
  mutate_at(vars(-Player), standardize)

wnba_perc <- 
  raw_wnba %>%
  select(comp_factors) %>%
  mutate_at(vars(-Player), hundred)
nba_perc <- 
  raw_nba %>%
  select(comp_factors) %>%
  mutate_at(vars(-Player), hundred)

wnba_perc[is.na(wnba_perc)] <- 0
nba_perc[is.na(nba_perc)] <- 0
wnba_comp[is.na(wnba_comp)] <- 0
nba_comp[is.na(nba_comp)] <- 0

gg_theme <-
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(fill = "grey90", color = "black"),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15)
  )

spacejam <- function(league_argument, player_argument){
  if (league_argument == 'NBA'){
    player_percentiles <- nba_perc
    hex_code <- "#FF0000"
  } else if (league_argument == 'WNBA'){
    player_percentiles <- wnba_perc
    hex_code <- "#6EA8DB"
  }
  
  player_percentiles <- 
    player_percentiles %>%
    filter(Player == player_argument) %>%
    gather(Categories, Percentile, -Player) %>%
    spread(Player, Percentile) 
  
  ggplot(data = player_percentiles) +
    coord_flip(ylim = c(0, 100)) + 
    theme_linedraw() +
    theme(legend.position = "none") + 
    theme(axis.title.y = element_blank()) + 
    ylab("Percentile") + 
    ggtitle(player_argument) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_segment(aes(x=Categories, xend=Categories, y=min(0), yend=max(100)), linetype="dashed", size=0.25) +
    geom_label(aes(x=Categories, y=player_percentiles[[2]], label=player_percentiles[[2]]), fill=hex_code, size=5) +
    gg_theme
}

diamond <- function(league, player_name){
  if (league == 'NBA'){
    nba_tbl <- filter(nba_comp, Player == player_name)
    row_indexer <- wnba_comp
  } else if (league == 'WNBA'){
    wnba_tbl <- filter(wnba_comp, Player == player_name)
    row_indexer <- nba_comp
  }
  
  comparisons <- data.frame(
    WNBA_Player=character(),
    NBA_Player=character(),
    DIAMOND=numeric()
  )
  
  for (col_index in seq(1, nrow(row_indexer), length=nrow(row_indexer)) ) {
    if (league == 'NBA'){
      wnba_tbl <- filter(row_indexer, row_number() == col_index)
    } else if (league == 'WNBA'){
      nba_tbl <- filter(row_indexer, row_number() == col_index)
    }
    
    diamond_rating <- 0
    for (comp_col in seq(2, length(comp_factors), length=length(comp_factors) - 1) ) {
      difference <- pull(wnba_tbl, comp_factors[comp_col]) - pull(nba_tbl, comp_factors[comp_col])

      if (comp_col == 4){
        diamond_rating <- diamond_rating + abs(difference*difference)
      } else {
        diamond_rating <- diamond_rating + abs(difference)
      }
    }
    
    comparisons <- add_row(
      comparisons,
      WNBA_Player = pull(wnba_tbl, Player),
      NBA_Player = pull(nba_tbl, Player),
      DIAMOND = round(diamond_rating, 2)
    )
  }
  
  comparisons <- arrange(comparisons, DIAMOND)
  View(comparisons)

  diamond_text <- paste0("DIAMOND Rating: ", comparisons[[3]][1])
  
  p1 <- spacejam(league, player_name)
  if (league == 'NBA'){
    p2 <- spacejam('WNBA', comparisons[[1]][1])
  } else if (league == 'WNBA'){
    p2 <- spacejam('NBA', comparisons[[2]][1])
  }
  grid.arrange(p1, p2, nrow=1)
  
  return(comparisons[[3]][1])
}

diamond('WNBA', "Diamond DeShields")
