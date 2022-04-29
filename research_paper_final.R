library(tidyverse)
library(nhlapi)
library(writexl)
library(kableExtra)
## ** DO NOT RUN THIS CODE BELOW IF YOU HAVE THE .Rdata FILE. GO FURTHER DOWN TO LOAD DATA **
{
  ##NHL API REQUESTS
  ##2012 SEASON REMOVED DUE TO A LOCKOUT
  ##2019 AND 2020 SEASONS REMOVED DUE TO COVID-19
  {
    ##1st 4 numbers are year, next 2 numbers are game type
    ##(01=preseason, 02=regular season, 03=playoffs, 04=all-star game)
    ##last 4 numbers are game # (regular season has games 0001-1230)
    get_games2010 <- lapply( 
      2010020001:2010021230,
      nhlapi::nhl_games_feed)
    
    get_games2011 <- lapply(
      2011020001:2011021230,
      nhlapi::nhl_games_feed)
    
    get_games2013 <- lapply(
      2013020001:2013021230,
      nhlapi::nhl_games_feed)
    
    get_games2014 <- lapply(
      2014020001:2014021230,
      nhlapi::nhl_games_feed)
    
    get_games2015 <- lapply(
      2015020001:2015021230,
      nhlapi::nhl_games_feed)
    
    get_games2016 <- lapply(
      2016020001:2016021230,
      nhlapi::nhl_games_feed)
    
    get_games2017 <- lapply(
      2017020001:2017021271,
      nhlapi::nhl_games_feed)
    
    get_games2018 <- lapply(
      2018020001:2018021271,
      nhlapi::nhl_games_feed)
    
    get_playoffgames2010 <- lapply( 
      2010030111:2010030417,
      nhlapi::nhl_games_feed)
    get_playoffgames2010 <- compact(get_playoffgames2010)
    
    get_playoffgames2011 <- lapply(
      2011030111:2011030417,
      nhlapi::nhl_games_feed)
    get_playoffgames2011 <- compact(get_playoffgames2011)
    
    get_playoffgames2013 <- lapply(
      2013030111:2013030417,
      nhlapi::nhl_games_feed)
    get_playoffgames2013 <- compact(get_playoffgames2013)
    
    get_playoffgames2014 <- lapply(
      2014030111:2014030417,
      nhlapi::nhl_games_feed)
    get_playoffgames2014 <- compact(get_playoffgames2014)
    
    get_playoffgames2015 <- lapply(
      2015030111:2015030417,
      nhlapi::nhl_games_feed)
    get_playoffgames2015 <- compact(get_playoffgames2015)
    
    get_playoffgames2016 <- lapply(
      2016030111:2016030417,
      nhlapi::nhl_games_feed)
    get_playoffgames2016 <- compact(get_playoffgames2016)
    
    get_playoffgames2017 <- lapply(
      2017030111:2017030417,
      nhlapi::nhl_games_feed)
    get_playoffgames2017 <- compact(get_playoffgames2017)
    
    get_playoffgames2018 <- lapply(
      2018030111:2018030417,
      nhlapi::nhl_games_feed)
    get_playoffgames2018 <- compact(get_playoffgames2018)
  }
  ##function taken from nhlapi package to pull game play data points from game feeds
  getPlaysDf <- function(x) {
    playsRes <- try(x[[1L]][["liveData"]][["plays"]][["allPlays"]]) 
    if (inherits(playsRes, "try-error")) data.frame() else playsRes
  }
  ##CREATE DF FOR ALL PLAYS IN EACH SEASON
  {
    ##put game play data points into a workable data.frame
    reg_szn_plays_2010 <- lapply(get_games2010, getPlaysDf)
    plays2010 <- nhlapi:::util_rbindlist(reg_szn_plays_2010)
    
    reg_szn_plays_2011 <- lapply(get_games2011, getPlaysDf)
    plays2011 <- nhlapi:::util_rbindlist(reg_szn_plays_2011)
    
    reg_szn_plays_2013 <- lapply(get_games2013, getPlaysDf)
    plays2013 <- nhlapi:::util_rbindlist(reg_szn_plays_2013)
    
    reg_szn_plays_2014 <- lapply(get_games2014, getPlaysDf)
    plays2014 <- nhlapi:::util_rbindlist(reg_szn_plays_2014)
    
    reg_szn_plays_2015 <- lapply(get_games2015, getPlaysDf)
    plays2015 <- nhlapi:::util_rbindlist(reg_szn_plays_2015)
    
    reg_szn_plays_2016 <- lapply(get_games2016, getPlaysDf)
    plays2016 <- nhlapi:::util_rbindlist(reg_szn_plays_2016)
    
    reg_szn_plays_2017 <- lapply(get_games2017, getPlaysDf)
    plays2017 <- nhlapi:::util_rbindlist(reg_szn_plays_2017)
    
    reg_szn_plays_2018 <- lapply(get_games2018, getPlaysDf)
    plays2018 <- nhlapi:::util_rbindlist(reg_szn_plays_2018)
    
    playoff_plays_2010 <- lapply(get_playoffgames2010, getPlaysDf)
    pplays2010 <- nhlapi:::util_rbindlist(playoff_plays_2010)
    
    playoff_plays_2011 <- lapply(get_playoffgames2011, getPlaysDf)
    pplays2011 <- nhlapi:::util_rbindlist(playoff_plays_2011)
    
    playoff_plays_2013 <- lapply(get_playoffgames2013, getPlaysDf)
    pplays2013 <- nhlapi:::util_rbindlist(playoff_plays_2013)
    
    playoff_plays_2014 <- lapply(get_playoffgames2014, getPlaysDf)
    pplays2014 <- nhlapi:::util_rbindlist(playoff_plays_2014)
    
    playoff_plays_2015 <- lapply(get_playoffgames2015, getPlaysDf)
    pplays2015 <- nhlapi:::util_rbindlist(playoff_plays_2015)
    
    playoff_plays_2016 <- lapply(get_playoffgames2016, getPlaysDf)
    pplays2016 <- nhlapi:::util_rbindlist(playoff_plays_2016)
    
    playoff_plays_2017 <- lapply(get_playoffgames2017, getPlaysDf)
    pplays2017 <- nhlapi:::util_rbindlist(playoff_plays_2017)
    
    playoff_plays_2018 <- lapply(get_playoffgames2018, getPlaysDf)
    pplays2018 <- nhlapi:::util_rbindlist(playoff_plays_2018)
  } 
  
  ##CREATE NEW VARIABLES FOR FURTHER ANALYSIS
  {
    ##create new variables for total seconds in a game, whether in the last 5 minutes of a period,
    ##when a new game starts, when a game ends, a game ID, whether a game is in OT
    new_vars <- function(df){
      df$minute <- as.integer(substr(df$about.periodTime, start = 1, stop = 2)) #pull out minutes
      df$seconds <- as.integer(substr(df$about.periodTime, start = 4, stop = 5)) #pull out seconds
      df$game_seconds <- case_when(
        df$about.period == 1 ~ df$minute * 60 + df$seconds, #covert 1st period to seconds
        df$about.period == 2 ~ (df$minute + 20) * 60 + df$seconds, #covert 2nd period to seconds
        df$about.period == 3 ~ (df$minute + 40) * 60 + df$seconds, #covert 3rd period to seconds
        df$about.period == 4 ~ (df$minute + 60) * 60 + df$seconds, #covert OT to seconds
      )
      df$last5min <- case_when( #create logical for last 5 min of each period
        df$game_seconds >= 900 & df$game_seconds < 1200 ~ 1,
        df$game_seconds >= 2100 & df$game_seconds < 2400 ~ 2,
        df$game_seconds >= 3300 & df$game_seconds < 3600 ~ 3,
        df$game_seconds >= 3600 & df$game_seconds <3900 ~ 4,
        TRUE ~ 0)
      df$gameChange <- case_when( #create logical for when a new game is started
        df$result.event == "Game Scheduled" ~ 1,
        TRUE ~ 0,
      )
      df$gameID <- cumsum(df$gameChange) #create gameID's for each new game
      df$inOT <- case_when( #create logical for if a game goes into OT
        df$about.period == 4 | df$about.period == 5 ~ 1,
        TRUE ~ 0,
      )
      df$OTgame <- case_when( #create logical for when a game ends in OT or shootout
        df$result.event == "Game End" & df$inOT == 1 ~ 1,
        TRUE ~ 0,
      )
      df
    }
    new_vars2010 <- new_vars(plays2010)
    new_vars2011 <- new_vars(plays2011)
    new_vars2013 <- new_vars(plays2013)
    new_vars2014 <- new_vars(plays2014)
    new_vars2015 <- new_vars(plays2015)
    new_vars2016 <- new_vars(plays2016)
    new_vars2017 <- new_vars(plays2017)
    new_vars2018 <- new_vars(plays2018)
    pnew_vars2010 <- new_vars(pplays2010)
    pnew_vars2011 <- new_vars(pplays2011)
    pnew_vars2013 <- new_vars(pplays2013)
    pnew_vars2014 <- new_vars(pplays2014)
    pnew_vars2015 <- new_vars(pplays2015)
    pnew_vars2016 <- new_vars(pplays2016)
    pnew_vars2017 <- new_vars(pplays2017)
    pnew_vars2018 <- new_vars(pplays2018)
    
    
    update_data <- function(df){
      df%>%
        group_by(gameID)%>%
        mutate(isOTgame = across(OTgame, ~ any(.x == 1))) #new logical for whether a game goes into OT by gameID
    }
    update_data_2010 <- update_data(new_vars2010) 
    update_data_2011 <- update_data(new_vars2011) 
    update_data_2013 <- update_data(new_vars2013) 
    update_data_2014 <- update_data(new_vars2014) 
    update_data_2015 <- update_data(new_vars2015) 
    update_data_2016 <- update_data(new_vars2016) 
    update_data_2017 <- update_data(new_vars2017) 
    update_data_2018 <- update_data(new_vars2018) 
    pupdate_data_2010 <- update_data(pnew_vars2010) 
    pupdate_data_2011 <- update_data(pnew_vars2011) 
    pupdate_data_2013 <- update_data(pnew_vars2013) 
    pupdate_data_2014 <- update_data(pnew_vars2014) 
    pupdate_data_2015 <- update_data(pnew_vars2015) 
    pupdate_data_2016 <- update_data(pnew_vars2016) 
    pupdate_data_2017 <- update_data(pnew_vars2017) 
    pupdate_data_2018 <- update_data(pnew_vars2018) 
    
    OT_game_minutes <- function(df) {
      df%>%
        filter(isOTgame == 'TRUE')%>% #only OT games
        filter(players != 'NULL')%>% #non-stoppage plays
        drop_na(result.event, team.triCode)%>% #remove NA's 
        group_by(gameID, team.triCode)%>% 
        summarize(last5_game_time = 5)%>% # equals 5 bc that is how long the last 5 min lasts
        ungroup()%>%
        group_by(team.triCode)%>%
        summarize(team_OT_last5_minutes = sum(last5_game_time)) #find total minutes played by each team in the last 5 min of a tied game
    }
    update_data2_2010 <- OT_game_minutes(update_data_2010)
    update_data2_2011 <- OT_game_minutes(update_data_2011)
    update_data2_2013 <- OT_game_minutes(update_data_2013)
    update_data2_2014 <- OT_game_minutes(update_data_2014)
    update_data2_2015 <- OT_game_minutes(update_data_2015)
    update_data2_2016 <- OT_game_minutes(update_data_2016)
    update_data2_2017 <- OT_game_minutes(update_data_2017)
    update_data2_2018 <- OT_game_minutes(update_data_2018)
    pupdate_data2_2010 <- OT_game_minutes(pupdate_data_2010)
    pupdate_data2_2011 <- OT_game_minutes(pupdate_data_2011)
    pupdate_data2_2013 <- OT_game_minutes(pupdate_data_2013)
    pupdate_data2_2014 <- OT_game_minutes(pupdate_data_2014)
    pupdate_data2_2015 <- OT_game_minutes(pupdate_data_2015)
    pupdate_data2_2016 <- OT_game_minutes(pupdate_data_2016)
    pupdate_data2_2017 <- OT_game_minutes(pupdate_data_2017)
    pupdate_data2_2018 <- OT_game_minutes(pupdate_data_2018)
    
    ##join update_data and update_data2 to add in OT game minutes column
    update_data3_2010 <- full_join(update_data_2010, update_data2_2010, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    update_data3_2011 <- full_join(update_data_2011, update_data2_2011, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    update_data3_2013 <- full_join(update_data_2013, update_data2_2013, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    update_data3_2014 <- full_join(update_data_2014, update_data2_2014, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    update_data3_2015 <- full_join(update_data_2015, update_data2_2015, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    update_data3_2016 <- full_join(update_data_2016, update_data2_2016, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    update_data3_2017 <- full_join(update_data_2017, update_data2_2017, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    update_data3_2018 <- full_join(update_data_2018, update_data2_2018, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    pupdate_data3_2010 <- full_join(pupdate_data_2010, pupdate_data2_2010, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    pupdate_data3_2011 <- full_join(pupdate_data_2011, pupdate_data2_2011, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    pupdate_data3_2013 <- full_join(pupdate_data_2013, pupdate_data2_2013, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    pupdate_data3_2014 <- full_join(pupdate_data_2014, pupdate_data2_2014, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    pupdate_data3_2015 <- full_join(pupdate_data_2015, pupdate_data2_2015, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    pupdate_data3_2016 <- full_join(pupdate_data_2016, pupdate_data2_2016, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    pupdate_data3_2017 <- full_join(pupdate_data_2017, pupdate_data2_2017, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    pupdate_data3_2018 <- full_join(pupdate_data_2018, pupdate_data2_2018, by=c('team.triCode' = 'team.triCode')) #join the team_last5_minutes column to the full dataset
    ##create function to create logicals for whether a play is a shot, hit, faceoff, block, or penalty
    update_data4 <- function(df){
      df%>%
        mutate(shot = result.event == "Shot" | result.event == "Goal")%>% ##logicals for different plays
        mutate(hit = result.event == "Hit")%>%
        mutate(faceoff = result.event == "Faceoff")%>%
        mutate(block = result.event == "Blocked Shot")%>%
        mutate(penalty = result.event == "Penalty")%>%
         mutate(game_minute = case_when( #create minute of game variable to do per minute analysis
                about.period == 1 ~ as.integer(minute) + as.integer(1),
                about.period == 2 ~ as.integer(minute) + as.integer(21),
                about.period == 3 ~ as.integer(minute) + as.integer(41),
                about.period == 4 ~ as.integer(minute) + as.integer(61),
                TRUE ~ as.integer(0),
                ))%>%
        mutate(tied = case_when(
          about.goals.home == about.goals.away ~ TRUE,
          about.goals.home != about.goals.away ~ FALSE,
        ))%>%
        mutate(tied_game = tied == TRUE & game_seconds >3600)%>%
        mutate(five_min_bins = case_when(
          game_seconds >= 0 & game_seconds <300 ~ 1,
          game_seconds >= 300 & game_seconds <600 ~ 2,
          game_seconds >= 600 & game_seconds <900 ~ 3,
          game_seconds >= 900 & game_seconds <1200 ~ 4,
          game_seconds >= 1200 & game_seconds <1500 ~ 5,
          game_seconds >= 1500 & game_seconds <1800 ~ 6,
          game_seconds >= 1800 & game_seconds <2100 ~ 7,
          game_seconds >= 2100 & game_seconds <2400 ~ 8,
          game_seconds >= 2400 & game_seconds <2700 ~ 9,
          game_seconds >= 2700 & game_seconds <3000 ~ 10,
          game_seconds >= 3000 & game_seconds <3300 ~ 11,
          game_seconds >= 3300 & game_seconds <3600 ~ 12,
          game_seconds >= 3600 & game_seconds <=3900 ~ 13,
          TRUE ~ 0,
        ))%>%
        select(gameID, team.triCode, result.event, about.period, game_minute, game_seconds, isOTgame, inOT, players, team_OT_last5_minutes, last5min, shot, hit, faceoff, block, penalty, result.penaltySeverity, result.penaltyMinutes, about.dateTime, about.goals.home, about.goals.away, gameChange, tied, tied_game, five_min_bins)
    }
    update_data4_2010 <- update_data4(update_data3_2010)
    update_data4_2011 <- update_data4(update_data3_2011)
    update_data4_2013 <- update_data4(update_data3_2013)
    update_data4_2014 <- update_data4(update_data3_2014)
    update_data4_2015 <- update_data4(update_data3_2015)
    update_data4_2016 <- update_data4(update_data3_2016)
    update_data4_2017 <- update_data4(update_data3_2017)
    update_data4_2018 <- update_data4(update_data3_2018)
    pupdate_data4_2010 <- update_data4(pupdate_data3_2010)
    pupdate_data4_2011 <- update_data4(pupdate_data3_2011)
    pupdate_data4_2013 <- update_data4(pupdate_data3_2013)
    pupdate_data4_2014 <- update_data4(pupdate_data3_2014)
    pupdate_data4_2015 <- update_data4(pupdate_data3_2015)
    pupdate_data4_2016 <- update_data4(pupdate_data3_2016)
    pupdate_data4_2017 <- update_data4(pupdate_data3_2017)
    pupdate_data4_2018 <- update_data4(pupdate_data3_2018)
    
    update_data5 <- function(df){
      df2 <- df%>%
        group_by(gameID)%>%
        summarize(is.tied_game = any(tied_game == TRUE))
      
      df3 <- full_join(df, df2, by=c('gameID' = 'gameID'))%>%
        filter(is.tied_game == TRUE)
      df3
    }
    update_data5_2010 <- update_data5(update_data4_2010)
    update_data5_2011 <- update_data5(update_data4_2011)
    update_data5_2013 <- update_data5(update_data4_2013)
    update_data5_2014 <- update_data5(update_data4_2014)
    update_data5_2015 <- update_data5(update_data4_2015)
    update_data5_2016 <- update_data5(update_data4_2016)
    update_data5_2017 <- update_data5(update_data4_2017)
    update_data5_2018 <- update_data5(update_data4_2018)
    pupdate_data5_2010 <- update_data5(pupdate_data4_2010)
    pupdate_data5_2011 <- update_data5(pupdate_data4_2011)
    pupdate_data5_2013 <- update_data5(pupdate_data4_2013)
    pupdate_data5_2014 <- update_data5(pupdate_data4_2014)
    pupdate_data5_2015 <- update_data5(pupdate_data4_2015)
    pupdate_data5_2016 <- update_data5(pupdate_data4_2016)
    pupdate_data5_2017 <- update_data5(pupdate_data4_2017)
    pupdate_data5_2018 <- update_data5(pupdate_data4_2018)
    save.image(file = "~/Desktop/QSS_30/research_paper_3_9_workspace.Rdata")
  }
}

##  ** START HERE IF YOU HAVE THE .Rdata FILE **
##  ** FOR OTHER COMPUTERS: 
# load(file_choose()) # * click the research_paper_workspace.Rdata file *
load("~/Desktop/QSS_30/research_paper_workspace_final.Rdata")


##FIND TOTAL GAMES THAT GO INTO OT
OT_games <- function(df){
  df%>%
    filter(game_minute <61, game_minute != 1, game_minute != 21, game_minute != 41)%>% #remove 1st minute of each period
    filter(about.period < 5)%>% #remove Shootout
    filter(isOTgame == TRUE)%>% #only games that go into OT
    count()#%>% #count games that go into OT
  
}
##COUNT TOTAL GAMES THAT GO INTO OT
OT_games_total <- function(df){
  nrow(OT_games(df))
}
{
games2010 <- OT_games_total(update_data5_2010)
games2011 <- OT_games_total(update_data5_2011)
games2013 <- OT_games_total(update_data5_2013)
games2014 <- OT_games_total(update_data5_2014)
games2015 <- OT_games_total(update_data5_2015)
games2016 <- OT_games_total(update_data5_2016)
games2017 <- OT_games_total(update_data5_2017)
games2018 <- OT_games_total(update_data5_2018)
pgames2010 <- OT_games_total(pupdate_data5_2010)
pgames2011 <- OT_games_total(pupdate_data5_2011)
pgames2013 <- OT_games_total(pupdate_data5_2013)
pgames2014 <- OT_games_total(pupdate_data5_2014)
pgames2015 <- OT_games_total(pupdate_data5_2015)
pgames2016 <- OT_games_total(pupdate_data5_2016)
pgames2017 <- OT_games_total(pupdate_data5_2017)
pgames2018 <- OT_games_total(pupdate_data5_2018)
}

##SHOTS
{
  shots <- function(df, x){
    df%>%
      filter(result.event == "Shot" | result.event == "Goal")%>% #include goals as shots
      filter(isOTgame == TRUE)%>% #only OT games
      filter(about.period < 5)%>% #remove SO events
      filter(game_minute <61, game_minute != 1, game_minute != 21, game_minute != 41)%>% #remove 1st minute of each period, only use regulation time
      group_by(game_minute, gameID)%>%
      summarize(shot_games = shot == TRUE)%>% #Logical if event is a shot
      filter(shot_games == TRUE)%>% #only select shots
      ungroup()%>%
      group_by(game_minute)%>%
      count(shot_games == TRUE)%>% #count shots by game
      summarize(spm = n/x) #divide by total games that go into OT that season
  }
  {
  shots2010 <- shots(update_data5_2010, games2010)
  shots2011 <- shots(update_data5_2011, games2011)
  shots2013 <- shots(update_data5_2013, games2013)
  shots2014 <- shots(update_data5_2014, games2014)
  shots2015 <- shots(update_data5_2015, games2015)
  shots2016 <- shots(update_data5_2016, games2016)
  shots2017 <- shots(update_data5_2017, games2017)
  shots2018 <- shots(update_data5_2018, games2017)
  pshots2010 <- shots(pupdate_data5_2010, pgames2010)
  pshots2011 <- shots(pupdate_data5_2011, pgames2011)
  pshots2013 <- shots(pupdate_data5_2013, pgames2013)
  pshots2014 <- shots(pupdate_data5_2014, pgames2014)
  pshots2015 <- shots(pupdate_data5_2015, pgames2015)
  pshots2016 <- shots(pupdate_data5_2016, pgames2016)
  pshots2017 <- shots(pupdate_data5_2017, pgames2017)
  pshots2018 <- shots(pupdate_data5_2018, pgames2018)
  }
  ##FIND AVERAGE SHOTS IN FIRST 55 MIN, LAST 5 MIN, MULTIPLY BY 60 TO FIND SHOTS/60 MIN
  {
    ##REGULAR SEASON
  dfshots_55 <- c(colMeans(shots2010 [1:52, 2]), colMeans(shots2011 [1:52, 2]), colMeans(shots2013 [1:52, 2]),
                  colMeans(shots2014 [1:52, 2]), colMeans(shots2015 [1:52, 2]), colMeans(shots2016 [1:52, 2]),
                  colMeans(shots2017 [1:52, 2]), colMeans(shots2018 [1:52, 2]))*60
  dfshots_5  <- c(colMeans(shots2010 [53:57, 2]), colMeans(shots2011 [53:57, 2]), colMeans(shots2013 [53:57, 2]),
                  colMeans(shots2014 [53:57, 2]), colMeans(shots2015 [53:57, 2]), colMeans(shots2016 [53:57, 2]),
                  colMeans(shots2017 [53:57, 2]), colMeans(shots2018 [53:57, 2]))*60
  df_mean_shots <- data.frame(dfshots_55, dfshots_5, row.names = c('2010', '2011', '2013', '2014',
                                                                   '2015', '2016', '2017', '2018'))
  ##PLAYOFFS
  dfpshots_55 <- c(colMeans(pshots2010 [1:52, 2]), colMeans(pshots2011 [1:52, 2]), colMeans(pshots2013 [1:52, 2]),
                   colMeans(pshots2014 [1:52, 2]), colMeans(pshots2015 [1:52, 2]), colMeans(pshots2016 [1:52, 2]),
                   colMeans(pshots2017 [1:52, 2]), colMeans(pshots2018 [1:52, 2]))*60
  dfpshots_5  <- c(colMeans(pshots2010 [53:57, 2]), colMeans(pshots2011 [53:57, 2]), colMeans(pshots2013 [53:57, 2]),
                   colMeans(pshots2014 [53:57, 2]), colMeans(pshots2015 [53:57, 2]), colMeans(pshots2016 [53:57, 2]),
                   colMeans(pshots2017 [53:57, 2]), colMeans(pshots2018 [53:57, 2]))*60
  df_mean_pshots <- data.frame(dfpshots_55, dfpshots_5, row.names = c('2010', '2011', '2013', '2014',
                                                                      '2015', '2016', '2017', '2018'))
  }
  ##GRAPH SHOTS PER MINUTE IN EACH SEASON
  graph_shots <- function(df, x){
    df%>%
      ggplot(aes(x=game_minute, y=spm, color = 'red'))+
      geom_line()+
      geom_smooth(aes(color = 'blue'), se=FALSE)+
      geom_hline(aes(yintercept = mean(spm), color = 'black'))+
      theme_minimal()+
      scale_x_continuous(breaks = seq(0,60,5))+
      labs(title = paste(x, 'Season Shots per Minute in tied games'), color = 'Color', x = 'Game Minute', y= "Shots per minute")+
      scale_color_manual(values =c('black', 'blue', 'red'), labels = c('Season average', 'Smoothed', 'Per Minute Raw'))
  }
  {
    graph_shots(shots2010, '2010')
    graph_shots(shots2011, '2011')
    graph_shots(shots2013, '2013')
    graph_shots(shots2014, '2014')
    graph_shots(shots2015, '2015')
    graph_shots(shots2016, '2016')
    graph_shots(shots2017, '2017')
    graph_shots(shots2018, '2018')
  }
  ##Graph of smoothed shot rates from each season
  ##REGULAR SEASON
  {
    plot(x=NULL, y=NULL, xlim = c(2.2, 57.8), ylim = c(50, 65), main="Smoothed Shot Rates per 60 minutes in Regular Season games\nthat go into OT by Season",
         xlab = 'Game Minute', ylab = 'Shots per minute')+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = shots2010$spm*60), col = 'blue', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = shots2011$spm*60), col = 'green', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = shots2013$spm*60), col = 'red', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = shots2014$spm*60), col = 'orange', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = shots2015$spm*60), col = 'yellow', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = shots2016$spm*60), col = 'purple', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = shots2017$spm*60), col = 'black', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = shots2018$spm*60), col = 'lightblue', lwd = 5)+
      lines(x=c(55,55), y=c(50, 65), lty = 'dashed')+
      legend('bottomleft', legend = c('2010', '2011', '2013', '2014', '2015', '2016', '2017', '2018'),
             col = c('blue', 'green', 'red', 'orange', 'yellow', 'purple', 'black', 'lightblue'), lty = 1, lwd = 5, cex = 0.8)
  }
  ##PLAYOFFS
  {
    plot(x=NULL, y=NULL, xlim = c(2.2, 57.8), ylim = c(50,65), main="Smoothed Shot Rates per 60 minutes in Playoff games\nthat go into OT by Season",
         xlab = 'Game Minute', ylab = 'Shots per minute')+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pshots2010$spm*60), col = 'blue', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pshots2011$spm*60), col = 'green', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pshots2013$spm*60), col = 'red', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pshots2014$spm*60), col = 'orange', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pshots2015$spm*60), col = 'yellow', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pshots2016$spm*60), col = 'purple', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pshots2017$spm*60), col = 'black', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pshots2018$spm*60), col = 'lightblue', lwd = 5)+
      lines(x=c(55,55), y=c(50,65), lty = 'dashed')+
      legend('bottomleft', legend = c('2010', '2011', '2013', '2014', '2015', '2016', '2017', '2018'),
             col = c('blue', 'green', 'red', 'orange', 'yellow', 'purple', 'black', 'lightblue'), lty = 1, lwd = 5, cex = 0.8)
  }
}


##PENALTIES
##FOR EXPLANATION OF FUNCTIONS, SEE SHOTS. ALL FUNCTIONS ARE THE SAME BUT SEARCH FOR PENALTIES INSTEAD OF SHOTS
{
  penalties <- function(df,x){
    df%>%
      filter(result.event == "Penalty")%>% 
      filter(isOTgame == TRUE)%>%
      filter(about.period < 5)%>%
      filter(game_minute <61, game_minute != 1, game_minute != 21, game_minute != 41)%>%
      group_by(game_minute, gameID)%>%
      summarize(penalty_games = penalty == TRUE)%>%
      filter(penalty_games == TRUE)%>%
      ungroup()%>%
      group_by(game_minute)%>%
      count(penalty_games == TRUE)%>%
      summarize(ppm = n/x)
  }
  {
  penalties2010 <- penalties(update_data5_2010, games2010)
  penalties2011 <- penalties(update_data5_2011, games2011)
  penalties2013 <- penalties(update_data5_2013, games2013)
  penalties2014 <- penalties(update_data5_2014, games2014)
  penalties2015 <- penalties(update_data5_2015, games2015)
  penalties2016 <- penalties(update_data5_2016, games2016)
  penalties2017 <- penalties(update_data5_2017, games2017)
  penalties2018 <- penalties(update_data5_2018, games2018)
  ppenalties2010 <- penalties(pupdate_data5_2010, pgames2010)%>%
    add_row(game_minute = 21, ppm = 0, .after = 19)%>%
    add_row(game_minute = 42, ppm = 0, .after = 39)%>%
    add_row(game_minute = 45, ppm = 0, .after = 42)%>%
    add_row(game_minute = 52, ppm = 0, .after = 48)%>%
    add_row(game_minute = 55, ppm = 0, .after = 51)%>%
    add_row(game_minute = 59, ppm = 0, .after = 55)%>%
    add_row(game_minute = 60, ppm = 0, .after = 56)
  ppenalties2011 <- penalties(pupdate_data5_2011, pgames2011)%>%
    add_row(game_minute = 56, ppm = 0, .after = 52)%>%
    add_row(game_minute = 59, ppm = 0, .after = 55)
  ppenalties2013 <- penalties(pupdate_data5_2013, pgames2013)%>%
    add_row(game_minute = 9, ppm = 0, .after = 7)%>%
    add_row(game_minute = 42, ppm = 0, .after = 38)%>%
    add_row(game_minute = 45, ppm = 0, .after = 41)%>%
    add_row(game_minute = 47, ppm = 0, .after = 43)%>%
    add_row(game_minute = 53, ppm = 0, .after = 49)%>%
    add_row(game_minute = 55, ppm = 0, .after = 51)%>%
    add_row(game_minute = 60, ppm = 0, .after = 56)
  ppenalties2014 <- penalties(pupdate_data5_2014, pgames2014)%>%
    add_row(game_minute = 15, ppm = 0, .after = 13)%>%
    add_row(game_minute = 35, ppm = 0, .after = 32)%>%
    add_row(game_minute = 38, ppm = 0, .after = 35)%>%
    add_row(game_minute = 44, ppm = 0, .after = 40)%>%
    add_row(game_minute = 50, ppm = 0, .after = 46)%>%
    add_row(game_minute = 54, ppm = 0, .after = 50)%>%
    add_row(game_minute = 55, ppm = 0, .after = 51)%>%
    add_row(game_minute = 56, ppm = 0, .after = 52)%>%
    add_row(game_minute = 60, ppm = 0, .after = 56)
  ppenalties2015 <- penalties(pupdate_data5_2015, pgames2015)%>%
    add_row(game_minute = 2, ppm = 0, .before = 1)%>%
    add_row(game_minute = 5, ppm = 0, .after = 3)%>%
    add_row(game_minute = 36, ppm = 0, .after = 33)%>%
    add_row(game_minute = 40, ppm = 0, .after = 36)%>%
    add_row(game_minute = 47, ppm = 0, .after = 43)%>%
    add_row(game_minute = 54, ppm = 0, .after = 50)%>%
    add_row(game_minute = 59, ppm = 0, .after = 55)
  ppenalties2016 <- penalties(pupdate_data5_2016, pgames2016)%>%
    add_row(game_minute = 59, ppm = 0, .after = 55)
  ppenalties2017 <- penalties(pupdate_data5_2017, pgames2017)%>%
    add_row(game_minute = 2, ppm = 0, .before = 1)%>%
    add_row(game_minute = 3, ppm = 0, .after = 1)%>%
    add_row(game_minute = 4, ppm = 0, .after = 2)%>%
    add_row(game_minute = 7, ppm = 0, .after = 5)%>%
    add_row(game_minute = 14, ppm = 0, .after = 12)%>%
    add_row(game_minute = 20, ppm = 0, .after = 18)%>%
    add_row(game_minute = 32, ppm = 0, .after = 29)%>%
    add_row(game_minute = 36, ppm = 0, .after = 33)%>%
    add_row(game_minute = 43, ppm = 0, .after = 39)%>%
    add_row(game_minute = 45, ppm = 0, .after = 41)%>%
    add_row(game_minute = 47, ppm = 0, .after = 43)%>%
    add_row(game_minute = 49, ppm = 0, .after = 45)%>%
    add_row(game_minute = 54, ppm = 0, .after = 50)%>%
    add_row(game_minute = 57, ppm = 0, .after = 53)%>%
    add_row(game_minute = 58, ppm = 0, .after = 54)%>%
    add_row(game_minute = 60, ppm = 0, .after = 56)
   
  ppenalties2018 <- penalties(pupdate_data5_2018, pgames2018)%>%
    add_row(game_minute = 3, ppm = 0, .after = 1)%>%
    add_row(game_minute = 6, ppm = 0, .after = 4)%>%
    add_row(game_minute = 9, ppm = 0, .after = 7)%>%
    add_row(game_minute = 12, ppm = 0, .after = 10)%>%
    add_row(game_minute = 20, ppm = 0, .after = 18)%>%
    add_row(game_minute = 37, ppm = 0, .after = 34)%>%
    add_row(game_minute = 44, ppm = 0, .after = 40)%>%
    add_row(game_minute = 45, ppm = 0, .after = 41)%>%
    add_row(game_minute = 48, ppm = 0, .after = 44)%>%
    add_row(game_minute = 52, ppm = 0, .after = 48)%>%
    add_row(game_minute = 53, ppm = 0, .after = 49)%>%
    add_row(game_minute = 56, ppm = 0, .after = 52)%>%
    add_row(game_minute = 60, ppm = 0, .after = 56)
} 
{
  dfpenalties_55 <- c(colMeans(penalties2010 [1:52, 2]), colMeans(penalties2011 [1:52, 2]), colMeans(penalties2013 [1:52, 2]),
                  colMeans(penalties2014 [1:52, 2]), colMeans(penalties2015 [1:52, 2]), colMeans(penalties2016 [1:52, 2]),
                  colMeans(penalties2017 [1:52, 2]), colMeans(penalties2018 [1:52, 2]))*60
  dfpenalties_5  <- c(colMeans(penalties2010 [53:57, 2]), colMeans(penalties2011 [53:57, 2]), colMeans(penalties2013 [53:57, 2]),
                  colMeans(penalties2014 [53:57, 2]), colMeans(penalties2015 [53:57, 2]), colMeans(penalties2016 [53:57, 2]),
                  colMeans(penalties2017 [53:57, 2]), colMeans(penalties2018 [53:57, 2]))*60
  df_mean_penalties <- data.frame(dfpenalties_55, dfpenalties_5, row.names = c('2010', '2011', '2013', '2014',
                                                                   '2015', '2016', '2017', '2018'))
  
  dfppenalties_55 <- c(colMeans(ppenalties2010 [1:52, 2]), colMeans(ppenalties2011 [1:52, 2]), colMeans(ppenalties2013 [1:52, 2]),
                   colMeans(ppenalties2014 [1:52, 2]), colMeans(ppenalties2015 [1:52, 2]), colMeans(ppenalties2016 [1:52, 2]),
                   colMeans(ppenalties2017 [1:52, 2]), colMeans(ppenalties2018 [1:52, 2]))*60
  dfppenalties_5  <- c(colMeans(ppenalties2010 [53:57, 2]), colMeans(ppenalties2011 [53:57, 2]), colMeans(ppenalties2013 [53:57, 2]),
                   colMeans(ppenalties2014 [53:57, 2]), colMeans(ppenalties2015 [53:57, 2]), colMeans(ppenalties2016 [53:57, 2]),
                   colMeans(ppenalties2017 [53:57, 2]), colMeans(ppenalties2018 [53:57, 2]))*60
  df_mean_ppenalties <- data.frame(dfppenalties_55, dfppenalties_5, row.names = c('2010', '2011', '2013', '2014',
                                                                      '2015', '2016', '2017', '2018'))
}
  graph_penalties <- function(df, x){
    df%>%
      ggplot(aes(x=game_minute, y=ppm, color = 'red'))+
      geom_line()+
      geom_smooth(aes(color = 'blue'), se=FALSE)+
      geom_hline(aes(yintercept = mean(ppm), color = 'black'))+
      theme_minimal()+
      scale_x_continuous(breaks = seq(0,60,5))+
      labs(title = paste(x, 'Season Penalties per Minute in games that go into OT'), color = 'Color', x = 'Game Minute', y= "Penalties per minute")+
      scale_color_manual(values =c('black', 'blue', 'red'), labels = c('Season average', 'Smoothed', 'Per Minute Raw'))
  }
  {
    graph_penalties(ppenalties2010, '2010')
    graph_penalties(ppenalties2011, '2011')
    graph_penalties(penalties2013, '2013')
    graph_penalties(penalties2014, '2014')
    graph_penalties(penalties2015, '2015')
    graph_penalties(penalties2016, '2016')
    graph_penalties(penalties2017, '2017')
    graph_penalties(penalties2018, '2018')
  }
  ##Graph of smoothed penalty rates from each season
  {
    plot(x=NULL, y=NULL, xlim = c(2.2,57.8), ylim = c(2,12), main = 'Smoothed Penalty Rates per 60 minutes in Regular Season games\nthat go into OT by season',
         xlab = 'Game Minute', ylab = 'Penalties per minute')+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = penalties2010$ppm*60), col = 'blue', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = penalties2011$ppm*60), col = 'green', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = penalties2013$ppm*60), col = 'red', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = penalties2014$ppm*60), col = 'orange', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = penalties2015$ppm*60), col = 'yellow', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = penalties2016$ppm*60), col = 'purple', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = penalties2017$ppm*60), col = 'black', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = penalties2018$ppm*60), col = 'lightblue', lwd = 5)+
      lines(x=c(55,55), y=c(2,12), lty = 'dashed')+
      legend('topright', legend = c('2010', '2011', '2013', '2014', '2015', '2016', '2017', '2018'),
             col = c('blue', 'green', 'red', 'orange', 'yellow', 'purple', 'black', 'lightblue'), lty = 1, lwd = 5, cex = 0.8)
  }
  {
    plot(x=NULL, y=NULL, xlim = c(2.2,57.8), ylim = c(2,12), main = 'Smoothed Penalty Rates per 60 minutes in Playoff games\nthat go into OT by season',
         xlab = 'Game Minute', ylab = 'Penalties per minute')+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = ppenalties2010$ppm*60), col = 'blue', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = ppenalties2011$ppm*60), col = 'green', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = ppenalties2013$ppm*60), col = 'red', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = ppenalties2014$ppm*60), col = 'orange', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = ppenalties2015$ppm*60), col = 'yellow', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = ppenalties2016$ppm*60), col = 'purple', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = ppenalties2017$ppm*60), col = 'black', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = ppenalties2018$ppm*60), col = 'lightblue', lwd = 5)+
      lines(x=c(55,55), y=c(2,12), lty = 'dashed')+
      legend('topright', legend = c('2010', '2011', '2013', '2014', '2015', '2016', '2017', '2018'),
             col = c('blue', 'green', 'red', 'orange', 'yellow', 'purple', 'black', 'lightblue'), lty = 1, lwd = 5, cex = 0.8)
  }
} 

##HITS
##FOR EXPLANATION OF FUNCTIONS, SEE SHOTS. ALL FUNCTIONS ARE THE SAME BUT SEARCH FOR HITS INSTEAD OF SHOTS
{
  hits <- function(df, x){
    df%>%
      filter(result.event == "Hit")%>% 
      filter(isOTgame == TRUE)%>%
      filter(about.period < 5)%>%
      filter(game_minute <61, game_minute != 1, game_minute != 21, game_minute != 41)%>%
      group_by(game_minute, gameID)%>%
      summarize(hit_games = hit == TRUE)%>%
      filter(hit_games == TRUE)%>%
      ungroup()%>%
      group_by(game_minute)%>%
      count(hit_games == TRUE)%>%
      summarize(hpm = n/x)
  }
  {
  hits2010 <- hits(update_data5_2010, games2010)
  hits2011 <- hits(update_data5_2011, games2011)
  hits2013 <- hits(update_data5_2013, games2013)
  hits2014 <- hits(update_data5_2014, games2014)
  hits2015 <- hits(update_data5_2015, games2015)
  hits2016 <- hits(update_data5_2016, games2016)
  hits2017 <- hits(update_data5_2017, games2017)
  hits2018 <- hits(update_data5_2018, games2018)
  phits2010 <- hits(pupdate_data5_2010, pgames2010)
  phits2011 <- hits(pupdate_data5_2011, pgames2011)
  phits2013 <- hits(pupdate_data5_2013, pgames2013)
  phits2014 <- hits(pupdate_data5_2014, pgames2014)
  phits2015 <- hits(pupdate_data5_2015, pgames2015)
  phits2016 <- hits(pupdate_data5_2016, pgames2016)
  phits2017 <- hits(pupdate_data5_2017, pgames2017)
  phits2018 <- hits(pupdate_data5_2018, pgames2018)
  }
  {
  dfhits_55 <- c(colMeans(hits2010 [1:52, 2]), colMeans(hits2011 [1:52, 2]), colMeans(hits2013 [1:52, 2]),
                      colMeans(hits2014 [1:52, 2]), colMeans(hits2015 [1:52, 2]), colMeans(hits2016 [1:52, 2]),
                      colMeans(hits2017 [1:52, 2]), colMeans(hits2018 [1:52, 2]))*60
  dfhits_5  <- c(colMeans(hits2010 [53:57, 2]), colMeans(hits2011 [53:57, 2]), colMeans(hits2013 [53:57, 2]),
                      colMeans(hits2014 [53:57, 2]), colMeans(hits2015 [53:57, 2]), colMeans(hits2016 [53:57, 2]),
                      colMeans(hits2017 [53:57, 2]), colMeans(hits2018 [53:57, 2]))*60
  df_mean_hits <- data.frame(dfhits_55, dfhits_5, row.names = c('2010', '2011', '2013', '2014',
                                                                               '2015', '2016', '2017', '2018'))
  
  dfphits_55 <- c(colMeans(phits2010 [1:52, 2]), colMeans(phits2011 [1:52, 2]), colMeans(phits2013 [1:52, 2]),
                       colMeans(phits2014 [1:52, 2]), colMeans(phits2015 [1:52, 2]), colMeans(phits2016 [1:52, 2]),
                       colMeans(phits2017 [1:52, 2]), colMeans(phits2018 [1:52, 2]))*60
  dfphits_5  <- c(colMeans(phits2010 [53:57, 2]), colMeans(phits2011 [53:57, 2]), colMeans(phits2013 [53:57, 2]),
                       colMeans(phits2014 [53:57, 2]), colMeans(phits2015 [53:57, 2]), colMeans(phits2016 [53:57, 2]),
                       colMeans(phits2017 [53:57, 2]), colMeans(phits2018 [53:57, 2]))*60
  df_mean_phits <- data.frame(dfphits_55, dfphits_5, row.names = c('2010', '2011', '2013', '2014',
                                                                                  '2015', '2016', '2017', '2018'))
  }
  graph_hits <- function(df, x){
    df%>%
      ggplot(aes(x=game_minute, y=hpm, color = 'red'))+
      geom_line()+
      geom_smooth(aes(color = 'blue'), se=FALSE)+
      geom_hline(aes(yintercept = mean(hpm), color = 'black'))+
      theme_minimal()+
      scale_x_continuous(breaks = seq(0,60,5))+
      labs(title = paste(x, 'Season Hits per Minute in tied games'), color = 'Color', x = 'Game Minute', y= "Hits per minute")+
      scale_color_manual(values =c('black', 'blue', 'red'), labels = c('Season average', 'Smoothed', 'Per Minute Raw'))
    
  }
  {
    graph_hits(hits2010, '2010')
    graph_hits(hits2011, '2011')
    graph_hits(hits2013, '2013')
    graph_hits(hits2014, '2014')
    graph_hits(hits2015, '2015')
    graph_hits(hits2016, '2016')
    graph_hits(hits2017, '2017')
    graph_hits(hits2018, '2018')
  }
  ##Graph of smoothed hit rates from each season
  {
    plot(x=NULL, y=NULL, xlim = c(2.2,57.8), ylim = c(40,100),  main="Smoothed Hit Rates per 60 minutes in Regular Season games\nthat go into OT by Season",
         xlab = 'Game Minute', ylab = 'Hits per minute')+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = hits2010$hpm*60), col = 'blue', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = hits2011$hpm*60), col = 'green', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = hits2013$hpm*60), col = 'red', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = hits2014$hpm*60), col = 'orange', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = hits2015$hpm*60), col = 'yellow', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = hits2016$hpm*60), col = 'purple', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = hits2017$hpm*60), col = 'black', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = hits2018$hpm*60), col = 'lightblue', lwd = 5)+
      lines(x=c(55,55), y=c(40,100), lty = 'dashed')+
      legend('topright', legend = c('2010', '2011', '2013', '2014', '2015', '2016', '2017', '2018'),
             col = c('blue', 'green', 'red', 'orange', 'yellow', 'purple', 'black', 'lightblue'), lty = 1, lwd = 5, cex = 0.8)
  }
  {
    plot(x=NULL, y=NULL, xlim = c(2.2,57.8), ylim = c(40,100),  main="Smoothed Hit Rates per 60 minutes in Playoff games\nthat go into OT by Season",
         xlab = 'Game Minute', ylab = 'Hits per minute')+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = phits2010$hpm*60), col = 'blue', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = phits2011$hpm*60), col = 'green', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = phits2013$hpm*60), col = 'red', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = phits2014$hpm*60), col = 'orange', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = phits2015$hpm*60), col = 'yellow', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = phits2016$hpm*60), col = 'purple', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = phits2017$hpm*60), col = 'black', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = phits2018$hpm*60), col = 'lightblue', lwd = 5)+
      lines(x=c(55,55), y=c(40,100), lty = 'dashed')+
      legend('topright', legend = c('2010', '2011', '2013', '2014', '2015', '2016', '2017', '2018'),
             col = c('blue', 'green', 'red', 'orange', 'yellow', 'purple', 'black', 'lightblue'), lty = 1, lwd = 5, cex = 0.8)
  }
}

##FACEOFFS
##FOR EXPLANATION OF FUNCTIONS, SEE SHOTS. ALL FUNCTIONS ARE THE SAME BUT SEARCH FOR FACEOFFS INSTEAD OF SHOTS
{
  faceoffs <- function(df, x){
    df%>%
      filter(result.event == "Faceoff")%>% 
      filter(isOTgame == TRUE)%>%
      filter(about.period < 5)%>%
      filter(game_minute <61, game_minute != 1, game_minute != 21, game_minute != 41)%>%
      group_by(game_minute, gameID)%>%
      summarize(faceoff_games = faceoff == TRUE)%>%
      filter(faceoff_games == TRUE)%>%
      ungroup()%>%
      group_by(game_minute)%>%
      count(faceoff_games == TRUE)%>%
      summarize(fpm = n/x)
  }
  {
  faceoffs2010 <- faceoffs(update_data5_2010, games2010)
  faceoffs2011 <- faceoffs(update_data5_2011, games2011)
  faceoffs2013 <- faceoffs(update_data5_2013, games2013)
  faceoffs2014 <- faceoffs(update_data5_2014, games2014)
  faceoffs2015 <- faceoffs(update_data5_2015, games2015)
  faceoffs2016 <- faceoffs(update_data5_2016, games2016)
  faceoffs2017 <- faceoffs(update_data5_2017, games2017)
  faceoffs2018 <- faceoffs(update_data5_2018, games2018)
  pfaceoffs2010 <- faceoffs(pupdate_data5_2010, pgames2010)
  pfaceoffs2011 <- faceoffs(pupdate_data5_2011, pgames2011)
  pfaceoffs2013 <- faceoffs(pupdate_data5_2013, pgames2013)
  pfaceoffs2014 <- faceoffs(pupdate_data5_2014, pgames2014)
  pfaceoffs2015 <- faceoffs(pupdate_data5_2015, pgames2015)
  pfaceoffs2016 <- faceoffs(pupdate_data5_2016, pgames2016)
  pfaceoffs2017 <- faceoffs(pupdate_data5_2017, pgames2017)
  pfaceoffs2018 <- faceoffs(pupdate_data5_2018, pgames2018)
  }
  {
    dffaceoffs_55 <- c(colMeans(faceoffs2010 [1:52, 2]), colMeans(faceoffs2011 [1:52, 2]), colMeans(faceoffs2013 [1:52, 2]),
                   colMeans(faceoffs2014 [1:52, 2]), colMeans(faceoffs2015 [1:52, 2]), colMeans(faceoffs2016 [1:52, 2]),
                   colMeans(faceoffs2017 [1:52, 2]), colMeans(faceoffs2018 [1:52, 2]))*60
    dffaceoffs_5  <- c(colMeans(faceoffs2010 [53:57, 2]), colMeans(faceoffs2011 [53:57, 2]), colMeans(faceoffs2013 [53:57, 2]),
                   colMeans(faceoffs2014 [53:57, 2]), colMeans(faceoffs2015 [53:57, 2]), colMeans(faceoffs2016 [53:57, 2]),
                   colMeans(faceoffs2017 [53:57, 2]), colMeans(faceoffs2018 [53:57, 2]))*60
    df_mean_faceoffs <- data.frame(dffaceoffs_55, dffaceoffs_5, row.names = c('2010', '2011', '2013', '2014',
                                                                  '2015', '2016', '2017', '2018'))
    
    dfpfaceoffs_55 <- c(colMeans(pfaceoffs2010 [1:52, 2]), colMeans(pfaceoffs2011 [1:52, 2]), colMeans(pfaceoffs2013 [1:52, 2]),
                    colMeans(pfaceoffs2014 [1:52, 2]), colMeans(pfaceoffs2015 [1:52, 2]), colMeans(pfaceoffs2016 [1:52, 2]),
                    colMeans(pfaceoffs2017 [1:52, 2]), colMeans(pfaceoffs2018 [1:52, 2]))*60
    dfpfaceoffs_5  <- c(colMeans(pfaceoffs2010 [53:57, 2]), colMeans(pfaceoffs2011 [53:57, 2]), colMeans(pfaceoffs2013 [53:57, 2]),
                    colMeans(pfaceoffs2014 [53:57, 2]), colMeans(pfaceoffs2015 [53:57, 2]), colMeans(pfaceoffs2016 [53:57, 2]),
                    colMeans(pfaceoffs2017 [53:57, 2]), colMeans(pfaceoffs2018 [53:57, 2]))*60
    df_mean_pfaceoffs <- data.frame(dfpfaceoffs_55, dfpfaceoffs_5, row.names = c('2010', '2011', '2013', '2014',
                                                                     '2015', '2016', '2017', '2018'))
  }
  
  graph_faceoffs <- function(df){
    df%>%
      ggplot(aes(x=game_minute, y=fpm, color = 'red'))+
      geom_line()+
      geom_smooth(aes(color = 'blue'), se=FALSE)+
      geom_hline(aes(yintercept = mean(fpm)))+
      theme_minimal()+
      scale_x_continuous(breaks = seq(0,60,5))
  }
  {
    graph_faceoffs(faceoffs2010)
    graph_faceoffs(faceoffs2011)
    graph_faceoffs(faceoffs2013)
    graph_faceoffs(faceoffs2014)
    graph_faceoffs(faceoffs2015)
    graph_faceoffs(faceoffs2016)
    graph_faceoffs(faceoffs2017)
    graph_faceoffs(faceoffs2018)
  }
  ##Graph of smoothed hit rates from each season
  {
    plot(x=NULL, y=NULL, xlim = c(2.2,57.8), ylim = c(50,70), main="Smoothed Faceoff Rates per 60 minutes in Regular Season games\nthat go into OT by Season",
         xlab = 'Game Minute', ylab = 'Faceoffs per minute')+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = faceoffs2010$fpm*60), col = 'blue', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = faceoffs2011$fpm*60), col = 'green', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = faceoffs2013$fpm*60), col = 'red', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = faceoffs2014$fpm*60), col = 'orange', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = faceoffs2015$fpm*60), col = 'yellow', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = faceoffs2016$fpm*60), col = 'purple', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = faceoffs2017$fpm*60), col = 'black', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = faceoffs2018$fpm*60), col = 'lightblue', lwd = 5)+
      lines(x=c(55,55), y=c(50,70), lty = 'dashed')+
      legend('topleft', legend = c('2010', '2011', '2013', '2014', '2015', '2016', '2017', '2018'),
             col = c('blue', 'green', 'red', 'orange', 'yellow', 'purple', 'black', 'lightblue'), lty = 1, lwd = 5, cex = .8)
  }
  {
    plot(x=NULL, y=NULL, xlim = c(2.2,57.8), ylim = c(50,70), main="Smoothed Faceoff Rates per 60 minutes in Playoff games\nthat go into OT by Season",
         xlab = 'Game Minute', ylab = 'Faceoffs per minute')+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pfaceoffs2010$fpm*60), col = 'blue', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pfaceoffs2011$fpm*60), col = 'green', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pfaceoffs2013$fpm*60), col = 'red', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pfaceoffs2014$fpm*60), col = 'orange', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pfaceoffs2015$fpm*60), col = 'yellow', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pfaceoffs2016$fpm*60), col = 'purple', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pfaceoffs2017$fpm*60), col = 'black', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pfaceoffs2018$fpm*60), col = 'lightblue', lwd = 5)+
      lines(x=c(55,55), y=c(50,70), lty = 'dashed')+
      legend('topleft', legend = c('2010', '2011', '2013', '2014', '2015', '2016', '2017', '2018'),
             col = c('blue', 'green', 'red', 'orange', 'yellow', 'purple', 'black', 'lightblue'), lty = 1, lwd = 5, cex = .8)
  }
}

##BLOCKS
##FOR EXPLANATION OF FUNCTIONS, SEE SHOTS. ALL FUNCTIONS ARE THE SAME BUT SEARCH FOR BLOCKS INSTEAD OF SHOTS
{
  blocks <- function(df, x){
    df%>%
      filter(result.event == "Blocked Shot")%>% 
      filter(isOTgame == TRUE)%>%
      filter(about.period < 5)%>%
      filter(game_minute <61, game_minute != 1, game_minute != 21, game_minute != 41)%>%
      group_by(game_minute, gameID)%>%
      summarize(block_games = block == TRUE)%>%
      filter(block_games == TRUE)%>%
      ungroup()%>%
      group_by(game_minute)%>%
      count(block_games == TRUE)%>%
      summarize(bpm = n/x)
  }
  {
  blocks2010 <- blocks(update_data5_2010, games2010)
  blocks2011 <- blocks(update_data5_2011, games2011)
  blocks2013 <- blocks(update_data5_2013, games2013)
  blocks2014 <- blocks(update_data5_2014, games2014)
  blocks2015 <- blocks(update_data5_2015, games2015)
  blocks2016 <- blocks(update_data5_2016, games2016)
  blocks2017 <- blocks(update_data5_2017, games2017)
  blocks2018 <- blocks(update_data5_2018, games2018)
  pblocks2010 <- blocks(pupdate_data5_2010, pgames2010)
  pblocks2011 <- blocks(pupdate_data5_2011, pgames2011)
  pblocks2013 <- blocks(pupdate_data5_2013, pgames2013)
  pblocks2014 <- blocks(pupdate_data5_2014, pgames2014)
  pblocks2015 <- blocks(pupdate_data5_2015, pgames2015)
  pblocks2016 <- blocks(pupdate_data5_2016, pgames2016)
  pblocks2017 <- blocks(pupdate_data5_2017, pgames2017)
  pblocks2018 <- blocks(pupdate_data5_2018, pgames2018)
  }
  {
    dfblocks_55 <- c(colMeans(blocks2010 [1:52, 2]), colMeans(blocks2011 [1:52, 2]), colMeans(blocks2013 [1:52, 2]),
                   colMeans(blocks2014 [1:52, 2]), colMeans(blocks2015 [1:52, 2]), colMeans(blocks2016 [1:52, 2]),
                   colMeans(blocks2017 [1:52, 2]), colMeans(blocks2018 [1:52, 2]))*60
    dfblocks_5  <- c(colMeans(blocks2010 [53:57, 2]), colMeans(blocks2011 [53:57, 2]), colMeans(blocks2013 [53:57, 2]),
                   colMeans(blocks2014 [53:57, 2]), colMeans(blocks2015 [53:57, 2]), colMeans(blocks2016 [53:57, 2]),
                   colMeans(blocks2017 [53:57, 2]), colMeans(blocks2018 [53:57, 2]))*60
    df_mean_blocks <- data.frame(dfblocks_55, dfblocks_5, row.names = c('2010', '2011', '2013', '2014',
                                                                  '2015', '2016', '2017', '2018'))
    
    dfpblocks_55 <- c(colMeans(pblocks2010 [1:52, 2]), colMeans(pblocks2011 [1:52, 2]), colMeans(pblocks2013 [1:52, 2]),
                    colMeans(pblocks2014 [1:52, 2]), colMeans(pblocks2015 [1:52, 2]), colMeans(pblocks2016 [1:52, 2]),
                    colMeans(pblocks2017 [1:52, 2]), colMeans(pblocks2018 [1:52, 2]))*60
    dfpblocks_5  <- c(colMeans(pblocks2010 [53:57, 2]), colMeans(pblocks2011 [53:57, 2]), colMeans(pblocks2013 [53:57, 2]),
                    colMeans(pblocks2014 [53:57, 2]), colMeans(pblocks2015 [53:57, 2]), colMeans(pblocks2016 [53:57, 2]),
                    colMeans(pblocks2017 [53:57, 2]), colMeans(pblocks2018 [53:57, 2]))*60
    df_mean_pblocks <- data.frame(dfpblocks_55, dfpblocks_5, row.names = c('2010', '2011', '2013', '2014',
                                                                     '2015', '2016', '2017', '2018'))
  }
  
  graph_blocks <- function(df){
    df%>%
      ggplot(aes(x=game_minute, y=bpm, color = 'red'))+
      geom_line()+
      geom_smooth(aes(color = 'blue'), se=FALSE)+
      geom_hline(aes(yintercept = mean(bpm)))+
      theme_minimal()+
      scale_x_continuous(breaks = seq(0,60,5))
  }
  {
    graph_blocks(blocks2010)
    graph_blocks(blocks2011)
    graph_blocks(blocks2013)
    graph_blocks(blocks2014)
    graph_blocks(blocks2015)
    graph_blocks(blocks2016)
    graph_blocks(blocks2017)
    graph_blocks(blocks2018)
  }
  ##Graph of smoothed hit rates from each season
  {
    plot(x=NULL, y=NULL, xlim=c(2.2,57.8), ylim = c(25,40), main = 'Smoothed Block Rates per 60 minutes in Regular Season games\nthat go into OT by Season',
         xlab = 'Game Minute', ylab = 'Blocks per minute')+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = blocks2010$bpm*60), col = 'blue', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = blocks2011$bpm*60), col = 'green', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = blocks2013$bpm*60), col = 'red', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = blocks2014$bpm*60), col = 'orange', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = blocks2015$bpm*60), col = 'yellow', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = blocks2016$bpm*60), col = 'purple', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = blocks2017$bpm*60), col = 'black', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = blocks2018$bpm*60), col = 'lightblue', lwd = 5)+
      lines(x=c(55,55), y=c(25,40), lty = 'dashed')+
      legend('topleft', legend = c('2010', '2011', '2013', '2014', '2015', '2016', '2017', '2018'),
             col = c('blue', 'green', 'red', 'orange', 'yellow', 'purple', 'black', 'lightblue'), lty = 1, lwd = 5, cex = 0.8)
  }
  {
    plot(x=NULL, y=NULL, xlim=c(2.2,57.8), ylim = c(25,40), main = 'Smoothed Block Rates per 60 minutes in Playoff games\nthat go into OT by Season',
         xlab = 'Game Minute', ylab = 'Blocks per minute')+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pblocks2010$bpm*60), col = 'blue', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pblocks2011$bpm*60), col = 'green', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pblocks2013$bpm*60), col = 'red', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pblocks2014$bpm*60), col = 'orange', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pblocks2015$bpm*60), col = 'yellow', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pblocks2016$bpm*60), col = 'purple', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pblocks2017$bpm*60), col = 'black', lwd = 5)+
      lines(lowess(x=c(2:20, 22:40, 42:60), y = pblocks2018$bpm*60), col = 'lightblue', lwd = 5)+
      lines(x=c(55,55), y=c(25,40), lty = 'dashed')+
      legend('topright', legend = c('2010', '2011', '2013', '2014', '2015', '2016', '2017', '2018'),
             col = c('blue', 'green', 'red', 'orange', 'yellow', 'purple', 'black', 'lightblue'), lty = 1, lwd = 5, cex = 0.8)
  }
}

## function to find z-statistic 
z <- function(df){
  lambda1 <- sum(df[1:52,2])/52
  lambda2 <- sum(df[53:57,2])/5
  var1 <- var(df[1:52,2])/52
  var2 <- var(df[53:57,2])/5
  (lambda1 - lambda2)/sqrt(var1+var2)
}
options(scipen=999) #print all decimals w/o scientific notation
##put z-stat and p-value into data frames 
{
  shots_z <- round(c(z(shots2010),
                     z(shots2011),
                     z(shots2013),
                     z(shots2014),
                     z(shots2015),
                     z(shots2016),
                     z(shots2017),
                     z(shots2018)), digits = 2)
  shots_p <- round(c((1-pnorm(z(shots2010)))*2,
                     (1-pnorm(z(shots2011)))*2,
                     (1-pnorm(z(shots2013)))*2,
                     (1-pnorm(z(shots2014)))*2,
                     (1-pnorm(z(shots2015)))*2,
                     (1-pnorm(z(shots2016)))*2,
                     (1-pnorm(z(shots2017)))*2,
                     (1-pnorm(z(shots2018)))*2), digits = 4)
  df_shots <- data.frame(shots_z, shots_p, row.names = c('2010', '2011', '2013', '2014',
                                                         '2015', '2016', '2017', '2018'))
  penalties_z <- round(c(z(penalties2010),
                         z(penalties2011),
                         z(penalties2013),
                         z(penalties2014),
                         z(penalties2015),
                         z(penalties2016),
                         z(penalties2017),
                         z(penalties2018)), digits = 2)
  penalties_p <- round(c((1-pnorm(z(penalties2010)))*2,
                         (1-pnorm(z(penalties2011)))*2,
                         (1-pnorm(z(penalties2013)))*2,
                         (1-pnorm(z(penalties2014)))*2,
                         (1-pnorm(z(penalties2015)))*2,
                         (1-pnorm(z(penalties2016)))*2,
                         (1-pnorm(z(penalties2017)))*2,
                         (1-pnorm(z(penalties2018)))*2), digits = 4)
  df_penalties <- data.frame(penalties_z, penalties_p, row.names = c('2010', '2011', '2013', '2014',
                                                                     '2015', '2016', '2017', '2018'))
  faceoffs_z <- round(c(z(faceoffs2010),
                        z(faceoffs2011),
                        z(faceoffs2013),
                        z(faceoffs2014),
                        z(faceoffs2015),
                        z(faceoffs2016),
                        z(faceoffs2017),
                        z(faceoffs2018)), digits = 2)
  faceoffs_p <- round(c((1-pnorm(z(faceoffs2010)))*2,
                        (1-pnorm(z(faceoffs2011)))*2,
                        (pnorm(z(faceoffs2013)))*2,
                        (1-pnorm(z(faceoffs2014)))*2,
                        (1-pnorm(z(faceoffs2015)))*2,
                        (1-pnorm(z(faceoffs2016)))*2,
                        (pnorm(z(faceoffs2017)))*2,
                        (pnorm(z(faceoffs2018)))*2), digits = 4)
  df_faceoffs <- data.frame(faceoffs_z, faceoffs_p, row.names = c('2010', '2011', '2013', '2014',
                                                                  '2015', '2016', '2017', '2018'))
  hits_z <- round(c(z(hits2010),
                    z(hits2011),
                    z(hits2013),
                    z(hits2014),
                    z(hits2015),
                    z(hits2016),
                    z(hits2017),
                    z(hits2018)), digits = 2)
  hits_p <- round(c((1-pnorm(z(hits2010)))*2,
                    (1-pnorm(z(hits2011)))*2,
                    (1-pnorm(z(hits2013)))*2,
                    (1-pnorm(z(hits2014)))*2,
                    (1-pnorm(z(hits2015)))*2,
                    (1-pnorm(z(hits2016)))*2,
                    (1-pnorm(z(hits2017)))*2,
                    (1-pnorm(z(hits2018)))*2), digits = 4)
  df_hits <- data.frame(hits_z, hits_p, row.names = c('2010', '2011', '2013', '2014',
                                                      '2015', '2016', '2017', '2018'))
  blocks_z <- round(c(z(blocks2010),
                      z(blocks2011),
                      z(blocks2013),
                      z(blocks2014),
                      z(blocks2015),
                      z(blocks2016),
                      z(blocks2017),
                      z(blocks2018)), digits = 2)
  blocks_p <- round(c((pnorm(z(blocks2010)))*2,
                      (1-pnorm(z(blocks2011)))*2,
                      (pnorm(z(blocks2013)))*2,
                      (1-pnorm(z(blocks2014)))*2,
                      (1-pnorm(z(blocks2015)))*2,
                      (1-pnorm(z(blocks2016)))*2,
                      (pnorm(z(blocks2017)))*2,
                      (1-pnorm(z(blocks2018)))*2), digits = 4)
  df_blocks <- data.frame(blocks_z, blocks_p, row.names = c('2010', '2011', '2013', '2014',
                                                            '2015', '2016', '2017', '2018'))
  pshots_z <- round(c(z(pshots2010),
                     z(pshots2011),
                     z(pshots2013),
                     z(pshots2014),
                     z(pshots2015),
                     z(pshots2016),
                     z(pshots2017),
                     z(pshots2018)), digits = 2)
  pshots_p <- round(c((1-pnorm(z(pshots2010)))*2,
                     (1-pnorm(z(pshots2011)))*2,
                     (pnorm(z(pshots2013)))*2,
                     (pnorm(z(pshots2014)))*2,
                     (1-pnorm(z(pshots2015)))*2,
                     (1-pnorm(z(pshots2016)))*2,
                     (1-pnorm(z(pshots2017)))*2,
                     (1-pnorm(z(pshots2018)))*2), digits = 4)
  df_pshots <- data.frame(pshots_z, pshots_p, row.names = c('2010', '2011', '2013', '2014',
                                                            '2015', '2016', '2017', '2018'))
  ppenalties_z <- round(c(z(ppenalties2010),
                         z(ppenalties2011),
                         z(ppenalties2013),
                         z(ppenalties2014),
                         z(ppenalties2015),
                         z(ppenalties2016),
                         z(ppenalties2017),
                         z(ppenalties2018)), digits = 2)
  ppenalties_p <- round(c((1-pnorm(z(ppenalties2010)))*2,
                         (1-pnorm(z(ppenalties2011)))*2,
                         (1-pnorm(z(ppenalties2013)))*2,
                         (1-pnorm(z(ppenalties2014)))*2,
                         (1-pnorm(z(ppenalties2015)))*2,
                         (1-pnorm(z(ppenalties2016)))*2,
                         (1-pnorm(z(ppenalties2017)))*2,
                         (1-pnorm(z(ppenalties2018)))*2), digits = 4)
  df_ppenalties <- data.frame(ppenalties_z, ppenalties_p, row.names = c('2010', '2011', '2013', '2014',
                                                                        '2015', '2016', '2017', '2018'))
  pfaceoffs_z <- round(c(z(pfaceoffs2010),
                        z(pfaceoffs2011),
                        z(pfaceoffs2013),
                        z(pfaceoffs2014),
                        z(pfaceoffs2015),
                        z(pfaceoffs2016),
                        z(pfaceoffs2017),
                        z(pfaceoffs2018)), digits = 2)
  pfaceoffs_p <- round(c((1-pnorm(z(pfaceoffs2010)))*2,
                        (pnorm(z(pfaceoffs2011)))*2,
                        (pnorm(z(pfaceoffs2013)))*2,
                        (pnorm(z(pfaceoffs2014)))*2,
                        (pnorm(z(pfaceoffs2015)))*2,
                        (pnorm(z(pfaceoffs2016)))*2,
                        (pnorm(z(pfaceoffs2017)))*2,
                        (pnorm(z(pfaceoffs2018)))*2), digits = 4)
  df_pfaceoffs <- data.frame(pfaceoffs_z, pfaceoffs_p, row.names = c('2010', '2011', '2013', '2014',
                                                                     '2015', '2016', '2017', '2018'))
  phits_z <- round(c(z(phits2010),
                    z(phits2011),
                    z(phits2013),
                    z(phits2014),
                    z(phits2015),
                    z(phits2016),
                    z(phits2017),
                    z(phits2018)), digits = 2)
  phits_p <- round(c((1-pnorm(z(phits2010)))*2,
                    (1-pnorm(z(phits2011)))*2,
                    (1-pnorm(z(phits2013)))*2,
                    (1-pnorm(z(phits2014)))*2,
                    (1-pnorm(z(phits2015)))*2,
                    (1-pnorm(z(phits2016)))*2,
                    (1-pnorm(z(phits2017)))*2,
                    (1-pnorm(z(phits2018)))*2), digits = 4)
  df_phits <- data.frame(phits_z, phits_p, row.names = c('2010', '2011', '2013', '2014',
                                                         '2015', '2016', '2017', '2018'))
  pblocks_z <- round(c(z(pblocks2010),
                      z(pblocks2011),
                      z(pblocks2013),
                      z(pblocks2014),
                      z(pblocks2015),
                      z(pblocks2016),
                      z(pblocks2017),
                      z(pblocks2018)), digits = 2)
  pblocks_p <- round(c((pnorm(z(pblocks2010)))*2,
                      (pnorm(z(pblocks2011)))*2,
                      (pnorm(z(pblocks2013)))*2,
                      (1-pnorm(z(pblocks2014)))*2,
                      (pnorm(z(pblocks2015)))*2,
                      (pnorm(z(pblocks2016)))*2,
                      (1-pnorm(z(pblocks2017)))*2,
                      (1-pnorm(z(pblocks2018)))*2), digits = 4)
  df_pblocks <- data.frame(pblocks_z, pblocks_p, row.names = c('2010', '2011', '2013', '2014',
                                                               '2015', '2016', '2017', '2018'))
  
  df_list <- list('df_shots' = df_shots, 'df_penalties' = df_penalties, 'df_hits' = df_hits, 'df_faceoffs' = df_faceoffs,
                  'df_blocks' = df_blocks, 'df_pshots' = df_pshots, 'df_ppenalties' = df_ppenalties, 'df_phits' = df_phits,
                  'df_pfaceoffs' = df_pfaceoffs, 'df_pblocks' = df_pblocks)
}

df_final <- data.frame(round(df_mean_shots,2), round(df_mean_penalties,2), round(df_mean_hits,2), round(df_mean_faceoffs,2), round(df_mean_blocks,2))
df_pfinal <- data.frame(round(df_mean_pshots,2), round(df_mean_ppenalties,2), round(df_mean_phits,2), round(df_mean_pfaceoffs,2), round(df_mean_pblocks,2))

df_zp_final <- data.frame(df_shots, df_penalties, df_hits, df_faceoffs, df_blocks)
df_zp_pfinal <- data.frame(df_pshots, df_ppenalties, df_phits, df_pfaceoffs, df_pblocks)

##Dataframes to convert into Latex 
{
df_final_games <- data.frame(Games = c(games2010, games2011, games2013, games2014, games2015, games2016, games2017, games2018))
df_final2 <- cbind(df_final_games, df_final)
df_zp_final2 <- cbind(df_final_games, df_zp_final)

df_final_pgames <- data.frame(Games = c(pgames2010, pgames2011, pgames2013, pgames2014, pgames2015, pgames2016, pgames2017, pgames2018))
df_pfinal2 <- cbind(df_final_pgames, df_pfinal)
df_zp_pfinal2 <- cbind(df_final_pgames, df_zp_pfinal)
}

##For Latex
{
df_final2 %>%
  kbl(caption="Summary Statistics of Events per 60 Minutes in Regular Season OT Games",
      format= "latex",
      col.names = c("N", "First 55","Last 5","First 55","Last 5","First 55","Last 5",
                    "First 55", "Last 5", "First 55", "Last 5"),
      align="r",
      booktabs = T) %>%
  kable_styling(position = 'center', latex_options = c('hold_position', 'scale_down', 'repeat_header'))%>%
  kable_classic(full_width = F, html_font = "helvetica")%>%
  add_header_above(c('','', 'Shots'=2, 'Penalties'=2, 'Hits'=2, 'Faceoffs'=2, 'Blocks'=2), bold = T)

df_zp_final2 %>%
  kbl(caption="Summary Statistics of Z-test Statistics and Corresponding P values for Regular Season OT Games",
      format= "latex",
      col.names = c("N", "Z score","P-value","Z score","P-value","Z score","P-value",
                    "Z score", "P-value", "Z score", "P-value"),
      align="r",
      booktabs = T) %>%
  kable_styling(position = 'center', latex_options = c('hold_position', 'scale_down', 'repeat_header'))%>%
  kable_classic(full_width = F, html_font = "helvetica")%>%
  add_header_above(c('', '', 'Shots'=2, 'Penalties'=2, 'Hits'=2, 'Faceoffs'=2, 'Blocks'=2), bold = T)


df_pfinal2 %>%
  kbl(caption="Summary Statistics of Events per 60 Minutes in Playoff OT Games",
      format= "latex",
      col.names = c("N", "First 55","Last 5","First 55","Last 5","First 55","Last 5",
                    "First 55", "Last 5", "First 55", "Last 5"),
      align="r",
      booktabs = T) %>%
  kable_styling(position = 'center', latex_options = c('hold_position', 'scale_down', 'repeat_header'))%>%
  kable_classic(full_width = F, html_font = "helvetica")%>%
  add_header_above(c('', '', 'Shots'=2, 'Penalties'=2, 'Hits'=2, 'Faceoffs'=2, 'Blocks'=2), bold = T)

df_zp_pfinal2 %>%
  kbl(caption="Summary Statistics of Z-test Statistics and Corresponding P values for Playoff OT Games",
      format= "latex",
      col.names = c("N", "First 55","Last 5","First 55","Last 5","First 55","Last 5",
                    "First 55", "Last 5", "First 55", "Last 5"),
      align="r",
      booktabs = T) %>%
  kable_styling(position = 'center', latex_options = c('hold_position', 'scale_down', 'repeat_header'))%>%
  kable_classic(full_width = F, html_font = "helvetica")%>%
  add_header_above(c('', '', 'Shots'=2, 'Penalties'=2, 'Hits'=2, 'Faceoffs'=2, 'Blocks'=2), bold = T)
}
save.image(file = '~/Desktop/QSS_30/research_paper_workspace_final.Rdata')
