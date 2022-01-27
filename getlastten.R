#Getting team goals and shots by game state, beginning with game url
get_team_goals_shots <- function(url) {
  #Getting json_urls from game url
  ##Loading packages
  library(rvest)
  library(xml2)
  library(glue)
  
  ##Extracting key
  key <- read_html(url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-feed_key")
  
  ##Extracting client_code (league)
  client_code <- read_html(url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-league")
  
  ##Extracting game_id
  game_id <- read_html(url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-path")
  
  ##Extracting lang_code
  lang_code <- read_html(url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-lang")
  
  ##Formulating json_sum_url
  json_sum_url <- glue(
    "https://cluster.leaguestat.com/feed/index.php?feed=gc",
    "&key=",
    key,
    "&client_code=",
    client_code,
    "&game_id=",
    game_id,
    "&lang_code=",
    lang_code,
    "&fmt=json",
    "&tab=gamesummary")
  
  #Formulating json_pxp_url
  json_pxp_url <- glue(
    "https://cluster.leaguestat.com/feed/index.php?feed=gc",
    "&key=",
    key,
    "&client_code=",
    client_code,
    "&game_id=",
    game_id,
    "&lang_code=",
    lang_code,
    "&fmt=json",
    "&tab=pxpverbose")
  
  library(rlist)
  library(rjson)
  library(tidyverse)
  json_data <- rjson::fromJSON(file=json_pxp_url)
  #Getting list of game events
  event_list <- json_data[['GC']][['Pxpverbose']]
  #Selecting only goal, shot, or penalty events
  event_list <- list.filter(event_list, event == 'goal' | event == 'shot' | event == 'penalty')
  ###Adding time_in_sec to each event
  #Calculating time in sec
  for (i in 1:length(event_list)) {
    #If event occurs in overtime
    if(event_list[[i]]$period_id == 'OT') {
      #If event is a shot or a goal
      if(event_list[[i]]$event == 'shot' | event_list[[i]]$event == 'goal') {
        event_list[[i]] <- event_list[[i]] %>% list.append(time_in_sec = 3600 + (strtoi(str_split(event_list[[i]]$time_formatted, ':')[[1]][1]) * 60) + strtoi(str_split(event_list[[i]]$time_formatted, ':')[[1]][2],base=10L))
        #If event is a penalty
      } else if(event_list[[i]]$event == 'penalty') {
        event_list[[i]] <- event_list[[i]] %>% list.append(time_in_sec = 3600 + (strtoi(str_split(event_list[[i]]$time_off_formatted, ':')[[1]][1]) * 60) + strtoi(str_split(event_list[[i]]$time_off_formatted, ':')[[1]][2],base=10L))
      }
      #If event occurs in regular time
    } else {
      #If event is a shot or a goal
      if(event_list[[i]]$event == 'shot' | event_list[[i]]$event == 'goal') {
        event_list[[i]] <- event_list[[i]] %>% list.append(time_in_sec = ((strtoi(event_list[[i]]$period_id)-1)*1200) + (strtoi(str_split(event_list[[i]]$time_formatted, ':')[[1]][1]) * 60) + strtoi(str_split(event_list[[i]]$time_formatted, ':')[[1]][2],base=10L))
        #If event is a penalty
      } else if(event_list[[i]]$event == 'penalty') {
        event_list[[i]] <- event_list[[i]] %>% list.append(time_in_sec = ((strtoi(event_list[[i]]$period_id)-1)*1200) + (strtoi(str_split(event_list[[i]]$time_off_formatted, ':')[[1]][1]) * 60) + strtoi(str_split(event_list[[i]]$time_off_formatted, ':')[[1]][2],base=10L))
      }
    }
  } ###End adding time in sec to each event
  #Removing fighting coincidental penalties and misconduct penalties
  to_remove = c()
  for (i in 1:(length(event_list)-1)) {
    if(event_list[[i]]$event == 'penalty' & event_list[[i+1]]$event == 'penalty' & event_list[[i]]$home != event_list[[i+1]]$home) {
      if(event_list[[i]]$minutes == '5.00' & event_list[[i+1]]$minutes == '5.00') {
        to_remove <- to_remove %>% append(c(i, i+1))
      }
      #Removing misconduct penalties
    } else if(event_list[[i]]$event == 'penalty') {
      if(event_list[[i]]$minutes == '0.00') {
        to_remove <- to_remove %>% append(i)
      }
    }
  }
  event_list[to_remove] = NULL
  #End removing fighting coincidental penalties and misconduct penalties
  
  #I think it will be best to maintain the active penalty list as a list similar to the event_list
  #For each penalty, create a list with player id, home or away, duration, start_time, end_time
  #Then add this list to a list of active penalties or waiting penalties
  
  #Fixing empty net goals
  for (i in 1:length(event_list)) {
    if(event_list[[i]]$event == 'shot') {
      if(event_list[[i]]['goalie']$goalie$player_id == '') {
        event_list[[i]]['goalie'] <- NULL
      }
    }
  }
  
  #Initializing active and waiting penalty lists
  active_penalties <- vector(mode='list')
  waiting_penalties <- vector(mode='list')
  #Initializing to_skip vector
  to_skip = c()
  
  #Looping through each play
  for (i in 1:length(event_list)) {
    #Assigning 5v5 to last i value if it is a penalty
    if(event_list[[i]]$event == 'penalty' & i == length(event_list)) {
      event_list[[i]] <- event_list[[i]] %>% list.append(game_state = event_list[[i-1]]$game_state)
      next
    }
    
    #Skipping iteration if i is in to_skip vector
    if(i %in% to_skip) {
      #Set game state to the game state of the previous event
      event_list[[i]] <- event_list[[i]] %>% list.append(game_state = event_list[[i-1]]$game_state)
      #Continue to next iteration
      next
    }
    #If event is a penalty and there are fewer than 2 active penalties for the infraction team
    if(event_list[[i]]$event == 'penalty' & length(list.filter(active_penalties, home == event_list[[i]]$home)) < 2) { #Error line
      #If the penalty is a double minor
      #Initializing double_minor boolean as FALSE
      double_minor <- FALSE
      if(event_list[[i]]$event == 'penalty' & event_list[[i+1]]$event == 'penalty') {
        if(event_list[[i]]$player_id == event_list[[i+1]]$player_id & event_list[[i]]$minutes == '2.00' & event_list[[i+1]]$minutes == '2.00') {
          #Add the first penalty to active_penalties
          active_penalties <- active_penalties %>% append(list(list(player_id = event_list[[i]]$player_id, home = strtoi(event_list[[i]]$home), duration = as.integer(event_list[[i]]$minutes), start_time = event_list[[i]]$time_in_sec, end_time = event_list[[i]]$time_in_sec + (as.integer(event_list[[i]]$minutes)*60))))
          #Add the second penalty to waiting_penalties
          waiting_penalties <- waiting_penalties %>% append(list(list(player_id = event_list[[i]]$player_id, home = strtoi(event_list[[i]]$home), duration = as.integer(event_list[[i]]$minutes), start_time = event_list[[i]]$time_in_sec, end_time = event_list[[i]]$time_in_sec + (as.integer(event_list[[i]]$minutes)*60))))
          #Add i+1 to to_skip vector
          to_skip <- to_skip %>% append(i+1)
          #Setting double minor to TRUE
          double_minor <- TRUE
        }
      }
      #If penalty is not a double minor
      if(double_minor == FALSE) {
        active_penalties <- active_penalties %>% append(list(list(player_id = event_list[[i]]$player_id, home = strtoi(event_list[[i]]$home), duration = as.integer(event_list[[i]]$minutes), start_time = event_list[[i]]$time_in_sec, end_time = event_list[[i]]$time_in_sec + (as.integer(event_list[[i]]$minutes)*60))))
      }
      #If event is a penalty and there are 2 or more active penalties for the infraction team
    } else if(event_list[[i]]$event == 'penalty') {
      waiting_penalties <- waiting_penalties %>% append(list(list(player_id = event_list[[i]]$player_id, home = strtoi(event_list[[i]]$home), duration = as.integer(event_list[[i]]$minutes), start_time = event_list[[i]]$time_in_sec, end_time = event_list[[i]]$time_in_sec + (as.integer(event_list[[i]]$minutes)*60))))
    }
    #Removing expired penalties and moving waiting penalties to active penalties when applicable -- works well
    for (j in active_penalties) {
      #If active penalty end time has passed
      if(event_list[[i]]$time_in_sec > j$end_time) {
        #Getting index of j
        index <- match(list(j), active_penalties)
        #Removing j from active_penalties
        active_penalties[index] <- NULL
        #If there is a penalty in waiting for the team whose penalty expired
        if(length(list.filter(waiting_penalties, home == j$home))>0) {
          #Get the first penalty in waiting for that side as temp
          temp <- list.filter(waiting_penalties, home == j$home)[[1]]
          #Set the penalty start time to the end time of the expired penalty
          temp$start_time <- j$end_time
          #Set the penalty end time to the start time plus the duration
          temp$end_time <- temp$start_time + (temp$duration*60)
          #Add the penalty to active_penalties
          active_penalties <- active_penalties %>% append(list(temp))
          #Remove the penalty from waiting_penalties
          waiting_penalties[[1]] <- NULL
        }
      }
    }
    #Recording game state
    event_list[[i]] <- event_list[[i]] %>% list.append(game_state = paste((5-length(list.filter(active_penalties, home == event_list[[i]]$home))), (5-length(list.filter(active_penalties, home != event_list[[i]]$home))), sep='v'))
    #Handling powerplay goals that cause expiration of a penalty
    #If a play is a goal
    penalty_removed <- FALSE
    if(event_list[[i]]$event == 'goal') {
      #Check if scoring team has less active penalties than non-scoring team
      if(length(list.filter(active_penalties, home == event_list[[i]]$home))<length(list.filter(active_penalties, home != event_list[[i]]$home))) {
        #Get first non-scoring team penalty from active_penalties as temp
        temp <- list.filter(active_penalties, home != event_list[[i]]$home)[[1]]
        #Getting index of temp in active_penalties
        index <- match(list(temp), active_penalties)
        #Removing penalty if it is a minor and setting penalty_removed to TRUE
        if(temp$duration == 2) {
          active_penalties[index] <- NULL
          penalty_removed <- TRUE
        }
      }
      #If a penalty was removed for non-scoring team and non-scoring team has a penalty in waiting, shift the first penalty in waiting to active penalties
      if(penalty_removed == TRUE & length(list.filter(waiting_penalties, home != event_list[[i]]$home))>0) {
        #Getting active_penalties player ids
        active_penalty_ids <- c()
        for (z in active_penalties) {
          active_penalty_ids <- active_penalty_ids %>% append(z$player_id)
        }
        #If there is a penalty in waiting that was not taken by a player already serving a penalty for that side
        if(length(list.filter(waiting_penalties, home != event_list[[i]]$home & !(player_id %in% active_penalty_ids)))>0) {
          #Get the first penalty in waiting for non-scoring team that was not taken by a player already serving a penalty for that side as temp
          temp <- list.filter(waiting_penalties, home != event_list[[i]]$home & !(player_id %in% active_penalty_ids))[[1]]
          #Set the penalty start time to the time of goal
          temp$start_time <- event_list[[i]]$time_in_sec
          #Set the penalty end time to the start time + duration in seconds
          temp$end_time <- temp$start_time + (temp$duration*60)
          #Add temp to active_penalties
          active_penalties <- active_penalties %>% append(list(temp))
          #Getting index of penalty in waiting_penalties
          index <- match(list(temp), waiting_penalties)
          #Removing temp from waiting_penalties
          waiting_penalties[index] <- NULL
        }
      }
    }
  }#End main for loop
  
  ###Getting game id and home and away teams from other json file
  sum_data <- jsonlite::fromJSON(json_sum_url)
  game_id <- sum_data[['GC']][['Parameters']][['game_id']]
  home_code <- sum_data[['GC']][['Gamesummary']][['home']][['code']]
  away_code <- sum_data[['GC']][['Gamesummary']][['visitor']][['code']]
  home_id <- sum_data[['GC']][['Gamesummary']][['home']][['team_id']]
  away_id <- sum_data[['GC']][['Gamesummary']][['visitor']][['team_id']]
  
  #Removing game goal id if it is empty
  for (i in event_list) {
    if(i$event == 'shot') {
      if(i$game_goal_id == "") {
        i$game_goal_id <- NULL
      }
    } else if(i$event == 'goal') {
      if(i$assist1_player_id == "") {
        i$assist1_player_id <- NULL
      }
      if(i$assist2_player_id == "") {
        i$assist2_player_id <- NULL
      }
    }
  }
  
  #Removing empty list elements
  library(sjmisc)
  for (i in 1:length(event_list)) {
    for(j in 1:length(event_list[[i]])) {
      if(event_list[[i]][j] == '') {
        event_list[[i]][j] <- NULL
      }
    }
    for(j in 1:length(event_list[[i]])) {
      if(is_empty(event_list[[i]][j])) {
        event_list[[i]][j] <- NULL
      }
    }
  }
  
  #Removing empty list elements from goalie
  for(i in 1:length(event_list)) {
    if(event_list[[i]]$event == "shot") {
      if(!(is.null(event_list[[i]][['goalie']]))) {
        if(event_list[[i]]$goalie$player_id == '0') {
          event_list[[i]]$goalie <- NULL
        }
      }
    }
  }
  
  library (plyr)
  df <- ldply (event_list, data.frame)
  #Getting shots by game state for each team
  home_shots_df <- df %>% subset(event == 'shot' & player.team_code == home_code)
  home_total_shots <- nrow(home_shots_df)
  home_ev_shots <- nrow(subset(home_shots_df, game_state == '5v5' | game_state == '4v4' | game_state == '3v3'))
  home_pp_shots <- nrow(subset(home_shots_df, game_state == '5v4' | game_state == '5v3' | game_state == '4v3'))
  home_sh_shots <- nrow(subset(home_shots_df, game_state == '4v5' | game_state == '3v5' | game_state == '3v4'))
  away_shots_df <- df %>% subset(event == 'shot' & player.team_code == away_code)
  away_total_shots <- nrow(away_shots_df)
  away_ev_shots <- nrow(subset(away_shots_df, game_state == '5v5' | game_state == '4v4' | game_state == '3v3'))
  away_pp_shots <- nrow(subset(away_shots_df, game_state == '5v4' | game_state == '5v3' | game_state == '4v3'))
  away_sh_shots <- nrow(subset(away_shots_df, game_state == '4v5' | game_state == '3v5' | game_state == '3v4'))
  
  #Getting goals by game state for each team
  home_goals_df <- df %>% subset(event == 'goal' & team_id == home_id)
  home_total_goals <- nrow(home_goals_df)
  home_ev_goals <- nrow(subset(home_goals_df, game_state == '5v5' | game_state == '4v4' | game_state == '3v3'))
  home_pp_goals <- nrow(subset(home_goals_df, game_state == '5v4' | game_state == '5v3' | game_state == '4v3'))
  home_sh_goals <- nrow(subset(home_goals_df, game_state == '4v5' | game_state == '3v5' | game_state == '3v4'))
  away_goals_df <- df %>% subset(event == 'goal' & team_id == away_id)
  away_total_goals <- nrow(away_goals_df)
  away_ev_goals <- nrow(subset(away_goals_df, game_state == '5v5' | game_state == '4v4' | game_state == '3v3'))
  away_pp_goals <- nrow(subset(away_goals_df, game_state == '5v4' | game_state == '5v3' | game_state == '4v3'))
  away_sh_goals <- nrow(subset(away_goals_df, game_state == '4v5' | game_state == '3v5' | game_state == '3v4'))
  
  game_goal_shot_info <- data.frame(game_id = c(game_id, game_id), team = c(home_code, away_code), total_goals_for = c(home_total_goals, away_total_goals), ev_goals_for = c(home_ev_goals, away_ev_goals), pp_goals_for = c(home_pp_goals, away_pp_goals), sh_goals_for = c(home_sh_goals, away_sh_goals), total_goals_against = c(away_total_goals, home_total_goals), ev_goals_against = c(away_ev_goals, home_ev_goals), pp_goals_against = c(away_pp_goals, home_pp_goals), sh_goals_against = c(away_sh_goals, home_sh_goals), total_shots_for = c(home_total_shots, away_total_shots), ev_shots_for = c(home_ev_shots, away_ev_shots), pp_shots_for = c(home_pp_shots, away_pp_shots), sh_shots_for = c(home_sh_shots, away_sh_shots), total_shots_against = c(away_total_shots, home_total_shots), ev_shots_against = c(away_ev_shots, home_ev_shots), pp_shots_against = c(away_pp_shots, home_pp_shots), sh_shots_against = c(away_sh_shots, home_sh_shots))
  return(game_goal_shot_info)
}

get_last_ten <- function(league, team_code) {
  #Getting team's last ten games from full whl schedule
  library(glue)
  library(rjson)
  library(tidyverse)
  library(rlist)
  if(league == 'ohl') {
    json_url <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=schedule&key=2976319eb44abe94&fmt=json&fmt=json&client_code=ohl&lang=en&season_id=70&team_id=&league_code="
  } else if(league == 'whl') {
    json_url <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=schedule&key=41b145a848f4bd67&fmt=json&fmt=json&client_code=whl&lang=en&season_id=275&team_id=&league_code="
  } else {
    json_url <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=schedule&key=f322673b6bcae299&fmt=json&fmt=json&client_code=lhjmq&lang=en&season_id=199&team_id=&league_code="
  }
  #Pulling schedule data from json file
  data <- rjson::fromJSON(file = json_url)[['SiteKit']][['Schedule']]
  #Getting only completed games by the specified team
  played_games <- data %>% list.filter(final == '1' & (home_team_code == team_code | visiting_team_code == team_code))
  #Getting game ids for the last ten games
  game_ids = c()
  for (i in (length(played_games)-9):length(played_games)) {
    game_ids <- game_ids %>% append(played_games[[i]][['game_id']])
  }
  #Formulating game url from game ids
  if(league == 'ohl') {
    game_urls <- paste(paste("https://ontariohockeyleague.com/gamecentre/", game_ids, sep=""),"/boxscore", sep="")
  } else if(league == 'whl') {
    game_urls <- paste(paste("https://whl.ca/gamecentre/", game_ids, sep=""),"/boxscore", sep="")
  } else {
    game_urls <- paste(paste("https://theqmjhl.ca/gamecentre/", game_ids, sep=""),"/boxscore", sep="")
  }
  #Initialized game_data datafrmae
  game_data <- data.frame()
  #Populating game_data dataframe with the shot and goal game state data for each game
  for (i in game_urls) {
    print(i)
    game_info <- get_team_goals_shots(i)
    game_data <- rbind(game_data, game_info)
  }
  game_data <- game_data %>% subset(team == team_code)
  return(game_data)
}

#b <- get_last_ten("qmjhl", "Gat")

get_team_goals_shots("https://theqmjhl.ca/gamecentre/29362/boxscore")
