#install.packages(c("tidyverse","GGally","gridExtra","esquisse","caret","baseballr","retrosheet","rvest"))
library(retrosheet)
library(baseballr)
library(tidyverse)
library(GGally)
library(esquisse)
library(gridExtra)
library(caret)
library(readr)
library(dplyr)
library(rvest)
library(car)
library(ggplot2)

# Remove scientific notation
options(scipen = 9999)

### WOBA WEIGHTS ###
wOBAWgt <- fg_guts() %>%
  select(-c(woba_scale,runSB,runCS,lg_r_pa,lg_r_w,cFIP))


### PITCHING WOBA MODEL ###
# Get 2023 pitching stats
ptchData <- bref_daily_pitcher(as.Date("2023-03-30"),as.Date(Sys.Date()))
# Calculate wOBA and predictor variable fields
ptchData <- ptchData %>%
  filter(BF >= 75 & HR != 0 & BB != 0 & H != 0) %>%
  left_join(wOBAWgt, by = 'season') %>%
  mutate(wOBA = ((wBB * uBB) + (wHBP * HBP) + (w1B * X1B) + (w2B * X2B) + (w3B * X3B) + (wHR * HR)) / (AB + BB - IBB + SF + HBP),
         HR9 = (HR / IP) * 9,
         BB9 = (BB / IP) * 9,
         SO9 = (SO / IP) * 9)
# Build linear regression model for calculating pitcher wOBA
model1 = lm(wOBA ~ HR9 + BB9 + SO9 + BAbip, data = ptchData)
summary(model1)
# Identify influential outliers
influential <- cooks.distance(model1)[(cooks.distance(model1) > 4 / (nrow(ptchData)-1-4))]
# Create new model excluding outlier data points
ptchData2 <- ptchData %>% anti_join(ptchData[names(influential),])
model2 <- lm(wOBA ~ HR9 + BB9 + SO9 + BAbip, data = ptchData2)
summary(model2)
# Evaluate model with partial regression plots
par(mfrow = c(2, 2))
crPlots(model2)


### PLAYER PROJECTIONS ###
# Unwanted array list of accented characters
removeSpecial = list('á'='a','á'='a','Á'='A','ç'='c','é'='e','í'='i','ñ'='n','ó'='o','Ó'='O','ú'='u','ý'='y')
# Create a vector of Fangraphs team #'s
team_ids <- 1:30
### PITCHING ###
# Scrape Fangraphs pitching projections team by team using sapply
ptchList <- sapply(team_ids, function(team_id) {
  page <- read_html(paste0("https://www.fangraphs.com/projections?pos=&stats=pit&type=rfangraphsdc&lg=&team=", team_id, "&statgroup=fantasy"))
  table <- html_table(html_nodes(page, ".table-fixed"), fill = TRUE)[[1]]
  table$team_id <- team_id
  table
}, simplify = FALSE)
fgPtchProj <- bind_rows(ptchList) %>%
  mutate()
# Convert integer fields to numeric
fgPtchProj <- fgPtchProj %>%
  mutate_if(is.integer, as.numeric)
# Remove special characters from player name
fgPtchProj <- fgPtchProj %>%
  mutate(Name = chartr(paste(names(removeSpecial), collapse=''),
                       paste(removeSpecial, collapse=''),Name),
         season = year(Sys.Date()))
# Clean pitching projection data frame
fgPtchProj <- fgPtchProj %>%
  select(-c(`#`,`team_id`)) %>%
  rename('SO9' = `K/9`,
         'BB9' = `BB/9`,
         'HR9' = `HR/9`,
         'BAbip' = BABIP)
# Calculate predicted wOBA using the model1
fgPtchProj$wOBA <- predict(model2, newdata = fgPtchProj)
# Final pitching projections
fgPtchProj <- fgPtchProj %>%
  filter(IP != 0) %>%
  select(Name,Team,GS,IP,HR9,BB9,SO9,BAbip,wOBA,season)


### BATTING ###
# Scrape advanced bat data for all teams using sapply
advBatList <- sapply(team_ids, function(team_id) {
  page <- read_html(paste0("https://www.fangraphs.com/projections?pos=all&stats=bat&type=rthebatx&lg=&team=", team_id,"&statgroup=fantasy"))
  table <- html_table(html_nodes(page, ".table-fixed"), fill = TRUE)[[1]]
  table$team_id <- team_id
  table
}, simplify = FALSE)
# Advanced Fangraphs batting projections
fgBatProj <- bind_rows(advBatList) %>%
  select(Name,PA,AB,Team,wOBA)
# Convert integer fields to numeric
fgBatProj <- fgBatProj %>%
  mutate_if(is.integer, as.numeric)
# Remove special characters from player name
fgBatProj <- fgBatProj %>%
  mutate(Name = chartr(paste(names(removeSpecial), collapse=''),
                       paste(removeSpecial, collapse=''),Name),
         season = year(Sys.Date()))


### BALLPARK FACTORS ###
# Define the data
teamMascot <- c("Angels", "Astros", "Athletics", "Blue Jays", "Braves", "Brewers", "Cardinals", "Cubs", "Diamondbacks", "Dodgers", "Giants", "Guardians", "Mariners", "Marlins", "Mets", "Nationals", "Orioles", "Padres", "Phillies", "Pirates", "Rangers", "Rays", "Red Sox", "Reds", "Rockies", "Royals", "Tigers", "Twins", "White Sox", "Yankees")
BH <- c(103, 100, 95, 98, 100, 100, 96, 100, 101, 101, 101, 98, 91, 98, 95, 100, 101, 94, 102, 101, 99, 96, 110, 110, 112, 103, 97, 99, 102, 99) / 100
RH <- c(104, 100, 94, 99, 101, 99, 98, 98, 99, 101, 101, 97, 93, 97, 93, 97, 101, 93, 102, 100, 99, 98, 108, 110, 117, 103, 98, 98, 102, 99) / 100
LH <- c(100, 99, 97, 97, 99, 101, 92, 102, 103, 101, 100, 99, 88, 100, 97, 103, 100, 95, 102, 102, 101, 93, 114, 109, 105, 103, 95, 102, 103, 98) / 100
# Adjust values closer to 1 by halving the value
BH <- ifelse(BH > 1, (BH - 1) / 2 + 1, (BH - 1) / 2 + 1)
RH <- ifelse(RH > 1, (RH - 1) / 2 + 1, (RH - 1) / 2 + 1)
LH <- ifelse(LH > 1, (LH - 1) / 2 + 1, (LH - 1) / 2 + 1)
# Create ballpark factor data frame
parkFx <- data.frame(teamMascot = teamMascot, BH = BH, RH = RH, LH = LH)


### TEAM DATA ###
teamDF <- mlb_teams(sport_ids = 1) %>%
  select(team_id, team_full_name, team_abbreviation, franchise_name, club_name, venue_id, venue_name) %>%
  mutate(team_abbreviation = case_when(
    team_abbreviation == "AZ"  ~ "ARI",
    team_abbreviation == "CWS" ~ "CHW",
    team_abbreviation == "KC"  ~ "KCR",
    team_abbreviation == "SD"  ~ "SDP",
    team_abbreviation == "SF"  ~ "SFG",
    team_abbreviation == "TB"  ~ "TBR",
    team_abbreviation == "WSH" ~ "WSN",
    TRUE ~ team_abbreviation)) %>%
  rename('teamID' = 'team_id',
         'teamName' = 'team_full_name',
         'teamCode' = 'team_abbreviation',
         'teamArea' = 'franchise_name',
         'teamMascot' = 'club_name',
         'venueID' = 'venue_id',
         'venueName' = 'venue_name')
# Vector of Team ID's
teamIDVec <- teamDF$teamID


### PLAYER DATA ###
# Get full season rosters
plyrList <- lapply(teamIDVec,mlb_rosters, season = year(Sys.Date()), roster_type = "fullRoster")
# Get player bat and pitch hand
plyrHand <- mlb_sports_players(sport_id = 1, season = year(Sys.Date())) %>%
  select(player_id,bat_side_code,pitch_hand_code)
# Batters
batterDF <- do.call(rbind.data.frame, plyrList) %>%
  filter(position_code != 1) %>%
  left_join(plyrHand, by = c("person_id" = "player_id")) %>%
  select(person_id,person_full_name,status_code,team_id,bat_side_code)
# Pitchers
pitcherDF <- do.call(rbind.data.frame, plyrList) %>%
  filter(position_code == 1 | position_code == "Y") %>%
  left_join(plyrHand, by = c("person_id" = "player_id")) %>%
  select(person_id,person_full_name,status_code,team_id,pitch_hand_code)

# Statcast expected stats for qualified batters
statcastBat <- statcast_leaderboards(
  leaderboard = "expected_statistics",
  year = 2023,
  min_pa = "q",
  player_type = "batter") %>%
  select(player_id,est_woba)

# Fill in batting projections with supplementary data
plyrBatProj <- fgBatProj %>%
  left_join(teamDF, by = c("Team" = "teamCode")) %>%
  left_join(batterDF, by = c("teamID" = "team_id",
                             "Name" = "person_full_name")) %>%
  left_join(statcastBat, by = c("person_id" = "player_id")) %>%
  mutate(batHand = case_when(bat_side_code == "R" ~ "R",
                             bat_side_code == "L" ~ "L",
                             TRUE ~ "S")) %>%
  left_join(parkFx, by = c('teamMascot')) %>%
  mutate(xwOBA = case_when(
    !is.na(est_woba) & batHand == 'R' ~ (est_woba + (wOBA / RH)) / 2,
    is.na(est_woba) & batHand == 'R' ~ wOBA / RH,
    !is.na(est_woba) & batHand == 'L' ~ (est_woba + (wOBA / LH)) / 2,
    is.na(est_woba) & batHand == 'L' ~ wOBA / LH,
    TRUE ~ wOBA / BH)) %>%
  select(person_id,Name,teamID,teamMascot,batHand,status_code,xwOBA) %>%
  rename("batID" = "person_id",
         "batName" = "Name",
         "status" = "status_code")

# Statcast expected stats for qualified pitchers
statcastPitch <- statcast_leaderboards(
  leaderboard = "expected_statistics",
  year = 2023,
  min_pa = "q",
  player_type = "pitcher") %>%
  select(player_id,est_woba)

# Fill in pitching projections with supplementary data
plyrPtchProj <- fgPtchProj %>%
  left_join(teamDF, by = c("Team" = "teamCode")) %>%
  left_join(pitcherDF, by = c("teamID" = "team_id",
                             "Name" = "person_full_name")) %>%
  left_join(statcastPitch, by = c("person_id" = "player_id")) %>%
  mutate(pitchHand = case_when(pitch_hand_code == "R" ~ "R",
                             pitch_hand_code == "L" ~ "L",
                             TRUE ~ "S")) %>%
  left_join(parkFx, by = c('teamMascot')) %>%
  mutate(xwOBA = case_when(
      !is.na(est_woba) ~ (est_woba + (wOBA / (((BH - 1) * 1) + 1))) / 2,
      TRUE ~ wOBA / BH),
      IPGS = case_when(
        GS > 3 ~ IP / GS,
        TRUE ~ NA)) %>%
  select(person_id,Name,teamID,teamMascot,pitchHand,status_code,xwOBA,IPGS) %>%
  rename("pitchID" = "person_id",
         "pitchName" = "Name",
         "status" = "status_code")

# Get team bullpens
teamBullpen <- bind_rows(
  tables <- read_html("https://www.fangraphs.com/depthcharts.aspx?position=RP") %>%
    html_nodes(".depth_chart") %>%
    html_table()) %>%
  filter(is.na(Team) & Name != "Total") %>%
  select(-c(Team))
# Remove special characters from player name
teamBullpen <- teamBullpen %>%
  mutate(Name = chartr(paste(names(removeSpecial), collapse=''),
                       paste(removeSpecial, collapse=''),Name)) %>%
  left_join(plyrPtchProj, by = c("Name" = "pitchName")) %>%
  filter(status == "A") %>%
  group_by(teamID, teamMascot) %>%
  mutate(teamWgt = IP / sum(IP)) %>%
  ungroup() %>%
  mutate(wOBAWgt = teamWgt * xwOBA) %>%
  group_by(teamID, teamMascot) %>%
  summarise(xwOBA = sum(wOBAWgt))


### MLB SCHEDULE ###
# Today's schedule
todayGames <- mlb_schedule(season = year(Sys.Date()), level_ids = "1") %>%
  filter(date == Sys.Date()) %>%
  select(game_pk,date,game_number,teams_away_team_id,teams_home_team_id,venue_id,scheduled_innings) %>%
  rename("gameID"="game_pk",
         "gameDate"="date",
         "gameNumber"="game_number",
         "homeTeamID"="teams_home_team_id",
         "awayTeamID"="teams_away_team_id",
         "venueID"="venue_id",
         "innings"="scheduled_innings")
# Vector of Game ID's
gameIDVec <- todayGames$gameID

### GET BATTING ORDER ###
# Today's team batting orders
todayBatters <- map_dfr(seq_along(gameIDVec), function(i) {
  data <- get_batting_orders(gameIDVec[i])
  data$gameID <- gameIDVec[i]
  data}) %>%
  rename("batID"="id",
         "batOrder"="batting_order",
         "teamLocation"="team") %>%
  select(gameID,batID,batOrder,teamID,teamLocation) %>%
  left_join(plyrBatProj,by = c('batID','teamID')) %>%
  left_join(todayGames,by = 'gameID') %>%
  mutate(
    oppID = case_when(
      teamID == homeTeamID ~ awayTeamID,
      TRUE ~ homeTeamID),
    venueTeamID = homeTeamID) %>%
  distinct(gameID,gameDate,teamID,teamMascot,batOrder,batID,batName,batHand,xwOBA,oppID,teamLocation) %>%
  arrange(gameID,teamLocation,batOrder)

# Batting order weights
batOrderWgt <- data.frame(orderNum = c(1,2,3,4,5,6,7,8,9),
                          orderValue = c(600,586,571,558,546,532,517,502,486),
                          orderDenominator = c(4898,4898,4898,4898,4898,4898,4898,4898,4898)
) %>%
  mutate(orderWgt = orderValue / orderDenominator,
         orderNum = as.character(orderNum))


### PROBABLE PITCHERS ###
# Probable starting pitchers
probablePitchList <- lapply(gameIDVec, get_probables_mlb)
# Today starting pitchers
todayPitchers <- do.call(rbind.data.frame, probablePitchList) %>%
  rename('gameID'='game_pk',
         'oppPtchID'='id',
         'oppPtchTeamID'='team_id') %>%
  select(gameID,oppPtchID,oppPtchTeamID) %>%
  left_join(plyrPtchProj,by = c('oppPtchID'='pitchID','oppPtchTeamID'='teamID')) %>%
  left_join(todayGames,by = 'gameID') %>%
  mutate(
    oppID = case_when(
      oppPtchTeamID == homeTeamID ~ awayTeamID,
      TRUE ~ homeTeamID),
    venueTeamID = homeTeamID) %>%
  mutate(sIP = case_when(
           IPGS > 7.5 ~ 3,
           IPGS < 2 ~ 3,
           is.na(IPGS) ~ 3,
           TRUE ~ IPGS)) %>%
  rename('oppPtchName'='pitchName',
         'oppMascot'='teamMascot',
         'swOBA' = 'xwOBA') %>%
  select(gameID,oppPtchTeamID,oppMascot,oppPtchID,oppPtchName,pitchHand,sIP,swOBA,innings)

### MODEL CONSTRUCTION ###
# Specify the years for game data
years <- 2015:2022
# Create an empty list to store game logs
gameLogs <- list()
# Retrieve game data for each year and store in the list
for (year in years) {
  gameLogs[[as.character(year)]] <- getRetrosheet("game", year)
}
# Combine game logs into a single data frame
gameLogs <- bind_rows(gameLogs, .id = "Year") %>%
  mutate(GameID = paste(HmTm,"_",VisTm,"_",Date,"_",ParkID,"_",DblHdr,"_",DayNight,sep = ""),
         season = as.integer(substring(Date,1,4)),
         HmUBB = (HmBB - HmIBB),
         VisUBB = (VisBB - VisIBB),
         NumInn = (NumOuts / 3) / 2) %>%
  left_join(fg_guts(),by='season') %>%
  mutate(HmS = HmH - (HmD + HmT + HmHR),
         VisS = VisH - (VisD + VisT + VisHR),
         HmwOBA = ((wBB * HmUBB) + (wHBP * HmHBP) + (w1B * HmS) + (w2B * HmD) + (w3B * HmT) + (wHR * HmHR)) / (HmAB + HmBB - HmIBB + HmSF + HmHBP),
         ViswOBA = ((wBB * VisUBB) + (wHBP * VisHBP) + (w1B * VisS) + (w2B * VisD) + (w3B * VisT) + (wHR * VisHR)) / (VisAB + VisBB - VisIBB + VisSF + VisHBP)
  ) %>%
  rename("TeamHm" = "HmTm",
         "TeamVis" = "VisTm") %>%
  pivot_longer(cols = starts_with("Team"),
               names_to = "TeamLocation",
               names_prefix = "Team",
               values_to = "Team") %>%
  mutate(TeamwOBA = case_when(TeamLocation == "Hm" ~ HmwOBA,
                              TRUE ~ ViswOBA),
         TeamRuns = case_when(TeamLocation == "Hm" ~ HmRuns,
                              TRUE ~ VisRuns)) %>%
  group_by(GameID, TeamLocation, TeamwOBA, TeamRuns) %>%
  summarize(TotalOuts = sum(NumOuts)) %>%
  mutate(
    InningsPlayed = if_else(TotalOuts == 54, 9, floor(TotalOuts / 6))
  ) %>%
  group_by(GameID) %>%
  mutate(
    FractionalOuts = TotalOuts %% 6,
    FractionalInnings = if_else(FractionalOuts > 3 & TeamLocation == "Hm", (FractionalOuts - 3) / 3,
                                if_else(FractionalOuts < 3 & TeamLocation == "Vis", FractionalOuts / 3, 0)),
    TeamInn = if_else(FractionalOuts > 3 & TeamLocation == "Vis", InningsPlayed + 1,
                      if_else(FractionalOuts < 3 & TeamLocation == "Hm", InningsPlayed - FractionalInnings, InningsPlayed + FractionalInnings)),
    TeamR9 = (TeamRuns / TeamInn) * 9
  ) %>%
  ungroup()
# Build linear regression model
model1 = lm(TeamR9 ~ TeamwOBA,data = gameLogs)
#summary(model1)
# Identify and remove influential outliers
cooksD <- cooks.distance(model1)
numObvs <- nrow(gameLogs)
influential <- cooks.distance(model1)[(cooks.distance(model1) > 4 / (numObvs-1-1))]
namesOfInfluential <- names(influential)
outliers <- gameLogs[namesOfInfluential,]
# Build new model and store model results
gameLogs2 <- gameLogs %>% anti_join(outliers)
model2 <- lm(TeamR9 ~ TeamwOBA, data = gameLogs2)
modelCoef <- as.data.frame(coef(summary(model2)))
lmIntercept <- modelCoef$Estimate[1]
lmCoef <- modelCoef$Estimate[2]


### MATCHUPS ###
# Join today's bat and pitch data frames
todayMatchups <- left_join(todayBatters,todayPitchers, by = c('gameID', 'oppID' = 'oppPtchTeamID'))

# Create team batting projection
todayTeamBat <- todayBatters %>%
  left_join(batOrderWgt,by = c('batOrder'='orderNum')) %>%
  mutate(wOBAVal = xwOBA * orderWgt) %>%
  group_by(gameID,gameDate,teamID,teamMascot,oppID,teamLocation) %>%
  summarise(bwOBA = sum(wOBAVal)) %>%
  arrange(desc(bwOBA))

# Create team pitching projection
todayTeamPitch <- todayPitchers %>%
  left_join(teamBullpen,by = c('oppPtchTeamID'='teamID','oppMascot'='teamMascot')) %>%
  mutate(rIP = innings - sIP,
         swOBA = swOBA * (sIP / innings),
         rwOBA = xwOBA * (rIP / innings),
         pwOBA = swOBA + rwOBA) %>%
  select(gameID,oppPtchID,oppPtchName,oppPtchTeamID,oppMascot,pwOBA)

### PREDICTIONS ###
# Run predictions for today's games
todayPredictions <- left_join(todayTeamBat,todayTeamPitch, by = c('gameID', 'oppID' = 'oppPtchTeamID')) %>%
  mutate(season = year(Sys.Date()),
         venueTeamMascot = case_when(
           teamLocation == 'home' ~ teamMascot,
           TRUE ~ oppMascot)) %>%
  left_join(wOBAWgt, by = "season") %>%
  left_join(parkFx, by = c("venueTeamMascot" = "teamMascot")) %>%
  mutate(wOBA = ((bwOBA - lg_woba) + (pwOBA - lg_woba) + lg_woba) * BH,
         runs = lmIntercept + (lmCoef * wOBA)) %>%
  select(gameID, gameDate, teamID, teamMascot, teamLocation, oppID, oppMascot, oppPtchID, oppPtchName, wOBA, runs)
# Sub select to get opponent's runs
todayPredictions <- merge(todayPredictions, todayPredictions[, c("teamID", "runs")], by.x = "oppID", by.y = "teamID", suffixes = c("", ".opp")) %>%
  select(gameID, gameDate, teamID, teamMascot, teamLocation, oppID, oppMascot, oppPtchID, oppPtchName, wOBA, runs, oppRuns = runs.opp)
# Select the desired fields
todayPredictions <- todayPredictions %>%
  select(gameID, gameDate, teamID, teamMascot, teamLocation, oppID, oppMascot, oppPtchID, oppPtchName, wOBA, runs, oppRuns) %>%
  arrange(gameID,teamLocation)
# Define a function to calculate win probability
calculateWinProbability <- function(runs, oppRuns) {
  # Win probability based on the difference between runs and oppRuns
  difference <- runs - oppRuns
  winProbability <- 1 / (1 + exp(-difference))
  return(winProbability)
}
# Apply the function to calculate win probability
todayPredictions <- todayPredictions %>%
  mutate(winProb = calculateWinProbability(runs, oppRuns))


### BETTING LINES ###
# Get MLB betting odds from ESPN
todayOdds <- read_html("https://www.espn.com/mlb/lines") %>%
  html_nodes(".Card") %>%
  html_table(fill = TRUE) %>%
  `[[`(1) %>%
  rename(team = 1, moneyline = 5, total = 6) %>%
  filter(REC != "REC") %>%
  mutate(
    moneyline = as.numeric(gsub("[+]", "", moneyline)),
    impliedProb = if_else(moneyline >= 0, 100 / (moneyline + 100), abs(moneyline) / (abs(moneyline) + 100))
  ) %>%
  left_join(teamDF, by = c('team'='teamName')) %>%
  select(teamID, teamMascot, moneyline, impliedProb)


### PUT IT ALL TOGETHER ###
# Combine win probability and implied odds to determine value
todayPredictions <- left_join(todayPredictions,todayOdds,by = c('teamID','teamMascot'))
# Today's bets
todayBets <- todayPredictions %>%
  mutate(probDiff = winProb - impliedProb) %>%
  select(gameID, teamMascot, teamLocation, oppMascot, oppPtchName, wOBA, runs, oppRuns, moneyline, impliedProb, winProb, probDiff) %>%
  filter(probDiff >= .05) %>%
  arrange(desc(probDiff))








