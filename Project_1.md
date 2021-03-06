Project 1
================
Kolton Wiebusch
9/15/2020

  - [Purpose](#purpose)
  - [Initial Setup](#initial-setup)
      - [Required Packages](#required-packages)
      - [NHL Records API](#nhl-records-api)
      - [NHL Stats API](#nhl-stats-api)
      - [Wrapper Function](#wrapper-function)
  - [Exploratory Data Analysis](#exploratory-data-analysis)
      - [Joining of Datasets](#joining-of-datasets)
      - [New variables](#new-variables)
      - [Contingency tables](#contingency-tables)
      - [Numerical Summaries](#numerical-summaries)
      - [Plots](#plots)
          - [Aggression Plot](#aggression-plot)
          - [Division Goal Differentials
            Plot](#division-goal-differentials-plot)
          - [St. Louis Blues Goalie
            Records](#st.-louis-blues-goalie-records)
          - [St. Louis Blues Skater
            Records](#st.-louis-blues-skater-records)
          - [Regular Season Shutout Percentage by
            Conference](#regular-season-shutout-percentage-by-conference)
          - [Regular Season Clutch Percentages (Shootout Win
            Percentage)](#regular-season-clutch-percentages-shootout-win-percentage)

# Purpose

The purpose of this project is to create a vignette reading and
summarizing data taken from NHL API’s in order to analyze the data.

# Initial Setup

This first section goes through the behind the scenes work of creating
functions to pull in data for later use

## Required Packages

These are the packages used in this project.

``` r
library(tidyverse)
library(knitr)
library(jsonlite)
library(httr)
```

## NHL Records API

This entails the creating of the functions to call on the NHL records
data

``` r
#Creating function to return franchise details
getFranchise <- function(ID = NULL, ...){
  franchise <- GET("https://records.nhl.com/site/api/franchise")
  franchise <- content(franchise, "text")
  franchise <- fromJSON(franchise, simplifyDataFrame = TRUE, flatten = TRUE)
  franchise <- as.data.frame(franchise)
    if(is.numeric(ID)){
      return(filter(franchise, data.mostRecentTeamId == ID))
    } else if(is.character(ID)){
      return(filter(franchise, data.teamCommonName == ID))
    }
  return(franchise)
} 

#Creating function to return franchise stats
getFranchiseTotals <- function(ID = NULL, ...){
  totals <- GET("https://records.nhl.com/site/api/franchise-team-totals")
  totals <- content(totals, "text")
  totals <- fromJSON(totals, simplifyDataFrame = TRUE, flatten = TRUE)
  totals <- as.data.frame(totals)
    if(is.numeric(ID)){
      return(filter(totals, data.franchiseId == ID))
    } else if(is.character(ID)){
      return(filter(totals, data.teamName == ID))
    }
  return(totals)
}
```

``` r
#Creating function to return season records for a franchise
getFranchiseSeasonRecords <- function(ID = NULL, ...){
  if(is.character(ID)){
  recordsURL <- GET("https://records.nhl.com/site/api/franchise-season-records")
  records <- content(recordsURL, "text")
  records <- fromJSON(records, simplifyDataFrame = TRUE, flatten = TRUE)
  records <- as.data.frame(records)
  return(filter(records, data.franchiseName == ID))
  }else if(is.numeric(ID)){
  recordsURL <- GET(paste0("https://records.nhl.com/site/api/franchise-season-records?cayenneExp=franchiseId=", ID))
  records <- content(recordsURL, "text")
  records <- fromJSON(records, simplifyDataFrame = TRUE, flatten = TRUE)
  records <- as.data.frame(records)
  return(records)
  }
}
```

``` r
#Creating function to return goalie records for a franchise
getFranchiseGoalieRecords <- function(ID = NULL, ...){
    if(is.character(ID)){
  goalie <- GET("https://records.nhl.com/site/api/franchise-goalie-records")
  goalie <- content(goalie, "text")
  goalie <- fromJSON(goalie, simplifyDataFrame = TRUE, flatten = TRUE)
  goalie <- as.data.frame(goalie)
  return(filter(goalie, data.franchiseName == ID))
  }else if(is.numeric(ID)){
  goalie <- GET(paste0("https://records.nhl.com/site/api/franchise-goalie-records?cayenneExp=franchiseId=", ID))
  goalie <- content(goalie, "text")
  goalie <- fromJSON(goalie, simplifyDataFrame = TRUE, flatten = TRUE)
  goalie <- as.data.frame(goalie)
  return(goalie)
  }
}
```

``` r
#Creating function to return skater records for a franchise
getFranchiseSkaterRecords <- function(ID = NULL, ...){
  if(is.character(ID)){
  skater <- GET("https://records.nhl.com/site/api/franchise-skater-records")
  skater <- content(skater, "text")
  skater <- fromJSON(skater, simplifyDataFrame = TRUE, flatten = TRUE)
  skater <- as.data.frame(skater)
  return(filter(skater, data.franchiseName == ID))
  }else if(is.numeric(ID)){
  skater <- GET(paste0("https://records.nhl.com/site/api/franchise-skater-records?cayenneExp=franchiseId=", ID))
  skater <- content(skater, "text")
  skater <- fromJSON(skater, simplifyDataFrame = TRUE, flatten = TRUE)
  skater <- as.data.frame(skater)
  return(skater)
  }
}
```

## NHL Stats API

This entails the creating of the function to call on the NHL stats data

``` r
   #Setting up modifiers
  "?expand=team.roster" -> mfirst
  "?expand=person.names" -> msecond
  "?expand=team.schedule.next" -> mthird
  "?expand=team.schedule.previous" -> mfourth
  "?expand=team.stats" -> mfifth
  "?expand=team.roster&season=" -> msixth
  "?teamId=" -> mseventh
  "?stats=statsSingleSeasonPlayoffs" -> meighth
  
  #Creating function to get stats 
getNHLStats <- function(ID = NULL, modifier = NULL, ...){
    stats <- GET("https://statsapi.web.nhl.com/api/v1/teams")
    stats <- content(stats, "text")
    stats <- fromJSON(stats, simplifyDataFrame = TRUE, flatten = TRUE)
    stats <- as.data.frame(stats)
    if(is.numeric(ID) & is.character(modifier)){
      stats <- GET(paste0("https://statsapi.web.nhl.com/api/v1/teams/", ID, "/", modifier))
      stats <- content(stats, "text")
      stats <- fromJSON(stats, simplifyDataFrame = TRUE, flatten = TRUE)
      stats <- as.data.frame(stats)
      return(stats)
    }else if(is.character(ID) & is.character(modifier)){
      stats <- GET(paste0("https://statsapi.web.nhl.com/api/v1/teams/", ID, "/", modifier))
      stats <- content(stats, "text")
      stats <- fromJSON(stats, simplifyDataFrame = TRUE, flatten = TRUE)
      stats <- as.data.frame(stats)
      return(stats)
    }else if(is.numeric(ID)){
      return(filter(stats, teams.franchiseId == ID))
    } else if(is.character(ID)){
      return(filter(stats, teams.name == ID | teams.teamName == ID | teams.abbreviation == ID | teams.shortName == ID))
    }
  return(stats)
}
```

## Wrapper Function

This function acts as a wrapper for all the previously discussed
endpoints for NHL records and stats.

``` r
#Creating wrapper function that can take in any previous functions created along with inputs
getNHL <- function(FUN, ID = NULL, m = NULL, ...){
  FUN <- match.fun(FUN)
  ID <- ID
  modifier <- m
  return(FUN(ID, modifier, ...))
}
```

# Exploratory Data Analysis

The next section’s purpose is to explore and analyze the NHL data with
specifications detailed in the assignment instructions.

## Joining of Datasets

Joining two data sets(according to franchiseAbbrv) for further data
exploration

``` r
#Filtering for active franchises only and renaming to get ready for join
getFranchiseTotals() %>% filter(data.activeFranchise == 1) %>% rename(franchiseAbbrv = data.triCode) -> fTT

#narrowing down the stats data to return only a few variables
getNHLStats() %>% rename(franchiseAbbrv = teams.abbreviation) %>% select(franchiseAbbrv, teams.division.name, teams.conference.name) -> fTT2

#Full join on franchiseAbbrv
full_join(fTT, fTT2) -> joinedStats
```

## New variables

Creating new variables for later analysis

``` r
#Creating new variables for use in data analysis
joinedStats %>% mutate(goalDifferential = data.goalsFor - data.goalsAgainst) -> joinedStats
joinedStats %>% mutate(penaltyMinsPerGame = data.penaltyMinutes/data.gamesPlayed) -> joinedStats
joinedStats %>% mutate(winPercentage = data.wins/data.gamesPlayed) -> joinedStats
joinedStats %>% mutate(shutoutPercentage = data.shutouts/data.wins) -> joinedStats
joinedStats %>% mutate(shootoutWinPercentage = ((data.shootoutWins)/(data.shootoutWins + data.shootoutLosses))) -> joinedStats
```

``` r
#This code chunk separates joinedStats into Regular season and playoff stats for active teams
joinedStats$data.gameTypeId <- as.factor(joinedStats$data.gameTypeId)
levels(joinedStats$data.gameTypeId) <- c("RegularSeason", "Playoffs")
joinedStats %>% filter(data.gameTypeId == "RegularSeason" & teams.division.name != is.na(joinedStats$teams.division.name)) -> RegularSeasonStats
joinedStats %>% filter(data.gameTypeId == "Playoffs" & teams.division.name != is.na(joinedStats$teams.division.name)) -> PlayoffStats
```

## Contingency tables

Here, some contingency tables are returned. For the first one, you can
see a breakdown of how many teams are in each division and conference
overall. For the second and third tables, I wanted to take a closer look
at my *favorite* team, the **St. Louis Blues**. The second table gives a
breakdown of their current roster by position type, and the third table
goes a step further and gives a breakdown of the number of position
names within each position type.

``` r
#Creates a table to show breakdown of NHL conferences and divisions
kable(table(RegularSeasonStats$teams.conference.name, RegularSeasonStats$teams.division.name))
```

|         | Atlantic | Central | Metropolitan | Pacific |
| :------ | -------: | ------: | -----------: | ------: |
| Eastern |        8 |       0 |            8 |       0 |
| Western |        0 |       7 |            0 |       8 |

``` r
#Return the Blues roster and break it down
getNHLStats(19, mfirst) -> g
as.data.frame(g[[31]]) -> BluesRoster
kable(table(BluesRoster$position.type))
```

| Var1       | Freq |
| :--------- | ---: |
| Defenseman |   11 |
| Forward    |   18 |
| Goalie     |    2 |

``` r
kable(table(BluesRoster$position.name, BluesRoster$position.type))
```

|            | Defenseman | Forward | Goalie |
| :--------- | ---------: | ------: | -----: |
| Center     |          0 |       8 |      0 |
| Defenseman |         11 |       0 |      0 |
| Goalie     |          0 |       0 |      2 |
| Left Wing  |          0 |       7 |      0 |
| Right Wing |          0 |       3 |      0 |

## Numerical Summaries

Here are some numerical summaries on quantitative variables that I have
created at different settings of categorical variables in the dataset.

The first summary shows that teams amount more penalty minutes per game
in the playoffs than in the regular season. When broken down further in
the second summary, we can see that the Central division amounts
considerably less penalty minutes per game than the other three
divisions, and that the Pacific division not only averages the most
penalty minutes per game in the playoffs, but also has the greatest jump
in MPG from the regular season to the playoffs.

The third summary shows averages and standard deviations for the shutout
percentage, or the percent of time that the winning team does not allow
the opposing team to score a goal all game. We can see a jump in
percentages from the regular season to the playoffs, possibly because of
more stifling defense. Also, the Eastern conference makes the better
improvement between the two conferences between game types. The last
summary breaks the conferences down into divisions. The Atlantic
division in the Eastern conference seems to be the source of this jump,
as their average shutout percentage in the playoffs is the highest among
all groups, while the Pacific division has the lowest average with the
highest standard deviation.

``` r
#Filtering out teams that are not current at this moment
joinedStats %>% filter(teams.division.name != is.na(joinedStats$teams.division.name)) -> activeTeamStats

#Returning numerical summaries
activeTeamStats %>% group_by(data.gameTypeId) %>% summarise(avgPenaltyMPG = round(mean(penaltyMinsPerGame), 3), sdPenaltyMPG = round(sd(penaltyMinsPerGame), 3)) -> ats1

kable(ats1)
```

| data.gameTypeId | avgPenaltyMPG | sdPenaltyMPG |
| :-------------- | ------------: | -----------: |
| RegularSeason   |        13.909 |        2.500 |
| Playoffs        |        15.133 |        3.637 |

``` r
activeTeamStats %>% group_by(data.gameTypeId, teams.conference.name, teams.division.name) %>% summarise(avgPenaltyMPG = round(mean(penaltyMinsPerGame), 3), sdPenaltyMPG = round(sd(penaltyMinsPerGame), 3)) -> ats2

kable(ats2)
```

| data.gameTypeId | teams.conference.name | teams.division.name | avgPenaltyMPG | sdPenaltyMPG |
| :-------------- | :-------------------- | :------------------ | ------------: | -----------: |
| RegularSeason   | Eastern               | Atlantic            |        13.922 |        0.783 |
| RegularSeason   | Eastern               | Metropolitan        |        14.709 |        2.340 |
| RegularSeason   | Western               | Central             |        12.770 |        1.904 |
| RegularSeason   | Western               | Pacific             |        14.094 |        3.940 |
| Playoffs        | Eastern               | Atlantic            |        15.230 |        1.527 |
| Playoffs        | Eastern               | Metropolitan        |        15.776 |        3.578 |
| Playoffs        | Western               | Central             |        13.326 |        3.538 |
| Playoffs        | Western               | Pacific             |        15.972 |        5.125 |

``` r
activeTeamStats %>% group_by(data.gameTypeId, teams.conference.name) %>% summarise(avgShutoutPctg = round(mean(shutoutPercentage), 3), sdShutoutPctg = round(sd(shutoutPercentage), 3)) -> ats3

kable(ats3)
```

| data.gameTypeId | teams.conference.name | avgShutoutPctg | sdShutoutPctg |
| :-------------- | :-------------------- | -------------: | ------------: |
| RegularSeason   | Eastern               |          0.131 |         0.020 |
| RegularSeason   | Western               |          0.130 |         0.023 |
| Playoffs        | Eastern               |          0.155 |         0.031 |
| Playoffs        | Western               |          0.143 |         0.068 |

``` r
activeTeamStats %>% group_by(data.gameTypeId, teams.conference.name, teams.division.name) %>% summarise(avgShutoutPctg = round(mean(shutoutPercentage), 3), sdShutoutPctg = round(sd(shutoutPercentage), 3)) -> ats4

kable(ats4)
```

| data.gameTypeId | teams.conference.name | teams.division.name | avgShutoutPctg | sdShutoutPctg |
| :-------------- | :-------------------- | :------------------ | -------------: | ------------: |
| RegularSeason   | Eastern               | Atlantic            |          0.139 |         0.017 |
| RegularSeason   | Eastern               | Metropolitan        |          0.122 |         0.021 |
| RegularSeason   | Western               | Central             |          0.139 |         0.017 |
| RegularSeason   | Western               | Pacific             |          0.122 |         0.025 |
| Playoffs        | Eastern               | Atlantic            |          0.162 |         0.025 |
| Playoffs        | Eastern               | Metropolitan        |          0.148 |         0.037 |
| Playoffs        | Western               | Central             |          0.149 |         0.034 |
| Playoffs        | Western               | Pacific             |          0.138 |         0.091 |

## Plots

This section includes various plots giving a visual summary of the data.

### Aggression Plot

These two scatter plots show how aggression(penalty minutes per game)
correlates with winning(win percentage) in both the regular season and
playoffs. There seems to be a slight negative correlation in the regular
season, but flips to a slight positive correlation in the playoffs. In
other words, it appears that there is a small indication that teams who
are more aggressive and rough up their opponents have a better chance of
winning in the playoffs.

``` r
#Creating scatter plots with franchise abbreviations for text points across the plot based on penalty minutes per game by win percantage for regular season and playoffs

aggression1 <- ggplot(RegularSeasonStats, aes(x = penaltyMinsPerGame, y = winPercentage))
aggression1 + geom_text(aes(label = franchiseAbbrv))  + geom_text(x = 17, y = .55, size = 4, label = paste0("Correlation = ", round(cor(RegularSeasonStats$penaltyMinsPerGame, RegularSeasonStats$winPercentage), 2))) + ggtitle("Regular Season Win Percentage vs Penalty Minutes Per Game for All Teams")
```

![](Project_1_files/figure-gfm/aggression-1.png)<!-- -->

``` r
aggression2 <- ggplot(PlayoffStats, aes(x = penaltyMinsPerGame, y = winPercentage))
aggression2 + geom_text(aes(label = franchiseAbbrv))  + geom_text(x = 20, y = .38, size = 4, label = paste0("Correlation = ", round(cor(PlayoffStats$penaltyMinsPerGame, PlayoffStats$winPercentage), 2))) + ggtitle("Playoff Win Percentage vs Penalty Minutes Per Game for All Teams")
```

![](Project_1_files/figure-gfm/aggression-2.png)<!-- -->

### Division Goal Differentials Plot

This boxplot shows summaries of goal differential for each division in
the NHL, playoff and regualar season combined. It appears that the
Pacific division historically has the teams with the average worst goal
differential, and has outliers that are the two lowest goal differential
totals in the entire NHL. Oppositely, the Atlantic division has teams
that appear to be historically dominant, with two teams that have a
historic goal differential of about +2000 and +3500.

``` r
#Creating boxplot to show goal differentials for regular season and playoffs by division
goaldiffs <- ggplot(activeTeamStats, aes(x = teams.division.name, y = goalDifferential))
goaldiffs + geom_boxplot(fill = "orange") + xlab("NHL Divisions") + ggtitle("NHL Division Total Goal Differentials")
```

![](Project_1_files/figure-gfm/goalDifferentials-1.png)<!-- -->

### St. Louis Blues Goalie Records

This histogram portrays the St. Louis Blues goalies’ goals against in
one game records. 11 goalies for the Blues have had 6 goals scored
against them in a game, and that is the most common amount. The data
appears slightly skewed to the right, as the frequency to the right of
this most common number is higher than to the left. 3 unlucky keepers
had 10 goals scored against them in one game.

``` r
#Creating histogram to show goals against records for the Blues goalies
ga <-getFranchiseGoalieRecords(18)
gap <- ggplot(ga, aes(x = data.mostGoalsAgainstOneGame))
gap + geom_histogram(color = "black", fill = "brown", binwidth = 1) + labs(title = "St. Louis Blues' Goalie GA Records", x = "Most Goals Against in One Game")
```

![](Project_1_files/figure-gfm/histogram-1.png)<!-- -->

### St. Louis Blues Skater Records

This bar plot shows the breakdown of records held by skaters
(non-goalies) for the St. Louis Blues (*my favorite team*) by position
(**Center, Defense, Left, and Right**). It also shows how many of the
skaters that hold the record are actively playing now, broken down by
position.

``` r
#Creating bar plot to show breakdown of records held by skaters for the Blues based on active or non active
s <- getFranchiseSkaterRecords("St. Louis Blues") %>% rename("Active_Player" = data.activePlayer) %>% group_by(data.positionCode, Active_Player) %>% summarise(count = n())
skate <- ggplot(s, aes(x = data.positionCode, y = count))
skate + geom_bar(aes(fill = Active_Player), stat = "identity", position = "dodge") + xlab("Position Code") + ggtitle("St. Louis Blues Skater Records by Position (Active vs Non-Active)")
```

![](Project_1_files/figure-gfm/skaters-1.png)<!-- -->

### Regular Season Shutout Percentage by Conference

The two following bar plots portray the shutout percentages (percentage
of total wins that are shutouts) for each team based on conference in
the regular season. For the first bar plot, Eastern Conference, it looks
like the Montreal Canadians, Columbus Blue Jackets, and Boston Bruins
had the only shutout percentages higher than 15%. New York Islanders and
Pittsburgh Penguins were the lowly teams in the East for this category.

For the second bar plot, the Western Conference, the Nashville Predators
and the Chicago Blackhawks led the pack in shutout percentage (both over
15%). The Edmonton Oilers finished last place by quite a decent margin
in this category.

``` r
#Creating bar plots for shutout percentages in the regular season by conference

east <- RegularSeasonStats %>% filter(teams.conference.name == "Eastern") 
d <- ggplot(east, aes(x = franchiseAbbrv, y = shutoutPercentage))
d + geom_col(color = "black", fill = "light blue") + labs(title = "Shutout Percentage per Team (Eastern Conference)", x = "Franchise Abbreviations") + ylim(0, 0.20)
```

![](Project_1_files/figure-gfm/shutouts-1.png)<!-- -->

``` r
west <- RegularSeasonStats %>% filter(teams.conference.name == "Western")
d2 <- ggplot(west, aes(x = franchiseAbbrv, y = shutoutPercentage))
d2 + geom_col(color = "black", fill = "light green") + labs(title = "Shutout Percentage per Team (Western Conference)", x = "Franchise Abbreviations") + ylim(0, 0.20)
```

![](Project_1_files/figure-gfm/shutouts-2.png)<!-- -->

### Regular Season Clutch Percentages (Shootout Win Percentage)

These last four bar plots show the ability of teams to perform well in
the clutch, or how often they win their OT shootouts. It is broken down
by teams in each division for further inspection.

Overall, the Colorado Avalanche of the Central division dominated
shootouts, being the only team in the NHL to win over 60% of the time in
their shootouts. Also, the Central division as a whole seemed to perform
best in shootouts, while the Atlantic division was on the bottom of the
list. The other division “winners” were the Pittsburgh Penguins of the
Metropolitan division, the Tampa Bay Lightning of the Atlantic division,
and the Vegas Golden Knights of the Pacific division.

``` r
#Creating bar plots to show shootout Win percentage of teams in the regular season broken down into divisions

M <- RegularSeasonStats %>% filter(teams.division.name == "Metropolitan")
M2 <- ggplot(M, aes(x = franchiseAbbrv, y = shootoutWinPercentage))
M2 + geom_col(color = "black", fill = "blue") + labs(title = "Shootout Win Percentage per Team (Metropolitan Division)", x = "Franchise Abbreviation") + ylim(0, 0.7)
```

![](Project_1_files/figure-gfm/clutch-1.png)<!-- -->

``` r
A <- RegularSeasonStats %>% filter(teams.division.name == "Atlantic")
A2 <- ggplot(A, aes(x = franchiseAbbrv, y = shootoutWinPercentage))
A2 + geom_col(color = "black", fill = "red") + labs(title = "Shootout Win Percentage per Team (Atlantic Division)", x = "Franchise Abbreviation") + ylim(0, 0.7)
```

![](Project_1_files/figure-gfm/clutch-2.png)<!-- -->

``` r
C <- RegularSeasonStats %>% filter(teams.division.name == "Central")
C2 <- ggplot(C, aes(x = franchiseAbbrv, y = shootoutWinPercentage))
C2 + geom_col(color = "black", fill = "purple") + labs(title = "Shootout Win Percentage per Team (Central Division)", x = "Franchise Abbreviation") + ylim(0, 0.7)
```

![](Project_1_files/figure-gfm/clutch-3.png)<!-- -->

``` r
P <- RegularSeasonStats %>% filter(teams.division.name == "Pacific")
P2 <- ggplot(P, aes(x = franchiseAbbrv, y = shootoutWinPercentage))
P2 + geom_col(color = "black", fill = "orange") + labs(title = "Shootout Win Percentage per Team (Pacific Division)", x = "Franchise Abbreviation") + ylim(0, 0.7)
```

![](Project_1_files/figure-gfm/clutch-4.png)<!-- -->

``` r
#End of Project 1
```
