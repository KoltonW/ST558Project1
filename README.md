Project 1
================
Kolton Wiebusch
9/15/2020

  - [Required Packages](#required-packages)
  - [NHL Records API](#nhl-records-api)
  - [NHL Stats API](#nhl-stats-api)
  - [Wrapper Function](#wrapper-function)

# Required Packages

These are the packages used in this project.

``` r
library(tidyverse)
library(knitr)
library(jsonlite)
library(httr)
```

# NHL Records API

This entails the creating of the functions to call on the NHL records
data

``` r
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

# NHL Stats API

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

# Wrapper Function

This function acts as a wrapper for all the previously discussed
endpoints for NHL records and stats.

``` r
getNHL <- function(FUN, ID = NULL, m = NULL, ...){
  FUN <- match.fun(FUN)
  ID <- ID
  modifier <- m
  return(FUN(ID, modifier, ...))
}
```
