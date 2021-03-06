#' Player Shooting Stats.
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param close.def.dist.range Either '', '6+ Feet - Wide Open', '2-4 Feet - Tight'
#' @param shot.dist.range e.g. '>=10.0'
#' @param season.type 'Regular Season' or 'Playoffs'
#' @return data frame with shooting stats
#' @keywords player shooting
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayerShootingStats(2014)

GetPlayerShootingStats <- function(year = CurrentYear(),
                                   close.def.dist.range = '',
                                   shot.dist.range = '',
                                   general.range = '',
                                   shot.clock.range = '',
                                   dribble.range = '',
                                   touch.time.range = '',
                                   per.mode = "Totals",
                                   season.type = 'Regular Season') {

  options(stringsAsFactors = FALSE)

  request <- GET(
    "http://stats.nba.com/stats/leaguedashplayerptshot",
    query = list(
      CloseDefDistRange = close.def.dist.range,
      College = "",
      Conference = "",
      Country = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      DraftPick = "",
      DraftYear = "",
      DribbleRange = dribble.range,
      GameSegment = "",
      GeneralRange = general.range,
      Height = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = 'N',
      PerMode = 'Totals',
      Period = 0,
      PlayerExperience = "",
      PlayerPosition = "",
      PlusMinus = "N",
      Rank = 'N',
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      ShotClockRange = shot.clock.range,
      ShotDistRange = shot.dist.range,
      TouchTimeRange = touch.time.range,
      StarterBench = "",
      TeamID = 0,
      TouchTimeRange = "",
      VsConference = "",
      VsDivision = "",
      Weight = ""
    ),
    add_headers(
      "user-agent" = 'Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36',
      "Dnt" = '1',
      "Accept-Encoding" = 'gzip, deflate, sdch',
      "Accept-Language" = 'en',
      "origin" = 'http://stats.nba.com'
    )
  )

  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)

  # Clean data frame
  char.cols <- c('PLAYER_ID', 'PLAYER_NAME', 'PLAYER_LAST_TEAM_ID', 'PLAYER_LAST_TEAM_ABBREVIATION')
  char.cols <- which(colnames(stats) %in% char.cols)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)

  return(stats)
}
