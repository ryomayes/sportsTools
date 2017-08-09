#' Player Shooting Stats by Location
#'
#' @importFrom httr GET content add_headers
#' @export

GetTeamShootingStats <- function(year = CurrentYear(),
                                 close.def.dist = '',
                                 distance.range,
                                 season.type = 'Regular Season',
                                 per.mode = 'Totals') {
  
  options(stringsAsFactors = FALSE)
  
  per.mode <- CleanParam(per.mode)
  
  request <- GET(
    "http://stats.nba.com/stats/leaguedashplayershotlocations",
    query = list(
      Conference = "",
      DateFrom = "",
      DateTo = "",
      DistanceRange = distance.range,
      Division = "",
      GameScope = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      MeasureType = 'Base',
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = 'N',
      PerMode = per.mode,
      Period = 0,
      PlayerExperience = "",
      PlayerPosition = "",
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      ShotClockRange = "",
      StarterBench = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers(
      "user-agent" = 'Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36',
      "Dnt" = '1',
      "Accept-Encoding" = 'gzip, deflate, sdch',
      "Accept-Language" = 'en',
      "origin" = 'http://stats.nba.com'
    )
  )
  
  content <- content(request, 'parsed')$resultSets
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
}
