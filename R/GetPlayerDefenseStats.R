#' Defensive stats for players
#'
#' @param year e.g. 2015 for 2014-15 season
#' @param stat which stat ('Greater Than 15Ft')
#' @param per.mode 'Per Game' or 'Totals
#' @return data frame of stats
#' @keywords defense player
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetPlayerDefenseStats(stat = 'Greater Than 15Ft')

GetPlayerDefenseStats <- function(year = CurrentYear(),
                                  stat = "Greater Than 15Ft",
                                  per.mode = "Totals",
                                  season.type = "Regular Season") {

  options(stringsAsFactors = FALSE)

  per.mode <- CleanParam(per.mode)

  request = GET(
    "http://stats.nba.com/stats/leaguedashptdefend",
    query = list(
      College = "",
      Conference = "",
      Country = "",
      DateFrom = "",
      DateTo = "",
      DefenseCategory = stat,
      Division = "",
      DraftPick = "",
      DraftYear = "",
      GameSegment = "",
      Height = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PerMode = per.mode,
      Period = 0,
      PlayerExperience = "",
      PlayerPosition = "",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      StarterBench = "",
      TeamID = 0,
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

  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)

  return(stats)
}
