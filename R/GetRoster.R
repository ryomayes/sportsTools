#' Team roster
#'
#' @param year NBA season (e.g. 2008 for the 2007-08 season)
#' @param team Either team name ('Golden State') or team id
#' @param team.ids Teams and their IDs
#' @return data frame with player information
#' @keywords roster
#' @importFrom httr GET content add_headers
#' @export
#' @examples
#' GetRoster(2014, 'Golden State')

GetRoster <- function(year = CurrentYear(), team, team.ids = NA) {

  options(stringsAsFactors = FALSE)

  team <- TeamNameToID(team, year, team.ids)

  request <- GET(
    "http://stats.nba.com/stats/commonteamroster",
    query = list(
      LeagueID = "00",
      Season = YearToSeason(year),
      TeamID = team
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
  roster <- ContentToDF(content)

  num.cols <- c('SEASON', 'NUM', 'WEIGHT', 'AGE', 'EXP')
  num.cols <- which(colnames(roster) %in% num.cols)
  roster[, num.cols] <- sapply(roster[, num.cols], as.numeric)
  roster[is.na(roster$EXP), 'EXP'] <- 0

  return(roster)
}
