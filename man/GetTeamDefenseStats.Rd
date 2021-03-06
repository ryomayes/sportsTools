% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetTeamDefenseStats.R
\name{GetTeamDefenseStats}
\alias{GetTeamDefenseStats}
\title{Team Defense Stats.}
\usage{
GetTeamDefenseStats(year = CurrentYear(),
  defense.category = "Less Than 6Ft", season.type = "Regular Season",
  per.mode = "Totals")
}
\arguments{
\item{year}{NBA season (e.g. 2008 for the 2007-08 season)}

\item{defense.category}{'Overall', '3 Pointers', '2 Pointers', 'Less Than 6Ft',
'Less Than 10Ft', 'Greater Than 15Ft'}

\item{season.type}{'Regular Season' or 'Playoffs'}

\item{per.mode}{'Per Game' or 'Totals'}
}
\value{
data frame with wins and losses for that season
}
\description{
Team Defense Stats.
}
\examples{
GetTeamDefenseStats(2017)
}
\keyword{shooting}
\keyword{team}

