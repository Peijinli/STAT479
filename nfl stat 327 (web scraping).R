rm(list=ls())

if (!require("XML")) { # for readHTMLTable()
  install.packages("XML")
  stopifnot(require("XML"))
}

TEAMS = "http://www.nfl.com/stats/categorystats?tabSeq=2&statisticCategory=GAME_STATS&role=TM&seasonType=REG"
lines = readLines(TEAMS) # thousands of lines of HTML
team.lines = grep(pattern="profile", x=lines, value=TRUE) # 32 team lines,
# each of the form:
# "\t\t\t\t\t\t\t\t\t\t\t<a href=\"/teams/greenbaypackers/profile?team=GB\">Green Bay Packers</a>"        
team.names = sub(pattern=".*teams/(.*)/profile.*", replacement="\\1", x=team.lines)
team.abbreviations = sub(pattern=".*team=(.*)\".*", replacement="\\1", x=team.lines)
n.teams = length(team.names)
rushing = numeric(n.teams)
receiving = numeric(n.teams)
for (i in 1:n.teams) {
  # assemble a link like
  # http://www.nfl.com/teams/greenbaypackers/statistics?team=GB
  link = paste0("http://www.nfl.com/teams/", team.names[i], "/statistics?team=", team.abbreviations[i])
  print(link)
  tables = readHTMLTable(link)
  rushing[i] = as.numeric(as.character(tables[[3]][2, 3]))
  receiving[i] = as.numeric(as.character(tables[[4]][2, 3]))
}

plot(x=rushing, y=receiving, xlim=c(0, max(rushing)), ylim=c(0, max(receiving)))
m = lm(receiving ~ rushing)
abline(m)
print(summary(m))
