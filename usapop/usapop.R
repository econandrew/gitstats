############################################################################ ###
# An R program to generate a single consistent historical population series
# for the USA, based on multiple US Census Bureau files.
#
# NOTE: If running from RStudio, note that the output is written to the
# current working director. Use setwd() to change this.
################################################################################

require(lubridate)
require(reshape2)
require(httr)

# Thanks http://stackoverflow.com/a/3611619/4023092
setAs("character", "num.with.commas", 
      function(from) as.numeric(gsub(",", "", from) ) )

############################################################################ ###
# Part 1/4: 1901-1999 Population
################################################################################

popclock <- suppressWarnings(read.fwf( # suppress NAs introduced
  "http://www.census.gov/popest/data/national/totals/pre-1980/tables/popclockest.txt",
  skip = 10,
  strip.white = TRUE,
  blank.lines.skip = TRUE,
  stringsAsFactors = FALSE,
  col.names = c("date", "pop", "pop.change", "pop.pc.change"),
  colClasses = c("character", "num.with.commas", "num.with.commas", NA),
  widths = c(16,19,20,12)
))

popclock <- popclock[complete.cases(popclock),]
popclock$date <- as.Date(popclock$date, format="%B %d, %Y")

popclock <- popclock[,c("date", "pop")]

############################################################################ ###
# Part 2/4: 1990-2000 Population, revised post 2000 census
################################################################################

pop1990s <- read.csv(
  "http://www.census.gov/popest/data/intercensal/national/files/US-EST90INT-07.csv",
  skip = 2,
  strip.white = TRUE,
  stringsAsFactors = FALSE,
  col.names = c("date", "age", "pop", "popmale", "popfemale")
)

pop1990s$date <- as.Date(pop1990s$date, format="%B %d, %Y")

pop1990s <- pop1990s[month(pop1990s$date) == 7 & pop1990s$age == 'All Age',c("date", "pop")]

############################################################################ ###
# Part 3/4: 2000-2010 Population, revised post 2010 census
################################################################################

pop2000s <- read.csv(
  "http://www.census.gov/popest/data/intercensal/national/files/US-EST00INT-TOT.csv",
  strip.white = TRUE,
  stringsAsFactors = FALSE,
  col.names = c("year", "month", "pop")
)

pop2000s$date <- as.Date(paste(pop2000s$year, pop2000s$month ,"1",sep="-"))

pop2000s <- pop2000s[month(pop2000s$date) == 7,c("date", "pop")]

############################################################################ ###
# Part 4/4: 2010s Population, estimate post 2010 census
################################################################################

this.year <- year(Sys.time())
for (vintage in this.year:2011) {
  vintage.url <- paste0(
    "http://www.census.gov/popest/data/state/totals/",
    vintage,
    "/tables/NST-EST",
    vintage,
    "-01.csv"
  )
  
  print(paste("Trying vintage",vintage))
  head <- HEAD(vintage.url)
  if (head$status_code == 200) {
    print("Found!")
    pop2010s <- read.csv(
      vintage.url,
      strip.white = TRUE,
      stringsAsFactors = FALSE,
      skip = 3
    )
    break
  } else {
    # nothing
  }
}

pop2010s = pop2010s[pop2010s[[1]]=="United States",-1:-3]
pop2010s = data.frame(
  date = colnames(pop2010s),
  pop = sapply(pop2010s[1,], function(x) as(x, "num.with.commas"))
)
pop2010s$date <- as.Date(paste(sub("X","",pop2010s$date),"7","1", sep="-"))

############################################################################ ###
# Merge files
################################################################################

pop <- rbind(
  popclock[year(popclock$date) <= 1989,],
  pop1990s[year(pop1990s$date) <= 1999,],
  pop2000s[year(pop2000s$date) <= 2009,],
  pop2010s
)
pop <- pop[order(pop$date),]
row.names(pop) <- NULL

write.csv(pop, "usapop.csv")

############################################################################ ###
# Optional: Check against OECD
################################################################################

# Thanks http://www.r-bloggers.com/reading-oecd-stat-into-r/

library(XML2R)

file <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/POP_FIVE_HIST/USA.YP99TLL1_ST.TT.A/all?startTime=1950&endTime=2014"

obs <- XML2Obs(file)
tables <- collapse_obs(obs)

# The data we care about is stored in the following three nodes
# We only care about the country variable in the keys node
keys <- tables[["MessageGroup//DataSet//Series//SeriesKey//Value"]]
dates <- tables[["MessageGroup//DataSet//Series//Obs//Time"]]
values <- tables[["MessageGroup//DataSet//Series//Obs//ObsValue"]]

# Extract the country part of the keys table
# Have to use both COU and COUNTRY as OECD don't use a standard name
country_list <- keys[keys[,1]== "COU" | keys[,1]== "COUNTRY"]
# The country names are stored in the middle third of the above list
country_list <- country_list[(length(country_list)*1/3+1):(length(country_list)*2/3)]

# Bind the existing date and value vectors
dat <- cbind.data.frame(as.numeric(dates[,1]),as.numeric(values[,1]))
colnames(dat) <- c('date', 'value')

# Add the country variable
# This code maps a new country each time the diff(dat$date)<=0 ...
# ...as there are a different number of readings for each country
# This is not particularly robust
dat$country <- c(country_list[1], country_list[cumsum(diff(dat$date) <= 0) + 1])
#created this as too many sig figs make the rChart ugly
dat$value2 <- signif(dat$value,2)

head(dat)

oecdpop <- data.frame(
  date = as.Date(paste(dat$date, "7","1",sep="-")),
  pop = dat$value
)

oecdpop <- merge(oecdpop, pop, all = F, by = "date", suffixes = c(".oecd",".uscb"))
oecdpop$diff <- oecdpop$pop.uscb - oecdpop$pop.oecd

############################################################################ ###
# Optional: Plot series comparison
################################################################################

xrange <- c(min(popclock$date), max(pop2010s$date))
yrange <- c(min(popclock$pop), max(pop2010s$pop))

plot(pop ~ date, data = popclock, pch=3, col="black", xlim=xrange, ylim=yrange)
points(pop1990s$date, pop1990s$pop, pch=3, col="red")
points(pop2000s$date, pop2000s$pop, pch=3, col="blue")
points(pop2010s$date, pop2010s$pop, pch=3, col="green")
points(oecdpop$date, oecdpop$pop, pch=20, col="yellow")

