
if (FALSE) {
	fileurl = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
	download.file(url = fileurl, destfile = "StormData.csv.bz2", method = "curl")
}

if (!exists("data.raw")) {
	data.raw <- read.csv("StormData.csv.bz2")
}

## 1993 12607
## 1994 20631
## 1995 27970

### select events from 1996 to present
if (FALSE) {
	# cols = c("BGN_DATE", "EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP",
	# "FATALITIES", "INJURIES")
# data.a <- subset(data.raw, select = cols)
data.a <- data.raw
	data.a$date <- strptime(data.a$BGN_DATE, "%m/%d/%Y")
	data.a <- subset(data.a, data.a$date >= strptime("1/1/1995", "%m/%d/%Y"))
	}

if (FALSE) {
	### add columns of property damage
	mult <- function(x, exp) {
		if (exp == "K") 
			return(x * 1000)
		if (exp == "M") 
			return(x * 1e+06)
		if (exp == "B") 
			return(x * 1e+09)
		return(x)
	}
	data.a$pdmg <- mapply(mult, data.a$PROPDMG, data.a$PROPDMGEXP)
	data.a$cdmg <- mapply(mult, data.a$CROPDMG, data.a$CROPDMGEXP)
	data.a$tdmg <- data.a$pdmg + data.a$cdmg
	data.a$casualties <- data.a$FATALITIES + data.a$INJURIES
	data.a <- subset(data.a, tdmg > 0 | casualties > 0)

	### correct data for Napa Flood, rows 567251, 605953
	data.a["567251", "PROPDMG"] <- 0
	data.a["567251", "CROPDMG"] <- 0
	data.a["567251", "PROPDMGEXP"] <- "0"
	data.a["567251", "CROPDMGEXP"] <- "0"
	data.a["605953", "PROPDMGEXP"] <- "M"

}

if (FALSE) {
	### make simple regex substitutions for EVTYPE
	data.a$EVTYPE <- toupper(data.a$EVTYPE)
	data.a$EVTYPE <- gsub("WINDS", "WIND", data.a$EVTYPE)
	data.a$EVTYPE <- gsub("RAINS", "RAIN", data.a$EVTYPE)
	data.a$EVTYPE <- gsub("HAIL [0-9].*", "HAIL", data.a$EVTYPE)
	data.a$EVTYPE <- gsub("TSTM", "THUNDERSTORM", data.a$EVTYPE)
	data.a$EVTYPE <- gsub("THUNDERSTORMS", "THUNDERSTORM", data.a$EVTYPE)
	data.a$EVTYPE <- gsub("FLOODING", "FLOOD", data.a$EVTYPE)
	data.a$EVTYPE <- gsub("FLOODS", "FLOOD", data.a$EVTYPE)
	data.a$EVTYPE <- gsub("TROPICAL STORM.*", "TROPICAL STORM", data.a$EVTYPE)
	data.a$EVTYPE <- gsub("HURRICANE.*", "HURRICANE", data.a$EVTYPE)
	data.a$EVTYPE <- gsub(".*WATERSPOUT.*", "WATERSPOUT", data.a$EVTYPE)
	data.a$EVTYPE <- gsub("TORNADO.*", "TORNADO", data.a$EVTYPE)
	data.a$EVTYPE <- gsub(".*FLASH FLOOD.*", "FLASH FLOOD", data.a$EVTYPE)
	data.a$EVTYPE <- gsub("THUNDERSTORM WIND .*", "THUNDERSTORM WIND", data.a$EVTYPE)
}

if (FALSE) {
	### make EVTYPE translations with translation file
	# create a new column for cleaned evtype
	data.a$event <- ""

	## capitalize evtype in data table
	data.a$EVTYPE <- toupper(data.a$EVTYPE)

	## read in list of event type lists
	events_comma <- read.table("storm_events_list.txt", sep = ";")
	events <- events_comma[, 1]
	events <- toupper(events)

	library("stringr")
	events <- sapply(events, function(x) {
		v <- strsplit(x, split = ",")[[1]]
		v <- str_trim(v)
		paste(v, collapse = ",")
	})
	events <- unname(events)
}

translate <- function(evtype) {
	for (i in seq_along(events)) {
		v <- strsplit(events[i], split = ",")[[1]]
		if (evtype %in% v) {
			return(v[1])
		}
	}
	return(evtype)
}

if (FALSE) {
	data.a$event <- sapply(data.a$EVTYPE, translate)
}



## process damage data for plotting
data.dmg <- subset(data.a, data.a$tdmg > 0)
dmg <- aggregate(tdmg ~ event, data = data.dmg, sum)
dmg$event <- factor(dmg$event)
dmg$tdmg <- round(dmg$tdmg / 1000000000, 2)
dmg <- dmg[order(dmg$tdmg, decreasing = TRUE), ]
dmg.top <- dmg[1:10, ]
dmg.top$event <- reorder(dmg.top$event, -dmg.top$tdmg)

## plot damage
library(ggplot2)
g <- ggplot(dmg.top, aes(dmg.top$event, dmg.top$tdmg))
g <- g + geom_bar(aes(fill=event), stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + xlab("Event type") + ylab("Total damage in billions of dollars")
g <- g + ggtitle("10 Most costly event types, 1995-2011") + theme(legend.position = "none")
g <- g + scale_y_continuous(breaks = seq(0,160,by = 10))

#print(g)


## process casualty data for plotting
data.cas <- subset(data.a, data.a$casualties > 0)
cas <- aggregate(casualties ~ event, data = data.cas, sum)
cas$event <- factor(cas$event)
cas$casualties <- round(cas$casualties / 1000, 2)
cas <- cas[order(cas$casualties, decreasing=TRUE), ]
cas.top <- cas[1:10, ]
cas.top$event <- reorder(cas.top$event, -cas.top$casualties)

## plot casualties
g <- ggplot(cas.top, aes(cas.top$event, cas.top$casualties))
g <- g + geom_bar(aes(fill=event), stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + xlab("Event type") + ylab("Total casualties in thousands") + theme(legend.position = "none")
g <- g + ggtitle("10 Most dangerous event types, 1995-2011")
g <- g + scale_y_continuous(breaks = seq(0,25,by = 5))

print(g)




