## tidy_stormdata.R
## the code below reclassifies the stormdata read from
## https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2
##
## Source to load data in CourseProjectTwo.Rmd
# library(dplyr)
# url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
# zipFile <- "repdata_data_StormData.bzip2"
# dataFile <- "stormdata.csv"
# download.file(url,destfile=zipFile,method="curl")
# stormdata <- read.csv(zipFile)
##
## The National Climatic Data Center Storm Events lists 48 different event types but
## exploring the data fromm the csv file has 985 different event types.  This includes
## typos, abbreviations and alternate names.  So the first step is to group the data by
## the 48 different event types.
## 
## using the Storm Data Documentation to make valid reclassifications
## https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf

evtypes <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")

impactVals <- c("FATALITIES","INJURIES","PROPDMG","CROPDMG")
impactFields <- c(impactVals,"PROPDMGEXP","CROPDMGEXP")

#We are interested in stormdata that has an impact on public health and the economy
stormdata$EVENTHARM <- with(stormdata,(FATALITIES+INJURIES+PROPDMG+CROPDMG)>0)
#other <- stormdata[!stormdata$EVENTHARM,]
#stormdata <- stormdata[stormdata$EVENTHARM,]

stormdata$EVTYPENEW <- toupper(stormdata$EVTYPE)
# Remove leading spaces
stormdata$EVTYPENEW <- gsub("^[ ]+|[ ]+$","",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("[ ]{2,}"," ",stormdata$EVTYPENEW)

#Remove a couple of mislabeled types and summary fields
stormdata[grep("^\\?|^APACHE|SUMMARY|^LACK|OTHER|MONTH|TURBULENCE|RECORD TEMP|NONE|RAPIDLY|DROWNING|MILD PATTERN|NO SEVERE WEATHER|NORTHERN LIGHTS|SOUTHEAST",toupper(stormdata$EVTYPE)),]$EVTYPENEW <- NA

# Debris Flow is not used a term in the database but was listed are an event type
# the following slides are debris flow events
stormdata$EVTYPENEW <- gsub("(LAND|ROCK|MUD|LAND |ROCK |MUD |MUD/ROCK )SLIDE(|S)|LANDSLUMP","DEBRIS FLOW",stormdata$EVTYPENEW)

# Remove plurals, abbrev, different terminal or spaces so the types match
stormdata$EVTYPENEW <- gsub("TSTM|THUDERSTORM|THUNDEERSTORM|THUNDERESTORM|THUNDERSNOW|THUNDERSTRON|THUNDERTORM|THUNERSTORM|THUNDESTORM|TUNDERSTORM|THUNDERTSORM|THUNDERSTROM|SEVERE THUNDERSTORM","THUNDERSTORM",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("FLOODING|FLOODIN|FLD|FLDG|FLOOODING|HIGH WATER|DAM BREAK|DAM FAILURE","FLOOD",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("WINDS|WINS|WND","WIND",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("STORMS","STORM", stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("CURRENTS","CURRENT",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub(" FIRES|FIRES| FIRE","FIRE",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("COASTAL/TIDAL|CSTL|BEACH","COASTAL",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("COASTALF","COASTAL F",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("HVY","HEAVY",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("COASTAL SURGE","COASTAL FLOOD",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("TIDES|SURGE/TIDE|SURGE","TIDE",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("^FREEZE$","FROST/FREEZE",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("AVALANCE","AVALANCHE",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("^PATCHY DENSE FOG|^FOG","DENSE FOG",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("BITTER","EXTREME",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("DEVEL","DEVIL",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("WHIRLWIND|LANDSPOUT","DUST DEVIL",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("BLOWING DUST|DUSTSTORM|SAHARAN DUST","DUST STORM",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("EXCESSIVE$","EXCESSIVE HEAT",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("EXCESSIVE PRECIPITATION|EXCESSIVE RAINFALL|EXCESSIVE RAIN|EXCESSIVE WETNESS|WET","HEAVY RAIN",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("WALL CLOUD","FUNNEL CLOUD",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("[\\]","/",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("HURRICANE-GENERATED SWELLS","HIGH SURF",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("BEACH EROSIN|BEACH EROSION|COASTAL EROSIN|COASTAL EROSION","HIGH SURF",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("BEACH","COASTAL",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("TORNDAO","TORNADO",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("VOG|VOLCANIC ERUPTION","VOLCANIC ASH",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("HEAVY MIX","HEAVY SNOW",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("WATER SPOUT|WAYTERSPOUT","WATERSPOUT",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("BLOW-OUT TIDE|ASTRONOMICAL HIGH TIDE|HIGH TIDE","STORM TIDE",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("(HAZARDOUS|HEAVY|ROUGH) SURF|(HIGH|HEAVY|ROUGH) SEAS|(HIGH|HEAVY) SWELLS|HIGH WAVES|ROGUE WAVE","HIGH SURF",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("WIND AND WAVE","SEICHE",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("MARINE (MISHAP|ACCIDENT)","MARINE STRONG WIND",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("LIGNTNING|LIGHTING","LIGHTNING",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("LOW TEMPERATURE|COOL|RECORD LOW|LOW TEMP$","COLD/WIND CHILL",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("HIGH TEMPERATURE|RECORD HIGH|^HIGH$","EXCESSIVE HEAT",stormdata$EVTYPENEW)
#stormdata$EVTYPENEW <- gsub("BELOW NORMAL PRECIPITATION","DROUGHT",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("PRECIPITATION|PRECIPATATION|PRECIP|SHOWER","RAIN",stormdata$EVTYPENEW)
stormdata$EVTYPENEW <- gsub("ABNORMAL |ABNORMALLY |RECORD |UNSEASONABLE |UNSEASONABLY |UNSEASONAL |VERY |EXCESSIVELY |LIGHT |GUSTY | GUSTS|GROUND |LARGE |ROTATING |SEVERE ","",stormdata$EVTYPENEW)


# Assign the appropriate classification for some of the more common mismatches
stormdata[grep("SMOKE",stormdata$EVTYPENEW),]$EVTYPENEW <- "DENSE SMOKE"
stormdata[grepl("DRY",stormdata$EVTYPENEW)&!grepl("MICRO",stormdata$EVTYPENEW),]$EVTYPENEW <- "DROUGHT"
stormdata[grep("^EXTREME COLD$|EXTREME WIND",stormdata$EVTYPENEW),]$EVTYPENEW <- "EXTREME COLD/WIND CHILL"
stormdata[grep("ICE FOG",stormdata$EVTYPENEW),]$EVTYPENEW <- "FREEZING FOG"
stormdata[grep("FUNNEL",stormdata$EVTYPENEW),]$EVTYPENEW <- "FUNNEL CLOUD"
stormdata[grepl("HAIL|ICE PELLETS",stormdata$EVTYPENEW)&!grepl("MARINE HAIL",stormdata$EVTYPENEW),]$EVTYPENEW <- "HAIL"
#need to do this replace after ICE FOG(FREEZING FOG) and ICE PELLETS(HAIL)
#also do not include the ICE STORM which is a valid event type
stormdata[grepl("FROST|FREEZE|ICE|ICY|GLAZE|FREEZING SPRAY",stormdata$EVTYPENEW)&!grepl("ICE STORM",stormdata$EVTYPENEW),]$EVTYPENEW <- "FROST/FREEZE"
stormdata[grep("^EXTREME HEAT$|WARM|HOT",stormdata$EVTYPENEW),]$EVTYPENEW <- "HEAT"
stormdata[grep("^HURRICANE|TYPHOON|REMNANTS",stormdata$EVTYPENEW),]$EVTYPENEW <- "HURRICANE/TYPHOON"
stormdata[grep("^URBAN|^SMALL STREAM|^RIVER|^ICE JAM",stormdata$EVTYPENEW),]$EVTYPENEW <- "FLOOD"
stormdata[grep("^RAIN",stormdata$EVTYPENEW),]$EVTYPENEW <- "HEAVY RAIN"
stormdata[grep("^SNOW",stormdata$EVTYPENEW),]$EVTYPENEW <- "HEAVY SNOW"
stormdata[grep("METRO STORM",stormdata$EVTYPENEW),]$EVTYPENEW <- "HIGH WIND"
stormdata[grep("COASTAL STORM|COASTALSTORM",stormdata$EVTYPENEW),]$EVTYPENEW <- "MARINE STRONG WIND"
#stormdata[grep("^WIND$",stormdata$EVTYPENEW),]$EVTYPENEW <- "STRONG WIND"
stormdata[grep("^THUNDERSTORM|MICROBURST|GUSTNADO|MIRCOBURST|DOWNBURST",stormdata$EVTYPENEW),]$EVTYPENEW <- "THUNDERSTORM WIND"
# Wildfire is the only type of fire in the 48 events
stormdata[grep("WILD|FOREST|BRUSH|GRASS|RED FLAG",stormdata$EVTYPENEW),]$EVTYPENEW <- "WILDFIRE"
stormdata[grep("^FREEZING RAIN|^FREEZING DRIZZLE|WINTRY MIX|WINTERY MIX|WINTER MIX",stormdata$EVTYPENEW),]$EVTYPENEW <- "WINTER WEATHER"
# Hypothermia, one is misspelt Hyper
cold <- grep("COLD|HYP",stormdata$EVTYPENEW)
extremecold <- grepl("EXTREME",stormdata[cold, ]$EVTYPENEW)
stormdata[cold,]$EVTYPENEW <- ifelse(extremecold,"EXTREME COLD/WIND CHILL","COLD/WIND CHILL")
snow <- grep("SNOW",stormdata$EVTYPENEW)
lakeeffectsnow <- grepl("LAKE",stormdata[snow, ]$EVTYPENEW)
stormdata[snow,]$EVTYPENEW <- ifelse(lakeeffectsnow,"LAKE-EFFECT SNOW","HEAVY SNOW")
flood <- grep("FLOOD",stormdata$EVTYPENEW)
lakeshoreflood <- grepl("LAKE",stormdata[flood, ]$EVTYPENEW)
flashflood <- grepl("FLASH",stormdata[flood, ]$EVTYPENEW)
coastalflood <- grepl("COASTAL|TIDAL",stormdata[flood, ]$EVTYPENEW)
stormdata[flood,]$EVTYPENEW <- "FLOOD"
stormdata[flood,][lakeshoreflood,]$EVTYPENEW <- "LAKESHORE FLOOD"
stormdata[flood,][flashflood,]$EVTYPENEW <- "FLASH FLOOD"
stormdata[flood,][coastalflood,]$EVTYPENEW <- "COASTAL FLOOD"

# Some events are classified into multiple events i.e. TORNADO/THUNDERSTORM WIND/HEAVY RAIN
# the first type to match is the appropriate event according to the documentation.
# Split by "/" or " AND " or "-" but remove the events that have a "/" or "-" in the event name.
multicategory <- grepl("/| AND|-| - ",stormdata$EVTYPENEW)&!grepl("COLD/WIND|HURRICANE/TYPHOON|FROST/FREEZE|LAKE-EFFECT",stormdata$EVTYPENEW)
multicat_list <- strsplit(stormdata[multicategory,]$EVTYPENEW,"/| AND | AND|-| - ")
source("define_multi_events.R")
multicat_str <- define_multi_events(multicat_list,evtypes)
stormdata[multicategory,]$EVTYPENEW <- multicat_str

#Anything that starts with a direct type match
for(i in 1:length(evtypes))
{
  sw <- startsWith(as.character(stormdata$EVTYPENEW),toupper(evtypes[i]))
  sw[is.na(sw)] <- FALSE
  if(sum(sw) > 0)
    stormdata[sw,]$EVTYPENEW <- toupper(evtypes[i])
}

wind <- grep("WIND",stormdata$EVTYPENEW)
coldwindchill <- grepl("^COLD|^LOW|^WIND CHILL",stormdata[wind, ]$EVTYPENEW)
extremecoldwindchill <- grepl("EXTREME",stormdata[wind, ]$EVTYPENEW)
highwind <- grepl("^HIGH",stormdata[wind, ]$EVTYPENEW)
marinehighwind <- grepl("^MARINE",stormdata[wind,]$EVTYPENEW)&grepl("HIGH",stormdata[wind, ]$EVTYPENEW)
marinestrongwind <- grepl("^MARINE|^LAKE",stormdata[wind,]$EVTYPENEW)&grepl("STRONG",stormdata[wind, ]$EVTYPENEW)
marinetstmwind <- grepl("^MARINE",stormdata[wind,]$EVTYPENEW)&grepl("THUNDERSTORM",stormdata[wind, ]$EVTYPENEW)
#strongwind <- grepl("^STRONG",stormdata[wind, ]$EVTYPENEW)
tstmwind <- grepl("^THUNDERSTORM|^SEVERE",stormdata[wind, ]$EVTYPENEW)
stormdata[wind,]$EVTYPENEW <- "STRONG WIND"
stormdata[wind,][coldwindchill,]$EVTYPENEW <- "COLD/WIND CHILL"
stormdata[wind,][extremecoldwindchill,]$EVTYPENEW <- "EXTREME COLD/WIND CHILL"
stormdata[wind,][highwind,]$EVTYPENEW <- "HIGH WIND"
stormdata[wind,][marinehighwind,]$EVTYPENEW <- "MARINE HIGH WIND"
stormdata[wind,][marinestrongwind,]$EVTYPENEW <- "MARINE STRONG WIND"
stormdata[wind,][marinetstmwind,]$EVTYPENEW <- "MARINE THUNDERSTORM WIND"
stormdata[wind,][tstmwind,]$EVTYPENEW <- "THUNDERSTORM WIND"

temperature <- grep("TEMPERATURE",stormdata$EVTYPENEW)
lowtemp <- grepl("LOW",toupper(stormdata[temperature,]$REMARKS))
stormdata[temperature,][lowtemp,]$EVTYPENEW <- "EXTREME COLD/WIND CHILL"
stormdata[temperature,][!lowtemp,]$EVTYPENEW <- "HEAT"

rain <- grep("RAIN",stormdata$EVTYPENEW)
lowrain <- grepl("LOW",stormdata[rain,]$EVTYPENEW)
stormdata[rain,][lowrain,]$EVTYPENEW <- "DROUGHT"
stormdata[rain,][!lowrain,]$EVTYPENEW <- "HEAVY RAIN"
