library("ggplot2")
library("reshape")

## I don't like relying on RStudio API to find "here"
##  https://stackoverflow.com/a/16046056
fDir <- dirname(sys.frame(1)$ofile)

msg  <- function(bits) {
  message(paste0(bits))
}

## Read historical data
## Source: https://www.wsj.com/market-data/quotes/index/SPX/historical-prices
histPath <- file.path(fDir, "SP500 Data 1978-01-03 to 2025-11-05.csv")
hDF      <- read.csv(histPath, header=TRUE, stringsAsFactors=FALSE)
## Formalize the Date column and order from oldest-to-new
hDF$Date <- as.Date(hDF$Date, "%m/%d/%y")
hDF      <- hDF[ order(hDF$Date, decreasing=FALSE), ]
rownames(hDF) <- as.character(hDF$Date)

## Some basic stats on our data
nr       <- nrow(hDF)
minD     <- min(hDF$Date)  # Earliest date
minCls   <- min(hDF$Close) # Lowest closing price
maxD     <- max(hDF$Date)  # Latest date
maxCls   <- max(hDF$Close) # Highest closing price
numY     <- signif(as.numeric(maxD - minD) / 365, 3)
msg(c("[>] Read ", nr, " data points over ", numY, " years"))

## Reccesion data from Wikipedia
recPath <- file.path(fDir, "Recessions.csv")
recs   <- read.csv(recPath, header=TRUE, stringsAsFactors=FALSE,
                   comment.char='#')
recs$Start <- as.Date(recs$Start, "%Y-%m-%d")
recs$End   <- as.Date(recs$End, "%Y-%m-%d")
## Calculate Recession middle (to help center labels)
recs$Center <- recs$Start + (recs$End-recs$Start)/2
## Remove entries older than primary data
recs <- recs[ recs$Start >= minD, ]

## Worst Case Scenario
## If I invested in the S&P 500 on any given date in the past, what is the
## absolute LONGEST time I might have to wait for the market to return
## to the price I purchased at? We will use closing price as our value metric
hDF$WorstCase <- as.numeric(NA)
## The date each worst case wait ends:
hDF$WorstEnd  <- as.Date(NA)

## Best Case Scenario
## Alternatively, what is the soonest I can sell a position to break even?
## To avoid being a pointless metric, this needs a minimum hold length
hDF$BestCase  <- as.numeric(NA)
bcHold        <- 30 # No day traders here - presume hold for at least a month

for (r in seq_len(nr)) {
  ## Ok, we are going to buy S&P 500 index on this day (row r)
  buy   <- hDF$Close[r] # What we pay for it
  today <- hDF$Date[r]  # The date we buy it
  ## Which days are <worse> than this one?
  bad <- which(hDF$Close < buy)
  ## Discard days in the past
  bad <- bad[ bad > r ]
  ## Record our finding
  wc <- if (length(bad) == 0) {
    ## Huh! Buying on this date was always a win! ... so far ...
    hDF[ r, "WorstEnd"] <- today
    0
  } else {
    ## Take the furthest most date, calculate days to it
    wr <- bad[length(bad)]
    hDF[ r, "WorstEnd"] <- hDF[ wr, "Date"]
    as.integer(hDF$Date[wr]- today)
  }
  ## Record as number of years
  hDF[ r, "WorstCase"] <- signif(wc / 365, 3)

  ## For Best Case, we will presume that we've held onto the position for at
  ## least bcHold days, and then find the first day that is >= our purchase price
  bad <- which(hDF$Close > buy & (hDF$Date > today + bcHold ))
  bc <- if (length(bad) == 0) {
    ## Still a (so far) guaranteed win
    0
  } else {
    ## Take the earliest day to break even, return number of days to it
    br <- bad[1]
    hDF[ r, "BestEnd"] <- hDF[ br, "Date"]
    signif((bad[1] - r) / 365, 3)
    as.integer(hDF$Date[br]- today)
  }
  ## Record as number of years:
  hDF[ r, "BestCase"] <- signif(bc / 365, 3)
}

## What are the longest worst-case periods?
## We want to filter out subsets that MOSTLY are contained by their bigger siblings
## Making it as a function to make it easier for me to noodle
getAbsWorst <- function(df) {
  rv <- data.frame(Start=as.Date(NULL), End=as.Date(NULL),
                   Close=numeric(), Years=numeric(), Label=character())
  while (nrow(df) > 0) {
    ## Pop off the worst remaining case
    d1 <- df[1, 1] # Date it started
    d2 <- df[1, 2] # Date it ended
    l  <- df[1, 3] # Years it lasted
    n  <- nrow(rv) + 1 # row number for next entry
    ## Capture our new worst-worst case in the return dataframe
    rv[n, "Start"] <- d1
    rv[n, "End"]   <- d2
    rv[n, "Years"] <- l
    rv[n, "Close"] <- df[1, "Close"]
    rv[n, "Label"] <- sprintf("#%d %.1f Years", n, l)
    
    ## We will consider anything "within 10%" of the date range to be part
    ## of the same fiasco. This is to prevent lots of little related spans
    fuzz <- floor(l * 365 * 0.1)
    
    ## Now remove anything that "overlaps" the fiasco
    olp <- df[[1]] >= d1 - fuzz & df[[2]] <= d2 + fuzz
    df  <- df[ !olp, ]
  }
  rv
}
## Start with worst cases
wcTmp  <-hDF[ , c("Date", "WorstEnd", "WorstCase", "Close")]
## Exclude anything less than a month
wcTmp  <- wcTmp[ wcTmp$WorstCase > 1/12, ]
## Worst-worst-cases at top
wcTmp  <- wcTmp[ order(wcTmp$WorstCase, decreasing=TRUE), ]

absoluteWorst <- getAbsWorst(wcTmp)
## Keep only the top finds to avoid clutter
## Keep only spans over a year
absoluteWorst <- absoluteWorst[ absoluteWorst$Years >= 1, ]
## Was still too cluttered, keep just the top 10
absoluteWorst <- absoluteWorst[ seq_len(10), ]

## Now do the worst-best cases
bcTmp  <-hDF[ , c("Date", "BestEnd", "BestCase", "Close")]
## Exclude anything less than a month
bcTmp  <- bcTmp[ bcTmp$BestCase > 1/12, ]
## Worst-best-cases at top
bcTmp  <- bcTmp[ order(bcTmp$BestCase, decreasing=TRUE), ]

bestWorst <- getAbsWorst(bcTmp)
## Take just the top 10 for later display
bestWorst <- bestWorst[ bestWorst$Years >= 1,  ]


## Reminding me how to set manual color scales
##   https://stackoverflow.com/a/49719424
## Date scales in ggplot
##   https://stackoverflow.com/a/34594044

## Make data long to appease ggplot
bcwc <- reshape::melt(hDF[ , c("Date", "BestCase", "WorstCase")], 
                      id.vars="Date", variable_name = "Metric")
## Make the axis ticks somewhat nicer
minYr <- as.Date("1970-01-01")
maxYr <- as.Date("2030-01-01")
maxHd <- 2*ceiling(max(bcwc$value)/2)

bcColor  <- "green"   # Best Case
wcColor  <- "orange"  # Worst Case
recColor <- "pink"    # Recessions

wcsPlot <- ggplot(bcwc, aes(Date, value)) +
  geom_point(aes(color=Metric), size=0.3) +
  labs(title="Best and Worst Case Scenarios",
       subtitle="How many years do I hold onto an S&P500 purchase before break-even?") +
  ylab("Years Held Before Break-Even") +
  xlab("Date of Purchase") + 
  scale_color_manual(values = c("BestCase" = bcColor, "WorstCase" = wcColor),
                     aesthetics=c("color")) +
  scale_y_continuous(breaks=seq(0, maxHd, 2),
                     minor_breaks=seq(0, maxHd, 1)) +
  scale_x_date(date_breaks="5 years", date_minor_breaks="1 year",
               date_labels="%Y") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = 'white', colour = 'gray'),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_line(color = "gray"),
        legend.position = "inside", legend.title=element_blank(),
        legend.position.inside = c(.1,.85))

bcwcPath <- file.path(fDir, "BestCase-WorstCase.png")
ggsave(bcwcPath, wcsPlot, width=2000,height=1200,units="px")


### Primary S&P 500 plot ###

sp500Path <- file.path(fDir, "SP500 History.png")
## Logarithmic ticks
logPri <- 2^seq(from=5, to=12)
## I want 4 minor ticks between each major one
## Make all vs all multiplication 
##  https://forum.posit.co/t/how-do-i-use-to-multiply-two-vectors-in-order-to-produce-a-matrix/107313
logSec <- logPri %*% t((1:4)/4)
logSec <- order(as.vector(logSec))
## Set up basic plot
sp500plot <- ggplot(hDF, aes(Date, Close))
## Add recessions
## Weird need to avoid data inheritance with inherit.aes
##  https://stackoverflow.com/a/68515865
sp500plot <- sp500plot +
  geom_rect(data=recs, inherit.aes=FALSE, fill=recColor, alpha=0.7,
            mapping=aes(xmin=Start, xmax=End), ymin=-Inf, ymax=Inf) +
  geom_text(data=recs, aes(label=Label, x=Center, y=6500),
            color="red", angle=90, size=2, hjust="inward")

## Primary index plot
sp500plot <- sp500plot +
  geom_point(size=0.05) +
  labs(title="S&P500 Historical Data - Closing Value",
       subtitle="Horizontal bars: What's the longest time I could go with zero gain?") +
  ylab("log(Value)") +
  xlab("Date") + 
  scale_y_continuous(trans="log2", breaks=logPri, minor_breaks=logSec) +
  scale_x_date(date_breaks="5 years", date_minor_breaks="1 year",
               date_labels="%Y") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = 'white', colour = 'gray'),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_line(color = "gray"))

# ggsave(sp500Path, sp500plot, width=2000,height=1200,units="px")

## Add in the worst-case scenarios as bars:
sp500plot <- sp500plot +
  geom_segment(data=absoluteWorst, aes(x=Start, xend=End, y=Close, yend=Close),
               color=wcColor) +
  geom_text(data=absoluteWorst, aes(label=Label, x=End, y=Close),
            color=wcColor, hjust=-0.1, size=2)

## Add in best-case as well:
sp500plot <- sp500plot +
  geom_segment(data=bestWorst, aes(x=Start, xend=End, y=Close, yend=Close),
               color=bcColor) +
  geom_text(data=bestWorst, aes(label=Label, x=Start, y=Close),
            color=bcColor, hjust="outward", vjust=-0.3, size=2)

ggsave(sp500Path, sp500plot, width=2000,height=1200,units="px")


## Permutation analysis of rebalancing strategy
## Goal - is there a rebalancing frequency that is more reliable / profitable?
## In particular: HOW OFTEN SHOULD REBALANCING BE PERFORMED?

## Permutation characteristics:
## How often do we rebalance? (days)
reFreq <- c(30, 90, 365)
## What time frame do we consider? (number of years)
reWindow <- c(5, 10, 15)
## What fraction of the investment is going to be SP500?
## The remainder will be a "safe" investment. For simplicity, we
## will treat it as cash, but in reality it will be a conservative bond/bill fund
reFrac   <- c(0.50, 0.25, 0.75)
## What metric are we using to measure the index?
reMetric <- "Close"
## How often do we provide an update of our progress? (seconds)
noteFreq <- 15
## This number is largely irrelevant, but it's what we will use as the initial
## investment
firstInvest <- 1000

getAPY <- function(x1, x2, yrs) {
  ## Calculate APY
  ## x1 = initial investment
  ## x2 = final investment value
  ## yrs = years investment was held
  signif(((x2/x1) ** (1/yrs)) - 1, 4)
}

balDat <- data.frame(Window=numeric(), Frequency=numeric(),
                     Start=as.Date(NULL), End=as.Date(NULL), Times=integer(), 
                     APY=numeric(), APYSP=numeric(), Market=numeric())
for (rw in reWindow) {
  ## Try out all investment windows - this is the number of years we'll track performance
  ## What's the latest starting date we can consider in our historic data and
  ## still fit inside this window?
  maxEnd <- maxD - (rw*365)
  winRng <- which(hDF$Date < maxEnd)
  msg(c("[Window] ",rw, " Years"))
  for (rf in reFreq) {
    ## Try out different time lapses between rebalances
    msg(c("  [Frequency] ",rf, " Days"))
    for (frac in reFrac) {
      ## Try out different balances (fractions) between SP500 and safe
      safe <- 1 - frac # The fraction we will keep invested in the safe account
      msg(c("    [S&P Fraction] ", frac))
      lastNote <- Sys.time()
      for (i in winRng) {
        ## And now brute force all available windows in the historic data
        ## Start by "seeding" our investment (x)
        ## x[1] = SP500, x[2] = safe investment
        x <- c(firstInvest * frac, firstInvest * safe)
        ## We will track the previous value and the previous date
        priVal  <- hDF[i, reMetric]
        priDate <- hDF[i, "Date"]
        nTimes  <- 0L
        ## Our run will finish by this date:
        winEnd <- priDate + (rw*365)
        we <- which(hDF$Date > winEnd)
        winIdx <- seq(from=i+1, to=we[1], by=1)
        ## Our summarized entry:
        n <- nrow(balDat) + 1
        balDat[n, "Window"]    <- rw
        balDat[n, "Frequency"] <- rf
        balDat[n, "Start"]     <- priDate
        for (j in winIdx) {
          today <- hDF[j, "Date"]
          ## Check if enough time has elapsed to rebalance
          if (today < priDate + rf) next
          ## Ok, it's time to rebalance!
          ## How much has our SP500 index grown? Or not?
          nowVal <- hDF[j, reMetric]     # the value of the index today
          nv     <- nowVal * x[1]/priVal # So value of our SP500 investment
          tot    <- nv + x[2]            # and our total value
          ## Rebalance!
          x      <- c(tot * frac, tot * safe)
          ## Update our tracking
          priVal  <- nowVal
          priDate <- today
          nTimes  <- nTimes + 1L
        }
        ## Note the actual last date that we rebalanced
        balDat[n, "End"]       <- priDate
        yrs  <- as.numeric((priDate - balDat[n, "Start"])/365)
        ## Calculate our TOTAL APY
        balDat[n, "APY"] <- getAPY(firstInvest, x[1]+x[2], yrs)
        ## Calculate our APY, only considering the unsafe initial investment
        balDat[n, "APYSP"] <- getAPY(firstInvest * frac, 
                                   x[1]+x[2]-(firstInvest * safe), yrs)
        ## Note how the market performed over same period:
        balDat[n, "Market"] <- getAPY(hDF[i, reMetric], priVal, yrs)
        balDat[n, "Times"] <- nTimes
        if (Sys.time() > lastNote + noteFreq) {
          lastNote <- Sys.time()
          msg(c("      [",as.character(balDat[n, "Start"]),"]"))
        }
      }
    }
  }
}


