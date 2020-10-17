##investment

data_csv <- read.csv("C:/Users/Huhu/Downloads/data_csv.csv", stringsAsFactors = FALSE)
head(data_csv)
# Doing stuff with dates:
# Reformatting the dates to make it readable by the system.

data_csv$ID <- seq.int(nrow(data_csv))
data_csv$Date[1513]
# S&P 500 was started in 1923; prior history is from Shiller. If you
# only want the "real" sp500 values, uncomment the line below:

#subset the  data from 2007
sp500<-subset(data_csv,data_csv$ID >= 1513)

#sp500$Date<-as.Date(sp500$Date,"%Y%m%d")

#Calculate real returns (Reinvested dividends)
sp500$real.return <- 1 # Start with initial conditions. I invest one dollar at the beginning of the stock market.
for(r in 2:nrow(sp500)){
  sp500$real.return[r]<-
    # Start with previous value:
    sp500$real.return[r-1]*
    # Multiply it by the % change in stock value in the last month:
    (((sp500$Real.Price[r]-sp500$Real.Price[r-1])/
        (sp500$Real.Price[r-1]))+1)+
    # Finally, add last month's dividends to the party; they get reinvested:
    (sp500$Real.Dividend[r-1]/sp500$Real.Price[r-1])*
    (sp500$real.return[r-1]/12)
}
sp500$real.return
summary(sp500$real.return)

sp500$rr.log = log(sp500$real.return)

x = sp500[ -c(1, 11, 12) ]

cor(x)
cormat<-signif(cor(x),2)
cormat
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cormat, col=col, symm=TRUE)




# Master Loop
# If you're not regenerating the source data, uncomment this part
# Warning: May take a very long time to solve.
###############
stocks<-data.frame(NA,NA,NA,NA)
names(stocks)<-c("year","real","percent","inv.date")
for(f in 0:nrow(sp500)){
  sp500$future.f<-NA    #Future S&P Price
  sp500$cpi.f <- NA     #Future CPI
  sp500$future.r <- NA  #Future Real Returns
  buffer<-data.frame(NA,NA,NA,NA)
  names(buffer)<-c("year","real","percent","inv.date")
  for(n in (f+1):nrow(sp500)){
    # Get values for "f" years in the future
    sp500$future.f[n-f] <- sp500$SP500[n]                      # Work our Future S&P Price into its own column
    sp500$cpi.f[n-f] <- sp500$Consumer.Price.Index[n]          # Work the Future CPI into its own column
    sp500$future.r[n-f] <- sp500$real.return[n]                # Work the Real Returns into its own column
    buffer<-rbind(buffer,c(f/12,sp500$future.r[n-f],                   # Record all history
                           (sp500$future.r[n-f]-sp500$real.return[n-f]) /
                             sp500$real.return[n-f],
                           as.character(sp500$Date[n-f])
    ))
  }
  stocks<-rbind(stocks,buffer)
  print(paste(f, " of ", nrow(sp500), " completed: ", signif(f*100/nrow(sp500),4),"%",sep=""))}
stocks<-subset(stocks,!is.na(stocks$percent))
rm(buffer)
# Use a cash multiplier instead of a percent:
stocks$multip<-as.numeric(stocks$percent)+1
stocks$year<-as.numeric(stocks$year)
stocks$real<-as.numeric(stocks$real)
stocks$percent<-as.numeric(stocks$percent)
#write.table(stocks,"returns.csv",sep=",")
head(stocks)


