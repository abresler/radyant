devtools::install_github("assertthat")
devtools::install_github("dplyr")
 
filter(hflights, Month == 1, DayofMonth == 1, Dest == "DFW")
head(select(hflights, Year:DayOfWeek))
summarise(hflights, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
 
 
by_dest <- group_by(hflights, Dest)
filter(by_dest, ArrDelay == max(ArrDelay))
 
 
res <- summarise(group_by(hflights, Dest), arr = mean(ArrDelay, na.rm = TRUE))
 
by_day <- group_by(hflights, Year, Month, DayofMonth)
by_month <- summarise(by_day, delayed = sum(ArrDelay > 0, na.rm = TRUE))
by_month
summarise(summarise(by_month, delayed = sum(delayed)), delayed = sum(delayed))
summarise(by_month, delayed = sum(delayed))
 
by_dest <- group_by(hflights, Dest)
filter(by_dest, ArrDelay == max(ArrDelay))
summarise(group_by(hflights, Dest), arr = mean(ArrDelay, na.rm = TRUE))