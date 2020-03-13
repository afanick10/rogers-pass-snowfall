library(readr)
library(dplyr)
library(magrittr)
library(graphics)
library(ggplot2)

rogers_pass_data <- read.csv("climate-daily.csv")
rows <- nrow(rogers_pass_data)
rogers_pass_data[is.na(rogers_pass_data)] <- 0
current_year <- rogers_pass_data[1, 8]
total_snow_by_year <- matrix(0, 50, 1)
i <- 1

for (day in 1:rows) {
    if (rogers_pass_data[day, 8] > current_year) {
        current_year <- current_year + 1
        i <- i + 1
    }
    total_snow_by_year[i, 1] <- total_snow_by_year[i, 1] + rogers_pass_data[day, 21]
}

total_snow_every_5_years <- vector(mode = "list", length = 10)
j <- 1
current_sum <- 0

for (i in 1:50) {
    if ((i != 1 && (i - 1) %% 5 == 0) || i == 50) {
        total_snow_every_5_years[j] <- current_sum
        j <- j + 1
        current_sum <- 0
    }
    current_sum <- current_sum + total_snow_by_year[i]
}

year_groups <- c("1965-1969", "1970-1974", "1975-1979", 
                    "1980-1984", "1985-1989", "1990-1994",
                    "1995-1999", "2000-2004", "2005-2009",
                    "2010-2014")

total_snow_numeric <- unlist(total_snow_every_5_years, use.names = FALSE)

qplot(year_groups, total_snow_numeric)

ggplot(data.frame(year_groups, total_snow_numeric),
       aes(year_groups, total_snow_numeric)) + 
       geom_point(color="blue") + ggtitle("Total Snowfall in Five-Year Intervals") +
       labs(y="Total Snowfall (mm)", x="Five-Year Intervals")
