install.packages("Ecdat")
library(Ecdat)
head(Cigarette)

# Boxplot on average number of packs per capita by state. Which states have the highest number of packs? Lowest?

ggplot(Cigarette, aes(x= state, y= packpc)) + geom_boxplot() + ggtitle("Average Number of Packs of Cigarettes Per Capita Per State") + xlab("State") + ylab("Average Number of Packs of Cigarettes Per Capita")

# Highest= NH, Lowest= UT

library(dplyr)
median <- Cigarette %>% select(year, state, packpc) %>% group_by(year) %>% summarise(medianCIG <- median(packpc))

# Cigarette usage decreased as the years went by

c <- ggplot(Cigarette, aes(x= avgprs, y= packpc, color= year))
c + geom_point() + geom_smooth(method= lm, se= FALSE, color= "goldenrod2") + ggtitle("Average Price Per Pack vs. The Number of Packs Per Capita") + xlab("Average Price Per Pack") + ylab("Number of Packs Per Capita")

mutate(Cigarette, Adj_price = avgprs/cpi) -> Cigarettew_adj
  
c <- ggplot(Cigarettew_adj, aes(x= Adj_price, y= packpc))
c + geom_point() + geom_smooth(method= lm, se= FALSE) + ggtitle("Adjusted Average Price Per Pack vs. The Number of Packs Per Capita") + xlab("Average Price Per Pack Adjusted Based on Inflation") + ylab("Number of Packs Per Capita")

Year_1985 <- Cigarettew_adj %>% filter(year == 1985) %>% select(state, year, packpc)

Year_1995 <- filter(Cigarette, year == 1995) %>% select(state, year, packpc)

CigYear <- data.frame(Year_1985, Year_1995)

t.cig <- t.test(CigYear$packpc, CigYear$packpc.1, paired= TRUE)
t.cig

d <- ggplot(Cigarette, aes(x= income, y= packpc))
d + geom_point() + geom_smooth(method= lm, se= FALSE)

h <- ggplot(Cigarette, aes(x= income))
h + geom_histogram(fill = "goldenrod", color = "deepskyblue4") + ggtitle("Lower Income vs. Higher Income Purchases of Cigarettes") + xlab("Income") + ylab("Count")


r <- Cigarette %>% filter(state %in% c("VA", "GA", "TX", "FL", "CA")) %>% group_by(state) %>% select(year, state, income, packpc)
rh <- ggplot(r, aes(x= year, y= packpc, color= state))
rh + geom_line() + ggtitle("Number of Packs Per Capita in Each State From 1985 To 1995") + xlab("Year") + ylab("Number of Packs Per Capita")


