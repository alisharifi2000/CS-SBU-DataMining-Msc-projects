library(ggplot2)
library(dplyr)
library(png)
library(ggpubr)
library(tidyr)
library(lattice)

setwd("~/Documents/Education/Master/Data\ mining/exercises/1/a/")
data <- read.csv("AB_NYC_2019.csv")
nrow(data)
length(unique(data$host_id))
host_id_freq <- as.data.frame(table(data$host_id))
summary(host_id_freq)
hist(host_id_freq$Freq, ylim = c(0,60))
t.test(host_id_freq$Freq, mu=2, alternative="greater")
filtered_data <- data[!duplicated(data[ , c("host_id")]),]
by_neighbourhood_group_and_room_type <- as.data.frame(table(filtered_data[c("neighbourhood_group", "room_type")]))
ggplot(by_neighbourhood_group_and_room_type, aes(fill = room_type,x = neighbourhood_group,y = Freq)) +
  geom_bar(position="dodge", stat="identity") + theme_minimal() + 
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5, position = position_dodge(0.9))
price_by_neighbourhood_group_and_room_type <- as.data.frame(aggregate(price ~ neighbourhood_group * room_type, data, mean))
ggplot(price_by_neighbourhood_group_and_room_type, aes(fill = room_type,x = neighbourhood_group,y = price)) +
  geom_bar(position="dodge", stat="identity") + theme_minimal() + 
  geom_text(aes(label=floor(price)), vjust=-0.3, size=3.5, position = position_dodge(0.9))
ggplot(data, aes(x = longitude, y = latitude)) + 
  background_image(img) +
  geom_point(aes(col=room_type, size = price), alpha = 0.5) +
  scale_size(range = c(0.5, 12))
pairs(price ~ number_of_reviews + latitude + longitude + availability_365, data)
anova.model <- aov(price ~ number_of_reviews + latitude + longitude + availability_365, data)
summary(anova.model)
reg.model <- lm(price ~ number_of_reviews + latitude + longitude + availability_365, data)
reg.model
summary(reg.model)
host_id_freq <- host_id_freq[order(-host_id_freq$Freq),]
most_frequent_host <- subset(data, host_id == 219517861)
length(unique(most_frequent_host$name))
unique_host_id <- unique(data$host_id)
temp_list = c()
for (index in unique_host_id) {
  temp <- subset(data, host_id == index)
  temp_list <- c(temp_list, length(unique(temp$name)))
}
print(temp_list)
plot(temp_list)
mean(temp_list)
others <- subset(data, host_id != 219517861)
boxplot(most_frequent_host$price, others$price, ylim=c(0,400), names = c("soner cases price", "other cases price"))
t.test(most_frequent_host$price, others$price,  alternative = "greater", var.equal = FALSE)
ggplot(data, aes(x = longitude, y = latitude)) + 
  background_image(img) +
  geom_point(aes(col=neighbourhood_group))
by_neighbourhood_group <- as.data.frame(table(filtered_data[c("neighbourhood_group")]))
ggplot(by_neighbourhood_group, aes(x = Var1,y = Freq,fill=Var1)) +
  geom_bar(position="dodge", stat="identity") + theme_minimal() + 
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5, position = position_dodge(0.9))
unique(data$neighbourhood_group)
Brooklyn <- subset(data, neighbourhood_group == "Brooklyn")
Manhattan <- subset(data, neighbourhood_group == "Manhattan")
Queens <- subset(data, neighbourhood_group == "Queens")
Staten_Island <- subset(data, neighbourhood_group == "Staten Island")
Bronx <- subset(data, neighbourhood_group == "Bronx")
boxplot(Brooklyn$price,Manhattan$price,Queens$price,Staten_Island$price,Bronx$price, ylim = c(0,400), names = c("Brooklyn","Manhattan","Queens","Staten_Island","Bronx"))
res <- bartlett.test(price ~ neighbourhood_group, data = data)
res
summary(res)
####
df <- read.csv("results.csv")
nrow(df)
df <- cbind(df, year=format(as.Date(df$date), "%Y"))
winners <- data.frame()
for (i in 1:nrow(df)) {
  match <- df[i,]
  print(c(match$away_score,  match$home_score))
  if (match$away_score >  match$home_score) {
    winners <- rbind(winners, c(team=match$away_team, year=match$year))
  } 
  if (match$away_score <  match$home_score ) {
    winners <- rbind(winners, c(team=match$home_team, year=match$year))
  }
}
colnames(winners) <- c("team", "year")
winners$year <- as.numeric(winners$year)
eras = c()
for (i in 1:nrow(winners)) {
  winner <- winners[i,]
  value <- floor(winner$year / 10) * 10
  eras <- c(eras, value)
}
winners <- cbind(winners, eras)
dominators = c()
for (i in seq(1870, 2020, by=10)) {
  temp <- subset(winners, winners$eras == i)
  target <- as.data.frame(tail(sort(table(temp$team)), 1))
  dominators <- rbind(dominators, target)
}
dominators <- cbind(dominators, seq(1870, 2020, by=10))
df$home_score <- as.numeric(df$home_score)
df$away_score <- as.numeric(df$away_score)
df$year <- as.numeric(df$year)
model <- away_score ~ home_score
anova.model <- aov(model, df)
summary(anova.model)
reg.model <- lm(model, df)
reg.model
summary(reg.model)
plot(model, df)
abline(reg.model, col="RED")
match_table <- as.data.frame(table(df$home_score, df$away_score))
match_table <- match_table[match_table$Freq != 0,]
ggplot(match_table, aes(x=Var1, y=Var2, size=Freq)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(1, 10))
levelplot(Freq ~ Var1*Var2, data=match_table, xlab="X", main="", col.regions = gray(100:0 /100))
matches <- as.data.frame(table(df$home_team, df$away_team))
matches <- matches[order(-matches$Freq),]
matches <- head(matches, 40)
temp <- data_frame()
for (i in 1:nrow(matches)) {
  match <- matches[i,]
  for (j in 1:nrow(matches)) {
    match2 <- matches[j,]
    if (match$Var1 == match2$Var2 && match$Var2 == match2$Var1) {
      temp <- rbind(temp, data_frame(match$Var1, match$Var2, match$Freq + match2$Freq))
      print(data_frame(match$Var1, match$Var2, match$Freq + match2$Freq))
    }
  }
}
colnames(temp) <- c("team_1", "team_2", "total_play")
temp <- temp[!duplicated(temp[,c('total_play')]),]
ggplot(temp, aes(x=team_1, y=total_play, fill=team_2)) +
  theme_minimal() + 
  geom_bar(position="dodge", stat="identity") +
  xlab("")
hosts <- c()
for (i in 1:nrow(df)) {
  match <- df[i,]
  if (match$home_team != match$country && match$away_team != match$country) {
    hosts <- c(hosts, match$country)
  }
}
hosts_and_freq <- as.data.frame(table(hosts))
hosts_and_freq <- hosts_and_freq[order(-hosts_and_freq$Freq),]
ggplot(head(hosts_and_freq), aes(x=hosts, y=Freq, fill=hosts)) +
  theme_minimal() + 
  geom_bar(position="dodge", stat="identity") +
  xlab("")
unique(df$tournament)
wordcup_matches <- df[df$tournament == "FIFA World Cup",]
winners <- data.frame()
for (i in 1:nrow(wordcup_matches)) {
  match <- wordcup_matches[i,]
  if (match$away_team == match$country || match$home_team == match$country) {
    if (match$away_score >  match$home_score) {
      winners <- rbind(winners, c(team=match$home_team, team=match$away_team, year=match$country))
    } 
    if (match$away_score <  match$home_score ) {
      winners <- rbind(winners, c(team=match$away_team, team=match$home_team, year=match$country))
    } 
  }
}
colnames(winners) <- c("loser" ,"winner", "host")
hosts <- unique(winners$host)
probs <- data.frame()
for (host in hosts) {
  matches <- winners[winners$host == host,]
  tot <- nrow(matches)
  probs <- rbind(probs, c(host, length(which(matches$winner==host)), tot, length(which(matches$winner==host))/tot))
  print(c(host, length(which(matches$winner==host)), tot))
  #print(matches)
}
colnames(probs) <- c("host" ,"count", "total", "prob")
probs$prob <- as.numeric(probs$prob)
View(probs)
summary(probs$prob)
t.test(probs$prob, mu=.5, alternative="greater")
friendly_matches <- df[df$tournament == "Friendly",]
View(table(friendly_matches$home_team))
View(table(friendly_matches$away_team))
