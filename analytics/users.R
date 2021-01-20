library(dplyr)

darkside_of_forum <- function(users, posts, forum) {
  badPosts = posts %>%
    filter(Score < 0)
  
  n_badPosts = nrow(badPosts)
  n_totalPosts = nrow(posts)
  
  badUsers = users %>%
    filter(Id != -1) %>%
    mutate(VoteDiff = DownVotes - UpVotes) %>%
    filter(VoteDiff > 0) %>%
    select(AccountId, VoteDiff)
  
  n_badUsers = nrow(badUsers)
  n_totalUsers = nrow(users)
  
  return(list(Forum = forum, BadUsers = n_badUsers, TotalUsers = n_totalUsers, BadPosts = n_badPosts, TotalPosts = n_totalPosts))
}

darkusers <- function(users, forum_name) {
  users %>%
    filter(Id != -1) %>%
    mutate(VoteDiff = DownVotes - UpVotes, Forum = forum_name) %>%
    filter(VoteDiff > 0) %>%
    select(Forum, DisplayName, VoteDiff, Reputation, AccountId, ProfileImageUrl) %>% 
    arrange(desc(VoteDiff)) %>%
    head(10)
}

darkside <- function() {
  df = data.frame(Forum = character(), BadUsers = numeric(), TotalUsers = numeric(), BadPosts = numeric(), TotalPosts = numeric())
  i = 1
  for (name in forums_list) {
    print(name)
    Posts__ = read.csv(paste("data/", name, "/Posts.csv", sep = ""))
    Users__ = read.csv(paste("data/", name, "/Users.csv", sep = ""))
    the_truth = darkside_of_forum(Users__, Posts__, name)
    df = rbind(df, the_truth)
    i = i + 1
  }
  write.csv(df, "analytics/darkside.csv", row.names = F)
}

all_darkusers <- function() {
  df = data.frame(Forum = character(), DisplayName = character(), VoteDiff = numeric(), Reputation = numeric(), AccountId = numeric(), ProfilePic = character())
  i = 1
  for (name in forums_list) {
    print(name)
    Users__ = read.csv(paste("data/", name, "/Users.csv", sep = ""))
    the_truth = darkusers(Users__, name)
    df = rbind(df, the_truth)
    i = i + 1
  }
  
  darkins = df %>%
    group_by(AccountId) %>%
    summarise(NumOfTerroredForums = n(), .groups = "drop") %>%
    arrange(desc(NumOfTerroredForums)) %>%
    head(5) %>%
    inner_join(df, by="AccountId") %>%
    arrange(desc(NumOfTerroredForums), AccountId)
  
  write.csv(df, "analytics/darkusers.csv", row.names = F)
  write.csv(darkins, "analytics/darkins.csv", row.names = F)
}

plot_darkness <- function() {
  darkside <- read.csv("analytics/darkside.csv")
  darkside$postRatio = darkside$BadPosts / darkside$TotalPosts * 100
  darkside$userRatio = darkside$BadUsers / darkside$TotalUsers * 1000
  fig <- plot_ly(x = darkside$Forum,
                 y = darkside$postRatio,
                 name = "Posts",
                 type = "bar") %>%
    add_trace(x = darkside$Forum,
              y = darkside$userRatio,
              name = "Users",
              type = "bar")
  return(fig)
}

