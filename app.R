# === Uncomment this if running the script for the first time ===
# install.packages('dash')
# install.packages("devtools")
# install.packages("rlang")
# library(devtools)
# install_github('facultyai/dash-bootstrap-components@r-release')

# === If the instalation of 'rlang' fails, try to run RStudio as an administrator ===
# === If loading devtools package failed, update 'rlang' ===

# Loading libraries
library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dplyr)
library(lubridate)
library(xml2)
library(purrr)
library(plotly)
library(stringr)

# Set working directory
setwd("D:\\Antek\\Programowanie\\R\\PD3")

# Load custom components
source("components/MainPage.R")
source("components/NavBar.R")
source("components/Tags.R")
source("components/Users.R")
source("components/Posts.R")

# Load analytics methods
source("analytics/tags.R")
source("analytics/posts.R")
source("analytics/users.R")

# Load custom stylesheets


# Load available forums
forums_list = list.dirs(path = "data/", full.names = F, recursive = F)
forums_list_dropdown = list()
for (i in 1:length(forums_list)) {
  size = round(sum(file.info(list.files(paste("data/", forums_list[i], sep = ""), all.files = T, recursive = T, full.names = T))$size) / 1024 / 1024, 0)
  print(size)
  forums_list_dropdown[[i]] = list(
    label = paste(toupper(substr(forums_list[i], 1, 1)), substr(forums_list[i], 2, nchar(forums_list[i])), " (", size, " MB)",  sep=""), 
    value = forums_list[i]
  )
}
  

# Build app, load bootstrap theme: LITERA
app = Dash$new(external_stylesheets = dbcThemes$LITERA)

# Update app layout
app$layout(
  htmlDiv(children = list(
    dccLocation(id = "url", refresh = F),
    NavBar(),
    htmlDiv(id = "page-content", style = list(padding = "30px 0px"))
  ))
)

# Set callbacks
# [1] NavBar: page swapper
app$callback(
  output = list(id = "page-content", property = 'children'),
  params = list(input(id = "url", property = 'pathname')),
  function(path) {
    if (path == '/') return(MainPage())
    if (path == '/users') return(Users())
    if (path == '/tags') return(Tags())
    if (path == '/posts') return(Posts())
    return(htmlP("Page not found..."))
  }
)

# [2] Tags:
app$callback(
  params = list(
    input(id = "selected-forum-tags", property = "value"),
    input(id = "year-range-tags", property = "value")
  ),
  output = list(
    output(id = "different-tags", property = "children"),
    output(id = "num-posts-tags",property = "children"),
    output(id = "most-popular-tag", property = "children"),
    output(id = "graph-popular-tags", property = "figure"),
    output(id = "graph-awaiting-time", property = "figure")
  ),
  function(forum, year_range) {
    from = paste(year_range[1], "-01-01", sep = "")
    to = paste(year_range[2], "-12-31", sep = "")
    
    Posts__ = read.csv(paste("data/", forum, "/Posts.csv", sep = ""))
    Tags__ = read.csv(paste("data/", forum, "/Tags.csv", sep = ""))
    
    num_posts = Posts__ %>%
      filter(PostTypeId == 1 & date(CreationDate) > from & date(CreationDate) < to) %>%
      nrow()
    tags_counter = popular_tags(from, to, Tags__, Posts__)
    num_tags = tags_counter %>% filter(Count > 0) %>% nrow()
    most_popular_tag = as.character(tags_counter %>% head(1) %>% select(TagName))
    fig_popular_tags = plot_popular_tags(tags_counter %>% head(10))
    fig_awaiting_time = plot_awaiting_time(awaiting_time(from, to, Posts__))
      
    return(list(
      paste(num_tags, "tags"), 
      paste(num_posts, "posts"), 
      most_popular_tag,
      fig_popular_tags,
      fig_awaiting_time
    ))
  }
)

# [3] Posts:
app$callback(
  params = list(
    input(id = "selected-forum-posts", property = "value"),
    input(id = "year-range-posts", property = "value")
  ),
  output = list(
    output(id = "num-posts", property = "children"),
    output(id = "avg-posts-per-week", property = "children"),
    output(id = "avg-posts-score", property = "children"),
    output(id = "questions-per-hour", property = "figure"),
    output(id = "posts-and-users", property = "figure")
  ),
  function(forum, year_range) {
    from = paste(year_range[1], "-01-01", sep = "")
    to = paste(year_range[2], "-12-31", sep = "")
    
    Posts__ = read.csv(paste("data/", forum, "/Posts.csv", sep = ""))
    Users__ = read.csv(paste("data/", forum, "/Users.csv", sep = ""))
    
    Posts__ = Posts__ %>%
      filter(PostTypeId == 1 & date(CreationDate) >= from & date(CreationDate) <= to) %>%
      select(CreationDate, Score)
    
    postsPerMonth = Posts__ %>%
      select(CreationDate) %>%
      mutate(month = ((year(CreationDate) - min(year(CreationDate))) * 12 + month(CreationDate) - min(month(CreationDate)))) %>%
      group_by(month) %>%
      summarise(postCount = n(), .groups = "drop") %>%
      head(-1)
    
    newUsersPerMonth = Users__ %>%
      filter(date(CreationDate) >= from & date(CreationDate) <= to) %>%
      select(CreationDate) %>%
      mutate(month = ((year(CreationDate) - min(year(CreationDate))) * 12 + month(CreationDate) - min(month(CreationDate)))) %>%
      group_by(month) %>%
      summarise(registrations = n(), .groups = "drop") %>%
      head(-1)
    
    postsPerHour = Posts__ %>%
      mutate(postHour = hour(gsub("T", " ", CreationDate))) %>%
      select(postHour) %>%
      group_by(postHour) %>%
      summarise(postCount = n(), .groups = "drop") 
    
    num_posts = nrow(Posts__)
    
    avg_score = (Posts__ %>% summarise(avg = mean(Score)))$avg
    avg_score = round(avg_score, 2)
    
    avg_per_week = (Posts__ %>% 
      mutate(week = ((year(CreationDate) - min(year(CreationDate))) * 52 + week(CreationDate) - min(week(CreationDate)))) %>%
      group_by(week) %>%
      summarise(postByWeek = n(), .groups = "drop") %>%
      summarise(avg = mean(postByWeek)))$avg
    avg_per_week = round(avg_per_week, 0)
    
    fig_posts_per_hour = plot_posts_per_hour(postsPerHour)
    fig_posts_and_users = plot_users_and_posts(postsPerMonth, newUsersPerMonth)
    
    return(list(
      paste(num_posts, "posts"), 
      paste("~", avg_per_week, "posts"), 
      avg_score, 
      fig_posts_per_hour, 
      fig_posts_and_users
    ))
  }
)

# [4] Users:
app$callback(
  params = list(
    input(id = "selected-forum-users", property = "value"),
    input(id = "year-range-users", property = "value")
  ),
  output = list(
    output(id = "num-users", property = "children"),
    output(id = "active-users", property = "children"),
    output(id = "best-user", property = "children")
  ),
  function(forum, year_range) {
    from = paste(year_range[1], "-01-01", sep = "")
    to = paste(year_range[2], "-12-31", sep = "")
    
    Users__ = read.csv(paste("data/", forum, "/Users.csv", sep = ""))
    DarkSide__ = read.csv("analytics/darkside.csv")
    DarkUsers__ = read.csv("analytics/darkusers.csv")
    
    num_users = nrow(Users__)
    active_users = Users__ %>%
      filter(as.numeric(difftime("2020-12-31", LastAccessDate, units = c("weeks"))) <= 8) %>%
      nrow()
    
    best_user = Users__ %>%
      arrange(desc(Reputation)) %>%
      select(DisplayName, Reputation) %>%
      head(1)
    
    return(list(paste(num_users, "users"), paste(active_users, "users"), paste(best_user$Reputation, "rep.")))
  }
)


# Run server, default: 127.0.0.1:8050
app$run_server(showcase = TRUE)

