library(dplyr)


plot_posts_per_hour <- function(postsPerHour) {
  fig__ <- plot_ly(x = ~postsPerHour$postHour, y = ~postsPerHour$postCount,  name = "SF Zoo",  type = "bar") %>%
    layout(title = "Questions asked vs. hours",
           xaxis = list(title = "Hour", tickmode = "linear", tick0 = 0.0, dtick = 1),
           yaxis = list(title = "Number of posts"))
  return(fig__)
}


plot_users_and_posts <- function(posts, users) {
  fig__ <- plot_ly(x = posts$month, y = posts$postCount, name = "Posts", type = "scatter", mode = "lines", line = list(shape = "spline", smoothing = 1.2)) %>%
    add_trace(x = users$month, y = users$registrations, name = "Users", mode = "lines", line = list(shape = "spline", smoothing = 1.2)) %>%
    layout(title = "Relation between new questions and new users",
           xaxis = list(title = "Months from the beginning date"),
           yaxis = list(title = "Number of new questions/ users"))
  return(fig__)
}

