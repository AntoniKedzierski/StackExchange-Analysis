library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(dashHtmlComponents)

source("analytics/users.R")

Users <- function() {
  darkins = read.csv("analytics/darkins.csv")
  avg_diff = darkins %>% group_by(AccountId) %>% summarise(avg = mean(VoteDiff), .groups = "drop")
  
  dbcContainer(
    children = list(
      dbcRow(children = list(
        dbcCol(width = 4, children = list(
          dbcCard(color = "primary", outline = T, children = list(
            dbcCardBody(children = list(
              htmlP("Select time interval:"),
              dccRangeSlider(id = "year-range-users", min = 2009, max = 2021, value = list(2009, 2020), marks = list(
                "2009" = list(label = "2009", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')), 
                "2010" = list(label = "", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')), 
                "2011" = list(label = "", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')),  
                "2012" = list(label = "2012", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')),  
                "2013" = list(label = "", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')), 
                "2014" = list(label = "", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')),  
                "2015" = list(label = "2015", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')), 
                "2016" = list(label = "", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')), 
                "2017" = list(label = "", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')), 
                "2018" = list(label = "2018", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')), 
                "2019" = list(label = "", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')), 
                "2020" = list(label = "", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')), 
                "2021" = list(label = "2021", style = list(transform = "rotate(-45deg)", marginLeft = '-20px', userSelect = 'none')) 
              )),
              htmlBr(),
              htmlP("Select forum:"),
              dccDropdown(id = "selected-forum-users", options = forums_list_dropdown, value = "lifehacks")
            ))
          ))
        )),
        dbcCol(width = 8, align = "center", children = list(
          dbcRow(justify = "center", children = list(
            dbcCard(className = "info-card", color = "primary", style = list(margin = "15px"), children = list(
              dbcCardHeader("Number of users:", style = list("color" = "white")),
              dbcCardBody(htmlH5("...", id = "num-users", style = list("color" = "white", textAlign = "center")))
            )),
            dbcCard(className = "info-card", color = "primary", style = list(margin = "15px"), children = list(
              dbcCardHeader("Active users:", style = list("color" = "white")),
              dbcCardBody(htmlH5("...", id = "active-users", style = list("color" = "white", textAlign = "center")))
            )),
            dbcCard(className = "info-card", color = "primary", style = list(margin = "15px"), children = list(
              dbcCardHeader("Highest reputation:", style = list("color" = "white")),
              dbcCardBody(htmlH5("...", id = "best-user", style = list("color" = "white", textAlign = "center")))
            ))
          ))
        ))
      )),
      dbcRow(style = list(margin = "30px 0px 30px"), children = list(
        dbcCol(children = list(
          htmlH3("1. How many users vote down more frequently rather than up?"),
          htmlP("It turned out that on each stackexchange.com forums are a few users who have DownVote count higher than UpVote. Voting down influences
                on your reputation by lowering it by one or two points. Hence not all users are able to achive this as they may not have enought
                reputation to make so many DownVotes."),
          htmlP("The following chart presents the ratio of beforementioned users (lets call them 'dark users') to the number of all users. We compare it
                with the ratio of negative score posts to the number of all posts. These values should interfere - more bad posts, more discontented users."),
          htmlP("The ratio of users was mulitplied by ten."),
          dccGraph(id = "graph-popular-tags", figure = plot_darkness()),
          htmlH3("2. Dark users hall of shame", style = list(margin = "40px 0px 0px 0px")),
          htmlP("After considering 18 stackexchange.com subforums we can see 'the worst' dark users. Suprisingly there are one user who deserved on the
                title of 'The Darkest User of Stackexchange.com' by being in top 10 of darkest users on each forum (having Diff = DownVotes - UpVotes > 0, sorted
                descending by Diff, head 10) for 16 times of 18!"),
          dbcRow(
            dbcCol(width = list(size = 4, offset = 4), children = list(
              dbcCard(color = 'dark', inverse = T, children = list(
                dbcCardHeader("The Darkest User", style = list(textAlign = "center")),
                dbcCardBody(
                  dbcCol(children = list(
                    htmlDiv(style = list(display = "flex", flexDirection = "column", alignItems = "center", margin = "10px 0px 30px 0px"),
                            children = list(
                      htmlImg(src = darkins$ProfileImageUrl[1], style = list(width = 200, height = 'auto')) 
                    )),
                    htmlH3(darkins$DisplayName[1]),
                    htmlP(paste("Number of terrored forums:", darkins$NumOfTerroredForums[1])),
                    htmlP(paste("Avg. votes difference:", round(avg_diff$avg[1], 2)))
                  ))
                )
              ))
            ))
          ),
          dbcRow(style = list(margin = "40px 0px 0px 0px"), children = list(
            dbcCol(width = 3, children = list(
              dbcCard(color = 'dark', inverse = T, children = list(
                dbcCardHeader("Dark User", style = list(textAlign = "center")),
                dbcCardBody(
                  dbcCol(children = list(
                    htmlH5(darkins$DisplayName[18]),
                    htmlP(paste("Number of terrored forums:", darkins$NumOfTerroredForums[18])),
                    htmlP(paste("Avg. votes difference:", round(avg_diff$avg[2], 2)))
                  ))
                )
              ))
            )),
            dbcCol(width = 3, children = list(
              dbcCard(color = 'dark', inverse = T, children = list(
                dbcCardHeader("Dark User", style = list(textAlign = "center")),
                dbcCardBody(
                  dbcCol(children = list(
                    htmlH5(darkins$DisplayName[22]),
                    htmlP(paste("Number of terrored forums:", darkins$NumOfTerroredForums[22])),
                    htmlP(paste("Avg. votes difference:", round(avg_diff$avg[3], 2)))
                  ))
                )
              ))
            )),
            dbcCol(width = 3, children = list(
              dbcCard(color = 'dark', inverse = T, children = list(
                dbcCardHeader("Dark User", style = list(textAlign = "center")),
                dbcCardBody(
                  dbcCol(children = list(
                    htmlH5(darkins$DisplayName[25]),
                    htmlP(paste("Number of terrored forums:", darkins$NumOfTerroredForums[25])),
                    htmlP(paste("Avg. votes difference:", round(avg_diff$avg[4], 2)))
                  ))
                )
              ))
            )),
            dbcCol(width = 3, children = list(
              dbcCard(color = 'dark', inverse = T, children = list(
                dbcCardHeader("Dark User", style = list(textAlign = "center")),
                dbcCardBody(
                  dbcCol(children = list(
                    htmlH5(darkins$DisplayName[28]),
                    htmlP(paste("Number of terrored forums:", darkins$NumOfTerroredForums[28])),
                    htmlP(paste("Avg. votes difference:", round(avg_diff$avg[5], 2)))
                  ))
                )
              ))
            ))
          ))
        ))
      ))
    )
  )
}