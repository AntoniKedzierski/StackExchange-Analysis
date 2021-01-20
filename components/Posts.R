library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(dashHtmlComponents)


Posts <- function() {
  dbcContainer(
    children = list(
      dbcRow(children = list(
        dbcCol(width = 4, children = list(
          dbcCard(color = "primary", outline = T, children = list(
            dbcCardBody(children = list(
              htmlP("Select time interval:"),
              dccRangeSlider(id = "year-range-posts", min = 2009, max = 2021, value = list(2009, 2020), marks = list(
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
              dccDropdown(id = "selected-forum-posts", options = forums_list_dropdown, value = "lifehacks")
            ))
          ))
        )),
        dbcCol(width = 8, align = "center", children = list(
          dbcRow(justify = "center", children = list(
            dbcCard(className = "info-card", color = "primary", style = list(margin = "15px"), children = list(
              dbcCardHeader("Number of posts:", style = list("color" = "white")),
              dbcCardBody(htmlH5("...", id = "num-posts", style = list("color" = "white", textAlign = "center")))
            )),
            dbcCard(className = "info-card", color = "primary", style = list(margin = "15px"), children = list(
              dbcCardHeader("Avg. posts per week:", style = list("color" = "white")),
              dbcCardBody(htmlH5("...", id = "avg-posts-per-week", style = list("color" = "white", textAlign = "center")))
            )),
            dbcCard(className = "info-card", color = "primary", style = list(margin = "15px"), children = list(
              dbcCardHeader("Avg. post score:", style = list("color" = "white")),
              dbcCardBody(htmlH5("...", id = "avg-posts-score", style = list("color" = "white", textAlign = "center")))
            ))
          ))
        ))
      )),
      dbcRow(style = list(margin = "30px 0px 30px"), children = list(
        dbcCol(children = list(
          htmlH3("1. How many questions are asked in each hour?"),
          htmlP("The bar plot shows the distribution of question posting hours."),
          dccGraph(id = "questions-per-hour"),
          htmlH3("2. Relation between number of posts and users", style = list(margin = "40px 0px 0px 0px")),
          htmlP("On the chart one can see how the number of new users and new posts vary in time. In most cases
                both values are correlated with each other."),
          dccGraph(id = "posts-and-users")
        ))
      ))
    )
  )
}