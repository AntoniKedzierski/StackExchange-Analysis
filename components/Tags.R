library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(dashHtmlComponents)


Tags <- function() {
  dbcContainer(
    children = list(
      dbcRow(children = list(
        dbcCol(width = 4, children = list(
          dbcCard(color = "primary", outline = T, children = list(
            dbcCardBody(children = list(
              htmlP("Select time interval:"),
              dccRangeSlider(id = "year-range-tags", min = 2009, max = 2021, value = list(2009, 2020), marks = list(
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
              dccDropdown(id = "selected-forum-tags", options = forums_list_dropdown, value = "lifehacks")
            ))
          ))
        )),
        dbcCol(width = 8, align = "center", children = list(
          dbcRow(justify = "center", children = list(
            dbcCard(className = "info-card", color = "primary", style = list(margin = "15px"), children = list(
              dbcCardHeader("Number of different tags:", style = list("color" = "white")),
              dbcCardBody(htmlH5("...", id = "different-tags", style = list("color" = "white", textAlign = "center")))
            )),
            dbcCard(className = "info-card", color = "primary", style = list(margin = "15px"), children = list(
              dbcCardHeader("Number of posts:", style = list("color" = "white")),
              dbcCardBody(htmlH5("...", id = "num-posts-tags", style = list("color" = "white", textAlign = "center")))
            )),
            dbcCard(className = "info-card", color = "primary", style = list(margin = "15px"), children = list(
              dbcCardHeader("Most popular tag:", style = list("color" = "white")),
              dbcCardBody(htmlH5("...", id = "most-popular-tag", style = list("color" = "white", textAlign = "center")))
            ))
          ))
        ))
      )),
      dbcRow(style = list(margin = "30px 0px 30px"), children = list(
        dbcCol(children = list(
          htmlH3("1. Which tags are most popular on the forum?"),
          htmlP("By counting the number of occurrence for each tag, we can evaluate
                which are most freqeuent."),
          dccGraph(id = "graph-popular-tags"),
          htmlH3("2. How many hours would you wait till you get an answer you would accept?", style = list(margin = "40px 0px 0px 0px")),
          htmlP("The chart presents estimating time you will wait if you tag your question with
                tags specified on x-axis as the first (or main) tag. Consider only those that were used
                under more than: avg(num) + std(num), where num is the number of questions marked with
                the tag as primary."),
          htmlP("The number over the bars indicate how many records were considered to calculate average time."),
          dccGraph(id = "graph-awaiting-time")
        ))
      ))
    )
  )
}








