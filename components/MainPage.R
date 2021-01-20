library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(dashHtmlComponents)

MainPage <- function() {
  dbcContainer(
    dbcJumbotron(children = list(
      htmlH1("StackExchange Analysis", className = "display-3"),
      htmlP("Antoni Kedzierski, Kinga Bryndza", className = "lead"),
      htmlHr(className = "my-2"),
      htmlP("The project made on 'Processing and Data Analysis in R Language' subject."),
      htmlP(dbcButton("Source code", color = "primary"), className = "lead")
    ))
  )
}
