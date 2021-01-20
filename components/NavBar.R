library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(dashHtmlComponents)

NavBar <- function() {
  dbcNavbarSimple(
    children = list(
      dbcNavItem(dbcNavLink("Dark users", href = "users")),
      dbcNavItem(dbcNavLink("Tags", href = "tags")),
      dbcNavItem(dbcNavLink("Posts", href = "posts"))
    ),
    brand = "StackExchange Analysis",
    brand_href = "/",
    color = "primary",
    dark = T
  )
}
