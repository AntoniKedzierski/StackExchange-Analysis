library(dplyr)

popular_tags<-function(from, to, Tags, Posts) {
  
  if (from>to) break
  
  Questions<-Posts %>%
    filter(PostTypeId==1, date(CreationDate)>=from, date(CreationDate)<=to) %>%
    select(Title, CreationDate, Tags)
  
  #lista tagow
  Tags<-Tags['TagName']
  
  counted<-as.data.frame(matrix(0, nrow=length(t(Tags)), ncol=2))
  colnames(counted)<-c("TagName", "Count")
  
  counted[,1]<-Tags    
  
  for (i in 1:length(t(Tags)) ){
    counted[i,2]<-length(grep(counted[i,1], Questions$Tags))
  }
  
  counted<-counted %>%
    arrange(desc(Count))
  
  counted <- droplevels(counted)
  
  return (counted)
}


plot_popular_tags <- function(popular_tags) {
  popular_tags$TagName <- factor(popular_tags$TagName, levels = unique(popular_tags$TagName)[order(popular_tags$Count, decreasing = T)]) 
  fig <- plot_ly(
    x = popular_tags$TagName,
    y = popular_tags$Count,
    name = "SF Zoo",
    type = "bar",
    marker = list(color = c('rgba(222,45,38,0.8)', rep('rgba(204,204,204,1)', times=length(popular_tags$TagName)-1)))
  )

  
  fig = fig %>% 
    layout(title = "Popularity of the tags",
           xaxis = list(title = "Tag"),
           yaxis = list(title = "Number of occurrence"))
  
  return(fig)
}

awaiting_time <- function(from, to, Posts__) {
  Questions<-Posts__ %>%
    filter(PostTypeId==1 & date(CreationDate)>=from & date(CreationDate)<=to) %>% #bierzemy pytania
    mutate(CreationDate = gsub("T", " ", CreationDate)) %>%
    select(Id, AcceptedAnswerId, CreationDate, Tags)
  
  Answers<-Posts__ %>%
    filter(PostTypeId==2, date(CreationDate)>=from, date(CreationDate)<=to) %>% #bierzemy pytania
    mutate(CreationDate = gsub("T", " ", CreationDate)) %>%
    select(Id, CreationDate)
  
  QA <- inner_join(Questions, Answers, by=c("AcceptedAnswerId"="Id"), suffix=c(".Q", ".A")) %>%
    mutate(AwaitingTime = as.numeric(difftime(gsub("T", " ", CreationDate.A), gsub("T", " ", CreationDate.Q), units = c("mins"))) / 60) %>%
    mutate(MainTag = str_remove(word(Tags, 1, sep = fixed('>')), "<")) %>%
    select(MainTag, AwaitingTime) %>%
    group_by(MainTag) %>%
    summarise(AvgAwaitingTime = mean(AwaitingTime), elem = n(), .groups = "drop") %>%
    filter(elem > (mean(elem, na.rm = T) + sd(elem, na.rm = T)) & AvgAwaitingTime > 0) %>%
    arrange(AvgAwaitingTime) %>%
    select(Tag = MainTag, AvgAwaitingTime, elem) %>%
    head(10)
  
  return(QA)
}


plot_awaiting_time <- function(awTime) {
  awTime$Tag <- factor(awTime$Tag, levels = unique(awTime$Tag)[order(awTime$AvgAwaitingTime)])
  fig <- plot_ly(
    x = awTime$Tag,
    y = awTime$AvgAwaitingTime,
    name = "SF Zoo",
    type = "bar",
    marker = list(color = c('rgba(222,45,38,0.8)', rep('rgba(204,204,204,1)', times=length(awTime$Tag)-1)))
  )
  
  annotations = list()
  for (i in 1:length(awTime$Tag)) {
    annotations[[i]] = list(
      x = awTime$Tag[i],
      y = awTime$AvgAwaitingTime[i],
      yanchor = "bottom",
      text = as.character(awTime$elem[i]),
      name = "SF Zoo",
      showarrow = F
    )
  }
  
  fig = fig %>% layout(title = "Average awaiting time",
                       xaxis = list(title = "Tag"),
                       yaxis = list(title = "Time [h]"),
                       annotations = annotations
                       )
  
  return(fig)
}




