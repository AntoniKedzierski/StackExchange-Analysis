install.packages("lubridate")
install.packages("plotly")
install.packages("stringr")


library(dplyr)
library(lubridate)
library(xml2)
library(purrr)
library(plotly)
library(stringr)


Badges_DIY<-read.csv("Badges_DIY.csv")
Comments_DIY<-read.csv("Comments_DIY.csv")
PostLinks_DIY<-read.csv("PostLinks_DIY.csv")
Posts_DIY<-read.csv("Posts_DIY.csv")
Tags_DIY<-read.csv("Tags_DIY.csv")
Users_DIY<-read.csv("Users_DIY.csv")
Votes_DIY<-read.csv("Votes_DIY.csv")

head(Badges_DIY)
head(Comments_DIY)
head(PostLinks)
head(Posts_DIY)
head(Tags_DIY)
head(Users_DIY)
head(Votes)


#1.
##########################################################################
##########################################################################


#najlepsze tagi za dowolny okres

from<-"2020-01-01"
to<-"2020-01-02"
from
to


#podaj rok-miesiac-dzien
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
    arrange(desc(Count)) %>%
    top_n(10, Count)
  
  counted <- droplevels(counted)
  
  return (counted)
}

?top_n
popular_tags<- popular_tags("2020-01-01", "2020-12-31", Tags_DIY, Posts_DIY)

fig <- plot_ly(
  x = popular_tags$Count,
  y = popular_tags$TagName,
  name = "SF Zoo",
  type = "bar",
  marker = list(color = c('rgba(222,45,38,0.8)', rep('rgba(204,204,204,1)', times=length(popular_tags$TagName)-1)))
)
fig


fig %>% layout(title = "Popularity of the tags",
               xaxis = list(title = "number of occurrence"),
               yaxis = list(title = "Tags"))

############################################################
#1b 

#Ile czekac na poprawna odp
# z podzialem na te najpopularniejsze tagi (nizej jest funckja i zebrane w kupie)

Posts<-Posts_DIY
Questions<-Posts %>%
  filter(PostTypeId==1) %>% #bierzemy pytania
  select(Id, AcceptedAnswerId, CreationDate, Tags)

Answers<-Posts %>%
  filter(PostTypeId==2) %>% #bierzemy pytania
  select(Id, CreationDate)


QandA<- inner_join(Questions, Answers, by=c("AcceptedAnswerId"="Id"), suffix=c(".Q", ".A"))

QandA$Period<-date(QandA$CreationDate.A)-date(QandA$CreationDate.Q)
QandA$Period[which(QandA$Period>10)]<-11

QandA<-droplevels(QandA)

#wyznacze ten glowny tag w kazdym pytaniu

QandA$MainTag<-str_remove(word(QandA[,"Tags"], 1, sep = fixed('>')), "<")
head(QandA)



#Przefiltrujemy sobie pytania tylko z najpopulrniejszymi tagami i sprawdze jak dlugo trzeba czekac na odp
GroupedQA<-QandA %>%
  filter(MainTag %in% popular_tags$TagName) %>%
  group_by(MainTag, Period) %>%
  tally()

Total<-GroupedQA %>%
  group_by(MainTag) %>%
  summarise(Total=sum(n))

GroupedQA$Percent<-GroupedQA$n/(inner_join(GroupedQA, Total, by="MainTag")$Total)*100

GroupedQA$MainTag

#ILOSCIOWO
fig <- plot_ly(GroupedQA, x = ~Period, y = ~n, color = ~GroupedQA$MainTag) 
fig <- fig %>% add_lines()
fig

#PROCENTOWO
fig_percent <- plot_ly(GroupedQA, x = ~Period, y = ~Percent,color = ~GroupedQA$MainTag) 
fig_percent <- fig_percent %>% add_lines()
fig_percent

#fig <- fig %>% layout(title = '',
                      #xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      #yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), "percent")

#################################################################
#1b FUNKCJA:

TimeToAnswer<-function(from, to, Tags, Posts){
  
      popular_tags<- popular_tags(from, to, Tags, Posts)
  
      Questions<-Posts %>%
        filter(PostTypeId==1, date(CreationDate)>=from, date(CreationDate)<=to) %>% #bierzemy pytania
        select(Id, AcceptedAnswerId, CreationDate, Tags)
      
      Answers<-Posts %>%
        filter(PostTypeId==2, date(CreationDate)>=from, date(CreationDate)<=to) %>% #bierzemy pytania
        select(Id, CreationDate)
      
      
      QandA<- inner_join(Questions, Answers, by=c("AcceptedAnswerId"="Id"), suffix=c(".Q", ".A"))
      
      QandA$Period<-as.numeric(difftime(QandA$CreationDate.A, QandA$CreationDate.Q, unit = c("hours")))
      # QandA$Period[which(QandA$Period>10)]<-11 #chodzi o to ze 11 i wiecej, po potem sa juz pojedyncze
      
      QandA<-droplevels(QandA)
      
      #wyznacze ten glowny tag w kazdym pytaniu
      
      QandA$MainTag<-str_remove(word(QandA[,"Tags"], 1, sep = fixed('>')), "<")
      QandA = QandA %>%
        select(DayDiff = Period, MainTag) %>%
        group_by(MainTag) %>%
        summarise(AvgAwaitingTime = mean(DayDiff), .groups = "drop") %>%
        filter(AvgAwaitingTime > 0) %>%
        arrange(AvgAwaitingTime)
      
      #Przefiltrujemy sobie pytania tylko z najpopulrniejszymi tagami i sprawdze jak dlugo trzeba czekac na odp
      # GroupedQA<-QandA %>%
      #   filter(MainTag %in% popular_tags$TagName) %>%
      #   group_by(MainTag, Period) %>%
      #   tally()
      # 
      # 
      # Total<-GroupedQA %>%
      #   group_by(MainTag) %>%
      #   summarise(Total=sum(n))
      # 
      # #procentowo
      # GroupedQA$Percent<-GroupedQA$n/(inner_join(GroupedQA, Total, by="MainTag")$Total)*100
      # 
      # return(GroupedQA)
      return(QandA)
}

TimeToAnswer<-TimeToAnswer("2020-01-01", "2020-12-31", Tags_DIY, Posts_DIY) 
TimeToAnswer

Plot_Answers_C<-function(TimeToAnswer){
  
      #ILOSCIOWO
      fig <- plot_ly(TimeToAnswer, x = ~Period, y = ~n, color = ~TimeToAnswer$MainTag) 
      fig <- fig %>% add_lines()
      return (fig)
      
}

Plot_Answers_C(TimeToAnswer)

Plot_Answers_P<-function(TimeToAnswer){
  
      #PROCENTOWO
      fig_percent <- plot_ly(GroupedQA, x = ~Period, y = ~Percent,color = ~GroupedQA$MainTag) 
      fig_percent <- fig_percent %>% add_lines()
      fig_percent
      
      return (fig_percent)
      
}

Plot_Answers_P(TimeToAnswer)

###############################################################
#2.


#PYTANIA
Posts<-Posts_DIY

Question <-Posts %>%
  filter(PostTypeId==1, year(CreationDate)==2020) %>%
  select(Title, CreationDate, Tags)

Question$Month<-month(Question$CreationDate)  

#Ile postow w kazdym miesiacy
Question<-Question %>%
  group_by(Month) %>%
  tally() 


#Answears

Answear <-Posts %>%
  filter(PostTypeId==2, year(CreationDate)==2020) %>%
  select(Title, CreationDate, Tags)

Answear$Month<-month(Answear$CreationDate)  

Answear<-Answear %>%
  group_by(Month) %>%
  tally() 

Question
Answear

QandA<-inner_join(Question,
                  Answear, by="Month", suffix=c(".Q", ".A"))


fig <- plot_ly(QandA, x = ~Month, y = ~n.Q, name = 'Questions', type = 'scatter', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~n.A, name = 'Answears', mode = 'lines+markers') 

fig <- fig %>% layout(title = "Number of questions and answers in 2020",
                      xaxis = list(title = "month"),
                      yaxis = list(title = ""))

fig

###############################################
#2. Funkcja

QandA_perMonth<- function(from, to, Posts){
        
        #Question
        Question <-Posts %>%
            filter(PostTypeId==1, date(CreationDate)>=from, date(CreationDate)<=to) %>%
            select(Title, CreationDate, Tags)
          
        Question$Month<-month(Question$CreationDate)  
        
        #Ile postow w kazdym miesiacu
        Question<-Question %>%
            group_by(Month) %>%
            tally() 
        
        #Answears
        
        Answear <-Posts %>%
            filter(PostTypeId==2, year(CreationDate)==2020) %>%
            select(Title, CreationDate, Tags)
        
        Answear$Month<-month(Answear$CreationDate)  
        
        Answear<-Answear %>%
            group_by(Month) %>%
            tally() 
        
        QandA<-inner_join(Question,
                          Answear, by="Month", suffix=c(".Q", ".A"))
        return(QandA)
}

QandA<-QandA_perMonth("2020-01-01", "2020-12-31", Posts_DIY)

Plot_QandA<-function(QandA){        
        fig <- plot_ly(QandA, x = ~Month, y = ~n.Q, name = 'Questions', type = 'scatter', mode = 'lines+markers') 
        fig <- fig %>% add_trace(y = ~n.A, name = 'Answears', mode = 'lines+markers') 
        
        fig <- fig %>% layout(title = "Number of questions and answers",
                              xaxis = list(title = "month"),
                              yaxis = list(title = ""))
        
        return(fig)

}

Plot_QandA(QandA)

###############################################################
#3.
head(Users_DIY)
Users_DIY$Difference<-Users_DIY$UpVotes-Users_DIY$DownVotes

Difference<-Users_DIY%>%
  select( Difference) %>%
  group_by(Difference)%>%
  tally()

Users

fig <- plot_ly(Difference,  x = ~Difference, y = ~n, type = 'scatter', mode='markers', 
               marker = list( size = 8))

fig <- fig %>% layout(title = "Users ",
                      xaxis = list(title = "month"),
                      yaxis = list(title = "users"))


fig

#Co to za czlowiek co ma takie zle rangi?
Users_DIY[which.min(Users_DIY$Difference), "AboutMe"]
#How many users have no

Users_DIY[which.max(Users_DIY$Difference), ""] # ten ma brak opisu to sprawdzmy wiecej
bestUser<-Users_DIY[which.max(Users_DIY$Difference), "Id"] 

PostType<-Posts_DIY[which(Posts_DIY$OwnerUserId==bestUser),] %>%
  group_by(PostTypeId) %>%
  tally()

#zobaczmy ile zrobil odpowiedzi i pytan
#1 -- Questions
#2 -- Answers
#3 -- Orphan Tag Wikis (tag wikis for tags that have since been deleted. ? mods can see these live)
#4 -- Tag Wiki Excerpts
#5 -- Tag Wiki Bodys
#6 -- Election nominations

labels = c('Questions', 'Answers','Tag Wiki Excerpts','Tag Wiki Bodys')


fig <- plot_ly(PostType, labels = labels, values = ~n, type = 'pie',
              
               textinfo = 'value+percent',
               insidetextfont = list(color = '#FFFFFF'),
               insidetextorientation='radial',
               hoverinfo = 'text',
               marker = list( line = list(color = '#FFFFFF', width = 1)),
               showlegend = TRUE,
              width = 600, height = 600, automargin=T)

fig <- fig %>% layout(title = 'Type tags',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

##################################################
#Wracajac do DIY

head(Posts_DIY)

#zaleznosc ilosci odpowiedzi

Posts_DIY2 <- Posts_DIY %>%
  select(PostTypeId, Score, ViewCount, AnswerCount) %>%
  filter(PostTypeId==1 || PostTypeId==2)
  

naomitposts <- Posts_DIY2[complete.cases(Posts_DIY2),]


fig <- plot_ly(naomitposts) 

fig <- fig %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label='AnswerCount', values=~AnswerCount),
      list(label='ViewCount', values=~ViewCount),
      list(label='Score', values=~Score)
      )
    ,
    marker = list(
      size = 5,
      line = list(
        width = 1,
        color = 'rgb(230,230,230)'
      )
    )
  ) 
fig


#usune tego z 77 odp
naomitposts<-naomitposts[-which.max(naomitposts$AnswerCount),]

#ViewCoubt od Score

fig2 <- plot_ly(naomitposts, x=~Score, y=~ViewCount, type='scatter', mode='markers', marker = list(size = 10,
                                                                 color = 'rgba(255, 182, 193, .9)',
                                                                 line = list(color = 'rgba(152, 0, 0, .8)',
                                                                             width = 2))) 
fig2
