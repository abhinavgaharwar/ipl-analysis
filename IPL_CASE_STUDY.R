#CASE STUDY OF IPL DATA
#-----------------------
library(dplyr)
ball_by_ball=read_xlsx('D:/R Programming/case study/ipl case study/Ball_by_Ball.xlsx')
match=read_xlsx('d:/R Programming/case study/ipl case study/Match.xlsx')
player=read_xlsx('d:/R Programming/case study/ipl case study/Player.xlsx')
player_match=read_xlsx('d:/R Programming/case study/ipl case study/Player_Match.xlsx')
season=read_xlsx('d:/R Programming/case study/ipl case study/Season.xlsx')
team=read_xlsx('d:/R Programming/case study/ipl case study/Team.xlsx')


T = merge(x= ball_by_ball[,c("Striker_Id","Batsman_Scored")],y=player[,c("Player_Id","Player_Name")],
          by.x="Striker_Id",by.y="Player_Id",all.x=TRUE)

#Q1a)Top 10 Batsman based on Runs scored
#-------------------------------------------

Q1=T %>% group_by(Player_Name) %>%
  summarise(Total_Runs = sum(as.integer(Batsman_Scored),na.rm=TRUE)) %>%
  arrange(-Total_Runs)%>%head(10)
View(Q1)

#Q1b) Top 10 batsman based on Number of 6s
#--------------------------------------------
Q2 = T %>% filter(Batsman_Scored=='6')%>%
  group_by(Player_Name) %>% summarise(no.of.sixes=n())%>%
  arrange(-no.of.sixes)%>%head(10)
View(Q2)

#Q1c) Top 10 batsmans based on Number of 4s
#--------------------------------------------
Q3 = T %>% filter(Batsman_Scored=='4')%>%
  group_by(Player_Name) %>% summarise(no.of.fours=n())%>%
  arrange(-no.of.fours)%>%head(10)
View(Q3)


#Q1d) Top 10 batsman based to Number of 100s
#---------------------------------------------
T1 = merge(x= ball_by_ball[,c("Match_Id","Striker_Id","Batsman_Scored")],y=player[,c("Player_Id","Player_Name")],
           by.x="Striker_Id",by.y="Player_Id",all.x=TRUE)

Q4 = T1 %>% group_by(Player_Name,Match_Id) %>%
  summarise(total_score = sum(as.integer(Batsman_Scored),na.rm=T)) %>%
  filter(total_score > 99) %>%
  summarise(no.of.centuries = n()) %>%
  arrange(-no.of.centuries) %>%
  head(10)%>%
  View()

#Q2a)	Top 10 Teams Based on number of matches won
#--------------------------------------------------
T3 = merge(x=team[,c("Team_Id","Team_Name")],y= match[,c("Match_Id","Match_Winner_Id")],
           by.x="Team_Id",by.y="Match_Winner_Id",all.x=TRUE)

T3 %>% group_by(Team_Name) %>%
  summarise(no.of.matches.won=n())%>%
  arrange(-no.of.matches.won)%>%
  View()

#Q2b) Top 10 teams Based on winning the toss and winning the match
#-----------------------------------------------------------------
T4 = merge(x=team[,c("Team_Id","Team_Name")],y= match[,c("Match_Winner_Id","Toss_Winner_Id")],
           by.x="Team_Id",by.y="Toss_Winner_Id")

T4 %>% group_by(Team_Name) %>% 
  filter(Team_Id==Match_Winner_Id) %>%
  summarise(no.of.matches.won = n()) %>% 
  arrange(-no.of.matches.won)%>%
  View()


#Q2c) Top 10 teams Based on team winning highest number of Man of the match.
#----------------------------------------------------------------------------
View(match)
a=merge(x = match[,c("Season_Id","Match_Id","Man_Of_The_Match_Id")],
        y = player_match[,c("Match_Id","Player_Id","Team_Id")],
        by.x = c("Match_Id"),
        by.y = c("Match_Id"))
View(a)
a %>%  filter(Player_Id==Man_Of_The_Match_Id) %>% 
  group_by(Team_Id) %>%
  summarise(NO_of_MOM = n()) %>%
  arrange(-NO_of_MOM) %>%
  View()

#OR Top two teams of each season

a %>%  filter(Player_Id==Man_Of_The_Match_Id) %>% 
  group_by(Season_Id,Team_Id) %>%
  summarise(NO_Of_MOM = n()) %>%
  arrange(Season_Id,-NO_Of_MOM) %>%
  top_n(2,wt = NO_Of_MOM) %>%       #wt = NO_of_MOM is optional if the column is last column
  View()


#Q3a)	Season wise Orange Cap holder
#------------------------------------
Q3a = merge(x=season[,c("Season_Year","Orange_Cap_Id")],
            y=player[,c("Player_Id","Player_Name")],
            by.x="Orange_Cap_Id",
            by.y="Player_Id")


Q3a  %>% select(Season_Year,Player_Name) %>% 
  arrange(Season_Year) %>% 
  View()

#Q3b)Season wise purple Cap holder
#----------------------------------

Q3b = merge(x=season[,c("Season_Year","Purple_Cap_Id")],y=player[,c("Player_Id","Player_Name")],
            by.x="Purple_Cap_Id",by.y="Player_Id")

Q3b  %>% select(Season_Year,Player_Name) %>% 
  arrange(Season_Year) %>% 
  View()

#Q4)	For each season matches with biggest win margin
#-------------------------------------------------------
Q4 = merge(x=season[,c("Season_Id","Season_Year")],y=match[,c("Season_Id","Won_By","Win_Type")])

Q4 %>% filter(tolower(Win_Type) == 'by runs')%>%
  group_by(Season_Year)%>%
  summarise(Margin = max(as.integer(Won_By),na.rm = T)) %>%
  View()

#Q5a)Total Number of : matches
#------------------------------

match %>% summarise(total_matches = n_distinct(Match_Id)) %>% View()

#Q5b)Total Number of : 100s
#-----------------------------

ball_by_ball %>% group_by(Striker_Id,Match_Id) %>%
  summarise(total_score = sum(as.integer(Batsman_Scored),na.rm=T)) %>%
  filter(total_score > 99) %>%
  summarise(no.of.centuries = n()) %>%
  summarise(total_centuries = sum(no.of.centuries)) %>%
  View()

#Q5c)	Total Number of : 50s
#-----------------------------

ball_by_ball %>% group_by(Striker_Id,Match_Id) %>%
  summarise(total_score = sum(as.integer(Batsman_Scored),na.rm=T)) %>%
  filter(total_score > 49 & total_score < 99 ) %>%
  summarise(no.of.fifties = n()) %>%
  summarise(total_fifties = sum(no.of.fifties)) %>%
  View()

#Q5d)Total Number of : 6s
#--------------------------
ball_by_ball %>% 
  summarise(total_sixes = sum(ball_by_ball$Batsman_Scored == 6,TRUE,FALSE,na.rm = TRUE)) %>%
  View()

#or
ball_by_ball %>% filter(Batsman_Scored == 6) %>%
  summarise(total_sixes = n()) %>%
  View()




#Q5e)	Total Number of :4s
#------------------------------------
ball_by_ball %>% 
  summarise(total_fours = sum(ball_by_ball$Batsman_Scored == 4,TRUE,FALSE,na.rm = TRUE)) %>%
  View()

#or

ball_by_ball %>%  filter(Batsman_Scored == 4) %>%
  summarise(total_fours = n() )%>%
  View()


#Q6)	Team wise count of title win.
#------------------------------------
match=read_xlsx('d:/R Programming/case study/ipl case study/Match.xlsx')
#str(match)
a=match %>% group_by(Season_Id) %>%  
  summarise(final_match_date = max(Match_Date))
#View(a)
b = merge(x=a,y=match,by.x = "final_match_date",by.y = "Match_Date" )

c = b %>% select(Season_Id.x,Match_Winner_Id) 
#View(c)

d = merge(x=c,y=team,by.x="Match_Winner_Id",by.y = "Team_Id")

d %>% group_by(Team_Name) %>%
  summarise(no.of.titles = n()) %>%
  arrange(-no.of.titles) %>%
  View() 


#or
#---------
a = match %>% group_by(Season_Id) %>%
  summarise(final_match_date = max(Match_Date)) %>%
  merge(match[,-5],by.x = "final_match_date",by.y = "Match_Date") %>%
  select(Season_Id,Match_Winner_Id) %>%
  merge(team[,-3],by.x = "Match_Winner_Id",by.y = "Team_Id") %>%
  group_by(Team_Name) %>%
  summarise(No_Of_Titles = n()) %>%
  arrange(-No_Of_Titles) %>%
  View()


