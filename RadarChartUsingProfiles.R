
##Radar charts

library(dplyr)
library(ggplot2)
install.packages("fmsb")
library(fmsb)

setwd("~/Desktop/Hackathon")
data = read.csv("User_profiles v2.csv")
data = data %>%select( -c(no_genres_listed_Mean, countNo))
data = data[complete.cases(data),]


data_ = data %>% summarise(Action = mean(Action_Mean),
                                       Adventure = mean(Adventure_Mean),
                                       Animation = mean(Animation_Mean),
                                       Childrens = mean(Childrens_Mean),
                                       Comedy = mean(Comedy_Mean),
                                       Crime = mean(Crime_Mean),
                                       Documentary = mean(Documentary_Mean),
                                       Drama = mean(Drama_Mean),
                                       Fantasy = mean(Fantasy_Mean),
                                       FilmNoir = mean(Film_Noir_Mean),
                                       Horror = mean(Horror_Mean),
                                       Musical = mean(Musical_Mean),
                                       Mystery = mean(Mystery_Mean),
                                       Romance = mean(Romance_Mean),
                                       SciFi = mean(Sci_Fi_Mean),
                                       Thriller = mean(Thriller_Mean),
                                       War = mean(War_Mean),
                                       Western = mean(Western_Mean))


##in all, what is the mean rating for each genre
data_2overallmean = data %>% summarise(Action_Adventure = ((mean(Action_Mean) + mean(Adventure_Mean))/2),
                                                             Animation = mean(Animation_Mean),
                                                             Childrens = mean(Childrens_Mean),
                                                             Documentary = mean(Documentary_Mean),
                                                             FilmNoir = mean(Film_Noir_Mean),
                                                             Horror_Thriller =( mean(Horror_Mean) +mean(Thriller_Mean))/2,
                                                             Musical = mean(Musical_Mean),
                           Romance_Comedy = (mean(Romance_Mean) + mean(Comedy_Mean))/2,
                                                             SciFi = mean(Sci_Fi_Mean),
                                                             War = mean(War_Mean),
                                                             Western = mean(Western_Mean))


data_ <- t(apply(data_, 1, function(x) {
  x[x <=3] <- 3
  return(x)
}))


##Documentary fans - they are a fan if they reviewed over 30 documentaries 
data_2 = data %>% filter(countDocumentary >30) %>% summarise(Action_Adventure = ((mean(Action_Mean) + mean(Adventure_Mean))/2),
                                                             Animation = mean(Animation_Mean),
                                                             Childrens = mean(Childrens_Mean),
                                                             Documentary = mean(Documentary_Mean),
                                                             FilmNoir = mean(Film_Noir_Mean),
                                                             Horror_Thriller =( mean(Horror_Mean) +mean(Thriller_Mean))/2,
                                                             Musical = mean(Musical_Mean),
                                                             Romance_Comedy = (mean(Romance_Mean) + mean(Comedy_Mean))/2,
                                                             SciFi = mean(Sci_Fi_Mean),
                                                             War = mean(War_Mean),
                                                             Western = mean(Western_Mean))

#define max and min for all categories in order to put into radarchart()

max_min_2 <- data.frame(
  Action_Adventure = c(4,3), 
  Animation = c(4,3),
  Childrens = c(4,3),
  Documentary = c(4,3), 
  FilmNoir = c(4,3), 
  Horror_Thriller = c(4,3), 
  Musical = c(4,3),
  Romance_Comedy = c(4,3), SciFi = c(4,3),
  War = c(4,3), Western = c(4,3)
)


rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min_2, data_2, data_2overallmean)

colors_border=c( rgb(0.2,0.8,0.5,0.9), rgb(0.9,0.6,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.8,0.5,0.3), rgb(0.9,0.6,0.5,0.3) , rgb(0.7,0.5,0.1,0.4) )

##From the radarchart, we can see that documentary fans are not fans of romance, scifi, thriller, action, horror, drama or childrens.
#They tend to like/rate higher Western, War, Animation and Film Noir(Fancy classic crime movies)
rownames(df) <- c("Max", "Min","Documentary Fans Rating", "Overall Avg for Genre")
radarchart(df, axistype = 1, pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, vlcex=2,
           caxislabels=seq(3,4,.25))

mtext(side = 3, line = 2.5, at = 0, cex = 2,"Documentary Fan Tendencies", font = 2)


legend(xjust = -.20, yjust = .5,x=0.7, y=1, legend = rownames(df[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.7, pt.cex=4)


####


##Fans of Romance


data_3overallmean = data %>% summarise(Action_Adventure = ((mean(Action_Mean) + mean(Adventure_Mean))/2),
                                       Animation = mean(Animation_Mean),
                                       Childrens = mean(Childrens_Mean),
                                       Documentary = mean(Documentary_Mean),
                                       FilmNoir = mean(Film_Noir_Mean),
                                       Horror_Thriller =( mean(Horror_Mean) +mean(Thriller_Mean))/2,
                                       Musical = mean(Musical_Mean),
                                       Romance_Comedy = (mean(Romance_Mean) + mean(Comedy_Mean))/2,
                                       SciFi = mean(Sci_Fi_Mean),
                                       War = mean(War_Mean))


data_3 = data %>% filter(countRomance >50) %>% summarise(Action_Adventure = ((mean(Action_Mean) + mean(Adventure_Mean))/2),
                                                         Animation = mean(Animation_Mean),
                                                         Childrens = mean(Childrens_Mean),
                                                         Documentary = mean(Documentary_Mean),
                                                         FilmNoir = mean(Film_Noir_Mean),
                                                         Horror_Thriller =( mean(Horror_Mean) +mean(Thriller_Mean))/2,
                                                         Musical = mean(Musical_Mean),
                                                         Romance_Comedy = (mean(Romance_Mean) + mean(Comedy_Mean))/2,
                                                         SciFi = mean(Sci_Fi_Mean),
                                                         War = mean(War_Mean))
#define max and min for all categories in order to put into radarchart()
max_min <- data.frame(
  Action_Adventure = c(4,3), 
  
  Animation = c(4,3),
  Childrens = c(4,3),
  
  Documentary = c(4,3), 
  
  FilmNoir = c(4,3), 
  Horror_Thriller = c(4,3), 
  Musical = c(4,3),
  Romance_Comedy = c(4,3), 
  SciFi = c(4,3), 
  War = c(4,3)
)

rownames(max_min) <- c("Max", "Min")
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.9,0.6,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.9,0.6,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# Bind the variable ranges to the data
df_3 <- rbind(max_min, data_3, data_3overallmean)
rownames(df_3) <- c("Max", "Min","Romance Fans Rating", "Overall Avg for Genre")

radarchart(df_3, axistype = 1, pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, vlcex=2,
           caxislabels=seq(3,4,.25))

mtext(side = 3, line = 2.5, at = 0, cex = 2,"Romance Fan Tendencies", font = 2)


legend(xjust = -.20, yjust = .5,x=0.7, y=1, legend = rownames(df_3[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.7, pt.cex=4)




###some analysis to prove:
#Evidence that movie ratings are biased: movie ratings are not representative of an objective judgement of art; rather they are at least partially, a result of how popular the genre is amongst the reviewers on the website.
data_4overallmeans = data %>% summarise(Action_Adventure = ((mean(Action_Mean) + mean(Adventure_Mean))/2),
                                        
                                        Animation = mean(Animation_Mean),
                                        Childrens = mean(Childrens_Mean),
                                        
                                        Documentary = mean(Documentary_Mean),
                                        FilmNoir = mean(Film_Noir_Mean),
                                        Horror = mean(Horror_Mean),
                                        Musical = mean(Musical_Mean),

                                        Romance_Comedy = ((mean(Romance_Mean) + mean(Comedy_Mean))/2),
                                        SciFi = mean(Sci_Fi_Mean))


##do same for horror/thriller fans 
data_4 = data %>% filter(countHorror >100 & countThriller >100) %>% summarise(Action_Adventure = ((mean(Action_Mean) + mean(Adventure_Mean))/2),
                                                                         
                                                                         Animation = mean(Animation_Mean),
                                                                         Childrens = mean(Childrens_Mean),
                                                       
                                                                         Documentary = mean(Documentary_Mean),
                                                                         FilmNoir = mean(Film_Noir_Mean),
                                                                         Horror = mean(Horror_Mean),
                                                                         Musical = mean(Musical_Mean),
                                                                         Romance_Comedy = ((mean(Romance_Mean) + mean(Comedy_Mean))/2),
                                                                         SciFi = mean(Sci_Fi_Mean))


#define max and min for all categories in order to put into radarchart()
max_min <- data.frame(
  Action_Adventure = c(4,3), 
  
  Animation = c(4,3),
  Childrens = c(4,3),
  
  Documentary = c(4,3), 

  FilmNoir = c(4,3), Horror = c(4,3), 
  Musical = c(4,3),
  Romance_Comedy = c(4,3), SciFi = c(4,3)
)

rownames(max_min) <- c("Max", "Min")
colors_border=c( rgb(0.2,0.3,0.5,0.9), rgb(0.9,0.6,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.3,0.5,0.4), rgb(0.9,0.6,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# Bind the variable ranges to the data
df_4 <- rbind(max_min, data_4, data_4overallmeans)
rownames(df_4) <- c("Max", "Min","Horror Fans Rating", "Overall Avg for Genre")

radarchart(df_4, axistype = 1, pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, vlcex=2,
           caxislabels=seq(3,4,.25))

mtext(side = 3, line = 2.5, at = 0, cex = 2,"Horror Fan Tendencies", font = 2)


legend(xjust = -.20, yjust = .5,x=0.7, y=1, legend = rownames(df_4[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.7, pt.cex=4)



