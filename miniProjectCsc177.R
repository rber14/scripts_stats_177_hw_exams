library(ggplot2)
library(plyr)
load('//sacfiles1/home/r/robertocampos/Desktop/movies.Rdata')
View(movies)

#copy movies data frame into a new variable
mdf = movies

#remove unwanted columns
mdf = movies[-c(2,4, 7:12, 24:32)]
#VIEW the new movie data frame (mdf) with the desired columns - we should only have 15 col 
# to do further data analysis
View(mdf)
awards = mdf[c(11:15)]
View(awards)

#trying to add horizonatally
#change yes to 1 and no to 0
awards$best_pic_nom = revalue(awards$best_pic_nom, c('no'=0))
awards$best_pic_nom = revalue(awards$best_pic_nom, c('yes'=1))
awards$best_pic_win = revalue(awards$best_pic_win, c('no'=0))
awards$best_pic_win = revalue(awards$best_pic_win, c('yes'=1))
awards$best_actor_win = revalue(awards$best_actor_win, c('no'=0))
awards$best_actor_win = revalue(awards$best_actor_win, c('yes'=1))
awards$best_actress_win = revalue(awards$best_actress_win, c('no'=0))
awards$best_actress_win= revalue(awards$best_actress_win, c('yes'=1))
awards$best_dir_win = revalue(awards$best_dir_win, c('no'=0))
awards$best_dir_win= revalue(awards$best_dir_win, c('yes'=1))

for(i in 1:nrow(awards))
{
  row = awards[i,]
  print(row)
}


plot = ggplot(mdf, aes(mdf$mpaa_rating, mdf$imdb_rating)) + geom_point()+ geom_smooth(method=lm)
plot2 = ggplot(mdf, aes(mdf$genre, mdf$imdb_rating)) + geom_point()
plot3 = ggplot(mdf, aes(mdf$best_dir_win, mdf$imdb_rating)) + geom_point() 
plot4 = ggplot(mdf, aes(mdf$best_actress_win, mdf$imdb_rating)) + geom_point()
show(plot)
show(plot2)
show(plot3)
show(plot4)

his = hist(movies$audience_score)
show(his)

a = ggpairs(movies, columns = 2:3)
show(a)
