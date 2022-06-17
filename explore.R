source("eda.R")
library(GGally)
library(cowplot)
library(gridExtra)

# Organize data -----------------------------------------------------------

tbl <- read_csv("nfl_passing_plays_2021.csv") #load data

cont_var <- c("yards_gained", "wpa", "home_score", "air_yards", "yardline_100", "away_score", "yards_after_catch", "play_clock", "epa", "half_seconds_remaining") #all continous variables
cont <- select(tbl, cont_var) #create tibble of only continous variables

make_cat <- function(tbl, cont) {
  return(select(tbl, setdiff(colnames(tbl), colnames(cont))) %>% #create tibble of categorical variables by finding all variables not in continous tibble
           mutate_all(as.factor)) #convert all variables to categorical variables (factor)
}


cat <- make_cat(tbl, cont)

cont <- cont %>% mutate(score_diff=home_score-away_score) %>% select(-home_score, -away_score) #replace home and away score with the difference between them

cont_std <- as_tibble(scale(cont))

games <- unique(tbl["game_id"])[[1]][1:5]
tbl_game <- filter(tbl, game_id %in% games)
cont_game <- select(tbl_game, cont_var)
cat_game <- make_cat(tbl_game, cont_game)

good_qb <- filter(tbl, passer_player_name=="Aa.Rodgers")
bad_qb <- filter(tbl, passer_player_name=="B.Mayfield") %>% na.omit()#according to ESPN's quarterback ratings for 2021 season 

good_qb_cont <- select(good_qb, cont_var)
good_qb_cat <- make_cat(good_qb, good_qb_cont)

bad_qb_cont <- select(bad_qb, cont_var)
bad_qb_cat <- make_cat(bad_qb, bad_qb_cont)

sacks <- filter(tbl, sack==1)
sacks_cont <- select(sacks, cont_var) %>% mutate(score_diff=abs(home_score-away_score))
sacks_cat <- make_cat(sacks, sacks_cont)


# Explore data ------------------------------------------------------------

all_histograms(cont)
all_ecdf(cont)

#Observations: yards_after_catch and yards_gained are pretty right-skewed
#EPA, air_yards, and wpa (to an extent) look normally distributed
#Histograms don't seem to shw anything overly surprising

ggpairs(cont, 1:4, mapping=aes(alpha=0.5))
#plot against yardline_100 shows a distinct line that looks very much like a y=x line for yards_gained.
#possibly touchdown plays (because then yards_gained=yardline)

plot_all_color(cont, cat, "yardline_100", "touchdown")
#this plot shows a very clear distinction between touchdown and non-touchdown plays
#clustering can be implemented and then analysis of the clustering algorithm (perhaps its feature importance) 
#can be used to figure out which types of plays lead to touchdowns

ggpairs(cont, 5:9, mapping=aes(alpha=0.5)) 
#points with highest yards_after_catch seem to be when play_clock is small

plot_all(cont, "play_clock")
#outlier high values for epa, wpa, yards_gained, and air_yards (to an extent) also happen at low play clock values. 

explore_plots_color(cont, cat, 5, 9, "posteam")
#differences in play clock and score_diff distributions is most prominent for different teams

density_plot_color(cont, cat, "play_clock", "posteam")
density_plot_color(cont, cat, "play_clock", "down")

#these distributions show noticeable differences. 

explore_plots_color(cont_game, cat_game, 1,4, "down")
#seems to be distinct clusters for wpa and yards_gained in smaller dataset

plot(tbl, "yards_gained", "wpa", "down")
#cluster possibly exists in larger dataset

explore_plots_color(cont_game, cat_game, 5, 9, "down")
plot_all_color(cont_game, cat_game, "play_clock", "down")
#clusters possibly exist for play_clock vs yards_after_catch, air_yards, epa, and wpa


good <- density_plot_color(good_qb_cont, good_qb_cat, "yardline_100", "down")
bad <- density_plot_color(bad_qb_cont, bad_qb_cat, "yardline_100", "down")

plot_grid(good, bad, ncol=1, labels=c("Rodgers", "Mayfield"))
#density of yardline position by down is very different for Aaron Rodgers and Baker Mayfield
#the third down peak for Rodgers is far from the first and second down peaks but for Mayfield its very close
#can't generalize from one example but it makes sense that better quarterbacks would have shorter third downs and this example supports that idea
#suggests that the ability of a quarterback can be rated by analyzing yardline_data by down

explore_plots_color(sacks_cont, sacks_cat, 5,9,"down")
density_plot_color(sacks_cont, sacks_cat, "play_clock", "down")


# Explore if recievers are better in different areas of the field ---------
receivers <- group_by(tbl, receiver_player_name, yardline_100) #group by recievers and where the line of scrimmage is
yardline_data <- summarise(receivers, avg_yards=mean(yards_gained)) #find out the average yards gained by a receiver (when they get the ball) for all the different line of scrimmages they line up on 


avg_yards_by_yardline <- function(ind) { #this graph shows a visualization of the above idea for select recievers 
wr_name <- best_recievers[ind]
te_name <- best_te[ind]
wr_data <- filter(yardline_data, receiver_player_name==wr_name)
te_data <- filter(yardline_data, receiver_player_name==te_name)
ggplot(wr_data, aes(x=yardline_100, y=avg_yards)) + 
  geom_point(alpha=0.5, color="blue") + 
  geom_point(data=te_data, color="red", alpha=0.5) +
  ggtitle(glue::glue({wr_name}, " (blue) ", " + ", {te_name}, " (red) "))
}
#the idea is that maybe some recievers are more effective at different areas of the field. 
#so looking at the average yards gained from everywhere the reciever lines up can be used to identify where on the field they are more dangerous


best_recievers <- c("C.Kupp", "D.Adams", "D.Samuel", "J.Jefferson") #best according to PFF
best_te <- c("T.Kelce", "G.Kittle", "D.Waller", "M.Andrews") #best according to PFF
wr <- lapply(seq(1,4), avg_yards_by_yardline) 
plot_grid(plotlist = wr, ncol=2) #the profiles for receivers seem similar to each other, but very different from TEs

#this technique can be used to automatically identify recievers and tight ends from the data (positions are not provided)
#it can also be used to assess which recievers play more like tight ends and also where on the field specific players are the most dangerous



# Explore categorical variables -------------------------------------------
make_mosaic(cat, "pass_location", "pass_length")
#high Pearson residual for deep middle passes. Additionally, pass location and pass length seem to be dependent
make_stacked_histogram(cat, "pass_location", "pass_length")
#fewer deep passes in left or right (fewer passes middle overall)

make_mosaic(cat, "sack", "down")
#extremely high Pearson residual when comparing sack and down (as compared to other mosaics). 
#Difference between observed and expected sacks for third down is much more than expected

make_stacked_histogram(cat, "sack", "down")
#However, the distribution for sacks for third down isn't especially surprising (there are less 3rd downs than 1st, so it makes sense that they're are slightly less sacks on third down)

make_mosaic(cat, "pass_location", "interception")
make_stacked_histogram(cat, "pass_location", "interception")
#more interceptions seem to happen in the middle of the field (given less overall passes go there), but not significantly so


# Conclusions -------------------------------------------------------------
#Avenues to explore:
#Touchdown plays are very distinctive when looking at yardline data vs air yards, yards_gained, and yards_after catch
#This can be used to cluster to find touchdown plays and then can be used to find types of plays that lead to touchdowns

#The play clock habits of teams can possibly be extracted through clustering (could be used to figure out how long teams generally take to run plays)
#This can also be combined with data about downs, as play clock habits vary for different downs
#Additionally, some correlations could exist between play clock and: epa, wpa, yards_gained, and air_yards

#scatterplot of wpa and yards_gained possibly shows distinct clusters

#level of quarterback play could be evaluated by looking at yardline_data compared to downs

#type of reciever (wr vs te) could possibly automatically be identified from the data. can be used to figure out play styles of recievers and areas where they are more effectivew
