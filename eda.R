library(tidyverse)
library(GGally)
library(ggridges)

all_histograms <- function(cont) {
  ggplot(gather(cont), aes(value)) + 
    geom_histogram(bins = 30) + 
    facet_wrap(~key, scales = 'free_x')
}

make_ecdf <- function(tbl, colname) { #make ecdf given data and continous variable
  ggplot(tbl, aes_string(x=colname)) + 
    stat_ecdf() +
    theme_bw()
}


all_ecdf <- function(tbl) { #make ecdf for every variable in data
  ggplot(gather(tbl), aes(value)) + 
    stat_ecdf() + 
    geom_rug(alpha=0.7) +
    facet_wrap(~key, scales = 'free_x') +
    theme_bw()
}

density_plot_color <- function(cont, cat, cont_var, cat_var) { #make density plot for continous variable and color it by categorical variable
  tbl <- bind_cols(cont, cat)
  ggplot(tbl, aes_string(x=cont_var, fill=cat_var, alpha="0.8")) +
    geom_density()
}

make_mosaic <- function(tbl, col1, col2) {
  mosaicplot(table(tbl[col1][[1]], tbl[col2][[1]]), shade=TRUE, main=glue::glue("Relationship between {col1} and {col2}"))
} #make a mosaic plot and shade it with Pearson residual

make_stacked_histogram <- function(tbl, col1, col2, side=TRUE) {
  tbl <- na.omit(tbl)
  ggplot(tbl, aes_string(x=col1, fill=col2)) + 
    theme_bw() +
    if(side){
      geom_bar(position="dodge")
    } else {
      geom_bar()
    }
} #stacked histogram (either side by side or stacked on top, depending on boolean side)

explore_plots_color <- function(cont, cat, col1, col2, colvar) { #scatterplot matrix for specified columns of continous data colored by categorical variable
  tbl <- bind_cols(cont, cat)
  ggpairs(tbl, columns=col1:col2, aes_string(colour=colvar, alpha="0.5"))
}

plot <- function(tbl, x, y, col) { #create basic ggplot colored by categorical variable
ggplot(tbl, aes_string(x=x, y=y, color=col, alpha="0.7")) +
  geom_point() +
  theme_bw() 
}

plot_all_color <- function(cont, cat, cont_var, cat_var) { #plot one continous variable against all others in data and color by categorical variable
  cont %>%
    mutate(color=cat[cat_var][[1]]) %>%
    gather(-cont_var, -color, key = "var", value = "value") %>% 
    ggplot(aes_string(x = "value", y = cont_var, color="color", alpha="0.5")) +
    geom_point() +
    facet_wrap(~ var, scales = "free") +
    theme_bw()
}

plot_all <- function(tbl, cont_var) { #plot one continous variable against all others in data
  tbl %>%
    gather(-cont_var, key = "var", value = "value") %>% 
    ggplot(aes_string(x = "value", y = cont_var, alpha="0.6")) +
    geom_point() +
    facet_wrap(~ var, scales = "free") +
    theme_bw()
}
