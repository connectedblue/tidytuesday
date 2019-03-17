library(tidyverse)

board_games_raw <- read_csv('board_games.csv')

# Investigate number of games published each year

p <- ggplot(better_than_average_board_games , aes(x=year_published)) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(paste0('Top ', top_n_categories, ' categories of board games with better than average rating')) +
  xlab('Year published') + ylab('')

print(p)





# The category column has a variable number of comma separated values
# split the category column into the maximum number of values there can be
# and label each one catN.  Melt the data back into a single column called category

cat_columns <- paste0('cat', 
                      seq_len(max(sapply(strsplit(board_games_raw$category, ','), length))))

top_n_categories <- 10


# find the better than average rated board games in the top categories

better_than_average_board_games <- board_games_raw %>% 
               separate(category, into=cat_columns, sep=',') %>%
               gather(key='cat_num', value='category', cat_columns, na.rm = TRUE) %>%
               select(-cat_num) %>%
               mutate(top_cat=fct_lump(category, top_n_categories )) %>%
               group_by(top_cat) %>%
               mutate(weighted_category_weighting=sum(users_rated*average_rating)/sum(users_rated)) %>%
               select(-category) %>%
               filter(top_cat!='Other', average_rating>weighted_category_weighting) %>%
               spread(top_cat, weighted_category_weighting)

# # plot the frequency of each category
# p <- ggplot(board_games, aes(x=fct_rev(fct_infreq(top_cat)))) +
#         geom_bar(stat = "count") +
#         theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#         ggtitle(paste0('Top ', top_n_categories, ' categories of board games')) +
#         xlab('') + ylab('')
# 
# print(p)



# plot the frequency of popular games by year
p <- ggplot(better_than_average_board_games , aes(x=year_published)) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(paste0('Top ', top_n_categories, ' categories of board games with better than average rating')) +
  xlab('Year published') + ylab('')

print(p)


# weighted rating by year
average_ratings <- function(data, title) {
  weighted_ratings_by_year <- data  %>% 
    group_by(year_published) %>%
    mutate(weighted_year_weighting=sum(users_rated*average_rating)/sum(users_rated)) %>%
    filter(row_number()==1) %>%
    ungroup()
  
  # plot the frequency of popular games by year
  p <- ggplot(weighted_ratings_by_year , aes(x=year_published, y=weighted_year_weighting)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(title) +
    xlab('Year published') + ylab('')
  
  p
}


paste0('Top ', top_n_categories, ' categories of board games with better than average rating')

print(average_ratings(board_games_raw, 'Average rating across all games'))

print(average_ratings(better_than_average_board_games, 'Average rating for better than average board games'))





