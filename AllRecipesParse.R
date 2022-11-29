rm(list=ls())
source('00_functions.R') # pull in the functions that we know work,
                         # start up the session, and let us get to work

ex_url <- "https://www.allrecipes.com/recipe/143069/super-delicious-zuppa-toscana/"
ex_page <- get_page(ex_url)
ex_list <- get_ingredient_list(ex_page)
ex_df <- parse_ingredient_list(ex_list)

ex1 <- "https://www.allrecipes.com/recipe/153014/mini-orange-mince-pies/"
ex2 <- "https://www.allrecipes.com/recipe/215756/homemade-mince-pie-with-crumbly-topping/"
ex3 <- "https://www.allrecipes.com/recipe/282028/mince-pies/"

df1 <- get_page(ex1) %>% get_ingredient_list %>% parse_ingredient_list
df2 <- get_page(ex2) %>% get_ingredient_list %>% parse_ingredient_list
df3 <- get_page(ex3) %>% get_ingredient_list %>% parse_ingredient_list

x <- multi_recipe_compare(urls_vect)

# salt parser
spice_parse <- function(ingredient_list_item){

}

# search for a term, get multiple recipes from it, return ingredients, titles, and urls, and ratings
