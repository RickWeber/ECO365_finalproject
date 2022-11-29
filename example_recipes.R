


# example URLs

front_page <- get_page("https://www.allrecipes.com")
front_page_links <- front_page %>%
  html_elements("a") %>% html_attr("href")
recipe_link_pattern <- "https://www.allrecipes.com/recipe/[0-9]+"
recipe_links_index <- front_page_links %>%
  grep(pattern=recipe_link_pattern)
recipe_links <- front_page_links[recipe_links_index]

recipe_list_pattern <- "https://www.allrecipes.com/recipes/[0-9]+"
recipe_list_index <- front_page_links %>%
  grep(pattern=recipe_list_pattern)
recipe_lists <- front_page_links[recipe_list_index]


pages <- lapply(recipe_links, get_page)
example_recipes <- map_df(pages, function(p){
  title <- p %>%
    html_elements("title") %>%
    html_text()
  l <- get_ingredient_list(p)
  ingredients <- parse_ingredient_list(l)
  tibble(title = title,
         ingredients = nest(ingredients))
})
colnames(example_recipes)[2] <- "ingredients"

example_recipes %>% unnest(cols=c(data))
