library(tidyverse)
library(rvest)
library(polite)

# Units of measure
units_of_measure <- c(
  "tablespoon", "tablespoons", "teaspoon", "teaspoons",
  "cup", "cups", "slices", "slice",
  "pound", "ounce", "lb", "oz", "pounds",
  "ounces", "egg", "eggs", "egg,", "eggs,",
  "grams", "gram", "kilogram", "kilograms",
  "bunch", "bunches", "large", "medium", "small",
  "stalk", "stalks", "can", "cans", "container", "package", "packages",
  "box", "boxes"
)
# Set up a scraping session
session <- bow("https://www.allrecipes.com", force = TRUE)
# Download the page in rvest form
get_page <- function(url){
  nod(session, url) %>%
    scrape()
}
# Take the ingredient list and convert to a vector of strings
get_ingredient_list <- function(page){
    page %>%
    html_elements(".mntl-structured-ingredients__list") %>%
    html_text2 %>%
    strsplit("\n\n") %>%
    unlist
}
# Convert fractional units (e.g. quarter tsp) into numeric
convert_fraction <- function(num){
  if(!is.character(num)){ # convert numbers to characters
    num <- as.character(num)
  }
  ind <- utf8ToInt(num) %>% as.character # convert utf code
  if(length(ind) > 1){
    return(as.numeric(num))
  }
  fraction_codes <- list(
    "188" = 1/4, "189" = 1/2, "190" = 3/4, "8528" = 1/7, "8529" = 1/9,
    "8530" = 1/10, "8531" = 1/3, "8532" = 2/3, "8533" = 1/5, "8534" = 2/5,
    "8535" = 3/5, "8536" = 4/5, "8537" = 1/6, "8538" = 5/6, "8539" = 1/8,
    "8540" = 3/8, "8541" = 5/8, "8542" = 7/8
  )
  # return the result if its in fraction_codes, otherwise the original input
  ifelse(!is.null(fraction_codes[[ind]]),
         fraction_codes[[ind]],
         as.numeric(num))
}
# Take a list item and split into individual words/numbers
tokenize_list_item <- function(ingredient_list_item){
  strsplit(ingredient_list_item, " ") %>% unlist
}
# Take a list item and extract the quantity
get_quantity <- function(ingredient_list_item){
  tokens <- tokenize_list_item(ingredient_list_item)
  any_fractions <- "UTF-8" %in% sapply(tokens, function(x){
    as.character(x) %>% Encoding
  })
  if(any_fractions){
    # grab the fraction plus anything to the left
    first_frac <- which(sapply(tokens, function(x){ (as.character(x) %>% Encoding) == "UTF-8" }))
    left <- tokens[1:first_frac] %>% sapply(convert_fraction)
  } else {
    if(!any(as.numeric(tokens) %in% 0:10000)){
      left <- 0
    } else {
      left <- tokens[as.numeric(tokens) %in% 0:10000] %>% sapply(as.numeric)
    }
  }
  sum(left)
}
# Take a list item and extract the name of the ingredient
get_name <- function(ingredient_list_item){
  tokens <- tokenize_list_item(ingredient_list_item)
  unit_index <- which(tokens %in% units_of_measure)
  if(length(unit_index) < 1){
    unit_index <- 1
  }
  right <- tokens[(unit_index + 1):length(tokens)]
  right %>% paste(collapse = " ")
}
# Helpful function for ingredients that come in cans
can_parser <- function(ingredient_list_item){
  # If the ingredient_list_item has "can" or "cans" in it,
  # look for the size of the can in parentheses between the quantity and the
  # word "can"
  tokens <- tokenize_list_item(tolower(ingredient_list_item))
  if(!("can" %in% tokens | "cans" %in% tokens)){
    return(ingredient_list_item)
  }
  can_size <- str_extract(ingredient_list_item, "\\(.*\\)")
  unit_index <- which(tokens %in% units_of_measure)
  paste(can_size, tokens[unit_index])
}
#
get_unit <- function(ingredient_list_item){
  # check if can is there, if not, do code below, otherwise, return call to `can_parser`
  can_unit <- can_parser(ingredient_list_item)
  if(can_unit != ingredient_list_item){
    return(can_unit)
  }
  tokens <- tokenize_list_item(ingredient_list_item)
  unit_index <- which(tokens %in% units_of_measure)
  # Should also get anything to the left that isn't captured by get_quantity.
  tokens[unit_index] %>% paste(collapse = " ")
}

parse_ingredient_item <- function(ingredient_list_item){
  tibble(
    ingredient = get_name(ingredient_list_item),
    unit = get_unit(ingredient_list_item),
    quantity = get_quantity(ingredient_list_item)
  )
}

parse_ingredient_list <- function(ingredient_list){
  map_df(ingredient_list, parse_ingredient_item) %>%
    arrange(ingredient)
}

# stacks recipes' ingredients side by side
multi_recipe_compare <- function(urls_vect){
  pages <- lapply(urls_vect, get_page)
  titles <- lapply(pages, function(p){
    p %>% html_elements("title") %>% html_text()
  })
  ingredients <- lapply(pages, function(p){
    p %>% get_ingredient_list %>% parse_ingredient_list
  })
  out <- tibble(ingredient = character(),
                unit = character(),
                quantity = double())
  for(recipe in 1:length(urls_vect)){
    out <- full_join(out,
                     ingredients[[recipe]],
                     by="ingredient",
                     suffix = c("", paste0("_", titles[[recipe]])))
  }
  out %>% select(-unit, -quantity)
}

# recursively retrieve all recipes linked to from a page
get_from_list <- function(url){
  page <- get_page(url)
  links <- page %>%
    html_elements("a") %>%
    html_attr("href")
  link_pattern <- "https://www.allrecipes.com/recipe/[0-9]+"
  links_index <- links %>%
    grep(pattern=link_pattern)
  recipe_links <- links[links_index]
  pages <- lapply(recipe_links, get_page)
  recipes <- map_df(recipe_links, function(url){
    p <- get_page(url)
    title <- p %>%
      html_elements("title") %>%
      html_text()
    l <- get_ingredient_list(p)
    parse_ingredient_list(l) %>% mutate(title = title, url = url)
  })
  recipes
}

# search for a term
# find out why this isn't working
search_url <- function(search_term){
  search_term <- gsub(" ","+",search_term)
  paste0("https://www.allrecipes.com/search?q=",
    search_term)
}
# get a search page and get any recipes linked to on that page
search <- function(search_term){
  get_from_list(search_url(search_term))
}
