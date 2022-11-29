# It turns out I can use rvest to get around the NYT paywall.
# When I look for a recipe:

mochi_stuffing_url <- "https://cooking.nytimes.com/recipes/1016889-mochi-rice-stuffing?action=click&module=Collection%20Page%20Recipe%20Card&region=Stuffing%20and%20Dressing%20Recipes%20for%20Thanksgiving&pgType=collection&rank=13"
mochi_page <- read_html(mochi_stuffing_url)
ingredients <- (mochi_page %>% html_elements("ul") %>% html_text)[3]
instructions <- (mochi_page %>% html_elements("ol") %>% html_text)
