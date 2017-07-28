library(magrittr)
library(tidyverse)

get_ingredients <- function(pg)
{

  pag <- pg %>%
    paste0("https://www.wikipedia.org", .) %>%
    xml2::read_html()

  cap <- pag %>%
    rvest::html_nodes("caption") %>%
    rvest::html_text() %>%
    tolower()

  idx <- switch(pg,
    "/wiki/Fizz_(cocktail)#Ramos_Gin_Fizz" = which(cap == "ramos fizz"),
    "/wiki/Fizz_(cocktail)#Gin_Fizz"       = which(cap == "gin fizz"),
    "/wiki/Sour_(cocktail)#White_Lady"     = which(cap == "white lady"),
    "/wiki/Godfather_(cocktail)"           = which(cap == "the godfather"),
    "/wiki/Godfather_(cocktail)#Variations" = which(cap == "the godmother"),
    1
  )

  cat(cap[idx], "\n", sep = "")

  tabs <- pag %>%
    rvest::html_nodes("table.hrecipe") %>%
    `[`(idx) %>%
    rvest::html_table() %>%
    `[[`(1) %>%
    set_colnames(c("Title", "Details")) %>%
    as_tibble() %>%
    add_row(Title = "Name", Details = cap[idx]) %>%
    filter(!grepl("International", Title, fixed = TRUE))
}

all_data <- "https://en.wikipedia.org/wiki/List_of_IBA_official_cocktails" %>%
  xml2::read_html() %>%
  rvest::html_nodes("div.div-col ul li a") %>%
  rvest::html_attr("href") %>%
  grep(pattern = "^[^\\?]+$", value = TRUE) %>% # get rid of red link
  unique() %>% # the dirty vs. dry martini are almost exactly the same
  map(get_ingredients)

extract_element <- function(df, whch, split = NA, none = character(0))
{
  tmp <- df$Details[tolower(df$Title) == tolower(whch)]

  if(length(tmp) == 0) return(none)

  tmp %>%
    strsplit(split = split) %>%
    `[[`(1) %>%
    tolower()
}

together <- tibble(all_data = all_data) %>%
  mutate(
    name = map(all_data, extract_element, "Name"),
    ingredients = map(all_data, extract_element, "IBA specified ingredients*", split = "\n"),
    n_ingredients = map_int(ingredients, length),
    garnish = map(all_data, extract_element, "Standard garnish", split = "[\n,]|( and )|( or )"),
    n_garnish = map_int(garnish, length),
    glass = map_chr(all_data, extract_element, "Standard drinkware", none = ""),
    served = map_chr(all_data, extract_element, "Served", none = ""),
    preparation = map_chr(all_data, extract_element, "Preparation", none = "")
  )

tomatch <- c("^[\\d\\.,]+\\s?cl\\s?(\\(.*\\))?",
             "^.*\\s?(d|spl)ash(es)?",
             "^.*\\s?t(ea)?sp(oon)?s?",
             "^\\d+\\sdrops?",
             "^[\\d/]+\\sbar\\s?spoons?",
             "^\\d",
             "^one")

to_cl <- function(x, y)
{
  x <- gsub(",", ".", x, fixed = TRUE)
  case_when(
    y %in% c("pepper", "celery salt") ~ 0.01,
    x == "2 drops" | y== "tabasco" ~ 0.05,
    x %in% c("dash", "1 dash", "1/4 barspoon") ~ 0.1,
    x %in% c("a splash", "a splash", "splash", "two dashes", "few dashes", "2 to 3 dashes",
             "1/2 barspoon") | y == "soda water" | grepl("sugar syrup (acc", y, fixed = TRUE) ~ 0.25,
    x %in% c("1 teaspoon", "1 tsp") | y == "sugar cube" ~ 0.5,
    x == "two teaspoons" ~ 1,
    x == "1,5cl (1 part)" ~ 1.5,
    TRUE ~ as.numeric(stringr::str_extract(x, "^[\\d\\.]+"))
  )
}

clean_ingred <- function(y)
{
  y <- sub("^fresh(ly)? (squeezed )?|, fresh$", "", y)
  case_when(
    grepl("sugar syrup", y, fixed = TRUE) ~ "sugar syrup",
    y %in% c("water", "plain water") ~ "water",
    grepl("kahl.a", y) ~ "kahlua",
    grepl("^angostura", y) ~ "angosture bitters",
    grepl("^cr.me de cassis", y) ~ "creme de cassis",
    grepl("^egg white", y) ~ "egg white",

    TRUE ~ y
  )
}

together2 <- together %>%
  select(name, ingredients) %>%
  unnest(ingredients, .drop = FALSE) %>%
  mutate(
    name = map_chr(name, ~ .x),
    amount = stringr::str_extract(ingredients, paste0(tomatch, collapse = "|")),
    ingred = stringr::str_replace(ingredients, paste0(tomatch, "\\s*(of)?\\s*", collapse = "|"), ""),
    ingred = clean_ingred(ingred),
    cl = to_cl(amount, ingred)
  ) %>%
  select(-ingredients, -amount) %>%
  filter(!is.na(cl)) %>%
  spread(key = name, value = cl, fill = 0) %>%
  arrange(ingred) %>%
  as.data.frame() %>%
  column_to_rownames("ingred") %>%
  as.matrix()

together3 <- together2 %>%
  apply(2, function(x) x*10/sum(x)) %>%
  t()

classes <- together3 %>%
  kmeans(6) %>%
  fitted(method = "classes")

together3 %>%
  prcomp() %>%
  `[[`("x") %>%
  data.frame(name = colnames(together2), PC1 = .[, "PC1"], PC2 = .[, "PC2"], k.mean = classes) %>%
  (function(df)
    ggplot(df, aes(text = name, x = PC1, y = PC2, color = factor(k.mean))) +
    geom_point()
  )%>%
  ggplotly(tooltip = "text")



