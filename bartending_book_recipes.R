library(magrittr)
library(tidyverse)

#hi

carrydown <- function(x)
{
  for(i in 2:length(x))
  {
    if(is.na(x[i]) || x[i] == "") x[i] <- x[i-1]
  }
  x
}

`%nin%` <- Negate(`%in%`)

recipes <- read_csv("recipes.csv", col_names = TRUE, col_types = cols()) %>%
  mutate(
    Chapter = carrydown(Chapter),
    Family = carrydown(Family),
    Name = carrydown(Name),
    Notes = ifelse(is.na(Notes), "ingredient", Notes)
  ) %>%
  nest(Amount, Ingredient, Notes) %>%
  mutate(
    Ingredients = map(data, function(df) filter(df, Notes == "ingredient")),
    Glass = map_chr(data, function(df) df$Ingredient[df$Notes == "glass"]),
    Ice = map_lgl(data, function(df) "ice" %in% df$Notes),
    Shaken = map_lgl(data, function(df) "shake" %in% df$Notes),
    Muddled = map_lgl(data, function(df) "muddle" %in% df$Notes),
    Blended = map_lgl(data, function(df) "blend" %in% df$Notes),
    Garnish = map(data, function(df) filter(df, Notes %in% "garnish"))
  )

to_oz <- function(x, y)
{
  mult <- c(
    "diced lemon" = 2,
    lemon = 2,
    lime = 1,
    "lime wedge" = 1/8,
    blueberries = 1/8,
    "maraschino cherries" = 1/6,
    raspberries = 1/6,
    "egg white" = 1,
    "1-inch melon chunks" = 0.5,
    strawberries = 0.75,
    "Oreo cookies" = 0.75,
    banana = 6,
    peach = 8,
    "mint leaves" = 0.03,
    "orange slice" = 0.5,
    "pineapple ring" = 4
  )
  x[x == "3 or 4"] <- "4"
  x[x == "12 to 15"] <- "15"
  case_when(
    x == "2 shakes" & y %in% c("salt", "pepper") ~ 0.02,
    x == "2 shakes" & y %in% c("Tabasco sauce", "Tabasco's Habanero sauce", "Worcestershire sauce") ~ 0.04,
    x == "2 dashes" ~ 0.0625,
    x == "3 dashes" ~ 0.09375,
    x == "4 dashes" ~ 0.125,
    x == "1 tsp" ~ 1/6,
    x == "0.5 tsp" ~ 1/12,
    x %in% c("1 cup", "2 scoops") ~ 8,
    !grepl(" oz$", x) & y %nin% names(mult) ~ NA_real_,
    TRUE ~ as.numeric(sub("^([0-9.]+).*", "\\1", x)) * ifelse(y %in% names(mult), mult[y], 1)
  )
}

amounts <- recipes %>%
  select(Name, Ingredients) %>%
  unnest(Ingredients) %>%
  mutate(oz = to_oz(Amount, Ingredient)) %>%
  select(Name, Ingredient, oz) %>%
  (function(x) {
    if(anyNA(x$oz)) stop("Missing oz's")
    x
  }) %>% 
  spread(key = Name, value = oz, fill = 0) %>%
  arrange(Ingredient) %>%
  as.data.frame() %>%
  column_to_rownames("Ingredient") %>%
  as.matrix() %>%
  "["(1:nrow(.), recipes$Name) # to get things in the correct order


together3 <- amounts %>%
  apply(2, function(x) x*5/sum(x)) %>%
  t()


recipes$k.means <- together3 %>%
  (function(x) {set.seed(88); x}) %>%
  kmeans(14) %>%
  fitted(method = "classes")

# library(plotly)
# together3 %>%
#   prcomp() %>%
#   `[[`("x") %>%
#   data.frame(name = colnames(amounts), PC1 = .[, "PC1"], PC2 = .[, "PC2"],
#              k.mean = recipes$k.means, family = recipes$Family, chapter = recipes$Chapter) %>%
#   (function(df)
#     ggplot(df, aes(text = paste0(name, "<br>", chapter, "<br>", family),
#                    x = PC1, y = PC2, color = factor(k.mean))) +
#      geom_point() +
#      theme(legend.position = 'none')
#   ) %>%
#   ggplotly(tooltip = "text")




find_drinks_by_ingredients <- function(ingredients, recipes)
{
  recipes %>%
    mutate(
      still.need = map(recipes$Ingredients, function(df) df$Ingredient[df$Ingredient %nin% ingredients]),
      n.need = map_int(still.need, length)
    ) %>%
    arrange(n.need) %>%
    select(Chapter, Family, Name, data, still.need, n.need)
}

