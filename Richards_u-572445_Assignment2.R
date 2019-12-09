## Byronie Richards
##u-572445

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

## Load data -------------------------------------------------------------------
#dataDir <- "./input/"
mushrooms <- read.delim ("./mushrooms.txt", stringsAsFactors = FALSE)
edibility <- read.delim("./edibility.txt", stringsAsFactors = FALSE)
survey <- read.delim("./clean_survey.txt", stringsAsFactors = FALSE)

## Exercise 1 ------------------------------------------------------------------

habitat_number <- function(input_df, habitat) {
  count = length(input_df$habitat[input_df$habitat == habitat])
  return(count)
}

# habitat_number(mushrooms, "grasses")

answer1 <-habitat_number(mushrooms, "leaves")

## Exercise 2 ------------------------------------------------------------------
mushroom_whiteparts <- function(df) {
  df <- df %>% mutate(white_parts = rowSums(df[-1] == "white"))
  return(df)
}

# mushroom_whiteparts(mushrooms) %>% glimpse()
whitepart_mushrooms = mushroom_whiteparts(mushrooms)

answer2 <- whitepart_mushrooms

## Exercise 3 ------------------------------------------------------------------

get_edibility <- function(mdf, edf){
  df <- mdf %>% mutate(edibility = edf$edibility[match(species,edf$species)])
  return(df)
}

# get_edibility(mushrooms, edibility) %>% glimpse()
mushroom_edibility <- get_edibility(mushrooms, edibility)

answer3_a <- mushroom_edibility

get_allArea_species <- function(mdf, sdf){
  df <- mdf %>% filter(match(species,sdf$species) && rowSums(survey != 0))
  return(df)
}

get_allArea_species(mushrooms, survey) %>% glimpse()

df <- mushrooms %>% filter(species %in% survey$species && any(survey == "0"))
df %>% glimpse()

## Exercise 4 ------------------------------------------------------------------
ggplot(answer4, aes(x = mushroom_part, fill = color_of_part)) + geom_bar() +
  scale_fill_manual(values = c("white" = "white", "yellow" = "goldenrod",
                               "pink" = "lightsalmon2", "buff" = "khaki", "brown" = "brown",
                               "gray" = "gray", "black" = "black", "green" = "darkolivegreen4",
                               "purple" = "mediumorchid4", "red" = "firebrick3", "cinnamon" = "tan",
                               "orange" = "darkorange", "chocolate" = "chocolate"), guide = FALSE)
## Exercise 5 ------------------------------------------------------------------
## Exercise 6 ------------------------------------------------------------------
## Exercise 7 ------------------------------------------------------------------
## Exercise 8 ------------------------------------------------------------------
## Exercise 9 ------------------------------------------------------------------
## Exercise 10 ------------------------------------------------------------------
edible_shrooms <- sample(edibility$species, 4)
backup_shrooms <- mushrooms
backup_shrooms$edibility = "poisonous" backup_shrooms$edibility[backup_shrooms$species %in% edible_shrooms] <- "edible"
