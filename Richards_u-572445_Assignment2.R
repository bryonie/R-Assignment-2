## Byronie Richards
##u-572445

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(stringr)
library(e1071)

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
  df <- mdf %>% filter(match(species,sdf$species) & rowSums(survey != 0))
  return(df)
}

get_allArea_species(mushrooms, survey) %>% glimpse()

df <- mushrooms %>% filter(species %in% survey$species && any(survey == "0"))
df %>% glimpse()

## Exercise 4 ------------------------------------------------------------------

reshape_mushroom <- function(mdf){
  df <- mushrooms %>% gather(mushroom_part, color_of_part, cap_color, 
  gill_color, spore_print_color, stalk_color, veil_color)

  return(df)
}

reshape_mushroom(mushrooms) %>% glimpse()

answer4 <- reshape_mushroom(mushrooms)
ggplot(answer4, aes(x = mushroom_part, fill = color_of_part)) + geom_bar() +
scale_fill_manual(values = c("white" = "white", "yellow" = "goldenrod",
          "pink" = "lightsalmon2", "buff" = "khaki", "brown" = "brown",
          "gray" = "gray", "black" = "black", "green" = "darkolivegreen4",
          "purple" = "mediumorchid4", "red" = "firebrick3", "cinnamon" = "tan",
          "orange" = "darkorange", "chocolate" = "chocolate"), guide = FALSE)
## Exercise 5 ------------------------------------------------------------------
raw_survey <- read.delim("./raw_survey.txt", skip=4, stringsAsFactors = FALSE)

get_new_survey <- function(rdf){
  df <- rdf %>% rename(
    species=X, january=X.1, february=X.2, march=X.3, april=X.4, may=X.5,
    june=X.6, july=X.7, august=X.8, september=X.9, october=X.10, 
    november=X.11, december=X.12
  ) %>% mutate_at(vars(-species), as.numeric) %>% 
  mutate_all(replace_na, replace=0)

  return(df)
}

# get_new_survey(raw_survey) %>% glimpse()

answer5 <- get_new_survey(raw_survey)
## Exercise 6 ------------------------------------------------------------------
species_model <- train(
  species ~.,
  method = "knn",
  trControl = trainControl(method="cv", number=3),
  data=mushrooms
)

species_model

summary(species_model)

answer6 <- species_model

## Exercise 7 ------------------------------------------------------------------

fit_bruise_model <- function(mdf){
  df <- mdf %>%
  mutate(odor = odor, ref="none")

  set.seed(1) 
  bruises_model <- train(
  bruises ~ (cap_color + odor),
  method = "glm",
  data = df,
  trControl = trainControl(method = 'cv', number = 3),
  family = binomial(link = "logit")
  )

  return(bruises_model)

}

bruise_model <- fit_bruise_model(mushrooms)
summary(bruise_model)

answer7 <- bruise_model
## Exercise 8 ------------------------------------------------------------------
edibile_survey <- function(sdf, edf){
  df <- sdf %>% 
  mutate(edibility = edf$edibility[match(species,edf$species)]) %>%
  filter(edibility == "edible") %>%
  gather(month, count, january:december) %>% 
  mutate(month = paste(toupper(substring(month,1,1)),
  substring(month, 2,3), sep="")) %>%
  mutate(month = match(month, month.abb))
  
  return(df)
}

# edibile_survey(survey, edibility) %>% glimpse()

answer6 <- edibile_survey(survey, edibility)

ggplot(answer6, aes(x=month, y=count, 
group=species, color=species)) + geom_line()

## Exercise 9 ------------------------------------------------------------------
 new_mush <- function(mdf){
   df <- mushrooms %>%
   mutate_at(vars(contains('cap')), funs(paste(.,".cap", sep=""))) %>%
   mutate_at(vars(contains('gill')), funs(paste(.,".gills", sep=""))) %>%
   mutate_at(vars(contains('stalk')), funs(paste(.,".stalk", sep=""))) %>%
   mutate_all(toupper) %>%
   rename_all(toupper) %>%
   rename_all(funs(str_replace(., "_", ".")))

   return(df)
 }

 new_mush(mushrooms) %>% glimpse()

 answer9 <- new_mush(mushrooms)
## Exercise 10 ------------------------------------------------------------------
train_test_split <- function(df, P){
  set.seed(1)
  trn_index = createDataPartition(y = df$edibility, p = P, list = FALSE)
  trn_df = df[trn_index, ]
  tst_df = df[-trn_index, ]

  return(list("trn"=trn_df, "tst"=tst_df))
}

edible <- train_test_split(answer3_a,0.8)

fit_model <- function(data){
  set.seed(1)
  model <- train(
    edibility ~.,
    method = "knn",
    trControl = trainControl(
      method="cv", 
      number=5, 
      classProbs = TRUE, 
      summaryFunction = prSummary
    ),
    data = data,
    metric = "Recall"
  )

  return(model)

}

edible_model <- fit_model(edible$trn)

summary(edible_model)

model_predict <- function(model,data){
  set.seed(1)
  predictions <- predict(model, data)
  return(predictions)
}

predict_edible <- model_predict(edible_model, edible$tst)

generate_accuracy <- function(pred, data){
  accuracy <- sum(pred == data$edibility) /length(data$edibility)
  accuracy
}

generate_accuracy(predict_edible, edible$tst)

get_matrix <- function(data, pred){

  data$edibility <- factor(data$edibility)
  confM <- confusionMatrix(pred, data$edibility)
  return(confM)
}

edible_matrix <- get_matrix(edible$tst, predict_edible)

get_matrix_classes <- function(matrix){
  return(list(
    "precision" = edible_matrix$byClass["Precision"],
    "recall" = edible_matrix$byClass["Recall"]
  ))
}

answer10 <- get_matrix_classes(edible_matrix)