library(tidyverse)
library(DT)
library(lubridate)
library(plotly)
library(stringr)
library(mice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library(party)
library(MASS)
library(ranger)
library(randomForest)


# Load and first trim of my data --------------------


#unused data for now
#reviews<- read.csv("reviews.csv")
#calendar<- read.csv("calendar.csv")


# Reading data
listings<- read.csv("listings.csv")


# Keeping variable who seems relevant to speed up the process

#listings_short<- listings[,c(1,4,9,15,16,17,18,22,26,28,32:39,40, 53,54,56,57,61:67,74)]


# second skimming 

listings_model<- listings[,c(1,28,32:34,36:40)]

# first look deep at trimed data
#summary(listings_model)

# here im alternating variable to look deeper into them 
# okok<- listings_model %>%
#   group_by( property_type)%>%
#   summarise(n=n())




listings_model$id <- as.character(listings_model$id)




# Cleaning my data --------------------

#cleaning the $ format
listings_model$price <- as.numeric(gsub('[$,]', '', listings_model$price))

#cleaning bathrooms_text

listings_model$bathrooms_text<- ifelse(listings_model$bathrooms_text == "Half-bath", 0.5, listings_model$bathrooms_text)

# add the shared part of bathroom text to the room type 
listings_model$room_type<- ifelse(grepl("shared", listings_model$bathrooms_text), paste(listings_model$room_type, '+ shared bath' ), listings_model$room_type)

listings_model$bathrooms_text<- str_replace_all(listings_model$bathrooms_text, 'shared ', '')
listings_model$bathrooms_text<- str_replace_all(listings_model$bathrooms_text, 'private ', '')
listings_model$bathrooms_text<- str_replace_all(listings_model$bathrooms_text, ' baths', '')
listings_model$bathrooms_text<- str_replace_all(listings_model$bathrooms_text, ' bath', '')
listings_model$bathrooms_text<- as.numeric(listings_model$bathrooms_text) 


#cleaning 2 last NA in the dataset (not an easy question cause both NA are probably at least one but for the futur a larger rules is better)

listings_model$bathrooms_text<- ifelse(is.na(listings_model$bathrooms_text) & listings_model$room_type== 'Hotel room', 1,
                                       listings_model$bathrooms_text)


#summary(is.na(listings_model$bathrooms_text) & listings_model$bathrooms_text== 'Hotel room')

# trying to get the most out of the NA 

# since we dont have a number i assumed thats all bed are for 2 
listings_model$beds<- ifelse(is.na(listings_model$beds) , round(listings_model$accommodates/2), listings_model$beds)


# I assumed here that you can have a bed without a bedroom like in a loft
listings_model$bedrooms<- ifelse(is.na(listings_model$bedrooms), 0, listings_model$bedrooms)







# cleaning amenities

# looking what is in the amenities to try put them in column 
  # bdy2 <- paste(listings_model$amenities, collapse=",")
  # 
  # bdy2<- str_replace_all(bdy2, '\\"|\\"', '')
  # 
  # bdy2<- str_replace_all(bdy2, '\\[|\\]', '')
  # 
  # out<- read.table(text = bdy2, sep=",", header = FALSE, col.names = 'amenities')
  # 
  # test<- out %>%
  #   mutate( amenities= tolower(amenities))%>%
  #   group_by(amenities) %>%
  #   summarise( n= n())


######### things i wanna know
# parking (free parking) (free street parking) !
# Wifi                                         !
# kitchen                                      !
# refrigerator                                 ! trop d'incertitude avec cette variable
# free washer and washer(solo)                 !
# oven                                         ! trop d'incertitude avec cette variable
# stove                                        ! trop d'incertitude avec cette variable
# patio or balcony                             !                   
# hot tub                                      !  
# pool                                         !  
#########

#clean to be able to do regex
listings_model$amenities<- tolower(listings_model$amenities)

listings_model$amenities<- str_replace_all(listings_model$amenities, '\\"|\\"', '')

listings_model$amenities<- str_replace_all(listings_model$amenities, '\\[|\\]', '')




# create the Parking column 

##############################
# 0 = No free parking        # 
# 1 = free street parking    #
# 2 = free dedicated parking #
##############################

listings_model$Parking<- 0

listings_model$Parking<- ifelse(grepl("free street parking",listings_model$amenities), 1, listings_model$Parking)

listings_model$Parking<- ifelse(grepl("free parking",listings_model$amenities) | grepl("free driveway parking",listings_model$amenities),
                                2, listings_model$Parking)

listings_model$Parking <- as.character(listings_model$Parking)



# create the wifi column 
listings_model$wifi<- 0
listings_model$wifi<- ifelse(grepl("wifi",listings_model$amenities), 1, 0) 


# create the kitchen column 
listings_model$kitchen<- 0
listings_model$kitchen<- ifelse(grepl("kitchen",listings_model$amenities), 1, 0)



# create the washer column  note: little problem with dishwasher counting as washer resolved by putting a space and a beggining of line
listings_model$washer<- 0
listings_model$washer<- ifelse(grepl(" washer",listings_model$amenities) & !grepl("paid washer",listings_model$amenities), 1, 0)
listings_model$washer<- ifelse(grepl("^washer",listings_model$amenities) & !grepl("paid washer",listings_model$amenities), 1, listings_model$washer)



# create the patio or balcony column 

listings_model$patio<- 0
listings_model$patio<- ifelse(grepl("patio or balcony",listings_model$amenities), 1, 0)



# create the hot tub column 
listings_model$hot_tub<- 0
listings_model$hot_tub<- ifelse(grepl("hot tub",listings_model$amenities), 1, 0)



# create the pool column 
listings_model$pool<- 0
listings_model$pool<- ifelse(grepl(" pool,",listings_model$amenities), 1, 0)
listings_model$pool<- ifelse(grepl("^pool",listings_model$amenities), 1, listings_model$pool)



# # create the refrigerator column 
# listings_model$refrigerator<- 0
# listings_model$refrigerator<- ifelse(grepl("refrigerator",listings_model$amenities), 1, 0)
# 
# 
# 
# # create the stove column 
# listings_model$stove<- 0
# listings_model$stove<- ifelse(grepl("stove",listings_model$amenities), 1, 0)
# 
# 
# 
# # create the oven column 
# listings_model$oven<- 0
# listings_model$oven<- ifelse(grepl("oven",listings_model$amenities), 1, 0)



# create the dedicated workspace column 
listings_model$dedicated_workspace<- 0
listings_model$dedicated_workspace<- ifelse(grepl("dedicated workspace",listings_model$amenities), 1, 0)



# okok<- listings_model %>%
#   filter(kitchen == 0)%>%
#   group_by( stove)%>%
#   summarise(n=n()) 


# Creating my train and my test dataset ------------------------- 

#removing amenities
listings_model<- listings_model %>%
  dplyr::select(-amenities, -property_type) 


myControl2 <- trainControl(
  method = "cv", 
  number = 10,
  savePredictions = 'final',
  verboseIter = TRUE
)


ind_rand2 <- sample(listings_model$id) 

prop <- 0.80


donsub_train <- listings_model %>% 
  filter(id %in% ind_rand2[1:round(length(ind_rand2)*prop)]) 


donsub_test <- listings_model %>% 
  filter(id %in% ind_rand2[round(length(ind_rand2)*prop)+1:length(ind_rand2)])



# Creating my regression -------------------------



data_pour_reg <- listings_model %>%
  dplyr::select(-id) 


mod1 <- lm(price~., data = data_pour_reg)

summary(mod1)

#plot(mod1)



# Training my random forest -------------------------



modelsub <- train(
  price~., 
  data = donsub_train,  
  method = 'ranger', 
  trControl = myControl2
)

sub_train <- predict(modelsub, donsub_test)

donsub_test$sub_train <- sub_train

donsub_test <- donsub_test %>%
  mutate(range = ifelse(abs(price-sub_train)<0.15*price,1,0))

plot(sub_train, donsub_test$price)










