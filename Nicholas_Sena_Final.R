pokemon <- read.csv('pokemon.csv', stringsAsFactors = TRUE)
#Look at the data set and view its variable types
View(pokemon)
str(pokemon)
#Look at statistics of data set
summary(pokemon)

#Make sure no empty rows (No elements in Type.2 column is ok)
sum(is.na(pokemon))

#We want to predict if a pokemon is a legendary or not

pokemon$isLegendary <- ifelse(pokemon$Legendary=='True',1,0)

table(pokemon$Generation)
table(pokemon$Type.1)
#A classification problem
#Create a model based on the BST (base stat total) and types of Pokemon
pokemon_model <- lm(isLegendary ~ Type.1 + Type.2 + Total + HP + Attack + Defense + Sp..Atk + Sp..Def + Speed + Generation, data = pokemon)

#Summary shows that the most important columns to determine if a Pokemon is a legendary are
#the Pokemon's base stat total, the special attack stat, and the first type of the Pokemon.
#Pokemon are more likely to be legendary if their first type is dragon, flying or psychic
#The adjusted R-squared is 0.2836
summary(pokemon_model)

pokemon_stat_model <- lm(Total ~ HP + Attack + Defense + Sp..Atk + Sp..Def + Speed, data = pokemon)
summary(pokemon_stat_model)
# Split data into training and testing randomly, 80% training and 20% testing
sample_size <- floor(0.8*nrow(pokemon))
set.seed(777)

# randomly split data in r
picked = sample(seq_len(nrow(pokemon)),size = sample_size)
pokemon_train <- pokemon[picked,]
pokemon_test <- pokemon[-picked,]

View(pokemon_test)

pokemon_logistic <- glm(isLegendary ~ Type.1 + Type.2 + Total + HP + Attack + Defense + Sp..Atk + Sp..Def + Speed + Generation, data = pokemon_train)
summary(pokemon_logistic) # In the train set, the type was mostly determined by the pokemon's first type
#In this case, the types that were the most important are Dragon, Flying, and psychic
#The second types were also a factor, specifically the dark and ground types
#The base stat total was not a factor, but the special attack stat was

pokemon_logistic_test <- glm(isLegendary ~ Type.1 + Type.2 + Total + HP + Attack + Defense + Sp..Atk + Sp..Def + Speed + Generation, data = pokemon_test)
summary(pokemon_logistic_test)
#In the test set, the base stat total was the biggest factor
#The individual defense and special defense stats were a factor as well
#The first type of the Pokemon was a factor

legendary_prob <- predict(pokemon_logistic,pokemon_test,type="response")

table(legendary_prob, pokemon_test$isLegendary)

# Clustering of Pokemon of different types -> BSTs

#KNN 

round(prop.table(table(pokemon$Total))*100,digits = 1)

round(prop.table(table(pokemon$Type.1))*100,digits = 1)

pokemon_train2 <- pokemon[1:600,5:11]
pokemon_test2 <-  pokemon[601:800,5:11]

pokemon_train_labels2 <- pokemon[1:600,13]
pokemon_test_labels2 <- pokemon[601:800,13]
library(class)
library(gmodels)

View(pokemon_test2)

pokemontypes_test_pred <- knn(train=pokemon_train2, test=pokemon_test2, cl=pokemon_train_labels2, k=10)
summary(pokemontypes_test_pred) #0 is not legendary, 1 is legendary
CrossTable(x=pokemon_test_labels2, y=pokemontypes_test_pred)

#Visualize stats of Pokemon
hist(pokemon$HP, main='HP distribution of all Pokemon', xlab='HP')
boxplot(pokemon$HP, main='HP distribution of all Pokemon', xlab='HP')
hist(pokemon$Attack, main='Attack distribution of all Pokemon', xlab='Attack')
boxplot(pokemon$Attack, main='Attack distribution of all Pokemon', xlab='Attack')
hist(pokemon$Defense, main='Defense distribution of all Pokemon', xlab='Defense')
boxplot(pokemon$Defense, main='Defense distribution of all Pokemon', xlab='Defense')
hist(pokemon$Sp..Atk, main='Special Attack distribution of all Pokemon', xlab='Special Attack')
boxplot(pokemon$Sp..Atk, main='Special Attack distribution of all Pokemon', xlab='Defense')
hist(pokemon$Sp..Def, main='Special Defense distribution of all Pokemon', xlab='Special Defense')
boxplot(pokemon$Sp..Def, main='Special Defense distribution of all Pokemon', xlab='Defense')
hist(pokemon$Speed, main='Speed distribution of all Pokemon', xlab='Speed')
boxplot(pokemon$Speed, main='Speed distribution of all Pokemon', xlab='Speed')

hist(pokemon$Total, main='BST of all Pokemon', xlab='Base Stat Total')

mean(pokemon$HP)
mean(pokemon$Attack)
mean(pokemon$Sp..Atk)
mean(pokemon$Defense)
mean(pokemon$Sp..Def)
mean(pokemon$Speed)

boxplot(Total ~ Generation, data = pokemon, xlab = "Generation", ylab = "Base Stat Total", main = "Base Stat Distribution per Generation")
hist(pokemon$Total, main='BST of all Pokemon', xlab='Base Stat Total')

#K means Clustering for stat distribution

#Clustering results based on BST stat total 

stats <- pokemon[5:11] #Dataframe of stats (not including total)
stats_z <- as.data.frame(lapply(stats, scale))
summary(stats_z)

View(stats)

#Sort stats into 5 clusters
stat_clusters <- kmeans(stats_z, 5)
stat_clusters$cluster
stat_clusters$size
stat_clusters$centers

stats$cluster <- stat_clusters$cluster

plot(x=stats$Speed, y=stats$Attack, col=stats$cluster, xlab='Speed', ylab='Attack', main='Comparison of Speed and Attack Stats')
plot(x=stats$Attack, y=stats$HP, col=stats$cluster, xlab='Attack', ylab='HP', main='Comparison of HP and Attack Stats')
plot(x=stats$Sp..Atk, y=stats$HP, col=stats$cluster, xlab='Special Attack', ylab='HP', main='Comparison of HP and Special Attack Stats')
plot(x=stats$Defense, y=stats$HP, col=stats$cluster, xlab='Defense', ylab='HP', main='Comparison of HP and Defense Stats')
plot(x=stats$Sp..Def, y=stats$HP, col=stats$cluster, xlab='Sp..Def', ylab='HP', main='Comparison of HP and Special efense Stats')
plot(x=stats$Defense, y=stats$Speed, col=stats$cluster, xlab='Defense', ylab='Speed', main='Comparison of Defense and Speed Stats')

#Mean BST by cluster
aggregate(data = stats, Total ~ cluster, mean)

aggregate(data = stats, HP ~ cluster, mean)

aggregate(data = stats, Attack ~ cluster, mean)

aggregate(data = stats, Defense ~ cluster, mean)

aggregate(data = stats, Sp..Atk ~ cluster, mean)

aggregate(data = stats, Sp..Def ~ cluster, mean)

aggregate(data = stats, Speed ~ cluster, mean)

#correlation matrix
cor(pokemon[c("HP","Attack","Defense","Sp..Atk","Sp..Def","Speed")])

#scatterplot matrix that summarizes comparisons
pairs(pokemon[c("HP","Attack","Defense","Sp..Atk","Sp..Def","Speed")])
