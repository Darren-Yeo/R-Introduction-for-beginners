# Section 2

#1)
print("Hello my name is Darren")
#2)
sqrt(1400)
2^6/8
254 %% 8
exp(5)
#3)
#no it cant, solution would be to use paste0 or paste
#4)
a <- 10
a/10
a*10
#Not possible unless you convert a to a character, then combine with paste0 or paste
#5)
#TRUE
#TRUE
#FALSE
#FALSE
#TRUE
#FALSE

# Section 3.3
#1) One is a integer and one is a float/numeric/double
#2) One is a integer while one is a character
#3) No, Integer or numerical values can be added to characters
#4) Yes, both are considered numerical

# Section 4.1.6
#1) 
Countries <- c("Germany", "USA", "Mexico","France")
Numbers <- c(4,6,7,3,5,7)
boolean <- c(TRUE,TRUE,FALSE,TRUE,FALSE)
Numbers <- as.integer(Numbers)
boolean <- as.character(boolean)

#2)
Numbers <- c(4,6,7,3,5,7)
Numbers2 <- c(7,5,2,10,2,4)
Numbers * Numbers2
Numbers / Numbers2

#3)
movie_genres <- c("Horror","Comedy","Documentary","Action","Adventure")

movie_genres <- c(movie_genres,"Biography")

movie_genres <- paste(movie_genres,"on demand",sep=" ")

movie_rating <- c("Watchable","Watchable","Bad","Good","Good","Bad")
movie_genres <- paste(movie_genres, movie_rating, sep= " ")

#4)
Animals <- c("Lion", "Tiger", "Cheetah", "Otter", "Civet")
Numbers <- c(4,6,8,2,5)

Animals[2:length(Animals)]
Numbers[2:length(Numbers)]

Animals[c(1,4)]
Numbers[c(1,4)]

Animals[Animals %in% "Civet"]
Numbers[Numbers < 4]
Numbers[Numbers >= 4]

# Sections 4.2.3
#1)
Numbers <-  c(1,5,7,8,4,3,10,-5,-3,-7,12,9,14,2,-8)
Number.mat <- matrix(Numbers, nrow = 5,ncol = 3)

#2)
log(Number.mat)

#3)
Numbers2 <-  c(6,9,10,15,23,6,8,3,5,2)
Number.mat2 <- matrix(Numbers2, nrow = 5,ncol = 2)
Number.mat3 <- cbind(Number.mat,Number.mat2)

#4)
colMeans(Number.mat)
rowMeans(Number.mat)
sum(Number.mat)

#5)
Fruits <- c("Apple","Watermelon","Grapes","Orange","Blueberry","Wintermelon","Mangosteen","Grapefruit","Lemon","Pear","Dragonfruit","Banana")
Fruits.mat <- matrix(Fruits, nrow = 4, ncol = 3)
Fruits.mat[3,]
Fruits.mat[4,2]
Fruits.mat[2:4,c(1,3)]
Fruits.mat[Fruits.mat %in% "Grapefruit"]

# Section 5.1

#1)
Greetings <- function(Name, Age){
  
  print(paste0("Welcome to the world of bioinformatics, my name is", Name, " and I am ", Age," years old."))
  
}
Greetings("Darren", 28)

#2)
Calculate_BMI <- function(Mass,Height){
  
  BMI <- Mass/Height^2
  
  print(BMI)
}

Calculate_BMI(47,1.65)

#3)
Calculate_area_circumfernce_cirle <- function(radius){
  
  Diameter <- radius*2
  
  Circumference <- pi * Diameter
  
  Area <- pi * radius^2
  
  Area_Cicumference <- list(Circle_Area = Area,
                            Circle_Circumference = Circumference)
  
  return(Area_Cicumference)
}
CircleAreaCircumference <- Calculate_area_circumfernce_cirle(6)
CircleAreaCircumference$Circle_Area
CircleAreaCircumference$Circle_Circumference

# Section 6.3
library(tidyverse)

#2)
diamonds_new <- diamonds %>% rename(Length = x,
                                    Width = y,
                                    Depth = z)
#3)
ggplot(diamonds_new, aes(x=carat, y=log(price), color = clarity)) +
  geom_point() + labs(y = "Price (USD)",
                      x = "Carat",
                      color = "Clarity")

#4)
diamonds_new_tidy <- diamonds_new %>% 
  select(price, carat, cut, color, depth) %>%
  pivot_longer(cols = c(price,carat,depth),
               names_to = "Measurements",
               values_to = "Values")

#5)
diamonds_new_tidy_carat<- diamonds_new_tidy %>% filter(str_detect(Measurements,"carat"))

ggplot(diamonds_new_tidy_carat, aes(x = cut, y = Values, fill = color)) + 
  geom_boxplot() + 
  labs(x = "Cut",
       y = "Carat",
       fill = "Color")


#6)
diamonds_new_tidy_price<- diamonds_new_tidy %>% filter(str_detect(Measurements,"price"))

ggplot(diamonds_new_tidy_price, aes(x=Values, fill = cut))+ 
  geom_histogram(binwidth = 100) 

#7)
diamonds_new_xyz<- diamonds_new %>% 
  select(clarity, Length, Width, Depth) %>% 
  pivot_longer(cols = c("Length","Width","Depth"),
               names_to = "Measurements",
               values_to = "Values")

diamonds_new_xyz_summary <- diamonds_new_xyz %>%
                            group_by(clarity, Measurements) %>%
                            summarise(
                              Means = mean(Values),
                              SD = sd(Values),
                              Median = median(Values),
                              Max = max(Values),
                              Min = min(Values)
                            )

#8)
ggplot(diamonds_new_xyz_summary, aes(y = Means, x = clarity, fill = Measurements)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = Means - SD,  
                    ymax = Means + SD), 
                    width = 0.2,
                    position = position_dodge(0.9)) + 
  facet_grid(~Measurements) +
  labs(y = "Mean of measurements",
       x = "Clarity",
       fill = "Measurement type")

#Bonus: Section 8.4
#1)
multiqc_star <- read.delim("/media/rna/BAIJI/Darren_Doktorand/Genomanalyse_und_Phylogenie_2023-2024/Tag_1_Einfuehrung_in_R/multiqc_star.txt")

#2)
mean(multiqc_star$total_reads)
mean(multiqc_star$uniquely_mapped)
mean(multiqc_star$multimapped)

multiqc_star_new <- mutate(multiqc_star,
                           unmapped_total = unmapped_tooshort + unmapped_other)

mean(multiqc_star_new$unmapped_total)

#3)
ggplot(multiqc_star_new, aes(x=log(num_splices), y=log(uniquely_mapped), color = Sample)) + 
  geom_point() +
  labs(x = "Number of splices",
       y = "Total uniquely mapped")

#4)
multiqc_star_new_long <- multiqc_star_new %>% 
  select(Sample,uniquely_mapped,multimapped,unmapped_total) %>%
  pivot_longer(cols = c("uniquely_mapped", "multimapped", "unmapped_total"),
               names_to = "Measurements",
               values_to = "Values")

ggplot(multiqc_star_new_long, aes(x = Measurements, y = log(Values), fill = Sample)) + 
  geom_boxplot() + 
  facet_grid(~Sample) + 
  theme(axis.text.x = element_text(angle = 90))
  
