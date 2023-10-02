library(tidyverse)
library(AmesHousing)
library(janitor)
library(hrbrthemes)
library(ggeasy)


theme_set(theme_ipsum_rc())


rv <- ames_raw %>% 
  clean_names() %>% 
  select(lot_area)


rv %>% 
  summarize(pop_mean = mean(lot_area),
            pop_variance = var(lot_area),
            pop_sd = sd(lot_area))



lot_area <- rv %>% 
  pull(lot_area)



### samples of size 50:

s50_1 <- sample(lot_area, 50)
s50_2 <- sample(lot_area, 50)
s50_3 <- sample(lot_area, 50)
s50_4 <- sample(lot_area, 50)

s50_1 %>% 
  mean()

s50_2 %>% 
  mean()

s50_3 %>% 
  mean()

s50_4 %>% 
  mean()



sample_means50 <- rep(NA, 5000)  ## creating an empty vector of 5000 values.

for(i in 1:5000){                ## starting the loop (5,000 iterations).
  s50_loop <- sample(lot_area, 50)        ## drawing samples of size n = 50
  sample_means50[i] <- mean(s50_loop) ## filling the empty values with the sample means.
}


sample_means50 <- sample_means50 %>% 
  as_tibble()

sample_means50 %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = "white") +
  scale_x_comma() +
  labs(x = "Mean value",
       y = "Count") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


sample_means50 %>% 
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "#800080", binwidth = 1000, alpha = 0.5) +
  geom_density(alpha = .3, linewidth = 1) +
  scale_x_comma()


sample_means50 <- sample_means50 %>% 
  mutate(std_area = (value - mean(value)) / sd(value))

sample_means50 %>% 
  ggplot(aes(x = std_area)) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "#800080", binwidth = 0.25, alpha = 0.5) +
  geom_density(alpha = .3, linewidth = 1)




### samples of size 500:

sample_means500 <- rep(NA, 5000)  ## creating an empty vector of 5000 values.

for(i in 1:5000){                ## starting the loop (5,000 iterations).
  s500_loop <- sample(lot_area, 500)        ## drawing samples of size n = 50
  sample_means500[i] <- mean(s500_loop) ## filling the empty values with the sample means.
}


sample_means500 <- sample_means500 %>% 
  as_tibble()

sample_means500 %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = "black", fill = "#ff8b8b", alpha = 0.5) +
  scale_x_comma()


sample_means500 <- sample_means500 %>% 
  mutate(std_area = (value - mean(value)) / sd(value))


sample_means500 %>% 
  ggplot(aes(x = std_area)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "#ff8b8b", binwidth = 0.25, alpha = 0.5) +
  geom_density(alpha = .3, linewidth = 1)



### samples of size 1500:

sample_means1500 <- rep(NA, 5000)  ## creating an empty vector of 5000 values.

for(i in 1:5000){                ## starting the loop (5,000 iterations).
  s1500_loop <- sample(lot_area, 1500)        ## drawing samples of size n = 50
  sample_means1500[i] <- mean(s1500_loop) ## filling the empty values with the sample means.
}


sample_means1500 <- sample_means1500 %>% 
  as_tibble()

sample_means1500 %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = "black", fill = "#769293", alpha = 0.5) +
  scale_x_comma()


sample_means1500 <- sample_means1500 %>% 
  mutate(std_area = (value - mean(value)) / sd(value))


sample_means1500 %>% 
  ggplot(aes(x = std_area)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "#769293", binwidth = 0.25, alpha = 0.5) +
  geom_density(alpha = .3, linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), geom = "area", fill = "#8b9dc3", alpha = 0.5)


sample_means1500 %>% 
  ggplot(aes(x = std_area)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), geom = "area", fill = "#8b9dc3", alpha = 0.5)
