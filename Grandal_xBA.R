setwd('C:/Users/Harman Takhar/Documents')
data <- read.csv("Grandal_Data_FB.csv")

library(ggplot2)
library(dplyr)


data <- data[complete.cases(ifelse(data$plate_x == "null", NA, data$plate_x)),]
data <- data[complete.cases(ifelse(data$plate_z == "null", NA, data$plate_z)),]
data$plate_x <- (as.numeric(as.character(data$plate_x)))
data$plate_z <- (as.numeric(as.character(data$plate_z)))

data <- data[complete.cases(ifelse(data$estimated_ba_using_speedangle == "null",NA,data$estimated_ba_using_speedangle)),]
data$estimated_ba_using_speedangle <- as.numeric(as.character(data$estimated_ba_using_speedangle))

data_summary <- data %>% 
  group_by(zone) %>%
  summarise(mean_ba = mean(estimated_ba_using_speedangle))
data <- data[complete.cases(ifelse(data$zone == "null",NA,data$zone)),]
levels(data$zone)[levels(data$zone) == "null"] <- NA
data$zone <- as.factor(data$zone)


coords <- data.frame(
  x = c(rep(c(-.75,-.25,-.25,-.75,-.75,-.25,.25,.25,-.25,-.25,.25,.75,.75,.25,.25),3),
        rep(c(0,-.75,-.75,-1.5,-1.5,0,0,
              0,.75,.75,1.5,1.5,0,0),2)
  ),
  y = c(rep(c(1.5,1.5,2.166667,2.166667,1.5),3),
        rep(c(2.166667,2.166667,2.833334,2.833334,2.166667),3),
        rep(c(2.833334,2.833334,3.5,3.5,2.833334),3),
        rep(c(1.5,1.5,2.5,2.5,1,1,1.5),2),
        rep(c(3.5,3.5,2.5,2.5,4,4,3.5),2)
  ),
  zone = as.factor(c(rep(c(7,8,9,4,5,6,1,2,3),each = 5),
                     (rep(c(13,14,11,12),each = 7))
  ))
)

data_summary$zone <- as.factor(data_summary$zone)

merge <- right_join(coords,data_summary,by = c("zone")) %>%
  merge(data, by = "zone") %>%
  select(zone,x,y,mean_ba,plate_x,plate_z) %>%
  mutate(mean_ba = as.numeric(mean_ba)) %>%
  group_by(zone) %>%
  mutate(mean_x = mean(x)) %>%
  mutate(mean_y = mean(y))

merge[merge$zone == 11,]$mean_y <- merge[merge$zone == 11,]$mean_y + .3
merge[merge$zone == 12,]$mean_y <- merge[merge$zone == 12,]$mean_y + .3
merge[merge$zone == 13,]$mean_y <- merge[merge$zone == 13,]$mean_y - .3
merge[merge$zone == 14,]$mean_y <- merge[merge$zone == 14,]$mean_y - .3

ggplot(merge, #aes(x = plate_x,y=plate_z, group = zone)) +
) + #geom_point(aes(color = zone)) +
  geom_polygon(aes(x,y,fill = mean_ba,label = mean_ba,color = zone),alpha = .5) + 
  scale_fill_gradient(low="blue", high="red") +
  geom_text(aes(x = mean_x,y = mean_y,label = round(mean_ba,3))) + 
  scale_colour_hue() + labs(title = "Mean xBA by Zone", subtitle = "Fastballs", 
                            caption = "2017-2019 Yasmani Grandal") + 
  theme(legend.position = "none")

setwd('C:/Users/Harman Takhar/Documents')
data <- read.csv("Grandal_Data.csv")

library(ggplot2)
library(dplyr)


data <- data[complete.cases(ifelse(data$plate_x == "null", NA, data$plate_x)),]
data <- data[complete.cases(ifelse(data$plate_z == "null", NA, data$plate_z)),]
data$plate_x <- (as.numeric(as.character(data$plate_x)))
data$plate_z <- (as.numeric(as.character(data$plate_z)))

data <- data[complete.cases(ifelse(data$estimated_ba_using_speedangle == "null",NA,data$estimated_ba_using_speedangle)),]
data$estimated_ba_using_speedangle <- as.numeric(as.character(data$estimated_ba_using_speedangle))

data_summary <- data %>% 
  group_by(zone) %>%
  summarise(mean_ba = mean(estimated_ba_using_speedangle))
data <- data[complete.cases(ifelse(data$zone == "null",NA,data$zone)),]
levels(data$zone)[levels(data$zone) == "null"] <- NA
data$zone <- as.factor(data$zone)


coords <- data.frame(
  x = c(rep(c(-.75,-.25,-.25,-.75,-.75,-.25,.25,.25,-.25,-.25,.25,.75,.75,.25,.25),3),
        rep(c(0,-.75,-.75,-1.5,-1.5,0,0,
              0,.75,.75,1.5,1.5,0,0),2)
  ),
  y = c(rep(c(1.5,1.5,2.166667,2.166667,1.5),3),
        rep(c(2.166667,2.166667,2.833334,2.833334,2.166667),3),
        rep(c(2.833334,2.833334,3.5,3.5,2.833334),3),
        rep(c(1.5,1.5,2.5,2.5,1,1,1.5),2),
        rep(c(3.5,3.5,2.5,2.5,4,4,3.5),2)
  ),
  zone = as.factor(c(rep(c(7,8,9,4,5,6,1,2,3),each = 5),
                     (rep(c(13,14,11,12),each = 7))
  ))
)

data_summary$zone <- as.factor(data_summary$zone)

merge <- right_join(coords,data_summary,by = c("zone")) %>%
  merge(data, by = "zone") %>%
  select(zone,x,y,mean_ba,plate_x,plate_z) %>%
  mutate(mean_ba = as.numeric(mean_ba)) %>%
  group_by(zone) %>%
  mutate(mean_x = mean(x)) %>%
  mutate(mean_y = mean(y))

merge[merge$zone == 11,]$mean_y <- merge[merge$zone == 11,]$mean_y + .3
merge[merge$zone == 12,]$mean_y <- merge[merge$zone == 12,]$mean_y + .3
merge[merge$zone == 13,]$mean_y <- merge[merge$zone == 13,]$mean_y - .3
merge[merge$zone == 14,]$mean_y <- merge[merge$zone == 14,]$mean_y - .3

ggplot(merge, #aes(x = plate_x,y=plate_z, group = zone)) +
) + #geom_point(aes(color = zone)) +
  geom_polygon(aes(x,y,fill = mean_ba,label = mean_ba,color = zone),alpha = .5) + 
  scale_fill_gradient(low="blue", high="red") +
  geom_text(aes(x = mean_x,y = mean_y,label = round(mean_ba,3))) + 
  scale_colour_hue() + labs(title = "Mean xBA by Zone", subtitle = "Fastball vs Righties", 
                            caption = "2017-2019 Yasmani Grandal") + 
  theme(legend.position = "none")

setwd('C:/Users/Harman Takhar/Documents')
data <- read.csv("Grandal_Datalefty_fb.csv")

library(ggplot2)
library(dplyr)


data <- data[complete.cases(ifelse(data$plate_x == "null", NA, data$plate_x)),]
data <- data[complete.cases(ifelse(data$plate_z == "null", NA, data$plate_z)),]
data$plate_x <- (as.numeric(as.character(data$plate_x)))
data$plate_z <- (as.numeric(as.character(data$plate_z)))

data <- data[complete.cases(ifelse(data$estimated_ba_using_speedangle == "null",NA,data$estimated_ba_using_speedangle)),]
data$estimated_ba_using_speedangle <- as.numeric(as.character(data$estimated_ba_using_speedangle))

data_summary <- data %>% 
  group_by(zone) %>%
  summarise(mean_ba = mean(estimated_ba_using_speedangle))
data <- data[complete.cases(ifelse(data$zone == "null",NA,data$zone)),]
levels(data$zone)[levels(data$zone) == "null"] <- NA
data$zone <- as.factor(data$zone)


coords <- data.frame(
  x = c(rep(c(-.75,-.25,-.25,-.75,-.75,-.25,.25,.25,-.25,-.25,.25,.75,.75,.25,.25),3),
        rep(c(0,-.75,-.75,-1.5,-1.5,0,0,
              0,.75,.75,1.5,1.5,0,0),2)
  ),
  y = c(rep(c(1.5,1.5,2.166667,2.166667,1.5),3),
        rep(c(2.166667,2.166667,2.833334,2.833334,2.166667),3),
        rep(c(2.833334,2.833334,3.5,3.5,2.833334),3),
        rep(c(1.5,1.5,2.5,2.5,1,1,1.5),2),
        rep(c(3.5,3.5,2.5,2.5,4,4,3.5),2)
  ),
  zone = as.factor(c(rep(c(7,8,9,4,5,6,1,2,3),each = 5),
                     (rep(c(13,14,11,12),each = 7))
  ))
)

data_summary$zone <- as.factor(data_summary$zone)

merge <- right_join(coords,data_summary,by = c("zone")) %>%
  merge(data, by = "zone") %>%
  select(zone,x,y,mean_ba,plate_x,plate_z) %>%
  mutate(mean_ba = as.numeric(mean_ba)) %>%
  group_by(zone) %>%
  mutate(mean_x = mean(x)) %>%
  mutate(mean_y = mean(y))

merge[merge$zone == 11,]$mean_y <- merge[merge$zone == 11,]$mean_y + .3
merge[merge$zone == 12,]$mean_y <- merge[merge$zone == 12,]$mean_y + .3
merge[merge$zone == 13,]$mean_y <- merge[merge$zone == 13,]$mean_y - .3
merge[merge$zone == 14,]$mean_y <- merge[merge$zone == 14,]$mean_y - .3

ggplot(merge, #aes(x = plate_x,y=plate_z, group = zone)) +
) + #geom_point(aes(color = zone)) +
  geom_polygon(aes(x,y,fill = mean_ba,label = mean_ba,color = zone),alpha = .5) + 
  scale_fill_gradient(low="blue", high="red") +
  geom_text(aes(x = mean_x,y = mean_y,label = round(mean_ba,3))) + 
  scale_colour_hue() + labs(title = "Mean xBA by Zone", subtitle = "Fastball vs Lefties", 
                            caption = "2017-2019 Yasmani Grandal") + 
  theme(legend.position = "none")

setwd('C:/Users/Harman Takhar/Documents')
data <- read.csv("Grandal_Data_OS.csv")

library(ggplot2)
library(dplyr)


data <- data[complete.cases(ifelse(data$plate_x == "null", NA, data$plate_x)),]
data <- data[complete.cases(ifelse(data$plate_z == "null", NA, data$plate_z)),]
data$plate_x <- (as.numeric(as.character(data$plate_x)))
data$plate_z <- (as.numeric(as.character(data$plate_z)))

data <- data[complete.cases(ifelse(data$estimated_ba_using_speedangle == "null",NA,data$estimated_ba_using_speedangle)),]
data$estimated_ba_using_speedangle <- as.numeric(as.character(data$estimated_ba_using_speedangle))

data_summary <- data %>% 
  group_by(zone) %>%
  summarise(mean_ba = mean(estimated_ba_using_speedangle))
data <- data[complete.cases(ifelse(data$zone == "null",NA,data$zone)),]
levels(data$zone)[levels(data$zone) == "null"] <- NA
data$zone <- as.factor(data$zone)


coords <- data.frame(
  x = c(rep(c(-.75,-.25,-.25,-.75,-.75,-.25,.25,.25,-.25,-.25,.25,.75,.75,.25,.25),3),
        rep(c(0,-.75,-.75,-1.5,-1.5,0,0,
              0,.75,.75,1.5,1.5,0,0),2)
  ),
  y = c(rep(c(1.5,1.5,2.166667,2.166667,1.5),3),
        rep(c(2.166667,2.166667,2.833334,2.833334,2.166667),3),
        rep(c(2.833334,2.833334,3.5,3.5,2.833334),3),
        rep(c(1.5,1.5,2.5,2.5,1,1,1.5),2),
        rep(c(3.5,3.5,2.5,2.5,4,4,3.5),2)
  ),
  zone = as.factor(c(rep(c(7,8,9,4,5,6,1,2,3),each = 5),
                     (rep(c(13,14,11,12),each = 7))
  ))
)

data_summary$zone <- as.factor(data_summary$zone)

merge <- right_join(coords,data_summary,by = c("zone")) %>%
  merge(data, by = "zone") %>%
  select(zone,x,y,mean_ba,plate_x,plate_z) %>%
  mutate(mean_ba = as.numeric(mean_ba)) %>%
  group_by(zone) %>%
  mutate(mean_x = mean(x)) %>%
  mutate(mean_y = mean(y))

merge[merge$zone == 11,]$mean_y <- merge[merge$zone == 11,]$mean_y + .3
merge[merge$zone == 12,]$mean_y <- merge[merge$zone == 12,]$mean_y + .3
merge[merge$zone == 13,]$mean_y <- merge[merge$zone == 13,]$mean_y - .3
merge[merge$zone == 14,]$mean_y <- merge[merge$zone == 14,]$mean_y - .3

ggplot(merge, #aes(x = plate_x,y=plate_z, group = zone)) +
) + #geom_point(aes(color = zone)) +
  geom_polygon(aes(x,y,fill = mean_ba,label = mean_ba,color = zone),alpha = .5) + 
  scale_fill_gradient(low="blue", high="red") +
  geom_text(aes(x = mean_x,y = mean_y,label = round(mean_ba,3))) + 
  scale_colour_hue() + labs(title = "Mean xBA by Zone", subtitle = "Breaking Balls", 
                            caption = "2017-2019 Yasmani Grandal") + 
  theme(legend.position = "none")

setwd('C:/Users/Harman Takhar/Documents')
data <- read.csv("Grandal_DataR_OS.csv")

library(ggplot2)
library(dplyr)


data <- data[complete.cases(ifelse(data$plate_x == "null", NA, data$plate_x)),]
data <- data[complete.cases(ifelse(data$plate_z == "null", NA, data$plate_z)),]
data$plate_x <- (as.numeric(as.character(data$plate_x)))
data$plate_z <- (as.numeric(as.character(data$plate_z)))

data <- data[complete.cases(ifelse(data$estimated_ba_using_speedangle == "null",NA,data$estimated_ba_using_speedangle)),]
data$estimated_ba_using_speedangle <- as.numeric(as.character(data$estimated_ba_using_speedangle))

data_summary <- data %>% 
  group_by(zone) %>%
  summarise(mean_ba = mean(estimated_ba_using_speedangle))
data <- data[complete.cases(ifelse(data$zone == "null",NA,data$zone)),]
levels(data$zone)[levels(data$zone) == "null"] <- NA
data$zone <- as.factor(data$zone)


coords <- data.frame(
  x = c(rep(c(-.75,-.25,-.25,-.75,-.75,-.25,.25,.25,-.25,-.25,.25,.75,.75,.25,.25),3),
        rep(c(0,-.75,-.75,-1.5,-1.5,0,0,
              0,.75,.75,1.5,1.5,0,0),2)
  ),
  y = c(rep(c(1.5,1.5,2.166667,2.166667,1.5),3),
        rep(c(2.166667,2.166667,2.833334,2.833334,2.166667),3),
        rep(c(2.833334,2.833334,3.5,3.5,2.833334),3),
        rep(c(1.5,1.5,2.5,2.5,1,1,1.5),2),
        rep(c(3.5,3.5,2.5,2.5,4,4,3.5),2)
  ),
  zone = as.factor(c(rep(c(7,8,9,4,5,6,1,2,3),each = 5),
                     (rep(c(13,14,11,12),each = 7))
  ))
)

data_summary$zone <- as.factor(data_summary$zone)

merge <- right_join(coords,data_summary,by = c("zone")) %>%
  merge(data, by = "zone") %>%
  select(zone,x,y,mean_ba,plate_x,plate_z) %>%
  mutate(mean_ba = as.numeric(mean_ba)) %>%
  group_by(zone) %>%
  mutate(mean_x = mean(x)) %>%
  mutate(mean_y = mean(y))

merge[merge$zone == 11,]$mean_y <- merge[merge$zone == 11,]$mean_y + .3
merge[merge$zone == 12,]$mean_y <- merge[merge$zone == 12,]$mean_y + .3
merge[merge$zone == 13,]$mean_y <- merge[merge$zone == 13,]$mean_y - .3
merge[merge$zone == 14,]$mean_y <- merge[merge$zone == 14,]$mean_y - .3

ggplot(merge, #aes(x = plate_x,y=plate_z, group = zone)) +
) + #geom_point(aes(color = zone)) +
  geom_polygon(aes(x,y,fill = mean_ba,label = mean_ba,color = zone),alpha = .5) + 
  scale_fill_gradient(low="blue", high="red") +
  geom_text(aes(x = mean_x,y = mean_y,label = round(mean_ba,3))) + 
  scale_colour_hue() + labs(title = "Mean xBA by Zone", subtitle = "Breaking Balls vs Righties", 
                            caption = "2017-2019 Yasmani Grandal") + 
  theme(legend.position = "none")

setwd('C:/Users/Harman Takhar/Documents')
data <- read.csv("Grandal_DataL_OS.csv")

library(ggplot2)
library(dplyr)


data <- data[complete.cases(ifelse(data$plate_x == "null", NA, data$plate_x)),]
data <- data[complete.cases(ifelse(data$plate_z == "null", NA, data$plate_z)),]
data$plate_x <- (as.numeric(as.character(data$plate_x)))
data$plate_z <- (as.numeric(as.character(data$plate_z)))

data <- data[complete.cases(ifelse(data$estimated_ba_using_speedangle == "null",NA,data$estimated_ba_using_speedangle)),]
data$estimated_ba_using_speedangle <- as.numeric(as.character(data$estimated_ba_using_speedangle))

data_summary <- data %>% 
  group_by(zone) %>%
  summarise(mean_ba = mean(estimated_ba_using_speedangle))
data <- data[complete.cases(ifelse(data$zone == "null",NA,data$zone)),]
levels(data$zone)[levels(data$zone) == "null"] <- NA
data$zone <- as.factor(data$zone)


coords <- data.frame(
  x = c(rep(c(-.75,-.25,-.25,-.75,-.75,-.25,.25,.25,-.25,-.25,.25,.75,.75,.25,.25),3),
        rep(c(0,-.75,-.75,-1.5,-1.5,0,0,
              0,.75,.75,1.5,1.5,0,0),2)
  ),
  y = c(rep(c(1.5,1.5,2.166667,2.166667,1.5),3),
        rep(c(2.166667,2.166667,2.833334,2.833334,2.166667),3),
        rep(c(2.833334,2.833334,3.5,3.5,2.833334),3),
        rep(c(1.5,1.5,2.5,2.5,1,1,1.5),2),
        rep(c(3.5,3.5,2.5,2.5,4,4,3.5),2)
  ),
  zone = as.factor(c(rep(c(7,8,9,4,5,6,1,2,3),each = 5),
                     (rep(c(13,14,11,12),each = 7))
  ))
)

data_summary$zone <- as.factor(data_summary$zone)

merge <- right_join(coords,data_summary,by = c("zone")) %>%
  merge(data, by = "zone") %>%
  select(zone,x,y,mean_ba,plate_x,plate_z) %>%
  mutate(mean_ba = as.numeric(mean_ba)) %>%
  group_by(zone) %>%
  mutate(mean_x = mean(x)) %>%
  mutate(mean_y = mean(y))

merge[merge$zone == 11,]$mean_y <- merge[merge$zone == 11,]$mean_y + .3
merge[merge$zone == 12,]$mean_y <- merge[merge$zone == 12,]$mean_y + .3
merge[merge$zone == 13,]$mean_y <- merge[merge$zone == 13,]$mean_y - .3
merge[merge$zone == 14,]$mean_y <- merge[merge$zone == 14,]$mean_y - .3

ggplot(merge, #aes(x = plate_x,y=plate_z, group = zone)) +
) + #geom_point(aes(color = zone)) +
  geom_polygon(aes(x,y,fill = mean_ba,label = mean_ba,color = zone),alpha = .5) + 
  scale_fill_gradient(low="blue", high="red") +
  geom_text(aes(x = mean_x,y = mean_y,label = round(mean_ba,3))) + 
  scale_colour_hue() + labs(title = "Mean xBA by Zone", subtitle = "Breaking Balls vs Lefties", 
                            caption = "2017-2019 Yasmani Grandal") + 
  theme(legend.position = "none")


