
# Density plots of monthly average temperatures

# Installed Packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggridges")
install.packages("viridis")

# Libraries
library(dplyr)
library(ggplot2)
library(ggridges)
library(viridis)

# Weather data set
weather <- read.csv("TemperatureData.csv") # Year 1970-2023
View(weather)

# To check the missing values
sapply(weather, function(x) sum(is.na(x)))

# To check the summary statistics 
summary(weather$MaxT..Deg.Cel.)
summary(weather$MinT.Deg.Cel.)

# Select years 1990-2023
weather01 <- weather |> 
  filter(Year >= 1990 & Year <= 2023)

sum(is.na(weather01))


# Year wise monthly average data temperature

weather.data <-  weather01 |>  
  group_by(Year, Month) |> 
  select(Station, Year, Month, MaxT..Deg.Cel., MinT.Deg.Cel.) |>
  summarise("T.max"=mean(MaxT..Deg.Cel.), 
            "T.min" =min(MinT.Deg.Cel.))

View(weather.data)

# Density plot of Average Monthly Maximum Temperature of Bangladesh 1990-2023
dd1 <- weather.data |> 
  ungroup() |> 
  select(Month, T.max) |> 
  mutate(Month = factor(Month)) |> 
  ggplot( aes(x = T.max, y = factor(Month, levels = 1:12, labels = month.name), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 3)) +
  scale_fill_viridis(name = "Temperature (°C)", option = "C") +
  labs(title = "Density plot of average monthly max. temperature of Bangladesh 1990-2023", x = "Maximum Temperature (°C)",
        y = "Month") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
dd1


# Density plot of Average Monthly Minimum Temperature of Bangladesh 1990-2023
dd2 <- weather.data |> 
  ungroup() |> 
  select(Month, T.min) |> 
  mutate(Month = factor(Month)) |>
  ggplot(aes(x = T.min, y = factor(Month, levels = 1:12, labels = month.name),
             fill = ..x..))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.)+
  scale_x_continuous(expand = c(0.01, 0))+
  scale_y_discrete(expand = c(0.01,0))+
  scale_fill_viridis(name = "Temperature (°C)", option = "C")+
  labs(title = "Density plot of average monthly min. temperature of Bangladesh 1990-2023",x = "Minimum Temperature (°C)",
       y = "Month") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))

dd2
