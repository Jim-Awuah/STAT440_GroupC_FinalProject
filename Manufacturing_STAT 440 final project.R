library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
install.packages("corrplot")
install.packages(c("readxl","tidyverse","ggplot2","dplyr","corrplot"))
library(readxl)
install.packages("readxl")
library(readxl)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("corrplot")
library(corrplot)
install.packages("tidyverse")

str(Manufacturer_data_for_PROJECT_stat_440_)
head()
names(Manufacturer_data_for_PROJECT_stat_440_)

colnames(Manufacturer_data_for_PROJECT_stat_440_) <- c(
  "SIC_Code",
  "Num_Employees",
  "Num_Prod_Workers",
  "Value_Added",
  "Cost_Materials",
  "Shipment_Value",
  "Capital_Exp",
  "Ending_Inventory",
  "Industry_Group"
)
names(Manufacturer_data_for_PROJECT_stat_440_)
colSums(is.na(Manufacturer_data_for_PROJECT_stat_440_))
Manufacturer_data_for_PROJECT_stat_440_$Industry_Group <- as.factor(Manufacturer_data_for_PROJECT_stat_440_$Industry_Group)
summary(Manufacturer_data_for_PROJECT_stat_440_)
summary(Manufacturer_data_for_PROJECT_stat_440_$Industry_Group)
sd(Manufacturer_data_for_PROJECT_stat_440_$SIC_Code)
sd(Manufacturer_data_for_PROJECT_stat_440_$Num_Employees)
sd(Manufacturer_data_for_PROJECT_stat_440_$Num_Prod_Workers)
sd(Manufacturer_data_for_PROJECT_stat_440_$Value_Added)
sd(Manufacturer_data_for_PROJECT_stat_440_$Cost_Materials)
sd(Manufacturer_data_for_PROJECT_stat_440_$Shipment_Value)
sd(Manufacturer_data_for_PROJECT_stat_440_$Capital_Exp)
sd(Manufacturer_data_for_PROJECT_stat_440_$Ending_Inventory)


cor_matrix <- cor(Manufacturer_data_for_PROJECT_stat_440_[,1:8])

corrplot(
  cor_matrix,
  method = "circle"
)


library(corrplot)

cor_matrix <- cor(Manufacturer_data_for_PROJECT_stat_440_[,2:8])
library(corrplot)

cor_matrix <- cor(data[,2:8])

corrplot(
  cor_matrix,
  type = "upper",
  method = "circle",
  order = "hclust",   
  tl.cex = 0.8        
)

round(cor(Manufacturer_data_for_PROJECT_stat_440_[,2:8]), 2)


library(ggplot2)
library(ggplot2)

ggplot(Manufacturer_data_for_PROJECT_stat_440_,
       aes(x = Shipment_Value)) +
  geom_bar( fill = "steelblue", color = "steelblue") +
  labs(
    title = "Distribution of Shipment Value"
  )
ggplot(Manufacturer_data_for_PROJECT_stat_440_, 
       aes(x = factor(Shipment_Value, 
                      levels = c("4","3","2","1")),
           fill = factor(Shipment_Value))) +
  geom_bar(color = "steelblue") +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Distribution of Shipment Value",
       x = "Shipment Value",
       y = "Count",
       fill = "Shipment Value") +
  theme_minimal()

ggplot(Manufacturer_data_for_PROJECT_stat_440_, aes(x = Cost_Materials, y = Shipment_Value)) +
  geom_jitter(height = 0.15, alpha = 0.4, color = "black") +
  geom_smooth(method = "lm", color = "steelblue") +
  labs(title = "Cost of Materials vs Shipment Value")


Manufacturer_data_for_PROJECT_stat_440_ %>%
  count(Industry_Group, Shipment_Value) %>%
  arrange(Shipment_Value, desc(n)) %>%
  ggplot(aes(x = as.factor(Industry_Group), 
             y = as.factor(Shipment_Value), 
             fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Shipment Value by Industry Group",
       x = "Industry Group", y = "Shipment Value",
       fill = "Count") +
  theme_minimal()

model <- lm(log(Value_Added) ~ log(Num_Employees) + 
              log(Cost_Materials) +
              log(Capital_Exp) + 
              log(Ending_Inventory) +
              as.factor(Industry_Group),
            data = Manufacturer_data_for_PROJECT_stat_440_)

summary(model)
vif(model)
library(car)
install.packages("car")
library(car)
install.packages("car", dependencies = TRUE)
library(car)
vif(model)

install.packages("car", repos = "https://cran.r-project.org")

ggplot(Manufacturer_data_for_PROJECT_stat_440_, aes(x = as.factor(Industry_Group), 
                 y = Value_Added, 
                 fill = as.factor(Industry_Group))) +
  geom_boxplot(show.legend = FALSE,
               outlier.colour = "darkgrey",
               outlier.shape = 16,
               outlier.size = 2,
               width = 0.6) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Value Added by Industry Group",
       subtitle = "black dots indicate outliers",
       x = "Industry Group", 
       y = "Value Added ($000s)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )


Manufacturer_data_for_PROJECT_stat_440_ %>%
  group_by(Industry_Group) %>%
  summarise(
    n              = n(),
    mean_va        = round(mean(Value_Added), 2),
    median_va      = round(median(Value_Added), 2),
    sd_va          = round(sd(Value_Added), 2),
    min_va         = min(Value_Added),
    max_va         = max(Value_Added),
    mean_emp       = round(mean(Num_Employees), 2),
    mean_cost      = round(mean(Cost_Materials), 2),
    mean_capex     = round(mean(Capital_Exp), 2)
  ) %>%
  arrange(desc(mean_va))

install.packages("moments")
library(moments)
Manufacturer_data_for_PROJECT_stat_440_ %>%
  select(Value_Added, Num_Employees, 
         Cost_Materials, Capital_Exp, 
         Ending_Inventory) %>%
  summarise(across(everything(), 
                   list(skewness = skewness, 
                        kurtosis = kurtosis)))


ggplot(Manufacturer_data_for_PROJECT_stat_440_, aes(x = as.factor(Industry_Group), 
                 y = Value_Added, 
                 fill = as.factor(Industry_Group))) +
  geom_boxplot(show.legend = FALSE,
               outlier.colour = "black",
               outlier.shape = 16,
               outlier.size = 2,
               width = 0.6) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Value Added by Industry Group",
       subtitle = "Black dots indicate outliers",
       x = "Industry Group", 
       y = "Value Added ($000s)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

ggsave("boxplot_value_added_clear.png", width = 18, height = 8, dpi = 300)
