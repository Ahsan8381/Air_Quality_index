---
title: "Data Visualization Project using R on Air Quality Dataset"
author: "Mohd Ahsan"
output: 
  html_document:
    theme: cerulean
    toc: true
    toc_depth: 2
    number_sections: true
---



## **Data Exploration, cleaning and preprocessing**

```{r, warning = FALSE, message = FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
.libPaths("C:/Users/roger/AppData/Local/R/win-library/4.4")
library(tidyverse)
library(dplyr)
library(lubridate)
library(patchwork)
library(cowplot)
library(corrplot)
library(ggplot2)
library(reshape2)
```

```{r, warning = FALSE, message = FALSE}
data = suppressMessages(read_delim("AirQualityUCI.csv", delim = ";", col_names = TRUE, show_col_types = F))
view(data)
```

```{r, warning = FALSE, message = FALSE}
head(data)
```

```{r, warning = FALSE, message = FALSE}
data = data[, -c(16, 17)]
```

Since the last two columns are entirely "NA" values. So the above code just simply removes them from the data.

```{r, warning = FALSE, message = FALSE}
data[data < 0] = NA
sum(is.na(data))
```
As stated above that all the -200 values are "NA". So the above code replaces -200 with "NA" and returns the total "NA" value count.

```{r, warning = FALSE, message = FALSE}
print(summary(data))
```

```{r, warning = FALSE, message = FALSE}
str(data)
```

```{r, warning = FALSE, message = FALSE}
cat('The column names are : ', colnames(data))

```

```{r, warning = FALSE, message = FALSE}
cat("The dimension of the data is : ", dim(data))
```
The above output states that there are 9471 rows and 15 columns in the data.

```{r, warning = FALSE, message = FALSE}
cat("Following is the count of 'NA' values in the data : ", sum(is.na(data)))
```

```{r, warning = FALSE, message = FALSE}
cat(
  "Following are the counts of the NA values of each column:\n",
  "Date:", sum(is.na(data$Date)), "\n",
  "Time:", sum(is.na(data$Time)), "\n",
  "CO(GT):", sum(is.na(data$`CO(GT)`)), "\n",
  "PT08.S1(CO):", sum(is.na(data$`PT08.S1(CO)`)), "\n",
  "NMHC(GT):", sum(is.na(data$`NMHC(GT)`)), "\n",
  "C6H6(GT):", sum(is.na(data$`C6H6(GT)`)), "\n",
  "PT08.S2(NMHC):", sum(is.na(data$`PT08.S2(NMHC)`)), "\n",
  "NOx(GT):", sum(is.na(data$`NOx(GT)`)), "\n",
  "PT08.S3(NOx):", sum(is.na(data$`PT08.S3(NOx)`)), "\n",
  "NO2(GT):", sum(is.na(data$`NO2(GT)`)), "\n",
  "PT08.S4(NO2):", sum(is.na(data$`PT08.S4(NO2)`)), "\n",
  "PT08.S5(O3):", sum(is.na(data$`PT08.S5(O3)`)), "\n",
  "T:", sum(is.na(data$T)), "\n",
  "RH:", sum(is.na(data$RH)), "\n",
  "AH:", sum(is.na(data$AH)), "\n"
)

```

```{r, warning = FALSE, message = FALSE}
data = mutate(data, `CO(GT)` = as.numeric(gsub(",", ".", data$`CO(GT)`)), `C6H6(GT)` = as.numeric(gsub(",", ".", data$`C6H6(GT)`)), AH = as.numeric(gsub(",", ".", data$AH)), T = as.numeric(gsub(",", ".", data$T)), Date = as.Date(data$Date, format = "%d/%m/%Y"))
```

The above code replace the "," within the values to "decimal" and converts the type to "numeric" from "characters" because correct format is needed to make the plots.

### **Note**

Since different columns are used for different plots, removing "NA" values would unnecessarily remove useful data that could be relevant for other analyses or visualizations. Therefore, it's important to keep the rows intact and handle missing values more selectively, without losing valuable information from other columns.

So the removal of "NA" values will be done accordingly as required.

```{r, warning = FALSE, message = FALSE}
view(data)
```

## **Data Visualization**

```{r, warning = FALSE, message = FALSE}
Time_data = select(data, Date, `CO(GT)`)
Time_data = drop_na(Time_data, Date)
Time_data = drop_na(Time_data, `CO(GT)`)
sum(is.na(Time_data$`CO(GT)`))
```

The above code selects the desired columns from the data, removes the "NA" values from the selected Data and returns that there is no "NA" values. 

```{r, warning = FALSE, message = FALSE}
plot1 = ggplot(data = Time_data, mapping = aes(x = `CO(GT)`)) + geom_histogram(aes(y = ..count..),color = 'black', fill= 'purple', alpha = 0.7) + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +theme_minimal() + labs(x = "Ground Truth hourly averaged concentrations for CO", y = "Frequency", caption = 'Source : Air Quality Dataset', title = "How Toxic Is the Air? A Look at CO Concentrations") + 
  
theme(plot.title = element_text(face = 'bold', color = 'black', hjust = 0.2, margin = margin(b = 20, t = 5)), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.title.x = element_text(margin = margin(t = 15, b = 10)), axis.title.y = element_text(margin = margin(l = 5, r = 20)), plot.background = element_rect(fill = "#FAF9F6", color = "black", size = 2), axis.text = element_text(color = "black"), panel.grid = element_line(color = "white"))

print(plot1)

ggsave("CO_distribution_plot.png", plot = plot1, width = 8, height = 6, dpi = 300)
```
## Motivation : 

This plot is to understand the distribution of CO concentrations over time and identify how often different levels occur. It helps to assess air quality and understand if CO levels are within safe limits or not, which could be useful for pollution control measures.

## Findings : 

1.The plot suggests that the data is right-skewed (positively skewed). This means that most of the data points are concentrated towards the lower range of CO concentrations (0-2), and as the concentrations increase, the frequency of occurrence decreases.
In a right-skewed distribution, the tail on the right side (higher concentrations) is longer than the left, which is consistent with the pattern where high CO concentrations are much less frequent.

2.Most of the time, the CO concentrations are low (0-3), which suggests the air quality is generally good in terms of CO. The sharp drop in frequency as concentrations rise suggests that high CO concentrations are uncommon.

```{r, warning = FALSE, message = FALSE}
Time_data1 = select(data, Date, `C6H6(GT)`)
Time_data1 = mutate(Time_data1, Day = format(Date, "%d"), Month = format(Date, "%m"), Year = format(Date, "%Y"))
Time_data1 = na.omit(Time_data1)
Time_data1 = group_by(Time_data1, Month, Year)
Time_data1 = summarise(Time_data1, Max = max(`C6H6(GT)`), Avg = mean(`C6H6(GT)`),  Min = min(`C6H6(GT)`), .groups = "drop")
Time_data1 = arrange(Time_data1, Year)
Time_data1 = mutate(Time_data1, MonthYear = as.Date(paste(Year, Month, "1", sep = "-")))
legend_plot = pivot_longer(Time_data1, `Max`:`Min`, names_to = "statistics", values_to = "count")
Time_data1 = pivot_longer(Time_data1, `Max`:`Avg`, names_to = "statistics", values_to = "count")
Time_data1 = pivot_longer(Time_data1, `Min`, names_to = "Min", values_to = "count2")
sum(is.na(Time_data1))

```

The code selects the Date and C6H6(GT) (Benzene) columns, extracts Day, Month, and Year, removes NA values, and groups data by Month and Year to calculate monthly stats (Max, Min, Avg). It arranges Year in order and creates a MonthYear column. The data is reshaped using pivot_longer() for legends and separate plots are made for Min, Max, and Avg due to scale differences. Finally, the structure is adjusted twice to separate Max/Avg from Min and replaces NA with 0.

```{r, warning = FALSE, message = FALSE}
p1 = ggplot(Time_data1, aes(x = MonthYear, y = count, fill = statistics)) + geom_bar(stat = 'identity', position = "dodge", color = "black") + scale_fill_manual(values = c("Max" = "purple", "Avg" = "pink")) + labs(title = "The Rise and Fall of Benzene: Monthly Insights", x = "Month", y = "Benzene Count", fill = "Statistics") + theme_classic() +scale_x_date(breaks = as.Date(c("2004-03-01", "2004-04-01", "2004-05-01", "2004-06-01", "2004-07-01", "2004-08-01", "2004-09-01", "2004-10-01", "2004-11-01", "2004-12-01", "2005-01-01", "2005-02-01", "2005-03-01", "2005-04-01")), date_labels = "%b %Y")  + theme(axis.text.x = element_text(angle = 45, hjust = 1 ,margin = margin(t = 1)), axis.title.y = element_text(hjust = 3.5, margin = margin(r = 10), size = 13), plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 30)), axis.title.x = element_blank())   

p2 = ggplot(data = Time_data1, mapping = aes(x = MonthYear, y = count2, fill = Min)) + geom_bar(stat = "identity", position = "dodge",fill = "orange", color = "black",aes(fill = Min)) + scale_x_date(breaks = as.Date(c("2004-03-01", "2004-04-01", "2004-05-01", "2004-06-01", "2004-07-01", "2004-08-01", "2004-09-01", "2004-10-01", "2004-11-01", "2004-12-01", "2005-01-01", "2005-02-01", "2005-03-01", "2005-04-01")), date_labels = "%b %Y") + theme_classic() +labs(x = "Month")  + theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y = element_blank(), axis.title.x = element_text(margin = margin(t = 8), size = 13))

sub_graph = ggplot(legend_plot, aes(x = MonthYear, y = count, fill = statistics)) + geom_bar(stat = 'identity', position = "dodge", color = "black") + scale_fill_manual(values = c("Max" = "purple", "Avg" = "pink", "Min" = "orange")) + labs(title = "Monthly Statistics of Benzene Levels", x = "Month", y = "Benzene Count", fill = "Statistics") + theme_classic() +scale_x_date(breaks = as.Date(c("2004-03-01", "2004-04-01", "2004-05-01", "2004-06-01", "2004-07-01", "2004-08-01", "2004-09-01", "2004-10-01", "2004-11-01", "2004-12-01", "2005-01-01", "2005-02-01", "2005-03-01", "2005-04-01")), date_labels = "%b %Y")  + theme(axis.text.x = element_text(angle = 45, hjust = 1))

legend = get_legend(sub_graph)


p3 = (p1 + p2) + plot_layout(nrow = 2, guides = "collect")
p4 = ggdraw(p3) + draw_grob(legend, x = 0.423, y = 0.04)

plot2 = wrap_plots(p4) + plot_annotation(theme = theme(plot.background = element_rect(color = "black", size = 1)), caption = "Source : Air Quality Dataset") 

ggsave("Benzene_Statistics_plot.png", plot = plot2, width = 8, height = 6, dpi = 300)
print(plot2)
```
## Motivation : 

This barplot of monthly statistics (min, max, avg) for C6H6 (benzene) concentrations is to track how benzene levels change over time. It helps identify any patterns or trends in air quality throughout the months, such as when benzene levels are highest or lowest. This can be useful for understanding pollution sources and assessing health risks.

## Findings:

The maximum (purple) part of the bar plot shows that Benzene concentrations increased from September to November 2004, then decreased from December 2004 to February 2005, suggesting higher levels in winter and lower in summer. The monthly averages stay fairly consistent. There was a sudden spike in the minimum concentration of Benzene in July 2004, more than double the previous month, and it gradually decreased through November 2004, though still higher than usual in July, August, and September.

```{r, warning = FALSE, message = FALSE}
Time_data3 = select(data, Date, Time,`CO(GT)`, `NOx(GT)`)
Time_data3 = na.omit(Time_data3)
sum(is.na(Time_data3))

```
The above code selects Date, Time, CO(GT), NOx(GT) and then removes all the "NA" values from the selected data and return the total count of "NA" values.

```{r, warning=FALSE, message=FALSE}
cor_value= cor(Time_data3$`CO(GT)`, Time_data3$`NOx(GT)`, use = "complete.obs")
plot3 = ggplot(data = Time_data3, mapping = aes(x = `CO(GT)`, y = `NOx(GT)`)) + geom_point(color = 'purple', alpha = 0.3)+ geom_smooth(method = 'lm', color = 'orange') + theme_classic() + labs(title = "CO and NOx: Strongly Aligned (r = 0.795)", x = "Ground Truth Hourly-Averaged CO Concentrations", y = "Hourly-Averaged NOx(GT) Concentrations", caption = "CO : Carbon Monoxide\nNOx : Nitrogen Oxides\nGT : Ground Truth\nSource : Air Quality Dataset") + 
  
theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 45, t = 5)), axis.title.y = element_text(margin = margin(r = 20, l = 5), vjust = 1), axis.title.x = element_text(margin = margin(t = 20, b = 20)), axis.line = element_blank(), axis.ticks = element_line(color = 'lightgray'),axis.ticks.x = element_blank(), panel.grid.major.y = element_line(color = 'lightgray'), plot.margin = margin(r = 45))

print(plot3)
print(paste("The correlation between Carbon Monoxide and Nitrogen Oxides is: ",cor_value))

ggsave("Correlation_Scatter_plot.png", plot = plot3, width = 8, height = 6, dpi = 300)
```

## Motivation : 

Since both CO and NOx are commonly produced by similar sources, such as vehicle emissions, the scatter plot helps in understanding how their levels are related. By calculating the correlation, it becomes easier to determine if higher concentrations of one pollutant tend to occur alongside the other.

## Findings : 

Yes, there is a positive correlation between Hourly-Averaged concentration of Carbon Monoxide and Hourly-Averaged Concentration of Nitrogen Oxides 0f 0.795, which is very high. This suggests that as the Concentration of one pollutant increases the concentration of other pollutant increases as well, and this clear by the scatter plot.

```{r, warning = FALSE, message = FALSE}
Time_data4 = select(data, Date, Time,`CO(GT)`,  `C6H6(GT)`, `NO2(GT)`,`NOx(GT)`, `NMHC(GT)`)
Time_data4 = drop_na(Time_data4, Date, Time,`CO(GT)`, `NOx(GT)`, `NO2(GT)`, `C6H6(GT)`, `NMHC(GT)`)
Time_data4 = mutate(Time_data4, Year = format(Date, "%Y"))
Time_data4 = head(Time_data4, 5177)
Time_data4 = pivot_longer(Time_data4, `NOx(GT)`: `NMHC(GT)`, names_to ="Pollutantsg1", values_to = "Countg1")
Time_data4 = pivot_longer(Time_data4, `CO(GT)` : `C6H6(GT)`, names_to = "Pollutantsg2",values_to = "countg2")
Time_data4 = pivot_longer(Time_data4, `NO2(GT)`, names_to = "Pollutantsg3", values_to = "countg3")
sum(is.na(Time_data4))
```
The above code selects the Date, Time, CO(GT), C6H6(GT), NO2(GT), NOx(GT), NMHC(GT), removes all the "NA" values, creates a separate Year column, because the scales of different pollutants are different so here the pivot_longer() function has been used thrice to separate the pollutants together with similar range of values to plot them separately, and returns the count of "NA" values.  


```{r, warning = FALSE, message = FALSE}
Time_data4$Pollutantsg1 = factor(Time_data4$Pollutantsg1)
Time_data4$Pollutantsg2 = factor(Time_data4$Pollutantsg2)
Time_data4$Pollutantsg3 = factor(Time_data4$Pollutantsg3)


p = ggplot(data = Time_data4, aes(x = Pollutantsg1, y = Countg1, fill = Pollutantsg1)) + geom_boxplot() + labs(caption = "NMHC(GT) : Non-Methane Hydrocarbons\nNOx(GT) : Nitrogen Oxides\n(GT) : Ground Total", y = "Count") + theme_classic() +theme(axis.title.x = element_blank(), legend.position = "none", plot.caption = element_text(size = 8), axis.text.x = element_text(size = 8, face = "bold"), plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white", color = "white"), axis.text = element_text(color = "black")) + scale_fill_manual(values = c("NMHC(GT)" = "green", "NOx(GT)" = "darkgreen"))



p1 = ggplot(data = Time_data4, aes(x = `Pollutantsg2`, y = countg2, fill = Pollutantsg2)) + geom_boxplot() + labs(caption = "CO(GT) : Carbon Monoxide\nC6H6(GT) : Benzene") + theme_classic() +theme(axis.title.x = element_blank(), legend.position = "none", axis.title.y = element_blank(), plot.caption = element_text(size = 8), axis.text.x = element_text(size = 8, face = "bold"), plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = 'white', color = "white"), axis.text = element_text(color = "black")) + scale_fill_manual(values = c("C6H6(GT)" = "purple", "CO(GT)" = "pink"))




p2 = ggplot(data = Time_data4, aes(x = `Pollutantsg3`, y = countg3, fill = Pollutantsg3)) + geom_boxplot() + labs(caption = "NO2(GT) : Nitrogen Dioxide\nSource : Air Quality Dataset") + theme_classic()+theme(axis.title.x = element_blank(), legend.position = "none", axis.title.y = element_blank(), plot.caption = element_text(size = 8), axis.text.x = element_text(size = 8, face = "bold"), plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white", color = "white"), axis.text = element_text(color = "black")) + scale_fill_manual(values = c("NO2(GT)" = "orange"))

plot4 = wrap_plots(p, p1, p2, nrow = 1) + plot_annotation(title = "Understanding Pollution Levels: Variability Across Major Air Contaminants", theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 25, t = 5)), plot.background = element_rect(fill = "white", color = "black", size = 1.5)))

print(plot4)

ggsave("Boxplot.png", plot = plot4, width = 8, height = 6, dpi = 300)
```
## Motivation : 

This Boxplot is to analyze and compare the distribution of pollutants like NMHC, NOx, C6H6, CO, and NO2. It helps to easily identify key statistics such as the median, range, and variability of each pollutant, as well as detect any outliers or unusual patterns. This visualization allows for a clear comparison of how different pollutants behave over time and highlights any significant differences or trends in their levels.

## Findings :

NMHC and C6H6 show high variability and frequent outliers, indicating potential pollution events, CO and NOx are more stable, with less outliers and lower variability, and NO2 has moderate variability but appears more consistent compared to NMHC and C6H6.

```{r, warning = FALSE, message = FALSE}
Time_data5 = select(data, `CO(GT)`, `NOx(GT)`, `NO2(GT)`, `NMHC(GT)`, `C6H6(GT)`, `T`, RH, AH)
Time_data5 = na.omit(Time_data5)
sum(is.na(Time_data5))
```
The above code selects the required columns and remove all the "NA" values and returns the total count of "NA" values as 0.

```{r, warning = FALSE, message = FALSE}
correlation_matrix = cor(Time_data5, use = "complete.obs")
correlation_long = melt(correlation_matrix)
colnames(correlation_long) = c("Variable1", y = "Variable2", "correlation")


plot5 = ggplot(data = correlation_long, aes(x = Variable1, y = Variable2, fill = correlation)) + geom_tile(color = "white") + labs(title = "Correlation Among Key Air Quality Indicators", caption = "Source: Air Quality Dataset\nNMHC : Non-Methane Hydrocarbons, NOx : Nitrogen Oxides\nCO : Carbon Monoxide, C6H6 : Benzene, NO2 : Nitrogen Dioxide\nT : Temperature, RH : Relative Humidity, AH : Absolute Humidity\n(GT) : Ground Total") + scale_fill_gradient2(low = "white", mid = "pink",high = "purple", midpoint = 0) + theme_minimal()  +
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 3) +theme_minimal() +labs(title = "Strong Connections in the Air: Pollutants, Temperature, and Humidity",fill = "correlation") +
  
  
theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank(), plot.title = element_text(face = "bold",hjust = 0.5, margin = margin(b = 20, t = 5)), plot.background = element_rect(fill = "#FAF9F6", color = "black", size = 1.5), legend.position = "none", plot.margin = margin(t = 10,r = 45, l = 5), axis.ticks = element_blank(), plot.caption = element_text(margin = margin(b = 5)), axis.text = element_text(color = "black"))

print(plot5)

ggsave("Correlation_Heatmap.png", plot = plot5, width = 8, height = 6, dpi = 300)
```

## Motivation : 

This plot is to understand how air pollutants are connected to each other, weather conditions like temperature, humidity, and moisture in the air. By seeing these relationships, we can gain insights into how factors like heat and humidity might influence pollution levels, helping us better understand and manage air quality.

## Findings : 

The findings reveal strong connections between different pollutants. For instance, C6H6 (Benzene) is closely linked to CO, NOx, NO2, and NMHC, meaning these pollutants often rise and fall together. NOx and CO also have a strong bond, showing they’re related in pollution patterns. On the weather side, the negative correlation between Temperature and Humidity suggests that as temperatures rise, humidity tends to drop. In conclusion, this heatmap highlights how air pollutants and weather factors are interconnected, giving us valuable insights into how they influence each other.


```{r, warning = FALSE, message = FALSE}

Time_data6 = select(data, `CO(GT)`, `NOx(GT)`)
Time_data6 = na.omit(Time_data6)
Time_data6 = filter(Time_data6, `NOx(GT)` <= 600, `CO(GT)` <=5)
sum(is.na(Time_data6))
```
The above code selects the required columns from the original data and removes all the "NA" values with the total count of "NA" values left after removal as 0.

```{r, warning = FALSE, message = FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
p_main = ggplot(Time_data6, aes(x = `CO(GT)`, y = `NOx(GT)`)) + stat_density2d_filled(na.rm = T) + scale_y_continuous(limits = c(0, 610)) + scale_x_continuous(limits = c(0, 5)) + labs(x = "Carbon Monoxide Concentration", y = "Nitrogen Oxides Concentration")+ theme_classic() + scale_fill_viridis_d(option = 'viridis') + theme(legend.position = "none", axis.title.y = element_text(margin = margin(r = 15, l = 20)), axis.title.x = element_text(margin = margin(t = 10)), panel.background = element_rect(fill = "beige",color = "black", size = 1.5), plot.margin = margin(t = -40, r = -40), )

p1 = ggplot(data = Time_data6) + geom_density(aes(x = `CO(GT)`), fill = "pink", color = "black") + theme_void() + scale_x_continuous(limits = c(0, 5)) + theme(plot.margin = margin(b = -20))

p2 = ggplot(data = Time_data6) + geom_density(aes(x = `NOx(GT)`), fill = "purple", alpha = 0.5, color = "black") + coord_flip() + theme_void()

plot6 = wrap_plots(p1, plot_spacer(), p_main, p2, nrow = 2, widths = c(1, 0.15), heights = c(0.15, 1)) + plot_annotation(title = "Pollutant Hotspots: A Visual Exploration of Concentration Density", caption = "Source: Air Quality Dataset", theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(t = 5, b = 20))))

print(plot6)

ggsave("Combined_Density_plot.png", plot = plot6, width = 8, height = 6, dpi = 300)
```

## Motivation : 

This plot is to visually explore the relationship between Carbon Monoxide (CO) and Nitrogen Oxides (NOx) concentrations. By using a density plot, we can easily see where the highest concentrations of both pollutants occur and how they are distributed across different values. This helps us understand if there’s a pattern or connection between CO and NOx levels.

## Findings : 

From the density plot, we can conclude that Carbon Monoxide (CO) and Nitrogen Oxides (NOx) concentrations are mostly centered around values of 0.8 for CO and 50 for NOx. The concentration of CO ranges from 0.5 to 1.3, with the highest density occurring around the center (0.8). As the concentration increases or decreases from this center, the density gradually decreases. Similarly, for NOx, the highest density is around 50, and it gradually decreases as the concentration moves away from this value.

The plot shows that both pollutants have a central concentration point, with the density decreasing as the values deviates from these central concentrations. This indicates that most of the values fall within a specific range for both CO and NOx.


```{r, warning = FALSE, message = FALSE}
Time_data7 = select(data, Date, `CO(GT)`,`NO2(GT)`, `NOx(GT)`)
Time_data7 = mutate(Time_data7, Day = format(Date, "%d"), month = format(Date, "%m"), year = format(Date, "%Y"))
Time_data7 = na.omit(Time_data7)
Time_data7 = group_by(Time_data7, Day, month ,year)
Time_data7 = summarise(Time_data7, CO = mean(`CO(GT)`), NO2 = mean(`NO2(GT)`), NOx = mean(`NOx(GT)`), .groups = "drop")
Time_data7 = arrange(Time_data7,year, month, Day)
Time_data7 = mutate(Time_data7, Date = as.Date(paste(year, month, Day, sep = "-")), monthyear = as.Date(paste(year, month, "1", sep = "-")))
sum(is.na(Time_data7))
```
The above code selects the desired columns for the visualization, created a new column of day, month, and year to at the last create a MonthYear column for using it as x axis, removes all the "NA" values, groups the data by day, month, and year, summarizes the data by taking mean, and finally arranging the data Date wise and returning the total count of "NA" values.

```{r, warning = FALSE, message = FALSE}
plot7 = ggplot(data = Time_data7) + geom_area(aes(x = Date, y = NOx, fill = "NOx"), color = "black", size = 0.55, alpha = 0.4) +

geom_area(aes(x = Date, y = NO2, fill = "NO2"), color = "black", size = 0.55, alpha = 0.4) + scale_x_date(breaks = as.Date(c("2004-03-01", "2004-04-01", "2004-05-01", "2004-06-01", "2004-07-01", "2004-08-01", "2004-09-01", "2004-10-01", "2004-11-01", "2004-12-01", "2005-01-01", "2005-02-01", "2005-03-01", "2005-04-01")), date_labels = "%b %Y") + scale_fill_manual(values = c("NOx" = "deeppink", "NO2" = "purple"), name = "Pollutant", labels = c("NOx", "NO2")) +  theme_minimal() + labs(title = "From Summer to Winter: How NOx and NO2 Fluctuate Month by Month", x = "Month", y = "Pollutant Concentration", caption = "NOx : Nitrogen Oxides\nN02 : Nitrogen Dioxide\nSource : Air Quality Dataset") + 
  
theme(axis.text.x = element_text(angle = 45, hjust = 0.5, margin = margin(t = 8)), panel.grid.minor.x = element_blank(), plot.background = element_rect(fill = "#FAF9F6", color = "black", size = 1.5), plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(t = 7, b = 20)), axis.title.y = element_text(margin = margin(r = 15, l = 8), hjust = 0.5), axis.title.x = element_text(margin = margin(b = 5)), plot.margin = margin(r = 5), plot.caption = element_text(margin = margin(b = 5)), panel.grid = element_line(color = "white"), axis.text = element_text(color = "black"))

print(plot7)

ggsave("Area_plot.png", plot = plot7, width = 8, height = 6, dpi = 300)
```
## Motivation : 

This area plot is to visualize how Nitrogen Oxides and Nitrogen Dioxide concentrations change throughout the year. By showing the monthly variations, we can easily spot patterns and trends in pollution levels, such as higher concentrations during certain months. This is important because it helps us understand how seasonal factors, like weather or human activity, impact air quality.

## Findings : 

The area plot shows a sharp rise in Nitrogen Oxides starting in September 2004. Before that, levels were under 250. From September to December 2004, they surged from 450 to 825, staying high into early 2005, then gradually dropped back to 500 by March and stayed similar through April 2005.

For Nitrogen Dioxide, levels stayed around 125 until July 2004, then dropped to 75, before steadily increasing to 250 by February 2005, and then began to decline.

```{r, warning = FALSE, message = FALSE}

Time_data8 = select(data, Date,`NOx(GT)`, RH, AH, T)
Time_data8 = na.omit(Time_data8)
Time_data8 = Time_data8%>%
  group_by(Date)%>%
  summarise(avgNOx = mean(`NOx(GT)`), avgRH = mean(RH), avgAH = mean(AH), avgT = mean(T))
```

The above code selects the required data for the Bubble plot and removes all the "NA" values, groups the data by Date and summarizes the selected columns by Daily mean.

```{r, warning = FALSE, message = FALSE}
plot8 = ggplot(data = Time_data8, mapping = aes(x = avgT, y = avgRH, color = avgAH, size = avgNOx)) + geom_point(alpha = 0.6)  +  scale_color_gradient(low = "deeppink", high = "purple") + labs(title = "The Climate Connection: How Temperature and Humidity Shape NOx", x = "Average Temperature", y = "Average Relative Humidity", caption = "Nox : Nitrogen Oxides\nAH : Absolute Humidity\nSource : Air Quality Dataset", size = "Average\nNox", color = "Average\nAH") + theme_classic() + 
  
theme(plot.title = element_text(face = "bold", margin = margin(b = 15), hjust = 0.5), axis.title.y = element_text(margin = margin(r = 15, l = 3)), axis.title.x = element_text(margin = margin(t = 10)), plot.background = element_rect(fill = "#FAF9F6", color = "black", size = 1), panel.background = element_rect(fill = "#FAF9F6"), legend.background = element_rect(fill = "#FAF9F6"), legend.spacing.x = unit(1, "lines"), legend.box.margin = margin(t = 75, b = 25), axis.line = element_line(color = "black"), axis.text = element_text(color = "black"))

print(plot8)

ggsave("Bubble_plot.png", plot = plot8, width = 8, height = 6, dpi = 300)
```
## Motivation : 

This bubble plot is to explore how temperature and humidity interact with Nitrogen Oxides (NOx) levels and Absolute Humidity (AH). By using bubble size to represent Average Nitrogen Oxides and color to show Average Absolute Humidity, the plot helps us visually uncover patterns and relationships between these variables.

## Findings : 

The data shows that when temperature is low (around 20°C or less), Nitrogen Oxides are higher, and as temperature rises above 20°C, Nitrogen Oxides decrease.

For Absolute Humidity (AH), it’s low at cooler temperatures but increases as temperature rises, showing a positive link between temperature and humidity.

The bubble plot reveals that cooler temperatures have higher Nitrogen Oxides, while warmer temperatures bring higher humidity. This suggests that weather and seasonal changes influence air pollution levels.

```{r, warning = FALSE, message = FALSE}
Time_data9 = select(data, Date, `PT08.S5(O3)`, `NOx(GT)`)
Time_data9 = na.omit(Time_data9)
Time_data9 = group_by(Time_data9, Date)
Time_data9 = summarise(Time_data9, avgNox = mean(`NOx(GT)`), avgO3 = mean(`PT08.S5(O3)`))
Time_data9 = mutate(Time_data9, month = format(Date, "%m"), Year = format(Date, "%Y") ,MonthYear = as.Date(paste(Year, month, "1", sep = "-")))
Time_data9$MonthYear = format(Time_data9$MonthYear, "%b %Y")
Time_data9 = head(Time_data9, 314)
Time_data9$MonthYear = factor(Time_data9$MonthYear, levels = unique(Time_data9$MonthYear))
sum(is.na(Time_data9))
```

The code selects the necessary columns, removes null values, groups data by Date, and calculates the daily mean. It then creates a MonthYear column, selects specific rows, converts MonthYear to a factor for facet_wrap(), and returns the total count of "NA" values.

```{r, warning = FALSE, message = FALSE}
plot9 = ggplot(data = Time_data9, mapping = aes(x = avgNox, y = avgO3)) + geom_point(alpha = 0.5) + geom_smooth(method = "lm", color = "darkorange", se = F) +    theme_bw() +facet_wrap(vars(factor(MonthYear))) + labs(title =  "The NOx Factor: How Nitrogen Oxides Shape Ozone Levels", x = "Average Nitrogen Oxides Concentration", y = "Average Ozone Levels", caption = "Nox : Nitrogen Oxides\nSource : Air Quality Dataset") +
  
theme(plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(t = 5, b = 15)), plot.background = element_rect(color = "black", fill = "beige", size = 1), axis.title.y = element_text(margin = margin(l = 5, r= 10)), axis.title.x = element_text(margin = margin(t = 10, b = 5)), plot.margin = margin(r = 50), panel.background = element_rect(fill = "white"), strip.background = element_rect(fill = "#9933CC"), strip.text = element_text(face = "bold", color = "beige"), axis.text = element_text(color = "black"), plot.caption = element_text(margin = margin(b = 5)), panel.grid = element_line(color = "beige"), panel.grid.minor = element_blank())
print(plot9)

ggsave("Facet_Wrap.png", plot = plot9, width = 8, height = 6, dpi = 300)
```
## Motivation : 

This scatter plot facet wrap is to explore how average monthly Nitrogen Oxides level affects average ozone levels throughout the year. By breaking the data down by month, we can identify seasonal patterns and trends, helping us understand if and how Nitrogen Oxides influences ozone concentrations over time.

## Findings : 


Generally, when in summers the Nitrogen Oxides count is low temperature plays a role in the variation of ozone as temperature rises the ozone concentration also rises. However, in winters when the temperature is low and Nitrogen Oxides concentration is at higher levels as compared to summer, the rise in ozone levels is caused by the increase of Nitrogen Oxides concentrations. This suggests that only temperature and only Nitrogen Oxides alone do not cause the variation instead it's an inverse relationship between Temperature and Nitrogen Oxides that leads to variation in Ozone.

```{r, warning = FALSE, message = FALSE}
Time_data10 = select(data, Date, `T`)
Time_data10[Time_data10 < 0] = NA
Time_data10 = na.omit(Time_data10)
Time_data10 = group_by(Time_data10, Date)
Time_data10 = summarise(Time_data10, minT = min(T), maxT = max(T))
Time_data10 = mutate(Time_data10, Month = format(Time_data10$Date, "%b %Y"))
Time_data10 = Time_data10[47:152, ]
sum(is.na(Time_data10))
```

The above code selects the desired columns, removes all the "NA" values, groups the data by Date and summarizes the the data by minimum and maximum temperature, retains the specific amount of rows, and return the total count of "Na" values left after the removal of the "NA" values.

```{r, warning = FALSE, message = FALSE}
plot10 = ggplot(data = Time_data10, mapping = aes(x = Date)) + geom_ribbon(aes(ymin = minT, ymax = maxT), fill = "white", color = "white") + geom_line(aes(y = minT, color = "Lower Extreme"), linewidth = 0.565) + geom_line(aes(y = maxT, color = "Upper Extreme"), linewidth = 0.565) + scale_x_date(breaks = as.Date(c("2004-05-01", "2004-06-01", "2004-07-01", "2004-08-01", "2004-09-01")),labels = c("May 2004", "Jun 2004", "Jul 2004", "Aug 2004", "Sep 2004")) +labs(title = "Summer Heatwave: Exploring Daily Temperature Extremes", x = "Month", y = "Temperature(°C)", color = "Temperature Extremes", caption = "Source : Air Quality Dataset") + scale_color_manual(values = c("Lower Extreme" = "purple", "Upper Extreme" = "orange")) + theme_minimal() +
  

theme(plot.title = element_text(color = "black", margin = margin(b = 15, t = 5), hjust = -1.2, face = "bold"), axis.title.y = element_text(color = "black",margin = margin(r = 13, l = 5)), axis.title.x = element_text(color = "black", margin = margin(t = 10, b = 5)), plot.caption = element_text(color = "black", margin = margin(b = 5)), panel.grid.minor = element_blank(), plot.background = element_rect(fill = "beige", color = "black", size = 1), panel.grid = element_line(color = "black"), axis.text = element_text(color = "black"))

print(plot10)

ggsave("Ribbon_Plot.png", plot = plot10, width = 8, height = 6, dpi = 300)
```

## Motivation : 

This plot is to reveal the daily fluctuations of summer temperatures—how the heat peaks during the day and cools off at night. It gives a clear view of the highs and lows, helping us understand the patterns of summer's intensity.

## Findings :

The plot shows a steady rise in temperatures from May to September 2004. During June, July, and August, temperatures exceeded 40°C, which could be indicative of a heatwave. By September, the temperatures began to cool down, marking the end of summer's intense heat. This pattern could be influenced not just by seasonal changes but also by the availability of pollutants and other environmental factors shaping the local climate, making the temperature hit more than 40°C.
