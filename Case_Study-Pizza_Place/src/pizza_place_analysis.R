install.packages("dplyr")
install.packages("janitor")
install.packages("lubridate")
install.packages("skimr")
install.packages("tidyverse")
install.packages("ggrepel")
install.packages("ggsci")

library(dplyr)
library(janitor)
library(lubridate)
library(skimr)
library(tidyverse)
library(scales)
library(ggrepel)
library(ggsci)

order_details <- read_csv("order_details.csv")
orders <- read_csv("orders.csv")
pizza_types <- read_csv("pizza_types.csv")
pizzas <- read_csv("pizzas.csv")

glimpse(order_details)
glimpse(orders)
glimpse(pizza_types)
glimpse(pizzas)

sum(order_details[duplicated(order_details), ])
sum(orders[duplicated(orders), ])
sum(pizza_types[duplicated(pizza_types), ])
sum(pizzas[duplicated(pizzas), ])

sum(is.na(order_details))
sum(is.na(orders))
sum(is.na(pizza_types))
sum(is.na(pizzas))

# check table limits
min(order_details$quantity)
max(order_details$quantity)

min(orders$date)
max(orders$date)

# check times in ascending and descending times
# to see if range is between 0-23
orders %>%
  arrange(time)
orders %>%
  arrange(desc(time))

pizza_info_merged <- pizzas %>%
  full_join(pizza_types, by = "pizza_type_id")

sum(pizza_info_merged[duplicated(pizza_info_merged), ])
sum(is.na(pizza_info_merged))

order_info_merged <- order_details %>%
  full_join(orders, by = "order_id")

sum(order_info_merged[duplicated(order_info_merged), ])
sum(is.na(order_info_merged))

write.csv(pizza_info_merged,
  "~/Code/github/Case_Study-Pizza_Place/data/processed/pizza_info_merged.csv")

write.csv(order_info_merged,
  "~/Code/github/Case_Study-Pizza_Place/data/processed/order_info_merged.csv")

# get all order info entries with pizza info
yearly_data <-
  left_join(order_info_merged, pizza_info_merged, by = "pizza_id")

sum(is.na(yearly_data))

write.csv(yearly_data,
          "~/Code/github/Case_Study-Pizza_Place/data/processed/yearly_data.csv")

rm(order_details, orders, pizza_types, pizzas)

# check if dfs contain same data
order_info_merged %>%
  count(order_id, name = "order_qty")

yearly_data %>%
  count(order_id, name = "order_qty")

yearly_data_breakdown <- yearly_data %>%
  group_by(order_id, category, name, size) %>%
  summarise(total = sum(price),
            qty = sum(quantity))

# get order sum and pizza quantity, printing shows rounded number
order_totals <- yearly_data %>%
  group_by(order_id) %>%
  summarise(total = sum(price),
            qty = sum(quantity))

# check order sum
yearly_data %>%
  filter(order_id == 16)

# average amount of pizzas sold per id
order_totals %>%
  summarise(year_qty_total = sum(qty, na.rm = TRUE),
            avg_order_qty = round(mean(qty, na.rm = TRUE)))

yearly_data %>%
  count(category)

yearly_data %>%
  group_by(name) %>%
  summarise(sum(quantity))

yearly_data %>%
  group_by(order_id) %>%
  summarise(total = sum(price),
            qty = sum(quantity))

yearly_data %>%
  group_by(name, size) %>%
  summarise(qty = sum(quantity)) %>%
  arrange(desc(qty))

# add week number to use weekly data
yearly_data <- yearly_data %>%
  mutate(week_num = week(as.Date(yearly_data$date, format = "%Y-%m-%d")))

# add an hour column
yearly_data$hour <- strftime(yearly_data$time, format = "%H")

# or just summarise?
yearly_data %>%
  mutate(strftime(yearly_data$time, format = "%H")) %>%
  group_by(date, hour) %>%
  summarise(count = n(),
            qty = sum(quantity))

# show sales $ by month
monthly_sales_data <- yearly_data %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(total_sales_revenue = sum(price),
            qty_of_pizzas_sold = sum(quantity))

monthly_pizza_types <- yearly_data %>%
  group_by(category, month = floor_date(date, "month")) %>%
  summarise(qty = sum(quantity))

# yearly data plots
size_plot <- yearly_data %>%
  group_by(size) %>%
  summarise(qty = sum(quantity))

ggplot(size_plot, aes(x = factor(size, level = c("S", "M", "L", "XL", "XXL")),
                      y = qty, fill = size)) +
  geom_col() +
  geom_text(aes(label = qty),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  theme(legend.position = "none") +
  xlab("Size") +
  ylab("Quantity Sold") +
  ggtitle("Yearly Sales by Pizza Size") +
  theme(plot.title = element_text(hjust = 0.5))

# monthly data plots
ggplot(monthly_sales_data,
       aes(x = as.factor(format(month, format = "%b")),
           total_sales_revenue, fill = qty_of_pizzas_sold)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("Sales Revenue") +
  scale_y_continuous(limits = c(0, 75000),
                     breaks = c(0, 15000, 25000, 35000, 45000, 55000, 65000, 75000)) +
  scale_x_discrete(limits = month.abb) +
  theme(legend.position = "none")

ggplot(monthly_pizza_types,
      aes(fill = category, x = as.factor(format(month, format = "%b")), qty)) +
  geom_bar(position = "dodge", stat = "identity", colour = "black") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(limits = c(0, 1400),
                     breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600)) +
  scale_color_futurama() +
  xlab("Month") +
  ylab("Quantity Sold") +
  ggtitle("Monthly Pizza Sold By Category") +
  theme(plot.title = element_text(hjust = 0.5))

# weekly data plots
# weekly view of sales
weekly_rev_sales_data <- yearly_data %>%
  group_by(week_num) %>%
  summarise(total_sales_revenue = sum(price),
            qty_of_pizzas_sold = sum(quantity))

weekly_pizza_sales <- yearly_data %>%
  group_by(week_num, name, size) %>%
  summarise(pizza_count = sum(quantity),
            total_sales_rev = sum(price))

weekly_rev_sales_data %>%
  summarise(min_weekly_sales = min(total_sales_revenue),
            max_weekly_sales = max(total_sales_revenue),
            min_qty_sold = min(qty_of_pizzas_sold),
            max_qty_sold = max(qty_of_pizzas_sold))

# show weekly revenue
ggplot(weekly_rev_sales_data, aes(week_num, total_sales_revenue)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 55, by = 5)) +
  ggtitle("Weekly Sales Revenue") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(weekly_rev_sales_data, aes(week_num, qty_of_pizzas_sold)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 55, by = 5)) +
  ggtitle("Weekly Pizza Quantities") +
  theme(plot.title = element_text(hjust = 0.5))
