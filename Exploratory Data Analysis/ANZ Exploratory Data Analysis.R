library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(lubridate)
library(tidygeocoder)
library(leaflet)
library(ggthemes)
library(htmlwidgets)

transaction <- readxl::read_xlsx("ANZ synthesised transaction dataset.xlsx")

summary(transaction)
str(transaction)

# 100 customers were involved in transactions in the three months period
transaction %>%
  group_by(account) %>%
  summarize()

#Total average transaction amount across the month period
mean(transaction$amount)

# August Transactions
august_transaction <- transaction %>%
  filter(date >= as.Date("2018-08-01") & date < as.Date("2018-09-01"))

# Average transaction amount across the month of August
mean(august_transaction$amount)

#September Transactions
september_transaction <- transaction %>%
  filter(date >= as.Date("2018-09-01") & date < as.Date("2018-10-01"))

# Average transaction amount across the month of September
mean(september_transaction$amount)

#October Transactions
october_transaction <- transaction %>%
  filter(date >= as.Date("2018-10-01") & date <= as.Date("2018-10-31"))

# Average transaction amount across the month of September
mean(october_transaction$amount)

# Number of transactions in August by each customer
num_of_august_transactions <- august_transaction %>%
  group_by(customer_id) %>%
  count() %>%
  rename(number_of_transactions = n)

# Average transactions by customer in August
mean(num_of_august_transactions$number_of_transactions)

# Number of transactions in September by each customer
num_of_september_transactions <- september_transaction %>%
  group_by(customer_id) %>%
  count() %>%
  rename(number_of_transactions = n)

# Average transactions by customer in September
mean(num_of_september_transactions$number_of_transactions)

# Number of transactions in October by each customer
num_of_october_transactions <- october_transaction %>%
  group_by(customer_id) %>%
  count() %>%
  rename(number_of_transactions = n)

# Average transactions by customer in September
mean(num_of_october_transactions$number_of_transactions)

# total number of transactions per day starting from the highest to the lowest
transactions_per_day <- transaction %>%
  group_by(date) %>%
  count() %>%
  arrange(desc(n)) %>%
  rename(number_of_transations_per_day = n)

summary(transactions_per_day)

# Average number of transactions per day
mean(transactions_per_day$number_of_transations_per_day)

# The average num ber of transactions per d6ay is approximately 132

# Finding the date with 132 transactions per day
# The was no day with 132 transactions per day so I find the date with the closest number of transactions to 132
transactions_per_day %>%
  filter(number_of_transations_per_day >= 130 & number_of_transations_per_day < 135)

# I choose 2018-10-21 as the average day of transactions. There were 133 transactions that day

avg_day_transaction <- transaction %>%
  filter(date == as.Date("2018-10-21")) %>%
  mutate(hour = lubridate::hour(extraction)) %>%
  select(first_name, account, customer_id, transaction_id, amount, hour)

# Amount involved in each transactions on 2018-10-21
ggplot(avg_day_transaction, aes(x = transaction_id, y = amount)) +
  geom_bar(stat = "identity", fill = "#007dba") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.05, size = 5), axis.text.y = element_text(size = 30), plot.title = element_text(size = 50)) +
  scale_y_continuous(breaks = seq(0, 8000, by = 500)) +
  labs(x = "Transaction ID",
       y = "Amounts",
       title = "Amount Involved in each Transaction on 2018-10-21")
  

# Transaction amount by each customer on 2018-10-21
avg_day_transaction %>%
  group_by(account) %>%
  summarise(total_amount = sum(amount)) %>%
  ggplot(aes(x = factor(account), y = total_amount)) +
  geom_bar(stat = "identity", fill = "#007dba") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.05), axis.text.y = element_text(size = 30), plot.title = element_text(size = 50)) +
  scale_y_continuous(breaks = seq(0, 8000, by = 500)) +
  labs(x = "Account Number",
       y = "Number of Transactions",
       title = "Total Transaction Amount Per Customer on 2018-10-21")

# Total Number of Transactions Per Customer on 2018-10-21
ggplot(avg_day_transaction, aes(x = factor(account))) +
  geom_bar(fill = "#007dba", size = 50) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.05, size = 30), axis.text.y = element_text(size = 30), plot.title = element_text(size = 50)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  labs(x = "Account Number",
       y = "Number of Transactions",
       title = "Total Number of Transactions Per Customer on 2018-10-21")


# Total transactions by hour on 2018-10-21
avg_day_transaction %>%
  group_by(hour) %>%
  count() %>%
  ggplot(aes(x = hour, y = n)) +
  geom_line(color = "#007dba") +
  theme_economist() +
  scale_x_continuous(breaks = seq(0, 24, by = 1)) +
  scale_y_continuous(breaks = seq(0, 20, by = 1)) +
  labs(x = "Hour",
       y = "Number of Transactions",
       title = "Total Number of Transactions Per Hour on 2018-10-21")

# total transactions amount by hour on 2018-10-21
avg_day_transaction %>%
  group_by(hour) %>%
  summarise(total_amount_by_hour = sum(amount)) %>%
  ggplot(aes(x = hour, total_amount_by_hour)) +
  geom_line(color = "#007dba") +
  theme_economist() +
  scale_x_continuous(breaks = seq(0, 24, by = 1)) +
  scale_y_continuous(breaks = seq(0, 8000, by = 500)) +
  labs(x = "Hour",
       y = "Amount",
       title = "Total Transactions Amount Per Hour on 2018-10-21")


top_10_transactions_with_the_highest_amount <- transaction %>%
  mutate(hour = lubridate::hour(extraction), day = lubridate::day(extraction), month = lubridate::month(extraction)) %>%
  select(first_name, account, customer_id, transaction_id, amount, hour, day, month) %>%
  arrange(desc(amount), 5)


# Total number of transactions by customer
# There were 100 customers that made transactions

total_number_of_transactions_by_customer <- transaction %>%
  select(first_name, account, customer_id, transaction_id, amount) %>%
  group_by(first_name, account, customer_id) %>%
  count() %>%
  rename(total_transactions = n) %>%
  arrange(desc(total_transactions))
  
ggplot(total_number_of_transactions_by_customer, aes(x = factor(account), y = total_transactions)) +
  geom_bar(stat = "identity", fill = "#007dba") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.05), 
        axis.text.y = element_text(size = 30), 
        plot.title = element_text(size = 50),
        axis.title = element_text(size = 30)) +
  scale_y_continuous(breaks = seq(0, 1000, by = 100)) +
  labs(x = "Account Number",
       y = "Number of Transactions",
       title = "Total Number of Transactions Per Customer")

# Total amount of transactions by customer

total_amount_of_transactions_by_customer <- transaction %>%
  select(first_name, account, customer_id, amount) %>%
  group_by(first_name, account, customer_id) %>%
  summarise(total_amount = sum(amount)) %>%
  arrange(desc(total_amount))
  

ggplot(total_amount_of_transactions_by_customer, aes(x = factor(account), y = total_amount)) +
  geom_bar(stat = "identity", fill = "#007dba") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.05), 
        axis.text.y = element_text(size = 30), 
        plot.title = element_text(size = 50), 
        axis.title = element_text(size = 30)) +
  scale_y_continuous(breaks = seq(0, 50000, by = 5000)) +
  labs(x = "Account Number",
       y = "Amount",
       title = "Total Amount of Transactions Per Customer")


# Remove the letter "T" from all fields in the extraction column
transaction$extraction <- gsub("T", " ", transaction$extraction)

# convert extraction column to a date time variable
transaction$extraction <- as.POSIXct(transaction$extraction, tz = "UTC", format="%Y-%m-%d %H:%M:%S")


# Total number of transactions per month
total_transactions_by_month <- transaction %>%
  group_by(month = lubridate::month(extraction)) %>%
  summarise(number_of_transactions=n()) %>%
  arrange(desc(number_of_transactions))

# Total number of transactions that occured in each day from 1-31
total_transactions_by_day <- transaction %>%
  group_by(day = lubridate::day(extraction)) %>%
  summarise(number_of_transactions=n()) %>%
  arrange(desc(number_of_transactions))

ggplot(total_transactions_by_day, aes(x = day, y = number_of_transactions)) +
  geom_line(size = 3, color = "#007dba") +
  theme_economist() +
  theme(axis.text = element_text(size = 30), 
        plot.title = element_text(size = 50), 
        axis.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(1, 31, by = 1)) +
  scale_y_continuous(breaks = seq(100, 500, by = 20)) +
  labs(x = "Day",
       y = "Number of Transactions",
       title = "Number of Transactions Per Day")

# Total number of transactions that occured each hour
total_transactions_by_hour <- transaction %>%
  group_by(hour = lubridate::hour(extraction)) %>%
  summarise(number_of_transactions=n()) %>%
  arrange(desc(number_of_transactions))

ggplot(total_transactions_by_hour, aes(x = hour, y = number_of_transactions)) +
  geom_line(size = 3, color = "#007dba") +
  theme_economist() +
  theme(axis.text = element_text(size = 30), 
        plot.title = element_text(size = 50), 
        axis.title = element_text(size = 30)) +
  scale_x_continuous(breaks = seq(1, 23, by = 1)) +
  scale_y_continuous(breaks = seq(100, 1200, by = 100)) +
  labs(x = "Hour(:00)",
       y = "Number of Transactions",
       title = "Number of Transactions Per Hour")


# Separating the longitude and latitude cordinates to become two differnt variables
transaction <- tidyr::separate(transaction, long_lat, c("lng", "lat"), sep = " ")

# Converting lng and lat to numeric variables
transaction$lng <- as.numeric(transaction$lng)
transaction$lat <- as.numeric(transaction$lat)


# Reverse geocoding the longitude and latitude coordinates to get more information about the location
transaction_loc <- transaction %>%
  group_by(first_name, account, customer_id, lng, lat) %>%
  summarise() %>%
  reverse_geocode(lat=lat,
                  long=lng,
                  full_results = TRUE)

# Selecting needed variables
transaction_location <- transaction_loc[c("first_name", 
                                          "account", 
                                          "customer_id", 
                                          "lng", 
                                          "lat", 
                                          "address", 
                                          "suburb", 
                                          "city", 
                                          "municipality", 
                                          "state", 
                                          "country")]


# Assigning states to missing values in state variable

transaction_location$state[grep("Northern Territory", transaction_location$address)] <- "Northern Territory"

transaction_location$state[grep("Australian Capital Territory", transaction_location$address)] <- "Australian Capital Territory"


# Plotting a map of the locations of all 100 customers that made transactions
transaction_location_map <- transaction_location %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addCircles(lng = ~lng, 
             lat = ~lat, 
             color = "#007dba", 
             weight = 10, 
             opacity = 1, 
             popup = ~paste0("First Name: ", first_name, 
                             "<br/>Account Number: ", account, 
                             "<br/>Customer ID: ", customer_id, 
                             "<br/>Address: ", address))


# Number of Transactions by State
transaction_by_state <- transaction_location %>%
  group_by(state) %>%
  count() %>%
  rename(number_of_transactions = n)

ggplot(transaction_by_state, aes(x = state, y = number_of_transactions)) +
  geom_bar(stat = "identity", fill = "#007dba") +
  theme_economist() +
  theme(axis.text = element_text(size = 30), 
        plot.title = element_text(size = 50), 
        axis.title = element_text(size = 30)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  labs(x = "State",
       y = "Number of Transactions",
       title = "Number of Transactions by State") +
  coord_flip()

customers_salary <- transaction %>%
  filter(txn_description == "PAY/SALARY", movement == "credit") %>%
  group_by(first_name, account, age, amount) %>%
  summarise() %>%
  rename(salary = amount)

customers_salary <- left_join(customers_salary, transaction_location[c("account", "state")])

# Removing Daniel with account number ACC-2901672282 from the dataset because his location could not be found from the coordinates provided in the dataset
customers_salary <- customers_salary[!is.na(customers_salary$state),]

write_csv(customers_salary, "ANZ_customers_salary.csv")