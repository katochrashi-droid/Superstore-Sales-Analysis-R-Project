## Basic calculation========
# total sales
total_sales <- sum(data$Sales)
total_sales

# total profit
total_profit <- sum(data$Profit)
total_profit

# total quantity
 total_quantity <- sum(data$Quantity)
 total_quantity

# average sales
average_sales <- mean(data$Sales)
average_sales

# average profit
average_profit <-mean(data$Profit)
average_profit

# maximum sales
max_sales <- max(data$Sales)
max_sales

#minimum sales
min_sales <- min(data$Sales)
min_sales

# median sales
median_sales <- median(data$Sales)
median_sales

# standard Deviation of sales
sd_sales <- sd(data$Sales)
sd_sales

# Category-wise Analysis==
category_sales <- aggregate(Sales~ Category, data = data,FUN = sum)
 category_sales
  
 # Category-wise total profit==
 category_profit <- aggregate(Profit ~ Category, data = data, FUN = sum)
 category_profit
 
 # region-wise total sales==
 region_sales <- aggregate(Sales~ Region,data = data, FUN = sum)
 region_sales
 
 # region-wise total profit==
 region_profit <- aggregate(Profit ~ Region, data = data, FUN = sum)
 region_profit
 
 # top 10 product by sales-----
 product_sales <- aggregate(Sales ~ Product.Name,data = data, FUN = sum)
 top10_product <- product_sales[order(-product_sales$Sales),][1:10,]
 top10_product

 # top 10 product by profit----
 product_profit <- aggregate(Profit ~ Product.Name,data = data, FUN = sum)
 top10_profit_product <- product_profit[order(-product_profit$Profit),][1:10,]
 top10_profit_product
 
 # top 10 customers by sales---
 customer_sales <- aggregate(Sales ~ Customer.Name,data = data, FUN = sum)
 top10_customers <- customer_sales[order(-customer_sales$Sales),][1:10,]
 top10_customers
 
 # top 10 customer by profit------
 customer_profit <- aggregate(Profit ~ Customer.Name,data = data, FUN = sum)
 top10_customers_profit <- customer_profit[order(-customer_profit$Profit),][1:10,]
 top10_customers_profit
 
 # Discount analysis---
 average_discount <- mean(data$Discount)
 average_discount
 # maximum Discount--
 max_discount <- max(data$Discount)
 max_discount
 
 # category-wise average discount---
 category_discount <- aggregate(Discount ~ Category, data = data, FUN=mean)
 category_discount
 
 #Region-wise average Discount---
 region_discount <- aggregate(Discount ~ Region, data = data, FUN=mean)
 region_discount
 
 # correlation Analysis--
 sales_profit_correlation <- cor(data$Sales, data$Profit)
 sales_profit_correlation
 
 # linear Regression--
 model <- lm(Profit ~ Sales, data =data)
 summary(model)
 
 library(ggplot2)
 
 ## visualization----
 
 # category-wise total sales---
 ggplot(category_sales,aes(x = Category , y = Sales, fill = Category)) +
  geom_col() + labs(title = "Category-wise Total Sales",
                     x = "Category",
                     y = "Total Sales") +
   theme_minimal()
 
 # category-wise Total Profit
 ggplot(category_profit,aes(x = Category , y = Profit, fill = Category)) +
   geom_col() + labs(title = "Category-wise Total Profit",
                     x = "Category",
                     y = "Total Profit") +
   theme_minimal()
 
 # Region- wise Total Profit----
 ggplot(region_profit,aes(x = Region , y = Profit, fill = Region)) +
   geom_col() + labs(title = "Region-wise Total Profit",
                     x = "Region",
                     y = "Total Profit") +
   theme_minimal()
 
 # Top 10 product by sales--
  ggplot(top10_product,aes(x = reorder(Product.Name, Sales),
                           y = Sales)) + geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Top 10 Product by Sales",
         x = "Product Name",
         y = "Total sales") +
    theme_minimal()
  
  ## Top 10 customer by sales----
  ggplot(top10_customers,aes(x = reorder(Customer.Name, Sales),
                           y = Sales)) + geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Top 10 Customer by Sales",
         x = "Customer Name",
         y = "Total sales") +
    theme_minimal()
  
  # Top 10 customer by Profit---
  ggplot(top10_customers_profit,aes(x = reorder(Customer.Name, Profit),
                             y = Profit)) + geom_col(fill = "Forestgreen") +
    coord_flip() +
    labs(title = "Top 10 Customer by Profit",
         x = "Customer Name",
         y = "Total Profit") +
    theme_minimal()
  
  # Sales vs Profit Scatter Plot---
  ggplot(data, aes(x = Sales, y = Profit)) +
    geom_point(color = "purple", alpha = 0.6) +
    labs(title = "Sales vs Profit",
         x = "Sales",
         y = "Profit") +
    theme_minimal()
  
  # convert order Date to Date format---
  
  data$Order.Date <- as.Date(data$Order.Date, format ="%m/%d/%Y")
  head(data$Order.Date)
  
  # Monthly Sales Trend---
  library(dplyr)
  
  monthly_sales <- data %>% mutate (Month = format(Order.Date,
                                                   "%y-%m"))%>%
    group_by(Month) %>%
    summarise(Total_Sales = 
                sum(Sales))
  head(monthly_sales)
  ggplot(monthly_sales, aes(x = Month, y = Total_Sales, group = 1)) +
    geom_line(color = "blue", linewidth = 1) + geom_point(color = "red",
                                                          size = 2) +
    labs(title = "Monthly Sales Trend",x = "Month",
         y = "Total Sales") + 
    scale_x_discrete(breaks = monthly_sales$Month[seq(1,
                                                      nrow(monthly_sales),
                                                      by = 3)])+
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Convert Monthly Sales to Time Series---
  ts_sales <- ts(monthly_sales$Total_Sales, frequency = 12)
   # Display Time series----
  ts_sales
  
  # Build Forecast Model----
  forecast_model <- auto.arima(ts_sales)
  
  # Model Summary---
  summary(forecast_model)
  
  # Forecast Next 12 month---
  sales_forecast <- forecast(forecast_model, h = 12)

  # Display Forecast---
  sales_forecast
  library(ggplot2)

  # Plot Forecast---
  plot(sales_forecast, main = "12-Month Sales Forecast",
                                 xlab = "Time (Months)",
                                 ylab = "Sales",
                             col = "blue",
                             lwd = 2
       ) 

  grid(col ="lightgray", lty = "dotted")