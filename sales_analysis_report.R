
# Date Query for Data Ananlysis
query <- "select so.FInterID, o.FItemID, o.FName, so.FDate, e.FItemID, e.FName, p.Fname, soi.FQty, soi.FPrice, soi.FAmount 
from SEOrder so, t_Organization o, SEOrderEntry soi, t_ICItemCore p, t_Base_Emp e 
where so.FCustID = o.FItemID and so.FInterID = soi.FInterId 
and soi.FItemID = p.FItemID and so.FEmpID  = e.FItemID and so.FStatus = 3"

library(RODBC)
kingDee <- odbcConnect("kingdee", uid="sa", pwd="lidong123.")
sale_df <- sqlQuery(kingDee, query, stringsAsFactors = FALSE)
odbcClose(kingDee)

names(sale_df) <- c("OrderID", "CustID", "CustName", "OrderDT", 
                    "EmpID", "EmpName", "PrdName", "FQty", "UnitPrice", 
                    "Amount")

library(lubridate)
sale_df$OrderDT <- as.Date(sale_df$OrderDT)
sale_df$Year <- year(sale_df$OrderDT)
sale_df$Month <- month(sale_df$OrderDT)

# sales_df <- sale_df
sale_df$Amount <- sale_df$Amount / 1000

sale_df1 <- sale_df[sale_df$Amount > 10000 & sale_df$Amount <=50000,]

library(plotly)
minx <- min(sale_df1$Amount)
maxx <- max(sale_df1$Amount)

plot_ly(sale_df1, x = Amount, type="histogram",
        xbins = list(start=minx, end=maxx, size = 2000))

library(ggplot2)
ggplot(sale_df1, aes(x = Amount)) +
      geom_histogram(aes(y=..density..),fill="blue", alpha = 0.5) +
      geom_density(col = "red", weight = 5)
  

# sales analysis for Month
sales_summary_by_month <- sale_df %>% group_by(Year, Month) %>%
  summarize(nth = n(),
            total = sum(Amount)/1000) %>%
  mutate(YM = as.Date(paste(Year,Month,1,sep="-")))
    
sales_summary_by_month$total <- as.integer(sales_summary_by_month$total)

plot_ly(sales_summary_by_month, x=YM, y=total) %>%
  layout(title = "Trend of Sales Volume", showlegend = FALSE) %>%
  filter(total == max(total)) %>%
  layout(annotations = list(x = YM, y = total, text = "Peak", showarrow = T))


# sales analysis for Customer
# ????????? ??????
sales_summary_by_customer <- sale_df %>% group_by(Year, CustName) %>%
  summarize(nth = n(),
            total = sum(Amount)/1000
            ) %>%
  arrange(desc(total)) %>%
  filter(total > 50, Year >=2013 & Year < 2016)

sales_summary_by_customer$total <- as.integer(sales_summary_by_customer$total)

mfrow(c(3,1))
plot_ly(sales_summary_by_customer[sales_summary_by_customer$Year == 2013,], 
        x=Year, y=total, type="bar", color=CustName)
plot_ly(sales_summary_by_customer[sales_summary_by_customer$Year == 2014,], 
        x=Year, y=total, type="bar", color=CustName)
plot_ly(sales_summary_by_customer[sales_summary_by_customer$Year == 2015,], 
        x=Year, y=total, type="bar", color=CustName)


# sales analysis for Product???
# ????????? ??????
sales_summary_by_product <- sale_df %>% group_by(Year,PrdName) %>%
  summarize(total = sum(Amount)/1000) %>%
  arrange(desc(total)) %>%
  filter(Year >=2011 & Year < 2016)

sales_summary_by_product$total <- as.integer(sales_summary_by_product$total)

plot_ly(sales_summary_by_product, x=Year, y=total, type = "bar", color = PrdName) 
plot_ly(sales_summary_by_product, x=Year, y=total, color = PrdName) 

# sales analysis for UnitPrice
# ????????? ?????? ??????
sales_summary_unit_price <- sale_df %>% group_by(Year,PrdName) %>%
  summarize(Average = mean(UnitPrice)) %>%
  arrange(desc(Average)) %>%
  filter(Year >=2011 & Year < 2016)

sales_summary_unit_price$Average <- as.integer(sales_summary_unit_price$Average)

plot_ly(sales_summary_unit_price, x=Year, y=Average, type = "bar", color = PrdName) 
plot_ly(sales_summary_unit_price, x=Year, y=Average, color = PrdName) 



