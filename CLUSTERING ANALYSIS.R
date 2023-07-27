#============
# CLUSTERING                              30/05/2022
#============
library(dplyr)
library(lubridate)
library(factoextra)
library(ggplot2)

# Theme
#-------
theme_new = theme(plot.title = element_text(size = 22, 
                                            face = "bold", 
                                            hjust = 0.5), 
                  legend.title = element_text(size = 15, 
                                              face = "bold"), 
                  plot.subtitle = element_text(size = 19, 
                                               hjust = 0.5), 
                  plot.caption = element_text(size = 14), 
                  plot.margin = margin(1, 1.5, 0.7, 0.7, "cm"), 
                  legend.text = element_text(size = 13), 
                  axis.title = element_text(size = 15, 
                                            face = "bold"), 
                  axis.text = element_text(size = 13, 
                                           face = "bold"), 
                  strip.background = element_blank(), 
                  strip.text = element_text(size = 16, 
                                            face = "bold", 
                                            color = 1))
theme_set(theme_light() + theme_new)
#---------------------------------------------------------

# Loading Workspace
load("E:/St. Xavier's College/Consultancy Data Analysis/Session_02/Senco_02/Data.RData")
glimpse(sales)

table(sales$ORDERACCOUNT)
table(sales$INVOICEID)
table(Location = sales$`STORE Name`, Code = sales$`STORE CODE`)

sales %>% 
     select(INVOICEDATE, ORDERACCOUNT, `STORE Name`, 
            `STORE CODE`, LINEAMOUNT) %>% 
     filter(LINEAMOUNT >= 0) %>% 
     rename(STORE_CODE = `STORE CODE`, 
            STORE_Name = `STORE Name`) %>% 
     mutate(INVOICEDATE = lubridate::date(INVOICEDATE),
            STORE_CODE = factor(STORE_CODE)) -> cls; cls

id = paste0(year(cls$INVOICEDATE), month(cls$INVOICEDATE))
cls$date_id = factor(id, levels = unique(id))
levels(cls$date_id) = gl(4, 12)

rfm = function(store_code = NULL, year_id)   
{
     rfm_data = cls %>% 
          filter(STORE_CODE == store_code,  
                 date_id == year_id) 
     
     n = nrow(rfm_data)
     
     res = rfm::rfm_table_order(rfm_data, ORDERACCOUNT, INVOICEDATE, 
                           LINEAMOUNT, rfm_data$INVOICEDATE[n])
     
     return(res$rfm %>% select(1:5))
}

std_data = rfm("026", year_id = "1"); std_data

stand = function(x) (x - mean(x))/sd(x)

std_data %>% select(-(1:2)) %>% 
     mutate(std_R = stand(recency_days), 
            std_F = stand(transaction_count), 
            std_M = stand(amount)) -> data2

scatterplot3d::scatterplot3d(data2$std_R, data2$std_F, 
                             data2$std_M, angle = 150, pch = 20, 
                             highlight.3d = T, 
                             main = "Scatterplot of RFM Model\nStore: Andheri Mumbai, Year: 2016 - '17", 
                             xlab = "Recency", 
                             ylab = "Frequency", 
                             zlab = "Monetary Value", box = F)

jpeg()

GGally::ggpairs(data2, columns = 4:6, 
                diag = list(continuous = "blankDiag"))

# t.wss = 0
# for (i in 1:10)
# {
#      dt2 = kmeans(data2[,4:6], i)
#      t.wss[i] = dt2$tot.withinss
# }
# 
# plot(t.wss, type = "b", pch = 19)

clus4 = kmeans(data2[,4:6], 4); str(clus4)

fviz_nbclust(data2[,4:6], kmeans, "wss") + 
     labs(subtitle = "Store: Andheri Mumbai, Year: 2016 - '17") + 
     theme_new

(data3 = data2 %>% mutate(Cluster = factor(clus4$cluster))) %>% 
     ggplot(aes(std_R, std_F, col = Cluster)) + 
     geom_point(size = 2) + 
     labs(title = "Recency-Frequency Plot", 
          subtitle = "Store: Andheri Mumbai, Year: 2016 - '17", 
          x = "\nRecency", y = "\nFrequency")

data3 %>% 
     ggplot(aes(std_M, std_F, col = Cluster)) + 
     geom_point(size = 2) + 
     labs(title = "Monetary Value-Frequency Plot", 
          subtitle = "Store: Andheri Mumbai, Year: 2016 - '17", 
          x = "\nMonetary Value", y = "Frequency\n")

data3 %>% 
ggplot(aes(std_R, std_M, col = Cluster)) + 
     geom_point(size = 2) + 
     labs(title = "Recency-Monetary Value Plot", 
          subtitle = "Store: Andheri Mumbai, Year: 2016 - '17", 
          x = "\nRecency", y = "Monetary Value\n")

#==============================================================