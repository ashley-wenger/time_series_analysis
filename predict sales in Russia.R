
require(ggplot2)
require(gridExtra)
require(DescTools)   #gives the AddMonths but is slow.  also masks BoxCox in forecast pkg so load this first and then add forecast to give precedence to it?
require(forecast)  #note this appears to also provide the %>% "operator"   
require(dplyr)
require(lubridate)






#read in shops
#=============
shops_df_raw <- read.csv("shops_translated.csv", stringsAsFactors = FALSE)

#str(shops_df_raw)

#get rid of punctuation
shops_df_raw$shop_name2 <- gsub("[^[[:alnum:][:space:]]", "", shops_df_raw$shop_name)

#put shops into regions based on city name
shops_df_raw$shop_name3 <- strsplit(trimws(shops_df_raw$shop_name2), " ")
shops_df_raw$location <- apply(shops_df_raw, 1, function(x) unlist(x$shop_name3)[1])


#drop the work-in-progress columns and rename the columns cleanly
shops_df <- subset(shops_df_raw, select=c("?...shop_id", "shop_name2", "location"))
colnames(shops_df) <- c("shop_id", "shop_name", "location")
str(shops_df)

#shops_df[substr(shops_df$shop_name, 1, 15) ="Nizhny Novgorod" ][location]

#fix the location for the cities with a 2-word name
shops_df[grep("Nizhny Novgorod", shops_df$shop_name), ]$location <- "Nizhny Novgorod"
shops_df[grep("St Petersburg", shops_df$shop_name), ]$location <- "St Petersburg"


#head(shops_df)





#read in categories
#==================
categories_df <- read.csv("item_categories_translated.csv", stringsAsFactors = FALSE)

str(categories_df)

#create a super-category - might have some trend info/smoother behavior that helps out
categories_df$super_cat1 <- strsplit(trimws(categories_df$item_category_name), "-")

categories_df$super_category <- apply(categories_df, 1, function(x) unlist(x$super_cat1)[1])
#looks like I could have used this instead.  double brackets to the get the element of the list, not a new list of length 1 
# as each element of the list is already a list, then use more [[ to get the nth element of that list.
#categories_df$super_cat1[[79]][[2]]

categories_df$super_cat1 <- NULL     # remove this column from the dataframe

#clean up the column names
colnames(categories_df) <- c("category_id", "category_name", "super_category")







#read in items data
#==================
items2_i_df <- read.delim("items_2_i.txt", stringsAsFactors = FALSE)
#str(items2_i_df)
#5000

items2_ii_df <- read.delim("items_2_ii.txt", stringsAsFactors = FALSE)
#str(items2_ii_df)
#6998


items2_iii_df <- read.delim("items_2_iii.txt", stringsAsFactors = FALSE)
#str(items2_iii_df)
#5002

items2_iv_df <- read.delim("items_2_iv.txt", stringsAsFactors = FALSE)
#str(items2_iv_df)
#5170

#5000+6998+5002+5170
#22170

#merge the subsets back together into a main working set
items_df <- rbind(items2_i_df, items2_ii_df, items2_iii_df, items2_iv_df)
str(items_df)

#clean up the column names
colnames(items_df) <- c("item_id", "category_id", "item_name_original", "item_name")

#remove subsets for space reasons  
remove(items2_i_df)
remove(items2_ii_df)
remove(items2_iii_df)
remove(items2_iv_df)

#nrow(items_df)
#head(items_df)



#checks on the data
#unique(items_df$item_id)
sum(duplicated(items_df$item_id))#0.   no dups
anyDuplicated(items_df$item_id)   #also 0.  perfect.  appear to have split, translated and rejoined w/o any errors.

sum(is.na(items_df$category_id))  #0.   good

length(unique(items_df$category_id))  #84,  good, all the categories have at least one item in said category
min(unique(items_df$category_id))  #0
max(unique(items_df$category_id))  #83


#item name
#19 dups - potentially due to translation process from Russian to English.  Going to make them distinct.
sum(duplicated(items_df$item_name))   #19
items_df[duplicated(items_df$item_name), ]$item_name
  #there appear to be only 2 rows max for a given name - no cases with 3, 4, ...  diff rows with the same name
  #  good, we can just append a diff value onto the name for the 2nd (and last) time that name shows up

items_df[duplicated(items_df$item_name), ]$item_name <- apply(items_df[duplicated(items_df$item_name), ], 
                                                              1, 
                                                              function(x) paste(x[["item_name"]], "_duplicate"))

sum(duplicated(items_df$item_name))   #0, perfect.  All resolved











#read in sales data (training dataset)
#=====================================
sales_df_raw <- read.csv("sales_train_v2.csv")

#str(sales_df_raw)
# 'data.frame':	2,935,849 obs. of  6 variables:
#   $ date          : Factor w/ 1034 levels "01.01.2013","01.01.2014",..: 35 69 137 171 477 307 35 103 341 69 ...
# $ date_block_num: int  0 0 0 0 0 0 0 0 0 0 ...
# $ shop_id       : int  59 25 25 25 25 25 25 25 25 25 ...
# $ item_id       : int  22154 2552 2552 2554 2555 2564 2565 2572 2572 2573 ...
# $ item_price    : num  999 899 899 1709 1099 ...
# $ item_cnt_day  : num  1 1 -1 1 1 1 1 1 1 3 ...


#convert the date field from a factor/char to a date
sales_df_raw$Sale_Date <- as.Date(sales_df_raw$date, format="%d.%m.%Y")

#head(sales_df_raw[, c("date", "Sale_Date")], 50)
#summary(sales_df_raw$item_price)

#note one record has a negative item price
#sales_df_raw[sales_df_raw$item_price <= 0, ]
#         date           date_block_num    shop_id     item_id    item_price    item_cnt_day
#484684 15.05.2013              4              32         2973         -1            1


#sum(sales_df_raw$item_price > 30000)
#240 records have an item_price over 30000
#nrow(sales_df_raw[sales_df_raw$item_price > 30000, ])


#nrow(sales_df_raw)  #2,935,849
#str(items_df)

#merge sales with items to get item name, category_id
sales_df <- merge(x=sales_df_raw, y=items_df, by = "item_id")

#nrow(sales_df)   #2,935,849   -- still same as original, good
#str(sales_df)


#merge sales with categories to get category_name, super_category
sales_df <- merge(x=sales_df, y=categories_df, by = "category_id")

#str(sales_df)
#nrow(sales_df)   #2,935,849   -- still same as original, good


#merge sales with shops to get shop_name, location
sales_df <- merge(x=sales_df, y=shops_df, by="shop_id")

#drop these columns, not helping.
sales_df$date <- NULL
sales_df$item_name_original <- NULL

#colnames(sales_df)


sales_df$mnth <- AddMonths(as.Date("01-01-2013", "%d-%m-%Y"), sales_df[ ,"date_block_num"])

#check that this worked as expected    unique(paste(sales_df$date_block_num, sales_df$mnth))  


#reorganize the columns
sales_df <- subset(sales_df, select=c("Sale_Date", "date_block_num", "mnth",    
                                      "shop_id", "shop_name", "location", 
                                      "item_id", "item_name",
                                      "category_id", "category_name", "super_category",         
                                      "item_cnt_day", "item_price"))

#str(sales_df)



#write these to disk so that I can skip down to here in teh future and reload w/o 
#   re-running all the steps above.  The AddMonths one takes ~ 5 min
# write.csv(shops_df, "shops_df_AKW.csv")
# write.csv(categories_df, "categories_df_AKW.csv")
# write.csv(items_df, "items_df_AKW.csv")
# write.csv(sales_df, "sales_df_AKW.csv")


#read them back in
#if restarting here, need to run the require steps at the top first, then setwd, then read in csv's
# setwd("C:/Users/ashle/Documents/Personal Data/Northwestern/2019_01 winter  MSDS413 Time Series/Project1")
# 
# shops_df <- read.csv("shops_df_AKW.csv")
# categories_df <- read.csv("categories_df_AKW.csv")
# items_df <- read.csv("items_df_AKW.csv")
# sales_df <- read.csv("sales_df_AKW.csv")





#get sales by month for EDA and maybe for use as base predictor data
  #group by every dim except sale date ==>   aggregate over days (Sale_Date) up to monthly level
monthly_sales <- sales_df %>% 
                      dplyr::group_by(date_block_num, mnth, mnth,  
                                 shop_id, shop_name, location, 
                                 item_id, item_name,
                                 category_id, category_name, super_category)  %>%
                        dplyr::summarize(
                                  Mnth = max(Sale_Date),
                                  Units_Sold = sum(item_cnt_day),
                                  Price_Min = min(item_price),
                                  Price_Max = max(item_price),
                                  Price_Avg = mean(item_price)
                                )

#str(monthly_sales)    
#attributes(monthly_sales)


# review results by category
# =========================

#prelim data gathering
#a.  get the full list of categories and ttl sales, so we don't overlook any and so we can sort big to small
sales_by_catgry <- monthly_sales %>%
                      group_by(category_id, category_name, super_category) %>%
                          summarize(category_sales_ttl=sum(Units_Sold))

#left outer join so all the categories are represented
categories_df_w_sales <- merge(categories_df, sales_by_catgry, by=c("category_id", "category_name", "super_category"), all.x=TRUE)
#sort by Ttl units sold desc, nulls last
categories_df_w_sales <- categories_df_w_sales[order(categories_df_w_sales$category_sales_ttl, decreasing=TRUE, na.last = TRUE), ]
#tail(categories_df_w_sales)  #note they all had a sale, so no need to deal w/ nulls



#sum by category
monthly_sales_by_category <- monthly_sales %>%
                                group_by(date_block_num,  mnth, category_id, category_name, super_category) %>%
                                    summarize(
                                              Units_Sold = sum(Units_Sold),
                                              Nbr_Stores_w_Sales = n_distinct(shop_id)
                                              )


# head(monthly_sales_by_category)
# unique(monthly_sales_by_category$category_name)
# monthly_sales_by_category[monthly_sales_by_category$category_id == 9, ]
#min(monthly_sales_by_category$date_block_num)
#max(monthly_sales_by_category$date_block_num)
#autoplot(ts(monthly_sales_by_category))


#how are the category sales trending over time
#---------------------------------------------
#ggplot(data=monthly_sales_by_category, aes(x=date_block_num, y=Units_Sold)) + geom_point() + 
#  facet_wrap( ~ category_name, ncol=1, scales="free_y")
#works but with so many facets each one is tiny (appears the overall size is relatively constant, so with more facets each one has to be smaller)

pdf("MoSales_by_Cat.pdf")
i <- 0
for (cat_nm in unique(categories_df_w_sales[1:4,]$category_name)) 
{
  i <- i + 1
  plt <- ggplot(data=monthly_sales_by_category[monthly_sales_by_category$category_name==cat_nm, ], 
                aes(x=mnth, y=Units_Sold)) + 
    geom_point() + xlim(as.Date("01-01-2013", "%d-%m-%Y"), as.Date("01-10-2015", "%d-%m-%Y")) +
    ggtitle(paste("Category:  ",cat_nm)) + xlab("Month") + ylab("Units Sold") + theme(plot.title = element_text(hjust = 0.5))
  print(plt)
  
  switch(i, plt1<-plt, plt2<-plt, plt3<-plt, plt4<-plt)
}
grid.arrange(plt1, plt2, plt3, plt4)
dev.off()
#just launched - Games - PS4 (started Oct 2014, big volume and store count);  utilities-tickets;  Books - Comics manga (getting pretty big!);  
#dead products - nothing in category for yearish - ex.   Net carriers (piece) (was pretty sizable at first, zero since ~Jan 2015) 
#                          Gifts - cards and labels?
#outliers vs. seasonality???    ex.   one of hte games I think hit a huge spike in one Dec but not in another.  Service" has a big runup in late 2014 but appears exceptional - nothing like in t the yr prior
  #lots (but not all) gift categories have a big spike! in Nov    Gifts - :  Bags Album.., Development, Board games, Gifts - Figures 72 ...
  #           games somewhat  - Games - XBOX 360, ..
pdf("StoresSelling_by_Cat.pdf")
for (cat_nm in unique(categories_df_w_sales$category_name)) 
{
  plt <- ggplot(data=monthly_sales_by_category[monthly_sales_by_category$category_name==cat_nm, ], 
                aes(x=mnth, y=Nbr_Stores_w_Sales)) + 
    geom_point() + xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-10-2015", "%d-%m-%Y")) + ylim(0, 80) +
    ggtitle(paste("Category:  ",cat_nm)) + xlab("Month") + ylab("Count of Distinct Stores with Sales")  + theme(plot.title = element_text(hjust = 0.5))
  print(plt)
}
dev.off()

# even the largest products are only sold at 45-50 stores max per month.  Any goofiness w/ some stores not selling much/some product?
#    TODO: check this out

# ???Games PC - The figure is only sold at ~1 store -- every month -- despite being a prtty sizable product
#     ditto for Delivery, Books - The figure; Payment cards -Live! (Digital); ....


#appear to be some weeks when stores didn't sell.  

#A bunch show a declining trend - stores closing or just a less appealing product?

#A few products are new and just ramping up (storewise) 
#- presumably model will handle that appropriately, if given start and end to modeling window that varies by item and/or category?   Think about using a damped trend for these in case it gets out of control?
# check for similar behavior in units sold - stores could be more scalable if it's just rolling out???




#how are the category sales distributed across stores
#----------------------------------------------------

mo_sales_by_cat_and_store <- monthly_sales %>%
                                group_by(date_block_num, mnth, category_id, category_name, super_category, shop_id, shop_name, location) %>%
                                     summarize(
                                              Units_Sold = sum(Units_Sold)
                                              )


#boxplot of units sold - group for each month;   pg for each category. 
pdf("BoxPlot_of_CategorySales_By_Store.pdf")
i <- 0
for (cat_nm in unique(categories_df_w_sales[1:4, ]$category_name)) 
{
  i <- i + 1  
  plt <- ggplot(data=mo_sales_by_cat_and_store[mo_sales_by_cat_and_store$category_name==cat_nm, ],
                    aes(x=mnth, y=Units_Sold, group=mnth)) + 
                geom_boxplot() +
                xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + #ylim(0, 80) +
                ggtitle(paste("Category Name: ",cat_nm)) + theme(plot.title = element_text(hjust = 0.5)) + 
                xlab("Month") + ylab("Units sold at each store")  
  print(plt)
  switch(i, plt1<-plt, plt2<-plt, plt3<-plt, plt4<-plt)
}
grid.arrange(plt1, plt2, plt3, plt4)
dev.off()

#lots of very high outliers.  The big stores are a lot bigger than the avg store.



#warnings()





#look by stores
#==============

#get a full list of stores and ttl sales so we can make sure zero-sales stores are not overlooked and sort stores big-small
#---------------------------------------------------------------------------------------------------
sales_ttl_by_store <- monthly_sales %>%
                          group_by(shop_id) %>%
                              summarize(shop_sales_ttl=sum(Units_Sold))

shops_df_w_sales <- merge(shops_df, sales_ttl_by_store, by="shop_id", all.x=TRUE)
shops_df_w_sales <- shops_df_w_sales %>% arrange(desc(shop_sales_ttl))

#sort by big stores first (get mean of sales over the time period?  use sum/34 b/c some stores might have had no sales in a mo??)
#stores w/ NA for total sales will be last
shops_df_w_sales <- shops_df_w_sales[order(shops_df_w_sales$shop_sales_ttl, decreasing=TRUE, na.last=TRUE), ]


mo_sales_by_store <- monthly_sales %>%
                        group_by(date_block_num, mnth, shop_id, shop_name, location) %>%
                            summarize(
                                      Units_Sold = sum(Units_Sold),
                                      NbrD_Items_Sold = n_distinct(item_id)
                                      )

nrow(mo_sales_by_store)
#length(unique(mo_sales_by_store$shop_id))  #60 stores, as desired.  All the stores have sales in at least one month


#do a left join to stores to ensure this includes all stores
#mo_sales_by_store <- merge(shops_df_w_sales, mo_sales_by_store, by=c("shop_id", "shop_name", "location"), all.x = TRUE)

#get a full matrix of store X mo
storesXmo_full_df <- monthly_sales %>% group_by(mnth, date_block_num) %>% summarize(jnk=n()) %>%
                        merge(shops_df[, c('shop_id', 'shop_name', 'location')])

#do a left join to make sure we have a record for each shopXmo, and then fill in zeros
mo_sales_by_store <- merge(storesXmo_full_df, mo_sales_by_store, by=c('shop_id', 'shop_name', 'location', 'mnth', 'date_block_num'), all.x = TRUE)

mo_sales_by_store$jnk <- NULL   #drop this junk column.   Needed it in the group by/ select distinct

str(mo_sales_by_store)

#check for nulls - expect these to be zero
# mo_sales_by_store[is.na(mo_sales_by_store$mnth), ]
# mo_sales_by_store[is.na(mo_sales_by_store$shop_id), ]

#fill in NAs (where the store had no sales) with zeros, so that it's clear
mo_sales_by_store[is.na(mo_sales_by_store$Units_Sold), ]$Units_Sold <- 0
mo_sales_by_store[is.na(mo_sales_by_store$NbrD_Items_Sold), ]$NbrD_Items_Sold <- 0


# length(unique(mo_sales_by_store$shop_id))
# head(mo_sales_by_store)




#graph # of units sold per mo, with one page/graph for each store.
#----------------------------------------------------------------
pdf("Nbr_Units_Sold_by_Store.pdf")
i <- 0
for (shp_nm in unique(shops_df_w_sales[1:4, ]$shop_name)) 
{  
  i <- i + 1
  plt <- ggplot(data=mo_sales_by_store[mo_sales_by_store$shop_name==shp_nm, ], 
                 aes(x=mnth, y=Units_Sold)) + 
                 geom_point() + 
                 xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
                 ggtitle(paste("Shop Name: ",shp_nm)) + theme(plot.title = element_text(hjust = 0.5)) + 
                 xlab("Month") + ylab("Units sold")  
   print(plt)
   switch(i, plt1<-plt, plt2<-plt, plt3<-plt, plt4<-plt)
}
grid.arrange(plt1, plt2, plt3, plt4)
dev.off()

#notes from reviewing PDF's:
#1. a spike at Nov for many of the largest stores
#2.  note a bunch of stores appear closed
    # this one looks like it closed in Q1-2015   Khimki shopping mall Mega  -- was big, too!
    #  Moscow shopping center MEGA Belaya Dacha II
    # Moscow shopping center Perlovskiy 
    # and a bunch more...
    #  Moscow Sales looks like it's ONLY open in Oct???  Only 2 mos of activity in the entire window, both appear to be Oct??
#3. check outliers:  May 2013 for Moscow shopping center MEGA Belaya Dacha II




#CHECK IF WE NEED FORECASTS FOR CLOSED STORES, which can&should be set to 0
#---------------------------------------------------------------------------
#stores with ZERO sales of ANY items for a whle   ==> set forecast to zero
  #a) closed last month, but not closed in Sept
  #Tyumen shopping mall Green Beach (shopID 51) has no results for Oct 2015.  
        #Blip or permanently closed?  Note there's a "Tyumen shopping mall" that's still going at usual rate in Oct 2015.  

#a) not closed last month, but never appear to sell in Nov  (noted in review of Units sold by store pdf) 
      #Visiting Commerce store has sales in every Oct, and ONLY in Octobers.  ==> forecast 0
      #Ditto for Moscow Sales store has sales in every Oct, and ONLY in Octobers.  ==> forecast 0
          # shops_df[shops_df$shop_name == 'Visiting Commerce' | shops_df$shop_name == 'Moscow Sales', ] 
          #   shopid 9, shopid 20


  
# head(mo_sales_by_store)
# mo_sales_by_store %>% group_by(mnth) %>% summarize(nbr_stores_selling=n_distinct(shop_id))  %>% View()  #45-50 per mo

#c) Otherwise, the stores either look closed (zero sales for 2+  3+? months)

      # subset(mo_sales_by_store, ( mnth==as.Date("1-10-2015","%d-%m-%Y") | mnth==as.Date("1-09-2015", "%d-%m-%Y") | mnth==as.Date("1-08-2015", "%d-%m-%Y")) & 
                      #            Units_Sold ==0 )
      
      #store 27:   Moscow shopping center MEGA Belaya Dacha II 
      # subset(mo_sales_by_store, shop_id == 27)
      # has 0 in Aug, -1 (return?) in Sept, 0 in Oct ==>   going to force the forecast to zero

      #otherwise, the stores look like they've been closed for a while - no sales in 2+ mos


#d) or the stores have results up to Oct 2015, ==>   need a forecast

#shopID 36; Novosibirsk SEC Gallery Novosibirsk
#false positive - looks like a potential closer b/c of no sales in Sept or Aug, but is exact opposite- is just opened



# exec summary:   force forecast to 0 for the stores closed in Oct PLUS shops 9 and 20, which appear to only be open in OCT
subset(mo_sales_by_store, mnth==as.Date("1-10-2015","%d-%m-%Y") & Units_Sold ==0, select = shop_id) %>% View()
  # 0 sales in Nov (and generally for a while prior):  shop_ids  0, 1, 8, 11, 13, 17, 23, 27, 29, 30, 32, 33, 40, 43, 51, 54
  #   add in 9, 20

subset(test_df, shop_id %in% c(0, 1, 8, 11, 13, 17, 23, 27, 29, 30, 32, 33, 40, 43, 51, 54, 9, 20)) %>%
  group_by(shop_id) %>% summarize(n_itms=n_distinct(item_id))
#*****************************
#0 results ==>   predictions aren't required for any of these.
#*****************************




#graph # of distinct items sold per mo, with one page/graph for each store.
#----------------------------------------------------------------
pdf("NbrD_Items_Sold_by_Store.pdf")
for (shp_nm in unique(shops_df_w_sales$shop_name)) 
{  
  plt <- ggplot(data=mo_sales_by_store[mo_sales_by_store$shop_name==shp_nm, ], 
                aes(x=mnth, y=NbrD_Items_Sold)) + 
                geom_point() + 
                xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
                ggtitle(paste("Shop Name: ",shp_nm)) + theme(plot.title = element_text(hjust = 0.5)) + 
                xlab("Month") + ylab("Count of Distinct Items Sold")  
  print(plt)
}
dev.off()


length(unique(items_df[items_df$item_id %in% test_df$item_id, ]$category_id))




#look by items
#==============

#get a full list of items and ttl sales so we can make sure zero-sales stores are not overlooked and sort items big-small
#---------------------------------------------------------------------------------------------------
sales_ttl_by_item <- monthly_sales %>%
  group_by(item_id) %>%
  summarize(Units_Sold_Ttl=sum(Units_Sold))

items_df_w_sales <- merge(items_df, sales_ttl_by_item, by="item_id", all.x=TRUE)
items_df_w_sales[is.na(items_df_w_sales)] <- 0

#sort by big stores first (get mean of sales over the time period?  use sum/34 b/c some stores might have had no sales in a mo??)
#stores w/ NA for total sales will be last
items_df_w_sales <- items_df_w_sales[order(items_df_w_sales$Units_Sold_Ttl, decreasing=TRUE, na.last=TRUE), ]

str(mo_sales_by_item)
str(items_df_w_sales)
head(items_df_w_sales)
tail(items_df_w_sales)
   #note there are items with no sales (ever)!!



#graph # of units sold per mo, with one graph for each item
#-----------------------------------------------------------
i <- 0
for (itm_id in items_df_w_sales[1:4, ]$item_id) 
{  
  i <- i + 1
  itm_nm <- items_df_w_sales[items_df_w_sales$item_id==itm_id, ]$item_name
  
  plt <- ggplot(data=mo_sales_by_item[mo_sales_by_item$item_id==itm_id, ], 
                aes(x=mnth, y=Units_Sold)) + 
    geom_point() + 
    xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
    ggtitle(itm_nm) + theme(plot.title = element_text(hjust = 0.5)) + 
    xlab("Month") + ylab("Units sold")  
  print(plt)
  switch(i, plt1<-plt, plt2<-plt, plt3<-plt, plt4<-plt)
}
grid.arrange(plt1, plt2, plt3, plt4)
dev.off()


#graph # of units sold per mo by store, with one page/graph for each item,  and one file per category
#----------------------------------------------------------------
# head(monthly_sales)
# head(sls_df)
# # for (shp_id in unique( shops_df_w_sales$shop_id)) 
# for (shp_id in c(31, 25, 54))   
#   {  
#     #create a new file for each store
#     pdf(paste("Units_Sold_by_Store__", shp_id, ".pdf", sep=""))
#     
#     #create a new page/graph for each category
#     for (cat_nm in unique(categories_df_w_sales$category_name)) 
#     {
#       sls_df <- subset(monthly_sales,
#                        shop_id==shp_id & category_name == cat_nm)
#       
#       if ( nrow(sls_df)!=0 )
#       {
#         plt <- ggplot(data=sls_df[], 
#                       aes(x=mnth, y=Units_Sold, group=item_name)) + 
#           geom_line(aes(color=item_name)) + 
#           geom_point(aes(color=item_name)) + 
#           xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
#           ggtitle(paste("Category: ",cat_nm)) + theme(plot.title = element_text(hjust = 0.5)) + 
#           xlab("Month") + ylab("Units Sold")  +
#           theme(legend.position="none")
#         print(plt)
#       }
#     }
#     dev.off()  #save the file
#   }



#graph # of units sold per mo by item, with one page/graph for each category,  and one file per store.
#----------------------------------------------------------------
# head(monthly_sales)
# head(sls_df)
for (shp_id in unique(shops_df_w_sales$shop_id)) 
#for (shp_id in c(31, 25, 54))   
{  
  #create a new file for each store
  pdf(paste("Units_Sold_by_Store__", shp_id, ".pdf", sep=""))

  #create a new page/graph for each category
  for (cat_nm in unique(categories_df_w_sales$category_name)) 
  {
    sls_df <- subset(monthly_sales,
                        shop_id==shp_id & category_name == cat_nm)
    
    if ( nrow(sls_df)!=0 )
    {
      plt <- ggplot(data=sls_df[], 
                   aes(x=mnth, y=Units_Sold, group=item_name)) + 
                  geom_line(aes(color=item_name)) + 
                  geom_point(aes(color=item_name)) + 
                  xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
                  ggtitle(paste("Category: ",cat_nm)) + theme(plot.title = element_text(hjust = 0.5)) + 
                  xlab("Month") + ylab("Units Sold")  +
                  theme(legend.position="none")
      print(plt)
    }
  }
  dev.off()  #save the file
}
#TODO:  music, movies, games show a distinct start hi, taper off pattern.  Perhaps should model them as months since launch?
  #seems to be by category - review categories, and then decide how to handle category by categoyr?
#   thinking of approach like cohort modeling - % decay in month 1, % decay in month 2, ...

# warnings()



#get a full matrix of item X Mo, so we can more easily find 0's
#--------------------------------------------------------------
mnth_df <- data.frame(mnth=seq.Date(from=as.Date("01-01-2013", "%d-%m-%Y"), to=as.Date("01-10-2015", "%d-%m-%Y"), by="month"),
                      date_block_num=seq(0,33))

itemXmo_df <- merge(items_df_w_sales, mnth_df)
#nrow(itemXmo_df)  753780
#753780*60 = 45,226,800



#get mo_sales_by_item  (in any store)
mo_sales_by_item <- monthly_sales %>%
                        group_by(item_id, mnth) %>%
                            summarize(Units_Sold=sum(Units_Sold))
#233,912 rows



#left outer join to the full matrix so that we have a record for all the itm X mo combos
mo_sales_by_item <- merge(itemXmo_df, mo_sales_by_item, by=c('item_id', 'mnth'), all.x=TRUE)

#replace nulls in any column with zero
mo_sales_by_item[is.na(mo_sales_by_item)] <- 0


#look for items that have historically had sales sometime over the lifetime, but have had no sales AT ANY SHOP in the last XX mos
#--------------------------------------------------------------------------------------------------------------------------------
itms_w_sales_but_not_sold_R1 <- subset(mo_sales_by_item, mnth==as.Date("01-10-2015", "%d-%m-%Y") & Units_Sold_Ttl >0) %>% 
        group_by(item_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0)
nrow(itms_w_sales_but_not_sold_R1)  #16,387


itms_w_sales_but_not_sold_R2 <- subset(mo_sales_by_item, mnth>=as.Date("01-09-2015", "%d-%m-%Y") & Units_Sold_Ttl >0) %>% 
  group_by(item_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0)
nrow(itms_w_sales_but_not_sold_R2)  #15,344

itms_w_sales_but_not_sold_R3 <- subset(mo_sales_by_item, mnth>=as.Date("01-08-2015", "%d-%m-%Y") & Units_Sold_Ttl >0) %>% 
  group_by(item_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0)
nrow(itms_w_sales_but_not_sold_R3)  #14,615

itms_w_sales_but_not_sold_R4 <- subset(mo_sales_by_item, mnth>=as.Date("01-07-2015", "%d-%m-%Y") & Units_Sold_Ttl >0) %>% 
  group_by(item_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0)
nrow(itms_w_sales_but_not_sold_R4)  #13,917

itms_w_sales_but_not_sold_R5 <- subset(mo_sales_by_item, mnth>=as.Date("01-06-2015", "%d-%m-%Y") & Units_Sold_Ttl >0) %>% 
  group_by(item_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0)
nrow(itms_w_sales_but_not_sold_R5)  #13,403

itms_w_sales_but_not_sold_R6 <- subset(mo_sales_by_item, mnth>=as.Date("01-05-2015", "%d-%m-%Y") & Units_Sold_Ttl >0) %>% 
  group_by(item_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0)
nrow(itms_w_sales_but_not_sold_R6)  #12,886



#CHECK IF WE NEED FORECASTS FOR DISCONTINUED ITEMS, which can&should be set to 0
#---------------------------------------------------------------------------
test_itms_not_sold_R6 <- merge(test_df, itms_w_sales_but_not_sold_R6, by='item_id')  
nrow(test_itms_not_sold_R6)  #7,812 combos!
length(unique(test_itms_not_sold_R6$item_id))   #from 186 distinct items

test_itms_not_sold_R3<- merge(test_df, itms_w_sales_but_not_sold_R3, by='item_id')  
nrow(test_itms_not_sold_R3)  #14,952 combos!
length(unique(test_itms_not_sold_R3$item_id))   #from 356 distinct items

test_itms_not_sold_R1 <- merge(test_df, itms_w_sales_but_not_sold_R1, by='item_id')  
nrow(test_itms_not_sold_R1)  #31,332 combos!
length(unique(test_itms_not_sold_R1$item_id))   #from 746 distinct items


#for sure the "not sold in last 6 mos" should get a forecast of 0.  How about the ones w/ no sales for a shorter period of time?




#check out some of the top selling items (over the entire window) that appear to be discontinued
#get the seemingly-discontinued items, and rank them by total sales desc.
top_sellers_discontd_R6_df <- data.frame(item_id=unique(test_itms_not_sold_R6$item_id)) %>%
  merge(items_df_w_sales, by='item_id') %>%
      group_by(item_id) %>% 
          summarize(MaxSalesTtl=max(Units_Sold_Ttl), MinSalesTtl=min(Units_Sold_Ttl)) %>%
              arrange(desc(MaxSalesTtl))

#graph them over time
pdf("Discontinued_Items_No_R6_Sales.pdf")
for (itm_id in top_sellers_discontd_R6_df[1:186,]$item_id)
{

  plt <-  ggplot(subset(mo_sales_by_item, item_id ==itm_id),
         aes(x=mnth, y=Units_Sold)) +
    geom_point() +
        xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
        ggtitle(paste("ItemID: ",itm_id)) + theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Month") + ylab("Units Sold")  +
        theme(legend.position="none")

  print(plt)
}
dev.off()


#check out seemingly discontinued as per no sales in last 3 mos
#get the seemingly-discontinued items, and rank them by total sales desc.
top_sellers_discontd_R3_full_df <- data.frame(item_id=unique(test_itms_not_sold_R3$item_id)) %>%
  merge(items_df_w_sales, by='item_id') %>%
  group_by(item_id) %>% 
  summarize(MaxSalesTtl=max(Units_Sold_Ttl), MinSalesTtl=min(Units_Sold_Ttl)) %>%
  arrange(desc(MaxSalesTtl))

#exclude the ones that were already considered in the "no sales in 6 mos" view
top_sellers_discontd_R3_df <- top_sellers_discontd_R3_full_df[!(top_sellers_discontd_R3_full_df$item_id %in% top_sellers_discontd_R6_df$item_id), ]
  
#graph them over time
pdf("Discontinued_Items_No_R3_Sales.pdf")
for (itm_id in top_sellers_discontd_R3_df[1:200,]$item_id)
{
  
  plt <-  ggplot(subset(mo_sales_by_item, item_id ==itm_id),
                 aes(x=mnth, y=Units_Sold)) +
    geom_point() +
    xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
    ggtitle(paste("ItemID: ",itm_id)) + theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Month") + ylab("Units Sold")  +
    theme(legend.position="none")
  
  print(plt)
}
dev.off()



#check out seemingly discontinued as per no sales in last 1 mo
#get the seemingly-discontinued items, and rank them by total sales desc.
top_sellers_discontd_R1_full_df <- data.frame(item_id=unique(test_itms_not_sold_R1$item_id)) %>%
  merge(items_df_w_sales, by='item_id') %>%
  group_by(item_id) %>% 
  summarize(MaxSalesTtl=max(Units_Sold_Ttl), MinSalesTtl=min(Units_Sold_Ttl)) %>%
  arrange(desc(MaxSalesTtl))

#exclude the ones that were already considered in the "no sales in 3 (or 6) mos" view 
top_sellers_discontd_R1_df <- top_sellers_discontd_R1_full_df[!(top_sellers_discontd_R1_full_df$item_id %in% top_sellers_discontd_R3_full_df$item_id), ]

#graph them over time
pdf("Discontinued_Items_No_R1_Sales.pdf")
for (itm_id in top_sellers_discontd_R1_df[,]$item_id)
{
  
  plt <-  ggplot(subset(mo_sales_by_item, item_id ==itm_id),
                 aes(x=mnth, y=Units_Sold)) +
    geom_point() +
    xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
    ggtitle(paste("ItemID: ",itm_id)) + theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Month") + ylab("Units Sold")  +
    theme(legend.position="none")
  
  print(plt)
}
dev.off()


#don't see any patterns that indicate decent chance of sales.  Expect some noise in the ones with sales in Sept etc but small 
#      for all stores in total so don't see any good way to predict a store breakdown.  Going to set these to zero and not bother
#      with a more mathematical algorithm.  Can always tighten the params on which ones to let down this path and send more down the 
#      "generate automated forecast" path if time permits.







# ========================================================================
##repeat same process for item&shop combos that have been zero for a while
# ========================================================================
#look for combos that have historically had sales sometime over the lifetime, but have had zero sales of that item&shop in the last XX mos
#--------------------------------------------------------------------------------------------------------------------------------

sales_ttl_by_item
str(monthly_sales)

#get the full matrix of combo X mo ( itemXshopXmo) 
itemXmoXshop_df <- merge(itemXmo_df, shops_df_w_sales)

itemXmoXshop_df_last6mo <- subset(itemXmoXshop_df, mnth>=as.Date("05-01-2015", "%m-%d-%Y"))



#get the total sales by combo, so we can exclude ones that have zero sale ever
sales_by_cmbo <- monthly_sales %>% 
  group_by(item_id, shop_id) %>%
  summarize(combo_ttl_sales=sum(Units_Sold))


str(shops_df_w_sales)
#monthly_sales_full <- merge(itemXmoXshop_df, monthly_sales, all.x = TRUE)  #not specifying a 'by' param ==> use default of intersection of columns
#got out of memory error

monthly_sales_full_last6mo <- merge(itemXmoXshop_df_last6mo, monthly_sales, all.x = TRUE)  #not specifying a 'by' param ==> use default of intersection of columns
monthly_sales_full_last6mo[is.na(monthly_sales_full_last6mo$Units_Sold), ]$Units_Sold <- 0


#write these to disk and remove from memory to free up space as we are done with them 
write.csv(itemXmoXshop_df, file='itemXmoXshop_df.csv')
remove(itemXmoXshop_df)
remove(itemXmoXshop_df_last6mo)


cmbos_w_sales_but_not_sold_R1 <- subset(monthly_sales_full_last6mo, mnth==as.Date("10-01-2015", "%m-%d-%Y")) %>% 
  group_by(item_id, shop_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0) %>%
  merge(sales_by_cmbo, by=c('item_id', 'shop_id'))   #filter the combos with no sales in the last XX mos down to the ones that did have sales previously  (removes combos found in test but not in training)
nrow(cmbos_w_sales_but_not_sold_R1)  #392,643

cmbos_w_sales_but_not_sold_R2 <- subset(monthly_sales_full_last6mo, mnth>=as.Date("09-01-2015", "%m-%d-%Y")) %>% 
  group_by(item_id, shop_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0) %>%
  merge(sales_by_cmbo, by=c('item_id', 'shop_id'))   
nrow(cmbos_w_sales_but_not_sold_R2)  #374742

cmbos_w_sales_but_not_sold_R3 <- subset(monthly_sales_full_last6mo, mnth>=as.Date("08-01-2015", "%m-%d-%Y")) %>% 
  group_by(item_id, shop_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0) %>%
  merge(sales_by_cmbo, by=c('item_id', 'shop_id'))   
nrow(cmbos_w_sales_but_not_sold_R3)  #359137

# cmbos_w_sales_but_not_sold_R4 <- subset(monthly_sales_full_last6mo, mnth>=as.Date("07-01-2015", "%m-%d-%Y")) %>% 
#   group_by(item_id, shop_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0) %>%
#   merge(sales_by_cmbo, by=c('item_id', 'shop_id'))   
# nrow(cmbos_w_sales_but_not_sold_R4)  
# 
# cmbos_w_sales_but_not_sold_R5 <- subset(monthly_sales_full_last6mo, mnth>=as.Date("06-01-2015", "%m-%d-%Y")) %>% 
#   group_by(item_id, shop_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0) %>%
#   merge(sales_by_cmbo, by=c('item_id', 'shop_id'))   
# nrow(cmbos_w_sales_but_not_sold_R5)  #13,403

cmbos_w_sales_but_not_sold_R6 <- subset(monthly_sales_full_last6mo, mnth>=as.Date("05-01-2015", "%m-%d-%Y")) %>% 
  group_by(item_id, shop_id) %>% summarize(Units_Sold=sum(Units_Sold)) %>% filter(Units_Sold==0) %>%
  merge(sales_by_cmbo, by=c('item_id', 'shop_id'))   
nrow(cmbos_w_sales_but_not_sold_R6)  #326240

#write this to disk and remove from memory to free up space as we are done with it 
write.csv(monthly_sales_full_last6mo, file='monthly_sales_full_last6mo_df.csv')
remove(monthly_sales_full_last6mo)


getwd()
#CHECK IF WE NEED FORECASTS FOR DISCONTINUED ITEMS, which can&should be set to 0
#---------------------------------------------------------------------------
test_cmbos_not_sold_R6 <- merge(test_df, cmbos_w_sales_but_not_sold_R6, by=c('item_id', 'shop_id'))  
nrow(test_cmbos_not_sold_R6)  #36978 combos!
length(unique(test_cmbos_not_sold_R6$item_id))   #from 3120 distinct items

test_cmbos_not_sold_R3<- merge(test_df, cmbos_w_sales_but_not_sold_R3, by=c('item_id', 'shop_id'))
nrow(test_cmbos_not_sold_R3)  #55536 combos!
length(unique(test_cmbos_not_sold_R3$item_id))   #from 3650 distinct items

test_cmbos_not_sold_R1 <- merge(test_df, cmbos_w_sales_but_not_sold_R1, by=c('item_id', 'shop_id')) 
nrow(test_cmbos_not_sold_R1)  #82770 combos!
length(unique(test_cmbos_not_sold_R1$item_id))   #from 4173 distinct items


#for sure the "not sold in last 6 mos" should get a forecast of 0.  How about the ones w/ no sales for a shorter period of time?



#add comboIDs for unique (and one-parameter!) identification
test_cmbos_not_sold_R6 <- merge(test_cmbos_not_sold_R6, combos_df, by=c('item_id', 'shop_id')) %>%
  select(combo_id, item_id, shop_id, Units_Sold, combo_ttl_sales)

test_cmbos_not_sold_R3 <- merge(test_cmbos_not_sold_R3, combos_df, by=c('item_id', 'shop_id')) %>%
  select(combo_id, item_id, shop_id, Units_Sold, combo_ttl_sales)

test_cmbos_not_sold_R1 <- merge(test_cmbos_not_sold_R1, combos_df, by=c('item_id', 'shop_id')) %>%
  select(combo_id, item_id, shop_id, Units_Sold, combo_ttl_sales)

monthly_sales <- merge(monthly_sales, combos_df[,c('item_id', 'shop_id', 'combo_id')], by=c('item_id', 'shop_id')) 



#check out some of the top selling items (over the entire window) that appear to be discontinued
#get the seemingly-discontinued items, and rank them by total sales desc.
test_cmbos_not_sold_R6 <- test_cmbos_not_sold_R6 %>% arrange(desc(combo_ttl_sales))
test_cmbos_not_sold_R3 <- test_cmbos_not_sold_R3 %>% arrange(desc(combo_ttl_sales))
test_cmbos_not_sold_R1 <- test_cmbos_not_sold_R1 %>% arrange(desc(combo_ttl_sales))

#colnames(monthly_sales)
# head(test_cmbos_not_sold_R6, 20)



#graph them over time
pdf("Discontinued_Combos_No_R6_Sales.pdf")
for (cmbo_id in test_cmbos_not_sold_R6[1:100,]$combo_id)
{
  
  plt <-  ggplot(subset(monthly_sales, combo_id ==cmbo_id),
                 aes(x=mnth, y=Units_Sold)) +
    geom_point() +
    xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
    ggtitle(paste("ComboID: ",cmbo_id)) + theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Month") + ylab("Units Sold")  +
    theme(legend.position="none")
  
  print(plt)
}
dev.off()


#check out seemingly discontinued as per no sales in last 3 mos
#get the seemingly-discontinued items, and rank them by total sales desc.

#exclude the ones that were already considered in the "no sales in 6 mos" view
test_cmbos_not_sold_R3_small <- test_cmbos_not_sold_R3[!(test_cmbos_not_sold_R3$combo_id %in% test_cmbos_not_sold_R6$combo_id), ]
test_cmbos_not_sold_R3_small <- test_cmbos_not_sold_R3_small %>% arrange(desc(combo_ttl_sales))


#graph them over time
pdf("Discontinued_Combos_No_R3_Sales.pdf")
for (cmbo_id in test_cmbos_not_sold_R3_small[1:100,]$combo_id)
{
  
  plt <-  ggplot(subset(monthly_sales, combo_id ==cmbo_id),
                 aes(x=mnth, y=Units_Sold)) +
    geom_point() +
    xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
    ggtitle(paste("ComboID: ",cmbo_id)) + theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Month") + ylab("Units Sold")  +
    theme(legend.position="none")
  
  print(plt)
}
dev.off()



#check out seemingly discontinued as per no sales in last 1 mo
#get the seemingly-discontinued items, and rank them by total sales desc.

#exclude the ones that were already considered in the "no sales in 3 (or 6) mos" view 
test_cmbos_not_sold_R1_small <- test_cmbos_not_sold_R1[!(test_cmbos_not_sold_R1$combo_id %in% test_cmbos_not_sold_R3$combo_id), ]
test_cmbos_not_sold_R1_small <- test_cmbos_not_sold_R1_small %>% arrange(desc(combo_ttl_sales))

#graph them over time
pdf("Discontinued_Combos_No_R1_Sales.pdf")
for (cmbo_id in test_cmbos_not_sold_R1_small[1:100,]$combo_id)
{
  
  plt <-  ggplot(subset(monthly_sales, combo_id ==cmbo_id),
                 aes(x=mnth, y=Units_Sold)) +
    geom_point() +
    xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
    ggtitle(paste("ComboID: ",cmbo_id)) + theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Month") + ylab("Units Sold")  +
    theme(legend.position="none")
  
  print(plt)
}
dev.off()


#don't see any patterns that indicate decent chance of sales.  Expect some noise in the ones with sales in Sept etc but small 
#      for all stores in total so don't see any good way to predict a store breakdown.  Going to set these to zero and not bother
#      with a more mathematical algorithm.  Can always tighten the params on which ones to let down this path and send more down the 
#      "generate automated forecast" path if time permits.










# ==================================================================================
# Look for stores with ZERO sales of ANY items for a whle   ==> set forecast to zero
# ==================================================================================
#a) closed last month, but not closed in Sept
#Tyumen shopping mall Green Beach (shopID 51) has no results for Oct 2015.  
#Blip or permanently closed?  Note there's a "Tyumen shopping mall" that's still going at usual rate in Oct 2015.  

#a) not closed last month, but never appear to sell in Nov  (noted in review of Units sold by store pdf) 
#Visiting Commerce store has sales in every Oct, and ONLY in Octobers.  ==> forecast 0
#Ditto for Moscow Sales store has sales in every Oct, and ONLY in Octobers.  ==> forecast 0
# shops_df[shops_df$shop_name == 'Visiting Commerce' | shops_df$shop_name == 'Moscow Sales', ] 
#   shopid 9, shopid 20



# head(mo_sales_by_store)
# mo_sales_by_store %>% group_by(mnth) %>% summarize(nbr_stores_selling=n_distinct(shop_id))  %>% View()  #45-50 per mo

#c) Otherwise, the stores either look closed (zero sales for 2+  3+? months)

# subset(mo_sales_by_store, ( mnth==as.Date("1-10-2015","%d-%m-%Y") | mnth==as.Date("1-09-2015", "%d-%m-%Y") | mnth==as.Date("1-08-2015", "%d-%m-%Y")) & 
#                             Units_Sold ==0 )

#store 27:   Moscow shopping center MEGA Belaya Dacha II 
# subset(mo_sales_by_store, shop_id == 27)
# has 0 in Aug, -1 (return?) in Sept, 0 in Oct ==>   going to force the forecast to zero

#otherwise, the stores look like they've been closed for a while - no sales in 2+ mos


#d) or the stores have results up to Oct 2015, ==>   need a forecast

#shopID 36; Novosibirsk SEC Gallery Novosibirsk
#false positive - looks like a potential closer b/c of no sales in Sept or Aug, but is exact opposite- is just opened



# exec summary:   force forecast to 0 for the stores closed in Oct PLUS shops 9 and 20, which appear to only be open in OCT
subset(mo_sales_by_store, mnth==as.Date("1-10-2015","%d-%m-%Y") & Units_Sold ==0, select = shop_id) %>% View()
# 0 sales in Nov (and generally for a while prior):  shop_ids  0, 1, 8, 11, 13, 17, 23, 27, 29, 30, 32, 33, 40, 43, 51, 54
#   add in 9, 20

subset(test_df, shop_id %in% c(0, 1, 8, 11, 13, 17, 23, 27, 29, 30, 32, 33, 40, 43, 51, 54, 9, 20)) %>%
  group_by(shop_id) %>% summarize(n_itms=n_distinct(item_id))
#*****************************
#0 results ==>   predictions aren't required for any of these.
#*****************************













#look by locations
#=================

#get a full list of locations and ttl sales so we can make sure zero-sales locations are not overlooked and sort locations big-small
#---------------------------------------------------------------------------------------------------
sales_ttl_by_location <- monthly_sales %>%
  group_by(location) %>%
  summarize(Units_Sold_Ttl=sum(Units_Sold))

sales_ttl_by_location <- sales_ttl_by_location[order(sales_ttl_by_location$Units_Sold_Ttl, decreasing=TRUE, na.last=TRUE), ]
items_df_w_locn_sales <- merge(items_df, sales_ttl_by_location, by="location", all.x=TRUE)

tail(sales_ttl_by_location)


mo_sales_by_locn <- monthly_sales %>%
                        group_by(date_block_num, mnth, location) %>%
                            summarize(
                                Units_Sold = sum(Units_Sold),
                                NbrD_Items_Sold = n_distinct(item_id)
                                    )



#graph # of units sold per mo, with one page/graph for each location
#----------------------------------------------------------------
pdf("Nbr_Units_Sold_by_Location.pdf")
i <- 0
for (locn in unique(sales_ttl_by_location[1:4, ]$location)) 
{  
  i <- i + 1
  plt <- ggplot(data=mo_sales_by_locn[mo_sales_by_locn$location==locn, ], 
                aes(x=mnth, y=Units_Sold)) + 
    geom_point() + 
    xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
    ggtitle(paste("Location: ",locn)) + theme(plot.title = element_text(hjust = 0.5)) + 
    xlab("Month") + ylab("Units sold")  
  print(plt)
  switch(i, plt1<-plt, plt2<-plt, plt3<-plt, plt4<-plt)
}
grid.arrange(plt1, plt2, plt3, plt4)
dev.off()





#graph # of distinct items sold per location, with one page/graph for each location
#----------------------------------------------------------------
pdf("NbrD_Items_Sold_by_Location.pdf")
for (locn in unique(sales_ttl_by_location$location)) 
{  
  plt <- ggplot(data=mo_sales_by_locn[mo_sales_by_locn$location==locn, ],
                aes(x=mnth, y=NbrD_Items_Sold)) + 
    geom_point() + 
    xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
    ggtitle(paste("Location: ",locn)) + theme(plot.title = element_text(hjust = 0.5)) + 
    xlab("Month") + ylab("Count of Distinct Items Sold")  
  print(plt)
}
dev.off()



#graph units sold with a series for each store, with one page/graph for each location
#----------------------------------------------------------------
pdf("Units_Sold_by_Location_and_Store.pdf")
i <- 0
for (locn in unique(sales_ttl_by_location$location)) 
{  
  i <- i + 1
  plt <- ggplot(data=mo_sales_by_store[mo_sales_by_store$location==locn, ],
                aes(x=mnth, y=Units_Sold), group=shop_name) + 
    geom_line(aes(color=shop_name)) + 
    xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
    ggtitle(paste("Location: ",locn)) + theme(plot.title = element_text(hjust = 0.5)) + 
    xlab("Month") + ylab("Units Sold") + 
    theme(legend.position = "bottom")
  print(plt)
  switch(i, plt1<-plt, plt2<-plt, plt3<-plt, plt4<-plt)
}
grid.arrange(plt1, plt2, plt3, plt4)
dev.off()



















# read in test.csv
# ================
test_df <- read.csv("test.csv")
str(test_df)
head(test_df)

nrow(test_df)  #214,200
length(unique(test_df$ID))  #214200, same as # of rows.  ==> every row has a unique ID
sum(is.na(test_df$ID))  #0    and none of them are null

test_df %>%
    group_by(item_id, shop_id) %>%
      summarize(nbr=n_distinct(ID)) %>%
          filter(nbr!=1)
#0 rows found.  good, no itemID X shopID combo is repeated

test_df %>%
  group_by(ID) %>%
      summarize(nbr=n()) %>%
          filter(nbr!=1)
#0 rows found.  good, another way of showing ID's are unique



length(unique(test_df$item_id))  #5100, much lower than # of items in training set
length(unique(monthly_sales$item_id))  #21807, much lower than # of items in training set
length(unique(na.omit(monthly_sales)$item_id))  #21807



#find itemsXshop combos that were found in test and:
# a) item and shop combo in test set were found in the training set
# b) item and shop combo in test set were NOT found in the training set
        #i)   the item in test was NEVER found in training, regardless of shop
        #ii)  the shop in test was NEVER found in training, regardless of item

        #iii) the item in test was found in training too, but not for the shop given in test
        #iv)  the shop in test was found in training too, but not for the item given in test





#get lists of items found in test and training
itms_in_test_df <- data.frame(unique(test_df$item_id))
colnames(itms_in_test_df) <- c("item_id")
itms_in_train_df <- data.frame(unique(monthly_sales$item_id))
colnames(itms_in_train_df) <- c("item_id")

item_ALL_df <- merge(itms_in_train_df, itms_in_test_df, by=c("item_id"), all=TRUE)
nrow(item_ALL_df)  #22170;   greater than 21807 in monthly sales ==> need forecasts for items not in training


shops_in_test_df <- data.frame(unique(test_df$shop_id))
colnames(shops_in_test_df) <- c("shop_id")
shops_in_train_df <- data.frame(unique(monthly_sales$shop_id))
colnames(shops_in_train_df) <- c("shop_id")

shops_ALL_df <- merge(shops_in_train_df, shops_in_test_df, by=c("shop_id"), all=TRUE)
nrow(shops_ALL_df)  #60

nrow(items_df)


#are there any shopIDs in training or test that aren't in the master list of shops, or vice versa?
# ------------------------------------------------------------------------------------------------
shops_df[ ! (shops_df$shop_id %in% unique(shops_ALL_df$shop_id)) ,  ]
#no records found.  good, no shop IDs in the master list that aren't in either the test or training


shops_ALL_df[ ! (shops_ALL_df$shop_id %in% unique(shops_df$shop_id)), ]
#no records found.  good, no addnl shop IDs in test or training that arent in the master list



#are there any itemIDs in training or test that aren't in the master list of items, or vice versa?
# ------------------------------------------------------------------------------------------------
items_df[ ! (items_df$item_id %in% unique(item_ALL_df$item_id)) , ]
#no records found.  good, no item IDs in the master list that aren't in either the test or training

item_ALL_df[ ! (item_ALL_df$item_id %in% unique(items_df$item_id)), ]
#no records found.  good, no addnl item IDs in test or training that arent in the master list

nrow(item_ALL_df) #22170
nrow(items_df)   #22170


itemXshop_train_df <- monthly_sales %>%
  group_by(item_id, shop_id) %>%
  summarize(Nbr_Mos=n_distinct(mnth),
            First_Mo=min(mnth),
            Last_Mo=max(mnth)
  )


itemXshop_test_df <- test_df %>%
  group_by(item_id, shop_id) %>%
  summarize(Nbr_Rcds=n_distinct(ID))

head(itemXshop_train_df)
head(itemXshop_test_df)
unique(itemXshop_test_df$Nbr_Rcds) #1, good, each itemXshop combo is only found once


#create the full outer join of combos in training and/or test
itemXshop_ALL_df <- merge(itemXshop_train_df, test_df, by=c("item_id", "shop_id"), all.x = TRUE, all.y=TRUE)
nrow(itemXshop_ALL_df)  #526,920

#nrow(itemXshop_train_df)  #424,124
#nrow(test_df)  #214,200
#  ==>  delta between 526k and 424k ==> a LOT of combos have no training data!!!


head(itemXshop_ALL_df)

#create a column to track status
itemXshop_ALL_df$combo_status <- 'Not found in both'

# a) item and shop combo in test set were found in the training set
combos_found_in_both <- !is.na(itemXshop_ALL_df$ID) & !is.na(itemXshop_ALL_df$Nbr_Mos)
itemXshop_ALL_df[combos_found_in_both, ]$combo_status <- 'Found in both'

# b) item and shop combo in test set, but not found in the training set
  #  these are problems we need to deal with!
combos_found_only_in_test <- !is.na(itemXshop_ALL_df$ID) & is.na(itemXshop_ALL_df$Nbr_Mos)
itemXshop_ALL_df[combos_found_only_in_test, ]$combo_status <- 'Found only in test'

# c) item and shop combo in training set, but not found in the test set
#  these are not problems.  Might be useful for modeling this/similar item and/or this/similar shop
#                          similar item == item from same category and/or supercategory;    similar shop==shop from same location
combos_found_only_in_training <- is.na(itemXshop_ALL_df$ID) & !is.na(itemXshop_ALL_df$Nbr_Mos)
itemXshop_ALL_df[combos_found_only_in_training, ]$combo_status <- 'Found only in train'

# d) for completeness, this would be times the combo is in neither test nor training (would have been found if we 
#             did a cross join on entire item list X entire shop list.  Since we started from the superset of combos in test
#              and/or train instead, we have omitted these in our construction of the itemXshop_ALL_df "complete" set)


#get counts by combo status
itemXshop_ALL_df %>% group_by(combo_status) %>% summarize(nbr=n())


head(itemXshop_ALL_df)

#give details about the item status, so we can subdivide "combo not in training" situations for optimal fcst
# ----------------------------------------------------------------------------------------------------------
itemXshop_ALL_df$item_status <- 'In either train or test'   #if this is done right, these should all get updated to something more specific

# a) item in test set was also found in the training set
items_found_in_both <- itemXshop_ALL_df$item_id %in% test_df$item_id & itemXshop_ALL_df$item_id %in% monthly_sales$item_id 
itemXshop_ALL_df[items_found_in_both, ]$item_status <- 'Found in both'

# b) item was in test set, but was not found in the training set
items_found_only_in_test <- itemXshop_ALL_df$item_id %in% test_df$item_id & !itemXshop_ALL_df$item_id %in% monthly_sales$item_id 
itemXshop_ALL_df[items_found_only_in_test, ]$item_status <- 'Found only in test'

# length(items_found_only_in_test)
# sum(items_found_only_in_test)  #15246    -- brand new item - no training data from ANY store to project from

# c) item was in training set, but was not found in the test set
items_found_only_in_train <- !itemXshop_ALL_df$item_id %in% test_df$item_id & itemXshop_ALL_df$item_id %in% monthly_sales$item_id 
itemXshop_ALL_df[items_found_only_in_train, ]$item_status <- 'Found only in train'

# length(items_found_only_in_train)  #526920
# sum(items_found_only_in_train)  #289924


#review the counts
itemXshop_ALL_df %>% group_by(item_status) %>% summarize(nbr=n())   #good, no "in either" cases left over
itemXshop_ALL_df %>% group_by(combo_status, item_status) %>% summarize(nbr=n())   #good, no "in either" cases left over




#give details about the shop status, so we can subdivide "combo not in training" situations for optimal fcst
# ----------------------------------------------------------------------------------------------------------
itemXshop_ALL_df$shop_status <- 'In either train or test'   #if this is done right, these should all get updated to something more specific

# a) item in test set was also found in the training set
shops_found_in_both <- itemXshop_ALL_df$shop_id %in% test_df$shop_id & itemXshop_ALL_df$shop_id %in% monthly_sales$shop_id 
itemXshop_ALL_df[shops_found_in_both, ]$shop_status <- 'Found in both'

# b) item was in test set, but was not found in the training set
shops_found_only_in_test <- itemXshop_ALL_df$shop_id %in% test_df$shop_id & !itemXshop_ALL_df$shop_id %in% monthly_sales$shop_id 
itemXshop_ALL_df[shops_found_only_in_test, ]$shop_status <- 'Found only in test'

# length(shops_found_only_in_test)
# sum(shops_found_only_in_test)  #15246    -- brand new item - no training data from ANY store to project from

# c) item was in training set, but was not found in the test set
shops_found_only_in_train <- !itemXshop_ALL_df$shop_id %in% test_df$shop_id & itemXshop_ALL_df$shop_id %in% monthly_sales$shop_id 
itemXshop_ALL_df[shops_found_only_in_train, ]$shop_status <- 'Found only in train'

# length(shops_found_only_in_train)  #526920
# sum(shops_found_only_in_train)  #289924


#review the counts
itemXshop_ALL_df %>% group_by(shop_status) %>% summarize(nbr=n())   #good, no "in either" cases left over
itemXshop_ALL_df %>% group_by(combo_status, shop_status) %>% summarize(nbr=n())   
  #good, found in both combo has only found inboth shop status;  found only in train shop status is only applicable to found only in train combos


itemXshop_ALL_df %>% group_by(combo_status, item_status, shop_status) %>% summarize(nbr=n())   
# combo_status        item_status         shop_status            nbr
# 1 Found in both       Found in both       Found in both       111404
    #most straightforward cases.  Hopefully can model these combos from only? with more wght on? their training data

# 2 Found only in test  Found in both       Found in both        87550
  #we have evidence how these items and stores have behaved, but not in combo

# 3 Found only in test  Found only in test  Found in both        15246
  #we have no training data for these items at all.   
    #At least we have info on how these stores are doing, albeit not for these specific items.
    #should we look at store performance relative to avg store performance???



# 4 Found only in train Found in both       Found only in train  22796
# 5 Found only in train Found only in train Found in both       225680
# 6 Found only in train Found only in train Found only in train  64244
      #these will be informative, but we don't need to make a prediction for them



#for case 1, how many months of training data do we typically have?
subset(itemXshop_ALL_df, combo_status =='Found in both' & item_status =='Found in both') %>%
  group_by(Nbr_Mos) %>% summarize(nbr_combos=n_distinct(ID)) %>% View()
  #26.9k combos with only one month of data;  18k with only 2 mos of data; ....


subset(itemXshop_ALL_df, combo_status =='Found in both' & Nbr_Mos < 10) %>%
  group_by(Last_Mo) %>% summarize(nbr_combos=n_distinct(ID)) %>% View()
#26.9k combos with only one month of data;  18k with only 2 mos of data; ....



#for case 3 - combo is in test, so forecast needed, but the item is not even found in the training set:
#what category are these in?
subset(itemXshop_ALL_df, combo_status =='Found only in test' & item_status =='Found only in test') %>%
    #nrow() #15246 
    #group_by() %>% summarize(nbrD=n_distinct(item_id))  #363 distinct item_ids
    #group_by(item_id) %>% summarize(nbr_shops=n_distinct(shop_id))  %>% filter(nbr_shops==42)     #which are each going to be sold in 42 shops
  merge(items_df, by='item_id') %>%
  merge(categories_df, by='category_id') %>%
  group_by(category_id, category_name) %>% summarize(nbr_items=n_distinct(item_id)) %>%
  View()   #are in 39 categories;  books, music, games, ...    from ~70 items per category to 1


#create a master list of combos, and which ones are needed for (in general), and which ones have a forecast by rule vs. automated forecast
combos_df <- merge(itemXshop_ALL_df, items_df_w_sales, by='item_id')   #merge in item details
combos_df <- merge(combos_df, shops_df_w_sales, by='shop_id')          #merge in shop details
combos_df <- merge(combos_df, categories_df_w_sales, by='category_id')   #merge in item details


#create a new, unique identifier for the combo for simplicity
combos_df$combo_id <-combos_df$ID
n_na <- nrow(combos_df[is.na(combos_df$combo_id), ])   #312,720
combos_df[is.na(combos_df$combo_id), ]$combo_id <- seq(1000001,(1000000+n_na))

# length(unique(combos_df$ID))  #214,200
# length(unique(combos_df$combo_id))  #526,920
#nrow(combos_df)

# min(combos_df[!is.na(combos_df$ID), ]$ID)  #0
# max(combos_df[!is.na(combos_df$ID), ]$ID)  #214,199
# 
# min(combos_df[!is.na(combos_df$combo_id), ]$combo_id)   #0
# max(combos_df[!is.na(combos_df$combo_id), ]$combo_id)   #1,312,720
# 
# length(unique(combos_df[combos_df$combo_id>1000000, ]$combo_id))  #312,720
# length(unique(combos_df[combos_df$combo_id<=1000000, ]$combo_id))  #214,200



#rearrange the columns and omit some unnecessary ones ('item_name_original', )
combos_df <- subset(combos_df, select=c('combo_id', 
                                               'item_id', 'item_name', 'Units_Sold_Ttl',
                                                'shop_id', 'shop_name', 'location', 'shop_sales_ttl',  
                                               'category_id', 'category_name', 'super_category', 'category_sales_ttl', 
                                               'combo_status', 'item_status', 'shop_status', 
                                                'Nbr_Mos', 'First_Mo', 'Last_Mo', 'ID'))

colnames(combos_df) <- c('combo_id', 'item_id', 'item_name', 'item_sales_ttl',
                                        'shop_id', 'shop_name', 'location', 'shop_sales_ttl',  
                                        'category_id', 'category_name', 'super_category', 'category_sales_ttl', 
                                        'combo_status', 'item_status', 'shop_status', 
                                        'Nbr_Mos', 'First_Mo', 'Last_Mo', 'ID__test_set')


#flag the combos needing forecasts for simplicity in finding them
combos_df$fcst_needed <- !is.na(combos_df$ID__test_set)





#Forecast by rule situations
#      a) store has been closed  (if we saw no sales in that store for several months, assume it's closed)
#                       there are a bunch of stores like this, but we aren't asked to make predictions for them
#
#      b) item has been discontinued - no sales in any store in recent months)  => forecast zero sales of that item in the next mo
#             potentially some of these tick back up b/c Nov seems to be a gift giving season but very low volume and/or trends have dropped
#             sharply in most cases so just set to zero and move on


#a) item has not been sold in ANY store in the last 1 (3?) mos
#create field, set flag based on this rule (will override w/ later rules as needed)
combos_df$fcsted_by_rule_flag <- combos_df$item_id %in% top_sellers_discontd_R1_full_df$item_id
    #checks:
    # sum(combos_df$fcsted_by_rule_flag & combos_df$fcst_needed)  #31332
    # sum(combos_df$fcsted_by_rule_flag)    #33,992 -- it's OK this is higher; happens b/c the full matrix of itemsXshops in here has extra shops for the 746 items
    
#create field, set value
combos_df$fcst_by_rule_val <- 0
#to avoid confusion, null it out for NON rule driven forecasts
combos_df[!combos_df$fcsted_by_rule_flag, ]$fcst_by_rule_val <- NA

#create field, set value
combos_df$fcst_rule_desc <- 'Zero sales for this item in ANY shop'
#to avoid confusion, null it out for NON rule driven forecasts
combos_df[!combos_df$fcsted_by_rule_flag, ]$fcst_rule_desc <- NA



#b) item has not been sold in this store
rws_to_update <- !combos_df$fcsted_by_rule_flag & combos_df$combo_id %in% test_cmbos_not_sold_R1$combo_id
# sum(rws_to_update) #72,195

combos_df[rws_to_update, ]$fcst_rule_desc <- 'Zero sales for this item in THIS shop'
combos_df[rws_to_update, ]$fcst_by_rule_val <- 0
combos_df[rws_to_update, ]$fcsted_by_rule_flag <- TRUE



#check counts:   combos_df %>% group_by(fcst_rule_desc, fcst_by_rule_val) %>% summarize(nbr_combos=n(), NbrD_CboIDs=n_distinct(combo_id), NbrD_items=n_distinct(item_id))

#110,673 combos still need a forecast
#nrow(combos_df[combos_df$fcst_needed & !combos_df$fcsted_by_rule_flag, ])  

#and 28,634 of them have training data for the exact item and shop needing a forecast
#nrow(combos_df[combos_df$fcst_needed & !combos_df$fcsted_by_rule_flag & combos_df$combo_status == 'Found in both', ])

#check
# combos_df[combos_df$fcst_needed & !combos_df$fcsted_by_rule_flag & combos_df$combo_status == 'Found in both', ] %>%
#     merge(monthly_sales, by=c('item_id', 'shop_id')) %>%
#         filter(mnth==as.Date('10-01-2015', '%m-%d-%Y') & Units_Sold == 0) 
#as expected, since we decided to make all the "last mo was zero" combos to be rule coded, there are no records needing processing w/o a record in Oct'15

#rlang::last_error()















#where an automated forecast is needed:
#forecast by naive
#forecast by analogy (category)
#forecast by ETS

# thought about, rejecting
# ------------------------
     # forecast by seasonal naive  - while Nov does show a lot of seasonality, there is 
                      #even more often a ton of negative trend, so doesn't seem likely to be worth the time
     #ditto for forecast by mean


#mark combos with a "naive" method
  #if the combo exists in training:
    #if have obs within the last 1? 2? ? mos, use that
    #if not - 
        #and the store has sales in the last 1-3 mos, use the avg at other stores??

  #if the combo DOES NOT exist in training:
      #if the ITEM has training history, 
          #naive1:   use the avg of last month results for that item at other shops in this location  (what if none?)
          #naive2:   use the avg of last month results for that item at other shops anywhere   (what if none?)

      #if the ITEM DOES NOT exist in training, 
          #naive3:   use the avg of first month results for other items in that category and shop   (what if none?)
          #naive4:   use the avg of first month results for other items in that category and location   (what if none?)
              #this is assuming that the item is new to the world, and will behave most like other "new to the world" items.  
              #seems the most likely scenario, esp. w/ books, games, etc

          #naive3:   use the avg of last month results for other items in that category and shop   (what if none?)
          #naive4:   use the avg of last month results for other items in that category and location   (what if none?)
              #this is assuming the item is new to this set of STORES, and will have demand like the "as of now" demand for other items in that category
  

#loop over combos, create the forecast, score it on the training set?  eval set? tsCV?

fcsts_still_needed_have_training_data <- combos_df$fcst_needed & !combos_df$fcsted_by_rule_flag & combos_df$combo_status == 'Found in both'
# just use this item&shop's data, and
# method 1:   lm
# method 2:   ets
# method 3, 4, 5, ...  get crazier as time permits
# combos_df[fcsts_still_needed_have_training_data, ] %>% 
#     group_by(combo_status, item_status, shop_status) %>% summarize(nbr_combos=n())



fcsts_still_needed_no_training_data <- combos_df$fcst_needed & !combos_df$fcsted_by_rule_flag & !combos_df$combo_status == 'Found in both'
fcsts_still_needed_NO_training_data_EXI_item <- combos_df$fcst_needed & 
                                                !combos_df$fcsted_by_rule_flag & 
                                                !combos_df$combo_status == 'Found in both' &
                                                combos_df$item_status == 'Found in both'
fcsts_still_needed_NO_training_data_NEW_item <- combos_df$fcst_needed & 
                                                !combos_df$fcsted_by_rule_flag & 
                                                !combos_df$combo_status == 'Found in both' &
                                                combos_df$item_status == 'Found only in test'

# combos_df[fcsts_still_needed_no_training_data, ] %>% 
     group_by(combo_status, item_status, shop_status) %>% summarize(nbr_combos=n())
# 
# combo_status       item_status        shop_status   nbr_combos
#   1 Found only in test Found in both      Found in both      66793
#           ==>   method 1:   filter for this item.   use all shops, and create an LM, ETS, ets
#           ==>   method 2:   filter for this item.   use all shops in this location, and create an LM, ETS, ets
#           ==>   ????method 3:   filter for this SHOP, use all items in this category??  and create an LM, ETS, ets
#           ==>   ????method 4:   filter for this SHOP, use all items  and create an LM, ETS, ets
#   
#   
#   2 Found only in test Found only in test Found in both      15246
#             ==>   Method1:   filter to this shop's performance; use the other items in this category to predict this item
#             ==>   Method2:   filter to the shops in this location; use the other items in this category to predict this item
# 



#create a list of forecasts to create for case1, method1:
fld_lst <- c('combo_id','item_id','shop_id', 'category_id', 'location')
nbr_cmbos <- sum(fcsts_still_needed_have_training_data)
df_fcsts1 <- data.frame(
                          cbind(combos_df[fcsts_still_needed_have_training_data, fld_lst]),
                          data_prep_fn = rep('byComboID',nbr_cmbos),
                          modeling_method = rep('naive',nbr_cmbos)
                      )
    

#create a list of forecasts to create for case2a, method1:
nbr_cmbos <- sum(fcsts_still_needed_NO_training_data_EXI_item)
df_fcsts2_EXI <- data.frame(
                        cbind(combos_df[fcsts_still_needed_NO_training_data_EXI_item, fld_lst]),
                        data_prep_fn = rep('byItemID', nbr_cmbos),
                        modeling_method = rep('mean',nbr_cmbos)
                        )

nbr_cmbos <- sum(fcsts_still_needed_NO_training_data_NEW_item)
df_fcsts2_NEW <- data.frame(
                          cbind(combos_df[fcsts_still_needed_NO_training_data_NEW_item, fld_lst]),
                          data_prep_fn = rep('byShopID_and_CtgryID', nbr_cmbos),
                          modeling_method = rep('mean',nbr_cmbos)
                        )

df_fcsts_all <- data.frame(rbind(df_fcsts1, df_fcsts2_EXI, df_fcsts2_NEW))



#create columns for forecast and accuracy metric(s)
df_fcsts_all$fcst_val <- 0
df_fcsts_all$acc_score1_train <- 0


nrow(df_fcsts1)  #28634
nrow(df_fcsts2_EXI)  #66793
nrow(df_fcsts2_NEW)  #15246

nrow(df_fcsts_all)  #28634





#create data prep functions
#--------------------------
#a) the results for this combo
fn_data_prep__ByComboID <- function(df, cmbo_id) 
{ 
  df_bycmboid <- subset(df, combo_id == cmbo_id, select=c('mnth', 'Units_Sold')) 
  
  mindt <- min(df_bycmboid$mnth)
  maxdt <- max(df_bycmboid$mnth)
  
  #fill in missing months, if any, with zeros
  if ( nrow(df_bycmboid) != as.integer(round((maxdt-mindt)/(365.25/12)) + 1)  )
  {
    full_seq <- data.frame( mnth=seq.Date(from=mindt, to=maxdt, by="month") )
    df_bycmboid <- merge(full_seq, df_bycmboid, by='mnth', all.x = TRUE)
    df_bycmboid[is.na(df_bycmboid$Units_Sold), 'Units_Sold'] <- 0
  }
  
  #sort into chronological order
  df_bycmboid <- df_bycmboid[order(df_bycmboid$mnth), ]
  
  return(df_bycmboid)
}
# tstfnout <- fn_data_prep__ByComboID(monthly_sales, 147869)





#b) the AVG sales for this item (avg across stores)
fn_data_prep__ByItemID <- function(df, itm_id) 
{ 

  df_byitmid <- subset(df, item_id == itm_id, select=c('mnth', 'Units_Sold', 'shop_id')) 
  
  df_byitmid2 <- group_by(df_byitmid, mnth) %>% summarize(Units_Sold=sum(Units_Sold))
  
  mindt <- min(df_byitmid2$mnth)
  maxdt <- max(df_byitmid2$mnth)
  mos_in_srs <- as.integer( round( (maxdt-mindt)/(365.25/12) ) + 1)
  
  #fill in missing months, if any, with zeros
  if ( nrow(df_byitmid2) != mos_in_srs )
  {
    full_seq <- data.frame( mnth=seq.Date(from=mindt, to=maxdt, by="month") )
    df_byitmid2 <- merge(full_seq, df_byitmid2, by='mnth', all.x = TRUE)
    df_byitmid2[is.na(df_byitmid2$Units_Sold), 'Units_Sold'] <- 0
  }
  
  #get the number of distinct shops selling this item (ever, not just for each month)
  NbrD_Shops <- df_byitmid %>% summarize(NbrD_Shops=n_distinct(shop_id))

  if (mos_in_srs > 5)
  {
    #if this item has been sold somewhere for a while (isn't a new-to-market product), assume it will NOT start being sold in the new store and 
    # and the lack of previous sales is the best predictor of how it will do next month  ==>0 is the best forecast
    df_byitmid2$Units_Sold <- 0
  } else
  {
    #if this item is a new product, assume it's being introduced to a new store and will sell there like it does elsewhere 
    #   ==> use the average per shop as the best predictor of what it will do in the new shop.   
    #divide by # of shops selling this item
    df_byitmid2$Units_Sold <- df_byitmid2$Units_Sold/NbrD_Shops[[1]]
  }
  
  #sort into chronological order
  df_byitmid2 <- df_byitmid2[order(df_byitmid2$mnth), ]

  return(df_byitmid2)
}
# tstfnout <- fn_data_prep__ByItemID(monthly_sales, 5638)



#c) the AVG sales for this shop of items in this category (avg across items)
fn_data_prep__ByShopID_and_CtgryID <- function(df, shp_id, ctgry_id) 
{ 
  
  df_byshp_and_ctgry <- subset(df, shop_id == shp_id & category_id == ctgry_id, select=c('mnth', 'Units_Sold', 'item_id')) 
  
  if ( nrow(df_byshp_and_ctgry) == 0 ) 
  {   #if no sales are found for this category, use all sales in the shop.  the shop should always have sales or we wouldn't have gotten here
    df_byshp_and_ctgry <- subset(df, shop_id == shp_id, select=c('mnth', 'Units_Sold', 'item_id')) 
  }
  
  #get sales per month
  df_byshp_and_ctgry2 <- group_by(df_byshp_and_ctgry, mnth) %>% summarize(Units_Sold=sum(Units_Sold))
  
  mindt <- min(df_byshp_and_ctgry2$mnth)
  maxdt <- max(df_byshp_and_ctgry2$mnth)
  
  #fill in missing months, if any, with zeros
  if ( nrow(df_byshp_and_ctgry2) != as.integer(round((maxdt-mindt)/(365.25/12)) + 1)  )
  {
    full_seq <- data.frame( mnth=seq.Date(from=mindt, to=maxdt, by="month") )
    df_byshp_and_ctgry2 <- merge(full_seq, df_byshp_and_ctgry2, by='mnth', all.x = TRUE)
    df_byshp_and_ctgry2[is.na(df_byshp_and_ctgry2$Units_Sold), 'Units_Sold'] <- 0
  }
  
  #get the number of distinct items sold in this shop from this category (ever, not just for each month)
  NbrD_Items <- df_byshp_and_ctgry %>% summarize(NbrD_Shops=n_distinct(item_id))
  
  #divide by # of shops selling this item
  df_byshp_and_ctgry2$Units_Sold <- df_byshp_and_ctgry2$Units_Sold/NbrD_Items[[1]]
  
  #sort into chronological order
  df_byshp_and_ctgry2 <- df_byshp_and_ctgry2[order(df_byshp_and_ctgry2$mnth), ]

    return(df_byshp_and_ctgry2)
}

# tstfnout <- fn_data_prep__ByShopID_and_CtgryID(monthly_sales, 6, 7)
# head(df_fcsts2_NEW)
# str(tstfnout)

# create some forecast functions
# ------------------------------
fn_fcst_naive <- function(ts) { naive(ts, h=1) }
fn_fcst_mean <- function(ts) { meanf(ts, h=1) }
fn_fcst_lm <- function(ts) 
{
  if (length(ts) > 12) 
    tslm_out <- tslm(ts ~ trend + season) else  
    tslm_out <- tslm(ts ~ trend)   #getting errors predicting a new season (month of Nov) if the original data didn't have that season (b/c it started after Nov 2014)

  return (forecast(tslm_out, h=1))  
}

fn_fcst_ets <- function(ts) 
{
    ets_out <- forecast::ets(ts)   
    return (forecast(ets_out, h=1))  
}



#attempt 2 - use tslm for all series
# df_fcsts_all$modeling_method <- 'tslm'
# df_fcsts_all$fcst_val <- NA
# df_fcsts_all$acc_score1_train <- NA


#attempt 3 - use ets for all series w/ data for that comboID.  Given the magnitude of assumptions being made for the "combo not found" cases, 
#   seems silly to do more sophisticated methods for them
df_fcsts_all[df_fcsts_all$combo_id %in% df_fcsts1$combo_id, ]$modeling_method <- 'ets'

#df_fcsts_all[df_fcsts_all$combo_id %in% df_fcsts1$combo_id[1:20], ]


# nrow(df_fcsts1)
# str(df_fcsts1)
#sum(df_fcsts_all$combo_id %in% df_fcsts1$combo_id)  #28,634


# df_fcsts_all[1:20, ]
# df_fcsts_all[df_fcsts_all$combo_id %in% df_fcsts2_EXI[1:20, 'combo_id'], ]
# df_fcsts_all[df_fcsts_all$combo_id %in% df_fcsts2_NEW[1:20, 'combo_id'], ]


start_time <- Sys.time()
i <- 0


#loop over the rows in df_fcsts_all and create the forecast for each one
#for (cmbo_id in df_fcsts_all[1:20, ]$combo_id)
#for (cmbo_id in df_fcsts2_EXI[1:20, ]$combo_id)
#for (cmbo_id in df_fcsts2_NEW[1:20, ]$combo_id)
#for (cmbo_id in big_vals_fcsts_by_itmID$minComboID)
#for (cmbo_id in df_fcsts_all$combo_id)
for (cmbo_id in df_fcsts1$combo_id)
  {
  if (i==0) {print(paste('Started at: ', start_time))}
  #cmbo_id <- 47345
  vctr_4_this_cmbo <- df_fcsts_all$combo_id==cmbo_id   #get this vector once, so it doesn't have to be recalced multiple times
  df_fcst_rw <- df_fcsts_all[vctr_4_this_cmbo, ]   #get this row once, so we don't have to keep looking it up
    
  itmID <- df_fcst_rw[1, 'item_id']
  shpID <- df_fcst_rw[1, 'shop_id']
  ctgryID <- df_fcst_rw[1, 'category_id']
  dataprep_fn <- df_fcst_rw[1, 'data_prep_fn']
  mdlg_method <- df_fcst_rw[1, 'modeling_method']
  
  #extract the data needed byComboID
  df1 <- if (dataprep_fn=="byComboID") { fn_data_prep__ByComboID(monthly_sales, cmbo_id) } else
            if (dataprep_fn=="byItemID") { fn_data_prep__ByItemID(monthly_sales, itmID) } else
              if (dataprep_fn=="byShopID_and_CtgryID") { fn_data_prep__ByShopID_and_CtgryID(monthly_sales, shpID, ctgryID) } 

  #create the fcst
  if (nrow(df1)>1) 
  {
    start_yr <- year(min(df1$mnth))
    start_mo <- month(min(df1$mnth))
    end_yr   <- year(max(df1$mnth))
    end_mo   <- month(max(df1$mnth))
    time_srs <- ts(df1$Units_Sold, start=c(start_yr, start_mo), end=c(end_yr, end_mo), frequency = 12)

    fcst_outpt <- if (mdlg_method == "naive") {fn_fcst_naive(time_srs)} else
                      if (mdlg_method == "mean") {fn_fcst_mean(time_srs)} else
                        if (mdlg_method == 'tslm') {fn_fcst_lm(time_srs)} else
                          if (mdlg_method == 'ets') {fn_fcst_ets(time_srs)}

    #round the forecasted value to an int, and trim to within the valid range >0 (to 20??)
    fcstd_val <- fcst_outpt$mean[1] %>% replace(is.na(.), 0) %>% round() %>% max(0)

    #get some accuracy metrics/score it
    acc_score <- accuracy(fcst_outpt)[,'RMSE']
    
  } else
  {
    fcstd_val <- 0
    acc_score <- 0
  }
  

  #save results
  df_fcsts_all[vctr_4_this_cmbo, 'fcst_val']  <- fcstd_val
  df_fcsts_all[vctr_4_this_cmbo, 'acc_score1_train']  <- acc_score
    
  i <- i+1
  if (i %% 1000 == 0) {print(paste('Completed', i, 'combos'))}
}

end_time <- Sys.time()
print(paste('Started at:', start_time))
print(paste('Ended at:', end_time))


# nrow(combos_df[combos_df$First_Mo == as.Date('11/01/2014', '%m/%d/%Y') & combos_df$fcst_needed & !combos_df$fcsted_by_rule_flag, ])
# str(combos_df)
# warnings()


# save these in case we want to go back to them
# ---------------------------------------------
#write.csv(df_fcsts_all, file="Fcsts_Round1.csv")
#df_fcsts_round1 <- df_fcsts_all

# write.csv(df_fcsts_all, file="Fcsts_tslm.csv")
# df_fcsts_tslm <- df_fcsts_all



# output submission-formatted results
# -----------------------------------
#merge results by rule with results by forecasting function
#test_rslts_rnd1; test_rslts_submit2
test_rslts_submit3 <-merge(combos_df[combos_df$fcst_needed, ], df_fcsts_all, 
                        by=c('combo_id', 'item_id', 'shop_id'), all.x = TRUE) %>% select(combo_id, item_id, shop_id, fcst_by_rule_val, fcst_val)

# nrow(combos_df[combos_df$fcst_needed, ]) #214,200
# nrow(test_df)   #214,200
# str(test_rslts_submit3)  #214,200

#bring them together into a single column
test_rslts_submit3[is.na(test_rslts_submit3$fcst_val), ]$fcst_val <- test_rslts_submit3[is.na(test_rslts_submit3$fcst_val), ]$fcst_by_rule_val

test_rslts_submit3_v2 <- test_rslts_submit3[, c('combo_id', 'item_id', 'shop_id', 'fcst_val')]

subset(test_rslts_submit3_v2, item_cnt_month >20)

#checks
#nrow(test_rslts_submit3_v2)
# #check that the comboID always equals the ID field in test
#merge(test_df, test_rslts_submit3_v2, by=c('item_id', 'shop_id')) %>%
#     filter(combo_id != ID)
# 0 rows found, ==> yes, they match


#get the submission in the right format with right column names
colnames(test_rslts_submit3_v2) <- c('ID', 'item_id', 'shop_id', 'item_cnt_month')
test_rslts_submit3_v2$item_id <- NULL
test_rslts_submit3_v2$shop_id <- NULL
test_rslts_submit3_v2$item_cnt_month <- round(test_rslts_submit3_v2$item_cnt_month)

#retry with the range trimmed to 0-20.  Comment on Kaggle indicates the true target values have been clipped that way
test_rslts_submit3_v2b <- test_rslts_submit3_v2
test_rslts_submit3_v2b[test_rslts_submit3_v2b$item_cnt_month>20, ]$item_cnt_month <- 20

write.csv(test_rslts_submit3_v2b, file="HW1_submission4_AKW.csv", row.names = FALSE)





#compare the tslm forecasts to the ETS forecasts.  If one method has a lower RMSE, use the forecast from that method.
df_fcsts_all %>% 
  group_by(modeling_method) %>% summarize(n=n())
# 1 ets             28634
# 2 tslm            82039
#from tslm:   1 tslm            110673

dfmdlg_mthd2 <- subset(   df_fcsts_all, modeling_method == 'ets',  
                          select=c('combo_id', 'item_id', 'shop_id', 'modeling_method', 'fcst_val', 'acc_score1_train')) 

colnames(dfmdlg_mthd2) <- c('combo_id', 'item_id', 'shop_id', 'mdlg_mthd_mthd2', 'fcst_val_mthd2', 'rmse_mthd2')
#28634


dfmdlg_mthd1 <- subset(   df_fcsts_tslm,  combo_id %in% dfmdlg_mthd2$combo_id, 
                          select=c('combo_id', 'item_id', 'shop_id', 'modeling_method', 'fcst_val', 'acc_score1_train')
                       ) 

# %>% rename(acc_score1_train=rmse_method1, modeling_method=mdlg_mthd1)
# rename(dfmdlg_mthd1, "acc_score1_train"="rmse_method1")
# rlang::last_error()

colnames(dfmdlg_mthd1) <- c('combo_id', 'item_id', 'shop_id', 'mdlg_mthd_mthd1', 'fcst_val_mthd1', 'rmse_mthd1')
str(dfmdlg_mthd1)


dfmdlg_mthd1and2 <- merge(dfmdlg_mthd1, dfmdlg_mthd2)
str(dfmdlg_mthd1and2)

#nrow(subset(dfmdlg_mthd1and2, rmse_mthd1 < rmse_mthd2 & fcst_val_mthd1 != fcst_val_mthd2))
  #21,035 where the tslm has a lower rmse,   and of those, 12,971 where it changed the forecast val

#get the ones that have cnhanged
diffs_v2vsv1 <- subset(dfmdlg_mthd1and2, rmse_mthd1 < rmse_mthd2 & fcst_val_mthd1 != fcst_val_mthd2)

#merge the lowest RMSE fcsts and submit
# -------------------------------------
test_rslts_submit3_v2c <- test_rslts_submit3_v2

test_rslts_submit3_v2c <- merge(test_rslts_submit3_v2c, diffs_v2vsv1[,c('combo_id', 'fcst_val_mthd1')], by.x='ID', by.y='combo_id', all.x = TRUE)
#sum(!is.na(test_rslts_submit3_v2c$fcst_val_mthd1))  #12,971 as expected

#where we have an improved value, put it in the key forecast column
test_rslts_submit3_v2c[!is.na(test_rslts_submit3_v2c$fcst_val_mthd1), ]$item_cnt_month <- test_rslts_submit3_v2c[!is.na(test_rslts_submit3_v2c$fcst_val_mthd1), ]$fcst_val_mthd1
test_rslts_submit3_v2c$fcst_val_mthd1 <- NULL    #drop this column; no longer needed and will cause problems in submitting
write.csv(test_rslts_submit3_v2c, file="HW1_submission5_AKW.csv", row.names = FALSE)

test_rslts_submit3_v2c[test_rslts_submit3_v2c$item_cnt_month>20,]$item_cnt_month <- 20
write.csv(test_rslts_submit3_v2c, file="HW1_submission5b_AKW.csv", row.names = FALSE)

nrow(test_rslts_submit3_v2c[test_rslts_submit3_v2c$item_cnt_month>20,])


#try using a naive forecast of 0 for the new items
# -------------------------------------
test_rslts_submit3_v2d <- test_rslts_submit3_v2

summary(test_rslts_submit3_v2d[test_rslts_submit3_v2d$ID %in% df_fcsts2_NEW$combo_id, ]$item_cnt_month)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.2815  0.0000 13.0000 

subset(test_rslts_submit3_v2d, ID %in% df_fcsts2_NEW$combo_id) %>%
  group_by(item_cnt_month) %>% summarize(NbrRcds=n())




#try using a naive forecast of 0 for the new items
# -------------------------------------
test_rslts_submit3_v2e <- test_rslts_submit3_v2

summary(test_rslts_submit3_v2e[test_rslts_submit3_v2e$ID %in% df_fcsts2_EXI$combo_id, ]$item_cnt_month)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.2815  0.0000 13.0000 

subset(test_rslts_submit3_v2d, ID %in% df_fcsts2_EXI$combo_id) %>%
  group_by(item_cnt_month) %>% summarize(NbrRcds=n())
# 1              0   57131
# 2              1    4864
# 3              2    2264
# 4              3    1052
# 5              4     592
# 6              5     369
# 7              6     120
# 8              7     137
# 9              9      15
# 10             10      42
# 11             14      42
# 12             15       1
# 13             16      41
# 14             17      41
# 15             20      82



#where the item is new, use naive forecast of 0
test_rslts_submit3_v2e[test_rslts_submit3_v2e$ID %in% df_fcsts2_EXI$combo_id, ]$item_cnt_month <- 0

test_rslts_submit3_v2e[test_rslts_submit3_v2e$item_cnt_month>20,]$item_cnt_month <- 20
write.csv(test_rslts_submit3_v2e, file="HW1_submission7_AKW.csv", row.names = FALSE)
#not an improvement.  Forecast of 0 is overly simplistic.  How to tell items that will be big sellers?   name like other big sellers?







# sum(is.na(test_rslts_rnd1$fcst_val))
# sum(is.na(test_rslts_rnd1$fcst_by_rule_val) & is.na(test_rslts_rnd1$fcst_val))

# sum(df_fcsts_all$combo_id %in% df_fcsts2_EXI[1:20, ]$combo_id)
# df_fcsts_all[df_fcsts_all$combo_id %in% df_fcsts2_NEW[1:20, ]$combo_id, ]
# subset(monthly_sales, combo_id == 147869)
# tstfc <- ts( fn_data_prep__ByComboID(monthly_sales, 147869))  %>% naive(h=1)
# attributes(tstfc)


#subset(monthly_sales, item_id == 11373, select=c('mnth', 'Units_Sold', 'shop_id')) 
#if it hasn't sold before, assume it will not sell this month?  assume it's now available in that store and will sell like other stores?



df_fcsts2_NEW


summary(df_fcsts_all$fcst_val)
boxplot(df_fcsts_all$fcst_val)

boxplot(df_fcsts_all[df_fcsts_all$fcst_val<10, 'fcst_val'])
boxplot(df_fcsts_all[df_fcsts_all$fcst_val>=10, 'fcst_val'])

big_vals_fcstd <- df_fcsts_all[df_fcsts_all$fcst_val>=100, ]
big_vals_fcstd <- big_vals_fcstd[order(big_vals_fcstd$acc_score1_train, decreasing = TRUE), ] 

# hist(df_fcsts_all[df_fcsts_all$fcst_val>=30, 'fcst_val'], breaks=seq(from=30, to=710, by=20))
# hist(df_fcsts_all[df_fcsts_all$fcst_val<10, 'fcst_val'])
# hist(df_fcsts_all[df_fcsts_all$fcst_val>0 & df_fcsts_all$fcst_val<10, 'fcst_val'], , breaks=seq(from=0, to=11, by=1))
# hist(df_fcsts_all[df_fcsts_all$fcst_val>=10 & df_fcsts_all$fcst_val<30, 'fcst_val'], , breaks=seq(from=9, to=31, by=1))


boxplot(df_fcsts_all[df_fcsts_all$combo_id %in% df_fcsts2_EXI$combo_id, 'fcst_val'])
sum(df_fcsts_all$combo_id %in% df_fcsts1$combo_id)  #28,634


sum(df_fcsts_all$combo_id %in% df_fcsts2_EXI$combo_id)  #66,793
  #max of ~50;  almost all 0!

sum(df_fcsts_all$combo_id %in% df_fcsts2_NEW$combo_id)  #15,246
  #max of 12;  almost all 0!




plot_cmbo_and_fcst <- function(cmbo_id) 
{
  itmID <- df_fcsts_all[df_fcsts_all$combo_id==cmbo_id, 'item_id']
  shpID <- df_fcsts_all[df_fcsts_all$combo_id==cmbo_id, 'shop_id']
  itm_nm <- combos_df[combos_df$combo_id==cmbo_id, 'item_name']
  shp_nm <- combos_df[combos_df$combo_id==cmbo_id, 'shop_name']
  ctgryID <- df_fcsts_all[df_fcsts_all$combo_id==cmbo_id, 'category_id']
  dataprep_fn <- df_fcsts_all[df_fcsts_all$combo_id==cmbo_id, 'data_prep_fn']
  mdlg_method <- df_fcsts_all[df_fcsts_all$combo_id==cmbo_id, 'modeling_method']
  
  #extract the data needed
  df1 <- if (dataprep_fn=="byComboID") { fn_data_prep__ByComboID(monthly_sales, cmbo_id) } else
    if (dataprep_fn=="byItemID") { fn_data_prep__ByItemID(monthly_sales, itmID) } else
      if (dataprep_fn=="byShopID_and_CtgryID") { fn_data_prep__ByShopID_and_CtgryID(monthly_sales, shpID, ctgryID) } 
  
  #create the fcst
  if (nrow(df1)>1) 
  {
    start_yr <- year(min(df1$mnth))
    start_mo <- month(min(df1$mnth))
    end_yr   <- year(max(df1$mnth))
    end_mo   <- month(max(df1$mnth))
    time_srs <- ts(df1$Units_Sold, start=c(start_yr, start_mo), end=c(end_yr, end_mo), frequency = 12)
    fcst_df <- data.frame(mnth=c(as.Date("01-11-2015", "%d-%m-%Y")), Units_Sold= df_fcsts_all[df_fcsts_all$combo_id==cmbo_id, 'fcst_val'])

    plt <- ggplot(data=df1,aes(x=mnth, y=Units_Sold)) + 
              geom_point() + 
              geom_point(data=fcst_df, mapping=aes(x=mnth, y=Units_Sold), color='red') +
              xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
              ggtitle(paste(itm_nm, 'at', shp_nm, '.  ComboID', cmbo_id)) + theme(plot.title = element_text(hjust = 0.5)) + 
              xlab("Month") + ylab("Units Sold") + theme(legend.position = "bottom") +
              labs(subtitle=paste("Data Aggregation:", dataprep_fn)) 
      print(plt)    
    
      }   
}
#cases where item is found in both test and train, but forecast is needed at a shop which had no previous sales of that item
#unique(big_vals_fcstd[big_vals_fcstd$data_prep_fn=='byItemID', ]$item_id)
  #item IDs:  11373 11370 11369  7967 20949   492  7966 13099 13098
#get a combo for each item;  all teh results will be the same so looking at 1 will suffice
big_vals_fcsts_by_itmID <- big_vals_fcstd[big_vals_fcstd$data_prep_fn=='byItemID', ] %>%
                            group_by(item_id) %>% summarize(minComboID=min(combo_id))


big_vals_fcstd <- df_fcsts_all[df_fcsts_all$fcst_val>=30, ]
big_vals_fcstd <- big_vals_fcstd[order(big_vals_fcstd$acc_score1_train, decreasing = FALSE), ] 

#for (comboID in big_vals_fcstd[big_vals_fcstd$data_prep_fn=='byComboID', ]$combo_id)
for (comboID in big_vals_fcsts_by_itmID$minComboID)
for (comboID in df_fcsts_all[df_fcsts_all$fcst_val>=30, ]$combo_id)
for (comboID in big_vals_fcstd[ , ]$combo_id)
{
  plot_cmbo_and_fcst(comboID)
}



plot_itmXshop_and_fcst <- function(itemID) 
{
  dfplt <- subset(monthly_sales, item_id == itemID)

  itm_nm <- items_df[items_df$item_id==itemID, 'item_name']
  cmbo_id <- big_vals_fcsts_by_itmID[big_vals_fcsts_by_itmID$item_id==itemID, ]$minComboID
  fcst_df <- data.frame(mnth=c(as.Date("01-11-2015", "%d-%m-%Y")), Units_Sold= df_fcsts_all[df_fcsts_all$combo_id==cmbo_id, 'fcst_val'], shop_id=c(-1))
    
  plt <- ggplot(data=dfplt, aes(x=mnth, y=Units_Sold, group=shop_id)) + 
      geom_point(aes(color=shop_id)) + 
      geom_point(data=fcst_df, mapping=aes(x=mnth, y=Units_Sold), shape=23, size=3) +
      xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
      ggtitle(itm_nm) + theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Month") + ylab("Units Sold") + theme(legend.position = "bottom") 
    print(plt)    
    
}

plot_itmXshop_and_fcst(11373)
#item IDs:  11373 11370 11369  7967 20949   492  7966 13099 13098

for (itm_id in c(11373, 11370, 11369, 7967, 20949, 492, 7966, 13099, 13098) )
{
  plot_itmXshop_and_fcst(itm_id)
}



#fcst_ts <- ts( c(df_fcsts_all[df_fcsts_all$combo_id==cmbo_id, 'fcst_val']), start=c(2015,11), end=c(2015,11), frequency = 12) 
#print(fcst_ts)

# plt <- autoplot(time_srs, main=paste(itm_nm, 'at', shp_nm, '.  ComboID', cmbo_id)) +
#         xlim(as.Date("01-01-2013", "%d-%m-%Y"),as.Date("01-01-2016", "%d-%m-%Y")) + ylim(0, NA) +
#         autolayer(fcst_ts, series="Fcst", color="red" ) 
# print(plt)
#aes(color=shop_name)


  # fcst_outpt <- if (mdlg_method == "naive") {fn_fcst_naive(time_srs)} else
  #   if (mdlg_method == "mean") {fn_fcst_mean(time_srs)} else
  #     if (mdlg_method == 'tslm') {fn_fcst_lm(time_srs)}
  
plot(subset(monthly_sales, combo_id == 37296, select=c('mnth', 'Units_Sold')))
#score by AIC?
#one-step-out-of-sample

#create cross section datasets to support these?
#create time section datasets to support these (at level of i) category and ii) store and/or location)


#can we test using tsCV to determine the optimal method and then refit on the entire train+eval set??
#can we test using AIC on eval set to determine the optimal method and then refit on the entire train+eval set??






#look at forecasting the new items (item not found in training).
# 
length(unique(df_fcsts2_NEW$item_id))  #363 distinct items
length(unique(df_fcsts2_NEW$category_id))  #from 39 distinct categories

str(df_fcsts2_NEW)

ctgrys_w_new_items <- categories_df_w_sales[categories_df_w_sales$category_id %in% unique(df_fcsts2_NEW$category_id), ]
#str(monthly_sales_by_category)
#str(monthly_sales)
# head(monthly_sales2)
monthly_sales2





#pdf("MoSales_by_Cat_and_Itm.pdf")
pdf("Sales_by_Cat_and_Itm__SinceRollout.pdf")
for (cat_nm in unique(ctgrys_w_new_items$category_name)) 
i <- 0
for (cat_nm in c('Movie - DVD', 'Games PC - Standard edition', 'Movie - Blu-Ray', 'Games PC - Additional publications'))
{
  # cat_nm <- "Movie - DVD" 
  i <- i + 1  
  sales_by_this_ctgrys_itms <- monthly_sales[monthly_sales$category_name==cat_nm, ] %>%
                                  group_by(mnth, item_id) %>% summarize(Units_Sold=sum(Units_Sold))

  itm_rollout_mo <-  monthly_sales[monthly_sales$category_name==cat_nm, ] %>%
                        group_by(item_id) %>% summarize(rollout_mo=min(mnth))
  
  sales_by_this_ctgrys_itms <- merge(sales_by_this_ctgrys_itms, itm_rollout_mo, by='item_id')
  
  sales_by_this_ctgrys_itms$mos_since_rollout <- as.integer( round( (sales_by_this_ctgrys_itms$mnth - sales_by_this_ctgrys_itms$rollout_mo)/(365.25/12) )  )
    
  #plt <- ggplot(data=sales_by_this_ctgrys_itms, aes(x=mos_since_rollout, y=Units_Sold, group=item_id)) + 
  #geom_line(aes(color=item_id)) + xlim(0,35) +
  plt <- ggplot(data=sales_by_this_ctgrys_itms, aes(x=mnth, y=Units_Sold, group=item_id)) + 
    geom_line(aes(color=item_id)) + xlim(as.Date("01-01-2013", "%m-%d-%Y"), as.Date("01-01-2016", "%m-%d-%Y")) +
    ggtitle(paste("Category:  ",cat_nm)) + xlab("Month") + ylab("Units Sold") + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "right")
  
  print(plt)
  switch(i, plt1<-plt, plt2<-plt, plt3<-plt, plt4<-plt)
}
grid.arrange(plt1, plt3)
dev.off()
# setTimeLimit(); setSessionTimeLimit()
# getTimeLimit();

pdf("Mo1Sales_by_Cat_and_Itm.pdf")
for (cat_nm in unique(ctgrys_w_new_items$category_name)) 
{
  # cat_nm <- "Movie - DVD"  #"Program - Teaching" 
  
  sales_by_shop_of_this_ctgrys_itms <- monthly_sales[monthly_sales$category_name==cat_nm, ] %>%
    group_by(mnth, item_id, shop_id) %>% summarize(Units_Sold=sum(Units_Sold))
  
  itm_rollout_mo <-  monthly_sales[monthly_sales$category_name==cat_nm, ] %>%
    group_by(item_id) %>% summarize(rollout_mo=min(mnth))
  
  sales_by_shop_of_this_ctgrys_itms <- merge(sales_by_shop_of_this_ctgrys_itms, itm_rollout_mo, by='item_id')
  
  sales_by_shop_of_this_ctgrys_itms$mos_since_rollout <- as.integer( round( (sales_by_shop_of_this_ctgrys_itms$mnth - sales_by_shop_of_this_ctgrys_itms$rollout_mo)/(365.25/12) )  )
  
  plt <- ggplot(data=sales_by_shop_of_this_ctgrys_itms[sales_by_shop_of_this_ctgrys_itms$mos_since_rollout == 0, ], aes(y=Units_Sold, group=item_id)) + 
    geom_boxplot() + coord_flip() + 
    ggtitle(paste("Category:  ",cat_nm)) + xlab("Item_ID") + ylab("Units Sold") + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "right")
  
  print(plt)

  subset(sales_by_shop_of_this_ctgrys_itms, mos_since_rollout == 0) %>% group_by(item_id) %>% 
      summarize(MinSold=min(Units_Sold), AvgSold=mean(Units_Sold), MaxSold=max(Units_Sold), StDevSales=sd(Units_Sold)) %>%
        arrange(desc(AvgSold))
}
dev.off()


hot_new_itm_ctgrys <- ctgrys_w_new_items[ ctgrys_w_new_items$super_category %in% c('Games ', 'Movie ', 'Games PC ') | ctgrys_w_new_items$category_name == 'Music - CD of local production', ]

test_df_dtld <- merge(test_df, items_df) %>% merge(categories_df) %>% merge(shops_df)
test_items_df_dtld <- data.frame(item_id=unique(subset(test_df, select=item_id))) %>% merge(items_df) %>% merge(categories_df)

test_items_hot_ctgrys <- test_items_df_dtld[test_items_df_dtld$category_id %in% hot_new_itm_ctgrys$category_id, ]
test_items_hot_ctgrys$item_name_original <- NULL
nrow(test_items_hot_ctgrys)  #2777

new_test_items_hot_ctgrys <- test_items_hot_ctgrys[test_items_hot_ctgrys$item_id %in% df_fcsts2_NEW$item_id, ]
nrow(new_test_items_hot_ctgrys)  #183

#created this hot_new_itms  by stepping through teh loop with the cat_nm set to movie-dvd and using the commented-out assignment line
for (cat_nm in hot_new_itm_ctgrys[2:11, ]$category_name) 
  {
    # cat_nm <- "Movie - DVD"   #already done to get the first one going
    
    sales_by_this_ctgrys_itms <- monthly_sales[monthly_sales$category_name==cat_nm, ] %>%
      group_by(mnth, item_id) %>% summarize(Units_Sold=sum(Units_Sold))
    
    itm_rollout_mo <-  monthly_sales[monthly_sales$category_name==cat_nm, ] %>%
      group_by(item_id) %>% summarize(rollout_mo=min(mnth), ttl_sales=sum(Units_Sold))
    
    sales_by_this_ctgrys_itms <- merge(sales_by_this_ctgrys_itms, itm_rollout_mo, by='item_id')
    
    #get the big items, in their first mo    
    sales_by_big_itms_mo1 <- subset(sales_by_this_ctgrys_itms, rollout_mo==mnth & ttl_sales>500 & Units_Sold/ttl_sales >0.35) %>% arrange(desc(ttl_sales))

    # hot_new_itms <- items_df_w_sales[items_df_w_sales$item_id %in% sales_by_big_itms_mo1$item_id, ]
    # str(hot_new_itms)   #278 obs
    hot_new_itms <- rbind(hot_new_itms, items_df_w_sales[items_df_w_sales$item_id %in% sales_by_big_itms_mo1$item_id, ])

  }  
  
hot_new_itms$item_name_original <- NULL
colnames(hot_new_itms) <- c('item_id_old', 'category_id_old', 'item_name_old', 'Units_Sold_Ttl')
#cross join with test_items_hot_ctgrys
nm_cmprsn <- merge(hot_new_itms, new_test_items_hot_ctgrys)

str(nm_cmprsn)
nrow(test_items_hot_ctgrys)  #183
nrow(hot_new_itms)  #278    278*183= 50,874
nrow(nm_cmprsn)  #50,874

nm_cmprsn$sim_index <- apply(nm_cmprsn, 1, function(x) RecordLinkage::levenshteinSim(x[['item_name_old']], x[['item_name']]))

best_comp <- nm_cmprsn %>% group_by(item_id_old) %>% summarize(max_sim=max(sim_index)) %>% 
                merge(nm_cmprsn, by.x=c('item_id_old', 'max_sim'), by.y=c('item_id_old', 'sim_index'))


str(best_comp)  #319 rows

best_comp <- best_comp %>% arrange(desc(max_sim), item_name_old, item_name)
best_comp[, c('item_name_old', 'item_name', 'max_sim')]

require(RecordLinkage)
install.packages("RecordLinkage")



#https://www.kaggle.com/c/competitive-data-science-predict-future-sales/

