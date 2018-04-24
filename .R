#------------------------------------------与初始数据的区别---------------------
#1.没有将缺失值标记为-1,而是将他们也化作为一类
#2.增加了对时间的特征,将时间划分成午夜0-6，早上7-12，下午13-18，晚上19-24，在feature段提取购买率，频数；在label段提取是否为午夜
#3.删掉部分特征
#4.将which.max(table())全部转换成median

#划分feature集和label集
almm_train<-read.table("F:/SUE/天池/round1_ijcai_18_train_20180301.txt",header=T,stringsAsFactors = T,
                       colClasses=c(rep("character",6),rep("numeric",4),"character","numeric",
                                    rep("numeric",3),"character",rep("numeric",2),rep("character",2),
                                    rep("numeric",7)))
almm_test_part2<-read.table("F:/SUE/天池/almm_test_0418.txt",header=T,stringsAsFactors = T,
                            colClasses=c(rep("character",6),rep("numeric",4),"character","numeric",
                                         rep("numeric",3),"character",rep("numeric",2),rep("character",2),
                                         rep("numeric",6)))
almm_test_part1<-read.table("F:/SUE/天池/almm_test.txt",header=T,stringsAsFactors = T,
                            colClasses=c(rep("character",6),rep("numeric",4),"character","numeric",
                                         rep("numeric",3),"character",rep("numeric",2),rep("character",2),
                                         rep("numeric",6)))
almm_test<-data.frame(rbind(almm_test_part1,almm_test_part2))   #将test_p1;test_p2合并

almm_train$almm_date<-format(as.POSIXct(almm_train$context_timestamp,origin="1970-01-01 00:00:00"),format="%m/%d")
almm_train$time_hour<-format(as.POSIXct(almm_train$context_timestamp,origin="1970-01-01 00:00:00"),format="%H") #补充时间变量
almm_train$time_seconds<-as.POSIXct(almm_train$context_timestamp,origin="1970-01-01 00:00:00")
almm_train$time_hour<-as.numeric(almm_train$time_hour)
almm_train<-almm_train[,! colnames(almm_train) %in% c("context_timestamp","predict_category_property")]
almm_train$time_class<-cut(almm_train$time_hour,c(0,7,13,19,24),right=F)
almm_train$is_midnight<-if_else(almm_train$time_class==unique(almm_train$time_class)[1],1,0)
almm_train$is_morning<-if_else(almm_train$time_class==unique(almm_train$time_class)[2],1,0)
almm_train$is_afternoon<-if_else(almm_train$time_class==unique(almm_train$time_class)[3],1,0)
almm_train$is_night<-if_else(almm_train$time_class==unique(almm_train$time_class)[4],1,0)
almm_train$is_male<-if_else(almm_train$user_gender_id==0,1,0)
almm_train$is_female<-if_else(almm_train$user_gender_id==1,1,0)


table(almm_train$time_class)

almm_test$almm_date<-format(as.POSIXct(almm_test$context_timestamp,origin="1970-01-01 00:00:00"),format="%m/%d")
almm_test$time_hour<-format(as.POSIXct(almm_test$context_timestamp,origin="1970-01-01 00:00:00"),format="%H") #补充时间变量
almm_test$time_seconds<-as.POSIXct(almm_test$context_timestamp,origin="1970-01-01 00:00:00")
almm_test$time_hour<-as.numeric(almm_test$time_hour)
almm_test<-almm_test[,! colnames(almm_test) %in% c("context_timestamp","predict_category_property")]
almm_test$time_class<-cut(almm_test$time_hour,c(0,7,13,19,24),right=F)
almm_test$is_midnight<-if_else(almm_test$time_class==unique(almm_test$time_class[1]),1,0)
almm_test$is_morning<-if_else(almm_test$time_class==unique(almm_test$time_class[2]),1,0)
almm_test$is_afternoon<-if_else(almm_test$time_class==unique(almm_test$time_class[3]),1,0)
almm_test$is_night<-if_else(almm_test$time_class==unique(almm_test$time_class[4]),1,0)
almm_test$is_male<-if_else(almm_test$user_gender_id==0,1,0)
almm_test$is_female<-if_else(almm_test$user_gender_id==1,1,0)

 c<-NULL
 for(i in 1:ncol(almm_train)){
   almm_train[which(almm_train[,i]==-1),i]<-NA
   c[i]<-length(which(is.na(almm_train[,i])))}
 
 for(i in 1:ncol(almm_test)){
   almm_test[which(almm_test[,i]==-1),i]<-NA
   c[i]<-length(which(is.na(almm_test[,i])))}

#划分标准
date_train1<-c("09/19","09/20","09/18","09/21")
train_p1<-almm_train[which(almm_train$almm_date %in% date_train1),]
test_p1<-almm_train[which(almm_train$almm_date=="09/22"),]
date_train2<-c("09/19","09/20","09/21","09/22")
train_p2<-almm_train[which(almm_train$almm_date %in% date_train2),]
test_p2<-almm_train[which(almm_train$almm_date=="09/23"),]
date_train3<-c("09/22","09/20","09/21","09/23")
train_p3<-almm_train[which(almm_train$almm_date %in% date_train3),]
test_p3<-almm_train[which(almm_train$almm_date=="09/24"),]
date_train4<-c("09/22","09/23","09/21","09/24")
train_p4<-almm_train[which(almm_train$almm_date %in% date_train4),]
test_p4<-almm_test
#----------------------------------------------------------------------特征提取-------------------------------------------
library(dplyr)
#-------------------------------------------------train_p1 提取feature
#1.0 用户id feature
uf1<-train_p1 %>%
  group_by(user_id) %>%
  summarize(user_category_num=length(unique(item_category_list)),
            user_open_times=length(user_id),
            user_buy_times=length(which(is_trade==1)),
            user_buy_prop=length(which(is_trade==1))/length(is_trade),
            user_price_level=median(item_price_level),
            user_brand_num=length(unique(item_brand_id)),   #光顾过多少品牌
            user_shop_num=length(unique(shop_id)),      #光顾过多少店铺
            user_time_hour=median(time_hour),          #用户经常在哪个时间逛淘宝
            user_review_num_level=median(shop_review_num_level)  #用户逛的店铺的评价数的中位数
            #user_shop_star_level=as.numeric(which.max(table(shop_star_level))) #用户逛的店铺的星级等级
  )  %>%
  data.frame() %>%
  arrange(desc(user_open_times))

uf2<-train_p1 %>%
  group_by(user_id,shop_id) %>%
  summarize(user_shop_open_times=length(is_trade),
            user_shop_buy_times=length(which(is_trade==1)),
            user_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()
uf3<-train_p1 %>%
  group_by(user_id,item_id) %>%
  summarize(user_item_open_times=length(is_trade),
            user_item_buy_times=length(which(is_trade==1)),
            user_item_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

uf4<-train_p1 %>%
  group_by(user_id,item_category_list) %>%
  summarize(user_category_open_times=length(is_trade),
            user_category_buy_times=length(which(is_trade==1)),
            user_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

uf5<-train_p1 %>%
  group_by(user_id,item_brand_id) %>%
  summarize(user_brand_open_times=length(is_trade),
            user_brand_buy_times=length(which(is_trade==1)),
            user_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

#------------------------------1将user_feature 填充到test_p1中去
user_test_p1<-test_p1[,colnames(test_p1) %in% 
                        c("instance_id","user_id","shop_id","item_id","item_category_list","item_brand_id")]
user_feature_p1<-user_test_p1%>%
  left_join(uf1,by=c("user_id"="user_id"))%>%
  left_join(uf2,by=c("user_id"="user_id","shop_id"="shop_id")) %>%
  left_join(uf3,by=c("user_id"="user_id","item_id"="item_id")) %>%
  left_join(uf4,by=c("user_id"="user_id","item_category_list"="item_category_list")) %>%
  left_join(uf5,by=c("user_id"="user_id","item_brand_id"="item_brand_id")) %>%
  mutate(user_shop_times_to_total_times_prop=user_shop_open_times/user_open_times,
         user_shop_buy_to_total_buy_prop=user_shop_buy_times/user_buy_times,
         user_item_times_to_total_times_prop=user_item_open_times/user_open_times,
         user_item_buy_to_total_buy_prop=user_item_buy_times/user_buy_times,
         user_category_times_to_total_times_prop=user_category_open_times/user_open_times,
         user_category_buy_to_total_buy_prop=user_category_buy_times/user_buy_times,
         user_brand_times_to_total_times_prop=user_brand_open_times/user_open_times,
         user_brand_buy_to_total_buy_prop=user_brand_buy_times/user_buy_times
  )%>%
  data.frame()
#1.1将user的其他分类特征与item的交互特征提取处理
ucf1<-train_p1 %>%
  group_by(user_age_level,shop_id)%>%
  summarize(age_shop_open_times=length(is_trade),
            age_shop_buy_times=length(which(is_trade==1)),
            age_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf2<-train_p1%>%
  group_by(user_gender_id,shop_id)%>%
  summarize(gender_shop_open_times=length(is_trade),
            gender_shop_buy_times=length(which(is_trade==1)),
            gender_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf3<-train_p1 %>%
  group_by(user_occupation_id,shop_id)%>%
  summarize(occupation_shop_open_times=length(is_trade),
            occupation_shop_buy_times=length(which(is_trade==1)),
            occupation_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf4<-train_p1 %>%
  group_by(user_age_level,item_category_list)%>%
  summarize(age_category_open_times=length(is_trade),
            age_category_buy_times=length(which(is_trade==1)),
            age_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf5<-train_p1 %>%
group_by(user_gender_id,item_category_list)%>%
  summarize(gender_category_open_times=length(is_trade),
            gender_category_buy_times=length(which(is_trade==1)),
            gender_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf6<-train_p1%>%
group_by(user_occupation_id,item_category_list)%>%
  summarize(occupation_category_open_times=length(is_trade),
            occupation_category_buy_times=length(which(is_trade==1)),
            occupation_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf7<-train_p1 %>%
  group_by(user_age_level,item_brand_id)%>%
  summarize(age_brand_open_times=length(is_trade),
            age_brand_buy_times=length(which(is_trade==1)),
            age_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf8<-train_p1 %>%
  group_by(user_gender_id,item_brand_id)%>%
  summarize(gender_brand_open_times=length(is_trade),
            gender_brand_buy_times=length(which(is_trade==1)),
            gender_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf9<-train_p1%>%
  group_by(user_occupation_id,item_brand_id)%>%
  summarize(occupation_brand_open_times=length(is_trade),
            occupation_brand_buy_times=length(which(is_trade==1)),
            occupation_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf10<-train_p1 %>%
  group_by(user_age_level,item_price_level)%>%
  summarize(age_price_open_times=length(is_trade),
            age_price_buy_times=length(which(is_trade==1)),
            age_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf11<-train_p1 %>%
  group_by(user_gender_id,item_price_level)%>%
  summarize(gender_price_open_times=length(is_trade),
            gender_price_buy_times=length(which(is_trade==1)),
            gender_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf12<-train_p1%>%
  group_by(user_occupation_id,item_price_level)%>%
  summarize(occupation_price_open_times=length(is_trade),
            occupation_price_buy_times=length(which(is_trade==1)),
            occupation_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf13<-train_p1 %>%
  group_by(user_age_level,item_sales_level)%>%
  summarize(age_sales_open_times=length(is_trade),
            age_sales_buy_times=length(which(is_trade==1)),
            age_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf14<-train_p1 %>%
  group_by(user_gender_id,item_sales_level)%>%
  summarize(gender_sales_open_times=length(is_trade),
            gender_sales_buy_times=length(which(is_trade==1)),
            gender_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf15<-train_p1%>%
  group_by(user_occupation_id,item_sales_level)%>%
  summarize(occupation_sales_open_times=length(is_trade),
            occupation_sales_buy_times=length(which(is_trade==1)),
            occupation_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf16<-train_p1 %>%
  group_by(user_age_level,item_id)%>%
  summarize(age_item_open_times=length(is_trade),
            age_item_buy_times=length(which(is_trade==1)),
            age_item_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf17<-train_p1 %>%
  group_by(user_gender_id,item_id)%>%
  summarize(gender_item__open_times=length(is_trade),
            gender_item__buy_times=length(which(is_trade==1)),
            gender_item__buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf18<-train_p1%>%
  group_by(user_occupation_id,item_id)%>%
  summarize(occupation_item_id_open_times=length(is_trade),
            occupation_item_id_buy_times=length(which(is_trade==1)),
            occupation_item_id_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

#-----------------------user_class 的特征填充到test_p1中去
user_class_test_p1<-test_p1[,colnames(test_p1) %in% 
                              c("instance_id","shop_id","item_id","item_category_list","item_brand_id",
                                "item_sales_level","item_price_level","user_age_level","user_occupation_id",
                                "user_gender_id")]
user_class_feature_p1<-user_class_test_p1%>%
  left_join(ucf1,by=c("user_age_level"="user_age_level","shop_id"="shop_id"))%>%
  left_join(ucf2,by=c("user_gender_id"="user_gender_id","shop_id"="shop_id"))%>%
  left_join(ucf3,by=c("user_occupation_id"="user_occupation_id","shop_id"="shop_id"))%>%
  left_join(ucf4,by=c("user_age_level"="user_age_level","item_category_list"="item_category_list"))%>%
  left_join(ucf5,by=c("user_gender_id"="user_gender_id","item_category_list"="item_category_list"))%>%
  left_join(ucf6,by=c("user_occupation_id"="user_occupation_id","item_category_list"="item_category_list"))%>%
  left_join(ucf7,by=c("user_age_level"="user_age_level","item_brand_id"="item_brand_id"))%>%
  left_join(ucf8,by=c("user_gender_id"="user_gender_id","item_brand_id"="item_brand_id"))%>%
  left_join(ucf9,by=c("user_occupation_id"="user_occupation_id","item_brand_id"="item_brand_id"))%>%
  left_join(ucf10,by=c("user_age_level"="user_age_level","item_price_level"="item_price_level"))%>%
  left_join(ucf11,by=c("user_gender_id"="user_gender_id","item_price_level"="item_price_level"))%>%
  left_join(ucf12,by=c("user_occupation_id"="user_occupation_id","item_price_level"="item_price_level"))%>%
  left_join(ucf13,by=c("user_age_level"="user_age_level","item_sales_level"="item_sales_level"))%>%
  left_join(ucf14,by=c("user_gender_id"="user_gender_id","item_sales_level"="item_sales_level"))%>%
  left_join(ucf15,by=c("user_occupation_id"="user_occupation_id","item_sales_level"="item_sales_level"))%>%
  left_join(ucf16,by=c("user_age_level"="user_age_level","item_id"="item_id"))%>%
  left_join(ucf17,by=c("user_gender_id"="user_gender_id","item_id"="item_id"))%>%
  left_join(ucf18,by=c("user_occupation_id"="user_occupation_id","item_id"="item_id"))%>%
  data.frame()

#2 item_id feature
if1<-train_p1 %>%
  group_by(item_id) %>%
  summarize(item_buy_prop=length(which(is_trade==1))/length(is_trade),
            item_open_times=length(is_trade),
            item_buy_times=length(which(is_trade==1)),
            item_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            item_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            item_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            item_age_level=median(user_age_level), 
            item_star_level=median(user_star_level), 
            item_median_hour=median(time_hour)
  ) %>%
  data.frame()  

#2.1  item_brand_feature
if2<-train_p1 %>%
  group_by(item_brand_id) %>%
  summarize(brand_buy_prop=length(which(is_trade==1))/length(is_trade),
            brand_open_times=length(is_trade),
            brand_buy_times=length(which(is_trade==1)),
            brand_item_num=length(unique(item_id)),
            brand_shop_num=length(unique(shop_id)),
            brand_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            brand_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            brand_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            brand_age_level=median(user_age_level), 
            brand_star_level=median(user_star_level), 
            brand_median_hour=median(time_hour)
  ) %>%
  data.frame()  


#2.2 item_shop_feature 
if3<-train_p1 %>%
  group_by(shop_id) %>%
  summarize(shop_buy_prop=length(which(is_trade==1))/length(is_trade),
            shop_open_times=length(is_trade),
            shop_buy_times=length(which(is_trade==1)),
            shop_item_num=length(unique(item_id)),
            shop_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            shop_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            shop_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            shop_age_level=median(user_age_level), 
            shop_user_star_level=median(user_star_level), 
            shop_median_hour=median(time_hour)
  ) %>%
  data.frame()  

#2.3 item_category_feature
if4<-train_p1 %>%  
  group_by(item_category_list) %>%
  summarize(category_buy_prop=length(which(is_trade==1))/length(is_trade),
            category_open_times=length(is_trade),
            category_buy_times=length(which(is_trade==1)),
            category_item_num=length(unique(item_id)),
            category_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            category_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            category_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            category_age_level=median(user_age_level), 
            category_user_star_level=median(user_star_level), 
            category_median_hour=median(time_hour)
            ) %>%
  data.frame()

#-------------------------------2 将item的特征填充到test_p1中
item_test_p1<-test_p1[,colnames(test_p1) %in% 
                        c("instance_id","item_id","shop_id","item_category_list","item_brand_id")]
item_feature_p1<-item_test_p1 %>%
  left_join(if1,by=c("item_id"="item_id")) %>%
  left_join(if2,by=c("item_brand_id"="item_brand_id")) %>%
  left_join(if3,by=c("shop_id"="shop_id")) %>%
  left_join(if4,by=c("item_category_list"="item_category_list"))%>%
  mutate(item_shop_times_to_total_times_prop=item_open_times/shop_open_times,
         item_shop_buy_to_total_buy_prop=item_buy_times/shop_buy_times,
         item_brand_times_to_total_times_prop=item_open_times/brand_open_times,
         item_brand_buy_to_total_buy_prop=item_buy_times/brand_buy_times,
         item_category_times_to_total_times_prop=item_open_times/category_open_times,
         item_category_buy_to_total_buy_prop=item_buy_times/category_buy_times
         )%>%
  data.frame()


#------------------------其他的分类变量的特征
#3.1 城市特征
item_city<-train_p1 %>%
  group_by(item_city_id) %>%
  summarize(city_prop=length(which(is_trade==1))/length(is_trade),
            city_open_times=length(is_trade),
            city_buy_times=length(which(is_trade==1)),
            city_price_level=median(item_price_level),
            city_sales_level=median(item_sales_level),
            city_pv_level=median(item_pv_level)
  )%>%
  data.frame()

#3.2 价格等级特征
item_price<-train_p1 %>%
  group_by(item_price_level) %>%
  summarize(item_price_prop=length(which(is_trade==1))/length(is_trade),
            item_price_num=length(is_trade),
            item_price_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(item_price_prop))%>%
  data.frame()

#3.3 销量等级特征
item_sales<-train_p1 %>%
  group_by(item_sales_level) %>%
  summarize(item_sales_prop=length(which(is_trade==1))/length(is_trade),
            item_sales_num=length(is_trade),
            item_sales_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(item_sales_prop))%>%
  data.frame() 

#3.4 展示量等级特征
item_pv<-train_p1 %>%
  group_by(item_pv_level) %>%
  summarize(item_pv_prop=length(which(is_trade==1))/length(is_trade),
            item_pv_num=length(is_trade),
            item_pv_buy_times=length(is_trade)) %>%
  arrange(desc(item_pv_prop))%>%
  data.frame() 

#3.5 用户职业特征
user_occ<-train_p1 %>%
  group_by(user_occupation_id) %>%
  summarize(user_occ_prop=length(which(is_trade==1))/length(is_trade),
            user_occ_num=length(is_trade),
            user_occ_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(user_occ_prop))%>%
  data.frame() 

#3.6 用户年龄的购买率
user_age<-train_p1 %>%
  group_by(user_age_level) %>%
  summarize(user_age_prop=length(which(is_trade==1))/length(is_trade),
            user_age_tiems=length(is_trade),
            user_age_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(user_age_prop))%>%
  data.frame()

user_gender<-train_p1 %>%
  group_by(user_gender_id) %>%
  summarize(user_gender_prop=length(which(is_trade==1))/length(is_trade),
            user_gender_tiems=length(is_trade),
            user_gender_buy_times=length(which(is_trade==1))) %>%
  data.frame()

#3.7 用户等级购买率
user_star<-train_p1 %>%
  group_by(user_star_level) %>%
  summarize(user_star_prop=length(which(is_trade==1))/length(is_trade),user_star_num=length(is_trade)) %>%
  arrange(desc(user_star_prop))%>%
  data.frame()

#3.8 小时的特征
time_hour<-train_p1 %>%
  group_by(time_hour) %>%
  summarize(time_hour_buy_prop=length(which(is_trade==1))/length(is_trade),
            time_hour_open_times=length(is_trade),
            time_hour_buy_times=length(which(is_trade==1))) %>%
  data.frame()
#3.9 时间分组的特征
time_class<-train_p1 %>%
  group_by(time_class) %>%
  summarize(time_class_buy_prop=length(which(is_trade==1))/length(is_trade),
            time_class_open_times=length(is_trade),
            time_class_buy_times=length(which(is_trade==1))) %>%
  data.frame()

#3.10 评论数与购买率
shop_review<-train_p1 %>%
  group_by(shop_review_num_level) %>%
  summarize(shop_review_prop=length(which(is_trade==1))/length(is_trade),
            shop_review_times=length(is_trade),
            shop_review_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(shop_review_prop))%>%
  data.frame()


#3.11 商铺等级与购买率
shop_star<-train_p1 %>%
  group_by(shop_star_level) %>%
  summarize(shop_star_prop=length(which(is_trade==1))/length(is_trade),
            shop_star_times=length(is_trade),
            shop_star_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(shop_star_prop)) %>%
  data.frame() 
# len_intersect<-function(x){
#   return(c(length(intersect(train_p3[,colnames(train_p3)%in%x],test_p3[,colnames(test_p3)%in%x])),           #检查训练集和特征集的id的交集情况
#            length(unique(test_p3[,colnames(test_p3)%in%x]))))
# }

#----------------3 将other_class_feature 填充到test_p1中
other_class_test_p1<-test_p1[,colnames(test_p1) %in% c("instance_id","item_city_id","item_pv_level","user_star_level","time_hour","time_class",
                             "item_sales_level","item_price_level","user_age_level","user_occupation_id",
                            "user_gender_id","shop_review_num_level","shop_star_level")]
                              
other_class_feature_p1<-other_class_test_p1 %>%
  left_join(item_city,by=c('item_city_id'='item_city_id')) %>%  #城市的购买率和频数
  left_join(item_price,by=c('item_price_level'='item_price_level')) %>% #价格的购买率和频数
  left_join(item_sales,by=c('item_sales_level'='item_sales_level'))  %>%#销量的购买率和频数
  left_join(item_pv,by=c('item_pv_level'='item_pv_level')) %>% #展示量的购买率和频数
  #left_join(instance_property,by=c("instance_id","instance_id"))    %>%      #商品的属性的加权购买率
  left_join(user_occ,by=c('user_occupation_id'='user_occupation_id')) %>% #用户职业的购买率和频数
  left_join(user_age,by=c('user_age_level'='user_age_level'))  %>%#用户年龄的购买率和频数
  left_join(user_gender,by=c('user_gender_id'='user_gender_id'))  %>%#用户性别的购买率和频数
  left_join(user_star,by=c('user_star_level'='user_star_level'))  %>%#用户星级的购买率和频数
  left_join(time_hour,by=c('time_hour'='time_hour')) %>% #小时的购买率和频数
  left_join(time_class,by=c('time_class'='time_class')) %>% #时间段的购买率和频数
  left_join(shop_review,by=c('shop_review_num_level'='shop_review_num_level'))  %>%
  left_join(shop_star,by=c('shop_star_level'='shop_star_level'))  %>%
  data.frame()

#---------------------------------------------------------------------test1 提取label --------------------------
l1<-test_p1 %>%
  group_by(user_id) %>%
  summarize(user_open_last=max(time_seconds),
            user_open_first=min(time_seconds),
            user_open_times_today=length(user_id),
            user_shop_num=length(unique(shop_id)),
            user_item_num=length(unique(item_id))
  )%>%
  data.frame()
l2<-test_p1 %>%
  group_by(user_id,item_category_list)%>%
  summarize(user_category_open_last=max(time_seconds),
            user_category_open_first=min(time_seconds),
            user_category_times=length(user_id)
  )%>%
  data.frame()
l3<-test_p1%>%
  group_by(user_id,shop_id)%>%
  summarize(user_shop_open_last=max(time_seconds),
            user_shop_open_first=min(time_seconds),
            user_shop_times=length(user_id)
  )%>%
  data.frame()
l4<-test_p1%>%
  group_by(user_id,item_id)%>%
  summarize(user_item_open_last=max(time_seconds),
            user_item_open_first=min(time_seconds),
            user_item_times=length(user_id)
  )%>%
  data.frame()
l5<-test_p1%>%
  group_by(user_id,item_brand_id)%>%
  summarize(user_brand_open_last=max(time_seconds),
            user_brand_open_first=min(time_seconds),
            user_brand_times=length(user_id)
  )%>%
  data.frame()

is_firstlast<-function(x){
  if(x==0)
    return(1)
  else
    return(0)
}

test_feature_label_1<-test_p1%>%
  left_join(l1,by=c("user_id"="user_id"))%>%
  left_join(l2,by=c("user_id"="user_id","item_category_list"="item_category_list"))%>%
  left_join(l3,by=c("user_id"="user_id","shop_id"="shop_id"))%>%
  left_join(l4,by=c("user_id"="user_id","item_id"="item_id"))%>%
  left_join(l5,by=c("user_id"="user_id","item_brand_id"="item_brand_id")) %>%
  mutate(
    user_to_category_times_prop=user_open_times_today/user_category_times,
    user_to_shop_times_prop=user_open_times_today/user_shop_times,
    user_to_item_times_prop=user_open_times_today/user_item_times,
    user_to_brand_times_prop=user_open_times_today/user_brand_times,
    user_open_last_time_diff=difftime(user_open_last,time_seconds,units="secs"),
    user_open_first_time_diff=difftime(time_seconds,user_open_first,units="secs"),
    user_cat_open_last_time_diff=difftime(user_category_open_last,time_seconds,units="secs"),
    user_cat_open_first_time_diff=difftime(time_seconds,user_category_open_first,units="secs"),
    user_shop_open_last_time_diff=difftime(user_shop_open_last,time_seconds,units="secs"),
    user_shop_open_first_time_diff=difftime(time_seconds,user_shop_open_first,units="secs"),
    user_item_open_last_time_diff=difftime(user_item_open_last,time_seconds,units="secs"),
    user_item_open_first_time_diff=difftime(time_seconds,user_item_open_first,units="secs"),
    user_brand_open_last_time_diff=difftime(user_brand_open_last,time_seconds,units="secs"),
    user_brand_open_first_time_diff=difftime(time_seconds,user_brand_open_first,units="secs"),
    is_user_open_last_time=apply(matrix(user_open_last_time_diff),1,FUN=is_firstlast),
    is_user_open_first_time=apply(matrix(user_open_first_time_diff),1,FUN=is_firstlast),
    is_user_cat_open_last_time=apply(matrix(user_cat_open_last_time_diff),1,FUN=is_firstlast),
    is_user_cat_open_first_time=apply(matrix(user_cat_open_first_time_diff),1,FUN=is_firstlast),
    is_user_shop_open_last_time=apply(matrix(user_shop_open_last_time_diff),1,FUN=is_firstlast),
    is_user_shop_open_first_time=apply(matrix(user_shop_open_first_time_diff),1,FUN=is_firstlast),
    is_user_item_open_last_time=apply(matrix(user_item_open_last_time_diff),1,FUN=is_firstlast),
    is_user_item_open_first_time=apply(matrix(user_item_open_first_time_diff),1,FUN=is_firstlast),
    is_user_brand_open_last_time=apply(matrix(user_brand_open_last_time_diff),1,FUN=is_firstlast),
    is_user_brand_open_first_time=apply(matrix(user_brand_open_first_time_diff),1,FUN=is_firstlast)
  )%>%
  select(-c(user_open_last,user_open_first,user_category_open_last,user_category_open_first,user_shop_open_last,user_shop_open_first,
            user_item_open_last,user_item_open_first,user_brand_open_last,user_brand_open_first,user_open_last_time_diff,
            user_open_first_time_diff,user_brand_open_last_time_diff,user_brand_open_first_time_diff,
            user_cat_open_last_time_diff,user_cat_open_first_time_diff,user_shop_open_last_time_diff,user_shop_open_first_time_diff,
            user_item_open_last_time_diff,user_item_open_first_time_diff))  %>%
  cbind(user_feature_p1[,-c(1:6)],user_class_feature_p1[,-c(1:10)],item_feature_p1[,-c(1:5)],other_class_feature_p1[,-c(1:13)]) 


#-----------------------------------------------------------train_p2 提取feature-----------------------
#1.0 用户id feature
uf1<-train_p2 %>%
  group_by(user_id) %>%
  summarize(user_category_num=length(unique(item_category_list)),
            user_open_times=length(user_id),
            user_buy_times=length(which(is_trade==1)),
            user_buy_prop=length(which(is_trade==1))/length(is_trade),
            user_price_level=median(item_price_level),
            user_brand_num=length(unique(item_brand_id)),   #光顾过多少品牌
            user_shop_num=length(unique(shop_id)),      #光顾过多少店铺
            user_time_hour=median(time_hour),          #用户经常在哪个时间逛淘宝
            user_review_num_level=median(shop_review_num_level)  #用户逛的店铺的评价数的中位数
            #user_shop_star_level=as.numeric(which.max(table(shop_star_level))) #用户逛的店铺的星级等级
  )  %>%
  data.frame() %>%
  arrange(desc(user_open_times))
uf2<-train_p2 %>%
  group_by(user_id,shop_id) %>%
  summarize(user_shop_open_times=length(is_trade),
            user_shop_buy_times=length(which(is_trade==1)),
            user_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()
uf3<-train_p2 %>%
  group_by(user_id,item_id) %>%
  summarize(user_item_open_times=length(is_trade),
            user_item_buy_times=length(which(is_trade==1)),
            user_item_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

uf4<-train_p2 %>%
  group_by(user_id,item_category_list) %>%
  summarize(user_category_open_times=length(is_trade),
            user_category_buy_times=length(which(is_trade==1)),
            user_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

uf5<-train_p2 %>%
  group_by(user_id,item_brand_id) %>%
  summarize(user_brand_open_times=length(is_trade),
            user_brand_buy_times=length(which(is_trade==1)),
            user_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

#------------------------------1将user_feature 填充到test_p1中去
user_test_p2<-test_p2[,colnames(test_p2) %in% 
                        c("instance_id","user_id","shop_id","item_id","item_category_list","item_brand_id")]
user_feature_p2<-user_test_p2%>%
  left_join(uf1,by=c("user_id"="user_id"))%>%
  left_join(uf2,by=c("user_id"="user_id","shop_id"="shop_id")) %>%
  left_join(uf3,by=c("user_id"="user_id","item_id"="item_id")) %>%
  left_join(uf4,by=c("user_id"="user_id","item_category_list"="item_category_list")) %>%
  left_join(uf5,by=c("user_id"="user_id","item_brand_id"="item_brand_id")) %>%
  mutate(user_shop_times_to_total_times_prop=user_shop_open_times/user_open_times,
         user_shop_buy_to_total_buy_prop=user_shop_buy_times/user_buy_times,
         user_item_times_to_total_times_prop=user_item_open_times/user_open_times,
         user_item_buy_to_total_buy_prop=user_item_buy_times/user_buy_times,
         user_category_times_to_total_times_prop=user_category_open_times/user_open_times,
         user_category_buy_to_total_buy_prop=user_category_buy_times/user_buy_times,
         user_brand_times_to_total_times_prop=user_brand_open_times/user_open_times,
         user_brand_buy_to_total_buy_prop=user_brand_buy_times/user_buy_times
  )%>%
  data.frame()
#1.1将user的其他分类特征与item的交互特征提取处理
ucf1<-train_p2 %>%
  group_by(user_age_level,shop_id)%>%
  summarize(age_shop_open_times=length(is_trade),
            age_shop_buy_times=length(which(is_trade==1)),
            age_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf2<-train_p2%>%
  group_by(user_gender_id,shop_id)%>%
  summarize(gender_shop_open_times=length(is_trade),
            gender_shop_buy_times=length(which(is_trade==1)),
            gender_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf3<-train_p2 %>%
  group_by(user_occupation_id,shop_id)%>%
  summarize(occupation_shop_open_times=length(is_trade),
            occupation_shop_buy_times=length(which(is_trade==1)),
            occupation_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf4<-train_p2 %>%
  group_by(user_age_level,item_category_list)%>%
  summarize(age_category_open_times=length(is_trade),
            age_category_buy_times=length(which(is_trade==1)),
            age_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf5<-train_p2 %>%
  group_by(user_gender_id,item_category_list)%>%
  summarize(gender_category_open_times=length(is_trade),
            gender_category_buy_times=length(which(is_trade==1)),
            gender_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf6<-train_p2%>%
  group_by(user_occupation_id,item_category_list)%>%
  summarize(occupation_category_open_times=length(is_trade),
            occupation_category_buy_times=length(which(is_trade==1)),
            occupation_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf7<-train_p2 %>%
  group_by(user_age_level,item_brand_id)%>%
  summarize(age_brand_open_times=length(is_trade),
            age_brand_buy_times=length(which(is_trade==1)),
            age_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf8<-train_p2 %>%
  group_by(user_gender_id,item_brand_id)%>%
  summarize(gender_brand_open_times=length(is_trade),
            gender_brand_buy_times=length(which(is_trade==1)),
            gender_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf9<-train_p2%>%
  group_by(user_occupation_id,item_brand_id)%>%
  summarize(occupation_brand_open_times=length(is_trade),
            occupation_brand_buy_times=length(which(is_trade==1)),
            occupation_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf10<-train_p2 %>%
  group_by(user_age_level,item_price_level)%>%
  summarize(age_price_open_times=length(is_trade),
            age_price_buy_times=length(which(is_trade==1)),
            age_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf11<-train_p2 %>%
  group_by(user_gender_id,item_price_level)%>%
  summarize(gender_price_open_times=length(is_trade),
            gender_price_buy_times=length(which(is_trade==1)),
            gender_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf12<-train_p2%>%
  group_by(user_occupation_id,item_price_level)%>%
  summarize(occupation_price_open_times=length(is_trade),
            occupation_price_buy_times=length(which(is_trade==1)),
            occupation_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf13<-train_p2 %>%
  group_by(user_age_level,item_sales_level)%>%
  summarize(age_sales_open_times=length(is_trade),
            age_sales_buy_times=length(which(is_trade==1)),
            age_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf14<-train_p2 %>%
  group_by(user_gender_id,item_sales_level)%>%
  summarize(gender_sales_open_times=length(is_trade),
            gender_sales_buy_times=length(which(is_trade==1)),
            gender_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf15<-train_p2%>%
  group_by(user_occupation_id,item_sales_level)%>%
  summarize(occupation_sales_open_times=length(is_trade),
            occupation_sales_buy_times=length(which(is_trade==1)),
            occupation_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf16<-train_p2 %>%
  group_by(user_age_level,item_id)%>%
  summarize(age_item_open_times=length(is_trade),
            age_item_buy_times=length(which(is_trade==1)),
            age_item_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf17<-train_p2 %>%
  group_by(user_gender_id,item_id)%>%
  summarize(gender_item__open_times=length(is_trade),
            gender_item__buy_times=length(which(is_trade==1)),
            gender_item__buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf18<-train_p2%>%
  group_by(user_occupation_id,item_id)%>%
  summarize(occupation_item_id_open_times=length(is_trade),
            occupation_item_id_buy_times=length(which(is_trade==1)),
            occupation_item_id_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

#-----------------------user_class 的特征填充到test_p1中去
user_class_test_p2<-test_p2[,colnames(test_p2) %in% 
                              c("instance_id","shop_id","item_id","item_category_list","item_brand_id",
                                "item_sales_level","item_price_level","user_age_level","user_occupation_id",
                                "user_gender_id")]
user_class_feature_p2<-user_class_test_p2%>%
  left_join(ucf1,by=c("user_age_level"="user_age_level","shop_id"="shop_id"))%>%
  left_join(ucf2,by=c("user_gender_id"="user_gender_id","shop_id"="shop_id"))%>%
  left_join(ucf3,by=c("user_occupation_id"="user_occupation_id","shop_id"="shop_id"))%>%
  left_join(ucf4,by=c("user_age_level"="user_age_level","item_category_list"="item_category_list"))%>%
  left_join(ucf5,by=c("user_gender_id"="user_gender_id","item_category_list"="item_category_list"))%>%
  left_join(ucf6,by=c("user_occupation_id"="user_occupation_id","item_category_list"="item_category_list"))%>%
  left_join(ucf7,by=c("user_age_level"="user_age_level","item_brand_id"="item_brand_id"))%>%
  left_join(ucf8,by=c("user_gender_id"="user_gender_id","item_brand_id"="item_brand_id"))%>%
  left_join(ucf9,by=c("user_occupation_id"="user_occupation_id","item_brand_id"="item_brand_id"))%>%
  left_join(ucf10,by=c("user_age_level"="user_age_level","item_price_level"="item_price_level"))%>%
  left_join(ucf11,by=c("user_gender_id"="user_gender_id","item_price_level"="item_price_level"))%>%
  left_join(ucf12,by=c("user_occupation_id"="user_occupation_id","item_price_level"="item_price_level"))%>%
  left_join(ucf13,by=c("user_age_level"="user_age_level","item_sales_level"="item_sales_level"))%>%
  left_join(ucf14,by=c("user_gender_id"="user_gender_id","item_sales_level"="item_sales_level"))%>%
  left_join(ucf15,by=c("user_occupation_id"="user_occupation_id","item_sales_level"="item_sales_level"))%>%
  left_join(ucf16,by=c("user_age_level"="user_age_level","item_id"="item_id"))%>%
  left_join(ucf17,by=c("user_gender_id"="user_gender_id","item_id"="item_id"))%>%
  left_join(ucf18,by=c("user_occupation_id"="user_occupation_id","item_id"="item_id"))%>%
  data.frame()

#2 item_id feature
if1<-train_p2 %>%
  group_by(item_id) %>%
  summarize(item_buy_prop=length(which(is_trade==1))/length(is_trade),
            item_open_times=length(is_trade),
            item_buy_times=length(which(is_trade==1)),
            item_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            item_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            item_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            item_age_level=median(user_age_level), 
            item_star_level=median(user_star_level), 
            item_median_hour=median(time_hour)
  ) %>%
  data.frame()  

#2.1  item_brand_feature
if2<-train_p2 %>%
  group_by(item_brand_id) %>%
  summarize(brand_buy_prop=length(which(is_trade==1))/length(is_trade),
            brand_open_times=length(is_trade),
            brand_buy_times=length(which(is_trade==1)),
            brand_item_num=length(unique(item_id)),
            brand_shop_num=length(unique(shop_id)),
            brand_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            brand_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            brand_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            brand_age_level=median(user_age_level), 
            brand_star_level=median(user_star_level), 
            brand_median_hour=median(time_hour)
  ) %>%
  data.frame()  


#2.2 item_shop_feature 
if3<-train_p2 %>%
  group_by(shop_id) %>%
  summarize(shop_buy_prop=length(which(is_trade==1))/length(is_trade),
            shop_open_times=length(is_trade),
            shop_buy_times=length(which(is_trade==1)),
            shop_item_num=length(unique(item_id)),
            shop_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            shop_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            shop_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            shop_age_level=median(user_age_level), 
            shop_user_star_level=median(user_star_level), 
            shop_median_hour=median(time_hour)
  ) %>%
  data.frame()  

#2.3 item_category_feature
if4<-train_p2 %>%  
  group_by(item_category_list) %>%
  summarize(category_buy_prop=length(which(is_trade==1))/length(is_trade),
            category_open_times=length(is_trade),
            category_buy_times=length(which(is_trade==1)),
            category_item_num=length(unique(item_id)),
            category_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            category_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            category_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            category_age_level=median(user_age_level), 
            category_user_star_level=median(user_star_level), 
            category_median_hour=median(time_hour)
  ) %>%
  data.frame()

#-------------------------------2 将item的特征填充到test_p1中
item_test_p2<-test_p2[,colnames(test_p2) %in% 
                        c("instance_id","item_id","shop_id","item_category_list","item_brand_id")]
item_feature_p2<-item_test_p2 %>%
  left_join(if1,by=c("item_id"="item_id")) %>%
  left_join(if2,by=c("item_brand_id"="item_brand_id")) %>%
  left_join(if3,by=c("shop_id"="shop_id")) %>%
  left_join(if4,by=c("item_category_list"="item_category_list"))%>%
  mutate(item_shop_times_to_total_times_prop=item_open_times/shop_open_times,
         item_shop_buy_to_total_buy_prop=item_buy_times/shop_buy_times,
         item_brand_times_to_total_times_prop=item_open_times/brand_open_times,
         item_brand_buy_to_total_buy_prop=item_buy_times/brand_buy_times,
         item_category_times_to_total_times_prop=item_open_times/category_open_times,
         item_category_buy_to_total_buy_prop=item_buy_times/category_buy_times
  )%>%
  data.frame()


#------------------------其他的分类变量的特征
#3.1 城市特征
item_city<-train_p2 %>%
  group_by(item_city_id) %>%
  summarize(city_prop=length(which(is_trade==1))/length(is_trade),
            city_open_times=length(is_trade),
            city_buy_times=length(which(is_trade==1)),
            city_price_level=median(item_price_level),
            city_sales_level=median(item_sales_level),
            city_pv_level=median(item_pv_level)
  )%>%
  data.frame()

#3.2 价格等级特征
item_price<-train_p2 %>%
  group_by(item_price_level) %>%
  summarize(item_price_prop=length(which(is_trade==1))/length(is_trade),
            item_price_num=length(is_trade),
            item_price_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(item_price_prop))%>%
  data.frame()

#3.3 销量等级特征
item_sales<-train_p2 %>%
  group_by(item_sales_level) %>%
  summarize(item_sales_prop=length(which(is_trade==1))/length(is_trade),
            item_sales_num=length(is_trade),
            item_sales_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(item_sales_prop))%>%
  data.frame() 

#3.4 展示量等级特征
item_pv<-train_p2 %>%
  group_by(item_pv_level) %>%
  summarize(item_pv_prop=length(which(is_trade==1))/length(is_trade),
            item_pv_num=length(is_trade),
            item_pv_buy_times=length(is_trade)) %>%
  arrange(desc(item_pv_prop))%>%
  data.frame() 

#3.5 用户职业特征
user_occ<-train_p2 %>%
  group_by(user_occupation_id) %>%
  summarize(user_occ_prop=length(which(is_trade==1))/length(is_trade),
            user_occ_num=length(is_trade),
            user_occ_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(user_occ_prop))%>%
  data.frame() 

#3.6 用户年龄的购买率
user_age<-train_p2 %>%
  group_by(user_age_level) %>%
  summarize(user_age_prop=length(which(is_trade==1))/length(is_trade),
            user_age_tiems=length(is_trade),
            user_age_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(user_age_prop))%>%
  data.frame()

user_gender<-train_p2 %>%
  group_by(user_gender_id) %>%
  summarize(user_gender_prop=length(which(is_trade==1))/length(is_trade),
            user_gender_tiems=length(is_trade),
            user_gender_buy_times=length(which(is_trade==1))) %>%
  data.frame()

#3.7 用户等级购买率
user_star<-train_p2 %>%
  group_by(user_star_level) %>%
  summarize(user_star_prop=length(which(is_trade==1))/length(is_trade),user_star_num=length(is_trade)) %>%
  arrange(desc(user_star_prop))%>%
  data.frame()

#3.8 小时的特征
time_hour<-train_p2 %>%
  group_by(time_hour) %>%
  summarize(time_hour_buy_prop=length(which(is_trade==1))/length(is_trade),
            time_hour_open_times=length(is_trade),
            time_hour_buy_times=length(which(is_trade==1))) %>%
  data.frame()
#3.9 时间分组的特征
time_class<-train_p2 %>%
  group_by(time_class) %>%
  summarize(time_class_buy_prop=length(which(is_trade==1))/length(is_trade),
            time_class_open_times=length(is_trade),
            time_class_buy_times=length(which(is_trade==1))) %>%
  data.frame()

#3.10 评论数与购买率
shop_review<-train_p2 %>%
  group_by(shop_review_num_level) %>%
  summarize(shop_review_prop=length(which(is_trade==1))/length(is_trade),
            shop_review_times=length(is_trade),
            shop_review_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(shop_review_prop))%>%
  data.frame()


#3.11 商铺等级与购买率
shop_star<-train_p2 %>%
  group_by(shop_star_level) %>%
  summarize(shop_star_prop=length(which(is_trade==1))/length(is_trade),
            shop_star_times=length(is_trade),
            shop_star_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(shop_star_prop)) %>%
  data.frame() 
# len_intersect<-function(x){
#   return(c(length(intersect(train_p3[,colnames(train_p3)%in%x],test_p3[,colnames(test_p3)%in%x])),           #检查训练集和特征集的id的交集情况
#            length(unique(test_p3[,colnames(test_p3)%in%x]))))
# }

#----------------3 将other_class_feature 填充到test_p1中
other_class_test_p2<-test_p2[,colnames(test_p2) %in% c("instance_id","item_city_id","item_pv_level","user_star_level","time_hour","time_class",
                                                       "item_sales_level","item_price_level","user_age_level","user_occupation_id",
                                                       "user_gender_id","shop_review_num_level","shop_star_level")]

other_class_feature_p2<-other_class_test_p2 %>%
  left_join(item_city,by=c('item_city_id'='item_city_id')) %>%  #城市的购买率和频数
  left_join(item_price,by=c('item_price_level'='item_price_level')) %>% #价格的购买率和频数
  left_join(item_sales,by=c('item_sales_level'='item_sales_level'))  %>%#销量的购买率和频数
  left_join(item_pv,by=c('item_pv_level'='item_pv_level')) %>% #展示量的购买率和频数
  #left_join(instance_property,by=c("instance_id","instance_id"))    %>%      #商品的属性的加权购买率
  left_join(user_occ,by=c('user_occupation_id'='user_occupation_id')) %>% #用户职业的购买率和频数
  left_join(user_age,by=c('user_age_level'='user_age_level'))  %>%#用户年龄的购买率和频数
  left_join(user_gender,by=c('user_gender_id'='user_gender_id'))  %>%#用户性别的购买率和频数
  left_join(user_star,by=c('user_star_level'='user_star_level'))  %>%#用户星级的购买率和频数
  left_join(time_hour,by=c('time_hour'='time_hour')) %>% #小时的购买率和频数
  left_join(time_class,by=c('time_class'='time_class')) %>% #时间段的购买率和频数
  left_join(shop_review,by=c('shop_review_num_level'='shop_review_num_level'))  %>%
  left_join(shop_star,by=c('shop_star_level'='shop_star_level'))  %>%
  data.frame()

#---------------------------------------------------------------------test2 提取label --------------------------
l1<-test_p2 %>%
  group_by(user_id) %>%
  summarize(user_open_last=max(time_seconds),
            user_open_first=min(time_seconds),
            user_open_times_today=length(user_id),
            user_shop_num=length(unique(shop_id)),
            user_item_num=length(unique(item_id))
  )%>%
  data.frame()
l2<-test_p2 %>%
  group_by(user_id,item_category_list)%>%
  summarize(user_category_open_last=max(time_seconds),
            user_category_open_first=min(time_seconds),
            user_category_times=length(user_id)
  )%>%
  data.frame()
l3<-test_p2%>%
  group_by(user_id,shop_id)%>%
  summarize(user_shop_open_last=max(time_seconds),
            user_shop_open_first=min(time_seconds),
            user_shop_times=length(user_id)
  )%>%
  data.frame()
l4<-test_p2%>%
  group_by(user_id,item_id)%>%
  summarize(user_item_open_last=max(time_seconds),
            user_item_open_first=min(time_seconds),
            user_item_times=length(user_id)
  )%>%
  data.frame()
l5<-test_p2%>%
  group_by(user_id,item_brand_id)%>%
  summarize(user_brand_open_last=max(time_seconds),
            user_brand_open_first=min(time_seconds),
            user_brand_times=length(user_id)
  )%>%
  data.frame()

is_firstlast<-function(x){
  if(x==0)
    return(1)
  else
    return(0)
}

test_feature_label_2<-test_p2%>%
  left_join(l1,by=c("user_id"="user_id"))%>%
  left_join(l2,by=c("user_id"="user_id","item_category_list"="item_category_list"))%>%
  left_join(l3,by=c("user_id"="user_id","shop_id"="shop_id"))%>%
  left_join(l4,by=c("user_id"="user_id","item_id"="item_id"))%>%
  left_join(l5,by=c("user_id"="user_id","item_brand_id"="item_brand_id")) %>%
  mutate(
    user_to_category_times_prop=user_open_times_today/user_category_times,
    user_to_shop_times_prop=user_open_times_today/user_shop_times,
    user_to_item_times_prop=user_open_times_today/user_item_times,
    user_to_brand_times_prop=user_open_times_today/user_brand_times,
    user_open_last_time_diff=difftime(user_open_last,time_seconds,units="secs"),
    user_open_first_time_diff=difftime(time_seconds,user_open_first,units="secs"),
    user_cat_open_last_time_diff=difftime(user_category_open_last,time_seconds,units="secs"),
    user_cat_open_first_time_diff=difftime(time_seconds,user_category_open_first,units="secs"),
    user_shop_open_last_time_diff=difftime(user_shop_open_last,time_seconds,units="secs"),
    user_shop_open_first_time_diff=difftime(time_seconds,user_shop_open_first,units="secs"),
    user_item_open_last_time_diff=difftime(user_item_open_last,time_seconds,units="secs"),
    user_item_open_first_time_diff=difftime(time_seconds,user_item_open_first,units="secs"),
    user_brand_open_last_time_diff=difftime(user_brand_open_last,time_seconds,units="secs"),
    user_brand_open_first_time_diff=difftime(time_seconds,user_brand_open_first,units="secs"),
    is_user_open_last_time=apply(matrix(user_open_last_time_diff),1,FUN=is_firstlast),
    is_user_open_first_time=apply(matrix(user_open_first_time_diff),1,FUN=is_firstlast),
    is_user_cat_open_last_time=apply(matrix(user_cat_open_last_time_diff),1,FUN=is_firstlast),
    is_user_cat_open_first_time=apply(matrix(user_cat_open_first_time_diff),1,FUN=is_firstlast),
    is_user_shop_open_last_time=apply(matrix(user_shop_open_last_time_diff),1,FUN=is_firstlast),
    is_user_shop_open_first_time=apply(matrix(user_shop_open_first_time_diff),1,FUN=is_firstlast),
    is_user_item_open_last_time=apply(matrix(user_item_open_last_time_diff),1,FUN=is_firstlast),
    is_user_item_open_first_time=apply(matrix(user_item_open_first_time_diff),1,FUN=is_firstlast),
    is_user_brand_open_last_time=apply(matrix(user_brand_open_last_time_diff),1,FUN=is_firstlast),
    is_user_brand_open_first_time=apply(matrix(user_brand_open_first_time_diff),1,FUN=is_firstlast)
  )%>%
  select(-c(user_open_last,user_open_first,user_category_open_last,user_category_open_first,user_shop_open_last,user_shop_open_first,
            user_item_open_last,user_item_open_first,user_brand_open_last,user_brand_open_first,user_open_last_time_diff,
            user_open_first_time_diff,user_brand_open_last_time_diff,user_brand_open_first_time_diff,
            user_cat_open_last_time_diff,user_cat_open_first_time_diff,user_shop_open_last_time_diff,user_shop_open_first_time_diff,
            user_item_open_last_time_diff,user_item_open_first_time_diff))  %>%
  cbind(user_feature_p2[,-c(1:6)],user_class_feature_p2[,-c(1:10)],item_feature_p2[,-c(1:5)],other_class_feature_p2[,-c(1:13)])

#-----------------------------------------------------------train_p3 提取feature-----------------------
#1.0 用户id feature
uf1<-train_p3 %>%
  group_by(user_id) %>%
  summarize(user_category_num=length(unique(item_category_list)),
            user_open_times=length(user_id),
            user_buy_times=length(which(is_trade==1)),
            user_buy_prop=length(which(is_trade==1))/length(is_trade),
            user_price_level=median(item_price_level),
            user_brand_num=length(unique(item_brand_id)),   #光顾过多少品牌
            user_shop_num=length(unique(shop_id)),      #光顾过多少店铺
            user_time_hour=median(time_hour),          #用户经常在哪个时间逛淘宝
            user_review_num_level=median(shop_review_num_level)  #用户逛的店铺的评价数的中位数
            #user_shop_star_level=as.numeric(which.max(table(shop_star_level))) #用户逛的店铺的星级等级
  )  %>%
  data.frame() %>%
  arrange(desc(user_open_times))
uf2<-train_p3 %>%
  group_by(user_id,shop_id) %>%
  summarize(user_shop_open_times=length(is_trade),
            user_shop_buy_times=length(which(is_trade==1)),
            user_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()
uf3<-train_p3 %>%
  group_by(user_id,item_id) %>%
  summarize(user_item_open_times=length(is_trade),
            user_item_buy_times=length(which(is_trade==1)),
            user_item_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

uf4<-train_p3 %>%
  group_by(user_id,item_category_list) %>%
  summarize(user_category_open_times=length(is_trade),
            user_category_buy_times=length(which(is_trade==1)),
            user_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

uf5<-train_p3 %>%
  group_by(user_id,item_brand_id) %>%
  summarize(user_brand_open_times=length(is_trade),
            user_brand_buy_times=length(which(is_trade==1)),
            user_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

#------------------------------1将user_feature 填充到test_p1中去
user_test_p3<-test_p3[,colnames(test_p3) %in% 
                        c("instance_id","user_id","shop_id","item_id","item_category_list","item_brand_id")]
user_feature_p3<-user_test_p3%>%
  left_join(uf1,by=c("user_id"="user_id"))%>%
  left_join(uf2,by=c("user_id"="user_id","shop_id"="shop_id")) %>%
  left_join(uf3,by=c("user_id"="user_id","item_id"="item_id")) %>%
  left_join(uf4,by=c("user_id"="user_id","item_category_list"="item_category_list")) %>%
  left_join(uf5,by=c("user_id"="user_id","item_brand_id"="item_brand_id")) %>%
  mutate(user_shop_times_to_total_times_prop=user_shop_open_times/user_open_times,
         user_shop_buy_to_total_buy_prop=user_shop_buy_times/user_buy_times,
         user_item_times_to_total_times_prop=user_item_open_times/user_open_times,
         user_item_buy_to_total_buy_prop=user_item_buy_times/user_buy_times,
         user_category_times_to_total_times_prop=user_category_open_times/user_open_times,
         user_category_buy_to_total_buy_prop=user_category_buy_times/user_buy_times,
         user_brand_times_to_total_times_prop=user_brand_open_times/user_open_times,
         user_brand_buy_to_total_buy_prop=user_brand_buy_times/user_buy_times
  )%>%
  data.frame()
#1.1将user的其他分类特征与item的交互特征提取处理
ucf1<-train_p3 %>%
  group_by(user_age_level,shop_id)%>%
  summarize(age_shop_open_times=length(is_trade),
            age_shop_buy_times=length(which(is_trade==1)),
            age_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf2<-train_p3%>%
  group_by(user_gender_id,shop_id)%>%
  summarize(gender_shop_open_times=length(is_trade),
            gender_shop_buy_times=length(which(is_trade==1)),
            gender_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf3<-train_p3 %>%
  group_by(user_occupation_id,shop_id)%>%
  summarize(occupation_shop_open_times=length(is_trade),
            occupation_shop_buy_times=length(which(is_trade==1)),
            occupation_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf4<-train_p3 %>%
  group_by(user_age_level,item_category_list)%>%
  summarize(age_category_open_times=length(is_trade),
            age_category_buy_times=length(which(is_trade==1)),
            age_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf5<-train_p3 %>%
  group_by(user_gender_id,item_category_list)%>%
  summarize(gender_category_open_times=length(is_trade),
            gender_category_buy_times=length(which(is_trade==1)),
            gender_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf6<-train_p3%>%
  group_by(user_occupation_id,item_category_list)%>%
  summarize(occupation_category_open_times=length(is_trade),
            occupation_category_buy_times=length(which(is_trade==1)),
            occupation_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf7<-train_p3 %>%
  group_by(user_age_level,item_brand_id)%>%
  summarize(age_brand_open_times=length(is_trade),
            age_brand_buy_times=length(which(is_trade==1)),
            age_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf8<-train_p3 %>%
  group_by(user_gender_id,item_brand_id)%>%
  summarize(gender_brand_open_times=length(is_trade),
            gender_brand_buy_times=length(which(is_trade==1)),
            gender_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf9<-train_p3%>%
  group_by(user_occupation_id,item_brand_id)%>%
  summarize(occupation_brand_open_times=length(is_trade),
            occupation_brand_buy_times=length(which(is_trade==1)),
            occupation_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf10<-train_p3 %>%
  group_by(user_age_level,item_price_level)%>%
  summarize(age_price_open_times=length(is_trade),
            age_price_buy_times=length(which(is_trade==1)),
            age_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf11<-train_p3 %>%
  group_by(user_gender_id,item_price_level)%>%
  summarize(gender_price_open_times=length(is_trade),
            gender_price_buy_times=length(which(is_trade==1)),
            gender_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf12<-train_p3%>%
  group_by(user_occupation_id,item_price_level)%>%
  summarize(occupation_price_open_times=length(is_trade),
            occupation_price_buy_times=length(which(is_trade==1)),
            occupation_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf13<-train_p3 %>%
  group_by(user_age_level,item_sales_level)%>%
  summarize(age_sales_open_times=length(is_trade),
            age_sales_buy_times=length(which(is_trade==1)),
            age_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf14<-train_p3 %>%
  group_by(user_gender_id,item_sales_level)%>%
  summarize(gender_sales_open_times=length(is_trade),
            gender_sales_buy_times=length(which(is_trade==1)),
            gender_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf15<-train_p3%>%
  group_by(user_occupation_id,item_sales_level)%>%
  summarize(occupation_sales_open_times=length(is_trade),
            occupation_sales_buy_times=length(which(is_trade==1)),
            occupation_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf16<-train_p3 %>%
  group_by(user_age_level,item_id)%>%
  summarize(age_item_open_times=length(is_trade),
            age_item_buy_times=length(which(is_trade==1)),
            age_item_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf17<-train_p3 %>%
  group_by(user_gender_id,item_id)%>%
  summarize(gender_item__open_times=length(is_trade),
            gender_item__buy_times=length(which(is_trade==1)),
            gender_item__buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf18<-train_p3%>%
  group_by(user_occupation_id,item_id)%>%
  summarize(occupation_item_id_open_times=length(is_trade),
            occupation_item_id_buy_times=length(which(is_trade==1)),
            occupation_item_id_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

#-----------------------user_class 的特征填充到test_p1中去
user_class_test_p3<-test_p3[,colnames(test_p3) %in% 
                              c("instance_id","shop_id","item_id","item_category_list","item_brand_id",
                                "item_sales_level","item_price_level","user_age_level","user_occupation_id",
                                "user_gender_id")]
user_class_feature_p3<-user_class_test_p3%>%
  left_join(ucf1,by=c("user_age_level"="user_age_level","shop_id"="shop_id"))%>%
  left_join(ucf2,by=c("user_gender_id"="user_gender_id","shop_id"="shop_id"))%>%
  left_join(ucf3,by=c("user_occupation_id"="user_occupation_id","shop_id"="shop_id"))%>%
  left_join(ucf4,by=c("user_age_level"="user_age_level","item_category_list"="item_category_list"))%>%
  left_join(ucf5,by=c("user_gender_id"="user_gender_id","item_category_list"="item_category_list"))%>%
  left_join(ucf6,by=c("user_occupation_id"="user_occupation_id","item_category_list"="item_category_list"))%>%
  left_join(ucf7,by=c("user_age_level"="user_age_level","item_brand_id"="item_brand_id"))%>%
  left_join(ucf8,by=c("user_gender_id"="user_gender_id","item_brand_id"="item_brand_id"))%>%
  left_join(ucf9,by=c("user_occupation_id"="user_occupation_id","item_brand_id"="item_brand_id"))%>%
  left_join(ucf10,by=c("user_age_level"="user_age_level","item_price_level"="item_price_level"))%>%
  left_join(ucf11,by=c("user_gender_id"="user_gender_id","item_price_level"="item_price_level"))%>%
  left_join(ucf12,by=c("user_occupation_id"="user_occupation_id","item_price_level"="item_price_level"))%>%
  left_join(ucf13,by=c("user_age_level"="user_age_level","item_sales_level"="item_sales_level"))%>%
  left_join(ucf14,by=c("user_gender_id"="user_gender_id","item_sales_level"="item_sales_level"))%>%
  left_join(ucf15,by=c("user_occupation_id"="user_occupation_id","item_sales_level"="item_sales_level"))%>%
  left_join(ucf16,by=c("user_age_level"="user_age_level","item_id"="item_id"))%>%
  left_join(ucf17,by=c("user_gender_id"="user_gender_id","item_id"="item_id"))%>%
  left_join(ucf18,by=c("user_occupation_id"="user_occupation_id","item_id"="item_id"))%>%
  data.frame()

#2 item_id feature
if1<-train_p3 %>%
  group_by(item_id) %>%
  summarize(item_buy_prop=length(which(is_trade==1))/length(is_trade),
            item_open_times=length(is_trade),
            item_buy_times=length(which(is_trade==1)),
            item_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            item_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            item_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            item_age_level=median(user_age_level), 
            item_star_level=median(user_star_level), 
            item_median_hour=median(time_hour)
  ) %>%
  data.frame()  

#2.1  item_brand_feature
if2<-train_p3 %>%
  group_by(item_brand_id) %>%
  summarize(brand_buy_prop=length(which(is_trade==1))/length(is_trade),
            brand_open_times=length(is_trade),
            brand_buy_times=length(which(is_trade==1)),
            brand_item_num=length(unique(item_id)),
            brand_shop_num=length(unique(shop_id)),
            brand_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            brand_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            brand_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            brand_age_level=median(user_age_level), 
            brand_star_level=median(user_star_level), 
            brand_median_hour=median(time_hour)
  ) %>%
  data.frame()  


#2.2 item_shop_feature 
if3<-train_p3 %>%
  group_by(shop_id) %>%
  summarize(shop_buy_prop=length(which(is_trade==1))/length(is_trade),
            shop_open_times=length(is_trade),
            shop_buy_times=length(which(is_trade==1)),
            shop_item_num=length(unique(item_id)),
            shop_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            shop_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            shop_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            shop_age_level=median(user_age_level), 
            shop_user_star_level=median(user_star_level), 
            shop_median_hour=median(time_hour)
  ) %>%
  data.frame()  

#2.3 item_category_feature
if4<-train_p3 %>%  
  group_by(item_category_list) %>%
  summarize(category_buy_prop=length(which(is_trade==1))/length(is_trade),
            category_open_times=length(is_trade),
            category_buy_times=length(which(is_trade==1)),
            category_item_num=length(unique(item_id)),
            category_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            category_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            category_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            category_age_level=median(user_age_level), 
            category_user_star_level=median(user_star_level), 
            category_median_hour=median(time_hour)
  ) %>%
  data.frame()

#-------------------------------2 将item的特征填充到test_p3中
item_test_p3<-test_p3[,colnames(test_p3) %in% 
                        c("instance_id","item_id","shop_id","item_category_list","item_brand_id")]
item_feature_p3<-item_test_p3 %>%
  left_join(if1,by=c("item_id"="item_id")) %>%
  left_join(if2,by=c("item_brand_id"="item_brand_id")) %>%
  left_join(if3,by=c("shop_id"="shop_id")) %>%
  left_join(if4,by=c("item_category_list"="item_category_list"))%>%
  mutate(item_shop_times_to_total_times_prop=item_open_times/shop_open_times,
         item_shop_buy_to_total_buy_prop=item_buy_times/shop_buy_times,
         item_brand_times_to_total_times_prop=item_open_times/brand_open_times,
         item_brand_buy_to_total_buy_prop=item_buy_times/brand_buy_times,
         item_category_times_to_total_times_prop=item_open_times/category_open_times,
         item_category_buy_to_total_buy_prop=item_buy_times/category_buy_times
  )%>%
  data.frame()


#------------------------其他的分类变量的特征
#3.1 城市特征
item_city<-train_p3 %>%
  group_by(item_city_id) %>%
  summarize(city_prop=length(which(is_trade==1))/length(is_trade),
            city_open_times=length(is_trade),
            city_buy_times=length(which(is_trade==1)),
            city_price_level=median(item_price_level),
            city_sales_level=median(item_sales_level),
            city_pv_level=median(item_pv_level)
  )%>%
  data.frame()

#3.2 价格等级特征
item_price<-train_p3 %>%
  group_by(item_price_level) %>%
  summarize(item_price_prop=length(which(is_trade==1))/length(is_trade),
            item_price_num=length(is_trade),
            item_price_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(item_price_prop))%>%
  data.frame()

#3.3 销量等级特征
item_sales<-train_p3 %>%
  group_by(item_sales_level) %>%
  summarize(item_sales_prop=length(which(is_trade==1))/length(is_trade),
            item_sales_num=length(is_trade),
            item_sales_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(item_sales_prop))%>%
  data.frame() 

#3.4 展示量等级特征
item_pv<-train_p3 %>%
  group_by(item_pv_level) %>%
  summarize(item_pv_prop=length(which(is_trade==1))/length(is_trade),
            item_pv_num=length(is_trade),
            item_pv_buy_times=length(is_trade)) %>%
  arrange(desc(item_pv_prop))%>%
  data.frame() 

#3.5 用户职业特征
user_occ<-train_p3 %>%
  group_by(user_occupation_id) %>%
  summarize(user_occ_prop=length(which(is_trade==1))/length(is_trade),
            user_occ_num=length(is_trade),
            user_occ_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(user_occ_prop))%>%
  data.frame() 

#3.6 用户年龄的购买率
user_age<-train_p3 %>%
  group_by(user_age_level) %>%
  summarize(user_age_prop=length(which(is_trade==1))/length(is_trade),
            user_age_tiems=length(is_trade),
            user_age_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(user_age_prop))%>%
  data.frame()

user_gender<-train_p3 %>%
  group_by(user_gender_id) %>%
  summarize(user_gender_prop=length(which(is_trade==1))/length(is_trade),
            user_gender_tiems=length(is_trade),
            user_gender_buy_times=length(which(is_trade==1))) %>%
  data.frame()

#3.7 用户等级购买率
user_star<-train_p3 %>%
  group_by(user_star_level) %>%
  summarize(user_star_prop=length(which(is_trade==1))/length(is_trade),user_star_num=length(is_trade)) %>%
  arrange(desc(user_star_prop))%>%
  data.frame()

#3.8 小时的特征
time_hour<-train_p3 %>%
  group_by(time_hour) %>%
  summarize(time_hour_buy_prop=length(which(is_trade==1))/length(is_trade),
            time_hour_open_times=length(is_trade),
            time_hour_buy_times=length(which(is_trade==1))) %>%
  data.frame()
#3.9 时间分组的特征
time_class<-train_p3 %>%
  group_by(time_class) %>%
  summarize(time_class_buy_prop=length(which(is_trade==1))/length(is_trade),
            time_class_open_times=length(is_trade),
            time_class_buy_times=length(which(is_trade==1))) %>%
  data.frame()

#3.10 评论数与购买率
shop_review<-train_p3 %>%
  group_by(shop_review_num_level) %>%
  summarize(shop_review_prop=length(which(is_trade==1))/length(is_trade),
            shop_review_times=length(is_trade),
            shop_review_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(shop_review_prop))%>%
  data.frame()


#3.11 商铺等级与购买率
shop_star<-train_p3 %>%
  group_by(shop_star_level) %>%
  summarize(shop_star_prop=length(which(is_trade==1))/length(is_trade),
            shop_star_times=length(is_trade),
            shop_star_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(shop_star_prop)) %>%
  data.frame() 
# len_intersect<-function(x){
#   return(c(length(intersect(train_p3[,colnames(train_p3)%in%x],test_p3[,colnames(test_p3)%in%x])),           #检查训练集和特征集的id的交集情况
#            length(unique(test_p3[,colnames(test_p3)%in%x]))))
# }

#----------------3 将other_class_feature 填充到test_p1中
other_class_test_p3<-test_p3[,colnames(test_p3) %in% c("instance_id","item_city_id","item_pv_level","user_star_level","time_hour","time_class",
                                                       "item_sales_level","item_price_level","user_age_level","user_occupation_id",
                                                       "user_gender_id","shop_review_num_level","shop_star_level")]

other_class_feature_p3<-other_class_test_p3 %>%
  left_join(item_city,by=c('item_city_id'='item_city_id')) %>%  #城市的购买率和频数
  left_join(item_price,by=c('item_price_level'='item_price_level')) %>% #价格的购买率和频数
  left_join(item_sales,by=c('item_sales_level'='item_sales_level'))  %>%#销量的购买率和频数
  left_join(item_pv,by=c('item_pv_level'='item_pv_level')) %>% #展示量的购买率和频数
  #left_join(instance_property,by=c("instance_id","instance_id"))    %>%      #商品的属性的加权购买率
  left_join(user_occ,by=c('user_occupation_id'='user_occupation_id')) %>% #用户职业的购买率和频数
  left_join(user_age,by=c('user_age_level'='user_age_level'))  %>%#用户年龄的购买率和频数
  left_join(user_gender,by=c('user_gender_id'='user_gender_id'))  %>%#用户性别的购买率和频数
  left_join(user_star,by=c('user_star_level'='user_star_level'))  %>%#用户星级的购买率和频数
  left_join(time_hour,by=c('time_hour'='time_hour')) %>% #小时的购买率和频数
  left_join(time_class,by=c('time_class'='time_class')) %>% #时间段的购买率和频数
  left_join(shop_review,by=c('shop_review_num_level'='shop_review_num_level'))  %>%
  left_join(shop_star,by=c('shop_star_level'='shop_star_level'))  %>%
  data.frame()

#---------------------------------------------------------------------test3 提取label --------------------------
l1<-test_p3 %>%
  group_by(user_id) %>%
  summarize(user_open_last=max(time_seconds),
            user_open_first=min(time_seconds),
            user_open_times_today=length(user_id),
            user_shop_num=length(unique(shop_id)),
            user_item_num=length(unique(item_id))
  )%>%
  data.frame()
l2<-test_p3 %>%
  group_by(user_id,item_category_list)%>%
  summarize(user_category_open_last=max(time_seconds),
            user_category_open_first=min(time_seconds),
            user_category_times=length(user_id)
  )%>%
  data.frame()
l3<-test_p3%>%
  group_by(user_id,shop_id)%>%
  summarize(user_shop_open_last=max(time_seconds),
            user_shop_open_first=min(time_seconds),
            user_shop_times=length(user_id)
  )%>%
  data.frame()
l4<-test_p3%>%
  group_by(user_id,item_id)%>%
  summarize(user_item_open_last=max(time_seconds),
            user_item_open_first=min(time_seconds),
            user_item_times=length(user_id)
  )%>%
  data.frame()
l5<-test_p3%>%
  group_by(user_id,item_brand_id)%>%
  summarize(user_brand_open_last=max(time_seconds),
            user_brand_open_first=min(time_seconds),
            user_brand_times=length(user_id)
  )%>%
  data.frame()

is_firstlast<-function(x){
  if(x==0)
    return(1)
  else
    return(0)
}

test_feature_label_3<-test_p3%>%
  left_join(l1,by=c("user_id"="user_id"))%>%
  left_join(l2,by=c("user_id"="user_id","item_category_list"="item_category_list"))%>%
  left_join(l3,by=c("user_id"="user_id","shop_id"="shop_id"))%>%
  left_join(l4,by=c("user_id"="user_id","item_id"="item_id"))%>%
  left_join(l5,by=c("user_id"="user_id","item_brand_id"="item_brand_id")) %>%
  mutate(
    user_to_category_times_prop=user_open_times_today/user_category_times,
    user_to_shop_times_prop=user_open_times_today/user_shop_times,
    user_to_item_times_prop=user_open_times_today/user_item_times,
    user_to_brand_times_prop=user_open_times_today/user_brand_times,
    user_open_last_time_diff=difftime(user_open_last,time_seconds,units="secs"),
    user_open_first_time_diff=difftime(time_seconds,user_open_first,units="secs"),
    user_cat_open_last_time_diff=difftime(user_category_open_last,time_seconds,units="secs"),
    user_cat_open_first_time_diff=difftime(time_seconds,user_category_open_first,units="secs"),
    user_shop_open_last_time_diff=difftime(user_shop_open_last,time_seconds,units="secs"),
    user_shop_open_first_time_diff=difftime(time_seconds,user_shop_open_first,units="secs"),
    user_item_open_last_time_diff=difftime(user_item_open_last,time_seconds,units="secs"),
    user_item_open_first_time_diff=difftime(time_seconds,user_item_open_first,units="secs"),
    user_brand_open_last_time_diff=difftime(user_brand_open_last,time_seconds,units="secs"),
    user_brand_open_first_time_diff=difftime(time_seconds,user_brand_open_first,units="secs"),
    is_user_open_last_time=apply(matrix(user_open_last_time_diff),1,FUN=is_firstlast),
    is_user_open_first_time=apply(matrix(user_open_first_time_diff),1,FUN=is_firstlast),
    is_user_cat_open_last_time=apply(matrix(user_cat_open_last_time_diff),1,FUN=is_firstlast),
    is_user_cat_open_first_time=apply(matrix(user_cat_open_first_time_diff),1,FUN=is_firstlast),
    is_user_shop_open_last_time=apply(matrix(user_shop_open_last_time_diff),1,FUN=is_firstlast),
    is_user_shop_open_first_time=apply(matrix(user_shop_open_first_time_diff),1,FUN=is_firstlast),
    is_user_item_open_last_time=apply(matrix(user_item_open_last_time_diff),1,FUN=is_firstlast),
    is_user_item_open_first_time=apply(matrix(user_item_open_first_time_diff),1,FUN=is_firstlast),
    is_user_brand_open_last_time=apply(matrix(user_brand_open_last_time_diff),1,FUN=is_firstlast),
    is_user_brand_open_first_time=apply(matrix(user_brand_open_first_time_diff),1,FUN=is_firstlast)
  )%>%
  select(-c(user_open_last,user_open_first,user_category_open_last,user_category_open_first,user_shop_open_last,user_shop_open_first,
            user_item_open_last,user_item_open_first,user_brand_open_last,user_brand_open_first,user_open_last_time_diff,
            user_open_first_time_diff,user_brand_open_last_time_diff,user_brand_open_first_time_diff,
            user_cat_open_last_time_diff,user_cat_open_first_time_diff,user_shop_open_last_time_diff,user_shop_open_first_time_diff,
            user_item_open_last_time_diff,user_item_open_first_time_diff))  %>%
  cbind(user_feature_p3[,-c(1:6)],user_class_feature_p3[,-c(1:10)],item_feature_p3[,-c(1:5)],other_class_feature_p3[,-c(1:13)])



#-----------------------------------------------------------train_p4 提取feature-----------------------
#1.0 用户id feature
uf1<-train_p4 %>%
  group_by(user_id) %>%
  summarize(user_category_num=length(unique(item_category_list)),
            user_open_times=length(user_id),
            user_buy_times=length(which(is_trade==1)),
            user_buy_prop=length(which(is_trade==1))/length(is_trade),
            user_price_level=median(item_price_level),
            user_brand_num=length(unique(item_brand_id)),   #光顾过多少品牌
            user_shop_num=length(unique(shop_id)),      #光顾过多少店铺
            user_time_hour=median(time_hour),          #用户经常在哪个时间逛淘宝
            user_review_num_level=median(shop_review_num_level)  #用户逛的店铺的评价数的中位数
            #user_shop_star_level=as.numeric(which.max(table(shop_star_level))) #用户逛的店铺的星级等级
  )  %>%
  data.frame() %>%
  arrange(desc(user_open_times))
uf2<-train_p4 %>%
  group_by(user_id,shop_id) %>%
  summarize(user_shop_open_times=length(is_trade),
            user_shop_buy_times=length(which(is_trade==1)),
            user_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()
uf3<-train_p4 %>%
  group_by(user_id,item_id) %>%
  summarize(user_item_open_times=length(is_trade),
            user_item_buy_times=length(which(is_trade==1)),
            user_item_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

uf4<-train_p4 %>%
  group_by(user_id,item_category_list) %>%
  summarize(user_category_open_times=length(is_trade),
            user_category_buy_times=length(which(is_trade==1)),
            user_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

uf5<-train_p4 %>%
  group_by(user_id,item_brand_id) %>%
  summarize(user_brand_open_times=length(is_trade),
            user_brand_buy_times=length(which(is_trade==1)),
            user_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  ) %>%
  data.frame()

#------------------------------1将user_feature 填充到test_p1中去
user_test_p4<-test_p4[,colnames(test_p4) %in% 
                        c("instance_id","user_id","shop_id","item_id","item_category_list","item_brand_id")]
user_feature_p4<-user_test_p4%>%
  left_join(uf1,by=c("user_id"="user_id"))%>%
  left_join(uf2,by=c("user_id"="user_id","shop_id"="shop_id")) %>%
  left_join(uf3,by=c("user_id"="user_id","item_id"="item_id")) %>%
  left_join(uf4,by=c("user_id"="user_id","item_category_list"="item_category_list")) %>%
  left_join(uf5,by=c("user_id"="user_id","item_brand_id"="item_brand_id")) %>%
  mutate(user_shop_times_to_total_times_prop=user_shop_open_times/user_open_times,
         user_shop_buy_to_total_buy_prop=user_shop_buy_times/user_buy_times,
         user_item_times_to_total_times_prop=user_item_open_times/user_open_times,
         user_item_buy_to_total_buy_prop=user_item_buy_times/user_buy_times,
         user_category_times_to_total_times_prop=user_category_open_times/user_open_times,
         user_category_buy_to_total_buy_prop=user_category_buy_times/user_buy_times,
         user_brand_times_to_total_times_prop=user_brand_open_times/user_open_times,
         user_brand_buy_to_total_buy_prop=user_brand_buy_times/user_buy_times
  )%>%
  data.frame()
#1.1将user的其他分类特征与item的交互特征提取处理
ucf1<-train_p4 %>%
  group_by(user_age_level,shop_id)%>%
  summarize(age_shop_open_times=length(is_trade),
            age_shop_buy_times=length(which(is_trade==1)),
            age_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf2<-train_p4%>%
  group_by(user_gender_id,shop_id)%>%
  summarize(gender_shop_open_times=length(is_trade),
            gender_shop_buy_times=length(which(is_trade==1)),
            gender_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf3<-train_p4 %>%
  group_by(user_occupation_id,shop_id)%>%
  summarize(occupation_shop_open_times=length(is_trade),
            occupation_shop_buy_times=length(which(is_trade==1)),
            occupation_shop_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf4<-train_p4 %>%
  group_by(user_age_level,item_category_list)%>%
  summarize(age_category_open_times=length(is_trade),
            age_category_buy_times=length(which(is_trade==1)),
            age_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf5<-train_p4 %>%
  group_by(user_gender_id,item_category_list)%>%
  summarize(gender_category_open_times=length(is_trade),
            gender_category_buy_times=length(which(is_trade==1)),
            gender_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf6<-train_p4%>%
  group_by(user_occupation_id,item_category_list)%>%
  summarize(occupation_category_open_times=length(is_trade),
            occupation_category_buy_times=length(which(is_trade==1)),
            occupation_category_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf7<-train_p4 %>%
  group_by(user_age_level,item_brand_id)%>%
  summarize(age_brand_open_times=length(is_trade),
            age_brand_buy_times=length(which(is_trade==1)),
            age_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf8<-train_p4 %>%
  group_by(user_gender_id,item_brand_id)%>%
  summarize(gender_brand_open_times=length(is_trade),
            gender_brand_buy_times=length(which(is_trade==1)),
            gender_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf9<-train_p4%>%
  group_by(user_occupation_id,item_brand_id)%>%
  summarize(occupation_brand_open_times=length(is_trade),
            occupation_brand_buy_times=length(which(is_trade==1)),
            occupation_brand_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf10<-train_p4 %>%
  group_by(user_age_level,item_price_level)%>%
  summarize(age_price_open_times=length(is_trade),
            age_price_buy_times=length(which(is_trade==1)),
            age_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf11<-train_p4 %>%
  group_by(user_gender_id,item_price_level)%>%
  summarize(gender_price_open_times=length(is_trade),
            gender_price_buy_times=length(which(is_trade==1)),
            gender_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf12<-train_p4%>%
  group_by(user_occupation_id,item_price_level)%>%
  summarize(occupation_price_open_times=length(is_trade),
            occupation_price_buy_times=length(which(is_trade==1)),
            occupation_price_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

ucf13<-train_p4 %>%
  group_by(user_age_level,item_sales_level)%>%
  summarize(age_sales_open_times=length(is_trade),
            age_sales_buy_times=length(which(is_trade==1)),
            age_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf14<-train_p4 %>%
  group_by(user_gender_id,item_sales_level)%>%
  summarize(gender_sales_open_times=length(is_trade),
            gender_sales_buy_times=length(which(is_trade==1)),
            gender_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf15<-train_p4%>%
  group_by(user_occupation_id,item_sales_level)%>%
  summarize(occupation_sales_open_times=length(is_trade),
            occupation_sales_buy_times=length(which(is_trade==1)),
            occupation_sales_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf16<-train_p4 %>%
  group_by(user_age_level,item_id)%>%
  summarize(age_item_open_times=length(is_trade),
            age_item_buy_times=length(which(is_trade==1)),
            age_item_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf17<-train_p4 %>%
  group_by(user_gender_id,item_id)%>%
  summarize(gender_item__open_times=length(is_trade),
            gender_item__buy_times=length(which(is_trade==1)),
            gender_item__buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()
ucf18<-train_p4%>%
  group_by(user_occupation_id,item_id)%>%
  summarize(occupation_item_id_open_times=length(is_trade),
            occupation_item_id_buy_times=length(which(is_trade==1)),
            occupation_item_id_buy_prop=length(which(is_trade==1))/length(is_trade)
  )%>%
  data.frame()

#-----------------------user_class 的特征填充到test_p4中去
user_class_test_p4<-test_p4[,colnames(test_p4) %in% 
                              c("instance_id","shop_id","item_id","item_category_list","item_brand_id",
                                "item_sales_level","item_price_level","user_age_level","user_occupation_id",
                                "user_gender_id")]
user_class_feature_p4<-user_class_test_p4%>%
  left_join(ucf1,by=c("user_age_level"="user_age_level","shop_id"="shop_id"))%>%
  left_join(ucf2,by=c("user_gender_id"="user_gender_id","shop_id"="shop_id"))%>%
  left_join(ucf3,by=c("user_occupation_id"="user_occupation_id","shop_id"="shop_id"))%>%
  left_join(ucf4,by=c("user_age_level"="user_age_level","item_category_list"="item_category_list"))%>%
  left_join(ucf5,by=c("user_gender_id"="user_gender_id","item_category_list"="item_category_list"))%>%
  left_join(ucf6,by=c("user_occupation_id"="user_occupation_id","item_category_list"="item_category_list"))%>%
  left_join(ucf7,by=c("user_age_level"="user_age_level","item_brand_id"="item_brand_id"))%>%
  left_join(ucf8,by=c("user_gender_id"="user_gender_id","item_brand_id"="item_brand_id"))%>%
  left_join(ucf9,by=c("user_occupation_id"="user_occupation_id","item_brand_id"="item_brand_id"))%>%
  left_join(ucf10,by=c("user_age_level"="user_age_level","item_price_level"="item_price_level"))%>%
  left_join(ucf11,by=c("user_gender_id"="user_gender_id","item_price_level"="item_price_level"))%>%
  left_join(ucf12,by=c("user_occupation_id"="user_occupation_id","item_price_level"="item_price_level"))%>%
  left_join(ucf13,by=c("user_age_level"="user_age_level","item_sales_level"="item_sales_level"))%>%
  left_join(ucf14,by=c("user_gender_id"="user_gender_id","item_sales_level"="item_sales_level"))%>%
  left_join(ucf15,by=c("user_occupation_id"="user_occupation_id","item_sales_level"="item_sales_level"))%>%
  left_join(ucf16,by=c("user_age_level"="user_age_level","item_id"="item_id"))%>%
  left_join(ucf17,by=c("user_gender_id"="user_gender_id","item_id"="item_id"))%>%
  left_join(ucf18,by=c("user_occupation_id"="user_occupation_id","item_id"="item_id"))%>%
  data.frame()

#2 item_id feature
if1<-train_p4 %>%
  group_by(item_id) %>%
  summarize(item_buy_prop=length(which(is_trade==1))/length(is_trade),
            item_open_times=length(is_trade),
            item_buy_times=length(which(is_trade==1)),
            item_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            item_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            item_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            item_age_level=median(user_age_level), 
            item_star_level=median(user_star_level), 
            item_median_hour=median(time_hour)
  ) %>%
  data.frame()  

#2.1  item_brand_feature
if2<-train_p4 %>%
  group_by(item_brand_id) %>%
  summarize(brand_buy_prop=length(which(is_trade==1))/length(is_trade),
            brand_open_times=length(is_trade),
            brand_buy_times=length(which(is_trade==1)),
            brand_item_num=length(unique(item_id)),
            brand_shop_num=length(unique(shop_id)),
            brand_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            brand_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            brand_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            brand_age_level=median(user_age_level), 
            brand_star_level=median(user_star_level), 
            brand_median_hour=median(time_hour)
  ) %>%
  data.frame()  


#2.2 item_shop_feature 
if3<-train_p4 %>%
  group_by(shop_id) %>%
  summarize(shop_buy_prop=length(which(is_trade==1))/length(is_trade),
            shop_open_times=length(is_trade),
            shop_buy_times=length(which(is_trade==1)),
            shop_item_num=length(unique(item_id)),
            shop_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            shop_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            shop_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            shop_age_level=median(user_age_level), 
            shop_user_star_level=median(user_star_level), 
            shop_median_hour=median(time_hour)
  ) %>%
  data.frame()  

#2.3 item_category_feature
if4<-train_p4 %>%  
  group_by(item_category_list) %>%
  summarize(category_buy_prop=length(which(is_trade==1))/length(is_trade),
            category_open_times=length(is_trade),
            category_buy_times=length(which(is_trade==1)),
            category_item_num=length(unique(item_id)),
            category_sex_man_prop=length(is_trade[which(user_gender_id==1)])/length(is_trade),
            category_sex_woman_prop=length(is_trade[which(user_gender_id==0)])/length(is_trade),
            category_sex_family_prop=length(is_trade[which(user_gender_id==2)])/length(is_trade),
            category_age_level=median(user_age_level), 
            category_user_star_level=median(user_star_level), 
            category_median_hour=median(time_hour)
  ) %>%
  data.frame()

#-------------------------------2 将item的特征填充到test_p4中
item_test_p4<-test_p4[,colnames(test_p4) %in% 
                        c("instance_id","item_id","shop_id","item_category_list","item_brand_id")]
item_feature_p4<-item_test_p4 %>%
  left_join(if1,by=c("item_id"="item_id")) %>%
  left_join(if2,by=c("item_brand_id"="item_brand_id")) %>%
  left_join(if3,by=c("shop_id"="shop_id")) %>%
  left_join(if4,by=c("item_category_list"="item_category_list"))%>%
  mutate(item_shop_times_to_total_times_prop=item_open_times/shop_open_times,
         item_shop_buy_to_total_buy_prop=item_buy_times/shop_buy_times,
         item_brand_times_to_total_times_prop=item_open_times/brand_open_times,
         item_brand_buy_to_total_buy_prop=item_buy_times/brand_buy_times,
         item_category_times_to_total_times_prop=item_open_times/category_open_times,
         item_category_buy_to_total_buy_prop=item_buy_times/category_buy_times
  )%>%
  data.frame()


#------------------------其他的分类变量的特征
#3.1 城市特征
item_city<-train_p4 %>%
  group_by(item_city_id) %>%
  summarize(city_prop=length(which(is_trade==1))/length(is_trade),
            city_open_times=length(is_trade),
            city_buy_times=length(which(is_trade==1)),
            city_price_level=median(item_price_level),
            city_sales_level=median(item_sales_level),
            city_pv_level=median(item_pv_level)
  )%>%
  data.frame()

#3.2 价格等级特征
item_price<-train_p4 %>%
  group_by(item_price_level) %>%
  summarize(item_price_prop=length(which(is_trade==1))/length(is_trade),
            item_price_num=length(is_trade),
            item_price_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(item_price_prop))%>%
  data.frame()

#3.3 销量等级特征
item_sales<-train_p4 %>%
  group_by(item_sales_level) %>%
  summarize(item_sales_prop=length(which(is_trade==1))/length(is_trade),
            item_sales_num=length(is_trade),
            item_sales_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(item_sales_prop))%>%
  data.frame() 

#3.4 展示量等级特征
item_pv<-train_p4 %>%
  group_by(item_pv_level) %>%
  summarize(item_pv_prop=length(which(is_trade==1))/length(is_trade),
            item_pv_num=length(is_trade),
            item_pv_buy_times=length(is_trade)) %>%
  arrange(desc(item_pv_prop))%>%
  data.frame() 

#3.5 用户职业特征
user_occ<-train_p4 %>%
  group_by(user_occupation_id) %>%
  summarize(user_occ_prop=length(which(is_trade==1))/length(is_trade),
            user_occ_num=length(is_trade),
            user_occ_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(user_occ_prop))%>%
  data.frame() 

#3.6 用户年龄的购买率
user_age<-train_p4 %>%
  group_by(user_age_level) %>%
  summarize(user_age_prop=length(which(is_trade==1))/length(is_trade),
            user_age_tiems=length(is_trade),
            user_age_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(user_age_prop))%>%
  data.frame()

user_gender<-train_p4 %>%
  group_by(user_gender_id) %>%
  summarize(user_gender_prop=length(which(is_trade==1))/length(is_trade),
            user_gender_tiems=length(is_trade),
            user_gender_buy_times=length(which(is_trade==1))) %>%
  data.frame()

#3.7 用户等级购买率
user_star<-train_p4 %>%
  group_by(user_star_level) %>%
  summarize(user_star_prop=length(which(is_trade==1))/length(is_trade),user_star_num=length(is_trade)) %>%
  arrange(desc(user_star_prop))%>%
  data.frame()

#3.8 小时的特征
time_hour<-train_p4 %>%
  group_by(time_hour) %>%
  summarize(time_hour_buy_prop=length(which(is_trade==1))/length(is_trade),
            time_hour_open_times=length(is_trade),
            time_hour_buy_times=length(which(is_trade==1))) %>%
  data.frame()
#3.9 时间分组的特征
time_class<-train_p4 %>%
  group_by(time_class) %>%
  summarize(time_class_buy_prop=length(which(is_trade==1))/length(is_trade),
            time_class_open_times=length(is_trade),
            time_class_buy_times=length(which(is_trade==1))) %>%
  data.frame()

#3.10 评论数与购买率
shop_review<-train_p4 %>%
  group_by(shop_review_num_level) %>%
  summarize(shop_review_prop=length(which(is_trade==1))/length(is_trade),
            shop_review_times=length(is_trade),
            shop_review_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(shop_review_prop))%>%
  data.frame()


#3.11 商铺等级与购买率
shop_star<-train_p4 %>%
  group_by(shop_star_level) %>%
  summarize(shop_star_prop=length(which(is_trade==1))/length(is_trade),
            shop_star_times=length(is_trade),
            shop_star_buy_times=length(which(is_trade==1))) %>%
  arrange(desc(shop_star_prop)) %>%
  data.frame() 
# len_intersect<-function(x){
#   return(c(length(intersect(train_p3[,colnames(train_p3)%in%x],test_p3[,colnames(test_p3)%in%x])),           #检查训练集和特征集的id的交集情况
#            length(unique(test_p3[,colnames(test_p3)%in%x]))))
# }

#----------------3 将other_class_feature 填充到test_p4中
other_class_test_p4<-test_p4[,colnames(test_p4) %in% c("instance_id","item_city_id","item_pv_level","user_star_level","time_hour","time_class",
                                                       "item_sales_level","item_price_level","user_age_level","user_occupation_id",
                                                       "user_gender_id","shop_review_num_level","shop_star_level")]

other_class_feature_p4<-other_class_test_p4 %>%
  left_join(item_city,by=c('item_city_id'='item_city_id')) %>%  #城市的购买率和频数
  left_join(item_price,by=c('item_price_level'='item_price_level')) %>% #价格的购买率和频数
  left_join(item_sales,by=c('item_sales_level'='item_sales_level'))  %>%#销量的购买率和频数
  left_join(item_pv,by=c('item_pv_level'='item_pv_level')) %>% #展示量的购买率和频数
  #left_join(instance_property,by=c("instance_id","instance_id"))    %>%      #商品的属性的加权购买率
  left_join(user_occ,by=c('user_occupation_id'='user_occupation_id')) %>% #用户职业的购买率和频数
  left_join(user_age,by=c('user_age_level'='user_age_level'))  %>%#用户年龄的购买率和频数
  left_join(user_gender,by=c('user_gender_id'='user_gender_id'))  %>%#用户性别的购买率和频数
  left_join(user_star,by=c('user_star_level'='user_star_level'))  %>%#用户星级的购买率和频数
  left_join(time_hour,by=c('time_hour'='time_hour')) %>% #小时的购买率和频数
  left_join(time_class,by=c('time_class'='time_class')) %>% #时间段的购买率和频数
  left_join(shop_review,by=c('shop_review_num_level'='shop_review_num_level'))  %>%
  left_join(shop_star,by=c('shop_star_level'='shop_star_level'))  %>%
  data.frame()

#---------------------------------------------------------------------test4 提取label --------------------------
l1<-test_p4 %>%
  group_by(user_id) %>%
  summarize(user_open_last=max(time_seconds),
            user_open_first=min(time_seconds),
            user_open_times_today=length(user_id),
            user_shop_num=length(unique(shop_id)),
            user_item_num=length(unique(item_id))
  )%>%
  data.frame()
l2<-test_p4 %>%
  group_by(user_id,item_category_list)%>%
  summarize(user_category_open_last=max(time_seconds),
            user_category_open_first=min(time_seconds),
            user_category_times=length(user_id)
  )%>%
  data.frame()
l3<-test_p4%>%
  group_by(user_id,shop_id)%>%
  summarize(user_shop_open_last=max(time_seconds),
            user_shop_open_first=min(time_seconds),
            user_shop_times=length(user_id)
  )%>%
  data.frame()
l4<-test_p4%>%
  group_by(user_id,item_id)%>%
  summarize(user_item_open_last=max(time_seconds),
            user_item_open_first=min(time_seconds),
            user_item_times=length(user_id)
  )%>%
  data.frame()
l5<-test_p4%>%
  group_by(user_id,item_brand_id)%>%
  summarize(user_brand_open_last=max(time_seconds),
            user_brand_open_first=min(time_seconds),
            user_brand_times=length(user_id)
  )%>%
  data.frame()

is_firstlast<-function(x){
  if(x==0)
    return(1)
  else
    return(0)
}

test_feature_label_4<-test_p4%>%
  left_join(l1,by=c("user_id"="user_id"))%>%
  left_join(l2,by=c("user_id"="user_id","item_category_list"="item_category_list"))%>%
  left_join(l3,by=c("user_id"="user_id","shop_id"="shop_id"))%>%
  left_join(l4,by=c("user_id"="user_id","item_id"="item_id"))%>%
  left_join(l5,by=c("user_id"="user_id","item_brand_id"="item_brand_id")) %>%
  mutate(
    user_to_category_times_prop=user_open_times_today/user_category_times,
    user_to_shop_times_prop=user_open_times_today/user_shop_times,
    user_to_item_times_prop=user_open_times_today/user_item_times,
    user_to_brand_times_prop=user_open_times_today/user_brand_times,
    user_open_last_time_diff=difftime(user_open_last,time_seconds,units="secs"),
    user_open_first_time_diff=difftime(time_seconds,user_open_first,units="secs"),
    user_cat_open_last_time_diff=difftime(user_category_open_last,time_seconds,units="secs"),
    user_cat_open_first_time_diff=difftime(time_seconds,user_category_open_first,units="secs"),
    user_shop_open_last_time_diff=difftime(user_shop_open_last,time_seconds,units="secs"),
    user_shop_open_first_time_diff=difftime(time_seconds,user_shop_open_first,units="secs"),
    user_item_open_last_time_diff=difftime(user_item_open_last,time_seconds,units="secs"),
    user_item_open_first_time_diff=difftime(time_seconds,user_item_open_first,units="secs"),
    user_brand_open_last_time_diff=difftime(user_brand_open_last,time_seconds,units="secs"),
    user_brand_open_first_time_diff=difftime(time_seconds,user_brand_open_first,units="secs"),
    is_user_open_last_time=apply(matrix(user_open_last_time_diff),1,FUN=is_firstlast),
    is_user_open_first_time=apply(matrix(user_open_first_time_diff),1,FUN=is_firstlast),
    is_user_cat_open_last_time=apply(matrix(user_cat_open_last_time_diff),1,FUN=is_firstlast),
    is_user_cat_open_first_time=apply(matrix(user_cat_open_first_time_diff),1,FUN=is_firstlast),
    is_user_shop_open_last_time=apply(matrix(user_shop_open_last_time_diff),1,FUN=is_firstlast),
    is_user_shop_open_first_time=apply(matrix(user_shop_open_first_time_diff),1,FUN=is_firstlast),
    is_user_item_open_last_time=apply(matrix(user_item_open_last_time_diff),1,FUN=is_firstlast),
    is_user_item_open_first_time=apply(matrix(user_item_open_first_time_diff),1,FUN=is_firstlast),
    is_user_brand_open_last_time=apply(matrix(user_brand_open_last_time_diff),1,FUN=is_firstlast),
    is_user_brand_open_first_time=apply(matrix(user_brand_open_first_time_diff),1,FUN=is_firstlast)
  )%>%
  select(-c(user_open_last,user_open_first,user_category_open_last,user_category_open_first,user_shop_open_last,user_shop_open_first,
            user_item_open_last,user_item_open_first,user_brand_open_last,user_brand_open_first,user_open_last_time_diff,
            user_open_first_time_diff,user_brand_open_last_time_diff,user_brand_open_first_time_diff,
            user_cat_open_last_time_diff,user_cat_open_first_time_diff,user_shop_open_last_time_diff,user_shop_open_first_time_diff,
            user_item_open_last_time_diff,user_item_open_first_time_diff))  %>%
  cbind(user_feature_p4[,-c(1:6)],user_class_feature_p4[,-c(1:10)],item_feature_p4[,-c(1:5)],other_class_feature_p4[,-c(1:13)])
