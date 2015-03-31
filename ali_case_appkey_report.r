
## 案件表的每日预警可视化
library(RODPS)
library(ggplot2)
library(sqldf)

#加载config
rodps.init("D:\\ODPS\\conf\\odps_config.ini")
if (rodps.exist.table('security_eco.adl_case_appkey_report_day') == T) {
  sql <- "drop table if exists security_eco.adl_case_appkey_report_day_tmp;
  create table security_eco.adl_case_appkey_report_day_tmp as
  select * 
  from security_eco.adl_case_appkey_report_day
  where ds=concat(substr(dateadd(datetrunc(GETDATE(), 'DD'), -1, 'dd'), 1, 4),
  substr(dateadd(datetrunc(GETDATE(), 'DD'), -1, 'dd'), 6, 2),
  substr(dateadd(datetrunc(GETDATE(), 'DD'), -1, 'dd'), 9, 2)) 
  limit 9999999;"
  rodps.query(sql)
  case_data <- rodps.load.table('adl_case_appkey_report_day_tmp')   
  
} else {
  print("the table is not exist!")
}

# rm(case_data)
#order_data <- read.csv("putishu_cainiao_monitor_tsm_20140826.csv")
if (is.data.frame(case_data) == F)
  case_data <- as.data.frame(case_data)
case_data[,6] <- as.numeric(substr(case_data[,6] ,1 ,3))*.01

# rm(case_data_select)
base_date <- format(Sys.Date()-3, "%Y%m%d")
sql <- sprintf("
               select * from case_data
               where appkey in
                    (  select distinct appkey 
                       from case_data
                       where  caseo_ratio >= 0.1  and
                              event_date = %s 
                    ) and title <> '淘宝交易'
               order by appkey desc, event_date desc
               ", base_date)

case_data_select <- sqldf(sql)
#case_data_select[1:30,6]


## 案件占比15天走势
monitor_ts <- ggplot(case_data_select, aes(x = format(as.Date(as.character(case_data_select$event_date),"%Y%m%d"),"%Y-%m-%d")))
monitor_ts <- monitor_ts + geom_point(aes(y = caseo_ratio), size=3.5, shape=16)
monitor_ts <- monitor_ts + geom_path(aes(y = caseo_ratio, group = title, color = title), size= 1.3)
monitor_ts <- monitor_ts + geom_text(aes(x = format(as.Date(as.character(case_data_select$event_date),"%Y%m%d"),"%Y-%m-%d"),
                                       y = caseo_ratio, label = case_data_select[,6]),
                                     data = case_data_select[, 1:6], vjust = -1, size = 5)

monitor_ts <- monitor_ts + xlab("日期") + ylab("案件占比") + 
  labs(title = paste("案件占比较大appkey15天走势（基准时间:", base_date,"）") )
monitor_ts


## 卖家覆盖数15天走势
monitor_ts <- ggplot(case_data_select, aes(x = event_date ))
monitor_ts <- monitor_ts + geom_point(aes(y = total_seller_day), size=3.5, shape=16)
monitor_ts <- monitor_ts + geom_path(aes(y = total_seller_day, group = title, color = title), size= 1.3)
monitor_ts <- monitor_ts + geom_text(aes(x = event_date, y = total_seller_day, label = case_data_select[,7]),
                                     data = case_data_select[, 1:7], vjust = -1, size = 5)
monitor_ts <- monitor_ts + xlab("日期") + ylab("卖家数") + 
  labs(title = paste("案件占比较大appkey15天涉及卖家数走势（基准时间:", base_date,"）") )
monitor_ts


# 发邮件
#install.packages(c('base64enc','sendmailR'))
library(base64enc)
library(sendmailR)

from <- sprintf("<kemey@163.com>",  Sys.info()[4])
to <- "<lkm900808@163.com>"
subject <- "###主 题###"
body <- list("###内 容###")
sendmail(from, to, subject, body, control=list(smtpServer="mail.163.com"))




