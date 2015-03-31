library(ggplot2)
library(scales)

weibo_data <- read.csv("kemey_weibo_data.csv")

# names(weibo_data)  "用户名" "屏幕名" "消息内容" "来源" "转发数" "评论数" "发布时间" "X.用户"
repost <- table(weibo_data$"转发数")   
response <- table(weibo_data$"评论数")
send_tools <- table(weibo_data$"来源")

 ## 发布微博数时序分析
send_time <- as.data.frame(weibo_data$"发布时间")
sep_time <- strsplit(t(send_time), " " )            # seprate time from date
date <- as.Date(t(as.data.frame(sep_time)[1,]))
time <- as.Date(t(as.data.frame(sep_time)[2,]))


new_time <- unlist(strsplit(time,":"))[c(seq(from = 1, to = length(time), by=2))] 
time_hour <- array(0,dim=length(time))

for (i in 1:length(time)){
    time_hour[i] <- strsplit(time,":")[[i]][1]
  
}


time[i] <- paste("",j,"点-",j+1,"点",sep = "")
  
strsplit(wdata$"发布时间","")


## 每日发布微博频数 
date_freq <- as.data.frame(table(date))
plot_date_freq <- ggplot(data = date_freq, aes(as.Date(date_freq$date), date_freq$Freq, 
                                               color = date_freq$date)) +
                 theme(legend.position = "none") + geom_bar(stat="identity") +
                 geom_smooth(data = date_freq, aes(group = 1), binwidth = 3, colour="black") +
                 labs(x = "日期", y = "微博数",size=2, title = "Kemey.RU发布微博频数") 
plot_date_freq <- plot_date_freq + theme(axis.text.x = element_text(angle=270, vjust=0.5))  
# plot_date_freq <- plot_date_freq + theme(axis.text.x=element_blank(), axis.ticks=element_blank()) 
plot_date_freq + scale_x_date(labels = date_format("20%y.%m"), breaks = "months")


 ## 微博被转发/评论数分析 √
library(RColorBrewer)

opar <- par(mfrow=c(2,1))

 ## 被转发数可视化
repost <- as.data.frame(repost)
if(is.matrix(repost) != TRUE)
repost <- as.matrix(repost)
repost_data <- as.numeric(repost[,2])
#repost <- repost[order(repost[,2]),]
pielabels <- sprintf("%s次占%2.1f%s", repost[,1],
                     100*repost_data/sum(repost_data), "%")	  
pie(repost_data, labels = pielabels, clockwise = TRUE, radius = 1, col = brewer.pal(5, "Set1"),
    border = "white", main = "Kemey.RU微博被转发数分布" )
text(c(.1, -0.35, -0.1, 0), c(-0.3, 0.4, 0.6, 0.85), 
     c(repost_data[1], repost_data[2], repost_data[3], repost_data[4]))

# repost 玫瑰图 
  pie_repost <- ggplot(data = repost, aes(repost[,1], repost[,2], fill = factor(repost[,1]))) +
              geom_bar(width = 1)
  pie_repost <- pie_repost + coord_polar(theta = "y")
# theme(axis.text.x = element_text(angle=270, vjust=0.5))  
# plot_date_freq <- plot_date_freq + theme(axis.text.x=element_blank(), axis.ticks=element_blank()) 
  pie_repost + scale_colour_hue(name = "被转发次数", breaks = c('A','B',"c","d"), 
                                labels = c("0次", "1次", "2次", "3次"))


## 被评论数可视化 
response <- as.data.frame(response)
plot_response <- ggplot(data = response, aes(response[, 1], response[, 2], 
                                             color = length(response[, 2]))) + 
                 geom_point(color = palette(rainbow(length(response[, 2]))), cex = 8) 
plot_response <- plot_response + geom_text(aes(response[, 1], response[, 2]+10, label = response[, 2]),
                                           data = response, vjust = -.5, cex = 5) +
                 xlab("被评论数") + ylab("微博数") + labs(title = "Kemey.RU微博被评论数气泡图")  
plot_response + theme(legend.position = "none")
par(opar)





