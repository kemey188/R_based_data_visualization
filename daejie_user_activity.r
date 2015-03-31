##活跃度人数可视化
library(RColorBrewer)

#activity <- read.csv("activity.csv")
activity <- read.table("activity.txt", header=TRUE)
level <- activity$activity
#freq <- table(level)
level0 <- length(level[level == 0])                        ##半年内无登陆记录的人数
level1 <- length(level[level == 1])                        ##一个月内有登陆记录的人数
level3 <- length(level[level == 3 | level == 1])           ##三个月内有登陆记录的人数
level6 <- length(level[level != 0])                        ##半年内有登陆记录的人数
h <- length(level)/50                      

freq <- data.frame("半年内无登陆" = level0, "一个月内登陆" = level1,
                   "三个月内登陆" = level3,     "半年内登陆" = level6)
if(is.matrix(freq) == F)
   freq <- as.matrix(freq)
barplot(freq, col = brewer.pal(4, "Set1"), beside = TRUE, main = "DaJie网用户各活跃度人数", 
        ylim = c(0, round(length(level), -6)), xlab = "活跃度级别", ylab = "人数")
grid(NA, round(length(level), -6)/1000000, lwd = 1, col = "grey")
text(c(seq(1.5, 9, by = 2)), c(freq[1,1] + h, freq[1, 2] + h, freq[1, 3] + h, freq[1, 4] + h),
     c(freq[1, 1], freq[1, 2], freq[1, 3], freq[1, 4]), col = brewer.pal(4, "Set1"))		

##活跃度等级比重可视化

dj_each_freq <- data.frame(x = c("半年内无登陆", "一个月内有登陆", 
                                 "一个月内无登陆但三个月内有登陆", "三个月内无登陆但六个月内有登陆"),
						   y = c(level0, level1, level3 - level1, level6 - level3))
if(is.matrix(dj_each_freq) != TRUE)
   dj_each_freq <- as.matrix(dj_each_freq)
djFreqData <- as.numeric(dj_each_freq[,2])
#dj_each_freq <- dj_each_freq[order(dj_each_freq[,2]),]
pielabels <- sprintf("%s = %2.1f%s", dj_each_freq[,1],
                     100*djFreqData/sum(djFreqData), "%")	
pie(djFreqData, labels = pielabels, clockwise = TRUE, radius = 1, col = brewer.pal(4, "Set1"),
    border = "white", main = "DaJie网用户活跃度等级占比" )
text(c(.5, -0.6, -0.5, -0.15), c(-0.2, -0.3, 0.4, 0.7), c(djFreqData[1], djFreqData[2], djFreqData[3], djFreqData[4]))
