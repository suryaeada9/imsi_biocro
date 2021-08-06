library(ggplot2)
library(dplyr)
library(patchwork)


#for sorghum graphs
setwd("C://Users/stark/OneDrive/Documents2021/biocro-dev/sorghum_stat_graphs_v3")

sorghum_stats <- read.csv("C:/Users/stark/OneDrive/Documents2021/biocro-dev/sorghum_stats.csv")

sorghum_stats['quality_stat'] <- 100 * (1-log(sorghum_stats[['Mahalanobis_normed']]))

sorghum_stats <- sorghum_stats %>% filter((grepl('all',training_set) & grepl('all',test_set)) | (grepl('final',training_set) & grepl('final',test_set)))

n <- nrow(sorghum_stats)

View(sorghum_stats)

training_or_test <- function(training_set,test_set){
  if(training_set == test_set){
    return('Training')
  }
  else{
    return('Validation')
  }
}

final_or_all <- function(training_set){
  if(grepl('all',training_set)){
    return("All Data")
  }
  else{
    return("Final Yield Only")
  }
}

for(i in 1:n){
  sorghum_stats[i,'training_test_same'] <- (training_or_test(sorghum_stats[i,'training_set'],sorghum_stats[i,'test_set']))
  sorghum_stats[i,'final_or_all'] <- (final_or_all(sorghum_stats[i,'training_set']))
}



#make a pdf file
pdf(
  file = "RSquared.pdf",
  width = 12,          # inches
  height = 12,         # inches
  useDingbats = FALSE # make sure symbols are rendered properly in the PDF
)

#graph to appear on the pdf 
graph_all <- ggplot(data=sorghum_stats,aes(x=training_test_same,y=RSquared)) +
  geom_point(aes(color=final_or_all,size=3,alpha=1/5)) +
  xlab("Data") + #x-axis label
  ylab("R-Squared") + #y-axis label
  labs(title = "R-Squared Sorghum") +
  theme(legend.position = "bottom") + #put legend at bottom 
  scale_color_manual(name = "Training Set",values = c("blue", "red")) +
  guides(size = FALSE, alpha = FALSE)

print(graph_all)

dev.off()


#for miscanthus graphs
setwd("C://Users/stark/OneDrive/Documents2021/biocro-dev/miscanthus_stat_graphs_v3")

misc_stats <- read.csv("C:/Users/stark/OneDrive/Documents2021/biocro-dev/miscanthus_stats.csv")

misc_stats <- misc_stats %>% filter((grepl('Peak',training_set) & grepl('Peak',test_set)) | (grepl('Only',training_set) & grepl('Only',test_set)))

n <- nrow(misc_stats)

final_or_all <- function(training_set){
  if(grepl('Peak',training_set)){
    return("Harvest and Peak")
  }
  else{
    return("Harvest Only")
  }
}

for(i in 1:n){
  misc_stats[i,'training_test_same'] <- (training_or_test(misc_stats[i,'training_set'],misc_stats[i,'test_set']))
  misc_stats[i,'final_or_all'] <- (final_or_all(misc_stats[i,'training_set']))
}

#make a pdf file
pdf(
  file = "ChiSquared.pdf",
  width = 12,          # inches
  height = 12,         # inches
  useDingbats = FALSE # make sure symbols are rendered properly in the PDF
)

#graph to appear on the pdf 
graph_all <- ggplot(data=misc_stats,aes(x=training_test_same,y=ChiSquared)) +
  geom_point(aes(color=final_or_all,size=3,alpha=1/5)) +
  xlab("Data") + #x-axis label
  ylab("Chi-Squared") + #y-axis label
  labs(title = "Chi-Squared Miscanthus") +
  theme(legend.position = "bottom") + #put legend at bottom 
  scale_color_manual(name = "Training Set",values = c("blue", "red")) +
  guides(size = FALSE, alpha = FALSE)

print(graph_all)

dev.off()


#make a pdf file
pdf(
  file = "MAE.pdf",
  width = 11,          # inches
  height = 30,         # inches
  useDingbats = FALSE # make sure symbols are rendered properly in the PDF
)

#graphs to appear on the pdf 
Leave_Out_Wageningen <- ggplot(data=misc_stats %>% filter(grepl("AdAbMPS",training_set) & grepl("Peak and Harvest",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Peak and Harvest Fit Leaving Out Wageningen") +
  scale_color_manual(name = "Test Set",values = c("blue", "red")) +
  theme(legend.position = "bottom") #put legend at bottom


H_Only_Leave_Out_Wageningen <- ggplot(data=misc_stats %>% filter(grepl("AdAbMPS",training_set) & grepl("Harvest Only",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Harvest Only Fit Leaving Out Wageningen") +
  scale_color_manual(name = "Test Set",values = c("blue", "red")) +
  theme(legend.position = "bottom") #put legend at bottom

Leave_Out_Stuttgart <- ggplot(data=misc_stats %>% filter(grepl("AdAbMPW",training_set) & grepl("Peak and Harvest",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Peak and Harvest Fit Leaving Out Stuttgart") +
  scale_color_manual(name = "Test Set",values = c("blue", "red")) +
  theme(legend.position = "bottom") #put legend at bottom


H_Only_Leave_Out_Stuttgart <- ggplot(data=misc_stats %>% filter(grepl("AdAbMPW",training_set) & grepl("Harvest Only",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Harvest Only Fit Leaving Out Stuttgart") +
  scale_color_manual(name = "Test Set",values = c("blue", "red")) +
  theme(legend.position = "bottom") #put legend at bottom

Leave_Out_Potash <- ggplot(data=misc_stats %>% filter(grepl("AdAbMSW",training_set) & grepl("Peak and Harvest",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Peak and Harvest Fit Leaving Out Potash") +
  scale_color_manual(name = "Test Set",values = c("blue", "red")) +
  theme(legend.position = "bottom") #put legend at bottom


H_Only_Leave_Out_Potash <- ggplot(data=misc_stats %>% filter(grepl("AdAbMSW",training_set) & grepl("Harvest Only",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Harvest Only Fit Leaving Out Potash") +
  scale_color_manual(name = "Test Set",values = c("blue", "red")) +
  theme(legend.position = "bottom") #put legend at bottom

Leave_Out_Moscow <- ggplot(data=misc_stats %>% filter(grepl("AdAbPSW",training_set) & grepl("Peak and Harvest",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Peak and Harvest Fit Leaving Out Moscow") +
  scale_color_manual(name = "Test Set",values = c("blue", "red")) +
  theme(legend.position = "bottom") #put legend at bottom


H_Only_Leave_Out_Moscow <- ggplot(data=misc_stats %>% filter(grepl("AdAbPSW",training_set) & grepl("Harvest Only",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Harvest Only Fit Leaving Out Moscow") +
  scale_color_manual(name = "Test Set",values = c("blue", "red")) +
  theme(legend.position = "bottom") #put legend at bottom

Leave_Out_Aberystwyth <- ggplot(data=misc_stats %>% filter(grepl("AdMPSW",training_set) & grepl("Peak and Harvest",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Peak and Harvest Fit Leaving Out Aberystwyth") +
  scale_color_manual(name = "Test Set",values = c("red", "blue")) +
  theme(legend.position = "bottom") #put legend at bottom


H_Only_Leave_Out_Aberystwyth <- ggplot(data=misc_stats %>% filter(grepl("AdMPSW",training_set) & grepl("Harvest Only",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Harvest Only Fit Leaving Out Aberystwyth") +
  scale_color_manual(name = "Test Set",values = c("red", "blue")) +
  theme(legend.position = "bottom") #put legend at bottom

Leave_Out_Adana <- ggplot(data=misc_stats %>% filter(grepl("AbMPSW",training_set) & grepl("Peak and Harvest",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Peak and Harvest Fit Leaving Out Adana") +
  scale_color_manual(name = "Test Set",values = c("blue", "red")) +
  theme(legend.position = "bottom") #put legend at bottom


H_Only_Leave_Out_Adana <- ggplot(data=misc_stats %>% filter(grepl("AbMPSW",training_set) & grepl("Harvest Only",test_set)), aes(x=factor(training_set,levels = rev(levels(factor(training_set)))),y=MAE, group=test_set)) +
  geom_line(aes(color=test_set))+
  geom_point() +
  xlab("Training Set") + #x-axis label
  ylab("MAE") + #y-axis label
  labs(title = "MAE Harvest Only Fit Leaving Out Adana") +
  scale_color_manual(name = "Test Set",values = c("blue", "red")) +
  theme(legend.position = "bottom") #put legend at bottom


#prints all the graphs on one page
print((Leave_Out_Adana + H_Only_Leave_Out_Adana) / (Leave_Out_Aberystwyth + H_Only_Leave_Out_Aberystwyth) / (Leave_Out_Moscow + H_Only_Leave_Out_Moscow)/ (Leave_Out_Potash + H_Only_Leave_Out_Potash) / (Leave_Out_Stuttgart + H_Only_Leave_Out_Stuttgart)/ (Leave_Out_Wageningen + H_Only_Leave_Out_Wageningen))

dev.off()

