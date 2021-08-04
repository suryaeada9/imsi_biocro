library(ggplot2)
library(dplyr)
library(patchwork)


#for sorghum graphs
setwd("C://Users/stark/OneDrive/Documents2021/biocro-dev/sorghum_stat_graphs")

sorghum_stats <- read.csv("C:/Users/stark/OneDrive/Documents2021/biocro-dev/sorghum_stats.csv")

sorghum_stats['quality_stat'] <- 100 * (1-sorghum_stats[['Mahalanobis_normed']] / 4)


#for miscanthus graphs
setwd("C://Users/stark/OneDrive/Documents2021/biocro-dev/miscanthus_stat_graphs")

misc_stats <- read.csv("C:/Users/stark/OneDrive/Documents2021/biocro-dev/miscanthus_stats.csv")


#make a pdf file
pdf(
  file = "MAE.pdf",
  width = 11,          # inches
  height = 30,         # inches
  useDingbats = FALSE # make sure symbols are rendered properly in the PDF
)

#graphs to appear on the pdf - need to change the filters and the titles when switching between miscanthus and sorghum
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

