library(ggplot2)
library(caret)
library(car)
library(caret)
#library(rattle)
library(LEAP)
library(tidyverse)
library(cowplot)
library(modelr)
options(na.action = na.warn)

body <- read.csv("BodyFat.csv",header = T)
body1 <- body
body <- body[c(-42,-39,-221,-86),]
body_plot <- body
body_plot$ABDOMEN <- body_plot$ABDOMEN %>% scale() %>% +2 %>% round() %>% factor()
####plot1 age, bodyfat with abdomen and adiposity
ggplot(data = body_plot,mapping = aes(x = AGE, y = BODYFAT)) + 
  geom_point(mapping = aes(color = ABDOMEN, size = ADIPOSITY)) +
  geom_smooth() +
  theme_bw() +
  scale_y_continuous(name = "Body fat percentage") +
  scale_x_continuous(name = "Age") +
  ggtitle("Body fat percentage generally increases with age") +
  theme(panel.grid.major = element_blank(), #bold labels and title
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 9),
        axis.text.y = element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 47.7)
ggsave("bodyfat_age.png", width=22, height=15, unit="cm", dpi=500)

####summary of relationships between each predictor and bodyfat 
g1 <-ggplot(data = body, aes(WEIGHT, BODYFAT)) + 
       geom_point(size = 1.5) +
       geom_smooth()    #No splines relations showing between bodyfat and other variables except age.
g2 <-ggplot(data = body, aes(HEIGHT, BODYFAT)) + 
       geom_point(size = 1.5) +
       geom_smooth()
g3 <-ggplot(data = body, aes(NECK, BODYFAT)) + 
       geom_point(size = 1.5) +
       geom_smooth()
g4 <-ggplot(data = body, aes(CHEST, BODYFAT)) + 
       geom_point(size = 1.5) +
       geom_smooth()
g5 <-ggplot(data = body, aes(ABDOMEN, BODYFAT)) + 
       geom_point(size = 1.5) +
       geom_smooth()
g6 <-ggplot(data = body, aes(HIP, BODYFAT)) + 
       geom_point(size = 1.5) +
       geom_smooth()
g7 <-ggplot(data = body, aes(THIGH, BODYFAT)) + 
       geom_point(size = 1.5) +
       geom_smooth()
g8 <-ggplot(data = body, aes(KNEE, BODYFAT)) + 
       geom_point(size = 1.5) +
       geom_smooth()
g9 <-ggplot(data = body, aes(ANKLE, BODYFAT)) + 
       geom_point(size = 1.5) +
       geom_smooth()
g10 <-ggplot(data = body, aes(BICEPS, BODYFAT)) + 
        geom_point(size = 1.5) +
        geom_smooth()
g11 <-ggplot(data = body, aes(FOREARM, BODYFAT)) + 
        geom_point(size = 1.5) +
        geom_smooth()
g12 <-ggplot(data = body, aes(WRIST, BODYFAT)) + 
        geom_point(size = 1.5) +
        geom_smooth()
plot_grid(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12,
          labels = "AUTO", align = "h")
ggsave("bodyfat_everyone.png", width = 25, height = 15, units = "cm")

###plot2 summary of bodyfat after data clean-up
ggplot(data = body_plot, mapping = aes(x = ABDOMEN, y = BODYFAT)) + 
  geom_boxplot() + 
  theme_bw() +
  scale_y_continuous(name = "Body fat percentage",
                     breaks = seq(0, 45, 5),
                     limits=c(0, 45)) +
  scale_x_discrete(name = "Abdomen") +
  ggtitle("Boxplot of body fat by level of abdomen") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), #bold labels and title
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) #title to center
ggsave("bodyfat_summary.png", width = 20, height = 15, units = "cm") 

###plot3  hist of body fat, abdomen, weight, height, wrist, and ankle
#ggplot(data = body_plot, mapping = aes(BODYFAT)) +
#  geom_histogram()

hist_bodyfat <-qplot(body1$BODYFAT,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Body Fat", 
      xlab = "Body fat",  
      fill=I("blue"), 
      col=I("black"), 
      alpha=I(.2),
      xlim=c(0,50)) +
  scale_y_continuous(name = "Frequency",
                     breaks = seq(0, 10, 2),
                     limits=c(0, 10)) +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), #bold labels and title
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 7),
        axis.text.y = element_text(colour="black", size = 7),
        axis.line = element_line(size=0.5, colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) #title to center
#ggsave("bodyfat_hist.png", width = 20, height = 15, units = "cm")
hist_weight <-qplot(body1$WEIGHT,
                     geom="histogram",
                     binwidth = 0.5,  
                     main = "Histogram for Weight", 
                     xlab = "Weight",  
                     fill=I("blue"), 
                     col=I("black"), 
                     alpha=I(.2),
                     xlim=c(110,370)) +
  scale_y_continuous(name = "Frequency",
                     breaks = seq(0, 6, 2),
                     limits=c(0, 6)) +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), #bold labels and title
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 7),
        axis.text.y = element_text(colour="black", size = 7),
        axis.line = element_line(size=0.5, colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) #title to center

hist_abdomen <-qplot(body1$ABDOMEN,
                     geom="histogram",
                     binwidth = 0.5,  
                     main = "Histogram for Abdomen", 
                     xlab = "Abdomen",  
                     fill=I("blue"), 
                     col=I("black"), 
                     alpha=I(.2),
                     xlim=c(65,150)) +
  scale_y_continuous(name = "Frequency",
                     breaks = seq(0, 10, 2),
                     limits=c(0, 10)) +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), #bold labels and title
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 7),
        axis.text.y = element_text(colour="black", size = 7),
        axis.line = element_line(size=0.5, colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) #title to center

hist_ankle <-qplot(body1$ANKLE,
                     geom="histogram",
                     binwidth = 0.5,  
                     main = "Histogram for Ankle", 
                     xlab = "Ankle",  
                     fill=I("blue"), 
                     col=I("black"), 
                     alpha=I(.2),
                     xlim=c(18,38)) +
  scale_y_continuous(name = "Frequency",
                     breaks = seq(0, 50, 10),
                     limits=c(0, 50)) +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), #bold labels and title
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 7),
        axis.text.y = element_text(colour="black", size = 7),
        axis.line = element_line(size=0.5, colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) #title to center

hist_wrist <-qplot(body1$WRIST,
                   geom="histogram",
                   binwidth = 0.5,  
                   main = "Histogram for Wrist", 
                   xlab = "Wrist",  
                   fill=I("blue"), 
                   col=I("black"), 
                   alpha=I(.2),
                   xlim=c(15,22)) +
  scale_y_continuous(name = "Frequency",
                     breaks = seq(0, 60, 10),
                     limits=c(0, 60)) +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), #bold labels and title
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 7),
        axis.text.y = element_text(colour="black", size = 7),
        axis.line = element_line(size=0.5, colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) #title to center

hist_height <-qplot(body1$HEIGHT,
                   geom="histogram",
                   binwidth = 0.5,  
                   main = "Histogram for Height", 
                   xlab = "Height",  
                   fill=I("blue"), 
                   col=I("black"), 
                   alpha=I(.2),
                   xlim=c(28,80)) +
  scale_y_continuous(name = "Frequency",
                     breaks = seq(0, 22, 5),
                     limits=c(0, 22)) +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), #bold labels and title
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 7),
        axis.text.y = element_text(colour="black", size = 7),
        axis.line = element_line(size=0.5, colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) #title to center

plot_grid(hist_bodyfat,hist_height,hist_weight,hist_abdomen,hist_wrist,hist_ankle,
          labels = "AUTO", align = "h")
ggsave("Histfor6Var.png", width = 25, height = 15, units = "cm")

####plot4 old people by abdomen and weight
##########
attach(body)
body_old <- body[AGE > 45,]

#plot4.1
ggplot(data = body_old,mapping = aes(x = WEIGHT, y = BODYFAT)) + 
  geom_point(mapping = aes(size = ABDOMEN)) +
  geom_smooth() +
  theme_bw() +
  scale_y_continuous(name = "Body fat percentage",
                     breaks = seq(0, 50, 5),
                     limits=c(0, 50)) +
  scale_x_continuous(name = "Weight") +
  ggtitle("Old perple body fat percentage only affacted by \nWeight and Abdomen") +
  theme(panel.grid.major = element_blank(), #bold labels and title
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("old_model.png", width = 22, height = 15, units = "cm")

#plot4.2 young people, BODYFAT~WEIGHT+ABDOMEN+WRIST+BICEPS+ANKLE
body_young <- body[AGE <= 45,]

g4.1 <-ggplot(data = body_young, aes(WEIGHT, BODYFAT)) + 
         geom_point(size = 1.5) +
         geom_smooth(span = 1.2)
g4.2 <-ggplot(data = body_young, aes(ABDOMEN, BODYFAT)) + 
         geom_point(size = 1.5) +
         geom_smooth(span = 1.2)
g4.3 <-ggplot(data = body_young, aes(WRIST, BODYFAT)) + 
         geom_point(size = 1.5) +
         geom_smooth(span = 1.2)
g4.4 <-ggplot(data = body_young, aes(BICEPS, BODYFAT)) + 
         geom_point(size = 1.5) +
         geom_smooth(span = 1.2)
g4.5 <-ggplot(data = body_young, aes(ANKLE, BODYFAT)) + 
         geom_point(size = 1.5) +
         geom_smooth(span = 1.2)

plot_grid(g4.1, g4.2, g4.3, g4.4, g4.5,
          labels = "AUTO", align = "h")
ggsave("young_model.png", width = 30, height = 20, units = "cm")

#source("final_code.R")
####plot5 diagnostic
#5.1 under 45
body_45 <- body_45 %>% add_residuals(under_45_final) %>% add_predictions(under_45_final)
par(mfrow = c(2,2))
plot(under_45_final)
dev.off()
#residual and fitted value.
ggplot(data = body_45, mapping = aes(pred, resid)) +
  geom_point(size = 1.5) +
  geom_smooth(se = F,span = 1.3) +
  scale_y_continuous(name = "Residuals") +
  scale_x_continuous(name = "Fitted values") +
  ggtitle("Residuals vs Fitted Values") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), #bold labels and title
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 9),
        axis.text.y = element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("under45_residual.png", width = 25, height = 15,units = "cm")
#qqplot
png("qqplot_under45.png",width = 800, height = 670)
qqnorm(rstandard(under_45_final),pch=20,bg="black",cex=1.2)
abline(a=0,b=1,col="red",lwd=3)
dev.off()
#other diagnostic
##normality good
ncvTest(under_45_final)##homoskedasticity
durbinWatsonTest(under_45_final)##independence
crPlots(under_45_final)##linearity

#5.2 beyond 45
body45_up <- body45_up[(body45_up$IDNO!=39&body45_up$IDNO!=86&body45_up$IDNO!=221),c(-1,-3)] %>% 
  add_residuals(up_45_final) %>% 
  add_predictions(up_45_final)
#residual and fitted value.
ggplot(data = body45_up, mapping = aes(pred, resid)) +
  geom_point(size = 1.5) +
  geom_smooth(se = F,span = 1000) +
  scale_y_continuous(name = "Residuals") +
  scale_x_continuous(name = "Fitted values") +
  ggtitle("Residuals vs Fitted Values") +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"), #bold labels and title
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 9),
        axis.text.y = element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("up45_residual.png", width = 25, height = 15,units = "cm")
#qqplot
png("qqplot_up45.png",width = 800, height = 670)
qqnorm(rstandard(up_45_final),pch=20,bg="black",cex=1.2)
abline(a=0,b=1,col="red",lwd=3)
dev.off()
#other diagnostic
##normality good
ncvTest(up_45_final)##homoskedasticity
durbinWatsonTest(up_45_final)##independence
crPlots(up_45_final)##linearity

