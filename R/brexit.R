# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# BAYESIAN MODEL OF BREXIT POLLS  
# 19 Jun 2016
# 
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# ***********************************************************************************
# NOTES
# 
#
# todo:
#   (x) Scrape data from https://ig.ft.com/sites/brexit-polling/
#   (x) Provide state space underpinning for time aggregation
#   (x) Prediction for the 23 June
# 
# ***********************************************************************************


# Libraries & file location
# ---------------------------------------------------------------------------
pacs <-c("readr","ggplot2","dplyr","tidyr","stringr","lubridate",
         "rvest","rstan","shinystan","scales")
sapply(pacs, require, character=TRUE)

dir <- "data/"


# load data & tidy up
# ---------------------------------------------------------------------------

# brexit <- read_csv(paste0(dir,"ftBrexitPolls.csv"),na="-",
#                    col_names=c("stay","leave","undecided","date",
#                                "pollster","sample"),skip=1)

# scrape the data
ft_data <- read_html("https://ig.ft.com/sites/brexit-polling/") %>% 
  html_table()
result <- c(48.1,51.9,0,"June 23, 2016","election",33551983)

names_num <- c("stay","leave","undecided","sample")
brexit <- ft_data[[1]] %>% 
  rbind(result) %>% 
  setNames(c("stay","leave","undecided","date","pollster","sample")) %>% 
  mutate(date = mdy(str_replace(date,",","")),
         sample = ifelse(sample=="-", 1000, 
                         str_replace(sample,",",""))) %>% 
  mutate_each_(funs(as.numeric),names_num) %>% 
  mutate(lead = stay-leave,
         y_in = round(stay*sample/100,0),
         y_out = round(leave*sample/100,0),
         y_dunno = round(undecided*sample/100,0),
         pollster = ifelse(pollster=="Comres","ComRes",pollster),
         pollster = ifelse(pollster=="Yougov","YouGov",pollster),
         pollster = as.factor(pollster),
         day=date-min(date) + 1,
         countdown = dmy("23-06-2016")-date) %>% 
  arrange(date)

drop <- c("y_in","y_out","y_dunno")
brexit.df <- brexit %>% 
  select(-one_of(drop)) %>% 
  gather(vote,share,-date,-pollster,-day,-countdown,-sample) %>% 
  mutate(share = share/100,
         var = ifelse(vote %in% c("stay","leave","undecided"),
                                   (share*(1-share)/sample),NA)) %>% 
  gather(variable,value,-vote,-date,-pollster,-day,-countdown,-sample) %>% 
  unite(vote_r,vote,variable) %>% 
  spread(vote_r,value) %>% 
  mutate(lead_var=stay_var+leave_var)


# graph
# ---------------------------------------------------------------------------

brexit.df %>% ggplot(aes(y=lead_share,x=date)) +
  geom_hline(yintercept = 0)  + geom_vline(xintercept=as.numeric(as.Date("2016-06-23")))+
  geom_point(shape=21,colour="white",fill="#0072B2",aes(size=1/lead_var^0.5),alpha=0.7) +
  geom_smooth(span=0.3,colour="tomato") +
  scale_x_date(date_labels = "%b %y",limits=c(dmy("01/01/2014"),NA),
               date_breaks = "3 months","") +
  scale_y_continuous(limits=c(-0.25,0.3),"Remain Lead, %",labels=percent) +
  labs(size="Precision")+
  ggtitle("BREXIT - Remain Lead") +
  theme (text = element_text(size = 13),
    plot.title=element_text(face="bold", size=, vjust=1.25,hjust = 0))
ggsave("images/raw_polls.png", width = 8, height = 5)


# bayesian model - simple normal of lead
# ---------------------------------------------------------------------------

df <- brexit.df %>% 
  # filter(date <= dmy("13 June 2016")) %>% 
  mutate(pollster = as.factor(pollster))


data_list <- list(n_polls = length(df$date),
                  n_span =  as.numeric(max(df$day)),
                  n_predict = as.numeric(dmy("24 June 2016")-min(df$date)+1),
                  n_pred = as.numeric(dmy("24 June 2016")-max(df$date)),
                  n_houses = length(unique(as.numeric(df$pollster))),
                  y = df$lead_share,
                  sampleSize = df$sample,
                  sampleSigma = (df$lead_var)^0.5,
                  house = as.numeric(df$pollster),
                  countdown = as.numeric(df$countdown),
                  day = as.numeric(df$day))

sm <- stan_model("R/brexit_trend.stan")
brexit_mod <- sampling(sm, data=data_list,
                       iter=2000,chains=4,cores=2,
                       verbose=TRUE)

# --- model summaries
# rethinking::precis(brexit_mod,depth=2)
post <- rethinking::extract.samples(brexit_mod)

# library(shinystan)
# my_sso <- launch_shinystan(brexit_mod)

# --- Plot historical underlying voting intention
quants <- c(0.005,  0.025,  0.10,  0.25,  0.50, 0.75,  0.90, 0.975,  0.995)
voting_intention <- plyr::adply(post$hidden_voting_intention,2,quantile,probs=quants) %>% 
  setNames(c("period",'l005', 'l025', 'l10', 'l25' ,'median', 'u75', 'u90', 'u975', 'u995')) 

voting_intention %>%
  mutate(date = seq(min(df$date),max(df$date),by='1 day')) %>% 
  left_join(df) %>% 
  ggplot(aes(x=date,y=median)) +
  geom_hline(yintercept = 0)  + geom_vline(xintercept=as.numeric(as.Date("2016-06-23"))) +
  geom_point(aes(x=date,y=lead_share,size=(1/lead_var^0.5)),shape=21,colour="white",fill="#0072B2",alpha=0.7)+
  geom_ribbon(aes(ymin = l005, ymax = u995), fill = "tomato",alpha=0.1) +
  geom_ribbon(aes(ymin = l10, ymax = u90), fill = "tomato",alpha=0.2) +
  geom_ribbon(aes(ymin = l25, ymax = u75), fill = "tomato",alpha=0.4) +
  geom_line(colour="tomato",size=1.2) +
  labs(size="Precision") +
  scale_x_date(date_labels = "%b %y",limits=c(dmy("01/01/2014"),NA),
               date_breaks = "3 months","") +
  scale_y_continuous(limits=c(-0.25,0.3),"Remain Lead, %",labels=percent) +
  ggtitle("REMAIN LEAD - Bayesian State Space Model") +
  theme (text = element_text(size = 13),
         plot.title=element_text(face="bold", size=, vjust=1.25,hjust = 0))
# ggsave("images/bayesian_ss.png", width = 8, height = 5)


# --- Plot predict vote
quants <- c(0.005,  0.025,  0.10,  0.25,  0.50, 0.75,  0.90, 0.975,  0.995)
brexit_predict <- plyr::adply(post$vote_predict,2,quantile,probs=quants) %>% 
  setNames(c("period",'l005', 'l025', 'l10', 'l25' ,'median', 'u75', 'u90', 'u975', 'u995')) %>%
  mutate(date = seq(max(df$date)+1,dmy("24 June 2016"),by='1 day'))


voting_intention %>%
  mutate(date = seq(min(df$date),max(df$date),by='1 day')) %>% 
  left_join(df) %>% 
  ggplot(aes(x=date,y=median)) +
  geom_hline(yintercept = 0)  + 
  geom_vline(xintercept=as.numeric(as.Date("2016-06-23"))) +
  geom_point(aes(x=date,y=lead_share,size=(1/lead_var^0.5)),shape=21,colour="white",fill="#0072B2",alpha=0.6) +
  geom_ribbon(aes(ymin = l005, ymax = u995), fill = "tomato",alpha=0.1) +
  geom_ribbon(aes(ymin = l10, ymax = u90), fill = "tomato",alpha=0.2) +
  geom_ribbon(aes(ymin = l25, ymax = u75), fill = "tomato",alpha=0.4) +
  geom_line(colour="tomato",size=1.3) +
  geom_ribbon(data=brexit_predict,aes(x=date,ymin = l005, ymax = u995), fill = "#2ca25f",alpha=0.1) +
  geom_ribbon(data=brexit_predict,aes(x=date,ymin = l10, ymax = u90), fill = "#2ca25f",alpha=0.2) +
  geom_ribbon(data=brexit_predict,aes(x=date,ymin = l25, ymax = u75), fill = "#2ca25f",alpha=0.4) +
  geom_line(data=brexit_predict,aes(x=date,y=median), colour="#2ca25f",size=1.3) +
  scale_x_date(date_labels = "%b %y",limits=c(dmy("01/01/2016"),NA),
               date_breaks = "1 months","") +
  scale_y_continuous(limits=c(-0.3,0.3),"Remain Lead, %",labels=percent) +
  ggtitle("Remain Lead - Prediction") +
  theme (text = element_text(size = 13),
         legend.position = "none",
         plot.title=element_text(face="bold", size=, vjust=1.25,hjust = 0))
# ggsave("images/bayesian_prediction.png", width = 8, height = 5)


# --- brexit prediction
data_frame(lead_predict = post$vote_predict[ ,data_list$n_pred]) %>% 
  ggplot(aes(lead_predict)) + geom_density(fill="tomato",alpha=0.7,size=0.1) +
  geom_vline(xintercept = 0) +
  scale_x_continuous("Remain Lead, %",labels=percent) +
  ggtitle("BREXIT - Remain Lead on 23 June") +
  theme (text = element_text(size = 13),
         plot.title=element_text(face="bold", size=, vjust=1.25,hjust = 0))
# ggsave("images/remain_probability.png", width = 8, height = 5)

election <- mean(post$vote_predict[ ,data_list$n_pred]>0)

median(post$vote_predict[ ,data_list$n_pred])

# --- house effects

house_effect <- plyr::adply(post$houseEffect ,2,quantile,probs=quants) %>% 
  setNames(c("pollster",'l005', 'l025', 'l10', 'l25' ,'median', 'u75', 'u90', 'u975', 'u995')) 

house_effect %>%
  mutate(pollster = factor(pollster,labels=levels(df$pollster))) %>% 
  ggplot(aes(x=pollster,y=median)) + coord_flip() +
  geom_hline(yintercept = 0) +
  geom_segment(aes(y = l005, yend = u995, x= pollster,xend=pollster), size = 10,colour = "#0072B2",alpha=0.2) +
  geom_segment(aes(y = l10, yend = u90, x= pollster,xend=pollster), size = 10,colour = "#0072B2",alpha=0.3) +
  geom_segment(aes(y = l25, yend = u75, x= pollster,xend=pollster), size = 10,,colour = "#0072B2",alpha=0.4) +
  geom_point(colour="white", fill = "#0072B2",size=3,shape=21) +
  scale_y_continuous("Remain Lead, %",labels=percent) +
  xlab("")+
  ggtitle("Pollster 'bias'") +
  theme (text = element_text(size = 13),
         plot.title=element_text(face="bold", size=, vjust=1.25,hjust = 0))
# ggsave("images/pollster_bias.png", width = 8, height = 5)
                

# # bayesian model
# # ---------------------------------------------------------------------------
# 
# # stan data set
# df <- brexit %>% na.omit() %>% mutate(period=as.factor(1:length(date)))
# 
# data_list <- list(K = length(df$date),
#                    Y = data.matrix(df[c("y_in","y_out","y_dunno")]),
#                    N = df$sample,
#                    beta = c(0.5,0.4,01))
# 
# sm <- stan_model("R/brexit.stan")
# brexit_mod <- sampling(sm, data=data_list,
#                        iter=2000,chains=4,cores=2,
#                        init=0,verbose=TRUE,
#                        diagnostic_file="helpme.csv")
# 
# # --- model summaries
# rethinking::precis(brexit_mod,depth=2)
# post <- rethinking::extract.samples(brexit_mod)
# 
# quants <- c(0.005,  0.025,  0.10,  0.25,  0.50, 0.75,  0.90, 0.975,  0.995)
# model_summary <- plyr::adply(post$YSim,2:3,quantile,probs=quants) %>% 
#   setNames(c("period","vote",'l005', 'l025', 'l10', 'l25' ,'median', 'u75', 'u90', 'u975', 'u995')) 
# 
# model_summary %>%
#   left_join(df) %>% 
#   mutate_each(funs(. / sample), l005:u995) %>% 
#   mutate_each(funs(. / sample), y_in:y_dunno) %>% 
#   mutate_each(funs(. / 100), stay:undecided) %>% 
#   ggplot(aes(x=date,y=median,group=vote)) +
#   geom_ribbon(aes(ymin = l005, ymax = u995), fill = "grey50",alpha=0.1) +
#   geom_point(aes(x=date,y=y_in))+
#   geom_line(aes(colour=vote)) +
#   scale_x_date(date_labels = "%b %Y") +
#   ggtitle("BREXIT Vote") +
#   theme(legend.position="none")



