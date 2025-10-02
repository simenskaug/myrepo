# Created by Simen Skaug-Hønsi
# 25 sep. 2025

#preliminiaries
rm(list=ls())
gc()


#libraries ####
library(performance)
library(tidyverse)
library(boot)
library(GGally)
library(patchwork)

theme_bw()

# read the data
df_shark <- read.table("Shark.txt", sep="\t", 
                       header=T, fileEncoding="UTF-8") %>% 
  rename(Total_length = Total.Length..cm., 
         Dorsal_height = Dorsal.fin.height..cm.,
         Common_name = Common.name)

# making Common name a factor
df_shark <- df_shark %>%
  mutate(Common_name = factor(Common_name),
         Sex = factor(Sex))

# create dataframe fro each shark

df_GW <- df_shark %>% 
  filter(Common_name == "Great White")

df_LM <- df_shark %>% 
  filter(Total_length < 431 & Common_name == "Longfin Mako")

df_PB <- df_shark %>% 
  filter(Total_length < 366 & Common_name == "Porbeagle")

df_SA <- df_shark %>% 
  filter(Total_length < 301 & Common_name == "Salmon")

df_SM <- df_shark %>% 
  filter(Total_length < 401 & Common_name == "Shortfin Mako")



names(df_shark)
levels(df_shark$Common_name)

# check for mistakes
df_shark %>% 
  ggplot(aes(x= Dorsal_height)) +
  geom_histogram()

df_shark %>% 
  ggplot(aes(x= Dorsal_height)) +
  geom_histogram()

# exploratory plots ####
df_PB %>% 
  ggplot(aes(x= Total_length, y= Dorsal_height)) + 
  geom_point(alpha = 0.5)

# making a model ####
lm_dorsal <- glm(Dorsal_height ~ Total_length + Common_name + Sex +
                   Total_length:Common_name, data = df_shark, family = poisson)

#check the model
check_model(lm_dorsal)

# VIF does not look good
# making another model

lm_dorsal2 <- glm(Dorsal_height ~ Total_length + Common_name,
                  data = df_shark, family = poisson)

# making models for each speceis
lm_GW <- glm(Dorsal_height ~ Total_length,
             data = df_GW, family = poisson)

lm_LM <- glm(Dorsal_height ~ Total_length,
             data = df_LM, family = poisson)

lm_PB <- glm(Dorsal_height ~ Total_length,
             data = df_PB, family = poisson)

lm_SA <- glm(Dorsal_height ~ Total_length,
             data = df_SA, family = poisson)

lm_SM <- glm(Dorsal_height ~ Total_length,
             data = df_SM, family = poisson)


check_model(lm_dorsal2)

# VIFs now look good

# making plots ####
summary(lm_dorsal2)

summary(df_shark$Total_length)

levels(df_shark$Common_name)
levels(df_shark$Sex)

# making predictions and a new dataframe
Length_new <- crossing(Total_length = seq(33.5, 700, length = 100),
                       Common_name= c("Great White", "Longfin Mako",
                                      "Porbeagle", "Salmon", "Shortfin Mako"))

# make predictions for all
Length_GW <- crossing(Total_length = seq(33.5, 700, length = 100),
                      Common_name= "Great White")

Length_LM <- crossing(Total_length = seq(33.5, 431, length = 100),
                      Common_name= "Longfin Mako")

Length_PB <- crossing(Total_length = seq(33.5, 366, length = 100),
                      Common_name= "Porbeagle")

Length_SA <- crossing(Total_length = seq(33.5, 301, length = 100),
                      Common_name= "Salmon")

Length_SM <- crossing(Total_length = seq(33.5, 401, length = 100),
                      Common_name= "Shortfin Mako")



# making a matrix
preds_dorsal <- predict(lm_dorsal2, newdata = Length_new,
                        type= "link", se.fit = TRUE)

preds_GW <- predict(lm_GW, newdata = Length_GW,
                    type= "link", se.fit = TRUE)

preds_LM <- predict(lm_LM, newdata = Length_LM,
                    type= "link", se.fit = TRUE)

preds_PB <- predict(lm_PB, newdata = Length_PB,
                    type= "link", se.fit = TRUE)

preds_SA <- predict(lm_SA, newdata = Length_SA,
                    type= "link", se.fit = TRUE)

preds_SM <- predict(lm_SM, newdata = Length_SM,
                    type= "link", se.fit = TRUE)

# binding new dataset and matri together
df_predsGW <- bind_cols(Length_GW, as_tibble(preds_GW))

df_predsLM <- bind_cols(Length_LM, as_tibble(preds_LM)) 

df_predsPB <- bind_cols(Length_PB, as_tibble(preds_PB))
  
df_predsSA <- bind_cols(Length_SA, as_tibble(preds_SA))
  
df_predsSM <- bind_cols(Length_SM, as_tibble(preds_SM))

summary(lm_GW)
summary(lm_LM)
summary(lm_PB)

# making a plots for all ####

PSA <- df_predsSA %>% 
  ggplot(aes(x = Total_length, y= exp(fit), color = Common_name)) +
  geom_ribbon(aes(ymax = exp(fit + 1.71 * se.fit), 
                  ymin = exp(fit - 1.71 * se.fit)),
              alpha= 0.3, linetype = 0) + 
  geom_line(color= "aquamarine3", size = 1) +
  geom_point(data = df_SA, aes(y= Dorsal_height), alpha= 0.5, color = "aquamarine4", size = 1.5) +
  labs(x= "Total shark length (cm)", y= "Dorsal fin height (cm)") + 
  facet_grid(Common_name ~.) + 
  scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700)) +
  theme_bw() + theme(legend.position = "none")


PGW <- df_predsGW %>% 
    ggplot(aes(x = Total_length, y= exp(fit), color = Common_name)) +
    geom_ribbon(aes(ymax = exp(fit + 1.3698 * se.fit), 
                    ymin = exp(fit - 1.3698 * se.fit)),
                alpha= 0.3, linetype = 0) + 
    geom_line(color= "aquamarine3", size = 1) +
    geom_point(data = df_GW, aes(y= Dorsal_height), alpha= 0.5, color = "aquamarine4", size = 1.5) +
    labs(x= "Total shark length (cm)", y= "Dorsal fin height (cm)") + 
    facet_grid(Common_name ~.) + 
    scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700)) +
    theme_bw() + theme(legend.position = "none")
  
PLM <- df_predsLM %>% 
    ggplot(aes(x = Total_length, y= exp(fit), color = Common_name)) +
    geom_ribbon(aes(ymax = exp(fit + 0.11 * se.fit), 
                    ymin = exp(fit - 0.11 * se.fit)),
                alpha= 0.3, linetype = 0) + 
    geom_line(color= "aquamarine3", size = 1) +
    geom_point(data = df_LM, aes(y= Dorsal_height), alpha= 0.5, color = "aquamarine4", size = 1.5) +
    labs(x= "Total shark length (cm)", y= "Dorsal fin height (cm)") + 
    facet_grid(Common_name ~.) + 
    scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700)) +
    theme_bw() + theme(legend.position = "none")
  
PPB <- df_predsPB %>% 
    ggplot(aes(x = Total_length, y= exp(fit), color = Common_name)) +
    geom_ribbon(aes(ymax = exp(fit + .71 * se.fit), 
                    ymin = exp(fit - .71 * se.fit)),
                alpha= 0.3, linetype = 0) + 
    geom_line(color= "aquamarine3", size = 1) +
    geom_point(data = df_PB, aes(y= Dorsal_height), alpha= 0.5, color = "aquamarine4", size = 1.5) +
    labs(x= "Total shark length (cm)", y= "Dorsal fin height (cm)") + 
    facet_grid(Common_name ~.) + 
    scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700)) +
    theme_bw() + theme(legend.position = "none")
  
PSM <- df_predsSM %>% 
  ggplot(aes(x = Total_length, y= exp(fit), color = Common_name)) +
  geom_ribbon(aes(ymax = exp(fit + .3 * se.fit), 
                  ymin = exp(fit - .3 * se.fit)),
              alpha= 0.3, linetype = 0) + 
  geom_line(color= "aquamarine3", size = 1) +
  geom_point(data = df_SM, aes(y= Dorsal_height), alpha= 0.5, color = "aquamarine4", size = 1.5) +
  labs(x= "Total shark length (cm)", y= "Dorsal fin height (cm)") + 
  facet_grid(Common_name ~.) + 
  scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700)) +
  theme_bw() + theme(legend.position = "none")


# making a plot with all on same
PGW + PLM + PPB + PSA + PSM
