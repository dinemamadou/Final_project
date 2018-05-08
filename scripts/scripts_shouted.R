library(tidyverse)
library(plot3D)
library(xaringan)
library(dbplyr)
library(readr)
library(here)
library(broom)
library(knitr)
library(MuMIn)
library(lme4)

shouted <- read_csv(here("data", "formantsdur.csv")) %>%
  mutate(., int_c = intensity - mean(intensity)) %>%
  mutate(., f1_c = F1_midpoint - mean(F1_midpoint)) %>%
  mutate(., f2_c = F2_midpoint - mean(F2_midpoint)) %>%
  mutate(., dur_c = duration - mean(duration)) 
 

shouted1 <- shouted %>% 
  separate(., Filename, into = c("gender", "condition"), remove = FALSE) %>% 
  separate(., TextGridLabel, into = c("word", "rep"), sep = "_") 

plot1 <- shouted1 %>%
  group_by(., vowel) %>%
  ggplot(., aes(x = F2_midpoint, y = F1_midpoint, 
                color = condition)) +
  geom_point() 

plot2 <- shouted1 %>%
  ggplot(., aes(x = dur_c, y = int_c, color = condition)) +
    facet_grid(. ~ gender) +
    geom_boxplot() +
    scale_color_brewer(palette = "Set1", name = "condition") +
     labs(x = "Centered duration (ms)", 
          y = "Centered intensidty (dB)", 
       title = "Intensity by duration", 
       caption = "Mean +/- SE")

#save1 <- ggsave(filename = "./figs/durByInt.png",
#       plot = plot2,
#       width = 9, height = 4,
#       unit = "in") #to save the plot in a folder of our choice


plot3 <- shouted1 %>%
  ggplot(., aes(x = dur_c, y = int_c, color = condition, shape = gender)) +
    geom_point() +
    scale_color_brewer(palette = "Set1", name = "condition") +
     labs(x = "Centered duration (ms)", 
          y = "Centered intensidty (dB)", 
       title = "Intensity by duration", 
       caption = "Mean +/- SE")

plot4 <- shouted1 %>%
  ggplot(., aes(x = int_c, y = f1_c, color = condition, shape = gender)) +
    geom_point() +
    scale_color_brewer(palette = "Set1", name = "condition") +
     labs(x = "Centered intensidty (dB)", 
          y = "Centered F1 (Hz)", 
       title = "F1 as a Function of Intensity", 
       caption = "Mean +/- SE")

#ggsave(filename = "./figs/intByF1.png",
#       plot = plot4,
#       width = 9, height = 4,
#       unit = "in")


#F1 ~


# 2. check structure
head(shouted1)  
#lme models

mod_f1 <- lmer(f1_c ~ int_c + dur_c + gender + f2_c + int_c:gender +
          (1 | gender) + (1 + rep | word), 
           data = shouted1, REML = F)
summary(mod_f1)

mod_f2 <- lmer(f2_c ~ int_c + dur_c + gender + f1_c + int_c:gender + 
          (1 | gender) + (1 + rep | word), 
           data = shouted1, REML = F)
summary(mod_f2)


mod_dur <- lmer(dur_c ~ int_c + f1_c + gender + f2_c + int_c:gender + 
          (1 | gender) + (1 + rep | word), 
           data = shouted1, REML = F)

summary(mod_dur)

d <- anova(mod_f1, mod_f2, mod_dur)

dur_resid <- plot(fitted(mod_dur), residuals(mod_dur))


#homoskedastic

dur_res1 <- qqnorm(residuals(mod_dur))
dur_res2 <- qqline(residuals(mod_dur))



# 3. Nested models with f1 as the criterion

mod_null <- glm(f1_c ~ 1, data = shouted1,
                  family = gaussian(link = "identity"))

mod_int_c <- glm(f1_c ~ 1 + int_c, 
                 data = shouted1, family = gaussian(link = "identity"))

mod3 <- glm(f1_c ~ 1 + int_c + gender, 
                 data = shouted1, family = gaussian(link = "identity"))

mod4 <- glm(f1_c ~ 1 + int_c + gender + dur_c, 
                 data = shouted1, family = gaussian(link = "identity"))

mod5 <- glm(f1_c ~ 1 + int_c + gender + dur_c + int_c:gender, 
                 data = shouted1, family = gaussian(link = "identity"))

mod_full <- glm(f1_c ~ 1 + int_c + gender + dur_c + int_c:gender + dur_c:gender, 
                 data = shouted1, family = gaussian(link = "identity"))

anova_f1 <- anova(mod_null, mod_int_c, mod3, mod4, mod5, mod_full, test = "Chisq")

summary(anova_f1)
r.squaredGLMM(mod5) #for r-squared

# Nested models with f2 as the criterion (no effect whatsoever, 
# so f2 will be removed from all models)


# Nested models with durationas the criterion

mod_null2 <- glm(dur_c ~ 1, data = shouted1,
                  family = gaussian(link = "identity"))

mod_int_c2 <- glm(dur_c ~ 1 + int_c, 
                 data = shouted1, family = gaussian(link = "identity"))

mod32 <- glm(dur_c ~ 1 + int_c + gender, 
                 data = shouted1, family = gaussian(link = "identity"))

mod42 <- glm(dur_c ~ 1 + int_c + gender + f1_c, 
                 data = shouted1, family = gaussian(link = "identity"))


mod52 <- glm(dur_c ~ 1 + int_c + gender + f1_c + int_c:gender, 
                 data = shouted1, family = gaussian(link = "identity"))

mod_full2 <- glm(dur_c ~ 1 + int_c + gender + f1_c + int_c:gender + f1_c:gender, 
                 data = shouted1, family = gaussian(link = "identity"))

summary(mod32)
r.squaredGLMM(mod52) #for r-squared


broom::tidy(mod32) %>% knitr::kable(., digits = 2)

anova_dur <- anova(mod_null2, mod_int_c2, mod32, mod42, mod52, mod_full2, test = "Chisq") %>% 
  knitr::kable(., format = 'latex', digits = 2)


dur_resid <- plot(fitted(mod_full2), residuals(mod_full2))
#homoskedastic

dur_res1 <- qqnorm(residuals(mod_full2))
dur_res2 <- qqline(residuals(mod_full2))



r.squaredGLMM(mod_full) #for r-squared

MainEffect <- matrix(c(662.89,24.75,26.78,2e-16,-3.4,10.56,-0.32,0.75,455.60,222.72,2.05,0.04,-127.95,125.29,-1.02,0.31),ncol=4,byrow=TRUE)
 colnames(MainEffect) <- c("estimate","std.Err","statistics","p-value")
 rownames(MainEffect) <- c("(Intercept)","int_c","dur_c","int_c:dur_c") 
 ME <- as.table(MainEffect)
 
#Main effect of duration

# to avoid a Type I error, I removed gender and condition as predictors.




#An interaction between


group <- shouted1 %>% 
  group_by(., condition) %>%
  summarize(., meanf1 = mean(F1_midpoint),
            sd.f1 = sd(F1_midpoint),
            mean.int = mean(intensity),
            sd.int = sd(intensity),
            mean.dur = mean(duration),
            sd.dur = sd(duration))%>%
  knitr::kable(., format = 'latex', digits = 2)



#test for interactions/
#anova <- anova(model_null, model_p, model_add, model_int, test = "Chisq") #Chisq includs the p value

# main effects
# 4. summary of best model

# 5. write up of output



# 6. generate and save plot

poisson_reg1 <- ggplot(poisson, aes(x = temp_c, y = units, color = city)) +
  geom_point(pch = 21) +
  geom_smooth(method = glm,
              method.args = list(family = "poisson"))

poisson_reg1

#ggsave(filename = "./figs/poisson_plot.png",
#       plot = poisson_reg1,
#       width = 9, height = 4,
#       unit = "in") #to save the plot in a folder of our choice

write_csv(shouted1, "./clean_shouted.csv")
