library(tidyverse)
library(plot3D)
library(xaringan)
library(dbplyr)
library(readr)
library(here)

shouted <- read_csv(here("data", "formantsdur.csv")) %>%
  mutate(., int_c = intensity - mean(intensity)) %>%
  mutate(., f1_c = F1_midpoint - mean(F1_midpoint)) %>%
  mutate(., f2_c = F2_midpoint - mean(F2_midpoint)) %>%
  mutate(., dur_c = duration - mean(duration)) 
 

shouted1 <- shouted %>% 
  separate(., Filename, into = c("gender", "condition"), remove = FALSE) %>% 
  separate(., TextGridLabel, into = c("word", "rep"), sep = "_") 

plot1 <- shouted1 %>%
  group_by(., gender) %>%
  ggplot(., aes(x = F2_midpoint, y = F1_midpoint, 
                color = condition, label = vowel)) +
  geom_point() +
  geom_label()

plot2 <- shouted1 %>%
  ggplot(., aes(x = dur_c, y = int_c, color = condition, shape = gender)) +
  geom_point() 
  

#F1 ~


# 2. check structure
head(shouted1)
#summary(poisson)
# 3. fit inclusive and nested models

mod_null <- glm(F1_midpoint ~ 1, data = shouted1,
                  family = gaussian(link = "identity"))

mod_int_c <- glm(F1_midpoint ~ int_c, 
                 data = shouted1, family = gaussian(link = "identity"))

mod_inter1 <- glm(F1_midpoint ~ int_c * gender, 
                 data = shouted1, family = gaussian(link = "identity"))

mod_inter2 <- glm(F1_midpoint ~ int_c * dur_c, 
                 data = shouted1, family = gaussian(link = "identity"))

mod_full1 <- glm(F1_midpoint ~ int_c * dur_c * gender, 
                data = shouted1, family = gaussian(link = "identity"))


summary(mod_full1)

MainEffect <- matrix(c(662.89,24.75,26.78,2e-16,-3.4,10.56,-0.32,0.75,455.60,222.72,2.05,0.04,-127.95,125.29,-1.02,0.31),ncol=4,byrow=TRUE)
 colnames(MainEffect) <- c("estimate","std.Err","statistics","p-value")
 rownames(MainEffect) <- c("(Intercept)","int_c","dur_c","int_c:dur_c") 
 ME <- as.table(MainEffect)
 
#Main effect of duration

# to avoid a Type I error, I removed gender and condition as predictors.
models <- anova(mod_null, mod_inter1, mod_inter2, mod_full1, test = "Chisq")

summary(models)



#An interaction between


group <- shouted1 %>% 
  group_by(., condition) %>%
  summarize(., meanf1 = mean(F1_midpoint),
            mean.int = mean(intensity),
            mean.dur = mean(duration)) %>%
  knitr::kable(., format = 'html', digits = 2)

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
