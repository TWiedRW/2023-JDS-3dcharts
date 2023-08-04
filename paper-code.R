## ---- include = F-------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = F, dpi = 300)

library(tidyverse)
library(RSQLite)
library(lme4)

con = dbConnect(SQLite(), '../../experiment_interface/department.db')
dbListTables(con)
results = dbReadTable(con, 'results')
users = dbReadTable(con, 'user')
userMatrix = dbReadTable(con, 'userMatrix')
dbDisconnect(con)

valid.users <- users %>% 
  mutate(subject = paste0(nickname, participantUnique)) %>% 
  filter(age != 'Under 19')


#Fill in correct values for incorrect 3d graph kits
# 1 result, need to fill in fileID, graphtype, and plot
results = results %>% 
  mutate(fileID = ifelse(graphCorrecter %in% 'id-01/Type1-Rep01', 1, fileID),
         graphtype = ifelse(graphCorrecter %in% 'id-01/Type1-Rep01', 'Type1', graphtype),
         plot = ifelse(graphCorrecter %in% 'id-01/Type1-Rep01', '3dPrint', plot))



load('../../experiment_interface/data/set85data.Rdata')
load('../../experiment_interface/data/kits.Rdata')


trueRatios = datasets %>% 
  mutate(ratio.df = map(data, function(x)(x[!is.na(x[,'IDchr']),4])),
         trueRatio = map(ratio.df, function(x)(x[1,'Height'] / x[2,'Height']))) %>% 
  unnest(trueRatio) %>% 
  filter(fileID != 15) %>% 
  #mutate(Height = Height * 100) %>% 
  select(fileID, Height)

res = results %>% 
  left_join(trueRatios, by = 'fileID') %>% 
  mutate(response = log2(abs(byHowMuch - Height*100) + 1/8),
         subject = paste0(nickname, participantUnique),
         ratioLabel = round(100*Height, 1)) %>% 
  filter(subject %in% valid.users$subject) %>% 
  arrange(appStartTime) %>% 
  filter(whichIsSmaller == 'Triangle (▲)',
         as.Date.POSIXct(appStartTime) <= '2023-05-22') #Only verified results from before the start of SDSS




## ----plotTypes, fig.cap = "Two dimensional, two-dimensional digital rendering, and 3D-printed charts used in this study.", out.width = "100%"----
knitr::include_graphics("plot-types.png", dpi = 300)


## ----studyDesign, fig.cap = "A graphical representation of the study design. Only five of the seven ratios were used in each kit where the ratios are randomly selected. A total of 21 kits were created to include all combinations of the five ratios."----
knitr::include_graphics("study-design.png")


## ----demographics, fig.cap="Demographic breakdown of participants in the study. All subjects were recruited from faculty and students in the statistics department at University of Nebraska-Lincoln.", message=FALSE, warning=FALSE, out.width='85%'----
library(patchwork)
valid.users$userAppStartTime <- as.POSIXct(valid.users$userAppStartTime,
           origin = '1970-1-1')

age.levels = sort(unique(valid.users$age))
p1 = valid.users[valid.users[,'userAppStartTime'] <= '2023-05-22',] %>% 
  group_by(age) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = factor(age, levels = age.levels),
                       y = count)) + 
  geom_bar(stat = 'identity') +
  geom_text(mapping = aes(y = count+1, label = count),
             size = 2) +
  labs(title = 'Age Groups',
       x = '',
       y = 'Count') +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, hjust=1, size = 8))



p2 = valid.users[valid.users[,'userAppStartTime'] <= '2023-05-22',] %>% 
  group_by(gender) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = gender,
                       y = count)) + 
  geom_bar(stat = 'identity') +
  geom_text(mapping = aes(y = count+1, label = count),
             size = 2) +
  labs(title = 'Gender Groups',
       x = '',
       y = 'Count') +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, hjust=1, size = 8))


educ.levels = sort(unique(valid.users$education))[c(2,4,3,1)]
p3 = valid.users[valid.users[,'userAppStartTime'] <= '2023-05-22',] %>% 
  group_by(education) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = factor(education, levels = educ.levels),
                       y = count)) + 
  geom_bar(stat = 'identity') +
  geom_text(mapping = aes(y = count+2, label = count),
             size = 2) +
  labs(title = 'Education Levels',
       x = '',
       y = 'Count') +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, hjust=1, size=8))

p = p1 + p2 + p3
p


## ----practice, fig.cap = "Screenshot of Shiny application practice screen. Three 2D bar charts with different ratios were provided, along with sliders indicating the correct proportion. Participants could practice with the sliders and preview the questions that would be asked as part of the task.", out.width = "80%"----
knitr::include_graphics("03-Practice-2.png")


## ----experiment3dRender, fig.cap = "Screenshot of the applet collecting data for a 3D rendered chart task. Participants were asked to select which bar (circle or triangle) was smaller, and then to estimate the ratio of the smaller bar to the larger bar.", out.width = "80%"----
knitr::include_graphics("05-Experiment-05-filled-in.png")


## ----midmeans-log-errors, fig.cap='Midmeans of log absolute errors for the true ratio of bars. Each overlaying line are computed with loess.', message=FALSE, warning=FALSE, out.width='80%'----
plot.types = c('2D', 'Rendered 3D', '3D Printed')
names(plot.types) = c('2dDigital', '3dDigital', '3dPrint')



res %>% 
  group_by(Height, graphtype, plot) %>% 
  summarize(midmean = mean(response, trim=0.25, na.rm = T)) %>% 
  ggplot(mapping = aes(x = Height, y = midmean,
                       color = graphtype)) +
  geom_point() +
  geom_smooth(se = F, alpha=1/2) +
  scale_color_discrete(labels = c('Adjacent', 'Separated')) +
  facet_wrap(~plot, labeller = labeller(plot = plot.types)) +
  labs(title = '',
       x = 'True Proportional Difference (%)',
       y = 'Midmeans of Log Error',
       color = 'Comparison Type') +
  theme_bw() +
  theme(legend.position = 'bottom')


## ---- include=FALSE-----------------------------------------------------------------------------------------
mod = lmer(response ~ (1|subject) + ratio + type + ratio:plot,
     data = res)
anova(mod)

