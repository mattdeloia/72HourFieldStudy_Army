library(readxl)
library(janitor)
library(tidyverse)
library(DT)
library(superheat)
library(stringr)
library(Hmisc)

df <- read_csv("72HFS_Raid_Raw Data.csv") %>% 
  clean_names() %>%
  mutate(description = gsub("[*]", "", description)) %>%
  mutate_at(vars(name, description), str_trim ) %>%
  mutate(level = if_else(participant_name %in% c("1st Platoon", "2nd Platoon", "3rd Platoon"), "Platoon", "Squad")) %>% 
  left_join(
    read_xlsx("Key_Raid.xlsx", sheet = "Key") %>%
      clean_names() %>% 
      mutate(description = gsub("[*]", "", description)) %>% 
      mutate_at(vars(name, description), str_trim ) 
  ) %>%
  left_join(
    read_xlsx("Key_Raid.xlsx", sheet = "Weightings") %>%
      clean_names() %>% 
      mutate(description = gsub("[*]", "", description)) %>% 
      mutate_at(vars(description), str_trim ) 
  ) %>%
  group_by(grader_name, participant_name, measure, name, description, level, result, point_total, global_weight) %>%
  summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% 
  rename(participant = participant_name) %>% 
  rename(grader = grader_name) %>% 
  filter(result=="PointsBased") %>%
  mutate(points = points_scored/point_total) %>% 
  select(grader, level, participant, measure, description, point_total, points_scored, points, global_weight) %>% 
  drop_na(measure)

observation_check_plt1 <- df %>%
  filter(level =="Platoon", measure=="Task") %>% 
  group_by(participant, name, description) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  pivot_wider(names_from = "participant", values_from = "count")

observation_check_plt2 <- df %>%
  filter(level =="Platoon", measure=="Performance") %>% 
  group_by(participant, name, description) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  pivot_wider(names_from = "participant", values_from = "count")


observation_check_squad1 <- df %>%
  filter(level =="Squad", measure =="Task") %>% 
  group_by(participant, name, description) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "participant", values_from = "count") %>%
  group_by(name, description) 

observation_check_squad2 <- df %>%
  filter(level =="Squad", measure =="Performance") %>% 
  group_by(participant, name, description) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "participant", values_from = "count") %>%
  group_by(name, description) 


#variability analysis

df %>% 
  group_by(name, level, participant, description) %>%
  filter(level=="Squad", measure=="Performance") %>% 
  summarise(points = median(points, na.rm = TRUE)) %>% 
  group_by(level, description) %>% 
  summarise(sd = round(sd(points),2)) %>%
  mutate(sd_zscore = scale(sd)) %>% 
  mutate(variability = if_else(sd_zscore < (-.5), "low", if_else(sd_zscore<.5, "medium", "high"))) %>% 
  rbind(
    df %>% 
      group_by(name, level, participant, description) %>%
      filter(level=="Squad", measure=="Task") %>% 
      summarise(points = median(points, na.rm = TRUE)) %>% 
      group_by(level, description) %>% 
      summarise(sd = round(sd(points),2)) %>%
      mutate(sd_zscore = scale(sd)) %>% 
      mutate(variability = if_else(sd_zscore < (-.5), "low", if_else(sd_zscore<.5, "medium", "high"))) 
  ) %>% 
  write.csv("measure_variability_Task_Squad_Raid.csv")

df %>% 
  group_by(name, level, participant, description) %>%
  filter(level=="Platoon", measure=="Performance") %>% 
  summarise(points = median(points, na.rm = TRUE)) %>% 
  group_by(level, description) %>% 
  summarise(sd = round(sd(points),2)) %>%
  mutate(sd_zscore = scale(sd)) %>% 
  mutate(variability = if_else(sd_zscore < (-.5), "low", if_else(sd_zscore<.5, "medium", "high"))) %>% 
  rbind(
    df %>% 
      group_by(name, level, participant, description) %>%
      filter(level=="Platoon", measure=="Task") %>% 
      summarise(points = median(points, na.rm = TRUE)) %>% 
      group_by(level, description) %>% 
      summarise(sd = round(sd(points),2)) %>%
      mutate(sd_zscore = scale(sd)) %>% 
      mutate(variability = if_else(sd_zscore < (-.5), "low", if_else(sd_zscore<.5, "medium", "high"))) 
  ) %>% 
  write.csv("measure_variability_Task_Platoon_Raid.csv")

#weighted scores analysis 
squad_performance <- df %>% 
  filter(level=="Squad")%>% 
  group_by(participant, level, measure, name, description, global_weight) %>% 
  summarise(points_scored = median(points, na.rm = TRUE)) %>%
  group_by(participant, level, measure, description, global_weight) %>% 
  summarise(points_scored2 = mean(points_scored, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate_at(vars(points_scored2), impute) %>% 
  mutate(score_wt = points_scored2*global_weight) %>%  #replace  with global weight
  group_by(participant, measure) %>%
  summarise(point_total = sum(score_wt, na.rm = TRUE)) %>%  # sum or mean score
  pivot_wider(names_from = "measure", values_from = "point_total") 

squad_performance %>% 
  write.csv("SquadResults_Raid.csv")

squad_performance %>%
  gather(Performance:Task, key=measure, value=point_total) %>% 
  ggplot(aes(x=reorder(participant, point_total, fun=mean), y=point_total, color=measure)) +
  geom_point(size=3) +
  coord_flip() +
  ylim(0,1)+
  ylab("score (proportion of total)") +
  xlab("") 

########


##OC scoring behaviors
df %>% filter(level =="Squad", grader %in% c("Raid R OC1", "Raid R OC2","Raid R OC3")) %>%
  mutate(measure = if_else(measure=="Performance", "Tactical Principles", measure)) %>% 
  ggplot(aes(x=points)) +
  geom_density(aes(fill=grader, color=grader), alpha = .5, adjust=1.5) +
  facet_grid(measure~., scales = "free") +
  theme(legend.position = "top") +
  xlab("proportion of total score") +
  ylab("") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Individual Judgement (level error)") + 
  ggsave("Grader_score_dist.png", width = 6.5, height = 5.2, units = "in")

df %>% filter(level =="Squad", grader %in% c("Raid R OC1", "Raid R OC2","Raid R OC3")) %>% 
  group_by(grader, measure) %>% 
  summarise(ave_rating = mean(points, na.rm=TRUE), sd = sd(points, na.rm=TRUE)) %>% 
  arrange(measure)
library(ggridges)

df %>% filter(level =="Squad", grader %in% c("Raid R OC1", "Raid R OC2","Raid R OC3")) %>% 
  # group_by(grader, measure) %>% 
  #summarise(ave_rating = mean(points, na.rm=TRUE), sd = sd(points, na.rm=TRUE)) %>% 
  ggplot(aes(x=points, color=measure)) +geom_density2d(aes(y=grader)) +
  facet_grid(.~measure)

df %>% filter( grader %in% c("Raid R OC1", "Raid R OC2","Raid R OC3"), measure !="Task") %>% 
  group_by(grader, measure, description, level, name) %>% 
  summarise(ave_rating = mean(points, na.rm=TRUE), sd = sd(points, na.rm=TRUE)) %>% 
  arrange(measure) %>% 
  ggplot(aes(x=description, y=ave_rating, color=grader, group = grader)) + 
  geom_point() + 
  geom_line() + 
  coord_flip() +
  facet_grid(level~.)

squad_performance_scaled <- df %>% ungroup() %>%  
  filter(grader %in% c("Raid R OC1"), level =="Squad", measure=="Performance") %>%
  mutate_at(vars(points), scale) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC1"), level =="Squad", measure=="Task") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC2"), level =="Squad", measure=="Performance") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC2"), level =="Squad", measure=="Task") %>%
      mutate_at(vars(points), scale)
  ) %>%
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC3"), level =="Squad", measure=="Performance") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC3"), level =="Squad", measure=="Task") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  group_by(participant, level, measure, name, description, global_weight) %>% 
  summarise(points_scored = median(points, na.rm = TRUE)) %>%
  group_by(participant, level, measure, description, global_weight) %>% 
  summarise(points_scored2 = mean(points_scored, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate_at(vars(points_scored2), impute) %>% 
  mutate(score_wt = points_scored2*global_weight) %>%  #replace  with global weight
  group_by(participant, measure) %>% 
  summarise(point_total = pnorm(sum(score_wt))) %>% 
  pivot_wider(names_from = "measure", values_from = "point_total")

squad_performance_scaled %>%  write.csv("SquadResults_Raid_scaled.csv")

df %>% ungroup() %>%  
  filter(grader %in% c("Raid R OC1"), level =="Squad", measure=="Performance") %>%
  mutate_at(vars(points), scale) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC1"), level =="Squad", measure=="Task") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC2"), level =="Squad", measure=="Performance") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC2"), level =="Squad", measure=="Task") %>%
      mutate_at(vars(points), scale)
  ) %>%
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC3"), level =="Squad", measure=="Performance") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC3"), level =="Squad", measure=="Task") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  group_by(participant, name, description) %>% 
  summarise(points_scored = median(points, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(participant, description) %>% 
  summarise(points_scored2 = mean(points_scored, na.rm = TRUE)) %>%
  pivot_wider(names_from = "description", values_from = "points_scored2") %>% 
  ungroup() %>% 
  mutate_if(is.numeric, impute) %>% 
  gather(2:56, key="description", value="points_scored2") %>%
  left_join(
    df %>% ungroup() %>% 
      filter(level=="Squad") %>% 
      select(description, measure, global_weight) %>%
      unique() ) %>% 
  mutate(score_wt = points_scored2*global_weight) %>%
  rename("raw_score"="points_scored2", "weight"="global_weight", "weighted_score"="score_wt", "measure_category"="measure") %>% 
  mutate_if(is.numeric, ~round(.x, 4)) %>% 
  mutate(event = "Raid") %>% 
  mutate(notes = "standardized score (z score) by measure category") %>% 
  select(participant, event, measure_category, description, weight, raw_score,  weighted_score, notes) %>% 
  arrange(participant, measure_category) %>% 
  write_csv("SquadResults_Raid_RawData.csv")

read_csv("SquadResults_Raid_RawData.csv") %>% 
  group_by(participant, measure_category) %>% 
  summarise(score = sum(weighted_score)) %>% 
  pivot_wider(names_from = "measure_category", values_from = "score") %>% ungroup %>% 
  
  mutate_if(is.numeric, scale) %>% 
  mutate_if(is.numeric, pnorm) %>% 
  group_by(participant) %>% 
  mutate(total = mean(c(Performance, Task)))

comparison <-  squad_performance %>%
  mutate(method = "unscaled") %>% 
  rbind(
    squad_performance_scaled %>% 
      mutate(method="scaled")
  ) %>% 
  mutate(method = as.factor(method)) %>% 
  gather(Performance:Task, key=measure, value=score) %>% 
  group_by(measure, method) %>% 
  mutate(rank = rank(-score))

comparison %>% 
  left_join(squad_performance %>% 
              gather(Performance:Task, key=measure, value=score) %>% 
              group_by(measure) %>% 
              mutate(rank2 = rank(-score)) %>% 
              select(participant, rank2)
  ) %>% 
  
  filter(measure =="Performance") %>% 
  ggplot(aes(x=reorder(participant, rank2, fun=mean ), y=rank, fill=method, label=rank)) +
  geom_col(position = position_dodge2())+ 
  ggtitle("Performance") +
  geom_text(aes(label = rank, color=method), position = position_dodge2(0.9), size = 4, vjust = -.5) +
  xlab("") +
  facet_grid(measure~.) +
  theme(legend.position = "top")

comparison %>% 
  left_join(squad_performance %>% 
              gather(Performance:Task, key=measure, value=score) %>% 
              group_by(measure) %>% 
              mutate(rank2 = rank(-score)) %>% 
              select(participant, rank2)
  ) %>% 
  
  filter(measure =="Task") %>% 
  ggplot(aes(x=reorder(participant, rank2, fun=mean ), y=rank, fill=method, label=rank)) +
  geom_col(position = position_dodge2())+ 
  ggtitle("Task") +
  geom_text(aes(label = rank, color=method), position = position_dodge(0.9), size = 4, vjust = -.5) +
  facet_grid(measure~.) +
  xlab("")+
  theme(legend.position = "top")

#Discriminating features
df_discrim <- df %>% 
  ungroup() %>%  
  filter(grader %in% c("Raid R OC1"), level =="Squad", measure=="Performance") %>%
  mutate_at(vars(points), scale) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC1"), level =="Squad", measure=="Task") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC2"), level =="Squad", measure=="Performance") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC2"), level =="Squad", measure=="Task") %>%
      mutate_at(vars(points), scale)
  ) %>%
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC3"), level =="Squad", measure=="Performance") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  rbind(
    df %>% ungroup() %>%  
      filter(grader %in% c("Raid R OC3"), level =="Squad", measure=="Task") %>%
      mutate_at(vars(points), scale)
  ) %>% 
  group_by(participant, level, measure, name, description) %>% 
  summarise(points_scored = median(points, na.rm = FALSE)) 



df_discrim_Task <- df_discrim %>% 
  filter(measure=="Task") %>% 
  ungroup() %>% 
  mutate(measure = paste(name, description, sep="_")) %>% 
  mutate(measure = str_trim(measure)) %>% 
  select(participant, measure, points_scored) %>% 
  pivot_wider(names_from = "measure", values_from = "points_scored") %>% 
  left_join(squad_performance_scaled %>% 
              mutate(overall_score= mean(c(Performance, Task))) %>% 
              select(participant, overall_score)) %>%
  column_to_rownames("participant") %>% 
  mutate_if(is.numeric, impute) %>% 
  as.data.frame()

df_discrim_Performance <- df_discrim %>% 
  filter(measure=="Performance") %>% 
  ungroup() %>% 
  mutate(measure = description) %>% 
  select(participant, measure, points_scored) %>% 
  group_by(participant, measure) %>% 
  summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "measure", values_from = "points_scored") %>% 
  left_join(squad_performance_scaled %>% 
              mutate(overall_score= mean(c(Performance, Task))) %>% 
              select(participant, overall_score)) %>% 
  column_to_rownames("participant") %>% 
  mutate_if(is.numeric, impute) %>% 
  as.data.frame()


cor(df_discrim_Task) %>%
  as.data.frame() %>% 
  rownames_to_column("measure") %>% 
  select(measure, overall_score) %>% 
  filter(overall_score >.5 | overall_score < -.5) %>% 
  arrange(-overall_score) %>% 
  mutate(overall_score = round(overall_score, 2)) %>% 
  write.csv("Task_cor.csv")  

cor(df_discrim_Performance) %>%
  as.data.frame() %>% 
  rownames_to_column("measure") %>% 
  select(measure, overall_score) %>% 
  filter(overall_score >.5 | overall_score < -.5) %>% 
  arrange(-overall_score) %>% 
  mutate(overall_score = round(overall_score, 2)) %>% 
  write.csv("Performance_cor.csv")
