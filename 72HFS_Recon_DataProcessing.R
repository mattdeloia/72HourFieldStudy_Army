library(readxl)
library(janitor)
library(tidyverse)
library(DT)
library(superheat)
library(Hmisc)

getwd()

df <- read_xlsx("72HFS_Recon_Raw Data.xlsx") %>% 
  clean_names() %>%
  mutate(description = gsub("[*]", "", description)) %>% 
  filter(result=="PointsBased") %>% 
  left_join(
    read_xlsx("Key_Recon.xlsx", sheet = "Key") %>% 
      mutate(description = gsub("[*]", "", description))
  ) %>% 
  rename(participant = participant_name) %>% 
  select(participant,measure, category, description, points_scored, grader_name, feature) %>%
  mutate(feature = as.factor(feature)) %>% 
  drop_na(measure)

observation_check <- df %>%  
  group_by(participant, feature) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "feature", values_from = "count")
  
  
df %>%  filter(measure =="Task") %>%
  ggplot(aes(x=feature, y=points_scored)) + 
  geom_boxplot()

df %>% filter(measure =="Task") %>% group_by(feature) %>% summarise(n = n()) %>% 
  ggplot(aes(x=reorder(feature, n, fun=mean), y=n)) + geom_col() + coord_flip()

df %>% filter(measure =="Performance") %>% group_by(category) %>% summarise(n = n()) %>% 
  ggplot(aes(x=reorder(category, n, fun=mean), y=n)) + geom_col() + coord_flip()

#variability analysis

df %>% 
  filter(measure =="Task") %>% 
  group_by(participant, description) %>% 
  summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
  group_by(description) %>% 
  summarise(sd = round(sd(points_scored),2)) %>%
  mutate(sd_zscore = scale(sd)) %>% 
  mutate(variability = if_else(sd_zscore < (-.5), "low", if_else(sd_zscore<.5, "medium", "high"))) %>% 
  write.csv("measure_variability_Task_Recon.csv")

#weighted scores analysis on three methods
df_weighted_scores <- df %>%  
  filter(measure=="Task") %>%
  group_by(participant, measure, description) %>% 
  summarise(points_scored = median(points_scored, na.rm = TRUE)) %>%
  left_join(read_xlsx("Key_Recon.xlsx", sheet="Weightings") %>%
              select(description,global_weight) %>% 
              mutate(description = gsub("[*]", "", description))) %>%
  mutate(score_wt = points_scored*global_weight) %>% 
  group_by(participant,  measure) %>% 
  summarise(point_total = sum(score_wt)/4) %>%
  rbind( df %>% 
           filter(measure=="Performance") %>% 
           group_by(participant, measure, description) %>% 
           summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
           ungroup() %>% 
           group_by(participant, measure) %>% 
           summarise(point_total = mean(points_scored, na.rm= TRUE)/10 )
  ) 

df %>%  
  filter(measure=="Task") %>%
  group_by(participant, measure, description) %>% 
  summarise(points_scored = median(points_scored, na.rm = TRUE)) %>%
  left_join(read_xlsx("Key_Recon.xlsx", sheet="Weightings") %>%
              select(description,global_weight) %>% 
              mutate(description = gsub("[*]", "", description))) %>%
  mutate(score_wt = points_scored*global_weight) %>% 
  mutate(point_total = 4) %>% 
  rbind( df %>% 
           filter(measure=="Performance") %>% 
           group_by(participant, measure, description) %>% 
           summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
           mutate(global_weight = 1/8) %>% 
           mutate(score_wt = global_weight*points_scored) %>% 
           mutate(point_total = 10)
  )%>% 
  rename("raw_score"="points_scored", "weight"="global_weight", "weighted_score"="score_wt", "measure_category"="measure") %>% 
  mutate_if(is.numeric, ~round(.x, 3)) %>% 
  mutate(event = "Recon") %>% 
  mutate(notes = "none") %>% 
  select(participant, event, measure_category, description,point_total, weight, raw_score,  weighted_score, notes) %>% 
  write_csv("SquadResults_Recon_RawData.csv")


df_weighted_scores %>% 
  pivot_wider(names_from = "measure", values_from = point_total) %>% 
  write.csv("SquadResults_Recon.csv")

df_weighted_scores_scaled <- df_weighted_scores %>%
  pivot_wider(names_from = "measure", values_from="point_total") %>% 
  ungroup() %>% 
  mutate_at (vars(Task:Performance), scale) %>% 
  gather(Task:Performance, key=measure, value=point_total) %>% 
  mutate(point_total=pnorm(point_total)*100)
  
  
df_weighted_scores %>% 
  ggplot(aes(x=reorder(participant, point_total, fun=mean), y=point_total, color=measure)) +
  geom_point(size=3) +
  coord_flip() +
  ylim(0,1)+
  ylab("score (proportion of total)") +
  xlab("")

df_weighted_scores_scaled %>% 
  ggplot(aes(x=reorder(participant, point_total, fun=mean), y=point_total, color=measure)) +
  geom_point(size=3, alpha=.8) +
  coord_flip() +
  ylim(0,100) +
  geom_hline(yintercept = 50, linetype = "dashed", color="black") +
  ylab("percentile") +
  xlab("")

heatmap_plot <- df %>%  
  filter(measure=="Task") %>%
  group_by(participant, measure, description) %>% 
  summarise(points_scored = median(points_scored, na.rm = TRUE)) %>%
  left_join(read_xlsx("Recon_Key.xlsx", sheet="Weightings") %>%
              select(description,global_weight) %>% 
              mutate(description = gsub("[*]", "", description))) %>%
  mutate(score_wt = points_scored*global_weight) %>% 
  group_by(participant,  measure) %>% 
  summarise(point_total = sum(score_wt)/2) %>%
  rbind( df %>% 
           filter(measure=="Performance") %>% 
           group_by(participant, measure, category) %>% 
           summarise(point_total = median(points_scored, na.rm = TRUE)) %>%
           mutate(measure = category) %>% 
           select(-category)
  ) %>% 
  pivot_wider(names_from = "measure", values_from = "point_total") %>% 
  ungroup() %>% 
  column_to_rownames("participant") %>% 
  mutate_at(vars(Task:`Fire_Effectiveness`), scale)  %>% drop_na()


cor( heatmap_plot) %>% 
  as.matrix() %>%superheat(title = "Performance Construct Inter-Correlations",
                           pretty.order.cols = TRUE,
                           pretty.order.rows = TRUE,
                           row.dendrogram = TRUE,
                           col.dendrogram = FALSE,
                           heat.pal = c("red", "red", "white", "white", "green", "green"),
                           heat.lim = c(-1,1),
                           heat.pal.values = c(0,.4, .5, .7,.75, 1),
                           X.text = round(as.matrix(cor(heatmap_plot)),1),
                           X.text.size = 2.5, 
                           bottom.label.text.angle = 90,
                           bottom.label.text.size = 3, 
                           bottom.label.size = .3,
                           bottom.label.text.alignment = "center",
                           left.label.text.size = 4, 
                           left.label.size = .3,
                           title.size = 3)

#Discriminating features
#Discriminating features
df_discrim <- df %>%
  group_by(participant, measure, category, description) %>% 
  summarise(points_scored = median(points_scored, na.rm = TRUE))




df_discrim_Task <- df_discrim %>% 
  filter(measure=="Task") %>% 
  group_by(participant, description) %>% 
  summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "description", values_from = "points_scored") %>% 
  left_join(df_weighted_scores %>% 
              pivot_wider(names_from ="measure", values_from = "point_total") %>%
              mutate(overall_score = mean(c(Task, Performance))) %>% 
              select(participant, overall_score)  ) %>%
  ungroup() %>% 
  column_to_rownames("participant") %>% 
  mutate_if(is.numeric, impute) %>% 
  mutate_if(is.numeric, scale) %>%
  as.data.frame()

df_discrim_Performance <- df_discrim %>% 
  filter(measure=="Performance") %>% 
  group_by(participant, category) %>% 
  summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "category", values_from = "points_scored") %>% 
  left_join(df_weighted_scores %>% 
              pivot_wider(names_from ="measure", values_from = "point_total") %>%
              mutate(overall_score = mean(c(Task, Performance))) %>% 
              select(participant, overall_score)  ) %>%
  ungroup() %>% 
  column_to_rownames("participant") %>% 
  mutate_if(is.numeric, impute) %>% 
  mutate_if(is.numeric, scale) %>% 
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

