library(readxl)
library(janitor)
library(tidyverse)
library(DT)
library(superheat)
library(Hmisc)

getwd()

df <- 
  read_xlsx("72HFS_Recon_Raw Data_2022.xlsx") %>% 
  clean_names() %>%
  mutate(description = gsub("[*]", "", description)) %>% 
  mutate(description = trimws(description)) %>% 
  mutate(points_scored = if_else(description =="(1) Answers higher HQ information requirements of both CCIR and SIR." & participant_name=="Plt4Sqd3", 2, points_scored )) %>% 
  mutate(result = if_else(description =="(1) Answers higher HQ information requirements of both CCIR and SIR." & participant_name=="Plt4Sqd3", "PointsBased", result )) %>% 
  filter(result=="PointsBased") %>% 
  left_join(
    read_xlsx("Key_Recon.xlsx", sheet = "Key") %>% 
      mutate(description = gsub("[*]", "", description)) %>% 
      mutate(description = trimws(description))
  ) %>% 
  rename(participant = participant_name) %>% 
  select(participant,measure, category, description, points_scored, grader_name, feature) %>%
  mutate(feature = as.factor(feature)) %>% 
  drop_na(measure)

df %>% group_by(grader_name, measure, points_scored) %>% summarise(count = n()) %>% 
  ggplot(aes(x=points_scored, y = count, fill=grader_name)) + 
  geom_col(position = "dodge2") + 
  facet_grid (measure~., scales = "free")

df %>% 
  ggplot(aes(x=points_scored,  y=grader_name)) + geom_boxplot() + facet_grid (measure~., scales = "free")


observation_check <- df %>%  
  group_by(participant, feature) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "feature", values_from = "count")
  
  
df %>%  filter(measure =="Task") %>%
  ggplot(aes(x=feature, y=points_scored)) + 
  geom_boxplot()

 df %>% 
  left_join(read_xlsx("Key_Recon.xlsx", sheet = "Weightings") %>% 
              mutate(description = gsub("[*]", "", description)) %>% 
              mutate(description = trimws(description)) %>% 
              rename(weight = global_weight) %>% select(description,weight)) %>% 
  mutate_at(vars(weight), ~round(.x,2)) %>%
  filter(measure =="Task") %>%
  separate(participant, into = c("plt", "sqd"), sep="Sqd") %>% 
  group_by(plt, feature, weight) %>% 
  summarise(n = n()) %>%
  mutate(color = if_else(n<3, "very limited", 
                         if_else(n<6, "limited", "good"))) %>% 
  ggplot(aes(x=feature, y=n, fill=color, label=weight)) +
  geom_col() +
  ylim(0,10) +
  geom_text(color="blue") +
  scale_fill_manual(values=c("very limited"="red", "limited"="skyblue", "good" = "lightgreen"))+
  coord_flip() + facet_grid(.~plt) + ylab("# of observations") +
  labs(title = "Recon Number of Observations by Feature", subtitle = "measure weightings in blue")
ggsave("Observation_report.jpg", width = 12, height = 6, units = "in")

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
df_weighted_scores <- 
  df %>%  
  filter(measure=="Task") %>%
  group_by(participant, measure, description) %>% 
  summarise(points_scored = median(points_scored, na.rm = TRUE)) %>%
  left_join(read_xlsx("Key_Recon.xlsx", sheet="Weightings") %>%
              select(description,global_weight) %>% 
              mutate(description = gsub("[*]", "", description)) %>% 
              mutate(description = trimws(description))
            )%>% 
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

cor((df_weighted_scores %>% pivot_wider(names_from = "measure", values_from = "point_total"))$Task, (df_weighted_scores %>% pivot_wider(names_from = "measure", values_from = "point_total"))$Performance)

df %>%  
  filter(measure=="Task") %>%
  group_by(participant, measure, description) %>% 
  summarise(points_scored = median(points_scored, na.rm = TRUE)) %>%
  left_join(read_xlsx("Key_Recon.xlsx", sheet="Weightings") %>%
              select(description,global_weight) %>% 
              mutate(description = gsub("[*]", "", description)) %>% 
              mutate(description = trimws(description))
  )%>% 
  mutate(score_wt = points_scored*global_weight) %>% 
  mutate(point_total = 4) %>% 
  rbind( df %>% 
           filter(measure=="Performance") %>% 
           group_by(participant, measure, description) %>% 
           summarise(points_scored = median(points_scored, na.rm = TRUE)) %>%
           pivot_wider(names_from = "description", values_from = "points_scored") %>% 
           ungroup() %>% 
           mutate_if(is.numeric, impute) %>% 
           gather(3:11, key="description", value="points_scored") %>% 
           mutate(global_weight = 1/9) %>% 
           mutate(score_wt = global_weight*points_scored) %>% 
           mutate(point_total = 10)
  )%>% 
  rename("raw_score"="points_scored", "weight"="global_weight", "weighted_score"="score_wt", "measure_category"="measure") %>% 
  mutate_if(is.numeric, ~round(.x, 3)) %>% 
  mutate(event = "Recon") %>% 
  mutate(notes = "none") %>% 
  select(participant, event, measure_category, description,point_total, weight, raw_score,  weighted_score, notes) %>% 
  arrange(participant, measure_category) %>% 
  write_csv("SquadResults_Recon_RawData.csv")


df_weighted_scores %>% 
  pivot_wider(names_from = "measure", values_from = point_total) %>% 
  group_by(participant) %>% 
  summarise(Recon_scores = (Task + Performance)/2) %>% 
  write.csv("SquadResults_Recon_2022.csv")

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
  xlab("") +
  ggtitle("Squad Results (unscaled)")
ggsave("Recon_weighted_unscaled_scores.jpg", width = 6, height = 5, units = "in")

df_weighted_scores_scaled %>% 
  ggplot(aes(x=reorder(participant, point_total, fun=mean), y=point_total, color=measure)) +
  geom_point(size=3, alpha=.8) +
  coord_flip() +
  ylim(0,100) +
  geom_hline(yintercept = 50, linetype = "dashed", color="black") +
  ylab("percentile") +
  xlab("") +
  ggtitle("Squad Results (scaled)")

ggsave("Recon_weighted_scaled_scores.jpg", width = 6, height = 5, units = "in")

heatmap_plot <- df %>% 
           filter(measure=="Performance") %>% 
            select(-description, -feature) %>%  
  pivot_wider(names_from = "category", values_from = "points_scored") %>% 
  ungroup() %>% 
  select(-participant, -measure, -grader_name) %>% 
  mutate_at(vars(1:9), impute) %>% 
  mutate_at(vars(1:9), scale) 

library(superheat)
png("correlation_superheat.png", height =600, width = 1200)
cor( heatmap_plot) %>% 
  as.matrix() %>%superheat(title = "Tactical Principles Inter-Correlations (Recon Lane)",
                           title.size = 8,
                           padding = 0.1,
                           pretty.order.cols = TRUE,
                           pretty.order.rows = TRUE,
                           row.dendrogram = TRUE,
                           col.dendrogram = FALSE,
                           heat.pal = c("red", "red", "white", "white", "lightgreen", "lightgreen"),
                           heat.lim = c(-1,1),
                           heat.pal.values = c(0,.25,.26, .5, .74,.75, 1),
                           X.text = round(as.matrix(cor(heatmap_plot)),1),
                           X.text.size = 6, 
                           bottom.label.text.angle = 90,
                           bottom.label.text.size = 6, 
                           bottom.label.size = .5,
                           bottom.label.text.alignment = "center",
                           left.label.text.size = 6, 
                           left.label.size = .3, 
                           legend = FALSE)

dev.off()


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

#factor analysis  
library(psych)
fa_task  <- df %>% filter(measure =="Task", !feature %in% c("23", "27", "33", "34", "35")) %>% 
  group_by(participant, grader_name, feature) %>% 
  summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% 
  arrange(feature) %>% pivot_wider(names_from ="feature", values_from = "points_scored") %>% as.data.frame() %>% select(-participant, -grader_name) %>% 
  mutate_if(is.numeric, impute)

task_cor <- cor(fa_task, use = "pairwise.complete")
scree(task_cor)

factors_data <- 
  fa(r = task_cor, nfactors = 6)

total_scales <- 
  paste("V",6, sep="")

factors_data$loadings
df_efa <- factors_data[["loadings"]] %>% 
  as.matrix.data.frame() %>% 
  as.data.frame()%>% 
  cbind(
    df %>% filter(measure =="Task", !feature %in% c("23", "27", "33", "34", "35")) %>% arrange(feature) %>% 
      select(feature) %>% 
      unique() %>% rename(Item = feature)) %>% 
  gather(V1:V6, key="Scale", value="Loading") %>% 
  mutate(Scale = gsub("V", "Scale_", Scale)) %>%
  mutate(Scale = as.factor(Scale) ) %>% 
  #right_join(df_question) %>% 
  dplyr::group_by(Item) %>% 
  mutate(rank = rank(-1*abs(Loading))) %>% 
  filter(rank==1) %>%  
  dplyr::group_by(Scale) %>% 
  mutate(LoadRank = rank(-1*abs(Loading))) %>% 
  filter(LoadRank<=15) %>% 
  ungroup() %>% 
  arrange(Scale, LoadRank) %>% 
  dplyr::select(Item, Scale, LoadRank, Loading) %>% 
  select(Scale, Item, Loading) %>% 
  arrange(Scale, -Loading)

fa_performance  <- df %>% filter(measure =="Performance") %>% 
  group_by(participant, grader_name, feature) %>% 
  summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% 
  arrange(feature) %>% pivot_wider(names_from ="feature", values_from = "points_scored") %>% as.data.frame() %>% select(-participant, -grader_name) %>% 
  mutate_if(is.numeric, impute)

performance_cor <- cor(fa_performance, use = "pairwise.complete")
scree(performance_cor)

factors_data <- 
  fa(r = performance_cor, nfactors = 2)

total_scales <- 
  paste("V",2, sep="")

factors_data$loadings
df_efa_performance <- factors_data[["loadings"]] %>% 
  as.matrix.data.frame() %>% 
  as.data.frame()%>% 
  cbind(
    df %>% filter(measure =="Performance") %>% arrange(feature) %>% 
      select(feature) %>% 
      unique() %>% rename(Item = feature)) %>% 
  gather(V1:V2, key="Scale", value="Loading") %>% 
  mutate(Scale = gsub("V", "Scale_", Scale)) %>%
  mutate(Scale = as.factor(Scale) ) %>% 
  #right_join(df_question) %>% 
  dplyr::group_by(Item) %>% 
  mutate(rank = rank(-1*abs(Loading))) %>% 
  filter(rank==1) %>%  
  dplyr::group_by(Scale) %>% 
  mutate(LoadRank = rank(-1*abs(Loading))) %>% 
  filter(LoadRank<=15) %>% 
  ungroup() %>% 
  arrange(Scale, LoadRank) %>% 
  dplyr::select(Item, Scale, LoadRank, Loading) %>% 
  select(Scale, Item, Loading) %>% 
  arrange(Scale, -Loading)

library(psych)
library(FactoMineR)
library(factoextra)
pca_output <- prcomp(fa_task)
pca_output$rotation
fviz_eig(pca_output, ncp=26 )
