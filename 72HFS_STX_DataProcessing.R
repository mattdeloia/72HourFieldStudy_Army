library(readxl)
library(janitor)
library(tidyverse)
library(DT)
library(superheat)
library(Hmisc)

getwd()

df <- read_xlsx("72HFS_STX_Raw Data_2022.xlsx") %>%
  # read_xlsx("72HFS_STX_Raw Data.xlsx") %>% 
  clean_names() %>%
  filter(result=="PointsBased") %>% 
  left_join(
    read_xlsx("Key_STX.xlsx", sheet = "Key") %>% 
      clean_names()
    ) %>% 
  rename(participant = participant_name) %>% 
  select(participant,measure, category, description, points_scored, grader_name, feature) %>%
  mutate(feature = as.factor(feature)) %>% 
  drop_na(category)

observation_check <- df %>%  
  group_by(participant, feature) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "feature", values_from = "count")

df %>% filter(measure =="Performance") %>% 
  ggplot(aes(x=points_scored,  y=grader_name)) + geom_boxplot() + facet_grid (measure~., scales = "free")

df %>%  filter(measure =="Performance") %>% group_by(grader_name, measure, points_scored) %>% summarise(count = n()) %>% 
  ggplot(aes(x=points_scored, y = count, fill=grader_name)) + geom_col(position = "dodge2") + facet_grid (measure~., scales = "free")

df %>%  filter(measure =="Task") %>%
  ggplot(aes(x=feature, y=points_scored)) + 
  geom_boxplot()

df %>% left_join(read_xlsx("Key_STX.xlsx", sheet = "Weightings") %>% select(description,weight)) %>% 
  mutate_at(vars(weight), ~round(.x,3)) %>% 
  filter(measure =="Task") %>%
  group_by(feature, weight) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=reorder(feature, n, fun=mean), y=n, label = weight)) + geom_col() + geom_text(color="blue", nudge_y = 1) + coord_flip()

df %>% filter(measure =="Performance") %>% group_by(category) %>% summarise(n = n()) %>% 
  ggplot(aes(x=reorder(category, n, fun=mean), y=n)) + geom_col() + coord_flip()


df %>% 
  left_join(read_xlsx("Key_STX.xlsx", sheet = "Weightings") %>% select(description,weight)) %>% 
  mutate_at(vars(weight), ~round(.x,2)) %>%
  filter(measure =="Task") %>%
  separate(participant, into = c("plt", "sqd"), sep="Sqd") %>% 
  group_by(plt, feature, weight) %>% 
  summarise(n = n()) %>%
  mutate(color = if_else(n<3, "very limited", 
                         if_else(n<6, "limited", "good"))) %>% 
  ggplot(aes(x=feature, y=n, fill=color, label=weight)) +
  geom_col() +
  geom_text(color="blue") +
  scale_fill_manual(values=c("very limited"="red", "limited"="skyblue", "good" = "lightgreen"))+
  coord_flip() + facet_grid(.~plt) + ylab("# of observations") +
  labs(title = "STX Number of Observations by Measure", subtitle="measure weightings in blue")
ggsave("Observation_report.jpg", width = 12, height = 6, units = "in")



#variability analysis

df %>% 
  filter(measure =="Task") %>% 
  group_by(participant, description) %>% 
  summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
  group_by(description) %>% 
  summarise(sd = round(sd(points_scored),2)) %>%
  mutate(sd_zscore = scale(sd)) %>% 
  mutate(variability = if_else(sd_zscore < (-.5), "low", if_else(sd_zscore<.5, "medium", "high"))) %>% 
  write.csv("measure_variability_Task_STX.csv")

#weighted scores analysis on three methods
df_weighted_scores <- df %>%  
  filter(measure=="Task") %>%
  group_by(participant, measure, description) %>% 
  summarise(points_scored = median(points_scored, na.rm = TRUE)) %>%
  left_join(read_xlsx("Key_STX.xlsx", sheet="Weightings") %>%
              select(description,weight)
            )%>%
  mutate(score_wt = points_scored*weight) %>% 
  group_by(participant,  measure) %>% 
  summarise(point_total = sum(score_wt)/2) %>%
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
  pivot_wider(names_from = "description", values_from = "points_scored") %>% 
  ungroup() %>% 
  mutate_if(is.numeric, impute) %>% 
  gather(3:47, key="description", value="points_scored") %>%
  left_join(read_xlsx("Key_STX.xlsx", sheet="Weightings") %>%
              select(description,weight)
  )%>%
  mutate(score_wt = points_scored*weight) %>% 
  group_by(participant,  measure) %>%
  mutate(point_total = 2) %>% 
  rbind( df %>% 
           filter(measure=="Performance") %>% 
           group_by(participant, measure, description) %>% 
           summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
           pivot_wider(names_from = "description", values_from = "points_scored") %>% 
           ungroup() %>% 
           mutate_if(is.numeric, impute) %>% 
           gather(3:17, key="description", value="points_scored") %>% 
           mutate(weight = 1/15) %>% 
           mutate(score_wt = weight*points_scored) %>% 
           mutate(point_total = 10)
           
  ) %>% 
  rename("raw_score"="points_scored", "weight"="weight", "weighted_score"="score_wt", "measure_category"="measure") %>% 
  mutate_if(is.numeric, ~round(.x, 3)) %>% 
  mutate(event = "React_to_Contact_STX") %>% 
  mutate(notes = "none") %>% 
  select(participant, event, measure_category, description, point_total, weight, raw_score, weighted_score, notes) %>% 
  write_csv("SquadResults_STX_RawData.csv")

df_weighted_scores %>% 
  pivot_wider(names_from ="measure", values_from = "point_total") %>% 
  mutate(STX_score = (Task+Performance)/2) %>% 
  select(participant, STX_score) %>% 
  write_csv("SquadResults_STX_2022.csv")

pnorm()

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
ggsave("STX_weighted_unscaled_scores.jpg", width = 6, height = 5, units = "in")



df_weighted_scores_scaled %>% 
  ggplot(aes(x=reorder(participant, point_total, fun=mean), y=point_total, color=measure)) +
  geom_point(size=3, alpha=.8) +
  coord_flip() +
  ylim(0,100) +
  geom_hline(yintercept = 50, linetype = "dashed", color="black") +
  ylab("percentile") +
  xlab("") +
  ggtitle("Squad Results (scaled)")

ggsave("STX_weighted_scaled_scores.jpg", width = 6, height = 5, units = "in")



#correlation of measures
heatmap_plot <- 
    df %>% 
           filter(measure=="Performance") %>% 
            select(-feature, -description, -measure) %>% 
  pivot_wider(names_from = "category", values_from = "points_scored") %>% 
  ungroup() %>% 
  select(-grader_name, -participant) %>% 
  mutate_at(vars(Security:`Rehearsals`), impute) %>% 
  mutate_at(vars(Security:`Rehearsals`), scale) 
  
png("correlation_superheat.png", height =600, width = 1200)
cor( heatmap_plot) %>% 
  as.matrix() %>%superheat(title = "Tactical Principles Inter-Correlations (STX Lane)",
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

df %>%  
  filter(measure=="Task") %>%
  group_by(participant, measure, grader_name) %>% 
  summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>%
  mutate(point_total = points_scored/2 ) %>% 
  rbind( df %>% 
           filter(measure=="Performance") %>% 
           group_by(participant, measure, grader_name) %>% 
           summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% 
                   mutate(point_total = points_scored/10 )
  ) %>% filter(measure=="Performance") %>% 
  ggplot(aes(x=reorder(participant, point_total, fun=mean), y=point_total)) + 
  geom_point(color="gray", size=4) +
  geom_line(color="darkgray") +
  ylim(0,1) +
  stat_summary(fun.y = "median", geom="point", color="red", size = 4) +
  coord_flip() + xlab("") + ylab("proportion of total score") +
  ggtitle("Group Judgement (STX Lane)") +
 # labs(caption = "Note: Gray - OC rating; Red - group rating") +
  ggsave("OC_Perfomrance_ratings.png", width = 6.5, height = 5.2, units="in")


#Discriminating features
df_discrim <- df %>%
  group_by(participant, measure, category, description) %>% 
  summarise(points_scored = median(points_scored, na.rm = TRUE)) 



df_discrim_Task <- df_discrim %>% 
  filter(measure=="Task") %>% 
  ungroup() %>% 
  select(participant, description, points_scored) %>% 
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
df_fa  <- df %>% filter(!feature %in% c("7", "8", "12", "13", "17", "18", "29", "32", "37", "38", "42", "43")) %>% 
  group_by(participant, grader_name, feature) %>% 
  summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% 
  arrange(feature) %>% pivot_wider(names_from ="feature", values_from = "points_scored") %>% as.data.frame() %>% select(-participant, -grader_name) %>% 
  mutate_if(is.numeric, impute)

fa_cor <- cor(df_fa, use = "pairwise.complete")
scree(fa_cor)

factors_data <- 
  fa(r = fa_cor, nfactors = 4)

total_scales <- 
  paste("V",4, sep="")

factors_data$loadings
df_efa <- factors_data[["loadings"]] %>% 
  as.matrix.data.frame() %>% 
  as.data.frame()%>% 
  cbind(
    df %>% filter(!feature %in% c("7", "8", "12", "13", "17", "18", "29", "32", "37", "38", "42", "43")) %>% arrange(feature) %>% 
      select(feature) %>% 
      unique() %>% rename(Item = feature)) %>% 
  gather(V1:V4, key="Scale", value="Loading") %>% 
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
pca_output <- prcomp(df_fa)
pca_output$rotation
fviz_eig(pca_output, ncp=26 )
