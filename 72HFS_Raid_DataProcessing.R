library(readxl)
library(janitor)
library(tidyverse)
library(DT)
library(superheat)
library(stringr)
library(Hmisc)

filter_out <- c("Establish a Patrol Base Metrics",
                "Priorities of Work",
                "TLPs",
                "SL issues a FRAGO to subordinates immediately after the initial assessment, which includes but is not limited to the following information:",
                "Unit conducts rehearsals to accomplish the following actions:",
                "Conduct Precombat Inspections/Checks",
                "Conduct Tactical Movement",
                "Actions on the Objective",
                "1. The squad leader",
                "2. The team leader (normally the number two Soldier) does the following:",
                "3. The first two Soldiers enter the room almost simultaneously as follows:",
                "7. The clearing team leader scans and clears the room by",
                "9. The squad leader enters the room and",               
                "e. Marks entry point according to the unit SOP.",
                "Establish CASEVAC")

performance_measures <- c('Security',
                          'Control',
                          'Weapons Handling',
                          'Communication',
                          'Information Exchange',
                          'Initiative / Leadership',
                          'Supporting Behavior',
                          'Speed',
                          'Cover and Concealment',
                          'Tactical and Technical Proficiency',
                          'Overall quality of this phase of the operation',
                          'Quality of Weapons Maintenance plan and execution',
                          'Quality of Squad Equipment Maintenance Plan and execution',
                          'Quality of Squad personal hygiene plan and execution',
                          'Overall Quality of the FRAGO',
                          'Overall Quality of the Rehearsal',
                          'Noise and light discipline',
                          'Overall impression of PCIs/PCCs',
                          'Troop disbursement/distance/interval/formation',
                          'Tactical/Technical proficiency',
                          'Reconaissance',
                          'Communication/Signaling',
                          'Planning',
                          'Noise/Light Discipline',
                          'Information Exchange/Lateral-Mutual Communication',
                          'Initiative/Leadership',
                          'Overall impression of Movement performance',
                          'Communcation',
                          'Surprise',
                          'Violence of Action',
                          'Fire Effectiveness (rate of fire and direction of fire)',
                          'Simplicity',
                          'Overall quality of Actions on the OBJ',
                          'Overall quality of Actions on the Objective')


phase <- rep(rep(c("Patrol Base", "Priorities", "TLPs", "PCIs", "Movement", "Actions on Objective", "CASEVAC"), c(17, 14, 18, 6, 16, 43,23)), 32) 

overall_measures <- c('Overall quality of this phase of the operation',
                      'Overall Quality of the FRAGO',
                      'Overall Quality of the Rehearsal',
                      'Overall impression of PCIs/PCCs',
                      'Overall impression of Movement performance',
                      'Overall quality of Actions on the OBJ',
                      'Overall quality of Actions on the Objective')

na_measures <- c("b. Ensuring all noncombatants are secured.", 
                 "R&S Patrol",
                 "Positions Checked and OPs relieved",
                 "5. The fourth Soldier moves opposite of the third Soldier to a position dominating their sector.",
                 "Squad clears CASEVAC site of potential obstacles and hazards",
                 "d. Requests needed sustainment to continue clearing the squad's sector.",
                 "Control point marked",
                 "Safe and secure location established for in coming casualties",
                 "SL establishes control point for follow on aid & litter",
                 "b. The time and place for issuing the OPORD."
                 )

df <- 
  read_xlsx('72HFS_Raid_Raw Data_2022.xlsx') %>% 
  #read_csv("72HFS_Raid_Raw Data.csv") %>% 
  clean_names() %>%
  rename(participant = participant_name) %>% 
  rename(grader = grader_name) %>% 
  filter(grader != "Raid R OC4") %>% 
  mutate(phase = phase) %>%
  filter(result=="PointsBased") %>%
  mutate(description = gsub("[*]", "", description)) %>%
  mutate(description = gsub("BEhavior", "Behavior", description)) %>%
  mutate(description = gsub("Communcation", "Communication", description)) %>%
  mutate_at(vars(description), str_trim ) %>% 
  filter(!(description %in% c(filter_out))) %>%   #removes construct outline
  mutate(measure = if_else(description %in% c(performance_measures), "Performance", "Task")) %>% 
  select(participant, phase, grader, measure, description, points_scored) 
  
df %>% select(measure, phase, description) %>% unique() %>%  group_by(measure) %>% summarise(count = n())
df %>% select(phase,measure, description) %>%
  unique() %>%
  group_by(phase, measure) %>%
  summarise(count = n()) %>%
  write.csv("measures_by_phase_total.csv")



variability_report <- df %>% 
  filter(!(description %in% c(overall_measures))) %>%   #remove overall measures
  filter(!(description %in% c(na_measures))) %>%   #remove measures not consistently rated
  group_by(grader, phase, measure, description) %>% 
  summarise(measure_sd = sd(points_scored)) %>%
 # mutate_at(vars(measure_sd), ~replace_na(.x, 0)) %>% 
  group_by(phase,description, measure) %>% 
  summarise(average_OC_sd = mean(measure_sd), median_OC_sd = median(measure_sd)) %>% 
  arrange(phase, measure, average_OC_sd) %>% 
  mutate(rating = if_else(measure =="Performance"& average_OC_sd <.48 |measure =="Task" & average_OC_sd<.18, "poor", "good")) %>%
  mutate_at(vars(4:5), ~round(.x, 3))

write_csv(variability_report, "variability_report.csv")

variability_report %>% filter(measure == "Task") %>% group_by(measure) %>% summary(average_OC_sd)
variability_report %>% filter(measure == "Performance") %>% group_by(measure) %>% summary(average_OC_sd)
variability_report %>% filter(rating =="good") %>% group_by( measure) %>% summarise(count = n())

variability_report %>%
  ggplot(aes(x=average_OC_sd)) + geom_histogram(bins=30, fill="skyblue") + 
  ylim(0,5) + 
  facet_grid(phase~., scales = "free") + 
  ylab("count of measures") +
  theme(axis.text = element_text(size = 10), axis.title = element_text (size=10)) +
  ggtitle(label = "Variability Report of OC Ratings")+
  facet_grid(phase~measure, scales = "free") +
  xlab("average OC rating sd") +
  labs(caption = "Does not include overall quality / impression measures")
ggsave("variability_report.png", width = 7, height=6, units = "in")

observation_check_squad <- df %>%
  group_by(participant, measure, phase, description) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "participant", values_from = "count")

#squad scores analysis 
# squad_performance <- df %>% 
#   filter(!(description %in% c(overall_measures))) %>%   #remove overall measures
#   filter(!(description %in% c(na_measures))) %>%
#   group_by(participant, phase, measure, description) %>% 
#   summarise(points = mean(points_scored, na.rm = TRUE)) %>%
#   ungroup() %>% 
#   left_join(variability_report) %>% 
#   filter(if_else(measure == "Performance", average_OC_sd >  .5, average_OC_sd >  .2 ))  #variability thresholds
# 
# squad_performance %>% select(measure, phase, description) %>% unique() %>% 
#   group_by(measure, phase) %>% summarise(count = n()) %>% 
#   group_by(phase) %>% 
#   summarise(total = sum(count)) %>% 
#   mutate(weight = total/sum(total))
 unique( (df %>% 
      filter(!(description %in% c(overall_measures))) %>%   #remove overall measures
      filter(!(description %in% c(na_measures))) %>%
      group_by(participant, phase, measure, description) %>% 
      summarise(points = median(points_scored, na.rm = TRUE)) %>%
      ungroup() %>% 
      left_join(variability_report))$phase)

squad_performance_scored <- function(performance_min, task_min) { 
  sd_value <- 
    df %>% 
  filter(!(description %in% c(overall_measures))) %>%   #remove overall measures
  filter(!(description %in% c(na_measures))) %>%
  group_by(participant, phase, measure, description) %>% 
  summarise(points = median(points_scored, na.rm = TRUE)) %>%
  ungroup() %>% 
  left_join(variability_report) %>%
  filter(if_else(measure == "Performance", average_OC_sd >=  performance_min, average_OC_sd >=  task_min )) %>%  
  group_by(participant, measure,  phase) %>%
  summarise(score = mean(points)) %>%
    mutate(category = if_else(phase %in% c("Patrol Base", "Priorities", "TLPs"), "Raid PB/TLPs", "Raid Execution")) %>%
    mutate(score = if_else(measure=="Performance", score/10, score/2))   %>%
  group_by(participant, category, phase) %>%
  summarise(score = mean(score)) %>%
    left_join(
      df %>% 
          filter(!(description %in% c(overall_measures))) %>%   #remove overall measures
          filter(!(description %in% c(na_measures))) %>%
          left_join(variability_report) %>%
        
          filter(if_else(measure == "Performance", average_OC_sd >=  performance_min, average_OC_sd >=  task_min ))%>%
        select(measure, phase, description) %>%
        unique() %>%
        mutate(category = if_else(phase %in% c("Patrol Base", "Priorities", "TLPs"), "Raid PB/TLPs", "Raid Execution")) %>%
        group_by(category, phase, measure) %>% 
        summarise(count = n()) %>%
        group_by(category, phase) %>%
        summarise(total = sum(count)) %>%
        mutate(weight = total/sum(total)) %>%
        select(phase, weight)

    )  
  %>%
  group_by(participant, category) %>%
  summarise(total_score = sum(score*weight))
  sd_value2 <- diff(range(sd_value$total_score))
   return(sd_value)
}


all <- squad_performance_scored(0,0) %>% rename(all = total_score)
reduced <- squad_performance_scored(.48,.18) %>% rename(reduced = total_score)

reduced %>% 
  mutate_if(is.numeric, ~round(.x, 2)) %>% 
  rename(event = category) %>% 
  mutate(point_total = if_else(measure =="Performance", 10, 2)) %>% 
  rename(raw_score = points) %>% 
  mutate(notes=phase) %>% 
  mutate(weight = 1) %>% 
  mutate(weighted_score = weight*raw_score) %>% 
  select(participant, event, measure, description, point_total, weight, raw_score, weighted_score, notes) %>% 
  rename(measure_category=measure) %>% 
  arrange(participant, event, notes, measure_category) %>% 
  write.csv("SquadResults_Raid_RawData.csv")


output_task_optimize <- NULL;

for (i in seq(from = 0, to= .54, by = .01)) {
  temp <- squad_performance_scored(1.6, i) %>% 
    pivot_wider(names_from = "participant", values_from = "total_score") %>% 
    mutate(OC_rating_SDmin = i)
  
  output_task_optimize <- bind_rows(output_task_optimize, temp)
}

output_task_optimize %>% gather(Plt4Sqd1:Plt6Sqd3, key=squad, value=score) %>% 
  ggplot(aes(x=OC_rating_SDmin, y=score, color=squad, group = squad)) + geom_point() + geom_line() + geom_vline(xintercept = .18, linetype = "dashed") + ggtitle("Sensitivity Analysis: Task Measures")


output_performance_optimize <- NULL;

for (i in seq(from = 0, to= 1.6, by = .01)) {
  temp <- squad_performance_scored(i, .18) %>% 
  pivot_wider(names_from = "participant", values_from = "total_score") %>% 
  mutate(OC_rating_SDmin = i)
 
  output_performance_optimize <- bind_rows(output_performance_optimize, temp)
}

output_performance_optimize %>% gather(Plt4Sqd1:Plt6Sqd3, key=squad, value=score) %>% 
  ggplot(aes(x=OC_rating_SDmin, y=score, color=squad, group = squad)) + geom_point() + geom_line() + geom_vline(xintercept = .48, linetype = "dashed") + ggtitle("Sensitivity Analysis: Performance Measures")




#reduced %>% mutate_if(is.numeric, ~round(.x, 2)) %>% write.csv("SquadResults_Raid_Detailed.csv")


impression <- df %>% 
  filter(description %in% c(overall_measures)) %>%   #filter in overall measures
  mutate(category = if_else(phase %in% c("Patrol Base", "Priorities", "TLPs"), "Raid PB/TLPs", "Raid Execution")) %>% 
  group_by(participant, category, phase, measure, description) %>% 
  summarise(points = median(points_scored, na.rm = TRUE)) %>%
  group_by(participant, category, measure, phase) %>% 
  summarise(score = mean(points)/10) %>%   
  group_by(category, participant) %>% 
  summarise(total_score = mean(score)) %>% 
  rename(impression = total_score)


score_compare <- all %>% 
  left_join(reduced) %>% 
  left_join(impression) %>% 
  mutate_if(is.numeric, ~round(.x, 3)) %>% 
  group_by(category) %>% 
  mutate(rank = rank(impression)) 
  

score_compare %>% 
  gather(all:impression, key = measure, value = point_total) %>% 
  ggplot(aes(x=reorder(participant, rank, fun=mean), y=point_total, color=measure, group = measure)) +
  geom_point(aes(size=measure)) +
  geom_line(aes(linetype = measure)) +
  scale_linetype_manual(values = c("blank", "dashed", "blank")) +
  scale_color_manual(values = c("green", "blue", "red")) +
  coord_flip() +
  ylab("score (proportion of total)") +
  xlab("") +
  ggtitle("Squad Results")+
  scale_size_manual(values = c(3, 4, 2 )) +
  facet_grid(.~category)
ggsave("Raid_scores.jpg", width = 6, height = 5, units = "in")


 

cor(score_compare$impression, score_compare$all)
 cor(score_compare$impression, score_compare$reduced)
 cor(score_compare$all, score_compare$reduced)

 df_superheat <- 
 
 library(superheat)
 png("scores_superheat_raid_RaidPB_TLPs.png", height =300, width = 300)
 score_compare %>% 
   ungroup() %>% 
   filter(category =="Raid PB/TLPs") %>% 
   select(-category, -rank) %>%  
   column_to_rownames("participant") %>% 
   superheat(title = "Scores (Raid PB/TLPs)",
                            title.size = 6,
                            padding = 0.1,
                            pretty.order.cols = FALSE,
                            pretty.order.rows = FALSE,
                            row.dendrogram = FALSE,
                            col.dendrogram = FALSE,
                            heat.pal = c("red", "red", "white", "white", "lightgreen", "lightgreen"),
                            # heat.lim = c(.421,.511),
                            # heat.pal.values = c(0,.25,.26, .5, .74,.75, 1),
                            X.text = round(score_compare %>% 
                                             ungroup() %>% 
                                             filter(category =="Raid PB/TLPs") %>% 
                                             select(-category, -rank) %>%  
                                             column_to_rownames("participant")
                              
                              ,2) %>% as.matrix(),
                            X.text.size = 6, 
                            bottom.label.text.angle = 90,
                            bottom.label.text.size = 6, 
                            bottom.label.size = .5,
                            bottom.label.text.alignment = "center",
                            left.label.text.size = 6, 
                            left.label.size = .3, 
                            legend = FALSE)
 
 dev.off()
 
 

reduced %>% rename(Raid_scores = reduced) %>% pivot_wider(names_from = "category", values_from = "Raid_scores") %>% 
  write.csv("SquadResults_Raid_2022.csv")


########

##OC scoring behaviors
df %>% 
  mutate(points_scored = if_else(measure=="Performance", points_scored/10, points_scored/2)) %>% 
  ggplot(aes(x=points_scored)) +
  geom_density(aes(fill=grader, color=grader), alpha = .5, adjust=1.5) +
  facet_grid(measure~., scales = "free") +
  theme(legend.position = "top") +
  xlab("proportion of total score") +
  ylab("") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Individual Judgement (level error)")  
  ggsave("Grader_score_dist.png", width = 6.5, height = 5.2, units = "in")

df %>% 
  group_by(grader, measure) %>% 
  summarise(ave_rating = mean(points_scored, na.rm=TRUE), sd = sd(points_scored, na.rm=TRUE)) %>% 
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

#factor analysis  
#factor analysis  
library(psych)
df_fa  <- df %>% filter(!feature %in% c("P7")) %>% 
  drop_na(feature) %>% 
  group_by(participant, grader, feature) %>% 
  summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% 
  arrange(feature) %>% pivot_wider(names_from ="feature", values_from = "points_scored") %>% as.data.frame() %>% select(-participant, -grader) %>% 
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
    df %>% filter(!feature %in% c("P7")) %>%
      drop_na(feature) %>% 
      arrange(feature) %>% 
      ungroup() %>% 
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
  filter(LoadRank<=25) %>% 
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
