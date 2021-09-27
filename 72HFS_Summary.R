library(tidyverse)
library(superheat)

read.csv("SquadResults_Recon.csv") %>% 
  mutate(event = "Recon") %>% 
  bind_rows(
    read.csv("SquadResults_STX.csv") %>% 
      mutate(event = "STX")
  ) %>% 
  bind_rows(
    read.csv("SquadResults_Raid_scaled.csv") %>% 
      mutate(event = "Raid")
  ) %>% 
  group_by(participant, event) %>% 
  summarise(total_score = mean(c(Task, Performance))) %>% 
  select(participant, event, total_score) %>%
  pivot_wider(names_from = "event", values_from = "total_score") %>% 
  ungroup() %>%
  mutate_if(is.numeric, scale) %>%  
  mutate_if(is.numeric, pnorm) %>%
  group_by(participant) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, ~round((100*.x),0)) %>% 
  gather(Raid:STX, key=event, value="score") %>% 
  mutate(note = "% of normal distribution") %>% 
  arrange(participant, event) %>% 
  write.csv("72HFS_TacticalTaskResults_SummaryReport_June2021.csv")


df_superheat <- df_perc %>% 
  mutate(color = if_else(mean<30, "skyblue", if_else(mean<70, "gray", "lightgreen"))) %>%
  separate(participant, into=c("platoon", "squad"), remove=FALSE, sep="_" ) %>% 
  mutate(color2 = if_else(platoon == "Plt1", "red", if_else(platoon =="Plt2", "darkgreen", "blue"))) %>% 
  column_to_rownames("participant")  %>%
    select(color, color2, mean, Raid, STX, Recon) %>% 
    arrange(mean)

png("results_superheat.png", height =600, width = 1200)
t(df_superheat %>% select (-mean, -color, -color2)) %>% superheat(
                            title = "72HFS Squad Performance Results (percentile scores)",
                            title.size = 8,
                            padding = 0.1,
                            
                           pretty.order.cols = FALSE,
                           pretty.order.rows = FALSE,
                           row.dendrogram = TRUE,
                           col.dendrogram = FALSE,
                           heat.pal = c("lightblue", "lightblue", "white", "white", "lightgreen", "lightgreen"),
                           heat.lim = c(0,100),
                           heat.pal.values = c(0,.3, .31, .69,.7, 1),
                           X.text = round(t(df_superheat %>% select(-mean, -color, -color2)),0),
                           X.text.size = 6, 
                           bottom.label.text.angle = 90,
                           bottom.label.text.size = 6, 
                           bottom.label.size = .3,
                           bottom.label.text.alignment = "center",
                           left.label.text.size = 6, 
                           left.label.size = .2,
                           #column.title = "Participant",
                           #row.title = "Event",
                           yt =df_superheat$mean,
                           yt.plot.type = "bar",
                           yt.axis.name = "mean percentile",
                           yt.plot.size = .5,
                           yt.axis.name.size = 14,
                           yt.num.ticks = 8,
                           yt.obs.col = df_superheat$color,
                           legend = FALSE,
                           bottom.label.text.col = df_superheat$color2,
                           left.label.text.col = "black",
                           grid.hline.col = "gray",
                           grid.vline.col = "gray",
                           grid.hline.size = 1.5,
                           grid.vline.size = 1.5
                           )
  
dev.off()

png("results_superheat_micro.png", height =300, width = 280)
as.matrix(df_superheat %>% select (-mean, -color, -color2)) %>%
  superheat(
  title = "Squad Results (percentile scores)",
  title.size = 4,
  padding = 0.1,
  
  pretty.order.cols = TRUE,
  pretty.order.rows = FALSE,
  row.dendrogram = FALSE,
  col.dendrogram = FALSE,
  heat.pal = c("lightblue", "lightblue", "white", "white", "lightgreen", "lightgreen"),
  heat.lim = c(0,100),
  heat.pal.values = c(0,.3, .31, .69,.7, 1),
  X.text = round(as.matrix(df_superheat %>% select (-mean, -color, -color2),0)),
  X.text.size = 4, 
  bottom.label.text.angle = 90,
  bottom.label.text.size = 4, 
  bottom.label.size = .3,
  bottom.label.text.alignment = "center",
  left.label.text.size = 4, 
  left.label.size = .3,
  # column.title = "Participant",
  # row.title = "Event",
  # yt =df_superheat$mean,
  # yt.plot.type = "bar",
  # yt.axis.name = "mean percentile",
  # yt.plot.size = .5,
  # yt.axis.name.size = 14,
  # yt.num.ticks = 8,
  # yt.obs.col = df_superheat$color,
  legend = FALSE,
  #bottom.label.text.col = df_superheat$color2,
  left.label.text.col = df_superheat$color2,
  grid.hline.col = "gray",
  grid.vline.col = "gray",
  grid.hline.size = 1.,
  grid.vline.size = 1.
)

dev.off()

#output total raw data file
read.csv("SquadResults_STX_RawData.csv") %>% 
  bind_rows(
    read.csv("SquadResults_Recon_RawData.csv") 
    ) %>% 
  bind_rows(
    read.csv("SquadResults_Raid_RawData.csv")
    ) %>% 
  arrange(participant, event, measure_category) %>% 
  rename("note"="notes") %>% 
  write.csv("72HFS_TacticalTaskResults_DetailedReport_June2021.csv")

