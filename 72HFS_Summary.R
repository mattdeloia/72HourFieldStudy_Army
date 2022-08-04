library(tidyverse)
library(superheat)
library(janitor)

read.csv("SquadResults_Recon_2022.csv") %>% as.data.frame() %>% 
  left_join(
    read.csv("SquadResults_STX_2022.csv" )%>% as.data.frame()
  ) %>% 
  left_join(
    read.csv("SquadResults_Raid_2022.csv") %>% as.data.frame())  %>% 
  mutate_if(is.numeric, scale) %>%
  mutate_if(is.numeric, pnorm) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~round((100*.x),1)) %>%
  gather(Recon_scores:Raid_PB_TLPs, key=event, value="score") %>%
  mutate(note = "% of normal distribution") %>%
  arrange(participant, event) %>%
  write.csv("72HFS_2022_Results_SummaryReport_4Aug2022.csv")


df_superheat <- 
  read.csv("SquadResults_Recon_2022.csv") %>% as.data.frame() %>% 
  left_join(
    read.csv("SquadResults_STX_2022.csv" )%>% as.data.frame()
  ) %>% 
  left_join(
    read.csv("SquadResults_Raid_2022.csv") %>% as.data.frame())  %>% 
  mutate_if(is.numeric, scale) %>%
  mutate_if(is.numeric, pnorm) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~round((100*.x),0)) %>% 
  rename_at(vars(2:5), ~gsub("_scores", "", .x)) %>%
  rename_at(vars(2:5), ~gsub("_score", "", .x)) %>%
  mutate_at(vars(2:5), ~replace_na(.x, 0)) %>% 
  ungroup() %>% 
  mutate(mean = (Raid_Execution + Raid_PB_TLPs +  Recon + STX)/4) %>% 
  mutate(color = if_else(mean<30, "skyblue", if_else(mean<70, "gray", "lightgreen"))) %>%
  separate(participant, into=c("platoon", "squad"), remove=FALSE, sep=4 ) %>% 
  mutate(color2 = if_else(platoon == "Plt4", "red", if_else(platoon =="Plt5", "darkgreen", "blue"))) %>% 
  column_to_rownames("participant")  %>%
    select(color, color2, Raid_Execution, Raid_PB_TLPs, STX, Recon, mean) %>% 
  arrange(mean) %>% 
  mutate_if(is.numeric, ~round(.x, 1))


png("results_superheat.png", height =600, width = 1200)
as.matrix(df_superheat %>% select (Raid_Execution:Recon)) %>% 
  superheat(
              title = "2022 72HFS Squad Performance Results (percentile scores)",
              title.size = 8,
              padding = 0.1,
              pretty.order.cols = FALSE,
             pretty.order.rows = FALSE,
             row.dendrogram = FALSE,
             col.dendrogram = FALSE,
             heat.pal = c("lightblue", "lightblue", "white",  "lightgreen", "lightgreen"),
             heat.lim = c(0,100),
             heat.pal.values = c(0,.3, .7, 1),
               X.text = as.matrix(df_superheat %>% select (Raid_Execution:Recon)) ,
             X.text.size = 6,
             bottom.label.text.angle = 90,
             bottom.label.text.size = 6,
             bottom.label.size = .3,
             bottom.label.text.alignment = "center",
             left.label.text.size = 6,
             left.label.size = .2,
            # column.title = "Event",
            # row.title = "Participant",
            yr =df_superheat$mean,
            yr.plot.type = "bar",
            yr.axis.name = "mean percentile",
            yr.plot.size = .5,
            yr.axis.name.size = 14,
            yr.num.ticks = 8,
            yr.obs.col = df_superheat$color,
            legend = FALSE,
            left.label.text.col = df_superheat$color2,
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
  write.csv("72HFS_2022_Results_DetailedReport_4Aug2022.csv")

