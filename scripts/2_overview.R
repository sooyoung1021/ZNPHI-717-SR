# Overview
p_overview <- data.frame(
  p_met_detect,
  p_met_notify,
  p_met_response,
  p_met_all
) %>% gather(cat, p) %>% 
  mutate(
    cat = factor(cat, levels = c("p_met_detect",
                                 "p_met_notify",
                                 "p_met_response",
                                 "p_met_all")),
    p_label = paste0(p, "%")) %>%
  ggplot() +
  geom_bar(aes(cat, p, fill = cat), stat = "identity") +
  geom_text(aes(cat, p+8, label = p_label))+
  scale_fill_manual(
    values = c(p_met_detect = "#ED5446",
               p_met_notify = "#F89736",
               p_met_response = "#2FBB4D",
               p_met_all = "#4C4C4F")
  ) +
  scale_x_discrete(
    labels = c("Detection", "Notification", "Response", "All targets"),
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 110, by = 20),
    labels = paste0(seq(0, 100, by = 20), "%")
  )+
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(face = "bold", size =10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 9),
        legend.position = "none") + 
  xlab("")+ ylab("Proportion of events meeting target")

overview_text <- paste0("Total event identified in period: ",
                        n_events, " <br>Total events evaluated against 7-1-7: ",n_events)
overview_list <- 
  list(
    richtext_grob("Scope",
              hjust = 0.5,
              gp = gpar(col = "black",
                        fontface = "bold"),
              box_gp = gpar(col = "black",
                            fill = "light grey",
                            lty = 1),
              padding = unit(c(0.02,0.45, 0.02, 0.45), "npc"),
              margin = unit(c(0,0,20, 0), "pt")
    ), 
    richtext_grob("% Meeting Targets",
                  hjust = 0.5,
                  gp = gpar(col = "black",
                            fontface = "bold"),
                  box_gp = gpar(col = "black",
                                fill = "light grey",
                                lty = 1),
                  padding = unit(c(0.02,0.45, 0.02, 0.45), "npc"),
                  margin = unit(c(0,0,20, 0), "pt")
                  
    ),
    richtext_grob(overview_text,
                  halign = 0,
                  gp = gpar(col = "black"),
                  use_markdown=T
    ),
    p_overview
  )


## Overall performance table
title_overall <-  richtext_grob(
  "Overall Performance",
  halign = 0.1,
  gp = gpar(col = "black",
            fontface = "bold"),
  box_gp = gpar(col = "black",
                fill = "light grey",
                lty = 1),
  padding = unit(c(0.02,1, 0.02, 1), "npc"),
  margin = unit(c(0,0,3, 0), "pt")
)
                                
tab_overall <- data.frame(
  `.` = c("# Met Target", "% Met Target"),
  Detection = c(n_met_detect, paste0(p_met_detect, " %")),
  Notification = c(n_met_notify, paste0(p_met_notify, " %")),
  Response = c(n_met_response, paste0(p_met_response, " %")),
  `All targets` = c(n_met_all, paste0(p_met_all, " %"))
)  %>% kbl(align = "c") %>% 
  kable_paper(html_font = "Arial", font_size = 16) %>%
  row_spec(0, bold = T, 
           background = c("#ffffff", "#ED5446","#F89736","#2FBB4D","#4C4C4F")) %>%
  column_spec(2, color = "#ED5446", bold = T) %>%
  column_spec(3, color = "#F89736", bold = T) %>%
  column_spec(4, color = "#2FBB4D", bold = T) %>%
  column_spec(5, color = "#4C4C4F", bold = T) %>%
  column_spec(1, background = "#E5E4E2")

save_kable(tab_overall, here::here("results", "tab_overall.png"), zoom = 1.5)

## Response actions
title_response <- richtext_grob(
  "Early response actions",
  halign = 0.1,
  gp = gpar(col = "black",
            fontface = "bold"),
  box_gp = gpar(col = "black",
                fill = "light grey",
                lty = 1),
  padding = unit(c(0.02,1, 0.02, 1), "npc"),
  margin = unit(c(0,0,3, 0), "pt")
)

met_res_target <- apply(dat[,paste0("d_response_", 1:7)], 2, function(x){sum(x<=7, na.rm=T)})
n_res_applicable <- apply(dat[,paste0("d_response_", 1:7)], 2, function(x){sum(!is.na(x), na.rm=T)})
p_met_res_target <- round(met_res_target/n_res_applicable*100, 0)

tab_response <- data.frame(
  `.` = c("", "# Met Target", "# Events applicable*", "% Met target"),
  `Action 1` = c("INITIATE INVESTIGATION", NA, NA, NA),
  `Action 2` = c("EPI CHARACTERIZATION & RISK ASSESSMENT", NA, NA, NA),
  `Action 3` = c("LAB CONFIRMATION", NA, NA, NA),
  `Action 4` = c("CASE MANAGEMENT/IPC", NA, NA, NA),
  `Action 5` = c("MCMs & PHSMs", NA, NA, NA),
  `Action 6` = c("RCCE", NA, NA, NA),
  `Action 7` = c("COORDINATION MECHANISM", NA, NA, NA)
)

tab_response[2:4,2:8] <- rbind(
  as.character(met_res_target),
  as.character(n_res_applicable),
  as.character(paste0(p_met_res_target, " %")))

tab_response %<>% kbl(align = "c") %>%
  kable_paper(html_font = "Arial", font_size = 14) %>%
  row_spec(0, bold = T, 
           background = "#E5E4E2") %>%
  row_spec(1, bold = F,
           background = "#E5E4E2", font_size = 10) %>%
  column_spec(1, background = "#E5E4E2", bold = T)%>%
  row_spec(4, bold = T) %>%
  footnote(general_title="", "* Events that reported a response action as “not applicable” were excluded")

save_kable(tab_response, here::here("results", "tab_response.png"), zoom = 1.5)

#### 7-1-7 performance by event
title_717 <- richtext_grob(
  "7-1-7 performance by event",
  halign = 0.1,
  gp = gpar(col = "black",
            fontface = "bold"),
  box_gp = gpar(col = "black",
                fill = "light grey",
                lty = 1),
  padding = unit(c(0.02,1, 0.02, 1), "npc"),
  margin = unit(c(0,0,3, 0), "pt")
)

tab_717 <- dat %>% select(event, district, d_detect, d_notify, d_response)

tab_717$d_detect <- cell_spec(tab_717$d_detect, 
                              background = ifelse(is.na(tab_717$d_detect), "light grey",
                                                        ifelse(tab_717$d_detect <=7, "#C1E1C1", "#F3CFC6")))
tab_717$d_notify <- cell_spec(tab_717$d_notify, 
                              background = ifelse(is.na(tab_717$d_notify), "light grey",
                                                        ifelse(tab_717$d_notify <=1, "#C1E1C1", "#F3CFC6")))
tab_717$d_response <- cell_spec(tab_717$d_response, 
                              background = ifelse(is.na(tab_717$d_response), "light grey",
                                                        ifelse(tab_717$d_response <=7, "#C1E1C1", "#F3CFC6")))
tab_717 %<>%
  kbl(align = "c", escape=F,
      col.names = c("Event", "District", 
                    "<font color='#ED5446'>DAYS TO DETECTION</font>", 
                    "<font color = '#F89736'>DAYS TO NOTIFICATION</font>", 
                    "<font color = '#2FBB4D'>DAYS TO EARLY RESPONSE</font>")) %>% 
  kable_paper(html_font = "Arial", font_size = 16) %>%
  row_spec(0, bold = T) 

save_kable(tab_717, here::here("results", "tab_717.png"), zoom = 1.5)
