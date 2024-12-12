# Annex 1

tab_annex1 <- dat %>% select(event, district, d_detect, d_notify, contains("d_response_"), d_response) %>%
  mutate_at(vars(contains("d_")), function(x){round(x,0)})

tab_annex1$d_detect <- cell_spec(tab_annex1$d_detect, 
                              background = ifelse(is.na(tab_annex1$d_detect), "light grey",
                                                  ifelse(tab_annex1$d_detect <=7, "#C1E1C1", "#F3CFC6")))
tab_annex1$d_notify <- cell_spec(tab_annex1$d_notify, 
                              background = ifelse(is.na(tab_annex1$d_notify), "light grey",
                                                  ifelse(tab_annex1$d_notify <=1, "#C1E1C1", "#F3CFC6")))
tab_annex1$d_response <- cell_spec(tab_annex1$d_response, 
                                background = ifelse(is.na(tab_annex1$d_response), "light grey",
                                                    ifelse(tab_annex1$d_response <=7, "#C1E1C1", "#F3CFC6")))
tab_annex1 %<>%
  kbl(align = "c", escape=F,
      col.names = c("Event", "District", 
                    "<font color='#ED5446'>DAYS TO DETECTION</font>", 
                    "<font color = '#F89736'>DAYS TO NOTIFICATION</font>", 
                    "<b>Action 1</b><br><font size=1>INITIATE INVESTIGATION</font>",
                    "<b>Action 2</b><br><font size=1>EPI CHARACTERIZATION & RISK ASSESSMENT</font>",
                    "<b>Action 3</b><br><font size=1>LAB CONFIRMATION</font>",
                    "<b>Action 4</b><br><font size=1>CASE MANAGEMENT/IPC</font>",
                    "<b>Action 5</b><br><font size=1>MCMs & PHSMs</font>",
                    "<b>Action 6</b><br><font size=1>RCCE</font>",
                    "<b>Action 7</b><br><font size=1>COORDINATION MECHANISM</font>",
                    "<font color = '#2FBB4D'>DAYS TO EARLY RESPONSE</font>")) %>% 
  kable_paper(html_font = "Arial", font_size = 12) %>%
  row_spec(0, bold = T, extra_css = 'vertical-align: top !important;') 

save_kable(tab_annex1, here::here("results", "tab_annex1.png"), zoom = 1.5)


# Annex 2 (completed remedial actions)
comp_act <- actions %>% filter(tolower(status) == "completed") %>% select(proposed, bn_addressed)
comp_act <- cbind(1:nrow(comp_act), comp_act)


# Annex 3

p_bn_by_cat <- bn %>% filter(!is.na(bn_cat)) %>% 
  mutate(interval2 = paste0(interval, " bottlenecks")) %>%
  ggplot(aes(x= forcats::fct_rev(forcats::fct_infreq(bn_cat)))) + 
  geom_bar(fill = "#0437F2") + 
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        strip.text = element_text(face = "bold", size = 12))+
  xlab("") + ylab("") + 
  ggforce::facet_col(~ interval2, space = "free", scales = "free_y")

p_annex_3_2 <- bn %>% mutate(
  level = str_to_sentence(level)) %>% 
  group_by(level, interval) %>% 
  summarize(n=n()) %>% ungroup() %>%
  filter(level %in% c("Health facility or community",
                      "Intermediate",
                      "Multiple levels", "National")) %>% 
  ggplot() + geom_bar(aes(level, n, fill = interval), stat = "identity") +
  stat_summary(
    aes(level, n, label = after_stat(y)),
    fun = "sum", geom = "text", vjust = 0,
    position = position_nudge(y = 1)
  ) +
  scale_fill_manual(values = c("Detection" = "#ED5446",
                                 "Notification" = "#F89736",
                                 "Response" = "#2FBB4D")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12)
        )+
  xlab("") + ylab("Number of bottlenecks")
 
