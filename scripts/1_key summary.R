# Set column names
cols <- c("id", "event", "type",
  "country","state","district",
  "date_start", "note_start",
  "date_detect", "note_detect",
  "detect_bn1", "detect_bn2", "detect_bn3",
  "detect_enabler",
  "date_notify", "note_notify",
  "notify_bn1", "notify_bn2", "notify_bn3",
  "notify_enabler",
  paste0("date_response_", 1:7),
  "date_resp_start",
  "date_resp_end",
  "note_response",
  "response_bn1", "response_bn2", "response_bn3",
  "response_enabler",
  "date_end",
  "note")
  
# read in data
dat <- rio::import(here::here("data", file_name),
                  which = "1. Input timeliness data", skip = 3, 
                  na = "NA",
                  col_names = cols) %>% filter(!is.na(event))

bn <- rio::import(here::here("data", file_name),
                  which = "4. Categorize bottlenecks")

actions <- rio::import(here::here("data", file_name),
                       which = "3. Track remedial actions")

######################## Filter out the events under scope ##################

# Filter the data to the date range for the report
dat %<>% mutate_at(vars(contains("date_")), function(x){
  if(is.numeric(x)){
    as.POSIXct(x, format = "%Y-%m-%d")}
  else if(is.character(x)){
    as.POSIXct(as.numeric(x)*60*60*24, origin = '1899-12-30', tz = "UTC")     
  }
  else if(is.Date(x)){
    as.POSIX(x)
  }else{return(x)}
}
)

dat %<>% filter(date_detect >= as.POSIXct(start_date, format = "%Y-%m-%d"),
                date_detect <= as.POSIXct(end_date, format = "%Y-%m-%d"))


############################# 7-1-7 targets ################################

# calculate the timeliness indicators
dat %<>% mutate(
  d_detect = difftime(date_detect, date_start, units = "days"),
  d_notify = difftime(date_notify, date_detect, units = "days"),
  d_response = difftime(date_resp_end, date_notify, units = "days")
)

dat[,paste0("d_response_", 1:7)] <- 
  apply(dat, 1, function(x){
      difftime(x[(paste0("date_response_", 1:7))], x["date_notify"], unit = "days")
}) %>% t()

# Number of events
n_events <- nrow(dat)

# Events meeting the detection target
n_met_detect <- dat %>% filter(d_detect <=7) %>% nrow()
p_met_detect <- round(n_met_detect/n_events * 100, 0)

# Events meeting the notification target
n_met_notify <- dat %>% filter(d_notify<=1) %>% nrow()
p_met_notify <- round(n_met_notify/n_events *100, 0)

# Events meeting the response target
n_met_response <- dat %>% filter(d_response <=7) %>% nrow()
p_met_response <- round(n_met_response/n_events * 100, 0)

# Events meeting all the target
n_met_all <- dat %>% filter(d_detect <=7 & d_notify <=1 & d_response <= 7) %>% nrow()
p_met_all <- round(n_met_all/n_events *100, 0)

######################### Bottlenecks #############################

colnames(bn)[1:6] <- c("bottlenecks", "event_id", "interval", "level", "bn_cat", "naphs_cat")

# Rank them based on the countes
bn_ranked <- bn %>% filter(!is.na(bn_cat)) %>%
  group_by(bn_cat) %>% 
  summarize(
  n = n()
  ) %>% 
  arrange(by=desc(n))

# Extract top 3 
bottleneck_1 <- bn_ranked[1,1] %>% as.character()
bottleneck_2 <- bn_ranked[2,1] %>% as.character()
bottleneck_3 <- bn_ranked[3,1] %>% as.character()


############################## Actions #############################

colnames(actions) <- c("proposed", "bn_addressed", "event_id", "responsible",
                       "target_start_date", "target_end_date", "status", "next_steps")

# Number of completed actions
n_actions_completed <- actions %>% filter(status == "Completed") %>% nrow()
n_actions <- actions %>% filter(!is.na(proposed)) %>% nrow()
p_actions_completed <- round(n_actions_completed/n_actions*100, 0)

n_actions_ongoing <- actions %>% filter(status ==  "In progress") %>% nrow()
n_actions_proposed <- n_actions - n_actions_completed - n_actions_ongoing
