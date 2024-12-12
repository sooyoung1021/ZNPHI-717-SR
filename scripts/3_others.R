# bottlenecks

bn_tab_wide <- bn %>% filter(!is.na(bn_cat)) %>%
  group_by(naphs_cat, bn_cat) %>% summarize(n=n()) %>%
  filter(n>1) %>%
  arrange(by=desc(n))

# enablers
tab_enabler <- c(
  dat$detect_enabler %>% unique(),
  dat$notify_enabler %>% unique(),
  dat$response_enabler %>% unique()
) %>% as.data.frame()

colnames(tab_enabler) <- "enabler"

tab_enabler %<>% mutate(
  enabler = str_remove_all(gsub("\\r\\n", "|", enabler), "•")
)

tab_enabler %<>% .[!duplicated(tab_enabler),]
tab_enabler %>% kable(col.names = c("<b>Common enablers</b>"))
