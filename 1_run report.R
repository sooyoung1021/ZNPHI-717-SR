###########################################################################
#                           PART 1. SETTING UP                            #
###########################################################################

##### Before you run the report, specify a couple of things ###############

## 1. Enter the data conslidation sheet name. 
## (This file should be saved in the "data" folder)
file_name <- "717 data file.xlsx"

## 2. Specify the dates in the format of YYYY-MM-DD
## (don't run these lines if you want to include all events in the data sheet)
start_date <- as.Date("2024-01-01")
end_date <- as.Date("2024-1-31")

## 3. Specify the report number
report_n <- 2


############################################################################
#                         PART 2. RUN THE REPORT                           #
############################################################################

##### Run the lines below this to generate the report ######################

## Remove all the old files
## Remove dummy files
dummies <- c("tab_717.png", "tab_actions.png", "tab_annex1.png", "tab_overall.png", "tab_response.png")
for(dum in dummies){
  if (file.exists(here::here("results", dum))) {
    file.remove(here::here("results", dum))
  }
}


###### Run the report
rmarkdown::render(here::here('scripts', '1_report template.Rmd'),
                  output_file = here::here("results",
                                           paste0("zambia_717_sr_", 
                                                  Sys.Date(),
                                                  ".docx"))
)

