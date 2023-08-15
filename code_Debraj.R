library(pacman)

`%notin%` <- Negate(`%in%`)

p_load(
  #--- Packages to Fit Maps
  geojsonio, tigris, leaflet, 
  #--- Packages to Produce Tables
  gtsummary, flextable, janitor, broom, officer, kableExtra, reactable, table1, 
  #--- Packages to Produce Figures
  crayon, ggsci, ggridges, ggthemes, ggforce, ggpubr, patchwork, grid, gridExtra, plotly,  survminer, viridis, ggridges, hrbrthemes, latex2exp, scales, glue, magrittr, showtext, ragg, extrafont, palmerpenguins, ggtext, showtext, extrafont, ggiraph, 
  #--- Packages for Data Retrieval & Pre-Processing
  readxl, here, rdrop2, lubridate, zoo, tidyverse, purrr, data.table, stringr, tidyr, dplyr
)

data <- readxl::read_xlsx(file.path(here(), "data", "7mile-Greenfield Survey 2021-2023 7mile-Greenfield community.xlsx"))

data_sub = data %>%
  select(c(9, 12:13, 16:19, 21:24, 26:37))

colnames(data_sub) = c("Age", "Race", "Safety_Scale", "Crime_Victim", "Important_Issue", "Homeowner", "Home_Loss_Risk",
                   "Household_Size", "Children", "Transportation", "Felons", "Education", "Length_Stay", "Financial_Consultation",
                   "Income_Source", "Occupation", "Block_Club", "Registered_Voter", "Voting_Status", "Why", 
                   "Follow_Up", "More_Info", "Additional_Info")

data_table = list(data = data_sub)

# AB (financial consultation)
data_table$table_fin_cons = data_table$data %>% 
  select(c(Financial_Consultation)) %>% 
  tbl_summary(label =  Financial_Consultation ~ "Financial consultation") %>% 
  as_gt()


# AC (current source of income)
data_table$table_inc_src = data_table$data %>% 
  select(c(Income_Source)) %>% 
  tbl_summary(label =  Income_Source ~ "Current source of income") %>% 
  as_gt()

data_table$table_inc_src_by_homeowner = data_table$data %>% 
  select(c(Income_Source, Homeowner)) %>% 
  tbl_summary(by = Homeowner, label =  Income_Source ~ "Current source of income") %>% 
  as_gt()


# AD (current occupation)
data_table$table_occup = data_table$data %>% 
  select(c(Occupation)) %>% 
  tbl_summary(label =  Occupation ~ "Current occupation") %>% 
  as_gt()


# AE (Block Club)
data_table$table_blk_clb = data_table$data %>% 
  select(c(Block_Club)) %>% 
  tbl_summary(label =  Block_Club ~ "Block Club") %>% 
  as_gt()

data_table$table_blk_clb_by_inc_src = data_table$data %>% 
  select(c(Block_Club, Income_Source)) %>% 
  tbl_summary(by = Block_Club, label =  Income_Source ~ "Current source of income") %>% 
  as_gt()

data_table$table_blk_clb_by_sft_scl = data_table$data %>% 
  select(c(Block_Club, Safety_Scale)) %>% 
  tbl_summary(by = Block_Club, label =  Safety_Scale ~ "Safety scale") %>% 
  as_gt()


# AF (Registered Voter)
data_table$table_reg_vot = data_table$data %>% 
  select(c(Registered_Voter)) %>% 
  tbl_summary(label =  Registered_Voter ~ "Registered_Voter") %>% 
  as_gt()

data_table$table_reg_vot_by_edu = data_table$data %>% 
  select(c(Registered_Voter, Education)) %>% 
  tbl_summary(by = Registered_Voter, label =  Education ~ "Education") %>% 
  as_gt()

data_table$table_reg_vot_by_inc_src = data_table$data %>% 
  select(c(Registered_Voter, Income_Source)) %>% 
  tbl_summary(by = Registered_Voter, label =  Income_Source ~ "Current source of income") %>% 
  as_gt()


# AG (Voting Status)
data_table$table_vot_sts = data_table$data %>% 
  select(c(Voting_Status)) %>% 
  tbl_summary(label =  Voting_Status ~ "Voting Status") %>% 
  as_gt()

data_table$table_vot_sts_by_reg_vot = data_table$data %>% 
  select(c(Voting_Status, Registered_Voter)) %>% 
  tbl_summary(by = Voting_Status, label =  Registered_Voter ~ "Registered Voter") %>% 
  as_gt()


# AI (Follow-up)
data_table$table_fol_up = data_table$data %>% 
  select(c(Follow_Up)) %>% 
  tbl_summary(label =  Follow_Up ~ "Follow-up") %>% 
  as_gt()

data_table$table_fol_up_by_inc_src = data_table$data %>% 
  select(c(Follow_Up, Income_Source)) %>% 
  tbl_summary(by = Follow_Up, label =  Income_Source ~ "Current source of income") %>% 
  as_gt()


# AJ (More Information about TPA)
data_table$data = data_table$data %>% mutate(More_Info = ifelse(is.na(More_Info), "Unknown", More_Info)) 
data_table$table_mor_inf = data_table$data %>% 
  select(c(More_Info)) %>% 
  tbl_summary(label = More_Info ~ "More Information") %>% 
  as_gt()


