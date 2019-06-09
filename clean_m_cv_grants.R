# hacking M-CV via Excel
# first paste current, submitted, and past grants 
# onto 3 tabs in a 
# GrantList excel workbook
# copy to this project within Rcode

library(tidyverse)
library(rvest)
library(readxl)
library(here)
library(lubridate)
library(knitr)
library(kableExtra)
library(scales)
library(flextable)
library(usethis)

## start with past grants

past_grants <- read_excel('GrantsListJune2019.xlsx', 
                          sheet ='Past')

past_grants %>% 
  filter(!grepl("Updt", Title)) %>% 
  select(-c(...8, ...9)) %>% 
  janitor::clean_names() %>% 
  mutate(role = factor(role_in_grant),
         role_in_grant = NULL,
         sponsor = factor(sponsor_name),
         sponsor_name = NULL) %>% 
  rename(total_dir_indir = total_direct_indirect) %>% 
  mutate(start = ymd(begin_dt),
         end = ymd(end_dt),
         begin_dt = NULL,
         end_dt = NULL) %>% 
  rename(grantnum = grant_number) %>% 
  filter(!is.na(total_dir_indir)) %>% 
  select(sponsor, total_dir_indir, start, end, role, 
         title, grantnum) %>% 
  arrange(start) ->
past_grants

# summarize
past_grants %>% 
  summarize(period = "Past",
            grants = n(),
            total_funding = sum(total_dir_indir)) ->
past_total


past_grants %>% 
  kable() %>% 
  kable_styling()


## now submitted grants

subm_grants <- read_excel('GrantsListJune2019.xlsx', 
                          sheet ='Submitted')

subm_grants %>% 
  filter(!grepl("Updt", Title)) %>% 
  select(-c(...8, ...9)) %>% 
  janitor::clean_names() %>% 
  mutate(role = factor(role_in_grant),
         role_in_grant = NULL,
         sponsor = factor(sponsor_name),
         sponsor_name = NULL) %>% 
  rename(total_dir_indir = total_direct_indirect) %>% 
  mutate(start = ymd(begin_dt),
         end = ymd(end_dt),
         begin_dt = NULL,
         end_dt = NULL) %>% 
  rename(grantnum = grant_number) %>% 
  filter(!is.na(total_dir_indir)) %>% 
  select(sponsor, total_dir_indir, start, end, role, 
         title, grantnum) %>% 
  arrange(start) ->
subm_grants

# summarize 
subm_grants %>% 
  summarize(period = "Submitted",
            grants = n(),
            total_funding = sum(total_dir_indir)) ->
subm_total


subm_grants %>% 
  kable() %>% 
  kable_styling()



## now current grants

curr_grants <- read_excel('GrantsListJune2019.xlsx', 
                          sheet ='Current')

curr_grants %>% 
  filter(!grepl("Updt", Title)) %>% 
  select(-c(...8, ...9)) %>% 
  janitor::clean_names() %>% 
  mutate(role = factor(role_in_grant),
         role_in_grant = NULL,
         sponsor = factor(sponsor_name),
         sponsor_name = NULL) %>% 
  rename(total_dir_indir = total_direct_indirect) %>% 
  mutate(start = ymd(begin_dt),
         end = ymd(end_dt),
         begin_dt = NULL,
         end_dt = NULL) %>% 
  rename(grantnum = grant_number) %>% 
  filter(!is.na(total_dir_indir)) %>% 
  select(sponsor, total_dir_indir, start, end, role, 
         title, grantnum)  ->
curr_grants

curr_grants %>% 
  summarize(period = "Current",
            grants = n(),
            total_funding = sum(total_dir_indir)) ->
curr_total


curr_grants %>% 
  kable() %>% 
  kable_styling()


# summarize all grants
summary <- rbind(past_total, curr_total, subm_total)

summary_total <- summary %>% 
  summarize(grants = sum(grants),
         total_funding = sum(as.numeric(total_funding))) %>% 
  mutate(period = "Total") %>% 
  select(period, grants, total_funding)

final <- rbind(summary, summary_total)  %>% 
  rename(funding = total_funding) %>% 
  mutate( funding = dollar(funding))

#style it

myft <- flextable(final) %>% 
  align(align = "center", part = "all") %>% 
  align(align= "right", j= "funding") %>% 
  fontsize(part = 'all', size=14) %>% 
  font(fontname = "Times", part = "all") %>% 
  autofit()
myft



