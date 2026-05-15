library(tidyverse)
library(gt)
library(RColorBrewer)
library(webshot2)

#Loading data
result <- readRDS("./data/amarok_data.rds")

#List of Observers
observer_list <- c(
  "Japan",
  "United Kingdom",
  "France",
  "Germany",
  "People's Republic of China",
  "Republic of India",
  "Republic of Korea",
  "Republic of Singapore",
  "Spain",
  "The Netherlands",
  "Switzerland",
  "Poland",
  "Italian Republic"
)

#Calculating total number of projects
observer_totals <- result |>
  summarise(across(all_of(observer_list), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(
    cols = everything(),
    names_to = "observer",
    values_to = "n_projects"
  ) |>
  arrange(desc(n_projects))

observer_total_table <- observer_totals |>
  gt() |>
  tab_header(
    title = "Observer State Engagement in AC Projects",
    subtitle = "Total Number of Projects"
  ) |>
  cols_label(
    observer = "Observer",
    n_projects = "Projects"
  )

gtsave(observer_total_table, "./tables/tbl2_projects_total.png")

#Calculating the number of projects by working group
observer_by_wg_table <- result |>
  pivot_longer(
    cols      = all_of(observer_list),
    names_to  = "observer",
    values_to = "present"
  ) |>
  select(working_groups, observer, present) |>
  group_by(working_groups, observer) |>
  summarise(n_projects = sum(present, na.rm = T), .groups = 'drop') |>
  pivot_wider(
    names_from  = working_groups,
    values_from = n_projects
  ) |>
  arrange(observer) |>
  select(observer, ACAP, AMAP, CAFF, PAME, EPPR, SDWG, everything()) |>
  arrange(desc(CAFF))

observer_by_wg_table <- observer_by_wg_table |>
  #These groups were selected because at least one observer participated in their projects
  select(observer, AMAP, CAFF, EPPR, PAME, SDWG, `EPPR/PAME`) |>
  gt() |>
  tab_header(
    title = "Observer State Engagement in AC Projects",
    subtitle = "Number of Projects by Working Group"
  ) |>
  cols_label(
    observer = "Observer"
  ) |>
  data_color(
    columns = c(-observer),
    palette = "Blues"
  )

gtsave(observer_by_wg_table, "./tables/tbl3_projects_by_wg.png")

