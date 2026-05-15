library(rvest)
library(purrr)
library(tidyverse)

#I used AI (ChatGPT) to help me develop this code to scrape the website.
#It was used to troubleshoot/correct errors in the code.

#Links for where the projects are found
BASE_URL <- "https://arctic-council.org"
PROJECT_LIST_URL <- "https://arctic-council.org/projects/"

#Accessing the page
proj_page <- read_html(PROJECT_LIST_URL)

#Getting the links for each project page, which are found under each entry
projects <- proj_page |>
  html_elements("div.projectTitle.h3 a") |>
  (\(nodes) {
    tibble(
      title = nodes |> html_text2(),
      link  = nodes |> html_attr("href")
    )
  })()

projects <- projects |>
  mutate(
    link = if_else(
      str_starts(link, "http"),
      link,
      paste0(BASE_URL, link)
    )
  )

# Extract working groups and observers from each project
get_project_details <- function(url) {
  pg <- read_html(url)
  
  #Getting the working group
  wg <- pg |>
    html_element("div#amarok_wgs") |>
    html_elements("a") |>
    html_text2()
  
  #Getting observers
  obs <- pg |>
    html_element("div#amarok_observers") |>
    html_elements("a") |>
    html_text2() |>
    str_trim()
  
  tibble(
    working_groups = ifelse(length(wg) == 0, NA, list(wg)),
    observers = ifelse(length(obs) == 0, NA, list(obs))
  )
}

project_details <- projects$link |>
  map_dfr(get_project_details)


#Connect the project title to the extracted information
result <- bind_cols(
  projects |> select(title),
  project_details
)

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

#Create binary indicator for each project and each observer
for (obs_name in observer_list) {
  col_name <- obs_name   # or make.names(obs_name) if you want syntactic names
  
  result[[col_name]] <- map_lgl(
    result$observers,
    function(obs_vec) {
      # If the list element is NA, treat as empty: all FALSE
      if (all(is.na(obs_vec))) {
        return(FALSE)
      }
      obs_name %in% obs_vec
    }
  )
}

result <- result |>
  mutate(
    working_groups = map_chr(
      working_groups,
      ~ if (.x[1] == "") {
        "EGBCM" #Filled in this blank manually because I saw they didn't list it right on the website, so it wasn't detected
      } else if (length(.x) == 1) {
        .x[1]
      } else {
        str_c(.x, collapse = "/")
      }
    )
  )

#Outputing the data
write_rds(result, "./data/amarok_data.rds")