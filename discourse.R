library(tidyverse)
library(httr)
library(whisker)



discourseconnection <- function(url, key, user){
  config <- list(headers = add_headers(.headers = c('Api-Key'= key, 'Api-Username' = user)),
                 base_url = url)
  testurl <-  whisker.render("{{url}}site.json")
  res <- content(GET(testurl, config$headers), 'text') %>% jsonlite::fromJSON() 
  res$connection <- config
  class(res) <- "discourseconnection"
  return(res)
}

print.discourseconnection=function(dc,...){
  paste("Connection to: ", dc$connection$base_url)
}

# dc <- discourseconnection("https://discourse.psy.plymouth.ac.uk/",
                          # "YOUR_API_KEY_HERE",
                          # "system")


group_members <- function(dc, group_id) UseMethod("group_members")
group_members.discourseconnection <- function(dc, group_id){
  base_url <- dc$connection$base_url
  theurl <-  whisker.render("{{base_url}}groups/{{group_id}}/members.json")
  res <- content(GET(theurl, dc$connection$headers), as="text") %>%
    jsonlite::fromJSON()
  df <- bind_rows(
    res$members %>% as_tibble() %>% mutate(role="member", .before=1),
    res$owners %>% as_tibble() %>% mutate(role="owners", .before=1)
  ) %>% mutate(group = group_id, .before=2)
  if (nrow(df)==0){
    df <- tibble(username=character())
  }
  return(
    df
  )
}
# group_members(dc, group_id="cap-2021-may")




groups <- function(dc) UseMethod("groups")
groups.discourseconnection <- function(dc){
  base_url <- dc$connection$base_url
  theurl <-  whisker.render("{{base_url}}groups.json")
  res <- content(GET(theurl, dc$connection$headers), as="text") %>%
    jsonlite::fromJSON()
  return(res$groups %>% as_tibble)
}



# reverse group ids and names
group_id_from_name <- function(dc, g) UseMethod("group_id_from_name")
group_id_from_name.discourseconnection <- function(dc, g){
  groups(dc) %>% filter(name==g) %>% pull(id)
}

group_name_from_id <- function(dc, g) UseMethod("group_name_from_id")
group_name_from_id.discourseconnection <- function(dc, g){
  groups(dc) %>% filter(id==g) %>% pull(name)
}

# agroup <- groups(dc)$name %>% first()
# group_id_from_name(dc, agroup)
# group_name_from_id(dc, group_id_from_name(dc, agroup)) == agroup


# amend groups 

add_group_members <- function(dc, ...) UseMethod("add_group_members")
add_group_members.discourseconnection <- function(dc, group_name, usernames=NULL){
  
  base_url <- dc$connection$base_url
  group_id = group_id_from_name(dc, group_name)
  theurl <-  whisker.render("{{base_url}}groups/{{group_id}}/members.json");
  res <- content(
    PUT(theurl, 
        body =  list('usernames'=paste0(usernames, collapse=",")), 
        dc$connection$headers
    ), as="text") %>% 
    jsonlite::fromJSON()
  df <- bind_rows(
    tibble(action="added", username=as.character(c(res$usernames))),
    tibble(action="error", message=as.character(c(res$errors))))  
  
  return(df)
}

remove_group_members <- function(dc, ...) UseMethod("remove_group_members")
remove_group_members.discourseconnection <- function(dc, group_name, usernames=NULL){
  
  base_url <- dc$connection$base_url
  group_id = group_id_from_name(dc, group_name)
  theurl <-  whisker.render("{{base_url}}groups/{{group_id}}/members.json");
  res <- content(
    DELETE(theurl, 
           body =  list('usernames'=paste0(usernames, collapse=",")), 
           dc$connection$headers
    ), as="text") %>% 
    jsonlite::fromJSON()
  df <- bind_rows(
    tibble(action="removed", username=as.character(c(res$usernames))),
    tibble(action="skipped", username=as.character(c(res$skipped_usernames))))
  
  return(df)
}

