### Advanced R Programming
# Part 2: Longitudinal Data Class and Methods
# Using S3 system

# Loading package
library(tidyverse)

# Create LongitudinalData object
make_LD <- function(data) {
  data_tbl <- suppressWarnings(data %>% nest(-id))
  structure(data_tbl, class = "LongitudinalData")
}

# Print method for Longitudinal Data
print.LongitudinalData <- function(x) {
  paste0("Longitudinal dataset with ", x$id %>% unique %>% length, " subjects")
}

### Define subject function and  subject related methods
subject <- function(x, id_num) {
  id_rows <- which(x[["id"]] == id_num)
  if(!length(id_rows)) return(NULL)
  structure(list(id = id_num, outdata = x[["data"]][[id_rows]]), class = "subject")
}

print.subject <- function(x) {
  if(is.null(x)) return(NULL)
  cat("Subject ID: ", x$id, "\n")
  invisible(x)
}

summary.subject <- function(x) {
  if(is.null(x)) return(NULL)
  outdat <- x$outdata %>% 
    group_by(visit, room) %>%
    summarise(val = mean(value), .groups = "drop") %>% 
    spread(room, val)
  structure(list(id = x[["id"]], outdat = outdat), class = "summary")
}

print.summary <- function(x) {
  cat("Subject ID: ", x$id, "\n")
  print(x$outdat)
}

### Define visit function and methods
visit <- function(x, visit_num) UseMethod("visit")

visit.subject <- function(x, visit_num) {
  if(!(visit_num %in% 0:2) | is.null(x)) return(NULL)
  outdat <- x[["outdata"]] %>% filter(visit == visit_num) %>% select(-visit)
  structure(list(
    id = x[["id"]],
    visit = visit_num,
    outdata = outdat
  ), class = "visit")
}

print.visit <- function(x) {
  if(is.null(x)) return(NULL)
  cat("Visit Number: ", x$visit, "\n")
  invisible(x)
}


# Define room functions and methods
room <- function(x, room_name) UseMethod("room")

room.visit <- function(x, room_name) {
  if(!room_name %in% x[["outdata"]][["room"]] | is.null(x)) return(NULL)
  outdata <- x[["outdata"]] %>% 
    filter(room == room_name) %>% 
    select(-room)
  structure(list(
    id = x[["id"]],
    visit = x[["visit"]],
    room = room_name,
    outdata = outdata), class = "room")
}

print.room <- function(x) {
  cat("Subject ID: ", x[["id"]], "\n")
  cat("Visit: ", x[["visit"]], "\n")
  cat("Room: ", x[["room"]])
  invisible(x)
}

summary.room <- function(x) {
  outdat <- summary(x[["outdata"]][["value"]])
  structure(list(id = x[["id"]], outdat = outdat), class = "summary")
}

