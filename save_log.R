con <- file("oop_output.txt")
sink(con, append = TRUE)
sink(con, append = TRUE, type = "message")

source("oop_output.R", echo = TRUE, max.deparse.length = 10000)

# Restore output to console
sink() 
sink(type="message")
