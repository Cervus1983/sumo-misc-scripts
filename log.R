library(tidyverse)

write_log <- function(job_name, comments) write_tsv(
	tibble(
		job = job_name,
		start = format(start_time, "%Y-%m-%d %H:%M:%S"),
		end = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
		duration = ceiling(difftime(Sys.time(), start_time, units = "secs")),
		comment = paste(comments, collapse = ", ")
	),
	path = "job-log.tsv",
	append = TRUE
)

last_run <- function(job_name) read_tsv("job-log.tsv") %>% 
	filter(job == job_name) %>% 
	pull(end) %>% 
	max()
