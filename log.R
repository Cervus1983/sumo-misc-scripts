write_log <- function(job, comments) readr::write_tsv(
	tibble(
		job = job,
		start = format(start_time, "%Y-%m-%d %H:%M:%S"),
		end = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
		duration = ceiling(difftime(Sys.time(), start_time, units = "secs")),
		comment = paste(comments, collapse = ", ")
	),
	path = "job-log.tsv",
	append = TRUE
)
