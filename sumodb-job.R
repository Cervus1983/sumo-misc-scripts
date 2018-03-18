start_time <- Sys.time()
comments <- list()

source("../sumodb/sumodb.R")

if (
	Sys.Date() %>% 
		format("%m") %>% 
		as.integer() %>% 
		`%%`(2) %>% 
		`==`(1)
) {
	yyyy.mm <- format(Sys.Date(), "%Y.%m")
	
	# banzuke
	banzuke <- sumodbBanzuke(yyyy.mm)
	fn <- paste0("banzuke/", yyyy.mm, ".csv")

	if (is.data.frame(banzuke) && nrow(banzuke) > 0 && !file.exists(fn)) {
		write_csv(
			banzuke,
			fn
		)
		
		comments <- c(
			comments,
			paste(yyyy.mm, "banzuke")
		)
	}
	
	# results
	results <- sumodbBout(yyyy.mm, division = c("m", "j"))
	
	if (is.data.frame(results) && nrow(results) > 0) {
		write_csv(
			results,
			paste0("results/", yyyy.mm, ".csv")
		)

		comments <- c(
			comments,
			paste(yyyy.mm, "day", max(results$day))
		)
	}
}

source("log.R")
write_log("sumodb", comments)
