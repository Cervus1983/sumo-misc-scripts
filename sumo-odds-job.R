lock_fn <- "sumo-odds.lock"

if (!file.exists(lock_fn)) {
	file.create(lock_fn)

	start_time <- Sys.time()
	
	source("log.R")
	comments <- list()
	
	# marathonbet
	source("../sumo-odds/parser/marathonbet.R")
	
	odds_now_fn <- "odds/marathonbet-now.csv"
	odds_hist_fn <- sprintf("odds/marathonbet_%s.csv", format(Sys.Date(), "%Y%m"))
	
	odds <- read_csv(odds_now_fn)
	
	new_odds <- "https://www.marathonbet.com/en/betting/Sumo/?menu=954952" %>% 
		httr::GET() %>% 
		httr::content("text") %>% 
		parse_marathonbet()
	
	if (is.data.frame(new_odds) && !isTRUE(all_equal(new_odds, odds))) {
		write_csv(new_odds, odds_now_fn)
		
		write_csv(
			new_odds %>% mutate(ts = format(start_time, "%Y-%m-%d %H:%M:%S")),
			odds_hist_fn,
			append = TRUE
		)
		
		comments <- c(comments, sprintf("marathonbet %s offers", nrow(new_odds)))
	}
	
	# 1xbet
	source("../sumo-odds/parser/1xbet.R")
	
	odds_now_fn <- "odds/1xbet-now.csv"
	odds_hist_fn <- sprintf("odds/1xbet_%s.csv", format(Sys.Date(), "%Y%m"))
	
	odds <- read_csv(odds_now_fn)
	
	new_odds <- "https://1xbet.com/en/line/Sumo/" %>% 
		httr::GET() %>% 
		httr::content("text") %>% 
		parse_1xbet()
	
	if (is.data.frame(new_odds) && !isTRUE(all_equal(new_odds, odds))) {
		write_csv(new_odds, odds_now_fn)
		
		write_csv(
			new_odds %>% mutate(ts = format(start_time, "%Y-%m-%d %H:%M:%S")),
			odds_hist_fn,
			append = file.exists(odds_hist_fn)
		)
		
		comments <- c(comments, sprintf("1xbet %s offers", nrow(new_odds)))
	}
	
	# log
	write_log("sumo-odds", comments)
	
	file.remove(lock_fn)
}
