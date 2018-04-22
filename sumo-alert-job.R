lock_fn <- "sumo-alert.lock"

if (!file.exists(lock_fn)) {
	file.create(lock_fn)

	start_time <- Sys.time()
	
	source("log.R")
	comments <- list()
	
	alert_fn <- "sumo-alert.csv"
	odds_fn <- "odds/marathonbet-now.csv"
	predictions_fn <- "sumo-predict.csv"
	
	
	
	if (file.exists(predictions_fn) && last_run("sumo-alert") < file.mtime(odds_fn)) {
		odds <- read_csv(odds_fn)
		
		offers <- odds %>% 
			# add model predictions
			inner_join(
				predictions_fn %>% 
					read_csv() %>% 
					rename(rikishi1 = rikishi1_shikona, rikishi2 = rikishi2_shikona)
			) %>% 
			# calculate EV
			mutate(
				EV1 = prob.yes * odds1 - 1,
				EV2 = (1 - prob.yes) * odds2 - 1,
				bet_on = ifelse(
					EV1 > 0 | EV2 > 0,
					ifelse(EV1 > EV2, rikishi1, rikishi2),
					NA
				),
				prob = ifelse(EV1 > EV2, prob.yes, 1 - prob.yes),
				EV = round(ifelse(EV1 > EV2, EV1, EV2), 3)
			) %>% 
			filter(!is.na(bet_on)) %>% 
			select(
				rikishi1,
				odds1,
				odds2,
				rikishi2,
				prob,
				bet_on,
				EV
			)
		
		best_offers <- alert_fn %>% 
			read_csv() %>% 
			rbind(offers) %>% 
			group_by(rikishi1, rikishi2, prob, bet_on) %>% 
			summarise(
				odds1 = last(odds1, order_by = EV),
				odds2 = last(odds2, order_by = EV),
				EV = max(EV)
			) %>% 
			ungroup() %>% 
			select(
				rikishi1,
				odds1,
				odds2,
				rikishi2,
				prob,
				bet_on,
				EV
			)
		
		new_offers <- best_offers %>% 
			anti_join(read_csv(alert_fn)) %>% 
			arrange(-EV)
			
		# send e-mail
		if (nrow(new_offers) > 0) {
			library(pander)
			library(sendmailR)
			
			panderOptions("table.split.table", Inf)
			
			msg_content <- mime_part(paste("
				<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
				<html xmlns=\"http://www.w3.org/1999/xhtml\">
					<head>
						<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
						<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"/>
					</head>
					<body>
						<pre>",
							paste(pander_return(new_offers, style = "multiline"), collapse = "\n"), "
						</pre>
						https://www.betmarathon.com/en/betting/Sumo/?menu=954952
					</body>
				</html>"
			))
			
			msg_content[["headers"]][["Content-Type"]] <- "text/html"
			
			sendmail(
				from = "sumo-alert@r",
				to = Sys.getenv("MY_EMAIL"),
				subject = new_offers %>% 
					transmute(
						subject = sprintf(
							"%s %.2f %.2f %s (%s * %.1f%% = %.3f)",
							rikishi1, odds1, odds2, rikishi2, bet_on, prob * 100, EV
						)
					) %>% 
					pull(subject) %>% 
					head(1),
				msg = msg_content
			)
			
			comments <- new_offers %>% 
				pull(bet_on) %>% 
				paste(collapse = ", ")
		}
		
		# rewrite file
		best_offers %>% 
			arrange(-EV) %>% 
			write_csv(alert_fn)
		
		write_log("sumo-alert", comments)
	} else write_log("sumo-alert-not-run", "")
	
	file.remove(lock_fn)
}
