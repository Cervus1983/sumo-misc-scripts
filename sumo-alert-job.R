start_time <- Sys.time()
comments <- list()

predictions_fn <- "sumo-predict.csv"
alert_fn <- "sumo-alert.csv"

if (file.exists(predictions_fn)) {
	library(tidyverse)
	
	odds <- "~/sumo-odds/odds" %>% 
		list.files(pattern = "\\.csv", full.names = TRUE) %>% 
		tail(1) %>% 
		read_csv() %>% 
		group_by(rikishi1, rikishi2) %>% 
		summarise(
			#open_ts = min(ts),
			odds1 =  last(odds1, order_by = ts),
			odds2 = last(odds2, order_by = ts)
		) %>% 
		ungroup()
	
	new_odds <- odds %>% 
		# remove already process odds
		anti_join(
			alert_fn %>% 
				read_csv() %>% 
				group_by(rikishi1, rikishi2) %>% 
				summarise(
					odds1 = max(odds1, na.rm = TRUE),
					odds2 = max(odds2, na.rm = TRUE)
				)
		) %>% 
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
				"-"
			),
			prob = ifelse(EV1 > EV2, prob.yes, 1 - prob.yes),
			EV = ifelse(EV1 > EV2, EV1, EV2)
		) %>% 
		select(
			rikishi1,
			odds1,
			odds2,
			rikishi2,
			prob,
			bet_on,
			EV
		) %>% 
		arrange(-EV)
	
	# send e-mail
	if (nrow(new_odds) > 0) {
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
						paste(pander_return(new_odds, style = "multiline"), collapse = "\n"), "
					</pre>
					https://www.betmarathon.com/en/betting/Sumo/?menu=954952
				</body>
			</html>"
		))
		
		msg_content[["headers"]][["Content-Type"]] <- "text/html"
		
		sendmail(
			from = "sumo-alert@r",
			to = "cervus1983@gmail.com",
			subject = new_odds %>% 
				transmute(
					subject = sprintf(
						"%s %.2f %.2f %s (%s * %.1f%% = %.2f)",
						rikishi1, odds1, odds2, rikishi2, bet_on, prob * 100, EV
					)
				) %>% 
				pull(subject) %>% 
				head(1),
			msg = msg_content
		)
		
		comments <- new_odds %>% 
			filter(bet_on != "-") %>% 
			pull(bet_on) %>% 
			paste(collapse = ", ")
	}
	
	# update file
	write_csv(odds, alert_fn)
}

source("log.R")
write_log("sumo-alert", comments)
