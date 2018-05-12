start_time <- Sys.time()

source("log.R")
comments <- list()

banzuke <- "banzuke" %>% 
	list.files(pattern = "\\.csv", full.names = TRUE) %>% 
	lapply(read_csv) %>% 
	do.call(rbind, .)

results <- "results" %>% 
	list.files(pattern = "\\.csv", full.names = TRUE) %>% 
	lapply(read_csv) %>% 
	do.call(rbind, .) %>% 
	mutate_at(vars(ends_with("_win")), as.integer)

upcoming <- results %>% 
	filter(basho == max(basho)) %>% 
	group_by(rikishi1_id) %>% 
	filter(
		day == 1 | !is.na(lag(rikishi1_win, order_by = day)),
		is.na(rikishi1_win)
	) %>% 
	ungroup()

# data frame with matchups -> character vector
hash <- function(data) data %>% 
	transmute(x = paste(rikishi1_shikona, rikishi2_shikona, sep = "-")) %>% 
	arrange(x) %>% 
	pull(x) %>% 
	paste(collapse = ", ")

predictions_fn <- "sumo-predict.csv"

if (nrow(upcoming) > 0 && (!file.exists(predictions_fn) || hash(read_csv(predictions_fn)) != hash(upcoming))) {
	source("~/sumo-predict-outcome/features.R")
	
	predictions <- results %>% 
		# remove bouts following an unsettled one
		group_by(basho, rikishi1_id) %>% 
		filter(day == 1 | !is.na(lag(rikishi1_win, order_by = day))) %>% 
		ungroup() %>% 
		# generate features for upcoming bouts (requires complete data set)
		add_banzuke_info(
			# replace NA height/weight with mean values
			banzuke %>% 
				group_by(id) %>% 
				mutate(
					height = ifelse(is.na(height), mean(height, na.rm = TRUE), height),
					weight = ifelse(is.na(weight), mean(weight, na.rm = TRUE), weight)
				)
		) %>% 
		add_age() %>% 
		parse_rank() %>% 
		add_form() %>% 
		add_kachi_koshi() %>% 
		add_streak() %>% 
		add_head_to_head() %>% 
		# only unsettled bouts
		filter(is.na(rikishi1_win)) %>% 
		# character variables -> factors
		mutate_if(is.character, as.factor)
	
	if (nrow(predictions) > 0) {
		library(mlr)
		
		model <- readRDS("~/sumo-predict-outcome/model/binomial.rds")
	
		predictions %>% 
			select(
				-basho,
				-rikishi1_id,
				-rikishi1_rank,
				-rikishi1_shikona,
				-rikishi1_result,
				-rikishi1_win,
				-kimarite,
				-rikishi2_id,
				-rikishi2_rank,
				-rikishi2_shikona,
				-rikishi2_result,
				-rikishi2_win,
				-rikishi1_birth_date,
				-rikishi1_prev,
				-rikishi2_birth_date,
				-rikishi2_prev,
				-rikishi1_form,
				-rikishi2_form
			) %>% 
			mutate_if(is.ordered, as.integer) %>% 
			predict(model, newdata = .) %>% 
			cbind(predictions, .) %>% 
			select(
				rikishi1_shikona,
				rikishi2_shikona,
				prob.yes
			) %>% 
			write_csv(predictions_fn)
		
		comments <- predictions %>% 
			count(basho, day) %>% 
			apply(1, paste , collapse = " ") %>% 
			c(comments, .)
	}
}

write_log("sumo-predict", comments)
