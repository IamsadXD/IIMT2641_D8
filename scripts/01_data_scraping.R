#!/usr/bin/env Rscript

suppressPackageStartupMessages({
	library(httr2)
	library(jsonlite)
	library(readr)
	library(dplyr)
	library(purrr)
	library(stringr)
	library(tibble)
})

`%||%` <- function(x, y) {
	if (is.null(x) || length(x) == 0) {
		return(y)
	}
	x
}

parse_owner_midpoint <- function(owner_str) {
	clean <- owner_str %>%
		coalesce("") %>%
		str_replace_all(",", "")

	split_ranges <- str_split_fixed(clean, "\\.\\.", 2)
	low <- suppressWarnings(as.numeric(str_squish(split_ranges[, 1])))
	high <- suppressWarnings(as.numeric(str_squish(split_ranges[, 2])))

	ifelse(is.na(low) | is.na(high), NA_real_, (low + high) / 2)
}

empty_steam_api_table <- function() {
	tibble(
		appid = integer(),
		game_title = character(),
		developer = character(),
		publisher = character(),
		score_rank = character(),
		positive = numeric(),
		negative = numeric(),
		userscore = numeric(),
		owners = character(),
		average_forever = numeric(),
		median_forever = numeric(),
		price = numeric(),
		initialprice = numeric(),
		discount = numeric(),
		ccu = numeric(),
		genres = character(),
		tags = character(),
		languages = character(),
		release_date = character(),
		windows = logical(),
		mac = logical(),
		linux = logical()
	)
}

empty_steam_reviews_table <- function() {
	tibble(
		appid = integer(),
		review_id = character(),
		review_text = character(),
		voted_up = logical(),
		votes_up = numeric(),
		votes_funny = numeric(),
		weighted_vote_score = numeric(),
		comment_count = numeric(),
		steam_purchase = logical(),
		received_for_free = logical(),
		written_during_early_access = logical(),
		timestamp_created = numeric(),
		author_num_reviews = numeric()
	)
}

rename_first_existing <- function(df, target, candidates) {
	matched <- intersect(candidates, names(df))
	if (length(matched) > 0 && matched[1] != target) {
		names(df)[names(df) == matched[1]] <- target
	}
	if (!(target %in% names(df))) {
		df[[target]] <- NA
	}
	df
}

normalize_key <- function(x) {
	x %>%
		tolower() %>%
		str_replace_all("[^a-z0-9]+", " ") %>%
		str_squish()
}

standardize_sales_schema <- function(df) {
	names(df) <- tolower(names(df))
	df <- rename_first_existing(df, "game_title", c("game_title", "name", "title"))
	df <- rename_first_existing(df, "release_year", c("release_year", "year", "release", "release_date"))
	df <- rename_first_existing(df, "genre", c("genre", "category"))
	df <- rename_first_existing(df, "publisher", c("publisher", "pub"))
	df <- rename_first_existing(df, "platform", c("platform", "console", "system"))
	df <- rename_first_existing(df, "na_sales", c("na_sales", "north_america_sales"))
	df <- rename_first_existing(df, "eu_sales", c("eu_sales", "pal_sales", "europe_sales"))
	df <- rename_first_existing(df, "jp_sales", c("jp_sales", "japan_sales"))
	df <- rename_first_existing(df, "other_sales", c("other_sales", "rest_of_world_sales", "row_sales"))
	df <- rename_first_existing(df, "global_sales", c("global_sales", "total_sales", "world_sales"))

	df %>%
		select(
			game_title,
			release_year,
			genre,
			publisher,
			platform,
			na_sales,
			eu_sales,
			jp_sales,
			other_sales,
			global_sales
		) %>%
		mutate(
			release_year = case_when(
				is.numeric(release_year) ~ as.integer(release_year),
				TRUE ~ suppressWarnings(as.integer(str_extract(as.character(release_year), "\\d{4}")))
			),
			game_title = str_squish(as.character(game_title)),
			genre = str_squish(as.character(genre)),
			publisher = str_squish(as.character(publisher)),
			platform = str_squish(as.character(platform)),
			na_sales = suppressWarnings(as.numeric(na_sales)),
			eu_sales = suppressWarnings(as.numeric(eu_sales)),
			jp_sales = suppressWarnings(as.numeric(jp_sales)),
			other_sales = suppressWarnings(as.numeric(other_sales)),
			global_sales = suppressWarnings(as.numeric(global_sales))
		)
}

project_root <- normalizePath(file.path(getwd(), "."), winslash = "/", mustWork = TRUE)
raw_dir <- file.path(project_root, "data", "raw")
processed_dir <- file.path(project_root, "data", "processed")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

message("[1/4] Scraping console-inclusive sales datasets from online sources...")

online_sales_sources <- tribble(
	~source_name, ~source_url,
	"vgsales_andvise", "https://raw.githubusercontent.com/andvise/DataAnalyticsDatasets/main/vgsales.csv",
	"vgsales_saemaqazi", "https://raw.githubusercontent.com/saemaqazi/vgsales.csv/main/vgsales.csv",
	"vgchartz_2024", "https://raw.githubusercontent.com/Bredmak/vgchartz-sales-analysis/main/vgchartz-2024.csv"
)

online_sales_data <- pmap_dfr(online_sales_sources, function(source_name, source_url) {
	tmp <- try(read_csv(source_url, show_col_types = FALSE, progress = FALSE), silent = TRUE)
	if (inherits(tmp, "try-error") || nrow(tmp) == 0) {
		message("Online sales source failed: ", source_name)
		return(tibble())
	}

	message("Online sales source loaded: ", source_name)
	standardized <- standardize_sales_schema(tmp)

	standardized %>%
		mutate(
			source_name = source_name,
			source_url = source_url
		)
}) %>%
	filter(!is.na(game_title), game_title != "")

if (nrow(online_sales_data) > 0) {
	online_sales_data <- online_sales_data %>%
		distinct(game_title, platform, release_year, .keep_all = TRUE)
	write_csv(online_sales_data, file.path(raw_dir, "console_sales_online.csv"))
	vg_data <- online_sales_data %>%
		select(game_title, release_year, genre, publisher, platform, na_sales, eu_sales, jp_sales, other_sales, global_sales)
} else {
	vg_data <- NULL
	message("All online sales sources failed. Falling back to local files if available.")

	local_vg_candidates <- c(
		file.path(raw_dir, "console_sales_online.csv"),
		file.path(raw_dir, "vgchartz_raw.csv"),
		file.path(raw_dir, "kaggle_vgsales.csv")
	)

	for (local_path in local_vg_candidates) {
		if (file.exists(local_path)) {
			tmp <- read_csv(local_path, show_col_types = FALSE, progress = FALSE)
			if (nrow(tmp) > 0) {
				vg_data <- standardize_sales_schema(tmp)
				message("VG sales source loaded from local file: ", basename(local_path))
				break
			}
		}
	}
}

publisher_market_input <- if (nrow(online_sales_data) > 0) {
	online_sales_data %>%
		select(game_title, release_year, genre, publisher, platform, global_sales, source_url)
} else {
	vg_data %>%
		mutate(source_url = "local_cache_or_fallback") %>%
		select(game_title, release_year, genre, publisher, platform, global_sales, source_url)
}

publisher_market_sales <- publisher_market_input %>%
	mutate(
		publisher = str_squish(as.character(publisher)),
		release_year = suppressWarnings(as.integer(release_year)),
		global_sales = suppressWarnings(as.numeric(global_sales))
	) %>%
	filter(
		!is.na(publisher),
		publisher != "",
		!is.na(release_year),
		!is.na(global_sales),
		global_sales > 0
	) %>%
	group_by(publisher, release_year) %>%
	summarise(
		publisher_global_sales = sum(global_sales, na.rm = TRUE),
		game_count = n(),
		avg_sales_per_game = mean(global_sales, na.rm = TRUE),
		platform_count = n_distinct(platform),
		genre_count = n_distinct(genre),
		source_url = paste(sort(unique(source_url)), collapse = " | "),
		.groups = "drop"
	) %>%
	group_by(release_year) %>%
	mutate(
		year_total_sales = sum(publisher_global_sales, na.rm = TRUE),
		market_share_pct = ifelse(
			year_total_sales > 0,
			100 * publisher_global_sales / year_total_sales,
			NA_real_
		),
		publisher_rank_in_year = min_rank(desc(publisher_global_sales))
	) %>%
	ungroup() %>%
	rename(year = release_year)

write_csv(publisher_market_sales, file.path(raw_dir, "publisher_market_sales_online.csv"))
write_csv(
	publisher_market_sales %>%
		select(publisher, year, market_share_pct, source_url),
	file.path(raw_dir, "publisher_market_share_template.csv")
)

message("[2/4] Pulling Steam data via SteamKit-powered endpoints...")

extract_association_names <- function(associations, assoc_type) {
	if (length(associations) == 0) {
		return(NA_character_)
	}

	vals <- character()
	for (a in associations) {
		a_type <- tolower(as.character(a$type %||% ""))
		a_name <- str_squish(as.character(a$name %||% ""))
		if (a_type == tolower(assoc_type) && a_name != "") {
			vals <- c(vals, a_name)
		}
	}

	vals <- unique(vals)
	if (length(vals) == 0) NA_character_ else paste(vals, collapse = "|")
}

perform_json_request <- function(
	endpoint,
	timeout_seconds = 12,
	max_tries = 2,
	max_wait_seconds = 3
) {
	req <- request(endpoint) %>%
		req_user_agent("IIMT2641_D8_SteamKit/1.0") %>%
		req_timeout(timeout_seconds) %>%
		req_retry(
			max_tries = max_tries,
			retry_on_failure = TRUE,
			max_seconds = max_wait_seconds,
			backoff = ~ runif(1, min = 0.3, max = 1.2)
		)

	try(req_perform(req), silent = TRUE)
}

build_title_variants <- function(title_value) {
	base <- str_squish(as.character(title_value %||% ""))
	if (base == "") {
		return(character())
	}

	variant_1 <- str_squish(str_remove(base, "\\s*\\([^)]*\\)$"))
	variant_2 <- str_squish(str_remove(variant_1, "\\s*[-:|].*$"))
	variant_3 <- str_squish(str_replace_all(variant_2, "[^A-Za-z0-9 ]", " "))

	unique(c(base, variant_1, variant_2, variant_3))
}

load_cache_csv <- function(file_path, required_cols) {
	empty_cache <- tibble()
	for (col_name in required_cols) {
		empty_cache[[col_name]] <- character()
	}

	if (!file.exists(file_path)) {
		return(empty_cache)
	}

	cache_candidate <- try(read_csv(file_path, show_col_types = FALSE, progress = FALSE), silent = TRUE)
	if (inherits(cache_candidate, "try-error") || !is.data.frame(cache_candidate)) {
		return(empty_cache)
	}

	cache_candidate <- as_tibble(cache_candidate)
	for (col_name in required_cols) {
		if (!(col_name %in% names(cache_candidate))) {
			cache_candidate[[col_name]] <- NA
		}
	}

	cache_candidate %>%
		select(all_of(required_cols))
}

normalize_logical_flag <- function(x) {
	x_chr <- tolower(str_squish(as.character(x)))
	case_when(
		x_chr %in% c("true", "t", "1", "yes", "y") ~ TRUE,
		x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
		x_chr == "" | is.na(x_chr) ~ NA,
		TRUE ~ as.logical(x)
	)
}

search_steam_app <- function(title_value) {
	title_variants <- build_title_variants(title_value)
	if (length(title_variants) == 0) {
		return(tibble())
	}

	fallback_result <- NULL

	for (query_value in title_variants) {
		if (nchar(query_value) < 2) {
			next
		}

		endpoint <- sprintf(
			"https://steamcommunity.com/actions/SearchApps/%s",
			utils::URLencode(query_value, reserved = TRUE)
		)

		res <- perform_json_request(
			endpoint,
			timeout_seconds = 8,
			max_tries = 2,
			max_wait_seconds = 2
		)
		if (inherits(res, "try-error")) {
			next
		}

		payload <- try(fromJSON(resp_body_string(res), simplifyVector = TRUE), silent = TRUE)
		if (inherits(payload, "try-error") || length(payload) == 0) {
			next
		}

		results <- as_tibble(payload)
		if (nrow(results) == 0 || !all(c("appid", "name") %in% names(results))) {
			next
		}

		search_key <- normalize_key(title_value)
		result_keys <- normalize_key(results$name)

		distance_raw <- as.numeric(adist(search_key, result_keys))
		normalizer <- pmax(nchar(search_key), nchar(result_keys), 1)
		distance_scaled <- distance_raw / normalizer
		chosen_idx <- which.min(distance_scaled)

		if (length(chosen_idx) == 0 || is.infinite(distance_scaled[chosen_idx])) {
			next
		}

		if (!is.na(distance_scaled[chosen_idx]) && distance_scaled[chosen_idx] <= 0.45) {
			return(tibble(
				search_title = title_value,
				appid = suppressWarnings(as.integer(results$appid[chosen_idx])),
				steam_name = as.character(results$name[chosen_idx])
			))
		}

		if (!is.na(distance_scaled[chosen_idx]) && distance_scaled[chosen_idx] <= 0.65) {
			fallback_result <- tibble(
				search_title = title_value,
				appid = suppressWarnings(as.integer(results$appid[chosen_idx])),
				steam_name = as.character(results$name[chosen_idx])
			)
		}

		if (is.null(fallback_result) && nrow(results) > 0) {
			fallback_result <- tibble(
				search_title = title_value,
				appid = suppressWarnings(as.integer(results$appid[1])),
				steam_name = as.character(results$name[1])
			)
		}
	}

	if (!is.null(fallback_result)) {
		return(fallback_result)
	}

	tibble()
}

fetch_steamkit_info <- function(appid_value) {
	endpoint <- sprintf("https://api.steamcmd.net/v1/info/%s", appid_value)
	res <- perform_json_request(
		endpoint,
		timeout_seconds = 8,
		max_tries = 2,
		max_wait_seconds = 2
	)

	if (inherits(res, "try-error")) {
		return(tibble())
	}

	payload <- try(fromJSON(resp_body_string(res), simplifyVector = FALSE), silent = TRUE)
	if (inherits(payload, "try-error") || is.null(payload$data)) {
		return(tibble())
	}

	app_node <- payload$data[[as.character(appid_value)]]
	if (is.null(app_node)) {
		return(tibble())
	}

	store_endpoint <- sprintf(
		"https://store.steampowered.com/api/appdetails?appids=%s&cc=us&l=en",
		appid_value
	)
	store_res <- perform_json_request(
		store_endpoint,
		timeout_seconds = 8,
		max_tries = 2,
		max_wait_seconds = 2
	)

	store_data <- NULL
	if (!inherits(store_res, "try-error")) {
		store_payload <- try(fromJSON(resp_body_string(store_res), simplifyVector = FALSE), silent = TRUE)
		if (!inherits(store_payload, "try-error")) {
			store_node <- store_payload[[as.character(appid_value)]] %||% list()
			if (isTRUE(store_node$success) && !is.null(store_node$data)) {
				store_data <- store_node$data
			}
		}
	}

	common <- app_node$common %||% list()
	associations <- common$associations %||% list()
	genre_values <- unname(unlist(common$genres %||% list(), use.names = FALSE))
	tag_values <- unname(unlist(common$store_tags %||% list(), use.names = FALSE))
	oslist <- tolower(as.character(common$oslist %||% ""))
	release_unix <- suppressWarnings(as.numeric(common$steam_release_date %||% NA_real_))

	price_overview <- store_data$price_overview %||% list()
	is_free <- isTRUE(store_data$is_free)
	price_initial_cents <- suppressWarnings(as.numeric(price_overview$initial %||% NA_real_))
	discount_pct <- suppressWarnings(as.numeric(price_overview$discount_percent %||% NA_real_))

	# Use non-discounted list price as a release-price proxy to avoid sale price volatility.
	list_price_usd <- if (is_free) {
		0
	} else {
		price_initial_cents / 100
	}

	tibble(
		appid = suppressWarnings(as.integer(appid_value)),
		steam_name = as.character(common$name %||% NA_character_),
		developer = extract_association_names(associations, "developer"),
		publisher = extract_association_names(associations, "publisher"),
		score_rank = NA_character_,
		positive = NA_real_,
		negative = NA_real_,
		userscore = suppressWarnings(as.numeric(common$review_percentage %||% NA_real_)),
		owners = NA_character_,
		average_forever = NA_real_,
		median_forever = NA_real_,
		price = list_price_usd,
		initialprice = list_price_usd,
		discount = discount_pct,
		ccu = NA_real_,
		genres = if (length(genre_values) == 0) NA_character_ else paste(unique(genre_values), collapse = "|"),
		tags = if (length(tag_values) == 0) NA_character_ else paste(unique(tag_values), collapse = "|"),
		languages = NA_character_,
		release_date = ifelse(
			is.na(release_unix),
			NA_character_,
			format(as.POSIXct(release_unix, origin = "1970-01-01", tz = "UTC"), "%m/%d/%Y")
		),
		windows = str_detect(oslist, "windows"),
		mac = str_detect(oslist, "mac"),
		linux = str_detect(oslist, "linux")
	)
}

fetch_store_price_info <- function(appid_value) {
	endpoint <- sprintf(
		"https://store.steampowered.com/api/appdetails?appids=%s&cc=us&l=en&filters=price_overview,is_free",
		appid_value
	)
	res <- perform_json_request(
		endpoint,
		timeout_seconds = 6,
		max_tries = 1,
		max_wait_seconds = 1
	)

	if (inherits(res, "try-error")) {
		return(tibble())
	}

	payload <- try(fromJSON(resp_body_string(res), simplifyVector = FALSE), silent = TRUE)
	if (inherits(payload, "try-error") || is.null(payload[[as.character(appid_value)]])) {
		return(tibble())
	}

	store_node <- payload[[as.character(appid_value)]] %||% list()
	if (!isTRUE(store_node$success)) {
		return(tibble())
	}

	data_node <- store_node$data %||% list()
	price_overview <- data_node$price_overview %||% list()
	is_free <- isTRUE(data_node$is_free)
	initial_cents <- suppressWarnings(as.numeric(price_overview$initial %||% NA_real_))
	discount_pct <- suppressWarnings(as.numeric(price_overview$discount_percent %||% NA_real_))
	list_price_usd <- if (is_free) 0 else initial_cents / 100

	tibble(
		appid = suppressWarnings(as.integer(appid_value)),
		price = list_price_usd,
		initialprice = list_price_usd,
		discount = discount_pct
	)
}

steam_api_raw <- NULL
steam_api_live <- FALSE

if (!is.null(vg_data) && nrow(vg_data) > 0) {
	max_title_queries <- suppressWarnings(as.integer(Sys.getenv("STEAM_MAX_TITLE_QUERIES", "0")))
	max_appid_fetches <- suppressWarnings(as.integer(Sys.getenv("STEAM_MAX_APPID_FETCHES", "0")))
	retry_not_found_titles <- identical(Sys.getenv("STEAM_RETRY_NOT_FOUND", "0"), "1")

	if (is.na(max_title_queries) || max_title_queries < 0) {
		max_title_queries <- 0
	}
	if (is.na(max_appid_fetches) || max_appid_fetches < 0) {
		max_appid_fetches <- 0
	}

	title_pool <- vg_data %>%
		filter(!is.na(game_title), game_title != "") %>%
		mutate(
			global_sales = suppressWarnings(as.numeric(global_sales)),
			platform = as.character(platform),
			release_year = suppressWarnings(as.integer(release_year)),
			is_pc_like = str_detect(toupper(coalesce(platform, "")), "PC|WIN|MAC|LINUX|STEAM"),
			is_recent = !is.na(release_year) & release_year >= 2010
		) %>%
		arrange(desc(is_pc_like), desc(is_recent), desc(coalesce(global_sales, 0))) %>%
		distinct(game_title, .keep_all = TRUE)

	pc_titles <- title_pool %>%
		filter(is_pc_like) %>%
		pull(game_title)

	other_titles <- title_pool %>%
		filter(!is_pc_like) %>%
		pull(game_title)

	title_candidates_full <- unique(c(pc_titles, other_titles))
	title_candidates <- if (max_title_queries > 0) {
		head(title_candidates_full, max_title_queries)
	} else {
		title_candidates_full
	}

	message(
		"SteamKit title queries planned: ",
		length(title_candidates),
		" (",
		ifelse(max_title_queries > 0, "limited", "all available"),
		")"
	)

	steam_match_cache_path <- file.path(raw_dir, "steam_match_cache.csv")
	steam_match_cache <- load_cache_csv(
		steam_match_cache_path,
		required_cols = c("search_title", "appid", "steam_name")
	) %>%
		mutate(
			search_title = str_squish(as.character(search_title)),
			appid = suppressWarnings(as.integer(appid)),
			steam_name = str_squish(as.character(steam_name))
		) %>%
		filter(!is.na(search_title), search_title != "", !is.na(appid)) %>%
		distinct(search_title, appid, .keep_all = TRUE)

	steam_attempt_cache_path <- file.path(raw_dir, "steam_search_attempt_cache.csv")
	steam_attempt_cache <- load_cache_csv(
		steam_attempt_cache_path,
		required_cols = c("search_title", "status", "last_attempt_utc", "appid", "steam_name")
	) %>%
		mutate(
			search_title = str_squish(as.character(search_title)),
			status = str_squish(as.character(status)),
			last_attempt_utc = as.character(last_attempt_utc),
			appid = suppressWarnings(as.integer(appid)),
			steam_name = str_squish(as.character(steam_name))
		) %>%
		filter(!is.na(search_title), search_title != "") %>%
		distinct(search_title, .keep_all = TRUE)

	if (nrow(steam_match_cache) > 0) {
		seed_from_matches <- steam_match_cache %>%
			select(search_title, appid, steam_name) %>%
			mutate(
				status = "matched",
				last_attempt_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
			)

		steam_attempt_cache <- bind_rows(steam_attempt_cache, seed_from_matches) %>%
			arrange(desc(status == "matched")) %>%
			distinct(search_title, .keep_all = TRUE)
	}

	attempted_titles <- if (retry_not_found_titles) {
		steam_attempt_cache %>%
			filter(status == "matched") %>%
			pull(search_title)
	} else {
		steam_attempt_cache$search_title
	}

	pending_titles <- setdiff(title_candidates, unique(attempted_titles))
	message(
		"SteamKit title cache: ",
		nrow(steam_match_cache),
		" matches already cached, ",
		nrow(steam_attempt_cache),
		" attempts recorded, ",
		length(pending_titles),
		" titles pending",
		ifelse(retry_not_found_titles, " (retrying prior not_found titles)", "")
	)

	steam_match_new_list <- vector("list", length(pending_titles))
	steam_attempt_new_list <- vector("list", length(pending_titles))
	if (length(pending_titles) > 0) {
		for (idx in seq_along(pending_titles)) {
			title_value <- pending_titles[[idx]]
			if (idx %% 25 == 0 || idx == length(pending_titles)) {
				message("SteamKit search progress: ", idx, "/", length(pending_titles), " pending titles")
			}

			search_result <- search_steam_app(title_value)
			steam_match_new_list[[idx]] <- search_result

			if (nrow(search_result) > 0) {
				steam_attempt_new_list[[idx]] <- search_result %>%
					transmute(
						search_title = str_squish(as.character(search_title)),
						status = "matched",
						last_attempt_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
						appid = suppressWarnings(as.integer(appid)),
						steam_name = str_squish(as.character(steam_name))
					)
			} else {
				steam_attempt_new_list[[idx]] <- tibble(
					search_title = title_value,
					status = "not_found",
					last_attempt_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
					appid = NA_integer_,
					steam_name = NA_character_
				)
			}

			if (idx %% 100 == 0 || idx == length(pending_titles)) {
				steam_match_snapshot <- bind_rows(steam_match_cache, bind_rows(steam_match_new_list)) %>%
					filter(!is.na(appid), !is.na(search_title), search_title != "") %>%
					distinct(search_title, appid, .keep_all = TRUE)

				steam_attempt_snapshot <- bind_rows(steam_attempt_cache, bind_rows(steam_attempt_new_list)) %>%
					mutate(
						search_title = str_squish(as.character(search_title)),
						status = str_squish(as.character(status)),
						last_attempt_utc = as.character(last_attempt_utc),
						appid = suppressWarnings(as.integer(appid)),
						steam_name = str_squish(as.character(steam_name))
					) %>%
					filter(!is.na(search_title), search_title != "") %>%
					arrange(desc(status == "matched")) %>%
					distinct(search_title, .keep_all = TRUE)

				write_csv(steam_match_snapshot, steam_match_cache_path)
				write_csv(steam_attempt_snapshot, steam_attempt_cache_path)
			}
		}
	}

	steam_match_table <- bind_rows(steam_match_cache, bind_rows(steam_match_new_list)) %>%
		filter(!is.na(appid), !is.na(search_title), search_title != "") %>%
		distinct(search_title, appid, .keep_all = TRUE)

	steam_attempt_table <- bind_rows(steam_attempt_cache, bind_rows(steam_attempt_new_list)) %>%
		mutate(
			search_title = str_squish(as.character(search_title)),
			status = str_squish(as.character(status)),
			last_attempt_utc = as.character(last_attempt_utc),
			appid = suppressWarnings(as.integer(appid)),
			steam_name = str_squish(as.character(steam_name))
		) %>%
		filter(!is.na(search_title), search_title != "") %>%
		arrange(desc(status == "matched")) %>%
		distinct(search_title, .keep_all = TRUE)

	write_csv(steam_match_table, steam_match_cache_path)
	write_csv(steam_attempt_table, steam_attempt_cache_path)

	message("SteamKit total matched title->appid pairs: ", nrow(steam_match_table))

	if (nrow(steam_match_table) > 0) {
		appids_ranked <- steam_match_table %>%
			count(appid, sort = TRUE) %>%
			pull(appid)
		appids_to_fetch <- if (max_appid_fetches > 0) {
			head(appids_ranked, max_appid_fetches)
		} else {
			appids_ranked
		}

		steam_info_cache_path <- file.path(raw_dir, "steam_info_cache.csv")
		steam_info_cache <- load_cache_csv(
			steam_info_cache_path,
			required_cols = c(
				"appid",
				"steam_name",
				"developer",
				"publisher",
				"score_rank",
				"positive",
				"negative",
				"userscore",
				"owners",
				"average_forever",
				"median_forever",
				"price",
				"initialprice",
				"discount",
				"ccu",
				"genres",
				"tags",
				"languages",
				"release_date",
				"windows",
				"mac",
				"linux"
			)
		) %>%
			mutate(
				appid = suppressWarnings(as.integer(appid)),
				steam_name = str_squish(as.character(steam_name)),
				developer = str_squish(as.character(developer)),
				publisher = str_squish(as.character(publisher)),
				score_rank = str_squish(as.character(score_rank)),
				positive = suppressWarnings(as.numeric(positive)),
				negative = suppressWarnings(as.numeric(negative)),
				userscore = suppressWarnings(as.numeric(userscore)),
				owners = str_squish(as.character(owners)),
				average_forever = suppressWarnings(as.numeric(average_forever)),
				median_forever = suppressWarnings(as.numeric(median_forever)),
				price = suppressWarnings(as.numeric(price)),
				initialprice = suppressWarnings(as.numeric(initialprice)),
				discount = suppressWarnings(as.numeric(discount)),
				ccu = suppressWarnings(as.numeric(ccu)),
				genres = str_squish(as.character(genres)),
				tags = str_squish(as.character(tags)),
				languages = str_squish(as.character(languages)),
				release_date = str_squish(as.character(release_date)),
				windows = normalize_logical_flag(windows),
				mac = normalize_logical_flag(mac),
				linux = normalize_logical_flag(linux)
			) %>%
			filter(!is.na(appid)) %>%
			distinct(appid, .keep_all = TRUE)

		pending_appids <- setdiff(appids_to_fetch, steam_info_cache$appid)

		message(
			"SteamKit app detail fetch planned: ",
			length(appids_to_fetch),
			" of ",
			n_distinct(steam_match_table$appid),
			" matched appids (",
			ifelse(max_appid_fetches > 0, "limited", "all available"),
			")"
		)
		message(
			"SteamKit app cache: ",
			nrow(steam_info_cache),
			" appids already cached, ",
			length(pending_appids),
			" appids pending"
		)

		steam_info_new_list <- vector("list", length(pending_appids))
		if (length(pending_appids) > 0) {
			for (idx in seq_along(pending_appids)) {
				appid_value <- pending_appids[[idx]]
				if (idx %% 20 == 0 || idx == length(pending_appids)) {
					message("SteamKit app detail progress: ", idx, "/", length(pending_appids), " pending appids")
				}

				steam_info_new_list[[idx]] <- fetch_steamkit_info(appid_value)

				if (idx %% 50 == 0 || idx == length(pending_appids)) {
					steam_info_snapshot <- bind_rows(bind_rows(steam_info_new_list), steam_info_cache) %>%
						mutate(
							appid = suppressWarnings(as.integer(appid)),
							steam_name = str_squish(as.character(steam_name)),
							developer = str_squish(as.character(developer)),
							publisher = str_squish(as.character(publisher)),
							score_rank = str_squish(as.character(score_rank)),
							positive = suppressWarnings(as.numeric(positive)),
							negative = suppressWarnings(as.numeric(negative)),
							userscore = suppressWarnings(as.numeric(userscore)),
							owners = str_squish(as.character(owners)),
							average_forever = suppressWarnings(as.numeric(average_forever)),
							median_forever = suppressWarnings(as.numeric(median_forever)),
							price = suppressWarnings(as.numeric(price)),
							initialprice = suppressWarnings(as.numeric(initialprice)),
							discount = suppressWarnings(as.numeric(discount)),
							ccu = suppressWarnings(as.numeric(ccu)),
							genres = str_squish(as.character(genres)),
							tags = str_squish(as.character(tags)),
							languages = str_squish(as.character(languages)),
							release_date = str_squish(as.character(release_date)),
							windows = normalize_logical_flag(windows),
							mac = normalize_logical_flag(mac),
							linux = normalize_logical_flag(linux)
						) %>%
						filter(!is.na(appid)) %>%
						distinct(appid, .keep_all = TRUE)
					write_csv(steam_info_snapshot, steam_info_cache_path)
				}
			}
		}

		steam_info_raw <- bind_rows(bind_rows(steam_info_new_list), steam_info_cache) %>%
			mutate(
				appid = suppressWarnings(as.integer(appid)),
				steam_name = str_squish(as.character(steam_name)),
				developer = str_squish(as.character(developer)),
				publisher = str_squish(as.character(publisher)),
				score_rank = str_squish(as.character(score_rank)),
				positive = suppressWarnings(as.numeric(positive)),
				negative = suppressWarnings(as.numeric(negative)),
				userscore = suppressWarnings(as.numeric(userscore)),
				owners = str_squish(as.character(owners)),
				average_forever = suppressWarnings(as.numeric(average_forever)),
				median_forever = suppressWarnings(as.numeric(median_forever)),
				price = suppressWarnings(as.numeric(price)),
				initialprice = suppressWarnings(as.numeric(initialprice)),
				discount = suppressWarnings(as.numeric(discount)),
				ccu = suppressWarnings(as.numeric(ccu)),
				genres = str_squish(as.character(genres)),
				tags = str_squish(as.character(tags)),
				languages = str_squish(as.character(languages)),
				release_date = str_squish(as.character(release_date)),
				windows = normalize_logical_flag(windows),
				mac = normalize_logical_flag(mac),
				linux = normalize_logical_flag(linux)
			) %>%
			filter(!is.na(appid)) %>%
			distinct(appid, .keep_all = TRUE)

		all_matched_appids <- steam_match_table %>%
			pull(appid) %>%
			suppressWarnings(as.integer()) %>%
			unique()
		priced_appids <- steam_info_raw %>%
			filter(!is.na(price) | !is.na(initialprice)) %>%
			pull(appid) %>%
			suppressWarnings(as.integer()) %>%
			unique()
		store_price_pending <- setdiff(all_matched_appids, priced_appids)
		max_price_fetches <- suppressWarnings(as.integer(Sys.getenv("STEAM_MAX_PRICE_FETCHES", "0")))
		if (is.na(max_price_fetches) || max_price_fetches < 0) {
			max_price_fetches <- 0
		}
		if (max_price_fetches > 0) {
			store_price_pending <- head(store_price_pending, max_price_fetches)
		}

		message(
			"Steam Store price fetch pending appids: ",
			length(store_price_pending),
			" (",
			ifelse(max_price_fetches > 0, "limited", "all available"),
			")"
		)

		store_price_list <- vector("list", length(store_price_pending))
		if (length(store_price_pending) > 0) {
			for (idx in seq_along(store_price_pending)) {
				appid_value <- store_price_pending[[idx]]
				if (idx %% 200 == 0 || idx == length(store_price_pending)) {
					message("Steam Store price progress: ", idx, "/", length(store_price_pending), " pending appids")
				}

				store_price_list[[idx]] <- fetch_store_price_info(appid_value)

				if (idx %% 500 == 0 || idx == length(store_price_pending)) {
					store_price_snapshot <- bind_rows(store_price_list)
					if (nrow(store_price_snapshot) > 0) {
						steam_info_snapshot <- bind_rows(
							steam_info_raw,
							store_price_snapshot %>%
								mutate(
									steam_name = NA_character_,
									developer = NA_character_,
									publisher = NA_character_,
									score_rank = NA_character_,
									positive = NA_real_,
									negative = NA_real_,
									userscore = NA_real_,
									owners = NA_character_,
									average_forever = NA_real_,
									median_forever = NA_real_,
									ccu = NA_real_,
									genres = NA_character_,
									tags = NA_character_,
									languages = NA_character_,
									release_date = NA_character_,
									windows = NA,
									mac = NA,
									linux = NA
								)
						) %>%
							arrange(desc(!is.na(price))) %>%
							distinct(appid, .keep_all = TRUE)

						write_csv(steam_info_snapshot, steam_info_cache_path)
					}
				}
			}
		}

		store_price_table <- bind_rows(store_price_list)
		if (nrow(store_price_table) == 0 || !("appid" %in% names(store_price_table))) {
			store_price_table <- tibble(
				appid = integer(),
				price = numeric(),
				initialprice = numeric(),
				discount = numeric()
			)
		} else {
			store_price_table <- store_price_table %>%
				mutate(
					appid = suppressWarnings(as.integer(appid)),
					price = suppressWarnings(as.numeric(price)),
					initialprice = suppressWarnings(as.numeric(initialprice)),
					discount = suppressWarnings(as.numeric(discount))
				) %>%
				filter(!is.na(appid)) %>%
				distinct(appid, .keep_all = TRUE)
		}

		steam_info_raw <- bind_rows(
			steam_info_raw,
			store_price_table %>%
				mutate(
					steam_name = NA_character_,
					developer = NA_character_,
					publisher = NA_character_,
					score_rank = NA_character_,
					positive = NA_real_,
					negative = NA_real_,
					userscore = NA_real_,
					owners = NA_character_,
					average_forever = NA_real_,
					median_forever = NA_real_,
					ccu = NA_real_,
					genres = NA_character_,
					tags = NA_character_,
					languages = NA_character_,
					release_date = NA_character_,
					windows = NA,
					mac = NA,
					linux = NA
				)
		) %>%
			arrange(desc(!is.na(price))) %>%
			distinct(appid, .keep_all = TRUE)

		write_csv(steam_info_raw, steam_info_cache_path)

		steam_api_raw <- steam_match_table %>%
			left_join(steam_info_raw, by = "appid") %>%
			transmute(
				appid = appid,
				game_title = str_squish(as.character(search_title)),
				developer = str_squish(as.character(developer)),
				publisher = str_squish(as.character(publisher)),
				score_rank = score_rank,
				positive = positive,
				negative = negative,
				userscore = userscore,
				owners = owners,
				average_forever = average_forever,
				median_forever = median_forever,
				price = price,
				initialprice = initialprice,
				discount = discount,
				ccu = ccu,
				genres = genres,
				tags = tags,
				languages = languages,
				release_date = release_date,
				windows = as.logical(windows),
				mac = as.logical(mac),
				linux = as.logical(linux)
			) %>%
			filter(!is.na(appid), !is.na(game_title), game_title != "") %>%
			distinct(appid, .keep_all = TRUE)

		steam_api_live <- nrow(steam_api_raw) > 0
		if (steam_api_live) {
			message("SteamKit pull completed with matched titles: ", nrow(steam_api_raw))
		}
	}
}

if (is.null(steam_api_raw) || nrow(steam_api_raw) == 0) {
	existing_steam_path <- file.path(raw_dir, "steam_api_raw.rds")
	if (file.exists(existing_steam_path)) {
		steam_candidate <- readRDS(existing_steam_path)
		if (!is.data.frame(steam_candidate)) {
			message("Existing steam_api_raw.rds is invalid. Using empty Steam table.")
			steam_api_raw <- empty_steam_api_table()
		} else {
			steam_candidate <- as_tibble(steam_candidate)
			title_values <- as.character(steam_candidate$game_title %||% character())
			synthetic_title_ratio <- if (length(title_values) > 0) {
				mean(str_detect(title_values, "^game_\\d+$"), na.rm = TRUE)
			} else {
				0
			}

			if (is.finite(synthetic_title_ratio) && synthetic_title_ratio >= 0.9) {
				message("Existing steam_api_raw.rds appears synthetic and will not be used.")
				steam_api_raw <- empty_steam_api_table()
			} else {
				steam_api_raw <- steam_candidate
				message("Loaded existing steam_api_raw.rds from local data/raw.")
			}
		}
	} else {
		message("No SteamKit data available and no valid local Steam cache found. Saving empty Steam table.")
		steam_api_raw <- empty_steam_api_table()
	}

	steam_api_live <- FALSE
}

saveRDS(steam_api_raw, file.path(raw_dir, "steam_api_raw.rds"))

if (is.null(vg_data) || nrow(vg_data) == 0) {
	stop("No real game-sales dataset available from online sources or local cache. Synthetic fallback is disabled.")
}

vg_data <- standardize_sales_schema(vg_data)

write_csv(vg_data, file.path(raw_dir, "vgchartz_raw.csv"))

if (file.exists(file.path(raw_dir, "kaggle_vgsales.csv"))) {
	message("kaggle_vgsales.csv already exists in data/raw; keeping existing file.")
} else {
	write_csv(vg_data, file.path(raw_dir, "kaggle_vgsales.csv"))
}

message("[3/4] Building Steam review summary rows from SteamKit metadata...")

if (nrow(steam_api_raw) > 0) {
	steam_reviews_raw <- steam_api_raw %>%
		transmute(
			appid = as.integer(appid),
			review_id = paste0("steamkit_summary_", as.integer(appid)),
			review_text = NA_character_,
			voted_up = ifelse(is.na(userscore), NA, userscore >= 70),
			votes_up = NA_real_,
			votes_funny = NA_real_,
			weighted_vote_score = ifelse(is.na(userscore), NA_real_, userscore / 100),
			comment_count = NA_real_,
			steam_purchase = NA,
			received_for_free = NA,
			written_during_early_access = NA,
			timestamp_created = suppressWarnings(as.numeric(as.POSIXct(release_date, format = "%m/%d/%Y", tz = "UTC"))),
			author_num_reviews = NA_real_
		)

	steam_reviews_raw <- bind_rows(
		empty_steam_reviews_table(),
		steam_reviews_raw
	)
} else {
	steam_reviews_raw <- empty_steam_reviews_table()
	message("No SteamKit metadata rows available. Writing empty steam_reviews_raw.rds.")
}

saveRDS(steam_reviews_raw, file.path(raw_dir, "steam_reviews_raw.rds"))

message("[4/4] Publisher market share data prepared from online sales sources.")

message("Data scraping completed.")
message("Rows written -> vgchartz_raw.csv: ", nrow(vg_data))
message("Rows written -> publisher_market_sales_online.csv: ", nrow(publisher_market_sales))
message("Rows written -> steam_api_raw.rds: ", nrow(steam_api_raw))
message("Rows written -> steam_reviews_raw.rds: ", nrow(steam_reviews_raw))

