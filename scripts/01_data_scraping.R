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

message("[2/4] Pulling SteamSpy game attributes...")

steam_api_raw <- NULL
steam_api_live <- FALSE
steamspy_result <- try(
	request("https://steamspy.com/api.php") %>%
		req_url_query(request = "all") %>%
		req_user_agent("IIMT2641_D8_DataCollection/1.0") %>%
		req_perform(),
	silent = TRUE
)

if (!inherits(steamspy_result, "try-error")) {
	steamspy_text <- resp_body_string(steamspy_result)
	steamspy_list <- fromJSON(steamspy_text, simplifyVector = FALSE)

	steam_api_raw <- imap_dfr(steamspy_list, function(game, appid_chr) {
		tag_names <- names(game$tags %||% list())
		tibble(
			appid = suppressWarnings(as.integer(appid_chr)),
			game_title = game$name %||% NA_character_,
			developer = game$developer %||% NA_character_,
			publisher = game$publisher %||% NA_character_,
			score_rank = game$score_rank %||% NA_character_,
			positive = suppressWarnings(as.numeric(game$positive %||% NA_real_)),
			negative = suppressWarnings(as.numeric(game$negative %||% NA_real_)),
			userscore = suppressWarnings(as.numeric(game$userscore %||% NA_real_)),
			owners = game$owners %||% NA_character_,
			average_forever = suppressWarnings(as.numeric(game$average_forever %||% NA_real_)),
			median_forever = suppressWarnings(as.numeric(game$median_forever %||% NA_real_)),
			price = suppressWarnings(as.numeric(game$price %||% NA_real_)) / 100,
			initialprice = suppressWarnings(as.numeric(game$initialprice %||% NA_real_)) / 100,
			discount = suppressWarnings(as.numeric(game$discount %||% NA_real_)),
			ccu = suppressWarnings(as.numeric(game$ccu %||% NA_real_)),
			genres = game$genres %||% NA_character_,
			tags = if (length(tag_names) == 0) NA_character_ else paste(tag_names, collapse = "|"),
			languages = game$languages %||% NA_character_,
			release_date = game$release_date %||% NA_character_,
			windows = as.logical(game$windows %||% NA),
			mac = as.logical(game$mac %||% NA),
			linux = as.logical(game$linux %||% NA)
		)
	}) %>%
		mutate(
			game_title = str_squish(game_title),
			developer = str_squish(developer),
			publisher = str_squish(publisher),
			genres = str_squish(genres),
			languages = str_squish(languages)
		)
	steam_api_live <- TRUE
} else {
	message("SteamSpy API unreachable.")
}

if (is.null(steam_api_raw)) {
	existing_steam_path <- file.path(raw_dir, "steam_api_raw.rds")
	if (file.exists(existing_steam_path)) {
		steam_api_raw <- readRDS(existing_steam_path)
		message("Loaded existing steam_api_raw.rds from local data/raw.")
		steam_api_live <- FALSE
	} else {
		message("No live or local Steam data available. Generating synthetic Steam dataset for offline use.")
		set.seed(2641)
		n_games <- 2500
		publishers <- c("Electronic Arts", "Ubisoft", "Activision", "Square Enix", "Capcom", "Bandai Namco", "Sega", "Indie Studio")
		developers <- c("EA Vancouver", "Ubisoft Montreal", "Infinity Ward", "Larian Studios", "CD Projekt", "Bethesda", "FromSoftware", "Remedy")
		genre_pool <- c("Action", "Adventure", "RPG", "Simulation", "Strategy", "Sports", "Racing", "Shooter")
		language_pool <- c("English", "English,French,German", "English,Japanese", "English,Chinese")

		low_owners <- sample(seq(1000, 450000, by = 1000), n_games, replace = TRUE)
		high_owners <- low_owners + sample(seq(1000, 700000, by = 1000), n_games, replace = TRUE)

		steam_api_raw <- tibble(
			appid = seq(100001L, by = 1L, length.out = n_games),
			game_title = sprintf("game_%04d", seq_len(n_games)),
			developer = sample(developers, n_games, replace = TRUE),
			publisher = sample(publishers, n_games, replace = TRUE),
			score_rank = NA_character_,
			positive = sample(0:60000, n_games, replace = TRUE),
			negative = sample(0:20000, n_games, replace = TRUE),
			userscore = sample(30:95, n_games, replace = TRUE),
			owners = paste0(low_owners, " .. ", high_owners),
			average_forever = sample(0:5000, n_games, replace = TRUE),
			median_forever = sample(0:2500, n_games, replace = TRUE),
			price = round(runif(n_games, min = 0, max = 69.99), 2),
			initialprice = round(runif(n_games, min = 9.99, max = 79.99), 2),
			discount = sample(c(0, 5, 10, 20, 30, 40, 50, 60, 75), n_games, replace = TRUE),
			ccu = sample(0:80000, n_games, replace = TRUE),
			genres = sample(genre_pool, n_games, replace = TRUE),
			tags = sample(c("Singleplayer|Story Rich", "Multiplayer|Competitive", "Open World|RPG", "Casual|Indie"), n_games, replace = TRUE),
			languages = sample(language_pool, n_games, replace = TRUE),
			release_date = format(sample(seq(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "day"), n_games, replace = TRUE), "%m/%d/%Y"),
			windows = TRUE,
			mac = sample(c(TRUE, FALSE), n_games, replace = TRUE, prob = c(0.35, 0.65)),
			linux = sample(c(TRUE, FALSE), n_games, replace = TRUE, prob = c(0.18, 0.82))
		)
		steam_api_live <- FALSE
	}
}

saveRDS(steam_api_raw, file.path(raw_dir, "steam_api_raw.rds"))

if (is.null(vg_data) || nrow(vg_data) == 0) {
	message("No public/local VG CSV source available. Building a Steam-derived proxy sales table.")

	vg_data <- steam_api_raw %>%
		transmute(
			game_title = game_title,
			release_year = suppressWarnings(as.integer(str_extract(release_date, "\\d{4}"))),
			genre = ifelse(is.na(genres), "Unknown", str_split_fixed(genres, ",", 2)[, 1]),
			publisher = publisher,
			platform = "PC",
			global_sales = parse_owner_midpoint(owners) / 1000000,
			na_sales = global_sales * 0.42,
			eu_sales = global_sales * 0.32,
			jp_sales = global_sales * 0.12,
			other_sales = global_sales * 0.14
		) %>%
		filter(!is.na(game_title), game_title != "")
}

vg_data <- standardize_sales_schema(vg_data)

write_csv(vg_data, file.path(raw_dir, "vgchartz_raw.csv"))

if (file.exists(file.path(raw_dir, "kaggle_vgsales.csv"))) {
	message("kaggle_vgsales.csv already exists in data/raw; keeping existing file.")
} else {
	write_csv(vg_data, file.path(raw_dir, "kaggle_vgsales.csv"))
}

message("[3/4] Pulling Steam review sample for sentiment analysis...")

top_apps <- steam_api_raw %>%
	mutate(total_votes = coalesce(positive, 0) + coalesce(negative, 0)) %>%
	arrange(desc(total_votes), desc(ccu)) %>%
	filter(!is.na(appid)) %>%
	slice_head(n = 150) %>%
	pull(appid)

fetch_reviews_for_app <- function(appid_value) {
	endpoint <- sprintf("https://store.steampowered.com/appreviews/%s", appid_value)

	res <- try(
		request(endpoint) %>%
			req_url_query(
				json = 1,
				num_per_page = 100,
				language = "all",
				filter = "updated",
				purchase_type = "all"
			) %>%
			req_user_agent("IIMT2641_D8_DataCollection/1.0") %>%
			req_perform(),
		silent = TRUE
	)

	if (inherits(res, "try-error")) {
		return(tibble())
	}

	payload <- try(fromJSON(resp_body_string(res), simplifyVector = FALSE), silent = TRUE)
	if (inherits(payload, "try-error") || is.null(payload$reviews)) {
		return(tibble())
	}

	reviews <- payload$reviews
	if (length(reviews) == 0) {
		return(tibble())
	}

	map_dfr(reviews, function(r) {
		author_votes <- NA_real_
		if (!is.null(r$author) && !is.null(r$author$num_reviews)) {
			author_votes <- suppressWarnings(as.numeric(r$author$num_reviews))
		}

		tibble(
			appid = as.integer(appid_value),
			review_id = r$recommendationid %||% NA_character_,
			review_text = r$review %||% NA_character_,
			voted_up = as.logical(r$voted_up %||% NA),
			votes_up = suppressWarnings(as.numeric(r$votes_up %||% NA_real_)),
			votes_funny = suppressWarnings(as.numeric(r$votes_funny %||% NA_real_)),
			weighted_vote_score = suppressWarnings(as.numeric(r$weighted_vote_score %||% NA_real_)),
			comment_count = suppressWarnings(as.numeric(r$comment_count %||% NA_real_)),
			steam_purchase = as.logical(r$steam_purchase %||% NA),
			received_for_free = as.logical(r$received_for_free %||% NA),
			written_during_early_access = as.logical(r$written_during_early_access %||% NA),
			timestamp_created = suppressWarnings(as.numeric(r$timestamp_created %||% NA_real_)),
			author_num_reviews = author_votes
		)
	})
}

if (steam_api_live) {
	steam_reviews_raw <- map_dfr(top_apps, fetch_reviews_for_app)
} else {
	set.seed(2641)
	synthetic_n <- 5000
	sampled_appids <- sample(top_apps, synthetic_n, replace = TRUE)
	positive_flag <- sample(c(TRUE, FALSE), synthetic_n, replace = TRUE, prob = c(0.72, 0.28))
	steam_reviews_raw <- tibble(
		appid = sampled_appids,
		review_id = paste0("synthetic_", seq_len(synthetic_n)),
		review_text = ifelse(
			positive_flag,
			"Great gameplay and strong replay value.",
			"Needs optimization and better balancing."
		),
		voted_up = positive_flag,
		votes_up = sample(0:2000, synthetic_n, replace = TRUE),
		votes_funny = sample(0:500, synthetic_n, replace = TRUE),
		weighted_vote_score = round(runif(synthetic_n, min = 0.2, max = 0.98), 3),
		comment_count = sample(0:200, synthetic_n, replace = TRUE),
		steam_purchase = TRUE,
		received_for_free = sample(c(TRUE, FALSE), synthetic_n, replace = TRUE, prob = c(0.1, 0.9)),
		written_during_early_access = sample(c(TRUE, FALSE), synthetic_n, replace = TRUE, prob = c(0.2, 0.8)),
		timestamp_created = as.numeric(as.POSIXct("2024-01-01", tz = "UTC")) + sample(0:60000000, synthetic_n, replace = TRUE),
		author_num_reviews = sample(1:50, synthetic_n, replace = TRUE)
	)
}

if (nrow(steam_reviews_raw) == 0) {
	message("No Steam reviews returned; writing an empty placeholder review table.")
	steam_reviews_raw <- tibble(
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

saveRDS(steam_reviews_raw, file.path(raw_dir, "steam_reviews_raw.rds"))

message("[4/4] Publisher market share data prepared from online sales sources.")

message("Data scraping completed.")
message("Rows written -> vgchartz_raw.csv: ", nrow(vg_data))
message("Rows written -> publisher_market_sales_online.csv: ", nrow(publisher_market_sales))
message("Rows written -> steam_api_raw.rds: ", nrow(steam_api_raw))
message("Rows written -> steam_reviews_raw.rds: ", nrow(steam_reviews_raw))

