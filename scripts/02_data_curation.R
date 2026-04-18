#!/usr/bin/env Rscript

suppressPackageStartupMessages({
	library(readr)
	library(dplyr)
	library(stringr)
	library(tidyr)
	library(ggplot2)
	library(lubridate)
	library(forcats)
	library(scales)
})

project_root <- normalizePath(file.path(getwd(), "."), winslash = "/", mustWork = TRUE)
raw_dir <- file.path(project_root, "data", "raw")
processed_dir <- file.path(project_root, "data", "processed")
plots_dir <- file.path(project_root, "data", "plots")

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

normalize_key <- function(x) {
	x %>%
		tolower() %>%
		str_replace_all("[^a-z0-9]+", " ") %>%
		str_squish()
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
	df <- rename_first_existing(df, "release_year", c("release_year", "year", "release"))
	df <- rename_first_existing(df, "genre", c("genre", "category"))
	df <- rename_first_existing(df, "publisher", c("publisher", "pub"))
	df <- rename_first_existing(df, "platform", c("platform", "console", "system"))
	df <- rename_first_existing(df, "na_sales", c("na_sales", "north_america_sales"))
	df <- rename_first_existing(df, "eu_sales", c("eu_sales", "europe_sales"))
	df <- rename_first_existing(df, "jp_sales", c("jp_sales", "japan_sales"))
	df <- rename_first_existing(df, "other_sales", c("other_sales", "rest_of_world_sales", "row_sales"))
	df <- rename_first_existing(df, "global_sales", c("global_sales", "world_sales", "total_sales"))

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
			game_title = as.character(game_title),
			release_year = suppressWarnings(as.integer(release_year)),
			genre = as.character(genre),
			publisher = as.character(publisher),
			platform = as.character(platform),
			na_sales = suppressWarnings(as.numeric(na_sales)),
			eu_sales = suppressWarnings(as.numeric(eu_sales)),
			jp_sales = suppressWarnings(as.numeric(jp_sales)),
			other_sales = suppressWarnings(as.numeric(other_sales)),
			global_sales = suppressWarnings(as.numeric(global_sales))
		)
}

derive_specific_genre <- function(primary_genre, steam_genres, steam_tags, game_title) {
	genre_text <- str_to_lower(
		paste(
			coalesce(primary_genre, ""),
			coalesce(steam_genres, ""),
			coalesce(steam_tags, ""),
			coalesce(game_title, ""),
			sep = " | "
		)
	)

	case_when(
		str_detect(genre_text, "battle\\s*royale|battleroyale") ~ "battle_royale",
		str_detect(genre_text, "roguelike|rogue-like|roguelite|rogue-lite") ~ "roguelike",
		str_detect(genre_text, "jrpg|j-rpg|japanese\\s*rpg") ~ "jrpg",
		str_detect(genre_text, "arpg|action\\s*rpg|action-rpg") ~ "arpg",
		str_detect(genre_text, "soulslike|souls-like") ~ "soulslike",
		str_detect(genre_text, "metroidvania") ~ "metroidvania",
		str_detect(genre_text, "moba|multiplayer\\s*online\\s*battle\\s*arena") ~ "moba",
		str_detect(genre_text, "deckbuilder|deck\\s*building|card\\s*battler") ~ "deckbuilder",
		str_detect(genre_text, "extraction\\s*shooter") ~ "extraction_shooter",
		str_detect(genre_text, "hero\\s*shooter") ~ "hero_shooter",
		str_detect(genre_text, "survival\\s*craft|crafting\\s*survival") ~ "survival_crafting",
		str_detect(genre_text, "visual\\s*novel") ~ "visual_novel",
		str_detect(genre_text, "strategy") ~ "strategy",
		str_detect(genre_text, "sports") ~ "sports",
		str_detect(genre_text, "racing") ~ "racing",
		str_detect(genre_text, "simulation") ~ "simulation",
		str_detect(genre_text, "adventure") ~ "adventure",
		str_detect(genre_text, "shooter") ~ "shooter",
		str_detect(genre_text, "role[- ]?playing|\\brpg\\b") ~ "rpg",
		str_detect(genre_text, "action") ~ "action",
		TRUE ~ "other"
	)
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

save_plot <- function(plot_obj, filename, width = 11, height = 7) {
	ggsave(
		filename = file.path(plots_dir, filename),
		plot = plot_obj,
		width = width,
		height = height,
		dpi = 300
	)
}

message("[1/6] Loading raw datasets...")

vg_path <- file.path(raw_dir, "vgchartz_raw.csv")
if (!file.exists(vg_path)) {
	stop("Missing data/raw/vgchartz_raw.csv. Run scripts/01_data_scraping.R first.")
}

console_sales_online_path <- file.path(raw_dir, "console_sales_online.csv")

steam_api_path <- file.path(raw_dir, "steam_api_raw.rds")
if (!file.exists(steam_api_path)) {
	stop("Missing data/raw/steam_api_raw.rds. Run scripts/01_data_scraping.R first.")
}

reviews_path <- file.path(raw_dir, "steam_reviews_raw.rds")
market_share_path <- file.path(raw_dir, "publisher_market_share_template.csv")

vg_raw <- read_csv(vg_path, show_col_types = FALSE)
steam_raw <- readRDS(steam_api_path)
reviews_raw <- if (file.exists(reviews_path)) readRDS(reviews_path) else tibble()
market_share_raw <- if (file.exists(market_share_path)) {
	read_csv(market_share_path, show_col_types = FALSE)
} else {
	tibble(publisher = character(), year = integer(), market_share_pct = numeric(), source_url = character())
}

vg_input <- standardize_sales_schema(vg_raw) %>%
	mutate(data_source = "vgchartz")

if (file.exists(console_sales_online_path)) {
	console_online_raw <- read_csv(console_sales_online_path, show_col_types = FALSE)
	console_online_input <- standardize_sales_schema(console_online_raw) %>%
		mutate(data_source = "console_online")
	vg_input <- bind_rows(vg_input, console_online_input)
	message("Online console sales file detected and appended: data/raw/console_sales_online.csv")
} else {
	message("No console_sales_online.csv found. Using vgchartz_raw.csv only.")
}

vg_input <- vg_input %>%
	distinct(game_title, platform, release_year, genre, publisher, .keep_all = TRUE)

message("[2/6] Cleaning and standardizing source data...")

vg_clean <- vg_input %>%
	mutate(
		game_title = str_squish(as.character(game_title)),
		platform = str_squish(as.character(platform)),
		genre = str_squish(as.character(genre)),
		publisher = str_squish(as.character(publisher)),
		release_year = suppressWarnings(as.integer(release_year)),
		across(c(na_sales, eu_sales, jp_sales, other_sales, global_sales), as.numeric),
		title_key = normalize_key(game_title),
		publisher_key = normalize_key(publisher)
	) %>%
	filter(!is.na(game_title), game_title != "")

steam_clean <- steam_raw %>%
	mutate(
		game_title = str_squish(as.character(game_title)),
		publisher = str_squish(as.character(publisher)),
		developer = str_squish(as.character(developer)),
		genres = str_squish(as.character(genres)),
		tags = str_squish(as.character(tags)),
		release_date = str_squish(as.character(release_date)),
		release_date_parsed = suppressWarnings(mdy(release_date)),
		steam_release_year = year(release_date_parsed),
		owner_midpoint = parse_owner_midpoint(owners),
		title_key = normalize_key(game_title),
		publisher_key = normalize_key(publisher)
	) %>%
	filter(!is.na(game_title), game_title != "") %>%
	group_by(title_key) %>%
	arrange(desc(coalesce(positive, 0) + coalesce(negative, 0)), .by_group = TRUE) %>%
	slice(1) %>%
	ungroup()

market_share_clean <- market_share_raw %>%
	mutate(
		publisher = str_squish(as.character(publisher)),
		year = suppressWarnings(as.integer(year)),
		market_share_pct = suppressWarnings(as.numeric(market_share_pct)),
		publisher_key = normalize_key(publisher)
	) %>%
	filter(!is.na(publisher_key), publisher_key != "", !is.na(year), !is.na(market_share_pct)) %>%
	group_by(publisher_key, year) %>%
	summarise(
		market_share_pct = mean(market_share_pct, na.rm = TRUE),
		publisher = first(publisher),
		.groups = "drop"
	)

message("[3/6] Creating sentiment score aggregates from review text...")

if (nrow(reviews_raw) > 0) {
	sentiment_scores <- reviews_raw %>%
		mutate(
			review_text = as.character(review_text),
			review_word_count = str_count(coalesce(review_text, ""), "\\S+"),
			voted_up = as.logical(voted_up),
			weighted_vote_score = suppressWarnings(as.numeric(weighted_vote_score)),
			votes_up = suppressWarnings(as.numeric(votes_up))
		) %>%
		group_by(appid) %>%
		summarise(
			review_count = n(),
			pct_positive_review = mean(voted_up, na.rm = TRUE),
			avg_weighted_vote_score = mean(weighted_vote_score, na.rm = TRUE),
			avg_votes_up = mean(votes_up, na.rm = TRUE),
			avg_review_word_count = mean(review_word_count, na.rm = TRUE),
			.groups = "drop"
		) %>%
		mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))
} else {
	sentiment_scores <- tibble(
		appid = integer(),
		review_count = integer(),
		pct_positive_review = numeric(),
		avg_weighted_vote_score = numeric(),
		avg_votes_up = numeric(),
		avg_review_word_count = numeric()
	)
}

write_csv(sentiment_scores, file.path(processed_dir, "sentiment_scores.csv"))

steam_enriched <- steam_clean %>%
	left_join(sentiment_scores, by = "appid")

message("[4/6] Joining datasets and engineering model-ready features...")

master_dataset <- vg_clean %>%
	left_join(
		steam_enriched %>%
			select(
				title_key,
				appid,
				developer,
				steam_publisher = publisher,
				steam_release_year,
				price,
				initialprice,
				discount,
				ccu,
				userscore,
				owner_midpoint,
				genres,
				tags,
				windows,
				mac,
				linux,
				review_count,
				pct_positive_review,
				avg_weighted_vote_score,
				avg_votes_up,
				avg_review_word_count
			),
		by = "title_key"
	) %>%
	mutate(
		year_for_join = coalesce(release_year, steam_release_year),
		market_publisher_key = coalesce(publisher_key, normalize_key(steam_publisher))
	) %>%
	left_join(
		market_share_clean %>%
			select(publisher_key, year, market_share_pct),
		by = c("market_publisher_key" = "publisher_key", "year_for_join" = "year")
	) %>%
	mutate(
		specific_genre = derive_specific_genre(genre, genres, tags, game_title),
		release_year_competition = coalesce(release_year, steam_release_year),
		log_global_sales = log1p(global_sales),
		avg_regional_sales = rowMeans(across(c(na_sales, eu_sales, jp_sales, other_sales)), na.rm = TRUE),
		commercial_success = ifelse(global_sales >= quantile(global_sales, 0.75, na.rm = TRUE), 1L, 0L),
		commercial_success = factor(commercial_success, levels = c(0L, 1L), labels = c("low", "high")),
		release_period = case_when(
			!is.na(release_year) & release_year < 2005 ~ "pre_2005",
			!is.na(release_year) & release_year >= 2005 & release_year < 2015 ~ "2005_2014",
			!is.na(release_year) & release_year >= 2015 ~ "2015_plus",
			TRUE ~ "unknown"
		)
	) %>%
	group_by(release_year_competition, specific_genre) %>%
	mutate(
		competitors = if_else(
			is.na(release_year_competition) | is.na(specific_genre),
			NA_integer_,
			as.integer(pmax(n() - 1L, 0L))
		)
	) %>%
	ungroup() %>%
	select(-release_year_competition)

master_dataset <- master_dataset %>%
	mutate(
		specific_genre = factor(specific_genre)
	)

all_missing_cols <- names(master_dataset)[vapply(master_dataset, function(col) all(is.na(col)), logical(1))]
if (length(all_missing_cols) > 0) {
	message(
		"Dropping columns with 100% missing values: ",
		paste(all_missing_cols, collapse = ", ")
	)
	master_dataset <- master_dataset %>%
		select(-all_of(all_missing_cols))
}

write_csv(master_dataset, file.path(processed_dir, "master_dataset.csv"))

message("[5/6] Building overview plots for numeric, categorical, temporal, and text-derived features...")

plot_top_genre <- master_dataset %>%
	filter(!is.na(genre), genre != "", str_to_lower(genre) != "other") %>%
	group_by(genre) %>%
	summarise(game_count = n(), .groups = "drop") %>%
	filter(game_count >= 20) %>%
	mutate(genre = fct_reorder(genre, game_count)) %>%
	ggplot(aes(x = genre, y = game_count)) +
	geom_col(fill = "#3B8EA5") +
	coord_flip() +
	scale_y_continuous(labels = label_number()) +
	labs(
		title = "Number of Games by Genre (n >= 20)",
		x = "Genre",
		y = "Game Count"
	) +
	theme_minimal(base_size = 13)
save_plot(plot_top_genre, "games_by_genre_count.png")

plot_platform_mix <- master_dataset %>%
	filter(!is.na(platform), platform != "") %>%
	mutate(platform = fct_lump_n(platform, n = 10, other_level = "Other")) %>%
	filter(platform != "Other") %>%
	count(platform, wt = global_sales, name = "weighted_sales") %>%
	mutate(platform = fct_reorder(platform, weighted_sales)) %>%
	ggplot(aes(x = platform, y = weighted_sales, fill = platform)) +
	geom_col(show.legend = FALSE) +
	coord_flip() +
	scale_y_continuous(labels = label_number()) +
	labs(
		title = "Weighted Sales by Platform",
		x = "Platform",
		y = "Total Global Sales (millions)"
	) +
	theme_minimal(base_size = 13)
save_plot(plot_platform_mix, "platform_weighted_global_sales.png")

steam_plot_data <- steam_clean %>%
	filter(!is.na(userscore), !is.na(steam_release_year), is.finite(userscore)) %>%
	mutate(
		deployment_type = case_when(
			windows & (mac | linux) ~ "cross_platform",
			windows & !(mac | linux) ~ "windows_only",
			mac | linux ~ "non_windows",
			TRUE ~ "unknown"
		)
	)

if (nrow(steam_plot_data) >= 10) {
	plot_steam_userscore_trend <- steam_plot_data %>%
		ggplot(aes(x = steam_release_year, y = userscore, color = deployment_type)) +
		geom_point(alpha = 0.45) +
		geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
		scale_y_continuous(labels = label_number(accuracy = 1)) +
		labs(
			title = "Steam Review Percentage by Release Year",
			x = "Steam Release Year",
			y = "Review Percentage",
			color = "Platform Support"
		) +
		theme_minimal(base_size = 13)
	save_plot(plot_steam_userscore_trend, "steam_review_percentage_trend.png")
} else {
	message("Skipping steam_review_percentage_trend.png due to insufficient Steam data.")
}

publisher_overview <- master_dataset %>%
	filter(!is.na(publisher), publisher != "") %>%
	group_by(publisher) %>%
	summarise(
		total_global_sales = sum(global_sales, na.rm = TRUE),
		game_count = n(),
		avg_sales_per_game = mean(global_sales, na.rm = TRUE),
		median_sales_per_game = median(global_sales, na.rm = TRUE),
		active_years = n_distinct(release_year[!is.na(release_year)]),
		platform_count = n_distinct(platform[!is.na(platform) & platform != ""]),
		genre_count = n_distinct(genre[!is.na(genre) & genre != ""]),
		avg_market_share_pct = mean(market_share_pct, na.rm = TRUE),
		high_success_rate = mean(commercial_success == "high", na.rm = TRUE),
		.groups = "drop"
	) %>%
	mutate(
		avg_market_share_pct = ifelse(is.nan(avg_market_share_pct), NA_real_, avg_market_share_pct),
		high_success_rate = ifelse(is.nan(high_success_rate), NA_real_, high_success_rate)
	) %>%
	arrange(desc(total_global_sales))

write_csv(publisher_overview, file.path(processed_dir, "publisher_overview.csv"))

plot_publisher_sales <- publisher_overview %>%
	slice_head(n = 20) %>%
	mutate(publisher = fct_reorder(publisher, total_global_sales)) %>%
	ggplot(aes(x = publisher, y = total_global_sales)) +
	geom_col(fill = "#6D597A") +
	coord_flip() +
	scale_y_continuous(labels = label_number()) +
	labs(
		title = "Top 20 Publishers by Total Global Sales",
		x = "Publisher",
		y = "Total Global Sales (millions)"
	) +
	theme_minimal(base_size = 13)
save_plot(plot_publisher_sales, "top_publishers_total_global_sales.png")

plot_publisher_bubble <- publisher_overview %>%
	filter(game_count >= 5, is.finite(total_global_sales), is.finite(avg_sales_per_game)) %>%
	slice_head(n = 40) %>%
	mutate(label = ifelse(row_number() <= 15, publisher, "")) %>%
	ggplot(aes(x = game_count, y = total_global_sales, size = avg_sales_per_game, color = platform_count)) +
	geom_point(alpha = 0.75) +
	geom_text(aes(label = label), size = 3, nudge_y = 0.4, check_overlap = TRUE, show.legend = FALSE) +
	scale_y_continuous(labels = label_number()) +
	scale_size_continuous(labels = label_number(accuracy = 0.01)) +
	labs(
		title = "Publisher Sales Overview Bubble Plot",
		x = "Game Count",
		y = "Total Global Sales (millions)",
		size = "Avg Sales per Game",
		color = "Platform Count"
	) +
	theme_minimal(base_size = 13)
save_plot(plot_publisher_bubble, "publisher_sales_overview_bubble.png")

top_publishers <- publisher_overview %>%
	slice_head(n = 8) %>%
	pull(publisher)

plot_publisher_share_trend <- market_share_clean %>%
	filter(publisher %in% top_publishers) %>%
	group_by(publisher, year) %>%
	summarise(market_share_pct = mean(market_share_pct, na.rm = TRUE), .groups = "drop") %>%
	ggplot(aes(x = year, y = market_share_pct, color = publisher)) +
	geom_line(linewidth = 0.9) +
	geom_point(size = 1.6) +
	labs(
		title = "Market Share Trend of Top Publishers",
		x = "Year",
		y = "Market Share (%)",
		color = "Publisher"
	) +
	theme_minimal(base_size = 13)
save_plot(plot_publisher_share_trend, "top_publishers_market_share_trend.png")

plot_year_trend <- master_dataset %>%
	filter(!is.na(release_year)) %>%
	group_by(release_year) %>%
	summarise(game_count = n(), .groups = "drop") %>%
	ggplot(aes(x = release_year, y = game_count)) +
	geom_line(color = "#1F7A8C", linewidth = 1) +
	geom_point(color = "#1F7A8C", size = 1.5) +
	labs(
		title = "Number of Games by Release Year",
		x = "Release Year",
		y = "Game Count"
	) +
	theme_minimal(base_size = 13)
save_plot(plot_year_trend, "games_by_release_year_count.png")

key_numeric_vars <- c(
	"global_sales",
	"na_sales",
	"eu_sales",
	"jp_sales",
	"other_sales",
	"price",
	"userscore",
	"ccu",
	"owner_midpoint",
	"review_count",
	"competitors"
)

key_numeric_vars <- intersect(key_numeric_vars, names(master_dataset))

key_variable_overview <- master_dataset %>%
	summarise(
		across(
			all_of(key_numeric_vars),
			list(
				non_missing = ~ sum(!is.na(.x)),
				missing_pct = ~ mean(is.na(.x)),
				mean = ~ ifelse(all(is.na(.x)), NA_real_, mean(.x, na.rm = TRUE)),
				median = ~ ifelse(all(is.na(.x)), NA_real_, median(.x, na.rm = TRUE)),
				min = ~ ifelse(all(is.na(.x)), NA_real_, min(.x, na.rm = TRUE)),
				max = ~ ifelse(all(is.na(.x)), NA_real_, max(.x, na.rm = TRUE))
			),
			.names = "{.col}__{.fn}"
		)
	) %>%
	pivot_longer(cols = everything(), names_to = "metric", values_to = "value") %>%
	separate(metric, into = c("variable", "stat"), sep = "__") %>%
	pivot_wider(names_from = stat, values_from = value) %>%
	mutate(missing_pct = round(missing_pct * 100, 2))

write_csv(key_variable_overview, file.path(processed_dir, "key_variable_overview.csv"))

plot_key_missing <- key_variable_overview %>%
	mutate(variable = fct_reorder(variable, missing_pct)) %>%
	ggplot(aes(x = variable, y = missing_pct)) +
	geom_col(fill = "#7A306C") +
	coord_flip() +
	labs(
		title = "Missing Percentage of Key Variables",
		x = "Variable",
		y = "Missing (%)"
	) +
	theme_minimal(base_size = 13)
save_plot(plot_key_missing, "key_variables_missing_percentage.png")

plot_key_non_missing <- key_variable_overview %>%
	mutate(variable = fct_reorder(variable, non_missing)) %>%
	ggplot(aes(x = variable, y = non_missing)) +
	geom_col(fill = "#2A6F97") +
	coord_flip() +
	scale_y_continuous(labels = label_number()) +
	labs(
		title = "Non-Missing Records of Key Variables",
		x = "Variable",
		y = "Record Count"
	) +
	theme_minimal(base_size = 13)
save_plot(plot_key_non_missing, "key_variables_non_missing_count.png")

plot_specific_genre_competition <- master_dataset %>%
	filter(!is.na(specific_genre), !is.na(competitors), as.character(specific_genre) != "other") %>%
	group_by(specific_genre) %>%
	summarise(avg_competitors = mean(competitors, na.rm = TRUE), n = n(), .groups = "drop") %>%
	filter(n >= 20) %>%
	mutate(specific_genre = fct_reorder(specific_genre, avg_competitors)) %>%
	ggplot(aes(x = specific_genre, y = avg_competitors)) +
	geom_col(fill = "#4C956C") +
	coord_flip() +
	labs(
		title = "Average Same-Genre Competitors at Release",
		x = "Specific Genre",
		y = "Average Competitors"
	) +
	theme_minimal(base_size = 13)
save_plot(plot_specific_genre_competition, "specific_genre_competition_overview.png")

missing_df <- master_dataset %>%
	summarise(across(everything(), ~ mean(is.na(.x)))) %>%
	pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_ratio") %>%
	arrange(desc(missing_ratio)) %>%
	slice_head(n = 20)

plot_missing <- missing_df %>%
	mutate(variable = fct_reorder(variable, missing_ratio)) %>%
	ggplot(aes(x = variable, y = missing_ratio)) +
	geom_col(fill = "#6C757D") +
	coord_flip() +
	scale_y_continuous(labels = label_percent(accuracy = 1)) +
	labs(
		title = "Top 20 Variables by Missingness",
		x = "Variable",
		y = "Missing Ratio"
	) +
	theme_minimal(base_size = 13)
save_plot(plot_missing, "top_variables_missingness.png")

message("[6/6] Data curation completed.")
message("Master rows: ", nrow(master_dataset))
message("Master columns: ", ncol(master_dataset))
message("Plots written to: data/plots")

