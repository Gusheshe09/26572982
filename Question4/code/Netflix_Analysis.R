# ============================================================
# Question 4: Netflix
# Author: 26572982
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# STEP 1: Loading data (as per exam instructions)
# ------------------------------------------------------------

Titles      <- readRDS("data/netflix/titles.rds")
Credits     <- readRDS("data/netflix/credits.rds")
Movie_Info  <- read_csv("data/netflix/netflix_movies.csv",
                        show_col_types = FALSE)

# I have to also load HBO data from Q2 for platform comparison
HBO_titles  <- readRDS("data/US_Baby_names/HBO_titles.rds")

# ------------------------------------------------------------
# STEP 2: Clean and prepare Titles
# ------------------------------------------------------------

clean_titles <- function(df) {
    df %>%
        # Clean genres column - remove brackets and quotes
        mutate(genres = str_remove_all(genres, "\\[|\\]|\\'")) %>%
        mutate(genres = str_trim(genres)) %>%
        # Clean production countries - same format issue
        mutate(production_countries = str_remove_all(
            production_countries, "\\[|\\]|\\'")) %>%
        mutate(production_countries = str_trim(production_countries)) %>%
        # Filter to reasonable year range
        filter(release_year >= 1980, release_year <= 2022) %>%
        # Flag movies vs shows
        mutate(type = factor(type, levels = c("MOVIE", "SHOW")))
}

clean_movies <- function(df) {
    df %>%
        # Extract numeric duration for movies (remove " min")
        mutate(duration_mins = as.numeric(str_extract(duration, "\\d+"))) %>%
        # Clean country column
        mutate(country = str_trim(country)) %>%
        # Clean date added
        mutate(date_added = mdy(date_added)) %>%
        filter(!is.na(date_added))
}

Titles_clean <- clean_titles(Titles)
Movies_clean <- clean_movies(Movie_Info)

# Quick checks
cat("=== TITLES CLEAN ===\n")
glimpse(Titles_clean)
cat("\nType distribution:\n")
print(table(Titles_clean$type))
cat("\nYear range:", range(Titles_clean$release_year), "\n")

cat("\n=== MOVIES CLEAN ===\n")
glimpse(Movies_clean)
cat("\nDuration range:", range(Movies_clean$duration_mins, na.rm=TRUE), "\n")




# ------------------------------------------------------------
# STEP 3: Plot 1 - Content by Country (Top 10)
# ------------------------------------------------------------

plot_content_by_country <- function(df) {
    df %>%
        filter(type == "MOVIE") %>%
        filter(!is.na(production_countries),
               production_countries != "") %>%
        # Some titles have multiple countries - take first one
        mutate(country = str_extract(production_countries,
                                     "^[A-Z]+")) %>%
        filter(!is.na(country)) %>%
        count(country, sort = TRUE) %>%
        slice_head(n = 10) %>%
        ggplot(aes(x = n,
                   y = reorder(country, n),
                   fill = n)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = n), hjust = -0.2, size = 4) +
        scale_fill_gradient(low = "#2980B9", high = "#1A252F") +
        coord_cartesian(xlim = c(0, 2200)) +
        labs(
            title = "Top 10 Countries by Number of Netflix Movies",
            subtitle = "Based on primary production country",
            x = "Number of Movies", y = "Country Code"
        ) +
        theme_minimal()
}

p1_country <- plot_content_by_country(Titles_clean)
p1_country




# ------------------------------------------------------------
# STEP 4: Plot 2 - IMDb Ratings by Country (Top 10)
# ------------------------------------------------------------

plot_ratings_by_country <- function(df) {
    df %>%
        filter(type == "MOVIE") %>%
        filter(!is.na(production_countries),
               production_countries != "") %>%
        mutate(country = str_extract(production_countries, "^[A-Z]+")) %>%
        filter(!is.na(country), !is.na(imdb_score)) %>%
        group_by(country) %>%
        summarise(
            avg_rating = mean(imdb_score, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        # Only keeping countries with at least 10 movies for reliability
        filter(n >= 10) %>%
        arrange(desc(avg_rating)) %>%
        slice_head(n = 10) %>%
        ggplot(aes(x = avg_rating,
                   y = reorder(country, avg_rating),
                   fill = avg_rating)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(round(avg_rating, 2),
                                     " (n=", n, ")")),
                  hjust = -0.1, size = 3.5) +
        scale_fill_gradient(low = "#2980B9", high = "#1A252F") +
        coord_cartesian(xlim = c(0, 9)) +
        labs(
            title = "Top 10 Countries by Average IMDb Rating on Netflix",
            subtitle = "Filtered to countries with at least 10 movies",
            x = "Average IMDb Score", y = "Country Code"
        ) +
        theme_minimal()
}

p2_ratings <- plot_ratings_by_country(Titles_clean)
p2_ratings






# ------------------------------------------------------------
# STEP 5: Plot 3 - Movie Length by Country
# ------------------------------------------------------------

plot_movie_length_by_country <- function(df) {
    df %>%
        filter(type == "MOVIE") %>%
        filter(!is.na(production_countries),
               production_countries != "") %>%
        mutate(country = str_extract(production_countries, "^[A-Z]+")) %>%
        filter(!is.na(country), !is.na(runtime)) %>%
        # Filtering out unrealistic runtimes
        filter(runtime >= 60, runtime <= 240) %>%
        group_by(country) %>%
        summarise(
            avg_runtime = mean(runtime, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        filter(n >= 10) %>%
        arrange(desc(avg_runtime)) %>%
        slice_head(n = 10) %>%
        ggplot(aes(x = avg_runtime,
                   y = reorder(country, avg_runtime),
                   fill = avg_runtime)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(round(avg_runtime, 0),
                                     " mins (n=", n, ")")),
                  hjust = -0.1, size = 3.5) +
        geom_vline(xintercept = 90, linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        annotate("text", x = 91, y = 1.5,
                 label = "90 min\nstandard",
                 color = "red", size = 3, hjust = 0) +
        scale_fill_gradient(low = "#2980B9", high = "#1A252F") +
        coord_cartesian(xlim = c(0, 160)) +
        labs(
            title = "Top 10 Countries by Average Movie Length on Netflix",
            subtitle = "Filtered to countries with at least 10 movies. Red line = 90 min industry standard",
            x = "Average Runtime (minutes)", y = "Country Code"
        ) +
        theme_minimal()
}

p3_length <- plot_movie_length_by_country(Titles_clean)
p3_length




# ------------------------------------------------------------
# STEP 6: Plot 4 - Top Genres on Netflix
# ------------------------------------------------------------

plot_top_genres <- function(df) {
    df %>%
        filter(type == "MOVIE") %>%
        filter(!is.na(genres), genres != "") %>%
        # Spliting multiple genres per movie into separate rows
        separate_rows(genres, sep = ",") %>%
        mutate(genres = str_trim(genres)) %>%
        filter(genres != "") %>%
        count(genres, sort = TRUE) %>%
        slice_head(n = 12) %>%
        ggplot(aes(x = n,
                   y = reorder(genres, n),
                   fill = n)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = n), hjust = -0.2, size = 4) +
        scale_fill_gradient(low = "#E50914", high = "#831010") +
        coord_cartesian(xlim = c(0, 2000)) +
        labs(
            title = "Most Common Movie Genres on Netflix",
            subtitle = "Movies can belong to multiple genres",
            x = "Number of Movies", y = "Genre"
        ) +
        theme_minimal()
}

# ------------------------------------------------------------
# STEP 7: Plot 5 - Text Analysis on Descriptions
# ------------------------------------------------------------

plot_description_words <- function(df) {

    # Common words to remove
    stop_words <- c("the", "a", "an", "and", "or", "but", "in",
                    "on", "at", "to", "for", "of", "with", "his",
                    "her", "their", "is", "are", "was", "were",
                    "he", "she", "they", "it", "be", "as", "by",
                    "from", "that", "this", "who", "when", "after",
                    "into", "has", "have", "while", "two", "its",
                    "between", "must", "what", "one", "finds",
                    "through", "about", "out", "up", "him", "them",
                    "set", "life", "new", "young", "also", "get")

    df %>%
        filter(type == "MOVIE") %>%
        filter(!is.na(description)) %>%
        # Tokenize descriptions into words
        mutate(description = str_to_lower(description)) %>%
        mutate(words = str_split(description, "\\s+")) %>%
        unnest(words) %>%
        # Clean words
        mutate(words = str_remove_all(words, "[^a-z]")) %>%
        filter(nchar(words) > 3) %>%
        filter(!words %in% stop_words) %>%
        count(words, sort = TRUE) %>%
        slice_head(n = 20) %>%
        ggplot(aes(x = n,
                   y = reorder(words, n),
                   fill = n)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = n), hjust = -0.2, size = 4) +
        scale_fill_gradient(low = "#E50914", high = "#831010") +
        coord_cartesian(xlim = c(0, 700)) +
        labs(
            title = "Most Common Words in Netflix Movie Descriptions",
            subtitle = "Stop words removed. Reveals dominant themes in Netflix content",
            x = "Word Count", y = "Word"
        ) +
        theme_minimal()
}

p4_genres <- plot_top_genres(Titles_clean)
p5_words  <- plot_description_words(Titles_clean)

p4_genres
p5_words





# ------------------------------------------------------------
# STEP 8: Plot 6 - Netflix vs HBO Platform Comparison
# ------------------------------------------------------------

# Preparing Netflix genres
netflix_genres <- Titles_clean %>%
    filter(type == "MOVIE") %>%
    filter(!is.na(genres), genres != "") %>%
    separate_rows(genres, sep = ",") %>%
    mutate(genres = str_trim(genres)) %>%
    filter(genres != "") %>%
    count(genres, sort = TRUE) %>%
    slice_head(n = 8) %>%
    mutate(platform = "Netflix")

# Preparing HBO genres
hbo_genres <- HBO_titles %>%
    filter(type == "MOVIE") %>%
    filter(!is.na(genres), genres != "") %>%
    mutate(genres = str_remove_all(genres, "\\[|\\]|\\'")) %>%
    separate_rows(genres, sep = ",") %>%
    mutate(genres = str_trim(genres)) %>%
    filter(genres != "") %>%
    count(genres, sort = TRUE) %>%
    slice_head(n = 8) %>%
    mutate(platform = "HBO")

# Combining them
platform_genres <- bind_rows(netflix_genres, hbo_genres)

plot_platform_comparison <- function(df) {
    df %>%
        ggplot(aes(x = n,
                   y = reorder(genres, n),
                   fill = platform)) +
        geom_col(position = "dodge", show.legend = TRUE) +
        scale_fill_manual(values = c(
            "Netflix" = "#E50914",
            "HBO"     = "#00A8E0"
        )) +
        labs(
            title = "Top Genres: Netflix vs HBO",
            subtitle = "Comparing content strategy across two major streaming platforms",
            x = "Number of Movies", y = "Genre",
            fill = "Platform"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
}

# ------------------------------------------------------------
# STEP 9: Plot 7 - Ratings comparison Netflix vs HBO
# ------------------------------------------------------------

plot_ratings_comparison <- function(netflix_df, hbo_df) {

    netflix_scores <- netflix_df %>%
        filter(type == "MOVIE", !is.na(imdb_score)) %>%
        select(imdb_score) %>%
        mutate(platform = "Netflix")

    hbo_scores <- hbo_df %>%
        filter(type == "MOVIE", !is.na(imdb_score)) %>%
        select(imdb_score) %>%
        mutate(platform = "HBO")

    bind_rows(netflix_scores, hbo_scores) %>%
        ggplot(aes(x = imdb_score, fill = platform)) +
        geom_density(alpha = 0.6) +
        scale_fill_manual(values = c(
            "Netflix" = "#E50914",
            "HBO"     = "#00A8E0"
        )) +
        geom_vline(xintercept = mean(netflix_scores$imdb_score, na.rm=TRUE),
                   color = "#E50914", linetype = "dashed", linewidth = 0.8) +
        geom_vline(xintercept = mean(hbo_scores$imdb_score, na.rm=TRUE),
                   color = "#00A8E0", linetype = "dashed", linewidth = 0.8) +
        labs(
            title = "IMDb Score Distribution: Netflix vs HBO",
            subtitle = "Dashed lines = platform average ratings",
            x = "IMDb Score", y = "Density",
            fill = "Platform"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
}

p6_platform <- plot_platform_comparison(platform_genres)
p7_ratings  <- plot_ratings_comparison(Titles_clean, HBO_titles)

p6_platform
p7_ratings




