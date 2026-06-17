# ============================================================
# Question 1: Coffee Hub
# Author: 26572982
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# FUNCTION: Loading th coffee data and handling encoding issues
# ------------------------------------------------------------
load_coffee <- function(path) {

    df <- read_csv(path, show_col_types = FALSE,
                   locale = locale(encoding = "UTF-8"))

    # Clean curly quotes and other special characters from name column
    df <- df %>%
        mutate(name = iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT"),
               name = str_squish(name),
               roast = ifelse(is.na(roast), "Unknown", roast))

    df
}

# ------------------------------------------------------------
# Loading the data
# ------------------------------------------------------------
Coffee <- load_coffee("data/Coffee/Coffee.csv")

# Quick check
glimpse(Coffee)




# ------------------------------------------------------------
# STEP 1: Cleaning and preparing the data
# ------------------------------------------------------------

clean_coffee <- function(df) {

    df %>%
        # Parse review date properly
        mutate(review_date = as.Date(paste0("01-", review_date),
                                     format = "%d-%b-%y")) %>%
        # Standardising roast levels as a ordered factor
        mutate(roast = factor(roast,
                              levels = c("Light", "Medium-Light", "Medium",
                                         "Medium-Dark", "Dark", "Unknown"),
                              ordered = TRUE)) %>%
        # Combine all three descriptions into one text column for keyword search
        mutate(full_desc = paste(desc_1, desc_2, desc_3, sep = " ")) %>%
        # Remove rows with missing cost
        filter(!is.na(Cost_Per_100g))
}

Coffee_clean <- clean_coffee(Coffee)

# Confirm
glimpse(Coffee_clean)


Coffee_clean %>% count(roast)
Coffee_clean %>% count(loc_country, sort = TRUE) %>% head(10)




# ------------------------------------------------------------
# STEP 2: Keyword matching from Stellenbosch student survey
# ------------------------------------------------------------

# Keywords I identified from the word cloud in the exam paper
stellenbosch_keywords <- c("chocolate", "sweet", "aroma", "mouthfeel",
                           "fruit", "bright", "floral", "citrus",
                           "balanced", "smooth", "dark", "rich",
                           "notes", "finish", "savory")

# Function that checks how many keywords appear in a coffee's description
match_keywords <- function(full_desc, keywords) {
    full_desc_lower <- str_to_lower(full_desc)
    matches <- map_lgl(keywords, ~ str_detect(full_desc_lower, .x))
    sum(matches)
}

# Applying to each row
Coffee_clean <- Coffee_clean %>%
    mutate(keyword_score = map_int(full_desc,
                                   ~ match_keywords(.x, stellenbosch_keywords)))

# Checking the distribution of keyword scores
Coffee_clean %>%
    count(keyword_score) %>%
    arrange(desc(keyword_score))




# ------------------------------------------------------------
# STEP 3: Filter to recommended coffees
# ------------------------------------------------------------

Coffee_recommended <- Coffee_clean %>%
    filter(keyword_score >= 10)

# ------------------------------------------------------------
# STEP 4: Plot 1 - Average Rating by Roast Type
# ------------------------------------------------------------

plot_roast_rating <- function(df) {

    df %>%
        filter(roast != "Unknown") %>%
        group_by(roast) %>%
        summarise(
            avg_rating = mean(Rating, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        ggplot(aes(x = roast, y = avg_rating, fill = roast)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = round(avg_rating, 1)),
                  vjust = -0.5, size = 4) +
        labs(
            title = "Average Expert Rating by Roast Type",
            subtitle = "Based on Stellenbosch student keyword-matched coffees",
            x = "Roast Type",
            y = "Average Rating (out of 100)"
        ) +
        theme_minimal()
}

p1 <- plot_roast_rating(Coffee_recommended)
p1


# ------------------------------------------------------------
# STEP 5: Plot 2 - Top 10 Countries by Average Rating
# ------------------------------------------------------------

plot_country_rating <- function(df) {

    df %>%
        group_by(loc_country) %>%
        summarise(
            avg_rating = mean(Rating, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        # Only keep countries with at least 5 coffees for reliability
        filter(n >= 5) %>%
        arrange(desc(avg_rating)) %>%
        slice_head(n = 10) %>%
        ggplot(aes(x = avg_rating,
                   y = reorder(loc_country, avg_rating),
                   fill = avg_rating)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(round(avg_rating, 1), " (n=", n, ")")),
                  hjust = -0.1, size = 3.5) +
        scale_x_continuous(limits = c(0, 100)) +
        labs(
            title = "Top Roaster Countries by Average Expert Rating",
            subtitle = "Filtered to countries with at least 5 coffees in recommended set",
            x = "Average Rating (out of 100)",
            y = "Country"
        ) +
        theme_minimal()
}

p2 <- plot_country_rating(Coffee_recommended)
p2


# ------------------------------------------------------------
# STEP 6: Plot 3 - Cost vs Rating scatter plot
# ------------------------------------------------------------

plot_cost_rating <- function(df) {

    df %>%
        ggplot(aes(x = Cost_Per_100g, y = Rating, color = roast)) +
        geom_point(alpha = 0.6, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "black",
                    linetype = "dashed") +
        scale_x_continuous(labels = scales::dollar_format()) +
        labs(
            title = "Cost vs Expert Rating for Recommended Coffees",
            subtitle = "Does paying more guarantee a better coffee?",
            x = "Cost per 100g (USD)",
            y = "Expert Rating (out of 100)",
            color = "Roast Type"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
}

p3 <- plot_cost_rating(Coffee_recommended)
p3




# ------------------------------------------------------------
# STEP 7: Plot 4 - Top 10 Roasters by Average Rating
# ------------------------------------------------------------

plot_top_roasters <- function(df) {

    df %>%
        group_by(roaster) %>%
        summarise(
            avg_rating = mean(Rating, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        # Only keep roasters with at least 3 coffees for reliability
        filter(n >= 3) %>%
        arrange(desc(avg_rating)) %>%
        slice_head(n = 10) %>%
        ggplot(aes(x = avg_rating,
                   y = reorder(roaster, avg_rating),
                   fill = avg_rating)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(round(avg_rating, 1), " (n=", n, ")")),
                  hjust = -0.1, size = 3.5) +
        scale_x_continuous(limits = c(0, 100)) +
        labs(
            title = "Top 10 Roasters by Average Expert Rating",
            subtitle = "Filtered to roasters with at least 3 coffees in recommended set",
            x = "Average Rating (out of 100)",
            y = "Roaster"
        ) +
        theme_minimal()
}

p4 <- plot_top_roasters(Coffee_recommended)
p4



# ------------------------------------------------------------
# STEP 8: Plot 5 - Rating Distribution by Roast Type (Boxplot)
# ------------------------------------------------------------

plot_rating_distribution <- function(df) {

    df %>%
        filter(roast != "Unknown") %>%
        ggplot(aes(x = roast, y = Rating, fill = roast)) +
        geom_boxplot(show.legend = FALSE, alpha = 0.7) +
        geom_jitter(aes(color = roast), show.legend = FALSE,
                    width = 0.2, alpha = 0.4, size = 1.5) +
        labs(
            title = "Rating Distribution by Roast Type",
            subtitle = "Spread and consistency of expert ratings across roast strengths",
            x = "Roast Type",
            y = "Expert Rating (out of 100)"
        ) +
        theme_minimal()
}

p5 <- plot_rating_distribution(Coffee_recommended)
p5


## Suggestion on the more consistent suppliers

Coffee_recommended %>%
  filter(roast == "Medium-Light") %>%
  group_by(roaster) %>%
  summarise(
    avg_rating = mean(Rating, na.rm = TRUE),
    min_rating = min(Rating, na.rm = TRUE),
    max_rating = max(Rating, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  # Filter for reliability - at least 3 coffees AND minimum rating above 93
  filter(n >= 3, min_rating >= 93) %>%
  arrange(desc(avg_rating))



# ------------------------------------------------------------
# STEP 9: Plot 6 - Most Consistent Medium-Light Roasters
# ------------------------------------------------------------

plot_consistent_medlight <- function(df) {

    df %>%
        filter(roast == "Medium-Light") %>%
        group_by(roaster) %>%
        summarise(
            avg_rating = mean(Rating, na.rm = TRUE),
            sd_rating = sd(Rating, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        # Need at least 3 coffees to measure consistency
        filter(n >= 3) %>%
        # Lower sd = more consistent
        arrange(sd_rating) %>%
        slice_head(n = 10) %>%
        ggplot(aes(x = sd_rating,
                   y = reorder(roaster, -sd_rating),
                   fill = avg_rating)) +
        geom_col(show.legend = TRUE) +
        geom_text(aes(label = paste0("Avg: ", round(avg_rating, 1),
                                     " | n=", n)),
                  hjust = -0.1, size = 3.5) +
        scale_x_continuous(limits = c(0, 3)) +
        scale_fill_gradient(low = "darkblue", high = "lightblue",
                            name = "Avg Rating") +
        labs(
            title = "Most Consistent Medium-Light Roasters",
            subtitle = "Ranked by lowest rating standard deviation (min. 3 coffees)",
            x = "Standard Deviation of Rating (lower = more consistent)",
            y = "Roaster"
        ) +
        theme_minimal()
}

p6 <- plot_consistent_medlight(Coffee_recommended)
p6


