# ============================================================
# Question 2: Baby Names
# Author: 26572982
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# STEP 1: Loading all data (as per exam instructions)
# ------------------------------------------------------------

Baby_Names    <- readRDS("data/US_Baby_names/Baby_Names_By_US_State.rds")
Top_100_Billboard <- readRDS("data/US_Baby_names/charts.rds") %>%
    rename(last_week = `last-week`,
           peak_rank = `peak-rank`,
           weeks_on_board = `weeks-on-board`)
HBO_titles  <- readRDS("data/US_Baby_names/HBO_titles.rds")
HBO_credits <- readRDS("data/US_Baby_names/HBO_credits.rds")

# ------------------------------------------------------------
# STEP 2: Aggregating nationally (sum across all states)
# ------------------------------------------------------------

aggregate_national <- function(df) {
    df %>%
        group_by(Year, Gender, Name) %>%
        summarise(Count = sum(Count), .groups = "drop")
}

Baby_National <- aggregate_national(Baby_Names)

# Quick check
glimpse(Baby_National)
Baby_National %>% count(Gender)


# ------------------------------------------------------------
# STEP 3: Spearman Rank Correlation over time
# ------------------------------------------------------------

# Get top 25 names per year per gender nationally
get_top25 <- function(df, gender) {
    df %>%
        filter(Gender == gender) %>%
        group_by(Year) %>%
        slice_max(Count, n = 25) %>%
        mutate(rank = rank(-Count, ties.method = "first")) %>%
        ungroup()
}

top25_boys  <- get_top25(Baby_National, "M")
top25_girls <- get_top25(Baby_National, "F")

# Function to compute Spearman correlation between
# a given year's top 25 and each of the next 3 years
spearman_persistence <- function(top25_df) {

    years <- unique(top25_df$Year) %>% sort()

    map_df(years, function(yr) {

        # The current year's top 25 names and ranks
        current <- top25_df %>%
            filter(Year == yr) %>%
            select(Name, rank)

        # Comparing to next 3 years
        map_df(1:3, function(lag) {

            future_yr <- yr + lag

            future <- top25_df %>%
                filter(Year == future_yr) %>%
                select(Name, rank_future = rank)

            # Joining on Name — only names that appear in both years
            joined <- inner_join(current, future, by = "Name")

            # I  need at least 5 common names to compute correlation
            if (nrow(joined) < 5) return(NULL)

            cor_val <- cor(joined$rank, joined$rank_future,
                           method = "spearman")

            tibble(Year = yr, lag = lag, spearman_cor = cor_val)
        })
    })
}

# Computing for both genders
spearman_boys  <- spearman_persistence(top25_boys)  %>% mutate(Gender = "Boys")
spearman_girls <- spearman_persistence(top25_girls) %>% mutate(Gender = "Girls")

spearman_all <- bind_rows(spearman_boys, spearman_girls)

# Quick check
glimpse(spearman_all)
head(spearman_all, 10)




# ------------------------------------------------------------
# STEP 4: Ploting Spearman Rank Correlation over time
# ------------------------------------------------------------

plot_spearman <- function(df) {
    df %>%
        mutate(lag = paste0("Lag ", lag, " year(s)")) %>%
        ggplot(aes(x = Year, y = spearman_cor, color = lag)) +
        geom_line(alpha = 0.8, linewidth = 0.7) +
        geom_smooth(se = FALSE, linetype = "dashed",
                    linewidth = 0.5) +
        facet_wrap(~Gender) +
        geom_vline(xintercept = 1990, linetype = "dotted",
                   color = "red", linewidth = 0.8) +
        annotate("text", x = 1991, y = 0.3,
                 label = "1990s", color = "red",
                 size = 3, hjust = 0) +
        scale_y_continuous(limits = c(0, 1)) +
        scale_x_continuous(breaks = seq(1910, 2014, by = 20)) +
        labs(
            title = "Persistence of Popular Baby Names Over Time",
            subtitle = "Spearman rank correlation between each year's top 25 names and the next 1-3 years",
            x = "Year",
            y = "Spearman Rank Correlation",
            color = "Forecast Horizon"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
}

p_spearman <- plot_spearman(spearman_all)
p_spearman



# ------------------------------------------------------------
# STEP 5: Identifying year-on-year name spikes
# ------------------------------------------------------------

find_name_spikes <- function(df) {
    df %>%
        arrange(Name, Gender, Year) %>%
        group_by(Name, Gender) %>%
        mutate(
            prev_count = lag(Count),
            pct_change = (Count - prev_count) / prev_count * 100
        ) %>%
        ungroup() %>%
        filter(!is.na(pct_change)) %>%
        # Only keeping meaningful spikes - name must have at least
        # 500 counts in spike year to avoid tiny name noise
        filter(Count >= 500) %>%
        arrange(desc(pct_change))
}

Name_Spikes <- find_name_spikes(Baby_National)

# The top 20 biggest spikes
Name_Spikes %>%
    select(Name, Gender, Year, prev_count, Count, pct_change) %>%
    head(20)



# ------------------------------------------------------------
# STEP 6: Cross referencing spikes with Billboard
# ------------------------------------------------------------

# Extracting the first name from artist column
extract_first_name <- function(artist) {
    artist %>%
        str_remove_all("\\(.*?\\)") %>%  # remove anything in brackets
        str_trim() %>%
        str_extract("^[A-Za-z]+")  # take first word only
}

Billboard_names <- Top_100_Billboard %>%
    mutate(
        year = lubridate::year(date),
        artist_firstname = extract_first_name(artist)
    ) %>%
    filter(!is.na(artist_firstname))

# Function to check if a name appears in Billboard
# charts around a spike year
check_billboard <- function(name, spike_year, window = 2) {
    Billboard_names %>%
        filter(
            str_to_lower(artist_firstname) == str_to_lower(name),
            year >= spike_year - window,
            year <= spike_year + window
        ) %>%
        select(year, artist, song, rank) %>%
        arrange(rank) %>%
        head(5)
}

# Testing with our known spikes
cat("=== AALIYAH 1994 ===\n")
print(check_billboard("Aaliyah", 1994))

cat("=== SHANIA 1995 ===\n")
print(check_billboard("Shania", 1995))

cat("=== JONI 1953 ===\n")
print(check_billboard("Joni", 1953))




# ------------------------------------------------------------
# STEP 7: Cross referencing spikes with HBO credits
# ------------------------------------------------------------

# Joining credits with titles to get release year
HBO_combined <- HBO_credits %>%
    inner_join(HBO_titles %>% select(id, title, release_year, type),
               by = "id")

# Function to check if a name appears as a character
# or actor in HBO around a spike year
check_hbo <- function(name, spike_year, window = 3) {
    HBO_combined %>%
        filter(
            release_year >= spike_year - window,
            release_year <= spike_year + window
        ) %>%
        filter(
            str_detect(str_to_lower(character), str_to_lower(name)) |
                str_detect(str_to_lower(name), str_to_lower(
                    str_extract(name, "^[A-Za-z]+")))
        ) %>%
        select(release_year, title, type, name, character, role) %>%
        arrange(release_year) %>%
        head(5)
}

# Better approach than the one i did - searching by character first name
check_character_name <- function(name, spike_year, window = 3) {
    HBO_combined %>%
        filter(
            release_year >= spike_year - window,
            release_year <= spike_year + window
        ) %>%
        filter(str_detect(str_to_lower(character),
                          paste0("\\b", str_to_lower(name), "\\b"))) %>%
        select(release_year, title, type, character, role) %>%
        arrange(release_year) %>%
        head(5)
}

# Testing with our known spikes
cat("=== KATINA 1972 (TV show spike) ===\n")
print(check_character_name("Katina", 1972))

cat("=== MALLORY 1983 ===\n")
print(check_character_name("Mallory", 1983))

cat("=== CATALEYA 2012 ===\n")
print(check_character_name("Cataleya", 2012))

cat("=== SHELBY 1936 ===\n")
print(check_character_name("Shelby", 1936))







# ------------------------------------------------------------
# STEP 8 : Two bubble plots
# Plot A - Top 5 names per decade (clean and readable)
# Plot B - Cultural spike names specifically
# ------------------------------------------------------------

create_decade_bubble <- function(df, gender, top_n = 5) {
    df %>%
        filter(Gender == gender) %>%
        mutate(Decade = floor(Year / 10) * 10) %>%
        group_by(Decade, Name) %>%
        summarise(Total_Count = sum(Count), .groups = "drop") %>%
        group_by(Decade) %>%
        slice_max(Total_Count, n = top_n) %>%
        ungroup()
}

bubble_boys  <- create_decade_bubble(Baby_National, "M")
bubble_girls <- create_decade_bubble(Baby_National, "F")

plot_bubble <- function(df, gender_label) {
    df %>%
        mutate(size_category = case_when(
            Total_Count >= 800000 ~ "800,000+",
            Total_Count >= 600000 ~ "600,000+",
            Total_Count >= 400000 ~ "400,000+",
            Total_Count >= 200000 ~ "200,000+",
            TRUE ~ "Below 200,000"
        )) %>%
        mutate(size_category = factor(size_category,
                                      levels = c("800,000+", "600,000+",
                                                 "400,000+", "200,000+",
                                                 "Below 200,000"))) %>%
        ggplot(aes(x = reorder(Name, Total_Count),
                   y = factor(Decade),
                   size = Total_Count,
                   color = size_category)) +
        geom_point(alpha = 0.7) +
        scale_size_continuous(range = c(3, 14),
                              labels = scales::comma) +
        scale_color_manual(values = c(
            "800,000+"      = "#7B2D00",
            "600,000+"      = "#C0392B",
            "400,000+"      = "#E67E22",
            "200,000+"      = "#2980B9",
            "Below 200,000" = "#BDC3C7"
        )) +
        labs(
            title = paste("Top 5", gender_label, "Baby Names by Decade"),
            subtitle = "Bubble size and colour = total babies named that decade",
            x = "Name", y = "Decade",
            size = "Total Count",
            color = "Count Range"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
}

p_bubble_boys  <- plot_bubble(bubble_boys, "Boys")
p_bubble_girls <- plot_bubble(bubble_girls, "Girls")

# ---- Plot B: Cultural spike names ----
cultural_spikes <- c("Aaliyah", "Shania", "Whitney",
                     "Mallory", "Katina", "Cataleya")

cultural_events <- tribble(
    ~Name,      ~event_year, ~event,
    "Aaliyah",  1994,        "Aaliyah debut album",
    "Shania",   1995,        "Shania Twain fame",
    "Whitney",  1985,        "Whitney Houston debut",
    "Mallory",  1983,        "Family Ties TV show",
    "Katina",   1972,        "Where the Heart Is TV",
    "Cataleya", 2012,        "Colombiana movie"
)

plot_cultural_spikes <- function(df, names, events) {
    df %>%
        filter(Name %in% names) %>%
        left_join(events, by = "Name") %>%
        ggplot(aes(x = Year, y = Count, color = Name)) +
        geom_line(linewidth = 0.8) +
        geom_vline(aes(xintercept = event_year, color = Name),
                   linetype = "dashed", alpha = 0.6) +
        geom_text(aes(x = event_year,
                      y = max(Count) * 0.9,
                      label = event),
                  size = 2.8, hjust = -0.05,
                  check_overlap = TRUE, color = "black") +
        facet_wrap(~Name, scales = "free_y") +
        labs(
            title = "Culturally Influenced Baby Name Spikes",
            subtitle = "Dashed line = cultural event that triggered the spike",
            x = "Year", y = "Count",
            color = "Name"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
}

p_cultural <- plot_cultural_spikes(Baby_National,
                                   cultural_spikes,
                                   cultural_events)

p_bubble_boys
p_bubble_girls
p_cultural





