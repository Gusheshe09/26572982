# ============================================================
# Question 3: Loans and Credit
# Author: 26572982
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# STEP 1: Loading the data
# ------------------------------------------------------------

Loan_credit <- readRDS("data/Loan_Cred/loan_data.rds")

# ------------------------------------------------------------
# STEP 2: Cleaning and prepare data
# ------------------------------------------------------------

clean_loans <- function(df) {
    df %>%
        # Creating default flag as per exam tip
        mutate(default_flag = case_when(
            loan_status == "Fully Paid"  ~ "Fully Paid",
            loan_status == "Current"     ~ "Current",
            loan_status %in% c("Charged Off", "Default") ~ "Defaulted",
            loan_status %in% c("Late (31-120 days)",
                               "Late (16-30 days)",
                               "In Grace Period") ~ "At Risk",
            TRUE ~ "Other"
        )) %>%
        # Clean DTI — removing extreme outliers (999 is clearly erroneous)
        mutate(dti = ifelse(dti > 100 | dti < 0, NA, dti)) %>%
        # Cleaning employment length — convert to ordered factor
        mutate(emp_length = factor(emp_length,
                                   levels = c("< 1 year", "1 year", "2 years",
                                              "3 years", "4 years", "5 years",
                                              "6 years", "7 years", "8 years",
                                              "9 years", "10+ years", "n/a"),
                                   ordered = TRUE)) %>%
        # Clean grade as ordered factor
        mutate(grade = factor(grade,
                              levels = c("A","B","C","D","E","F","G"),
                              ordered = TRUE)) %>%
        # Cleaning term — remove " months" text
        mutate(term = str_trim(term)) %>%
        # Keeping only the relevant columns for analysis
        select(loan_amnt, funded_amnt, term, int_rate, grade,
               emp_length, home_ownership, annual_inc,
               loan_status, default_flag, addr_state, dti,
               purpose, revol_util, delinq_2yrs, pub_rec,
               open_acc, total_acc, inq_last_6mths)
}

Loan_clean <- clean_loans(Loan_credit)

# Quick check
glimpse(Loan_clean)
cat('\nDefault flag distribution:\n')
print(table(Loan_clean$default_flag))






# ------------------------------------------------------------
# STEP 3: Create analysis subset
# Defaulted vs Fully Paid only for clean comparison
# ------------------------------------------------------------

# Default rate overall
default_rate_overall <- Loan_clean %>%
    filter(default_flag %in% c("Defaulted", "Fully Paid")) %>%
    summarise(
        total = n(),
        defaulted = sum(default_flag == "Defaulted"),
        default_rate = round(defaulted / total * 100, 2)
    )

cat("Overall default rate:\n")
print(default_rate_overall)

# Analysis subset
Loan_analysis <- Loan_clean %>%
    filter(default_flag %in% c("Defaulted", "Fully Paid")) %>%
    mutate(is_default = ifelse(default_flag == "Defaulted", 1, 0))

cat("\nAnalysis subset size:", nrow(Loan_analysis), "\n")



# ------------------------------------------------------------
# STEP 4: Plot 1 - Default rate by Credit Grade
# ------------------------------------------------------------

plot_default_by_grade <- function(df) {
    df %>%
        group_by(grade) %>%
        summarise(
            total = n(),
            default_rate = mean(is_default) * 100,
            .groups = "drop"
        ) %>%
        ggplot(aes(x = grade, y = default_rate, fill = grade)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(round(default_rate, 1), "%")),
                  vjust = -0.5, size = 4) +
        geom_hline(yintercept = 22.5, linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        annotate("text", x = 1, y = 24,
                 label = "Overall avg: 22.5%",
                 color = "red", size = 3.5, hjust = 0) +
        scale_fill_manual(values = c(
            "A" = "#27AE60", "B" = "#2ECC71",
            "C" = "#F39C12", "D" = "#E67E22",
            "E" = "#E74C3C", "F" = "#C0392B",
            "G" = "#7B241C"
        )) +
        coord_cartesian(ylim = c(0, 60)) +
        labs(
            title = "Default Rate by Credit Grade",
            subtitle = "Red dashed line = overall average default rate (22.5%)",
            x = "Credit Grade", y = "Default Rate (%)"
        ) +
        theme_minimal()
}

p1_grade <- plot_default_by_grade(Loan_analysis)
p1_grade



# ------------------------------------------------------------
# STEP 5: Plot 2 - Default rate by Home Ownership
# ------------------------------------------------------------

plot_default_by_ownership <- function(df) {
    df %>%
        filter(home_ownership %in% c("RENT", "MORTGAGE", "OWN")) %>%
        group_by(home_ownership) %>%
        summarise(
            total = n(),
            default_rate = mean(is_default) * 100,
            .groups = "drop"
        ) %>%
        ggplot(aes(x = reorder(home_ownership, default_rate),
                   y = default_rate,
                   fill = home_ownership)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(round(default_rate, 1), "%",
                                     "\n(n=", scales::comma(total), ")")),
                  vjust = -0.3, size = 4) +
        geom_hline(yintercept = 22.5, linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        annotate("text", x = 0.6, y = 24,
                 label = "Overall avg: 22.5%",
                 color = "red", size = 3.5, hjust = 0) +
        scale_fill_manual(values = c(
            "OWN"      = "#27AE60",
            "MORTGAGE" = "#F39C12",
            "RENT"     = "#E74C3C"
        )) +
        coord_cartesian(ylim = c(0, 30)) +
        labs(
            title = "Default Rate by Home Ownership",
            subtitle = "Testing the Institute's heuristic: do home owners default less?",
            x = "Home Ownership", y = "Default Rate (%)"
        ) +
        theme_minimal()
}

# ------------------------------------------------------------
# STEP 6: Plot 3 - Default rate by Employment Length
# ------------------------------------------------------------

plot_default_by_employment <- function(df) {
    df %>%
        filter(!is.na(emp_length), emp_length != "n/a") %>%
        group_by(emp_length) %>%
        summarise(
            total = n(),
            default_rate = mean(is_default) * 100,
            .groups = "drop"
        ) %>%
        ggplot(aes(x = emp_length,
                   y = default_rate,
                   fill = default_rate)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(round(default_rate, 1), "%")),
                  vjust = -0.5, size = 3.5) +
        geom_hline(yintercept = 22.5, linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        annotate("text", x = 1, y = 24,
                 label = "Overall avg: 22.5%",
                 color = "red", size = 3.5, hjust = 0) +
        scale_fill_gradient(low = "#27AE60", high = "#E74C3C") +
        coord_cartesian(ylim = c(0, 30)) +
        labs(
            title = "Default Rate by Employment Length",
            subtitle = "Testing the Institute's heuristic: do longer-employed borrowers default less?",
            x = "Employment Length", y = "Default Rate (%)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

p2_ownership   <- plot_default_by_ownership(Loan_analysis)
p3_employment  <- plot_default_by_employment(Loan_analysis)

p2_ownership
p3_employment




# ------------------------------------------------------------
# STEP 7: Plot 4 - DTI Analysis
# ------------------------------------------------------------

plot_default_by_dti <- function(df) {
    df %>%
        filter(!is.na(dti)) %>%
        mutate(dti_band = case_when(
            dti < 5        ~ "0-5",
            dti < 10       ~ "5-10",
            dti < 15       ~ "10-15",
            dti < 20       ~ "15-20",
            dti < 25       ~ "20-25",
            dti < 30       ~ "25-30",
            dti < 35       ~ "30-35",
            dti >= 35      ~ "35+"
        )) %>%
        mutate(dti_band = factor(dti_band,
                                 levels = c("0-5","5-10","10-15",
                                            "15-20","20-25","25-30",
                                            "30-35","35+"))) %>%
        group_by(dti_band) %>%
        summarise(
            total = n(),
            default_rate = mean(is_default) * 100,
            .groups = "drop"
        ) %>%
        ggplot(aes(x = dti_band, y = default_rate, fill = default_rate)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(round(default_rate, 1), "%",
                                     "\n(n=", scales::comma(total), ")")),
                  vjust = -0.3, size = 3.5) +
        geom_hline(yintercept = 22.5, linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        geom_vline(xintercept = 4.5, linetype = "solid",
                   color = "darkred", linewidth = 1.2) +
        annotate("text", x = 4.7, y = 35,
                 label = "Recommended\nDTI cap: 20",
                 color = "darkred", size = 3.5, hjust = 0) +
        annotate("text", x = 1, y = 24,
                 label = "Overall avg: 22.5%",
                 color = "red", size = 3.5, hjust = 0) +
        scale_fill_gradient(low = "#27AE60", high = "#E74C3C") +
        coord_cartesian(ylim = c(0, 40)) +
        labs(
            title = "Default Rate by Debt-to-Income (DTI) Band",
            subtitle = "What is an appropriate hard cap for DTI levels?",
            x = "DTI Band", y = "Default Rate (%)"
        ) +
        theme_minimal()
}

p4_dti <- plot_default_by_dti(Loan_analysis)
p4_dti




# ------------------------------------------------------------
# STEP 8: Plot 5 - Default rate by State
# ------------------------------------------------------------

plot_default_by_state <- function(df) {

    # Calculating default rate per state
    state_defaults <- df %>%
        group_by(addr_state) %>%
        summarise(
            total = n(),
            default_rate = mean(is_default) * 100,
            .groups = "drop"
        ) %>%
        # Only keeping states with at least 500 loans for reliability
        filter(total >= 500) %>%
        arrange(desc(default_rate)) %>%
        mutate(is_texas = ifelse(addr_state == "TX", "Texas", "Other State"))

    state_defaults %>%
        ggplot(aes(x = reorder(addr_state, default_rate),
                   y = default_rate,
                   fill = is_texas)) +
        geom_col(show.legend = FALSE) +
        geom_hline(yintercept = 22.5, linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        geom_hline(yintercept = mean(state_defaults$default_rate),
                   linetype = "dotted",
                   color = "blue", linewidth = 0.8) +
        scale_fill_manual(values = c(
            "Texas"       = "#E74C3C",
            "Other State" = "steelblue"
        )) +
        annotate("text", x = 2, y = 23.5,
                 label = "Overall avg: 22.5%",
                 color = "red", size = 3, hjust = 0) +
        labs(
            title = "Default Rate by US State",
            subtitle = "Red bar = Texas | Red dashed = overall average | Blue dotted = state average",
            x = "State", y = "Default Rate (%)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90,
                                         hjust = 1,
                                         size = 7))
}

p5_state <- plot_default_by_state(Loan_analysis)
p5_state


# ------------------------------------------------------------
# STEP 9: Plot 6 - Interest Rate by Grade and Default Status
# ------------------------------------------------------------

plot_intrate_by_grade <- function(df) {
    df %>%
        ggplot(aes(x = grade, y = int_rate, fill = default_flag)) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(values = c(
            "Defaulted"  = "#E74C3C",
            "Fully Paid" = "#27AE60"
        )) +
        labs(
            title = "Interest Rate Distribution by Credit Grade and Default Status",
            subtitle = "Are interest rates clearly determined by credit grade?",
            x = "Credit Grade", y = "Interest Rate (%)",
            fill = "Loan Outcome"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
}



# ------------------------------------------------------------
# STEP 10: Plot 7 - Default rate by Loan Purpose
# ------------------------------------------------------------

plot_default_by_purpose <- function(df) {
    df %>%
        group_by(purpose) %>%
        summarise(
            total = n(),
            default_rate = mean(is_default) * 100,
            .groups = "drop"
        ) %>%
        filter(total >= 1000) %>%
        arrange(desc(default_rate)) %>%
        ggplot(aes(x = default_rate,
                   y = reorder(purpose, default_rate),
                   fill = default_rate)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = paste0(round(default_rate, 1), "%",
                                     " (n=", scales::comma(total), ")")),
                  hjust = -0.1, size = 3.5) +
        geom_vline(xintercept = 22.5, linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        scale_fill_gradient(low = "#27AE60", high = "#E74C3C") +
        coord_cartesian(xlim = c(0, 35)) +
        labs(
            title = "Default Rate by Loan Purpose",
            subtitle = "What are borrowers using the loans for?",
            x = "Default Rate (%)", y = "Loan Purpose"
        ) +
        theme_minimal()
}

p6_intrate  <- plot_intrate_by_grade(Loan_analysis)
p7_purpose  <- plot_default_by_purpose(Loan_analysis)

p6_intrate
p7_purpose
