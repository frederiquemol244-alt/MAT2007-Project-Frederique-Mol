# Ramen ratings analysis (MAT2007 project)
#
# Research question: which country produces the best-rated instant ramen,
# and is the top country's lead statistically meaningful?
#
# How the script works:
#   1. Test the helper functions on small hand-made examples
#   2. Load ramen-ratings.csv (stop with a clear error if it can't be opened)
#   3. Clean the data: convert Stars to numbers, drop unrated rows,
#      and standardize country names
#   4. Compute the number of reviews, mean rating, standard deviation and
#      standard error for each country
#   5. Keep only countries with enough reviews and rank them by mean rating
#   6. Compare countries: difference of the means +/- combined standard error
#   7. Cross-check the uncertainty of the overall mean with subsampling
#   8. Plot the top countries with error bars
#   9. Plot mean rating against number of reviews for all countries
#
# The file has three parts: settings, helper functions, and the main
# analysis that calls the helper functions in order.
#
# Note: the CSV must be in the working directory (check with getwd()).


# Settings ---------------------------------------------------------------

# All the numbers that can be changed are collected here, so nothing is
# hidden further down in the code.
data_file       <- "ramen-ratings.csv"
min_reviews     <- 10   # a country needs at least this many reviews
n_top           <- 10   # number of countries shown in the bar chart
sigma_threshold <- 2    # a difference above 2 sigma counts as significant
n_subsamples    <- 10   # number of subsamples for the cross-check in step 7


# Helper functions: loading and cleaning ---------------------------------

# Loads the CSV and checks that it is usable. stop() halts the script with a
# message that tells the user where R is looking for the file.
# stringsAsFactors = FALSE keeps text columns as plain text, not factors.
load_ramen_data <- function(filename) {
  if (!file.exists(filename)) {
    stop("ERROR: could not find ", filename, ". ",
         "Check the file name and working directory: ", getwd())
  }

  data <- read.csv(filename, stringsAsFactors = FALSE)

  # The analysis needs these two columns and at least one row
  if (nrow(data) == 0 || !all(c("Country", "Stars") %in% names(data))) {
    stop("ERROR: ", filename, " is empty or has no Country/Stars columns.")
  }
  return(data)
}

# Stars is read in as text because some rows contain non-numeric values
# (e.g. "Unrated"). as.numeric() turns those into NA, and we then drop every
# row where Stars is NA so that only rated products remain.
# as.numeric() would warn about "NAs introduced by coercion". That is exactly
# what we want here, so suppressWarnings() hides the warning.
clean_stars <- function(data) {
  data$Stars <- suppressWarnings(as.numeric(data$Stars))
  data_clean <- data[!is.na(data$Stars), ]
  return(data_clean)
}

# The same country can appear under different names, which would split its
# reviews into separate groups. Each name in old_names is replaced by the
# name at the same position in new_names (e.g. "Holland" -> "Netherlands").
# Sarawak is a Malaysian state, so it is merged into Malaysia.
standardize_country_names <- function(data) {
  old_names <- c("Holland", "Dubai", "United States", "Sarawak")
  new_names <- c("Netherlands", "United Arab Emirates", "USA", "Malaysia")

  # Loop over each pair and overwrite matching Country values
  for (i in seq_along(old_names)) {
    data$Country[data$Country == old_names[i]] <- new_names[i]
  }
  return(data)
}


# Helper functions: statistics -------------------------------------------

# Standard error of the mean: the uncertainty on an average of n values.
# (NA for a single value, since the standard deviation needs 2+ values.)
standard_error <- function(values) {
  return(sd(values) / sqrt(length(values)))
}

# Builds a summary table with one row per country. Instead of using a
# built-in grouping function, it loops through each unique country, picks out
# that country's rows, and records the number of reviews, mean, SD and SE.
compute_country_stats <- function(data) {
  countries <- unique(data$Country)   # each country appears once in this list
  n_countries <- length(countries)

  # Pre-allocate empty vectors of the right length. Filling them in inside
  # the loop is faster than growing a vector one element at a time.
  result_n    <- numeric(n_countries)
  result_mean <- numeric(n_countries)
  result_sd   <- numeric(n_countries)
  result_se   <- numeric(n_countries)

  for (i in seq_along(countries)) {
    # All ratings that belong to this country
    stars <- data$Stars[data$Country == countries[i]]

    result_n[i]    <- length(stars)          # number of reviews
    result_mean[i] <- mean(stars)            # average rating
    result_sd[i]   <- sd(stars)              # spread of ratings
    result_se[i]   <- standard_error(stars)  # uncertainty on the average
  }

  # Combine the result vectors into a single data frame
  stats <- data.frame(
    Country = countries,
    n       = result_n,
    mean    = result_mean,
    sd      = result_sd,
    se      = result_se
  )
  return(stats)
}

# An average based on only a few reviews is unreliable, so keep only the
# countries whose review count (n) is at least min_n.
filter_by_min_reviews <- function(stats, min_n) {
  filtered <- stats[stats$n >= min_n, ]
  return(filtered)
}

# Sorts the countries from the highest to the lowest mean rating
rank_by_average <- function(stats) {
  ranked <- stats[order(-stats$mean), ]
  row.names(ranked) <- NULL   # reset row numbers so they equal the rank
  return(ranked)
}

# Compares two countries. The means are treated as independent, so the
# uncertainty of the difference is the two standard errors added in
# quadrature. n_sigma says how many of those uncertainties the gap is.
compare_countries <- function(stats, country_a, country_b) {
  a <- stats[stats$Country == country_a, ]
  b <- stats[stats$Country == country_b, ]

  diff_mean <- a$mean - b$mean
  diff_se   <- sqrt(a$se^2 + b$se^2)

  result <- list(
    country_a = country_a,
    country_b = country_b,
    diff_mean = diff_mean,
    diff_se   = diff_se,
    n_sigma   = diff_mean / diff_se
  )
  return(result)
}

# Subsampling method: split the values into n_sub random groups, take the
# mean of each group, and use the spread of those means as the uncertainty.
# The values are shuffled first because the file is ordered by review
# number, so consecutive blocks would be different time periods.
subsample_mean <- function(values, n_sub) {
  shuffled <- sample(values)
  group <- rep(1:n_sub, length.out = length(shuffled))

  sub_means <- numeric(n_sub)
  for (i in 1:n_sub) {
    sub_means[i] <- mean(shuffled[group == i])
  }

  result <- list(mean = mean(sub_means), se = standard_error(sub_means))
  return(result)
}


# Helper functions: printing ---------------------------------------------

# Writes a value and its uncertainty as text, e.g. "4.16 +/- 0.06"
format_value <- function(value, uncertainty) {
  return(sprintf("%.2f +/- %.2f", value, uncertainty))
}

# Prints a header above each part of the output, so it is clear which step
# of the analysis the numbers below it belong to.
print_header <- function(title) {
  line <- strrep("=", 72)
  cat("\n", line, "\n", title, "\n", line, "\n", sep = "")
}

# Prints a list of labels with their values in two aligned columns.
print_summary <- function(labels, values) {
  cat(paste0("  ", format(labels), "  ", values), sep = "\n")
}

# Prints a data frame as a table with a header row and a line under it.
# Text columns are left-aligned and number columns right-aligned. Decimal
# numbers are always shown with 2 decimals, so the columns line up.
print_table <- function(table) {
  lines <- rep("", nrow(table) + 1)   # the header row plus one line per row

  for (name in names(table)) {
    values <- table[[name]]
    side <- ifelse(is.character(values), "left", "right")
    if (is.double(values)) {
      values <- sprintf("%.2f", values)
    }
    # format() pads the header and the values to the same width
    cells <- format(c(name, as.character(values)), justify = side)
    lines <- paste0(lines, "  ", cells)
  }

  # Remove the padding after the last column, then draw the line as wide
  # as the widest row
  lines <- trimws(lines, which = "right")
  rule <- paste0("  ", strrep("-", max(nchar(lines)) - 2))
  cat(lines[1], rule, lines[-1], sep = "\n")
}

# Returns the first rows of the raw data in a form that fits on the screen:
# only the columns that matter, with long product names cut off.
make_preview_table <- function(data, n_rows, max_chars = 28) {
  preview <- head(data, n_rows)

  long <- nchar(preview$Variety) > max_chars
  preview$Variety[long] <- paste0(substr(preview$Variety[long], 1,
                                         max_chars - 3), "...")

  preview <- data.frame(
    Review  = preview$Review..,
    Brand   = preview$Brand,
    Variety = preview$Variety,
    Style   = preview$Style,
    Country = preview$Country,
    Stars   = preview$Stars
  )
  return(preview)
}

# Returns a copy of the table that is easier to read: a rank column in
# front, clear column names and the decimals rounded. The rounding is only
# for display; the calculations always use the unrounded numbers.
make_display_table <- function(ranked) {
  display <- data.frame(
    Rank    = seq_len(nrow(ranked)),
    Country = ranked$Country,
    Reviews = as.integer(ranked$n),
    Mean    = round(ranked$mean, 2),
    SD      = round(ranked$sd, 2),
    SE      = round(ranked$se, 2)
  )
  return(display)
}

# Turns a list of comparisons into a table with one row per comparison and
# states the conclusion of each one explicitly.
make_comparison_table <- function(comparisons, threshold) {
  n_comparisons <- length(comparisons)

  pair       <- character(n_comparisons)
  difference <- character(n_comparisons)
  sigma      <- numeric(n_comparisons)
  conclusion <- character(n_comparisons)

  for (i in seq_along(comparisons)) {
    comparison <- comparisons[[i]]

    pair[i]       <- paste(comparison$country_a, "-", comparison$country_b)
    difference[i] <- format_value(comparison$diff_mean, comparison$diff_se)
    sigma[i]      <- comparison$n_sigma

    if (comparison$n_sigma > threshold) {
      conclusion[i] <- "meaningfully higher-rated"
    } else {
      conclusion[i] <- "statistically close (could be chance)"
    }
  }

  table <- data.frame(
    Comparison = pair,
    Difference = difference,
    Sigma      = sigma,
    Conclusion = conclusion
  )
  return(table)
}


# Helper functions: plotting ---------------------------------------------

# Draws vertical error bars of +/- 1 standard error. arrows() with a 90
# degree head on both ends (code = 3) gives the usual error-bar shape.
draw_error_bars <- function(x, y, se, cap_length, color = "black") {
  arrows(
    x0 = x, y0 = y - se,
    x1 = x, y1 = y + se,
    angle = 90, code = 3, length = cap_length, col = color
  )
}

# Bar chart of the n_countries best-rated countries with error bars
plot_top_countries <- function(ranked, n_countries, min_n, filename) {
  top_n <- head(ranked, n_countries)

  png(filename, width = 1400, height = 900, res = 200)
  # Extra bottom margin so the rotated country names are not cut off
  par(mar = c(7, 4, 3, 1))

  bar_positions <- barplot(
    top_n$mean,
    names.arg = top_n$Country,
    las = 2,
    ylim = c(0, 5.5),
    col = "steelblue",
    ylab = "Average Stars",
    main = paste("Average Ramen Rating by Country (min", min_n, "reviews)")
  )

  draw_error_bars(bar_positions, top_n$mean, top_n$se, cap_length = 0.05)

  # invisible() stops dev.off() from printing "null device" in the console
  invisible(dev.off())
}

# Scatter plot of mean rating against number of reviews for all countries.
# Shows why the minimum-review cutoff is needed: countries with few reviews
# have large error bars and extreme averages, while countries with many
# reviews settle close to the overall mean. Countries below the cutoff are
# drawn as open grey circles, the ones we keep as filled blue circles.
plot_rating_vs_reviews <- function(stats, overall_mean, min_n, filename) {
  kept <- stats$n >= min_n
  point_color <- ifelse(kept, "steelblue", "grey55")

  png(filename, width = 1400, height = 900, res = 200)
  par(mar = c(4.5, 4.5, 3, 1))

  plot(
    stats$n, stats$mean,
    log = "x",
    xlim = c(0.8, 500), ylim = c(1, 5),
    pch = ifelse(kept, 19, 1),
    col = point_color,
    xlab = "Number of reviews (log scale)",
    ylab = "Average Stars",
    main = "Average Rating vs Number of Reviews per Country"
  )

  # Error bars are skipped where the standard error is NA (1 review)
  has_se <- !is.na(stats$se) & stats$se > 0
  draw_error_bars(stats$n[has_se], stats$mean[has_se], stats$se[has_se],
                  cap_length = 0.02, color = point_color[has_se])

  # Reference lines: overall mean rating and the review cutoff
  abline(h = overall_mean, lty = 2, col = "grey30")
  abline(v = min_n, lty = 3, col = "grey30")
  text(0.8, overall_mean, paste("overall mean =", round(overall_mean, 2)),
       adj = c(0, -0.5), cex = 0.7, col = "grey30")
  text(min_n, 1, paste("cutoff: n =", min_n), pos = 4, cex = 0.7,
       col = "grey30")

  # Label only the countries discussed in the report. Each label gets its
  # own side (1 = below, 2 = left, 3 = above, 4 = right) so that the
  # close-together top countries do not overlap.
  to_label  <- c("Malaysia", "Singapore", "Indonesia", "Japan", "USA",
                 "Canada", "Nigeria", "Brazil")
  label_pos <- c(4, 2, 3, 4, 4, 4, 4, 4)
  for (i in seq_along(to_label)) {
    row <- stats[stats$Country == to_label[i], ]
    if (row$Country == "Indonesia") {
      # Too close to Singapore: lift the label and draw a short leader line
      segments(row$n, row$mean + 0.05, row$n, row$mean + 0.3, col = "grey30")
      text(row$n, row$mean + 0.3, row$Country, pos = 3, cex = 0.7,
           offset = 0.2)
    } else {
      text(row$n, row$mean, row$Country, pos = label_pos[i],
           cex = 0.7, offset = 0.5)
    }
  }

  legend("bottomright",
         legend = c(paste0("kept (n >= ", min_n, ")"),
                    paste0("excluded (n < ", min_n, ")")),
         pch = c(19, 1), col = c("steelblue", "grey55"),
         bty = "n", cex = 0.8)

  invisible(dev.off())
}


# Helper functions: tests ------------------------------------------------

# Black box tests: each helper function gets a small hand-made input for
# which we know the right answer. stopifnot() halts the script if any of
# them is wrong, so a broken function can never produce a result silently.
run_tests <- function() {
  test_data <- data.frame(
    Country = c("Holland", "Netherlands", "Japan", "Japan", "Sarawak"),
    Stars   = c("2", "4", "5", "3", "Unrated"),
    stringsAsFactors = FALSE
  )

  # Cleaning: the "Unrated" row is dropped and Stars becomes numeric
  cleaned <- clean_stars(test_data)
  stopifnot(nrow(cleaned) == 4)
  stopifnot(is.numeric(cleaned$Stars))

  # Country names: Holland is merged into Netherlands
  cleaned <- standardize_country_names(cleaned)
  stopifnot(!("Holland" %in% cleaned$Country))
  stopifnot(sum(cleaned$Country == "Netherlands") == 2)

  # Standard error: sd of (1, 3) is sqrt(2), so the SE is sqrt(2)/sqrt(2) = 1
  stopifnot(isTRUE(all.equal(standard_error(c(1, 3)), 1)))

  # Country statistics: Netherlands has ratings 2 and 4, Japan 5 and 3
  stats <- compute_country_stats(cleaned)
  stopifnot(nrow(stats) == 2)
  stopifnot(stats$n[stats$Country == "Netherlands"] == 2)
  stopifnot(stats$mean[stats$Country == "Netherlands"] == 3)
  stopifnot(stats$mean[stats$Country == "Japan"] == 4)

  # Cutoff: both countries have 2 reviews, so a cutoff of 3 removes them
  stopifnot(nrow(filter_by_min_reviews(stats, 2)) == 2)
  stopifnot(nrow(filter_by_min_reviews(stats, 3)) == 0)

  # Ranking: Japan (mean 4) comes before Netherlands (mean 3)
  stopifnot(rank_by_average(stats)$Country[1] == "Japan")

  # Comparison: difference 4 - 3 = 1, both SEs are 1, so sqrt(1 + 1)
  comparison <- compare_countries(stats, "Japan", "Netherlands")
  stopifnot(comparison$diff_mean == 1)
  stopifnot(isTRUE(all.equal(comparison$diff_se, sqrt(2))))

  # Subsampling: identical values have a mean of 3 with no uncertainty
  sub <- subsample_mean(rep(3, 20), 4)
  stopifnot(sub$mean == 3, sub$se == 0)

  cat("All helper-function tests passed.\n")
}


# Main analysis ----------------------------------------------------------

# Step 1: Test the helper functions --------------------------------------

# The tests run before the real data is touched
print_header("Step 1: Tests of the helper functions")
run_tests()

# Step 2: Load and inspect -----------------------------------------------

print_header("Step 2: Load the data")
ramen_raw <- load_ramen_data(data_file)
cat("SUCCESS:", data_file, "is open.\n")

# Quick look at the first 6 rows and the size of the dataset
cat("\nFirst 6 rows:\n")
print_table(make_preview_table(ramen_raw, 6))

cat("\nSize of the dataset:\n")
print_summary(
  c("Rows loaded", "Country labels (raw)", "Brands"),
  c(nrow(ramen_raw), length(unique(ramen_raw$Country)),
    length(unique(ramen_raw$Brand)))
)

# Step 3: Clean the data -------------------------------------------------

print_header("Step 3: Clean the data")
ramen_clean <- clean_stars(ramen_raw)
ramen_clean <- standardize_country_names(ramen_clean)

# Compare row counts to see how many unrated rows were removed. The last
# line is a sanity check: it should show FALSE if the replacement worked.
print_summary(
  c("Rows before cleaning", "Rows after cleaning", "Unrated rows removed",
    "Any 'Sarawak' left?"),
  c(nrow(ramen_raw), nrow(ramen_clean), nrow(ramen_raw) - nrow(ramen_clean),
    as.character("Sarawak" %in% ramen_clean$Country))
)

# Step 4: Statistics per country and overall -----------------------------

print_header("Step 4: Statistics for all products together")
country_stats <- compute_country_stats(ramen_clean)

overall_mean <- mean(ramen_clean$Stars)
overall_se   <- standard_error(ramen_clean$Stars)
print_summary(
  c("Rated products", "Countries", "Overall mean rating"),
  c(nrow(ramen_clean), nrow(country_stats),
    format_value(overall_mean, overall_se))
)

# Step 5: Filter and rank the countries ----------------------------------

print_header("Step 5: Countries ranked by mean rating")
country_filtered <- filter_by_min_reviews(country_stats, min_reviews)
country_ranked   <- rank_by_average(country_filtered)

# Show how many countries (and reviews) the cutoff keeps
print_summary(
  c("Countries before cutoff",
    paste0("Countries after cutoff (min ", min_reviews, " reviews)"),
    "Reviews covered by those countries"),
  c(nrow(country_stats), nrow(country_ranked), sum(country_ranked$n))
)

# The ranked table is printed and also saved, so the numbers quoted in the
# report can be looked up without running the script again
ranking_table <- make_display_table(country_ranked)
cat("\n")
print_table(ranking_table)
write.csv(ranking_table, "ramen_country_ranking.csv", row.names = FALSE)
cat("\nTable saved as ramen_country_ranking.csv\n")

# The cutoff matters: without it, countries with a handful of reviews would
# take the first and the last place
all_ranked <- rank_by_average(country_stats)
without_cutoff <- make_display_table(all_ranked)[c(1, nrow(all_ranked)), ]
cat("\nFirst and last place without the cutoff:\n")
print_table(without_cutoff)

# Step 6: Compare countries ----------------------------------------------

print_header("Step 6: Is the top country's lead meaningful?")
top1   <- country_ranked$Country[1]
top2   <- country_ranked$Country[2]
bottom <- country_ranked$Country[nrow(country_ranked)]

# The top country against the runner-up, against Japan (the country people
# often assume is best), against the lowest-ranked country and against
# Myanmar (the widest error bar in the bar chart)
comparisons <- list(
  compare_countries(country_ranked, top1, top2),
  compare_countries(country_ranked, top1, "Japan"),
  compare_countries(country_ranked, top1, bottom),
  compare_countries(country_ranked, top1, "Myanmar")
)
print_table(make_comparison_table(comparisons, sigma_threshold))
cat("\nDifference = difference of the mean ratings in stars. A gap above",
    sigma_threshold, "sigma\ncounts as meaningful.\n")

# Why Myanmar's error bar is so wide: few reviews, widely spread, one brand
myanmar <- ramen_clean[ramen_clean$Country == "Myanmar", ]
cat("\nMyanmar:", nrow(myanmar), "reviews from",
    length(unique(tolower(myanmar$Brand))), "brand, ratings from",
    min(myanmar$Stars), "to", max(myanmar$Stars), "stars.\n")

# Step 7: Cross-check with subsampling -----------------------------------

print_header("Step 7: Cross-check of the overall mean with subsampling")
# set.seed() makes the random shuffle the same on every run
set.seed(2007)
subsample_result <- subsample_mean(ramen_clean$Stars, n_subsamples)

# 3 decimals here, because the two standard errors are equal at 2 decimals
crosscheck_table <- data.frame(
  Method = c("sd / sqrt(n)", paste(n_subsamples, "subsamples")),
  Mean   = sprintf("%.3f", c(overall_mean, subsample_result$mean)),
  SE     = sprintf("%.3f", c(overall_se, subsample_result$se))
)
print_table(crosscheck_table)

# Step 8: Bar chart of the top countries ---------------------------------

print_header("Steps 8 and 9: Plots")
plot_top_countries(country_ranked, n_top, min_reviews,
                   "ramen_top_countries.png")
cat("Plot saved as ramen_top_countries.png\n")

# Step 9: Scatter plot of mean rating vs number of reviews ---------------

plot_rating_vs_reviews(country_stats, overall_mean, min_reviews,
                       "ramen_rating_vs_reviews.png")
cat("Plot saved as ramen_rating_vs_reviews.png\n")
