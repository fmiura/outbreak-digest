#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# R script: monitor_who_substack.R
# Purpose:
#   - Fetch WHO DON and Outbreak News Today RSS feeds
#   - Compare with previously seen items (stored in data/last_seen.json)
#   - Summarize new items via ChatGPT API
#   - Save a daily markdown report in /reports/ instead of sending email
# -------------------------------------------------------------------

# Required packages --------------------------------------------------
required <- c("tidyRSS", "jsonlite", "httr2", "glue", 
              "dplyr", "stringr", "lubridate")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(tidyRSS)
library(jsonlite)
library(httr2)
library(glue)
library(dplyr)
library(stringr)
library(lubridate)

# Environment variables (Secrets) -----------------------------------
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
OPENAI_MODEL   <- Sys.getenv("OPENAI_MODEL", "gpt-4o-mini")

if (OPENAI_API_KEY == "") {
  stop("Missing OPENAI_API_KEY environment variable")
}

# Feeds to monitor --------------------------------------------------
feeds <- tibble::tibble(
  source = c("WHO DON", "Outbreak News Today"),
  url    = c(
    "https://www.who.int/feeds/entity/csr/don/en/rss.xml",
    "https://outbreaknewstoday.substack.com/feed"
  )
)

# Persistent state file ----------------------------------------------
state_path <- "data/last_seen.json"
if (!dir.exists("data")) dir.create("data", recursive = TRUE)
state <- if (file.exists(state_path)) jsonlite::fromJSON(state_path) else list(seen = list())

# Helpers -------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
clean_text <- function(x) x %>% str_replace_all("\\s+", " ") %>% str_trim()

fetch_feed <- function(u) {
  suppressWarnings(tryCatch(tidyRSS::tidyfeed(u), error = function(e) NULL))
}

summarize_with_chatgpt <- function(title, link, description, source) {
  body <- paste0(
    "Source: ", source, "\n",
    "Title: ", title, "\n",
    "Link: ", link, "\n\n",
    "Body:\n", description
  )
  req <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers(Authorization = paste("Bearer", OPENAI_API_KEY)) |>
    req_body_json(list(
      model = OPENAI_MODEL,
      temperature = 0.2,
      messages = list(
        list(role = "system",
             content = paste(
               "Summarize concisely (<=120 words).",
               "Include: what/where/when, key numbers, short risk/implications.",
               "End with: 'Note for NL/JP:' and one line."
             )),
        list(role = "user", content = body)
      )
    ))
  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  out  <- if (is.null(resp)) NULL else tryCatch(resp_body_json(resp), error = function(e) NULL)
  msg  <- if (is.null(out)) NULL else out$choices[[1]]$message$content
  msg %||% "(Summary unavailable.)"
}

# Collect new items --------------------------------------------------
now_ams <- with_tz(now(tzone = "UTC"), tzone = "Europe/Amsterdam")
new_items <- list()

for (i in seq_len(nrow(feeds))) {
  src <- feeds$source[i]; url <- feeds$url[i]
  feed <- fetch_feed(url)
  if (is.null(feed)) {
    new_items[[length(new_items) + 1]] <- list(source = src, error = TRUE, error_msg = "Feed unreachable.")
    next
  }
  entries <- head(feed, 5)
  seen_hash <- state$seen[[src]] %||% character(0)
  
  for (j in seq_len(nrow(entries))) {
    ttl  <- entries$item_title[j] %||% "(no title)"
    lnk  <- entries$item_link[j] %||% ""
    desc <- entries$item_description[j] %||% entries$item_content[j] %||% ""
    pub  <- entries$item_pub_date[j] %||% entries$item_date[j] %||% ""
    
    item <- list(
      title = clean_text(ttl),
      link  = lnk,
      description = clean_text(desc),
      pub_date = as.character(pub),
      source = src
    )
    id <- paste0(item$title, "::", item$link)
    if (!(id %in% seen_hash)) {
      item$is_new <- TRUE
      new_items[[length(new_items) + 1]] <- item
      state$seen[[src]] <- unique(c(id, seen_hash))
    }
  }
}

# Build report body --------------------------------------------------
has_new <- any(vapply(new_items, function(x) isTRUE(x$is_new), logical(1)))
header_line <- if (has_new) "Some sources updated." else "No new items."

sections <- c()
for (src in feeds$source) {
  src_items <- Filter(function(x) !is.null(x$source) && x$source == src, new_items)
  if (length(src_items) == 0) {
    err <- Filter(function(x) isTRUE(x$error) && x$source == src, new_items)
    if (length(err)) sections <- c(sections, glue("### {src}\n- Feed unreachable (no change check)."))
    else             sections <- c(sections, glue("### {src}\n- No change."))
    next
  }
  lines <- c(glue("### {src}"))
  for (it in src_items) {
    if (isTRUE(it$error)) {
      lines <- c(lines, "- Feed unreachable (no change check)."); next
    }
    summ <- summarize_with_chatgpt(it$title, it$link, it$description, it$source)
    stamp <- if (nzchar(it$pub_date)) paste0(" (", it$pub_date, ")") else ""
    lines <- c(lines,
               glue("- **NEW** {it$title}{stamp}\n  {it$link}\n  Summary: {summ}")
    )
  }
  sections <- c(sections, paste(lines, collapse = "\n"))
}

report_md <- glue("
# WHO & Outbreak News — Summary — {format(now_ams, '%Y-%m-%d %H:%M %Z')}

**Overall status:** {header_line}

{paste(sections, collapse = '\n\n')}
")

# Save to file -------------------------------------------------------
if (!dir.exists("reports")) dir.create("reports", recursive = TRUE)
report_path <- file.path("reports", paste0("daily_report_", format(now_ams, "%Y%m%d"), ".md"))
writeLines(report_md, con = report_path)
cat("Report saved to", report_path, "\n")

# Save state for next run --------------------------------------------
write_json(state, state_path, pretty = TRUE, auto_unbox = TRUE)
