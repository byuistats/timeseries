# ------------------------------------------------------------------------------
# Continuous (15-minute) data for:
#   MF Salmon River at MF Lodge nr Yellow Pine, ID  -  USGS-13309220
#
# Uses the modern USGS Water Data API via dataRetrieval::read_waterdata_continuous()
# (the older readNWISuv() / WaterServices are retired Feb 22, 2027).
#
# Notes from the dataRetrieval docs:
#   * The service returns at most ~3 years per request, so longer periods are
#     pulled in 3-year chunks below.
#   * Times are returned in UTC; this script converts them to local time.
#   * An optional API token (API_USGS_PAT in .Renviron) raises hourly limits:
#     https://api.waterdata.usgs.gov/signup/
# ------------------------------------------------------------------------------

# install.packages("dataRetrieval")   # run once if needed
library(dataRetrieval)

# ---- Settings ----------------------------------------------------------------
site       <- "USGS-13309220"
pcodes     <- c("00060")            # 00060 = discharge (ft^3/s); add "00065" for gage height, "00010" for water temp
start_date <- as.Date("1999-03-19") # adjust to the period you want
end_date   <- Sys.Date()
local_tz   <- "America/Boise"       # Mountain time (Yellow Pine, ID)
out_csv    <- "USGS-13309220_continuous.csv"

# ---- Quick option: just the last N days --------------------------------------
# recent <- read_waterdata_continuous(monitoring_location_id = site,
#                                     parameter_code = pcodes,
#                                     time = "P7D")   # last 7 days

# ---- Build ~3-year request chunks --------------------------------------------
n_days      <- as.numeric(difftime(end_date, start_date, units = "days"))
n_chunks    <- max(1, ceiling(n_days / (3 * 365.25)))
chunk_dates <- seq(from = start_date, to = end_date, length.out = n_chunks + 1)

chunks <- data.frame(
  start = chunk_dates[-length(chunk_dates)],
  end   = c(chunk_dates[-c(1, length(chunk_dates))] - 1, end_date)
)

# ---- Download each chunk -----------------------------------------------------
results <- vector("list", nrow(chunks))

for (i in seq_len(nrow(chunks))) {
  message(sprintf("Chunk %d of %d: %s to %s",
                  i, nrow(chunks), chunks$start[i], chunks$end[i]))
  results[[i]] <- tryCatch(
    read_waterdata_continuous(
      monitoring_location_id = site,
      parameter_code         = pcodes,
      time                   = c(chunks$start[i], chunks$end[i]),
      properties             = c("monitoring_location_id", "parameter_code",
                                 "time", "value", "unit_of_measure",
                                 "approval_status", "qualifier")
    ),
    error = function(e) {
      warning(sprintf("Chunk %d failed: %s", i, conditionMessage(e)))
      NULL
    }
  )
}

all_data <- do.call(rbind, results[!vapply(results, is.null, logical(1))])

# ---- Clean up ----------------------------------------------------------------
if (is.null(all_data) || nrow(all_data) == 0) {
  stop("No data returned. Check the site ID, parameter code, and date range.")
}

# Drop any duplicate timestamps where chunks meet, then sort
all_data <- all_data[!duplicated(all_data[, c("parameter_code", "time")]), ]
all_data <- all_data[order(all_data$parameter_code, all_data$time), ]

# Convert UTC timestamps to local time (same instant, different display zone)
all_data$time_local <- all_data$time
attr(all_data$time_local, "tzone") <- local_tz

# Qualifiers can come back as a list column; flatten for CSV export
if (is.list(all_data$qualifier)) {
  all_data$qualifier <- vapply(all_data$qualifier,
                               function(q) paste(q, collapse = ";"),
                               character(1))
}

# ---- Summarize & save --------------------------------------------------------
message(sprintf("Retrieved %s records from %s to %s",
                format(nrow(all_data), big.mark = ","),
                format(min(all_data$time_local), "%Y-%m-%d %H:%M %Z"),
                format(max(all_data$time_local), "%Y-%m-%d %H:%M %Z")))
print(table(all_data$parameter_code, all_data$approval_status))
print(summary(all_data$value))

write.csv(all_data, out_csv, row.names = FALSE)
message("Saved to ", out_csv)

# ---- Quick plot --------------------------------------------------------------
q <- all_data[all_data$parameter_code == "00060", ]
plot(q$time_local, q$value, type = "l",
     xlab = "", ylab = "Discharge (ft³/s)",
     main = "MF Salmon River at MF Lodge nr Yellow Pine, ID (USGS-13309220)")
