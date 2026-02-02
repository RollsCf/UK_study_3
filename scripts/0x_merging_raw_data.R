dat_raw <- fread("data_raw/participant_data.csv", data.table = FALSE)
height  <- fread("data_raw/height.csv", data.table = FALSE)

View(height)

dat_merged <- dat_raw %>%
  left_join(
    height %>% select(eid, p50_i0),
    by = "eid"
  )

write.csv(
  dat_merged,
  "data_raw/participant_data_with_height.csv",
  row.names = FALSE
)

## Manually rename back to participant_data for further analysis