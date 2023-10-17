convert_time_string <- function(time_str) {
  time_parts <- unlist(strsplit(time_str, "[:\\.]"))
  minutes <- as.integer(time_parts[1])
  seconds <- as.integer(time_parts[2])
  milliseconds <- as.integer(time_parts[3])
  total_seconds <- minutes * 60 + seconds + milliseconds / 1000
  return(total_seconds)
}