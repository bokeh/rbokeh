co2_df <- data.frame(
  date = lubridate::date_decimal(as.vector(time(co2))),
  co2 = as.vector(co2))

use_data(co2_df)
