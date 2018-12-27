#
#   Fama French data download
#
#   on GitHub
#
#   e.g.  FamaFrenchDownload("2018-08-31")
#
#   ignore the warning message
#


require(xts)
require(lubridate)

FamaFrenchDownload <- function(last_date){

  ### Download & Unzip the MOMENTUM .zip data

  ff_mom_url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_TXT.zip"
  download.file(ff_mom_url, destfile = "F-F_Momentum_Factor_TXT.zip", mode = 'wb')
  unzip("F-F_Momentum_Factor_TXT.zip")

  ### Calculate the NUMBER OF MONTHS which gives the NUMBER OF ROWS to extract from the data file

  mom_startDate <- as.Date("1927-01-31")
  mom_endDate   <- as.Date(last_date)
  mom_interval  <- interval(mom_startDate, mom_endDate)
  mom_interval  <- mom_interval %/% months(1)
  mom_interval  <- mom_interval + 1

  ### Read in the unzipped .txt file

  ff.mom_factor = read.delim("F-F_Momentum_Factor.txt",
                             sep = "",
                             header = FALSE,
                             skip= 14,
                             nrow = mom_interval,
                             stringsAsFactors = FALSE)

  ### Convert to xts format (create an xts date sequence)

  ff.mom_factor_date_index    <- seq(mom_startDate+1, mom_endDate+1, by = 'months')-1
  ff.mom_factor.xts           <- xts(ff.mom_factor[,2]/100, order.by = ff.mom_factor_date_index)
  colnames(ff.mom_factor.xts) <- "Mom"

  ### Download & Unzip the FIVE FACTORS .zip data

  ff.5factors.url = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_TXT.zip"
  download.file(ff.5factors.url, destfile = "F-F_5factors.zip", mode = 'wb')
  unzip("F-F_5factors.zip")

  ### Calculate the NUMBER OF MONTHS which gives the NUMBER OF ROWS to extract from the data file

  FiveFactor_startDate <- as.Date("1963-07-31")
  FiveFactor_endDate   <- as.Date(last_date)
  FiveFactor_interval  <- lubridate::interval(FiveFactor_startDate, FiveFactor_endDate)
  FiveFactor_interval  <- FiveFactor_interval %/% months(1)
  FiveFactor_interval  <- FiveFactor_interval + 1

  ff.5factors = read.delim("F-F_Research_Data_5_Factors_2x3.txt",
                           sep = "",
                           header = FALSE,
                           skip= 4,
                           nrow = FiveFactor_interval,
                           stringsAsFactors = FALSE)

  colnames(ff.5factors)<- c("Date", "Mkt-RF", "SMB","HML","RMW","CMA","RF")

  ### Convert to xts format (create an xts date sequence)

  ff.5factors_date_index <- seq(FiveFactor_startDate+1, FiveFactor_endDate+1, by = 'months')-1
  ff.5factors.xts        <- xts(ff.5factors[,2:7]/100, order.by = ff.5factors_date_index)

  ### Combine MOMENTUM with FIVE FACTORS

  ff.6factors.xts        <- merge.xts(ff.5factors.xts, ff.mom_factor.xts, join = "inner")

  return(ff.6factors.xts)
}
