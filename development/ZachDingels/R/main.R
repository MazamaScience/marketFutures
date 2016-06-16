library(futile.logger)

main <- function(dataDir='~/Data/Quandl', outputDir='~/Data/Quandl',
                 logLevel='INFO', transcript='MF_TRANSCRIPT.txt') {
  
  # Set up a new transcript file
  if (!is.null(transcript)) {
    transcriptPath <- file.path(outputDir,transcript)
    file.remove(transcriptPath)
    futile.logger::flog.appender(futile.logger::appender.file(transcriptPath))
  }
  
  # Set log level
  futile.logger::flog.threshold(get('logLevel'))
  
  # Silence other warning messages
  options(warn=-1)
  
}