#################################################
# Utility functions (lowest level)

# Set data directory. Guarantee that DATA_DIR is a full path
setDataDir <- function(dataDir) {
  FUTURES_DATA_DIR <<- normalizePath(dataDir)
}

# Return data directory 
getDataDir <- function() {
  if (FUTURES_DATA_DIR == "") {
    warning(paste0('The data directory has not been set. Please set it with setDataDir(YOUR_DATA_DIR).'))
  }
  return(FUTURES_DATA_DIR)
}
