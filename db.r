# For database connectivity. Currently philosophy is to use DSN files created via Windows' DataSources (ODBC)
# application under File DSN

library(RODBC) # Connectivity package

# Modify this variable in your code (not here) if you access DSN's at a different location.
dsn.path <- "r:/work/tools/dsn"

# See:
# http://dba.stackexchange.com/questions/2910/can-a-username-and-password-be-hard-coded-in-a-system-dsn
# https://www.connectionstrings.com/odbc-dsn/file-dsn/
# https://groups.google.com/forum/#!topic/microsoft.public.data.odbc/NW6ACwEm9Dw
dsnConnect <- function(src) {
	odbcDriverConnect(paste("FILEDSN=", dsn.path, "/", src, ".dsn", sep = ""))
}

query <- function(sql, database = F, src = "DATA1") {
	channel <- dsnConnect(src)
  	if (database) sqlQuery(channel, paste("USE", database))
  	result <- sqlQuery(channel, sql, stringsAsFactors = F)
  	odbcCloseAll()
  	return (result)
}

# Insert a dataframe into a MSSQL table. If append = F, will not allow insert into existing table.
importDf <- function(table, database, df, src = "DATA1", append = T) {
	channel <- dsnConnect(src)
	sqlQuery(channel, paste("USE", database))
	sqlSave(channel, df, tablename = table, rownames = F, append = append)
	odbcCloseAll()
}