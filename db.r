#
# Functions for talking to Microsoft SQL.
#
# Currently philosophy is to use DSN files created via Windows' DataSources (ODBC)
# application under File DSN

library(RODBC) # Connectivity package

# Modify this variable in your code (not here) if you access DSN's at a different location.
if (!exists(ignore.dsn)) dsn.path <- "r:/tools/dsn"

# Sample DSN:
# [ODBC]
# DRIVER=SQL Server
# UID=username
# PWD=password
# WSID=SERVER_NAME
# SERVER=SERVER.IP
# Description=DATABASE

# See:
# http://dba.stackexchange.com/questions/2910/can-a-username-and-password-be-hard-coded-in-a-system-dsn
# https://www.connectionstrings.com/odbc-dsn/file-dsn/
# https://groups.google.com/forum/#!topic/microsoft.public.data.odbc/NW6ACwEm9Dw
dsnConnect <- function(src) {
	odbcDriverConnect(paste("FILEDSN=", dsn.path, "/", src, ".dsn", sep = ""))
}

query <- function(sql, database = F, src = "DATA1") {
	channel <- dsnConnect(src)
  	if (database != F) sqlQuery(channel, paste("USE", database))
  	result <- sqlQuery(channel, sql, stringsAsFactors = F)
  	odbcCloseAll()
  	return (result)
}

# Insert a dataframe into a MSSQL table. If append = F, will not allow insert into existing table.
importDf <- function(table, database, df, src, append = T) {
	channel <- dsnConnect(src)
	sqlQuery(channel, paste("USE", database))
	sqlSave(channel, df, tablename = table, rownames = F, append = append)
	odbcCloseAll()
}

# Read and execute a query directly from a file
queryFromFile <- function(filename, database = F, src = "DATA1") {
	qstring <- readChar(filename, file.info(filename)$size)
	query(qstring, database, src)
}
