LoadDataYear<- function(y)
{
  helpF<- function(fileName)
  {
    dataFrameName<- paste("dt", substr(fileName, 17,22),sep = "")
    temp<- read.csv(file = fileName)
    names(temp)<- tolower(names(temp))
    assign(dataFrameName,temp,envir = globalenv() )
  }
  months<- sprintf("Part2/JCdata/JC-%d%02d-citibike-tripdata.csv",y,1:12)
  x<- sapply(months, helpF)
  
}
LoadData<- function()
{
  suppressMessages(library(dplyr))
  x<- sapply(2016:2019, LoadDataYear)
}