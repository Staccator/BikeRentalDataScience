CountStatisticsYear<- function(year)
{
  helpF<- function(m,y)
  {
    return(nrow(get(sprintf("dt%d%02d",y,m))))
  }
  return(sapply(1:12, helpF, y=year))
}
CountStatistics<- function()
{
  helpF<- function(y)
  {
    return(sum(CountStatisticsYear(y)))
  }
  result<- as.matrix(sapply(2016:2019, helpF))
  rownames(result)<- 2016:2019
  colnames(result)<- c("Count")
  
  write.csv(result,"ChartsData/CountYearly.csv")
  return(result)
}