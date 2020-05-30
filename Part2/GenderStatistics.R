GenderStatisticsYear<- function(year)
{
  helpF<- function(m,y)
  {
    return(pull(arrange(count(group_by(get(sprintf("dt%d%02d",y,m)),gender)),gender)[,2]))
  }
  return(sapply(1:12, helpF, y=year))
}
GenderStatistics<- function()
{
  helpF<- function(y)
  {
    res<- GenderStatisticsYear(y)
    return(apply(res,MARGIN = 1,FUN = sum))
  }
  result<- t(sapply(2016:2019, helpF))
  rownames(result)<- 2016:2019
  colnames(result)<- c("Unknown","Male","Female")
  
  write.csv(result,"ChartsData/GenderYearly.csv")
  return(result)
}