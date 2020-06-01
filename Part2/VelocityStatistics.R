VelocityStatisticsYearly<- function(year)
{
  helpF<- function(m,y)
  {
    get(sprintf("dt%d%02d",y,m))%>%filter(start.station.id!=end.station.id)-> stationsData
    a<- sin((stationsData$end.station.latitude- stationsData$start.station.latitude)*pi/360)*
      sin((stationsData$end.station.latitude- stationsData$start.station.latitude)*pi/360)+
      cos(stationsData$end.station.latitude*pi/180)*
      cos(stationsData$start.station.latitude*pi/180)*
      sin((stationsData$end.station.longitude- stationsData$start.station.longitude)*pi/360)*
      sin((stationsData$end.station.longitude- stationsData$start.station.longitude)*pi/360)
    c<- 2* atan2(sqrt(a),sqrt(1-a))
    R<- 6371000
    Distances<- R*c
    
    Velocity<- Distances/stationsData$trip.duration *36/10
    Velocity[Velocity>50]<- 0
    return(c(max(Velocity[stationsData$gender==1]),max(Velocity[stationsData$gender==2]) ))
  }
  data.frame(t((sapply(1:12, helpF, y=year))))->data
  colnames(data)<- c("Male","Female")
  rownames(data)<- substr(month.name,1,3)
  write.csv(data, sprintf("ChartsData/VelocityMonthly%d.csv",year))
  return(data)
}
VelocityStatistics<- function()
{
  helpF<- function(y)
  {
    res<- VelocityStatisticsYearly(y)
    return(apply(res,MARGIN = 1,FUN = sum))
  }
  sapply(2016:2019, helpF)->x
}