#This function will plot the results of the SurveyGrid Simulations using Kernel Density Plots.
#All the girds that want to be compared should be grouped into 1 list (list(a,b,c,etc...))
#The plot function allow you to chose from different parts of SurveySummaries youo want to plot.
  #plotting options are: sites.found, sites.missed, survey.hits, survey.missed
#labels: a vector of strings for the legend, in the same order of the matrices in list

PlotSurveySumm<-function(SummaryList,plot="sites.found",labels){
  
  #1.Define the variable to be plotted
  if(plot=="sites.found"){
    targetcol=5
    MainTitle="Frequency of sites discovered"
  }

  if(plot=="sites.missed"){
    targetcol=5
    MainTitle="Frequency of sites missed"
  }

  if(plot=="survey.hits"){
    targetcol=6
    MainTitle="Frequency of surveys that located sites"
  }

  if(plot=="survey.missed"){
    targetcol=6
    MainTitle="Frequency of negative surveys"
  }
  densities<-list()
  means<-rep(0,length(SummaryList))
  stdevs<-rep(0,length(SummaryList))
  maxy<-rep(0,length(SummaryList))
  
  #2.Create the density curves and get summary stats
  for(a in 1:length(SummaryList)){
    if(plot=="sites.found" || plot=="survey.hits"){
    densities[[a]]<-density(SummaryList[[a]][,targetcol])
    means[a]<-mean(SummaryList[[a]][,targetcol])
    stdevs[a]<-sd(SummaryList[[a]][,targetcol])
    }
    
    if(plot=="sites.missed" || plot=="survey.missed"){
      densities[[a]]<-density(1-SummaryList[[a]][,targetcol])
    means[a]<-mean(1-SummaryList[[a]][,targetcol])
    stdevs[a]<-sd(1-SummaryList[[a]][,targetcol])
    }
    
    maxy[a]<-max(densities[[a]]$y)
  }
  
  #3.Create the plot
  plot.new()
  plot.window(c(0,1),c(0,max(maxy)))
  
  axis(1, at=seq(0,1,by=0.1))
  axis(2)
  
  ColorLine<-rainbow(length(SummaryList),alpha=0.7)
  ColorSolid<-rainbow(length(SummaryList),alpha=0.4)
  
  for(a in 1:length(SummaryList)){
    polygon(densities[[a]],col=ColorSolid[a])
    lines(densities[[a]],col=ColorSolid[a])
    
  }
  #4.Plot the summary stats and a legend
  
  legend("topright",paste(labels," (",round(means,2),"\u00B1", round(stdevs,2),")"),fill=ColorSolid)
  
  #5.Add some titles
  
  
  title(main=MainTitle)
  
}

  