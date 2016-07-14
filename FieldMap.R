#This function will create a grid and plot sites (as ellipses) in the grid. It is the first part of the Survey program.
FieldMap<-function(Area,site.density,site.area,overlap=0.25,plot=FALSE){
  #Variable properties:
  ##Area: vector with horizontal and vertical size of area surveyed in km.
  ##SiteDensity: measures as number of sites/km2 can be either one value or a vector with 2 values (min and max) to create a range of densities.
  ##site.area: it will be one of two options:
    #1. one value indicating the are of all sites, in meter^2 
    #2. a vector with 4 values: min, max, mean (or median), and standard deviation in meter^2. 
    #Or one single value for uniform sites.  
  ##overlap: maximum overlap of site area, ranging from 0 as no overlap to 1 as complete overlap.
  ##Plot: if the area should be plotted.
  #OBS: Sites will all be ellipses with radii not too different and random angles.
  
  
  #1. First we create the data.frame that will store the information for all sites.
  if(length(site.density)>1){
    nsites=ceiling(sample(site.density[1]:site.density[2],1)*Area[1]*Area[2])
  }else{
    nsites<-ceiling(site.density*Area[1]*Area[2])
  }
  
  #this will bring the site area to km2
  
  site.area<-site.area/1e6
   
  site.frame<-matrix(0,nsites,8)
  
  colnames(site.frame)=c("Site","Area","Eccentricity","Angle","center.x","center.y","ellipse.a","ellipse.b")
  
  #2.We fill the data.frame
  for(a in 1:nsites){
    site.frame[a,1]<-a
    
    if(length(site.area)>1){
    site.frame[a,2]<-rnorm(1,site.area[3],site.area[4])
    }else{
      site.frame[a,2]<-site.area
    }
    
    site.frame[a,3]<-runif(1,0,0.85)
    site.frame[a,4]<-runif(1,0,pi)
    #We will create a seed of random positions and at each loop we will 
    #remove the values that fall within the ellipsis, so that sites never overlap by much they never overlap
    
    temp.x<-runif(nsites*10,0,Area[1])
    temp.y<-runif(nsites*10,0,Area[2])
    
    #x<<-temp.x
    #y<<-temp.y
    
      site.frame[a,5]<-temp.x[1]
      site.frame[a,6]<-temp.y[1]
    
      #Here is were we remove from the seed vector all points within the ellipsis defined in the loop. 
      site.frame[a,7]<-(site.frame[a,2]/(pi*(1-site.frame[a,3]^2)^0.5))^0.5
      site.frame[a,8]<-((site.frame[a,2]*(1-site.frame[a,3]^2)^0.5)/pi)^0.5  
      #this is the ellipse function from Cara.
      ellipse.function<-function(x) ((x[1]-site.frame[a,5])*cos(site.frame[a,4])+(x[2]-site.frame[a,6])*sin(site.frame[a,4]))^2/site.frame[a,7]^2+
                                    ((x[1]-site.frame[a,5])*sin(site.frame[a,4])-(x[2]-site.frame[a,6])*cos(site.frame[a,4]))^2/site.frame[a,8]^2<=1      
    
      toBeRemoved<-apply(rbind(temp.x,temp.y), 2, ellipse.function)
      
      temp.x<-temp.x[which(toBeRemoved==FALSE)]
      temp.y<-temp.y[which(toBeRemoved==FALSE)]
      
      #this is just an error to check how often we would run out of random seeds, since I'm using N*10...
      if(length(temp.x)==0){
        stop(paste("ERROR! Routine ran out of random seeds after ", a,"/",nsites," loops!"))
      }
    }
    
  
    #Here we plot, if plot = TRUE
  
  if(plot==TRUE){
    
    plot.new()
    plot.window(c(0,Area[1]),c(0,Area[2]))
    
    axis(1)
    axis(2)
    box()
    
    for(a in 1:nsites){
      text(site.frame[a,5],site.frame[a,6],site.frame[a,1],pos=3,cex=0.5)
      points(site.frame[a,5],site.frame[a,6],pch=16)
    
    #1.Get the angles, x and y to plot the ellypses
      angles<-seq(0,2*pi,length=72)
      
      #x= h + a cos(t)*cos(c)-b*sin(t)*sin(c)
      xcoords<- site.frame[a,5]+site.frame[a,7]*cos(angles)*cos(site.frame[a,4])-site.frame[a,8]*sin(angles)*sin(site.frame[a,4]) 
      #y= k + b sin(t)*cos(c)+a*cos(t)*sin(c)  
      ycoords<- site.frame[a,6]+site.frame[a,8]*sin(angles)*cos(site.frame[a,4])+site.frame[a,7]*cos(angles)*sin(site.frame[a,4])  
    
      polygon(xcoords,ycoords,col=rgb(0,0,1,0.5),border=NA)
      
      }
    
  }
  
  return(site.frame)
  
  
}