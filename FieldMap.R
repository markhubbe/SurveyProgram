#This function will create a grid and plot sites (as ellipses) in the grid. It is the first part of the Survey program.
FieldMap<-function(Area,site.density,site.area,overlap=0.50,plot=FALSE,accu.area=FALSE){
  #Variable properties:
  ##Area: vector with horizontal and vertical size of area surveyed in km.
  ##SiteDensity: measures as number of sites/km2 can be either one value or a vector with 2 values (min and max) to create a range of densities.
  ##site.area: it will be one of two options:
    #1. one value indicating the are of all sites, in meter^2 
    #2. a vector with 4 values: min, max, mean (or median), and standard deviation in meter^2. 
    #Or one single value for uniform sites.  
  ##overlap: maximum overlap of site area, ranging from 0 as no overlap to 1 as complete overlap.
  ##Plot: if the area should be plotted.
  ##accu.area=Calculates tha area of the sites, considering the overlap. More time consuming.
  #OBS: Sites will all be ellipses with radii not too different and random angles.
  
  
  #1. First we create the data.frame that will store the information for all sites.
  if(length(site.density)>1){
    nsites=ceiling(sample(site.density[1]:site.density[2],1)*Area[1]*Area[2])
  }else{
    nsites<-ceiling(site.density*Area[1]*Area[2])
  }
  
  #this will bring the site area to km2
  
  site.area<-site.area/1e6
   
  site.frame<-matrix(0,nsites,9)
  
  colnames(site.frame)=c("Site","Area","Eccentricity","Angle","center.x","center.y","ellipse.a","ellipse.b","Area")
  
  #We will create a seed of random positions and at each loop we will 
  #remove the values that fall within the ellipsis, so that sites never overlap by much 
  
  temp.x<-runif(nsites*10,0,Area[1])
  temp.y<-runif(nsites*10,0,Area[2])
  
  
  #2.We fill the data.frame
  for(a in 1:nsites){
    
    site.frame[a,1]<-a
    
    if(length(site.area)>1){
      while(site.frame[a,2]<site.area[1]||site.frame[a,2]>site.area[2]){
    site.frame[a,2]<-rnorm(1,site.area[3],site.area[4])
      }
    }else{
      site.frame[a,2]<-site.area
    }
    
    site.frame[a,3]<-runif(1,0,0.85)
    site.frame[a,4]<-runif(1,0,pi)
        
    
      site.frame[a,5]<-temp.x[1]
      site.frame[a,6]<-temp.y[1]
    
      #Here is were we remove from the seed vector all points within the ellipsis defined in the loop. 
      site.frame[a,7]<-(site.frame[a,2]/(pi*(1-site.frame[a,3]^2)^0.5))^0.5
      site.frame[a,8]<-((site.frame[a,2]*(1-site.frame[a,3]^2)^0.5)/pi)^0.5  
      if(overlap==1){
        temp.x<-temp.x[-1]
        temp.y<-temp.y[-1]  
      }else{
      #this is the ellipse function from Cara.
      ellipse.function<-function(x) ((x[1]-site.frame[a,5])*cos(site.frame[a,4])+(x[2]-site.frame[a,6])*sin(site.frame[a,4]))^2/((2-2*overlap)*site.frame[a,7])^2+
                                    ((x[1]-site.frame[a,5])*sin(site.frame[a,4])-(x[2]-site.frame[a,6])*cos(site.frame[a,4]))^2/((2-2*overlap)*site.frame[a,8])^2<=1      
    
      toBeRemoved<-apply(rbind(temp.x,temp.y), 2, ellipse.function)
      
      temp.x<-temp.x[which(toBeRemoved==FALSE)]
      temp.y<-temp.y[which(toBeRemoved==FALSE)]
      }
      
      site.frame[a,9]<-pi*site.frame[a,7]*site.frame[a,8]
      
      sites.created<-a
      
      #this will stop the loop if no more sites can be added to the map or we  run out of random seeds, since I'm using N*10...
      if(length(temp.x)==0){
        print(paste("Routine ran out of random seeds after ", a,"/",nsites," loops!"))
        break
        }
      
    }
    
  
    #3. Here we plot, if plot = TRUE
  
  if(plot==TRUE){
    
    plot.new()
    plot.window(c(0,Area[1]),c(0,Area[2]))
    
    axis(1)
    axis(2)
    box()
    
    for(a in 1:sites.created){
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
  
  #4.We get some of the overall statistics (Sites created, Total Site Area, accurate area of sites) and put them is a list
  
  results<-list(sites.created=sites.created)
  TotalArea<-sum(site.frame[,9])
  results[[2]]<-TotalArea
  names(results)[2]<-"total.area"
  
  if(accu.area==TRUE){
    #Here we will calculate the actual coverage area of the sites, using another of Cara's equation. 
    #The loops and polynomials make this slow, though, reason why this is optional.
    overlap.area<-matrix(0,nsites,nsites)
    
    for(a in 1:(sites.created-1)){
      for(b in (a+1):sites.created){
        distance<-((site.frame[a,5]-site.frame[b,5])^2+(site.frame[a,6]-site.frame[b,6])^2)^0.5
        if(distance<(site.frame[a,7]+site.frame[b,7])){
          tmpOverlap<-ellipses.overlap(c(site.frame[a,5],site.frame[a,6],site.frame[a,7],site.frame[a,8],site.frame[a,4]),c(site.frame[b,5],site.frame[b,6],site.frame[b,7],site.frame[b,8],site.frame[b,4]))
          overlap.area[a,b]<-tmpOverlap$area.overlap
          
        }
      }
    }
  
  overlap.sum<-sum(overlap.area)
  
  results[[3]]<-overlap.sum
  names(results)[3]<-"overlap.area"
  
  }
  
  results[[length(results)+1]]<-site.frame[1:sites.created,]
  names(results)[length(results)]<-"site.dataframe"
  
  return(results)
  
  
}