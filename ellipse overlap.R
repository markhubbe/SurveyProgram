
#This function was coded based on Cara's math, and approximates the area that 2 ellises overlap.
#Ellipse1 and 2 are vectors with (position in x, position in y, major axis, minor axis, angle in radians). 

ellipses.overlap<-function(ellipse1,ellipse2, plot=FALSE){
  
  
  #--------------------------------
  #The equation of a rotated ellipse is provided here for reference.
  #NOTE: The direction of rotation is counterclockwise and the angle 
  #of rotation is measured in radians.
  
  # (cos(c)*(x-h)+sin(c)*(y-k))^2/a^2 + (cos(c)*(y-k)-sin(c)*(x-h))^2/b^2 = 1
  #--------------------------------
  #Get coordinates for each ellipse.
  
  
  h1<-ellipse1[1]
  k1<-ellipse1[2]
  a1<-ellipse1[3]
  b1<-ellipse1[4]
  c1<-ellipse1[5] #measured in radians
  
  h2<-ellipse2[1]
  k2<-ellipse2[2]
  a2<-ellipse2[3]
  b2<-ellipse2[4]
  c2<-ellipse2[5] #measured in radians
  
  #Generate x-values for the first ellipse without rotation
  e1x<-seq(h1-a1,h1+a1, length=50)
  e1xd<-sort(e1x, decreasing = TRUE)
  #Determine corresponding y-values without rotation
  #OBS, the round here is to get rid of very small negative numbers, which happens with small ellipses.
  e1yp <- k1 + sqrt(round(b1^2*(1-((e1x-h1)/a1)^2),16))
  e1yn <- k1 - sqrt(round(b1^2*(1-((e1xd-h1)/a1)^2),16))
  #Build a dataframe of unique coordinates
  e1coordp<-data.frame(e1x, e1yp)
  names(e1coordp)[names(e1coordp)=="e1yp"]<-"e1y"
  e1coordn<-data.frame(e1xd, e1yn)
  names(e1coordn)[names(e1coordn)=="e1xd"]<-"e1x"
  names(e1coordn)[names(e1coordn)=="e1yn"]<-"e1y"
  e1coord<-unique(rbind(e1coordp,e1coordn))
  
  #Now rotate coordinates by angle c1
  e1<-data.frame(x=vector(length=nrow(e1coord)),
                 y=vector(length=nrow(e1coord)))
  e1$x<-(e1coord$e1x-h1)*cos(c1)-(e1coord$e1y-k1)*sin(c1)+h1
  e1$y<-(e1coord$e1x-h1)*sin(c1)+(e1coord$e1y-k1)*cos(c1)+k1
  
  #Generate x-values for the second ellipse without rotation
  e2x<-seq(h2-a2,h2+a2, length=50)
  e2xd<-sort(e2x, decreasing = TRUE)
  #Determine corresponding y-values without rotation
  e2yp <- k2 + sqrt(round(b2^2*(1-(e2x-h2)^2/a2^2),16))
  e2yn <- k2 - sqrt(round(b2^2*(1-(e2xd-h2)^2/a2^2),16))
  #Build a dataframe of unique coordinates
  e2coordp<-data.frame(e2x, e2yp)
  names(e2coordp)[names(e2coordp)=="e2yp"]<-"e2y"
  e2coordn<-data.frame(e2xd, e2yn)
  names(e2coordn)[names(e2coordn)=="e2xd"]<-"e2x"
  names(e2coordn)[names(e2coordn)=="e2yn"]<-"e2y"
  e2coord<-unique(rbind(e2coordp,e2coordn))
  
  #Now rotate coordinates by angle c2
  e2<-data.frame(x=vector(length=nrow(e2coord)),
                 y=vector(length=nrow(e2coord)))
  e2$x<-(e2coord$e2x-h2)*cos(c2)-(e2coord$e2y-k2)*sin(c2)+h2
  e2$y<-(e2coord$e2x-h2)*sin(c2)+(e2coord$e2y-k2)*cos(c2)+k2
  
  #for fun...
  if(plot==TRUE){
  plot(e1$x,e1$y, 
       xlim=c(min(e1$x,e2$x),max(e1$x,e2$x)), 
       ylim=c(min(e1$y,e2$y),max(e1$y,e2$y)),
       xlab="",ylab=""
  )
  points(e2$x,e2$y)
  }
  #--------------------------------
  #Select the points of interest on each graph. Points of interest 
  #are those which are contained within the other graph. These form 
  #the boundaries of the overlapping area.
  
  #e1 points inside e2
  e1keep<-e1[(cos(c2)*(e1[,1]-h2)+sin(c2)*(e1[,2]-k2))^2/a2^2 + (cos(c2)*(e1[,2]-k2)-sin(c2)*(e1[,1]-h2))^2/b2^2 <= 1,]
  
  #e2 points inside e1
  e2keep<-e2[(cos(c1)*(e2[,1]-h1)+sin(c1)*(e2[,2]-k1))^2/a1^2 + (cos(c1)*(e2[,2]-k1)-sin(c1)*(e2[,1]-h1))^2/b1^2 <= 1,]
  
  #for fun...
  if(plot==TRUE){
  if(nrow(e1keep)!=0 & nrow(e2keep)!=0){
    plot(e1keep$x,e1keep$y, 
         xlim=c(min(e1keep$x,e2keep$x),max(e1keep$x,e2keep$x)), 
         ylim=c(min(e1keep$y,e2keep$y),max(e1keep$y,e2keep$y)),
         xlab="",ylab=""
    )
    points(e2keep$x,e2keep$y)
  }
  }
  
  #--------------------------------  
  #Merge the two sets of coordinates into 'polycoord' - Mark's code :D
  
  if(nrow(e1keep)==0 & nrow(e2keep)==0){
    #when there are no points contained within either ellipse, return 0
    area<-0
  } else {
    if(nrow(e1keep)==0){
      #if ellipse2 is entirely within ellipse1, the overlap area
      #is equal to the area of the inner ellipse
      #The area is still calculated using a polygon
      polycoord<-e2keep
    } else {
      if(nrow(e2keep)==0){
        #if ellipse1 is entirely within ellipse2, the overlap area
        #is equal to the area of the inner ellipse
        #The area is still calculated using a polygon
        polycoord<-e1keep
      } else {
        polycoord<-matrix(0,(nrow(e1keep)+nrow(e2keep)),2)
        minE1X<-min(e1keep[,1])
        maxE1X<-max(e1keep[,1])
        walkDown=FALSE
        triggered=FALSE
        countE1=1
        countE2=1
        for(a in 1:nrow(polycoord)){
          if(e1keep[countE1,1]==maxE1X && triggered==FALSE){
            walkDown=TRUE
            triggered=TRUE
          }
          
          if (walkDown==FALSE){
            polycoord[a,]<-as.matrix(e1keep[countE1,])
            countE1<-countE1+1
          }
          if (walkDown==TRUE){
            if(countE2<= nrow(e2keep)){
              polycoord[a,]<-as.matrix(e2keep[countE2,])
              countE2<-countE2+1
            }else{
              polycoord[a,]<-as.matrix(e1keep[countE1,])
              countE1<-countE1+1
            }
          }
        } 
      }
    }
  }
  
  #--------------------------------  
  #Calculate the area of each ellipse
  area_e1<-pi*a1*b1
  area_e2<-pi*a2*b2
  
  #Calculate the area of the polygon using the shoelace algorithm
  
  if(nrow(e1keep)!=0 | nrow(e2keep)!=0){
    l<-nrow(polycoord)
    products<-vector(length=l)
    xval<-polycoord[,1]
    yval<-polycoord[,2]
    
    for(i in 1:(l-1)){
      products[i]<-(xval[i]*yval[i+1]-yval[i]*xval[i+1])
    }
    products[l]<-xval[l]*yval[1]-yval[l]*xval[1]
    
    area<-abs(sum(products, na.rm=TRUE)/2)
  }
  
  results<-list(area.el1=area_e1,area.el2=area_e2,area.overlap=area)
  
  return(results)
  
}