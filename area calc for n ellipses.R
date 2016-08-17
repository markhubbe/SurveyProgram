#This script calculates the area of overlap between n ellipses.

#Store the data for ellipses 1 through n in the df ellipsedata:
# ellipsedata<-data.frame(n=c(1,2,...,n), h=c(...), k=c(...), a=c(...), b=c(...), c=c(...))

#TEST EXAMPLE
ellipsedata<-data.frame(n=c(1,2,3),
                        h=c(0,1,2),
                        k=c(2,4,3),
                        a=c(1,1,4), 
                        b=c(2,1,1), 
                        c=c(0,12,25)
                        )

d<-0.05 #Horizontal distance between points. Can be increased for speed or 
        #decreased for accuracy.

#Function 'coordinates' builds a df of coordinates for each ellipse
coordinates <- function(n, h, k, a, b, c){
  
  #Generate x-values without rotation
  xval<-seq(h-a,h+a, by=d)
  xvaldesc<-sort(xval, decreasing = TRUE)
  
  #Determine corresponding y-values without rotation
  ypos <- k + sqrt(b^2*(1-(xval-h)^2/a^2))
  yneg <- k - sqrt(b^2*(1-(xvaldesc-h)^2/a^2))
  
  #Build a dataframe of unique coordinates
  coordpos<-data.frame(xval, ypos)
  names(coordpos)[names(coordpos)=="xval"]<-"x"
  names(coordpos)[names(coordpos)=="ypos"]<-"y"
  coordneg<-data.frame(xvaldesc, yneg)
  names(coordneg)[names(coordneg)=="xvaldesc"]<-"x"
  names(coordneg)[names(coordneg)=="yneg"]<-"y"
  coord<-unique(rbind(coordpos,coordneg))

  #Now rotate coordinates by angle c
  points<-data.frame(x=vector(length=nrow(coord)),y=vector(length=nrow(coord)))
  points$x<-(coord$x-h)*cos(c*pi/180)-(coord$y-k)*sin(c*pi/180)+h
  points$y<-(coord$x-h)*sin(c*pi/180)+(coord$y-k)*cos(c*pi/180)+k

  #Rename the df according to the ellipse number n
  assign(paste0("points", n), points, envir=.GlobalEnv)
}

#Apply the function 'coordinates' to the df 'ellipsedata'
apply(ellipsedata, 
      MARGIN=1, 
      function(x) coordinates(x['n'],x['h'],x['k'],x['a'],x['b'],x['c']))

#for fun...
ellipselist<-lapply(ls(pattern = "points[0-9]"), get)
plot(points1$x, points1$y, 
     xlim = c(
       min(unlist(lapply(ellipselist, function(data) min(data$x)))),
       max(unlist(lapply(ellipselist, function(data) max(data$x))))),
     ylim = c(
       min(unlist(lapply(ellipselist, function(data) min(data$y)))),
       max(unlist(lapply(ellipselist, function(data) max(data$y))))),
     xlab="", ylab=""
)
lapply(ellipselist, function(data) points(data$x,data$y))

#Determine which points in each ellipse to keep. These are the points
#that are contained within all other ellipses.

#The 'check_point' function checks one point in a given ellipse
check_point<-function(x,y,h,k,a,b,c){
  (cos(c*pi/180)*(x-h)+sin(c*pi/180)*(y-k))^2/a^2 + (cos(c*pi/180)*(y-k)-sin(c*pi/180)*(x-h))^2/b^2 <= 1
}

#The 'check_equations' function uses 'check_point' to check each point in all ellipses
check_equations<-function(x,y){
  apply(ellipsedata, MARGIN=1,
       function(data) check_point(x, y, 
             data['h'], 
             data['k'], 
             data['a'], 
             data['b'], 
             data['c'])
       )
}

#The 'check_all' function applies 'check_equations' to all points
check_all<-sapply(ellipselist, 
                  function(data) check_equations(data$x,data$y), 
                  simplify = "array")

#Name each logical data frame using the 'check_all' data
for(i in 1:nrow(ellipsedata)){
  assign(paste0('logi',i), data.frame(check_all[i]))
}

#Build a list of n logical data frames
loglist<-lapply(ls(pattern = "logi[0-9]"), get)

#Determine the number of TRUE values in each row of each logical data frame
sums<-lapply(loglist, rowSums)

#Create an empty data frame to house all ellipse coordinates and logical sums
allcoords<-data.frame(x = numeric(0), 
                      y = numeric(0), 
                      sum = numeric(0))

#Fill 'allcoords' with the coordinates and logical sums
for(i in 1:nrow(ellipsedata)){
  temp<-data.frame("x" = data.frame(ellipselist[i])$x, 
                   "y" = data.frame(ellipselist[i])$y,
                   "sum" = unlist(sums[i])) 
  allcoords<-rbind(allcoords, temp) 
}

#Select the points from 'allcoords' that we keep. These are the points that are inside
#all n ellipses, or when sum = n.
n<-nrow(ellipsedata)
keep<-allcoords[allcoords$sum == n,]

#for fun...
plot(keep$x,keep$y, 
     xlim=c(min(keep$x),max(keep$x)), 
     ylim=c(min(keep$y),max(keep$y)),
     xlab="",ylab=""
)


# *************** Sort the points. (Mod Mark's code...???) ***************


#Calculate the area of the polygon using the shoelace algorithm.
l<-nrow(keep)
products<-vector(length=l)
xval<-keep[,1]
yval<-keep[,2]

for(i in 1:(l-1)){
  products[i]<-(xval[i]*yval[i+1]-yval[i]*xval[i+1])
}
products[l]<-xval[l]*yval[1]-yval[l]*xval[1]

area<-abs(sum(products, na.rm=TRUE)/2)
print(paste("Area of Overlap =",area))