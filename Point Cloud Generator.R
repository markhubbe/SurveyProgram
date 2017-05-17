
#This script generates a cloud of points within a rotated and shifted ellipse.

#-----------------------------------
#Ellipse Characteristics

#Specific values selected for script testing:

a<-3 #x-direction axial length
b<-5 #y-direction axial length
alpha<-pi/4 #ellipse angle of rotation IN RADIANS - does this need to be degrees?? If so, no problem :)
h<-1 #x-direction shift
k<--3 #y-direction shift
  
#-----------------------------------
#Density and Volume of Point Cloud
n<-10000 #number of points in cloud
angl<-runif(n, 0, pi) #list of n random angles from 0 to pi
magn<-runif(n, -1, 1) #starting with a uniform dist - MOD THIS FOR NORMAL DIST...

#Determine the distance to the ellipse from the center at each angle theta
d<-sqrt(a^2*b^2/((sin(angl))^2*(a^2-b^2)+b^2))

#Generate Points Without Rotation
xval_unrot<-magn*d*cos(angl)
yval_unrot<-magn*d*sin(angl)

#Rotate points through angle alpha, then shift by (h,k)
xval<-xval_unrot*cos(alpha)-yval_unrot*sin(alpha)+h
yval<-xval_unrot*sin(alpha)+yval_unrot*cos(alpha)+k

#-----------------------------------
#Plot Points
plot(0,
     xlim=c(min(xval),max(xval)), 
     ylim=c(min(yval),max(yval)),
     xlab="",ylab=""
)
points(xval,yval)