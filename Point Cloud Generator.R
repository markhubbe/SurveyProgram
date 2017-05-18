#This script generates cloud of points within a given ellipse.
pointCloud<-function(a, b, h, k, theta, n, type){
  
  # a = x-direction axial length / x-direction radius
  # b = y-direction axial length / y-direction radius
  # theta = ellipse angle of rotation IN RADIANS
  # h = x-direction shift / x-coordinate of center
  # k = y-direction shift / y-coordinate of center
  # n = number of points in cloud
  # type = "unif" or "norm"
  
  #Generate list of n random angles from 0 to pi
  angl<-runif(n, 0, pi) 
  
  #Determine the distance to the ellipse from the center at each angle theta
  d<-sqrt(a^2*b^2/((sin(angl))^2*(a^2-b^2)+b^2))

  #Generate list of n random distances based on distr type
  if(type == "norm"){
    magn<-rnorm(n, mean=0, sd=0.5102) #95% of points are contained within the ellipse
    } else if (type == "unif"){
      magn<-runif(n, -1, 1) #all points are contained within the ellipse
    } else {
      print("Type is invalid. Please choose unif or norm.")
    }

  #Generate cloud points without rotation or translation
  xval_unrot<-magn*d*cos(angl)
  yval_unrot<-magn*d*sin(angl)

  #Rotate cloud points through angle theta, then translate by (h,k) to match ellipse
  xval<-xval_unrot*cos(theta)-yval_unrot*sin(theta)+h
  yval<-xval_unrot*sin(theta)+yval_unrot*cos(theta)+k

  #Plot Points
  plot(xval, yval, xlab="", ylab="")
  
}