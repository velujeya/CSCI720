
# HW00_Vell_Jey.R
# In this function we take a range of speeds within a given limits of min and max
# values to find the flux for that speed. The flux calculation consists of
# determining the total distance a car requires
# Car Default Length
# Default Time to respond  (human brain) => 4 seconds
# Time required for car to respond  = proportional to kinetic speed car holds
# The Time to Stop is maximum value between "Default time to respond" and "Time
# required for car to respond" as they could happen in parallel.
# Finally the calculation is:
# The safety distance between two cars => timeToStop * 1/60 min * 1/60 hours * Car Velocity miles/hour
#

fluxCal <- function(minSpeed, maxSpeed){
  #For loop counter
  loop <- 1
  #Different values of CarFlux array
  CarFluxArray <- c()
  #Different values of Car Velocity array
  CarVelocityArray <- c()
  #Car kinetic enery to time conversion constant
  alpha <- 0.0055
  #Average Car Length
  carLength <- 10.0
  #miles to feet conversation rate
  conversionRate <- 5280.0

  for (v in minSpeed :maxSpeed) {

    #Safety distance = d
    #Time take a human to respond = 4 seconds
    timeToRespond <- 4.0
    timeToStop <- computerCarReactionTime(v, alpha)
    timeRequired = max(timeToStop, timeToRespond)
    # the total distance per car needed for stopping in foot
    totalDistancePerCar <- carLength + v * (timeRequired)* conversionRate *(1/60)*(1/60)
    # Car Density per mile is
    carDensity <- 1/(totalDistancePerCar/conversionRate)
    # the flux for Car is
    carFlux <- carDensity * v
    CarFluxArray[loop] <- carFlux
    CarVelocityArray[loop] <- v
    loop <- loop + 1
  }

  #CarDataFrame with flux and speed
  CarDataFrame  = data.frame(CarFluxArray,CarVelocityArray)

  #Find the index for maximum car flux
  maxCarFluxIndex <- which.max(CarFluxArray)

  #Find the relevant car speed for maximum car flux
  optimumCarSpeed  <- CarVelocityArray[maxCarFluxIndex]
  print (c('The most efficient road speed was : ', optimumCarSpeed))
  #Find the maximum car flux
  maxflux <- CarFluxArray [maxCarFluxIndex]
  print(c('The best efficiency was :  ', maxflux ,' cars per hour'))

  #Calling to Graph Funciton to create a plot for CarFlux agains CarSpeed
  p <- plotCarFluxVsSpeed(CarDataFrame, CarVelocityArray, CarFluxArray, optimumCarSpeed, maxflux)

  #Actually plotting the graph
  plot(p)

}

# For a given speed any car will hold 1/2 *Mass of the Car *v^2 kinetic energy
# The reaction time depends on how much kinetic energy that car holds
# if the energy is high, it will take long time to stop
# Speed :  Speed at that moment
# alpha : Is the default constant for proportionality

computerCarReactionTime <- function(Speed, alpha){
  timeToStop <- alpha * (Speed^2)
  return (timeToStop)
}


#Function to plot the graph for Flux (Cars per hour) against Speed
plotCarFluxVsSpeed <- function(CarDataFrame, CarVelocityArray, CarFluxArray, optimumCarSpeed, maxflux){
  p <- ggplot(data=CarDataFrame, aes(x=CarVelocityArray, y=CarFluxArray, group=1)) +
    geom_line(color="red")+
    geom_point() +
    geom_segment(aes(x=optimumCarSpeed, y=0, xend=optimumCarSpeed, yend=maxflux), color ="green", linetype=2) +
    geom_segment(aes(x=0, y=maxflux, xend=optimumCarSpeed, yend=maxflux),color ="green", linetype=2)
  p = p+xlab("Speed (mph)") + ylab("Flux (car density x mph), or Cars per hour")
  ggtitle("Road Efficency as a Function of Speed.") +
    theme(plot.title = element_text(family="Menlo-Bold", color = "black", size=14, face="bold", hjust=0.5))
  return(p)
}



fluxCal(0,120)


