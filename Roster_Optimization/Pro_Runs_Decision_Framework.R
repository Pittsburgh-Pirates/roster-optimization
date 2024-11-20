# discount rate
pricePerRun <- 1.047
discount <- .1
discountVec <- (1-discount)^(1:3)

baseStott <- 7.7
baseMarsh <- 20.4
baseBohm <- 11.5

1.4*x - 1.36 = 11


# (Surplus_y + Cost_y)/WAR_y
(11 + 1.36)/1.4 # Gonzales
(18 + 2.52)/2 # Stott
(4 + 1.6)/.8 # Santana

payStott <- c(2.52, 4.17, 6.54)*3.5/2.52  # adjust multiplier 
payMarsh <- c(2.38, 3.67, 5.49)*3/2.38
payBohm <- c(5.46, 6.87)*8.1/5.46


# Raw pay
sum(payStott)
sum(payMarsh)
sum(payBohm)

# discounted pay
sum(payStott*discountVec)
sum(payMarsh*discountVec)
sum(payBohm*discountVec[1:length(payBohm)])

## IKF Factor
savingsIKF <- 7.5
tradeValIKF <- 5 # runs
IKFreplace <- savingsIKF/pricePerRun

## Joe Factor
savingsJoe <- 3.13
tradeValJoe <- 0
Joereplace <- savingsJoe/pricePerRun



# dollar savings
payBohm[1]*pricePerRun


### Stott Value ###
grossStott <- baseStott + IKFreplace + tradeValIKF
netStott <- grossStott - payStott[1]/pricePerRun

### Marsh Value ### 
grossMarsh <- baseMarsh + Joereplace + tradeValJoe
netMarsh <- grossMarsh - payMarsh[1]/pricePerRun

### Bohm Value ### 
grossBohm <- baseBohm + Joereplace + tradeValJoe
netBohm <- grossBohm - payBohm[1]/pricePerRun

