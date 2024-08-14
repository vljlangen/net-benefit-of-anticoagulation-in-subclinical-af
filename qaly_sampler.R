qaly_sampler <- function(rate = NULL, af_rate = NULL, policy = NULL, severity = NULL, qaly = NULL, months = 120, size = 1000, base_qaly = NULL, initial_state = NULL, seed = NULL, end.NOAC.after.bleeding = FALSE) {
  #rate: input rates from which the sample is taken
  #af_rate: input rates from which the clinical flimmer patients samples are taken
  #policy: the policy to medicate patients
  #severity: proportions of different severities of the outcome
  #qaly: quality adjusted life years for different observed outcomes
  #months: maximum time in months to simulate
  #size: number of simulations
  #base_qaly: quality adjusted life without any outcomes
  #initial_state: the initial health state to start the simulation
  #seed: integer seed for RNG
  states <- 1:(dim(rate)[1]) # get health states
  af_states <- 1:(length(af_rate)) #get clinical af health states
  actions <- 1:(dim(policy)[1])# get actions
  observed_state <- initial_state
  Time <- 1:months #timeline to simulate over to
  output <- list() #temporal working variable
  death_rates_coefficients_after_disability <- c(rate[1,1], rate[1,1], -log(1-0.14)/12, -log(1-0.16)/12, -log(1-0.16)/12, rate[1,1], rate[1,1]) / rate[1,1]
  severity <- array(c(
    cbind( matrix(c(1,0,0,0,0,0,0,0,0,1), ncol = 2), severity[,,1]),
    cbind(matrix(c(1,0,0,0,0,0,0,0,0,1), ncol = 2), severity[,,2])
  ),
  dim = c(5,6,2),
  )
  #---------------simulation loop starts--------------------
  #loop for subclinical atrial flimmer
  #set seed for reproducibility
  cores <- detectCores()
  cl <- makeCluster(cores[1])
  registerDoParallel(cl)
  set.seed(seed)
  output <- foreach (i = 1:size) %dorng% {
    observed_state <- initial_state
    observation <- data.frame(state = observed_state, morbidity = 5, qalm = base_qaly[1]) #observed state, disability (5 is no disability), base qaly 0.794
    af_diagnosis <- FALSE
    lifeLine <- rep(1,months)
    probLine <- array(rep(rate,months), dim = c(dim(rate), months)) #time dependent probabilities
    for (n in 1:months){
      action <- sample(actions, 1, prob = policy[,observed_state])
      observed_state <- sample(states, 1, prob = probLine[, action, n]) # the next observed state
      #Clinical atrial flimmer
      if (observed_state == 7) {
        af_diagnosis <- TRUE
        lifeLine[n] <- 1*lifeLine[n]
        morbidity <- 5
      }
      else {
        morbidity <- sample(1:5, 1, prob = severity[,observed_state,action]) # random morbidity from observed health state
        lifeLine[n:(n+5)] <- qaly[morbidity, 1,n]*lifeLine[n:(n+5)] #qaly until 6 months after event
        lifeLine[(n+6):months] <- qaly[morbidity,2,n]*lifeLine[(n+6):months] #qaly from 6 months after event
        #geometrically increase death rate for a year after the disability
        temprate <- 1-colSums(rbind( probLine[1,1,n]*death_rates_coefficients_after_disability[observed_state], rate[3:7,] ))
        probLine[c(1,2),,(n):min((n+12), months)] <- rbind(Death = probLine[1,1,n]*death_rates_coefficients_after_disability[observed_state], Susceptible = temprate) #increased base rate to die for a year after event
        if (end.NOAC.after.bleeding & observed_state %in% c(4,5)) {
          #end medication if hemorrhagic stroke or other intracranial bleeding
          policy <- array(c(1, 0,
                            1, 0,
                            1, 0,
                            1, 0,
                            1, 0,
                            1, 0,
                            0, 1),
                          dim = c(2,7),
                          dimnames = list(c("No NOAC", "NOAC"),
                                          c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation")
                          ))
        }
        if (end.NOAC.after.bleeding & observed_state == 3) {
          #start medication if stroke
          policy <- array(c(0, 1,
                            0, 1,
                            0, 1,
                            1, 0,
                            1, 0,
                            1, 0,
                            0, 1),
                          dim = c(2,7),
                          dimnames = list(c("No NOAC", "NOAC"),
                                          c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation")
                          ))
        }
      }
      #build the sample path
      observation <- rbind(observation, c(observed_state, morbidity, lifeLine[n]*base_qaly[n]))
      #death by stroke or bleed or accident breaks the loop
      if (lifeLine[n] == 0 | af_diagnosis) {break}
    }
    #loop for clinical atrial flimmer if diagnosed
    if (af_diagnosis & n!=months){
      #af_states <- 1:length(af_rate)
      action <- 2 #choose to medicate always
      af_probLine <- array(rep(af_rate, months - n), dim = c(length(af_rate), months - n))
      for (t in (n+1):months){
        observed_state <- sample(1:6, 1, prob = af_probLine[, t-n]) # the next observed state
        morbidity <- sample(1:5, 1, prob = severity[,observed_state,action]) # random morbidity from observed health state
        lifeLine[t:(t+5)] <- qaly[morbidity, 1,t]*lifeLine[t:(t+5)] #qaly until 6 months after event
        lifeLine[(t+6):months] <- qaly[morbidity,2,t]*lifeLine[(t+6):months] #qaly from 6 months after event
        #geometrically increase death rate for a year after the disability
        temprate <- 1-sum(c( af_probLine[1,t-n]*death_rates_coefficients_after_disability[observed_state], rate[3:7,] ))
        af_probLine[c(1,2),(t-n):min((t+12-n), months-n)] <- c(Death = af_probLine[1,t-n]*death_rates_coefficients_after_disability[observed_state], Susceptible = temprate) #increased base rate to die for a year after event
        #build the sample path
        observation <- rbind(observation, c(observed_state, morbidity, lifeLine[t]*base_qaly[t]))
        #death by stroke or bleed or accident breaks the loop
        if (lifeLine[t] == 0) {break}
      }
    }
    observation <- cbind(observation, cumsum(observation[,3]))
    try(colnames(observation) <- c("Observation", "Morbidity", "QALM", "Cumulative QALM"))
    observation
  }
  #------------------------simulation loop ends---------------------------
  stopCluster(cl)
  return(output)
}