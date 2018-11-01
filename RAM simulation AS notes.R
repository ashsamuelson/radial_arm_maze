#RAM SIMULATIONS 

#CHANCE
#This runs a simulation of 100000 trials in the RAM, assuming a bee visits flowers at random (ie no stereotypical behaviour or spatial memory)
totvbfe<-numeric(0)  #sets up empty vectors to add to after each loop
toterrors<-numeric(0)
totcaperrors<-numeric(0)
totcorr8<-numeric(0)
iteration<-numeric(0)

for (j in 1:100000){   #select how many iterations of the simulation 

  flowers<-1:8  #makes a vector of flower numbers from 1 to 8 to sample from
  choices<-numeric(0)  # empty vectors
  complete<-numeric(0)
  errors<-0
  vbfe<-0
  correctfirst8<-0
  cap.errors<-0

    for (i in 1:100){   # this loop is one trial of a single bee, looping through visits to flowers
      ch<-sample(flowers, 1, replace = T)  #sample one of the flowers, with replacement 
      if(ch %in% choices) errors<-errors+1  # if the sampled flower is already present in the vector of previous choices, add one to the errors total
      if(errors == 1 & vbfe==0) vbfe<- i-1  #if there has been an error, and visits before first error (vbfe) has not been assigned already, assign 
 #                                           current visit number - 1 to vbfe
      if(i==8) correctfirst8 <- 8- errors   #if we are on visit number 8, assign (8 - errors) i.e. correct choices to correct choices in first 8 
      if(i==18) cap.errors<-errors          #if we are on visit 18, store the number of errors (see RAM paper for explanation for capping)
      choices<-append(choices, ch)          # add current choice to vector of choices
      complete<-flowers %in% choices *1     # make a vector of which flowers have been visited in the choices vector
      if (sum(complete)==8) break           # if 8 have been visited, stop the loop
    }
    if (cap.errors==0) cap.errors<-errors   #if cap.errors hasn't been assigned (ie if the loop stopped before 18), store errors as cap.errors

  totvbfe<-append(totvbfe, vbfe)            #add the results from this bee to the vectors of totals
  toterrors<-append(toterrors, errors)
  totcaperrors<-append(totcaperrors, cap.errors)
  totcorr8<-append(totcorr8, correctfirst8)
  iteration<-append(iteration, j)       #store the iteration number

}


sim.performance<-data.frame(iteration=iteration, visits.before.error=totvbfe, correct.first.8 = totcorr8,  
                            total.revisits = toterrors, capped.revisits =totcaperrors)  #make a dataframe of results
summary(sim.performance)


#STEREOTYPICAL BEHAVIOUR
#This runs the same simulation, but changes the probability of the bee selecting a flower based on her previous choice, using the 
#transition probabilities from observed bees on the RAM (I used bouts from bees that had been fully trained on the RAM).
#This means that stereotypical behaviour (e.g. "nearest neighbour" preference) is included in the simulation, but still not spatial memory.


#load csv of transition probability matrix. Start flowers should be ordered according to rownumber, 
#e.g. row 2 has the probabilities of transitioning from flower 2 to flowers 1 to 8. row 9 is the probabilities from leaving the nest (ie first visit)
trans.matrix<-as.matrix(example_TPM) 

totvbfe<-numeric(0)
toterrors<-numeric(0)
totcaperrors<-numeric(0)
totcorr8<-numeric(0)
iteration<-numeric(0)

for (j in 1:10000){
  
  flowers<-1:8
  choices<-numeric(0)
  complete<-numeric(0)
  errors<-0
  vbfe<-0
  correctfirst8<-0
  cap.errors<-0
  for (i in 1:100){
    ifelse(sum(choices)==0, last<-9, last<-choices[length(choices)])#designate the last flower choice as "last", unless the first choice (assign 9 for tunnel)
    probs<-as.numeric(trans.matrix[last,]) #select the row of the matrix from the last flower number 
   #                                                 (this is a vector of probabilities of moving to subsequent flowers)
    ch<-sample(flowers, 1, replace = T, prob=probs)    #sample a flower according to those probabilities
    if(ch %in% choices) errors<-errors+1
    if(errors == 1 & vbfe==0) vbfe<- i-1
    if(i==8) correctfirst8 <- 8- errors
    if(i==18) cap.errors<-errors
    choices<-append(choices, ch)
    complete<-flowers %in% choices *1 
    if (sum(complete)==8) break
  }
  if (cap.errors==0) cap.errors<-errors
  
  totvbfe<-append(totvbfe, vbfe)
  toterrors<-append(toterrors, errors)
  totcaperrors<-append(totcaperrors, cap.errors)
  totcorr8<-append(totcorr8, correctfirst8)
  iteration<-append(iteration, j)
  
}


sim.performance<-data.frame(iteration=iteration, visits.before.error=totvbfe, correct.first.8 = totcorr8, 
                            total.revisits = toterrors, capped.revisits =totcaperrors)
summary(sim.performance)


