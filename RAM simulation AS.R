#CHANCE
totvbfe<-numeric(0)
toterrors<-numeric(0)
totcaperrors<-numeric(0)
totcorr8<-numeric(0)
iteration<-numeric(0)

for (j in 1:100000){

flowers<-1:8
choices<-numeric(0)
complete<-numeric(0)
errors<-0
vbfe<-0
correctfirst8<-0
cap.errors<-0
for (i in 1:50){
  ch<-sample(flowers, 1, replace = T)
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


#STEREOTYPICAL BEHAVIOUR

#load csv of transition probability matrix. Start flowers are ordered according to rownumber, 
#e.g. row 2 has the probabilities of transitioning from flower 2 to flowers 1 to 8. row 9 is the probabilities from leaving the nest (ie first flower)
trans.matrix<-as.matrix(example_TPM) 

totvbfe<-numeric(0)
toterrors<-numeric(0)
totcaperrors<-numeric(0)
totcorr8<-numeric(0)
iteration<-numeric(0)

for (j in 1:100000){
  
  flowers<-1:8
  choices<-numeric(0)
  complete<-numeric(0)
  errors<-0
  vbfe<-0
  correctfirst8<-0
  cap.errors<-0
  for (i in 1:100){
    ifelse(sum(choices)==0, last<-9, last<-choices[length(choices)]) #9 refers to tunnel
    probs<-as.numeric(trans.matrix[last,])
    ch<-sample(flowers, 1, replace = T, prob=probs)
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



