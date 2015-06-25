rankall <- function(outcome, num = "best") {
    ## Read outcome data
    care_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    #creates an data frame
    ranking <- data.frame() 
    
    ## Check that the  outcome is valid
    if ((is.element(outcome, c("heart attack", "heart failure", "pneumonia")))) {
        
    ## For each state, find the hospital of the given rank

        ## find all of the unique states
        states <- unique(care_outcome$State)
        
        ## alphabetize the states
        states <- states[order(states)]
        
        #set decreasing bool
        decreasing_bool <- FALSE;
        
        ## if worst is the num argument, reverse sort and pick top
        if (num == "worst") {
            decreasing_bool <- TRUE;
            num <- 1;
        } 
        
        ## if best is used for argument, pick the top one
        if (num == "best") {
            num <- 1;
        }
        
        ## loop through each state and find the matching outcome and ranking
        for (state in states) {
            
            ## create state subset 
            state_outcome <- subset(care_outcome, care_outcome$State == state)
            
            #create data frame with less crazy column names
            state_outcome <- state_outcome[,c(2,7,11,17,23)]
            names(state_outcome) <- c("Hospital.Name","State","Heart.Attack.Mortality","Heart.Failure.Mortality","Pneumonia.Mortality")
            
            #convert mortality columns to numbers
            state_outcome[,3:5] <- sapply(state_outcome[,3:5], as.character)
            state_outcome[,3:5] <- sapply(state_outcome[,3:5], as.numeric)
            
            
            ## check for heart attack
            if (outcome == "heart attack") {
                heart_attack_sort <- state_outcome[order(state_outcome$Heart.Attack.Mortality, state_outcome$Hospital.Name, na.last = TRUE, decreasing = decreasing_bool),] 
                hospital <- heart_attack_sort[num,1]
                }
            
            ## check for heart failure
            else if (outcome == "heart failure") {
                heart_failure_sort <- state_outcome[order(state_outcome$Heart.Failure.Mortality, state_outcome$Hospital.Name, na.last = TRUE, decreasing = decreasing_bool),] 
                hospital <- heart_failure_sort[num,1] 
            }
            
            ## check for pneumonia
            else if (outcome == "pneumonia") {
                pneumonia_sort <- state_outcome[order(state_outcome$Pneumonia.Mortality, state_outcome$Hospital.Name, na.last = TRUE, decreasing = decreasing_bool),] 
                hospital <- pneumonia_sort[num,1]
            }
            
            ## create new data frame row and add to data frame
            row.ranking <- data.frame(hospital, state)
            ranking <- rbind(ranking, row.ranking) #TODO: Identify why WORST isn't working  
        }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
        return(ranking)
    }
}