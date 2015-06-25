rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    care_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    ## Check that state and outcome are valid
    if (!(sum(care_outcome$State == state) > 0)) {
        stop("invalid state")
    } 
    if  (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    } 

    ## Check that state and outcome are valid
    if ((sum(care_outcome$State == state) > 0) && (is.element(outcome, c("heart attack", "heart failure", "pneumonia")))) {
       
        ## create state subset 
        state_outcome <- subset(care_outcome, care_outcome$State == state)
        
        #create data frame with less crazy column names
        state_outcome <- state_outcome[,c(2,7,11,17,23)]
        names(state_outcome) <- c("Hospital.Name","State","Heart.Attack.Mortality","Heart.Failure.Mortality","Pneumonia.Mortality")
        
        #convert mortality columns to numbers
        state_outcome[,3:5] <- sapply(state_outcome[,3:5], as.character)
        state_outcome[,3:5] <- sapply(state_outcome[,3:5], as.numeric)
        
        #set decreasing bool
        decreasing_bool <- FALSE;
        
        ##TODO: ADD BEST AND WORST
         ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        ## if best is used for argument, pick the top one
        if (num == "best") {
            num <- 1;
        }
       
        ## if worst is the num argument, reverse sort and pick top
         if (num == "worst") {
            decreasing_bool <- TRUE;
             num <- 1;
         } 
        
        ## check for heart attack
        if (outcome == "heart attack") {
            heart_attack_sort <- state_outcome[order(state_outcome$Heart.Attack.Mortality, state_outcome$Hospital.Name, na.last = TRUE, decreasing = decreasing_bool),] 
            heart_attack_sort[num,1]
        }
        
        ## check for heart failure
        else if (outcome == "heart failure") {
            heart_failure_sort <- state_outcome[order(state_outcome$Heart.Failure.Mortality, state_outcome$Hospital.Name, na.last = TRUE, decreasing = decreasing_bool),] 
            heart_failure_sort[num,1] 
        }
        
        ## check for pneumonia
        else if (outcome == "pneumonia") {
            pneumonia_sort <- state_outcome[order(state_outcome$Pneumonia.Mortality, state_outcome$Hospital.Name, na.last = TRUE, decreasing = decreasing_bool),] 
            pneumonia_sort[num,1]
            }
        
    }
}