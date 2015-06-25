best <- function(state, outcome) {
    ## Read outcome data
    care_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")

    #create data frame with less crazy column names
    care_outcome <- care_outcome[,c(2,7,11,17,23)]
    names(care_outcome) <- c("Hospital.Name","State","Heart.Attack.Mortality","Heart.Failure.Mortality","Pneumonia.Mortality")

    #convert mortality columns to numbers
    care_outcome[,3:5] <- sapply(care_outcome[,3:5], as.character)
    care_outcome[,3:5] <- sapply(care_outcome[,3:5], as.numeric)
    
    ## Check that state and outcome are valid
    if ((sum(care_outcome$State == state) > 0) && (is.element(outcome, c("heart attack", "heart failure", "pneumonia")))) {

        state_outcome <- subset(care_outcome, care_outcome$State == state)
        ## Return hospital name ($Hospital.Name) in that state (#State) with lowest 30-day death
        ## rate

        ## check for heart attack
        if (outcome == "heart attack") {
            state_outcome$Hospital.Name[which(state_outcome$Heart.Attack.Mortality == min(state_outcome$Heart.Attack.Mortality, na.rm = TRUE))]            
            }
        
        ## check for heart failure
        else if (outcome == "heart failure") {
            state_outcome$Hospital.Name[which(state_outcome$Heart.Failure.Mortality == min(state_outcome$Heart.Failure.Mortality, na.rm = TRUE))]            
            #min(state_outcome$Heart.Failure.Mortality)
            
               }
        
        ## check for pneumonia
        else if (outcome == "pneumonia") {
            state_outcome$Hospital.Name[which(state_outcome$Pneumonia.Mortality == min(state_outcome$Pneumonia.Mortality, na.rm = TRUE))]
        }
        
    #check for bad state and outcomes
    }  else if (!(sum(care_outcome$State == state) > 0)) {
        stop("invalid state")
        
    }  else if  (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    } 
}