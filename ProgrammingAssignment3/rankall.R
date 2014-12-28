# Programa 3.2 Curs R-programming de Coursera.
# 

##############
# Funció rankhospital.
##############
#' Obtenir el ranking dels hospitals per Estat i malaltia.
#' 
#' 
#' \code{rankhospital} Obtenir el ranking dels hospitals per Estat i malaltia.
#' De les dades ubicades a l'arxiu data/outcome-of-care-measures.csv
#' obtenir el millor hospital de l'estat ( dades més baixes) per a les
#' malalties "heart attack", "heart failure", or pneumonia"
#' En cas d'empat, torna el primer ordenat alfabèticament.
#' "heart attack": Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
#' "Heart failure": Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
#' "pneumonia": Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
#' 
#' @param estat Estat del que volem el càlcul "XX"
#' @param malaltia cadena on indiquem la malaltia. 
#' "heart attack", "heart failure", or pneumonia"
#' @param rank Posició de l'hospital que volem consultar.
#' 
#' @return Nom de l'hospital classificat en la posició indicada ordenat alfabèticament.
#' 
#' 



rank_hospital <- function(estat, malaltia, num = "best") 
{
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    # Definim la llista d'estats valids
    llista.estats <- c( "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA",
                        "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA",
                        "MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY",
                        "NC","ND","OH","OK","OR","PA","PR","RI","SC","SD","TN",
                        "TX","UT","VT","VI","VA","WA","WV","WI","WY","GU")
    # Definim les malalties i les columnes on buscar les dades.
    malalties <- c("heart attack","heart failure","pneumonia")
    #    columnes <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    #                  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    #                  "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    columnes <- c(11,17,23)
    llista.malalties <- data.frame(malalties,columnes)
    # Nom de l'arxiu per carregar les dades:
    filename <- "data/outcome-of-care-measures.csv"
    
    # Comprovem que l'estat és correcte.
    if ( ! estat %in% llista.estats )
    {
        stop("invalid state")
    }
    if ( ! malaltia %in% malalties) 
    {
        stop ("invalid outcome")
    }
    
    # Busquem la columna de les dades en base a la malaltia.
    malaltia.columna <- subset(llista.malalties,llista.malalties$malalties == malaltia)
    #    message (" el camp a buscar per la malaltia és:", malaltia.columna$columnes)
    dades <- read.csv(filename,colClasses = "character")
    # Convertim les dades de les columnes que necessitem a numèric.
    dades[,malaltia.columna$columnes] <- suppressWarnings(as.numeric(dades[,malaltia.columna$columnes]))
    hospital.estat <- subset(dades,dades$State==estat)
    # Ordenem els resultats en funció de les dades per la malaltia i el nom de l'hospital    
    sort.millors <- hospital.estat[order(hospital.estat[,malaltia.columna$columnes],
                                         hospital.estat$Hospital.Name,na.last = TRUE),]
    # Gestionem la sortida que tornarem en funció del paràmetre num.
    # Si el num demanat és més gran que el nombre d'hospitals tornem NA
    # Si no hi ha num definit "best" tornem el primer
    # Si hi definim "worst" tornem el darrer.
    if ( num == "best" )
    {
        num <- 1
    }
    else if ( num == "worst")
    {
        num = nrow(sort.millors)
    }
    else if ( ! is.numeric(num) )
    {
        stop("invalid rank")
    }
    if ( num > nrow(sort.millors))
    {
        print ( NA )
    }
    else
    {
        sort.millors[num,2]
    }
}

##############
# Funció rankall.
##############
#' Obtenir el ranking dels hospitals per malaltia de tots els Estat.
#'  
#' 
#' \code{rankall} Obtenir el ranking dels hospitals per malaltia de tots els Estat.
#' De les dades ubicades a l'arxiu data/outcome-of-care-measures.csv
#' obtenir el millor hospital de l'estat ( dades més baixes) per a les
#' malalties "heart attack", "heart failure", or pneumonia"
#' En cas d'empat, torna el primer ordenat alfabèticament.
#' "heart attack": Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
#' "Heart failure": Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
#' "pneumonia": Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
#' 
#' @param malaltia cadena on indiquem la malaltia. 
#' "heart attack", "heart failure", or pneumonia"
#' @param rank Posició de l'hospital que volem consultar.
#' 
#' @return taula amb el nom de l'estat i l'hospital classificat en el lloc indicat.
#' 
#' 


rankall <- function ( malaltia,num="best")
{
    # Definim la llista d'estats valids
    llista.estats <- c( "AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA",
                        "GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME",
                        "MI","MN","MS","MO","MT","NC","ND","NE","NH","NJ","NM","NV","NY",
                        "OH","OK","OR","PA","PR","RI","SC","SD","TN",
                        "TX","UT","VA","VI","VT","WA","WI","WV","WY")
    # Definim les malalties i les columnes on buscar les dades.
    malalties <- c("heart attack","heart failure","pneumonia")
    #    columnes <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    #                  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    #                  "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    columnes <- c(11,17,23)
    llista.malalties <- data.frame(malalties,columnes)
    # Nom de l'arxiu per carregar les dades:
    filename <- "data/outcome-of-care-measures.csv"
    
    # Comprovem que els paràmetres són correctes.
    
    if ( ! malaltia %in% malalties) 
    {
        stop ("invalid outcome")
    }
    # Busquem la columna de les dades en base a la malaltia.
    malaltia.columna <- subset(llista.malalties,llista.malalties$malalties == malaltia)
    #    message (" el camp a buscar per la malaltia és:", malaltia.columna$columnes)
    dades <- read.csv(filename,colClasses = "character")
    # Convertim les dades de les columnes que necessitem a numèric.
    dades[,malaltia.columna$columnes] <- suppressWarnings(as.numeric(dades[,malaltia.columna$columnes]))
# Creem un data.frame buit per guardar les dades de sortida
#    sortida <- data.frame(matrix(vector(), 0, 2, 
#                                dimnames=list(c(), c("State", "Hospital"))),
#                         stringsAsFactors=F)
    sortida <- data.frame (hospital=character(),
                           state=character(), 
                           stringsAsFactors=FALSE
                           ) 
    for ( estat in llista.estats )
    {
        
        hospital.estat <- subset(dades,dades$State==estat)
        # Ordenem els resultats en funció de les dades per la malaltia i el nom de l'hospital    
        sort.millors <- hospital.estat[order(hospital.estat[,malaltia.columna$columnes],
                                             hospital.estat$Hospital.Name,na.last = TRUE),]
        # Gestionem la sortida que tornarem en funció del paràmetre num.
        # Si el num demanat és més gran que el nombre d'hospitals tornem NA
        # Si no hi ha num definit "best" tornem el primer
        # Si hi definim "worst" tornem el darrer.
        if ( num == "best" )
        {
            num <- 1
        }
        else if ( num == "worst")
        {
            num = nrow(sort.millors)
        }
        else if ( ! is.numeric(num) )
        {
            stop("invalid rank")
        }
        
        if ( num > nrow(sort.millors))
        {
            hospital <- c(NA)
        }
        else
        {
            hospital <-sort.millors[num,2]
        }
        new.row <- c(hospital=as.character(hospital),state=as.character(estat))
        sortida[estat,] <- new.row
# No sé el motiu però després del bind canvia els noms de les columnes i converteix
# les files en factors i falla tot. amb la següent commanda això deixa de passar.
#sortida <- rbind ( sortida, new.row )
# sortida[,c(1,2)] <- sapply(sortida[,c(1,2)],as.character) 
#        tmp
        #        print ( new.row )
        
    }

    sortida  

    
    
    
    
}