# Programa 3.1 Curs R-programming de Coursera.
# 

##############
# Funció best.
##############
#' Obtenir el millor hospital per Estat i malaltia.
#' 
#' Afegir zeros a l'esquerra.
#' \code{padz} Afegeix zeros a l'esquerra d'una cadena
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
#' 
#' @return Nom del millor hospital ordenat alfabèticament.
#' 
#' 
best <- function ( estat,malaltia)
{
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
    hospitals.millors <- subset(hospital.estat,
                                hospital.estat[,malaltia.columna$columnes]==min(hospital.estat[,malaltia.columna$columnes],na.rm=TRUE))
    
    sort.millors <- hospitals.millors[order(hospitals.millors$Hospital.Name),]
    sort.millors[1,2]
    #sort.millors
}