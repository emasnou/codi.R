# Programa 1 Curs R-programming de Coursera.
# 

##############
# Funció padz.
##############
#' Afegir zeros a l'esquerra.
#' \code{padz} Afegeix zeros a l'esquerra d'una cadena
#' 
#' Aquesta funció afegeix zeros a l'esquerra d'una cadena fins que aquesta té 
#' el nombre indicat de zeros a l'esquerra
#' 
#' @param x Variable a completar
#' @param n Longitud que ha de tenir la cadena x. Per defecte la longitud de x
#' 
#' @return Una cadena amb la longitud demanada a n i amb 0 a l'esquerra.
#' 
padz <- function(x, n=max(nchar(x))) 
{
    gsub(" ", "0", formatC(x, width=n)) 
}

#######################
# Funció pollutantmean.
#######################
#' Calcula la mitjana de l'element de polució indicat.
#' \code{pollutantmean} Calcula la mitjana de l'element de pol·lució.
#' 
#' Aquesta funció calcula la mitjana del conjunt de dades que hi ha al directori.
#' Cal indicar el nom del directori on són les dades el component que es vol calcular
#' i les estacions d'on es vol calcular identificades per enters.
#' 
#' @param directory Cadena amb el nom del directori on hi ha les dades.
#' @param pollutant Cadena amb el nom del component. Pot ser: "nitrate" o "sulfate"
#' @param id Vector numeric amb els identificadors de les estacions a analitzar.
#' Per defecte les analitza totes.
#' 
#' @return La mitjana de les dades sense tenir en compte NA
#' 

pollutantmean <- function (directory, pollutant, id = 1:332)
{
    # Comprovació dels valors passats.
    # DIRECTORI
    if (! file.exists(directory))
        stop ("Error No existeix el directori: ", directory)
    # Pollutant
    if ( ( pollutant != "sulfate") & ( pollutant != "nitrate"))
        stop ("Error el contaminant introduit no és correcte: ", pollutant)
    # Comencem el procés.
    contaminant <- c()
    for (mesura in id)
    {
        taula <- read.table(file = paste(directory,"/",
                                         as.character(padz(mesura,3), length=3),
                                         ".csv", sep=""),
                            header = TRUE, sep=',')
        contaminant <- c(contaminant, taula[, eval(pollutant)])
    }
    mean(contaminant, na.rm=TRUE)
}

# La manera de fer-ho a la brava:

#        if (pollutant == "sulfate")
#        {
#            print(taula$sulfate)
#            contaminant <- c(contaminant,taula$sulfate)
#        }
#        else 
#        {
#            print (taula$nitrate)
#            contaminant <- c(contaminant,taula$nitrate)
#        }
