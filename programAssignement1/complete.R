# Programa 2 Curs R-programming de Coursera.



#complete <- function(directory, id = 1:332) {
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
#}

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
# Funció complete.
#######################
#' Calcula el nombre de registres complets de les dades.
#' \code{complete} Calcula els registres complets.
#' 
#' Aquesta funció comprova el nombre de registres complets que tenim per a 
#' cadascuna de les estacions d'observació.
#' Un registre complet és aquell que no té cap NA.
#' 
#' @param directory Cadena amb el nom del directori on hi ha les dades.
#' @param id Vector numeric amb els identificadors de les estacions a analitzar.
#' Per defecte les analitza totes.
#' 
#' @return una taula amb l'identificador de l'estació i el nombre de registres complets
#' 

complete <- function(directory, id = 1:332)
{
    # Comprovació dels valors passats.
    # DIRECTORI
    if (! file.exists(directory))
        stop ("Error No existeix el directori: ", directory)    
    # definim la taula que tornarem
    taula.final <- data.frame()
    for ( estacio in id)
    {
        taula.tmp <- read.table(file = paste(directory,"/",
                                             as.character(padz(estacio,3), length=3),
                                             ".csv", sep=""),
                                header = TRUE, sep=',')
        taula.final <- rbind(taula.final,c(estacio, sum(complete.cases(taula.tmp))))
    }
    names(taula.final) <- c("id", "nobs")
    taula.final
}

# La funció complete.cases(taula) retorna un vector logic amb TRUE o FALSE per 
# cadascuna de les línes si té totes les dades.
# Al fer la suma d'aquest vector, tenim el nombre de casos complets.
