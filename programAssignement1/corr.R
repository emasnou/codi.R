# Programa 2 Curs R-programming de Coursera.



#corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
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
# Funció corr.
#######################
#' Calcula la correlació entre els valors de nitrats i sulfats.
#' \code{corr} Calcula la correlació entre nitrats i sulfats.
#' 
#' Aquesta funció calcula la correlació entre nitrats i sulfats per a cadascuna 
#' de les estacions que compleixi tenir més observacions completes que el limit
#' marcat per la variable threshold.
#' 
#' @param directory Cadena amb el nom del directori on hi ha les dades.
#' @param threshold Integer amb el mínim de registres complets per fer el càlcul.
#' Per defecte el límit és 0.
#' 
#' @return un vector amb les correlacions.
#' 

corr <- function(directory, threshold = 0)
{
    # Comprovació dels valors passats.
    # DIRECTORI
    if (! file.exists(directory))
        stop ("Error No existeix el directori: ", directory)    
    # definim la taula que tornarem
    corr.final <- c()
    for ( estacio in 1:332)
    {
        taula.tmp <- read.table(file = paste(directory,"/",
                                         as.character(padz(estacio,3), length=3),
                                         ".csv", sep=""),
                            header = TRUE, sep=',')
        if ( sum(complete.cases(taula.tmp)) > threshold )
        {
            corr.final <- c(corr.final, 
                            cor(taula.tmp$nitrate, taula.tmp$sulfate,
                                use = "complete.obs"))
        }
    }
    corr.final
    
}

# La funció complete.cases(taula) retorna un vector logic amb TRUE o FALSE per 
# cadascuna de les línes si té totes les dades.
# Al fer la suma d'aquest vector, tenim el nombre de casos complets.
#     cor(x, y = NULL, use = "complete.obs",
#           method = c("pearson", "kendall", "spearman"))
