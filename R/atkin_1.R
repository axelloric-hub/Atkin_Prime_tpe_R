#'crible d'atkin
#'@param n Limite supérieure pour notre crible
#'@returns  La fonction va renvoyer le premier nombres premiers supérieurs à la limite supérieure
#'@export


atkin_prime <- function(limit){
  #1- tableau des booléens initialisé à False
  limite <- 2*limit
  sieve <- rep(FALSE,limite)
  # valeur limite de x et y
  lim <- floor(sqrt(limite))
  # étapes des " trois équations


  for (x in 1:lim) {
    x2 <- x*x
    for (y in 1:lim) {
      y2 <- y*y

      n <- 4*x2+y2
      if( n<=limite &&(n%% 12== 1 || n%%12 ==5)){

        sieve[n] <- !sieve[n]
      }
      n <- 3*x2+y2
      if( n<=limite &&(n%% 12== 7 )){

        sieve[n] <- !sieve[n]
      }
      if(x2>y2){
        n <- 3*x2-y2
        if( n<=limite &&(n%% 12== 11 )){

          sieve[n] <- !sieve[n]
        }
      }

    }

  }
  ## elimination des multiples des carrés
  k <- 1

  while( k*k <= limite){

    if(sieve[k]){
      j <- k*k
      while( j<=limite){
        sieve[j] <- FALSE
        j <- j+k*k
      }
    }
    k <- k+1
  }

  # 5-Ajouter 2 et 3 , puis récuperons les TRUE

  primes <- c(2,3, which(sieve))
  primes <- primes[ primes<=limite]
  alli <- primes[1]



  for (  a in 1:length(primes)) {
    if( primes[a]>limit){
      return(primes[a])

    }

  }







}








#' Créer une table de hachage optimisée avec un nombre premier
#' @param n limite supérieure pour la taille de la table, la taille finale sera le premier nombre premier > limit

#' @returns une liste contenant la table (liste), sa taille et la fonction de hachage
#' @export
create_hash <- function(limit){
  # fonction qui renvoie le premier nombre premier supérieur à la limite
  size <- atkin_prime(limit)
  table <- vector("list", size)
  #fonction de hachage simple
  hash_fun <- function(key){
   ( key %%  size)+1
  }
  list(

    table= table,
    size= size,
    hash =hash_fun
  )

}

#' Insérer un élément dans la table de hackage
#'
#' @param h la table de hackage créée avec create_hash()
#' @param key la clé numérique
#' @param value la valeur à stocker
#' @returns la table mise à jour
#' @export
hash_insert <- function( h, key, value){
  index <- h$hash(key)
  h$table[[index]] <- value
  h
}
#' Récupérer un élément dans la table de hackage
#'
#' @param h la table de hackage créée avec create_hash()
#' @param key la clé numérique
#' @returns La valeur stockée ou Null si non trouvée
#' @export
hash_get <- function(h, key){
  h$table[[h$hash(key)]]
}

#'Crible d'atkin
#'@param n Limite supérieure pour notre crible
#'@returns  La fonction va renvoyer la list des nombres premiers inférieurs à la limite
#'@export

atkin_list<- function(n){
  #1- tableau des booléens initialisé à False
  limit <- n
  sieve <- rep(FALSE,limit)
  # valeur limite de x et y
  lim <- floor(sqrt(limit))
  # étapes des " trois équations

  #4x²+y²
  for (x in 1:lim) {
    x2 <- x*x
    for (y in 1:lim) {
      y2 <- y*y

      n <- 4*x2+y2
      if( n<=limit &&(n%% 12== 1 || n%%12 ==5)){

        sieve[n] <- !sieve[n]
      }
      #3x²+y²
      n <- 3*x2+y2
      if( n<=limit &&(n%% 12== 7 )){

        sieve[n] <- !sieve[n]
      }
      if(x2>y2){
        #3x²-y²
        n <- 3*x2-y2
        if( n<=limit &&(n%% 12== 11 )){

          sieve[n] <- !sieve[n]
        }
      }

    }

  }
  #elimination des multiples des carrés
  k <- 1
  while( k*k <= limit){

    if(sieve[k]){
      j <- k*k
      while( j<=limit){
        sieve[j] <- FALSE
        j <- j+k*k
      }
    }
    k <- k+1
  }

  # 5-Ajouter 2 et 3 , puis récuperons les TRUE

  primes <- c(2,3, which(sieve))
  primes <- primes[ primes<=limit]

  return(primes)



}

#'Crible d'atkin
#'@param n Limite supérieure pour notre crible
#'@returns  True si le nombre est premier
#'@export
is_prime_atkin <- function(n){
  if(n<2){
    return(FALSE)
  }
  if(n==2 || n==3){
    return(TRUE)
  }

  m <- n-1
  f <-  atkin_prime(m) # on utilise la fonction atkin_prime qui renvoie directement le premier nombres premiers supérieures
  if(f==n){
    return(TRUE)
  }else{
    return(FALSE)
  }

}
#'Crible d'atkin
#'@param n un entier supérieur à 1
#'@returns  un vecteur des facteurs premiers de N
#'@export
prime_factors_atkin <- function(n){
  if(is_prime_atkin(n) ){
    return("c est un nombre premier")

  }
  factors <- c()
  remaining <- n
  #Générer tous les nombres premiers jusqu'à sqrt(n)
  limit <- floor(sqrt(n))+1
  liste <- atkin_list(limit)
  for( p in liste){
    while(remaining %% p ==0){
      factors <- c(factors,p)
      remaining <- remaining/p
    }
    if(remaining==1) break
  }
  if (remaining> 1){
    factors <- c(factors, remaining)


  return(factors)


  }


  }


#'Genere les paramètres d'une clé RSA Simulée
#'Simule l'étape de generation de cle RSA pour l 'anonymisation des données médicales
#'En utilisant le crible d'atkin pour trouver les nombres  premiers p et q
#'@param n Taille minimale que p et q doivent atteindre
#'@returns une liste contenant les paramètres (p,q et n)
#'@export
pam_rsa_atkin <- function(n){
  #on liste les nombres premiers
  premiers <- atkin_list(n)
  if(length(premiers)<2){
    return("crible d'atkin n a pas pu trouver assez de nombre premiers veuillez augmenter la taille de N")
  }
  #selectionne deux indices distincts
  indices <- sample(1:length(premiers),2,replace = FALSE)
  p <- premiers[indices[1]]
  q <- premiers[indices[2]]
  # calcul du module n
  # ce module est la cle publique qui assure la securite des données

  n <- p*q

  message(paste( "generation simulee de la base de la cle RSA reussie:","p=",p,";q=",q,"Module n(cle publique)=",n))
  return(list(p=p,q=q,module_n=n))
}

