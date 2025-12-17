#'crible d'atkinit
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

#' Génération de clés, Chiffrement et Déchiffrement RSA
#'
#' @description
#' Simule l'étape de génération de clé RSA pour l'anonymisation des données médicales
#' en utilisant le crible d'Atkin, puis fournit les outils de cryptage.
#'
#' @param n_atkin Taille minimale pour la recherche des nombres premiers.
#' @return Une liste contenant les clés et les paramètres p, q.
#'
#' @examples
#' # 1. Générer le système
#' mon_rsa <- rsa_full_process(1000)
#'
#' # 2. Chiffrer un identifiant patient
#' id_anonyme <- rsa_encrypt(123, mon_rsa$public_key)
#' print(id_anonyme)
#'
#' # 3. Déchiffrer pour retrouver l'original
#' id_origine <- rsa_decrypt(id_anonyme, mon_rsa$private_key)
#' print(as.character(id_origine))
#'
#' @export
rsa_full_process <- function(n_atkin) {
  if (!requireNamespace("gmp", quietly = TRUE)) {
    stop("Le package 'gmp' est nécessaire. Installez-le avec install.packages('gmp')")
  }

  # --- 1. Utilisation de votre logique (Crible d'Atkin) ---
  premiers <- atkin_list(n_atkin) # Appel de votre fonction existante

  if(length(premiers) < 2) {
    stop("Le crible d'Atkin n'a pas trouvé assez de nombres premiers. Augmentez n_atkin.")
  }

  indices <- sample(1:length(premiers), 2, replace = FALSE)
  p <- gmp::as.bigz(premiers[indices[1]])
  q <- gmp::as.bigz(premiers[indices[2]])

  # --- 2. Calculs RSA ---
  # Module n
  n <- p * q

  # Indicateur d'Euler phi(n) = (p-1)(q-1)
  phi_n <- (p - 1) * (q - 1)

  # Choix de l'exposant public e (65537 est le standard)
  e <- gmp::as.bigz(65537)
  if (gmp::gcd.bigz(e, phi_n) != 1) {
    e <- gmp::as.bigz(3)
    while (gmp::gcd.bigz(e, phi_n) != 1) { e <- e + 2 }
  }

  # Calcul de l'exposant privé d (inverse modulaire)
  d <- gmp::inv.bigz(e, phi_n)

  message(paste("Génération RSA réussie. Module n =", as.character(n)))

  return(list(
    p = p,
    q = q,
    public_key = list(e = e, n = n),
    private_key = list(d = d, n = n)
  ))
}

#' @rdname rsa_full_process
#' @param message_int Un entier (ou bigz) à chiffrer
#' @param pub_key La clé publique (liste e, n)
#' @export
rsa_encrypt <- function(message_int, pub_key) {
  # Formule : c = m^e mod n
  gmp::powm(gmp::as.bigz(message_int), pub_key$e, pub_key$n)
}

#' @rdname rsa_full_process
#' @param cipher_int L'entier chiffré
#' @param priv_key La clé privée (liste d, n)
#' @export
rsa_decrypt <- function(cipher_int, priv_key) {
  # Formule : m = c^d mod n
  gmp::powm(gmp::as.bigz(cipher_int), priv_key$d, priv_key$n)
}

#'Crible d'atkin
#'@param n Limite supérieure pour notre crible
#'@returns  La fonction va renvoyer la list des nombres premiers inférieurs à la limite
#'@examples
#' atkin_list(50) # Renvoie 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47
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
#' @examples
#' is_prime_atkin(17) # TRUE
#' is_prime_atkin(20) # FALSE
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
#' @examples
#' prime_factors_atkin(100) # Renvoie 2, 2, 5, 5
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






#' Créer une table de hachage optimisée avec un nombre premier
#' @param n limite supérieure pour la taille de la table, la taille finale sera le premier nombre premier > limit

#' @returns une liste contenant la table (liste), sa taille et la fonction de hachage
#' @examples
#' my_hash <- create_hash(100)
#' print(my_hash$size) # Affiche le premier nombre premier > 100
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
#' @examples
#' h <- create_hash(50)
#' h <- hash_insert(h, 12345, "Donnée Patient A")
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
#' @examples
#' h <- create_hash(50)
#' h <- hash_insert(h, 99, "Test")
#' hash_get(h, 99)
#' @export
hash_get <- function(h, key){
  h$table[[h$hash(key)]]
}




