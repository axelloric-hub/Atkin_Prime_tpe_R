#' Sieve of Atkin (List generation)
#'
#' @param n Upper limit for the sieve.
#' @returns A vector of prime numbers up to n.
#' @examples
#' atkin_list(50) # Returns 2, 3, 5, ..., 47
#' @export
atkin_list<- function(n){
  limit <- n
  sieve <- rep(FALSE,limit)
  lim <- floor(sqrt(limit))

  for (x in 1:lim) {
    x2 <- x*x
    for (y in 1:lim) {
      y2 <- y*y
      n_val <- 4*x2+y2
      if( n_val<=limit && (n_val%% 12== 1 || n_val%%12 ==5)){
        sieve[n_val] <- !sieve[n_val]
      }
      n_val <- 3*x2+y2
      if( n_val<=limit && (n_val%% 12== 7 )){
        sieve[n_val] <- !sieve[n_val]
      }
      if(x2>y2){
        n_val <- 3*x2-y2
        if( n_val<=limit && (n_val%% 12== 11 )){
          sieve[n_val] <- !sieve[n_val]
        }
      }
    }
  }

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

  primes <- c(2,3, which(sieve))
  primes <- primes[ primes<=limit]
  return(primes)
}
#' Sieve of Atkin (Find next prime)
#'
#' @param limit The upper bound for the search.
#' @returns The first prime number strictly greater than the limit.
#' @examples
#' atkin_prime(10) # Returns 11
#' atkin_prime(20) # Returns 23
#' @export
atkin_prime <- function(limit){
  # 1- Boolean array initialized to FALSE
  limite <- 2*limit
  sieve <- rep(FALSE, limite)
  lim <- floor(sqrt(limite))

  # 3 equations of Atkin
  for (x in 1:lim) {
    x2 <- x*x
    for (y in 1:lim) {
      y2 <- y*y

      n <- 4*x2+y2
      if( n<=limite && (n%% 12== 1 || n%%12 ==5)){
        sieve[n] <- !sieve[n]
      }
      n <- 3*x2+y2
      if( n<=limite && (n%% 12== 7 )){
        sieve[n] <- !sieve[n]
      }
      if(x2>y2){
        n <- 3*x2-y2
        if( n<=limite && (n%% 12== 11 )){
          sieve[n] <- !sieve[n]
        }
      }
    }
  }

  ## Elimination of square multiples
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

  # 5- Add 2 and 3, then collect TRUE values
  primes <- c(2,3, which(sieve))
  primes <- primes[ primes<=limite]

  for (a in 1:length(primes)) {
    if( primes[a]>limit){
      return(primes[a])
    }
  }
}

#' RSA Key Generation, Encryption, and Decryption
#'
#' @description
#' Simulates the RSA key generation step for medical data anonymization
#' using the Sieve of Atkin, then provides tools for encryption.
#'
#' @param n_atkin Minimum size for the prime number search.
#' @return A list containing the public/private keys and parameters p, q.
#'
#' @examples
#' # 1. Generate system keys
#' my_rsa <- rsa_full_process(1000)
#'
#' # 2. Encrypt a patient ID
#' anon_id <- rsa_encrypt(123, my_rsa$public_key)
#' print(anon_id)
#'
#' # 3. Decrypt to find original
#' original_id <- rsa_decrypt(anon_id, my_rsa$private_key)
#' print(as.character(original_id))
#'
#' @export
rsa_full_process <- function(n_atkin) {
  if (!requireNamespace("gmp", quietly = TRUE)) {
    stop("The 'gmp' package is required. Install it with install.packages('gmp')")
  }

  # --- 1. Sieve of Atkin logic ---
  primes <- atkin_list(n_atkin)

  if(length(primes) < 2) {
    stop("Sieve of Atkin did not find enough primes. Increase n_atkin.")
  }

  indices <- sample(1:length(primes), 2, replace = FALSE)
  p <- gmp::as.bigz(primes[indices[1]])
  q <- gmp::as.bigz(primes[indices[2]])

  # --- 2. RSA Calculations ---
  n <- p * q
  phi_n <- (p - 1) * (q - 1)

  e <- gmp::as.bigz(65537)
  if (gmp::gcd.bigz(e, phi_n) != 1) {
    e <- gmp::as.bigz(3)
    while (gmp::gcd.bigz(e, phi_n) != 1) { e <- e + 2 }
  }

  d <- gmp::inv.bigz(e, phi_n)

  message(paste("RSA Generation successful. Module n =", as.character(n)))

  return(list(
    p = p,
    q = q,
    public_key = list(e = e, n = n),
    private_key = list(d = d, n = n)
  ))
}

#' @rdname rsa_full_process
#' @param message_int An integer (or bigz) to encrypt.
#' @param pub_key Public key (list containing e and n).
#' @export
rsa_encrypt <- function(message_int, pub_key) {
  # Formula: c = m^e mod n
  gmp::powm(gmp::as.bigz(message_int), pub_key$e, pub_key$n)
}

#' @rdname rsa_full_process
#' @param cipher_int The encrypted integer.
#' @param priv_key Private key (list containing d and n).
#' @export
rsa_decrypt <- function(cipher_int, priv_key) {
  # Formula: m = c^d mod n
  gmp::powm(gmp::as.bigz(cipher_int), priv_key$d, priv_key$n)
}

#' Anonymize a column of IDs in a DataFrame
#'
#' @description
#' Applies RSA encryption to each row of a specific column.
#' Results are stored as strings to maintain precision.
#'
#' @param df The dataframe containing the data.
#' @param column_name Name of the column to encrypt (e.g., "ID").
#' @param pub_key Public key from rsa_full_process.
#' @return Dataframe with the transformed column.
#' @examples
#' data_med <- data.frame(ID = c(101, 102), Note = c("A", "B"))
#' keys <- rsa_full_process(1000)
#' data_med <- rsa_encrypt_column(data_med, "ID", keys$public_key)
#' @export
rsa_encrypt_column <- function(df, column_name, pub_key) {
  df[[column_name]] <- sapply(df[[column_name]], function(x) {
    res <- rsa_encrypt(x, pub_key)
    return(as.character(res))
  })
  return(df)
}

#' Decrypt a column of IDs in a DataFrame
#'
#' @description
#' Reverses the anonymization process to retrieve original IDs.
#'
#' @param df Dataframe with anonymized data.
#' @param column_name Name of the column to decrypt.
#' @param priv_key Private key from rsa_full_process.
#' @return Dataframe with the restored column.
#' @examples
#' # Assuming data_med is already encrypted
#' # data_med <- rsa_decrypt_column(data_med, "ID", keys$private_key)
#' @export
rsa_decrypt_column <- function(df, column_name, priv_key) {
  df[[column_name]] <- sapply(df[[column_name]], function(x) {
    res <- rsa_decrypt(gmp::as.bigz(x), priv_key)
    return(as.numeric(res))
  })
  return(df)
}
#' Encrypt a CSV file column using RSA
#'
#' @description
#' Reads a CSV file, encrypts a specific column (e.g., Patient IDs) using an RSA public key,
#' and saves the result to a new CSV file.
#'
#' @param chemin_1 Path to the input CSV file.
#' @param column_name Name of the column to encrypt.
#' @param chemin_2 Path where the encrypted CSV will be saved.
#' @param public_key The RSA public key from rsa_full_process.
#'
#' @examples
#' # keys <- rsa_full_process(1000)
#' # rsa_encode_csv("patients.csv", "ID", "patients_anon.csv", keys$public_key)
#' @export
rsa_encode_csv <- function(chemin_1, column_name, chemin_2, public_key) {
  # 1. Read the input file
  # Defaulting to semicolon separator as per your draft, adjust to "," if needed
  if (!file.exists(chemin_1)) stop("Input file does not exist.")
  data_med <- read.csv(chemin_1, sep = ";", stringsAsFactors = FALSE)

  # 2. Encrypt the specified column
  # Reuses the rsa_encrypt_column function defined earlier
  data_med <- rsa_encrypt_column(data_med, column_name, public_key)

  # 3. Write the encrypted file
  # row.names = FALSE prevents adding an unnecessary index column
  write.csv(data_med, chemin_2, row.names = FALSE)

  message(paste("Encrypted file successfully saved to:", chemin_2))
}

#' Decrypt a CSV file column using RSA
#'
#' @description
#' Reads an encrypted CSV file, decrypts a specific column using an RSA private key,
#' and saves the restored data to a new CSV file.
#'
#' @param chemin_1 Path to the encrypted input CSV file.
#' @param column_name Name of the column to decrypt.
#' @param chemin_2 Path where the decrypted CSV will be saved.
#' @param private_key The RSA private key from rsa_full_process.
#'
#' @examples
#' # keys <- rsa_full_process(1000)
#' # rsa_decrypt_csv("patients_anon.csv", "ID", "patients_restored.csv", keys$private_key)
#' @export
rsa_decrypt_csv <- function(chemin_1, column_name, chemin_2, private_key) {
  # 1. Read the encrypted file
  # Note: read.csv defaults to "," which matches write.csv output
  if (!file.exists(chemin_1)) stop("Input file does not exist.")
  data_med <- read.csv(chemin_1, stringsAsFactors = FALSE)

  # Check if the target column exists in the dataframe
  if (!(column_name %in% names(data_med))) {
    stop(paste("Column", column_name, "not found in the CSV file."))
  }

  # 2. Decrypt the specified column
  # Uses the rsa_decrypt_column function for the restoration process
  data_med <- rsa_decrypt_column(data_med, column_name, private_key)

  # 3. Write the restored file
  write.csv(data_med, chemin_2, row.names = FALSE)

  message(paste("Decrypted file successfully saved to:", chemin_2))
}




#' Check if a number is prime using Sieve of Atkin
#'
#' @param n The integer to check.
#' @returns Logical TRUE if prime, FALSE otherwise.
#' @examples
#' is_prime_atkin(17) # TRUE
#' is_prime_atkin(20) # FALSE
#' @export
is_prime_atkin <- function(n){
  if(n<2) return(FALSE)
  if(n==2 || n==3) return(TRUE)

  m <- n-1
  f <- atkin_prime(m)
  if(f == n) return(TRUE) else return(FALSE)
}

#' Prime Factorization using Sieve of Atkin
#'
#' @param n An integer greater than 1.
#' @returns A vector of prime factors.
#' @examples
#' prime_factors_atkin(100) # Returns 2, 2, 5, 5
#' @export
prime_factors_atkin <- function(n){
  if(is_prime_atkin(n)) return(n)

  factors <- c()
  remaining <- n
  limit <- floor(sqrt(n))+1
  liste <- atkin_list(limit)

  for( p in liste){
    while(remaining %% p == 0){
      factors <- c(factors, p)
      remaining <- remaining/p
    }
    if(remaining == 1) break
  }
  if (remaining > 1) factors <- c(factors, remaining)
  return(factors)
}

#' Create an Optimized Hash Table
#'
#' @param limit Upper limit for the table size; actual size will be the next prime > limit.
#' @returns A list containing the table, its size, and the hash function.
#' @examples
#' my_hash <- create_hash(100)
#' print(my_hash$size) # Next prime after 100
#' @export
create_hash <- function(limit){
  size <- atkin_prime(limit)
  table <- vector("list", size)
  hash_fun <- function(key){
    ( key %%  size)+1
  }
  list(table=table, size=size, hash=hash_fun)
}

#' Insert an element into the Hash Table
#'
#' @param h The hash table created with create_hash().
#' @param key Numeric key.
#' @param value The value to store.
#' @returns The updated hash table.
#' @examples
#' h <- create_hash(50)
#' h <- hash_insert(h, 123, "Patient Data")
#' @export
hash_insert <- function( h, key, value){
  index <- h$hash(key)
  h$table[[index]] <- value
  h
}

#' Retrieve an element from the Hash Table
#'
#' @param h The hash table.
#' @param key Numeric key.
#' @returns The stored value or NULL if not found.
#' @examples
#' h <- create_hash(50)
#' h <- hash_insert(h, 99, "Test Value")
#' hash_get(h, 99)
#' @export
hash_get <- function(h, key){
  h$table[[h$hash(key)]]
}
