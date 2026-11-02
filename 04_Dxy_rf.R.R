# 1. Definisi Fungsi f(x)
f <- function(x) {
  x^3 - x - 2
}

# 2. Fungsi Metode Regula Falsi
regula_falsi <- function(f, a, b, 
                         tol = 1e-6, 
                         max_iter = 100) {
  
  if (f(a) * f(b) > 0) {
    stop("Tidak ada perubahan tanda pada interval [a,b].")
  }
  
  hasil <- data.frame(
    iterasi = integer(),
    a = numeric(),
    b = numeric(),
    c = numeric(),
    f_c = numeric(),
    error = numeric()
  )
  
  c_old <- NA
  
  for (i in 1:max_iter) {
    
    fa <- f(a)
    fb <- f(b)
    
    c <- (a * fb - b * fa) / (fb - fa)
    fc <- f(c)
    
    if (is.na(c_old)) {
      error <- NA
    } else {
      error <- abs(c - c_old)
    }
    
    hasil <- rbind(
      hasil,
      data.frame(
        iterasi = i,
        a = a,
        b = b,
        c = c,
        f_c = fc,
        error = error
      )
    )
    
    if (abs(fc) < tol || (!is.na(error) && error < tol)) {
      break
    }
    
    if (fa * fc < 0) {
      b <- c
    } else {
      a <- c
    }
    
    c_old <- c
  }
  
  list(
    akar = c,
    iterasi = i,
    tabel = hasil
  )
}

# 3. Eksekusi dengan interval a = 1 dan b = 2
hasil_regula <- regula_falsi(
  f = f,
  a = 1,
  b = 2,
  tol = 1e-6
)

# Menampilkan nilai akar
hasil_regula$akar

# Menampilkan tabel iterasi
hasil_regula$tabel