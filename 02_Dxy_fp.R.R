# 1. Definisi Fungsi Iterasi g(x) dari f(x) = x^3 - x - 2
g <- function(x) {
  (x + 2)^(1/3)
}

# 2. Fungsi Metode Fixed-Point Iteration
fixed_point <- function(g, x0, 
                        tol = 1e-6, 
                        max_iter = 100) {
  
  hasil <- data.frame(
    iterasi = integer(),
    x_n = numeric(),
    x_next = numeric(),
    error = numeric()
  )
  
  x <- x0
  
  for (i in 1:max_iter) {
    
    x_next <- g(x)
    
    error <- abs(x_next - x)
    
    hasil <- rbind(
      hasil,
      data.frame(
        iterasi = i,
        x_n = x,
        x_next = x_next,
        error = error
      )
    )
    
    if (error < tol) {
      break
    }
    
    x <- x_next
  }
  
  list(
    akar = x_next,
    iterasi = i,
    tabel = hasil
  )
}

# 3. Eksekusi dengan tebakan awal x0 = 1.5
hasil_fixed <- fixed_point(
  g = g,
  x0 = 1.5,
  tol = 1e-6
)

# Menampilkan nilai akar
hasil_fixed$akar

# Menampilkan tabel iterasi
hasil_fixed$tabel