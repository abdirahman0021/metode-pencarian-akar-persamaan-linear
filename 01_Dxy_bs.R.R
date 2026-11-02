bisection <- function(f, a, b, tol = 1e-6, max_iter = 100){
	
	if (f(a) * f(b) > 0){
		stop ("interval tidak mengandung perubahan tanda.")
	}
	hasil <- data.frame()
	
	for(i in 1:max_iter){
		
		c <- (a+b)/2
		fc <- f(c)
		
		
		hasil <- rbind (
		hasil,
		data.frame(
			iterasi = i,
			a=a,
			b=b,
			c=c,
			f_c=fc,
			error=abs(b-a)
		)
	)
	
	if (abs(b-a) < tol || abs(fc) < tol){
		break
	}
	
	if (f(a)* fc < 0){
		b<-c
		} else {
			a <- c
	}
	}
	
	list (
		akar = c,
		iterasi = hasil
	)
}



f <- function (x){
	x^3 - x - 2
}

hasil_bisection <- bisection(
f=f,
a=1,
b=2
)

hasil_bisection$akar