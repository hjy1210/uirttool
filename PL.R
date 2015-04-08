PL<-function (name, a, bk, g){
	pdf <- function(thetas) {
		f <- function(thetas) {
			g + (1 - g) * 1/(1 + exp(-a * thetas - ck))
		}
		num <- matrix(0, nrow = length(thetas), 2)
		num[, 2] <- f(thetas)
		num[, 1] <- 1 - num[, 2]
		num
		}
    # Baker, Item Response Theory: parameter estimation techniques 2004
    # page 75, table 3.2
    # use following three commented statements to test
    #item<-PL("V1",a=0.8,bk=0,g=0.2)
    #thetas<-seq(-3,3,by=0.5)
    #item$info(thetas)
    info<-function(thetas){
      p<-pdf(thetas)
      a*a*p[,1]/p[,2]*((p[,2]-g)/(1-g))^2
    }
    ck <- -bk[1] * a
	ncat <- 2
	res <- list(name = name, nfac = 1, itemtype = "pl", ncat = ncat, 
		a = a, ck = ck, bk = bk, g = g, pdf = pdf,info=info)
	class(res) <- "uirt"
	res
}
