PL<-function (name, a, b, c,as=NULL){
	pdf <- function(thetas) {
		f <- function(thetas) {
			c + (1 - c) * 1/(1 + exp(-ak * thetas - ck))
		}
		num <- matrix(0, nrow = length(thetas), 2)
		num[, 2] <- f(thetas)
		num[, 1] <- 1 - num[, 2]
		num
		}
    # Baker, Item Response Theory: parameter estimation techniques 2004
    # page 75, table 3.2
    # use following three commented statements to test
    #item<-PL("V1",a=0.8,b=0,c=0.2)
    #thetas<-seq(-3,3,by=0.5)
    #item$info(thetas)
    info<-function(thetas){
      p<-pdf(thetas)
      ak*ak*p[,1]/p[,2]*((p[,2]-c)/(1-c))^2
    }
    if (is.null(as)) as<-1
    ak<-a*as
    ck <- -b * ak
	ncat <- 2
    res <- list(name = name, nfac = 1, itemtype = "pl", ncat = ncat, 
		a = a, ak=ak,ck = ck, bk = b, c = c,as=as, pdf = pdf,info2=info)
	class(res) <- "uirt"
	res
}
