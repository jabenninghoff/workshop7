# nolint start

###############################################
# Invoke R by double clicking its desktop icon
# Enter rm(list = ls()) to clear R's memory
# Click File / Source R code on the main menu
# Locate and Open the file sra.r
###############################################


# slide 22 of "NoData 2019 Arlington.ppt"
normal(3,1)
uniform(0,6)
exponential(1)
N(3,1)


# 23
binomial(7,.4)


# 30
a = N(50,13)
a
cut.mc(a, 0.67)     # cut <- function(x,s,...) {UseMethod("fractile")}; cut(a, 0.67)


# 35
X = normal(5,1)
Y = triangular(1,2,4)
#plot.mclist(X, Y, X + Y)
plot.mclist(c(X,Y,X+Y))


# 39
L = U(80, 120) 		    
i = U(0.0003, 0.0008)	
K = lognormal(1000, 750)	    # 'cause we just clobbered L
n = lognormal(0.25, 0.05)  	    
BD = lognormal(1650, 100)
foc = U(0.0001, 0.005)
Koc = N(10, 3)
T = (n + BD * foc * Koc) * L / (K * i) 
summary(T)


# 40
L = uniform(80, 120) 	
i = uniform(0.0003, 0.0008)
K = lognormal(1000, 750)
K = truncate(K, 300, 3000)
n = lognormal(0.25, 0.05)
n = truncate(n, 0.2, 0.35)
BD = lognormal(1650, 100)
BD = truncate(BD, 1500, 1750)
foc = uniform(0.0001, 0.005)
Koc = normal(10, 3)
Koc = truncate(Koc, 5, 20)
T = (n + BD * foc * Koc) * L / (K * i) 
summary(T)


# 41
par(mfrow=c(2,4))
plot(L)
plot(i)
plot(K)
plot(n)
plot(BD)
plot(foc)
plot(Koc)
par(mfrow=c(1,1))


# 42
plot(T)


# 44
plot(T, xlim=c(0,500))


# 51
Data = c(
0.653,    
0.178 ,
0.263 ,    
0.424 ,
0.284 ,    
0.438 ,
0.471 ,    
0.852 ,
0.480 ,    
0.375 ,
0.148 ,    
0.185 , 
0.320 ,     
0.642 , 
0.247 ,    
0.784 ,
0.643 ,    
0.261 ,
0.636 ,    
0.487)
normal(mean(Data), sd(Data))


# 52 
MLnormal(Data)


# 57
histogram(Data)                    # the figure doesn't match the data


# 61
par(mfrow=c(3,4))
betapert(1,12, 1)
betapert(1,12, 2)
betapert(1,12, 3)
betapert(1,12, 4)
betapert(1,12, 5)
betapert(1,12, 6)
betapert(1,12, 7)
betapert(1,12, 8)
betapert(1,12, 9)
betapert(1,12, 10)
betapert(1,12, 11) 
betapert(1,12, 12)
par(mfrow=c(1,1))


# 70
L   = MEmmms(80,120,100,11.55)  		# source-receptor distance
i   = MEmmms(0.0003,0.0008,0.00055,0.0001443)	# hydraulic gradient
K   = MEmmms(300,3000,1000,750)  		# hydraulic conductivity
n   = MEmmms(0.2,0.35,0.25,0.05)  		# effective soil porosity
BD  = MEmmms(1500,1750,1650,100) 		# soil bulk density
foc = MEmmms(0.0001,0.005,0.00255,0.001415)   	# fraction organic carbon
Koc = MEmmms(5,20,10,3)    			# organic partition coefficient 
Tind  = (n + BD * foc* Koc) * L / (K * i) 
summary(Tind)


# 71
par(mfrow=c(2,4))
plot(L,col='red')
plot(i,col='red')
plot(K,col='red')
plot(n,col='red')
plot(BD,col='red')
plot(foc,col='red')
plot(Koc,col='red')
par(mfrow=c(1,1))


# 72
Maxent = Tind
pert = function(x) betapert(left(x), right(x), mean(x))     # notice use of "mean"
L   = pert(L)
i   = pert(i)
K   = pert(K)
n   = pert(n)
BD  = pert(BD)
foc = pert(foc)
Koc = pert(Koc)
PERT  = (n + BD * foc* Koc) * L / (K * i) 
plot(NULL, xlim=c(0,100000), ylim=c(0,1))
lines(PERT, col='lightblue')
lines(Maxent, col='red')


# 76 
BW = normal( 608, 66.9) 
A = uniform(0.354,0.47)
B = uniform(0.836,0.888) 
FMR = A * BW ^ B 
AEfish = MEminmaxmean(0.77, 0.98, 0.91)   
AEinverts = MEminmaxmean(0.72, 0.96, 0.87)   
GEfish = normal(1200, 240) 
GEinverts = normal(1050, 225) 
Cfish = 0.3   # uniform(0.1,0.3)
Cinverts = 0.06   # uniform(0.02, 0.06) 
Pfish = 0.9  
Pinverts = 0.1 
TDI = FMR * (Cfish * Pfish / (AEfish * GEfish) + Cinverts * Pinverts / (AEinverts * GEinverts))


#77
par(mfrow=c(2,5))
plot(BW,col='black')
plot(FMR,col='black')
plot(AEfish,col='black')
plot(AEinverts,col='black')
plot(GEfish,col='black')
plot(GEinverts,col='black')
plot(Cfish,col='black')
plot(Cinverts,col='black')
plot(Pfish,col='black')
plot(Pinverts,col='black')
par(mfrow=c(1,1))


# 78
plot(TDI, cumulative=FALSE)
mean(TDI)
median(TDI)
cut(TDI, 0.95)
sd(TDI)
TDI_ME = TDI

# close R by entering q() and answering No to the question about saving the workspace




###############################################
# Invoke R by double clicking its desktop icon
# Enter rm(list = ls()) to clear R's memory
# Click File / Source R code on the main menu
# Locate and Open the file pba.r
###############################################


# 123
A = lognormal(interval(.05,.06), sqrt(interval(0.0001,0.001)))
B = minmaxmode(0, 0.05, 0.03)
C = histogram(c(0.2, 0.5, 0.6, 0.7, 0.75, 0.8), mn=0, mx=1)
D = uniform(0, 1)
par(mfrow=c(2,2))
plot(A, col='blue')
plot(B, col='pink')
plot(C, col='black')
plot(D, col='green')


# 124
par(mfrow=c(1,1))
fi = A %|&|% B %|&|% C %|&|% D
f = A %&% B %&% C %&% D
plot(f, col='orange')
lines(fi, col='black')


# 125
range(fi)
median(fi)
mean(fi)
var(fi)
sd(fi)
range(f)
median(f)
mean(f)
var(f)
sd(f)


# 128
A = env(mix.equal.numeric(c(10,20,30,40)), mix.equal.numeric(c(30,50,60,70)))
prob(A, 15)
cut(A, 0.95)          # cut.pbox(A, 0.95)


# 130-132
A = mixture(i(1,3), i(2,4), i(3,5))
B = mixture(i(2,8), i(6,10), i(8,12))
A + B


# 136
par(mfrow=c(3,3))
plot(minmax(5,7))
plot(minmaxmedian(5,7,6))
plot(minmaxmode(5,7,6))
plot(minmaxmean(5,7,6))
plot(unimmms(5,7,6,1,6))          # plot(minmaxmeanismode(5,7,6))  # minmaxmeanismode has a bug
plot(minmaxmedianismode(5,7,6))
plot(meanstd(5,1))
plot(symmeanstd(5,1))
plot(mmms(5,7,6,0.5))
par(mfrow=c(1,1))


# 142
a.1=i(1.00, 2.00) 
a.2=i(2.68, 2.98) 
a.3=i(7.52, 7.67) 
a.4=i(7.73, 8.35) 
a.5=i(9.44, 9.99) 
a.6=i(3.66, 4.58)
b.1=i(3.5, 6.4)
b.2=i(6.9, 8.8)
b.3=i(6.1, 8.4)
b.4=i(2.8, 6.7)
b.5=i(3.5, 9.7)
b.6=i(6.5, 9.9)
b.7=i(0.15, 3.8)
b.8=i(4.5, 4.9)
b.9=i(7.1, 7.9)
a = mixture(a.1,a.2,a.3,a.4,a.5,a.6)
b = mixture(b.1,b.2,b.3,b.4,b.5,b.6,b.7,b.8,b.9)


# 144
plot(a, col='red')
plot(b, col='blue')


# 146
# it's not easy to compute the fitted normals in pba.r


# 148
data = runif(30,340,460)  # not the correct data for the graph on the slide
histogram(data)


# 149
# needs RAMAS Risk Calc
#setdefault(confidence,3) // 95%
#H = histogram(0.001,9.99,a.1,a.2,a.3,a.4,a.5,a.6, b.1,b.2,b.3,b.4,b.5,b.6,b.7,b.8,b.9)
#setdefault(confidence,0) // 0%
#h = histogram(0.001,9.99,a.1,a.2,a.3,a.4,a.5,a.6, b.1,b.2,b.3,b.4,b.5,b.6,b.7,b.8,b.9)
#plot(h,col='black')
#plot(H,col='green')
# the green graph on the slide is in error; the confidence levels seems to be less than 95%


# 152-153
onepointconfidenceband.normal = function(x, c=0.95) {
  stopifnot(length(x)==1)
  tm = c(4.83952, 9.678851, 48.39413)
  ts = c(8,17,70)
  k = which(c==c(0.9, 0.95, 0.99))
  normal(x+tm[[k]]*abs(x)*interval(-1,1),interval(0,ts[[k]]*abs(x)))
  }
datum = 500
onepointconfidenceband.normal(datum, 0.9)


# 158
#plot(CBbinomial(2,10))  # equivalent to CBbernoulli(c(0,0,1,0,1,0,0,0,0,0))    # these are the corresponding compound distributions
plot(CBbinomial.p(2,10))  # equivalent to CBbernoulli.p(c(0,0,1,0,1,0,0,0,0,0))


# 162
# the mmms function is not optimal in pba.r
L   = mmms(80,120,100,11.55)  			# source-receptor distance
i   = mmms(0.0003,0.0008,0.00055,0.0001443)	# hydraulic gradient
K   = mmms(300,3000,1000,750)  			# hydraulic conductivity
n   = mmms(0.2,0.35,0.25,0.05)  		# effective soil porosity
BD  = mmms(1500,1750,1650,100) 			# soil bulk density
foc = mmms(0.0001,0.005,0.00255,0.001415)   	# fraction organic carbon
Koc = mmms(5,20,10,3)    			# organic partition coefficient 


# 163
# the mmms function is not optimal in pba.r
par(mfrow=c(2,4))
plot(L)
plot(i)
plot(K)
plot(n)
plot(BD)
plot(foc)
plot(Koc)
par(mfrow=c(1,1))


# 164
# T is not optimal because the mmms function is not optimal in pba.r
T  = (n %+% BD %*% foc %*% Koc) %*% L %/% (K %*% i) 
T


# 165
# T is not optimal because the mmms function is not optimal in pba.r
plot(T, xlim=c(0,500))
 

# 170
BW = normal( 608, 66.9)   
FMR = interval(0.412 + c(-1,1)*0.058) * BW ^ interval(0.862 + c(-1,1)*0.026)
AEfish = minmaxmean(0.77, 0.98, 0.91)   
AEinverts = minmaxmean(0.72, 0.96, 0.87)   
GEfish = normal(1200, 240)   
GEinverts = normal(1050, 225)   
Cfish = interval(0.1,0.3)  
Cinverts = interval(0.02, 0.06)
Pfish = 0.9  
Pinverts = 0.1  

# 171
par(mfrow=c(2,5))
plot(BW, col='black')
plot(FMR, col='black')
plot(AEfish, col='black')
plot(AEinverts, col='black')
plot(GEfish, col='black')
plot(GEinverts, col='black')
plot(Cfish, col='black')
plot(Cinverts, col='black')
plot(Pfish, col='black')
plot(Pinverts, col='black')
par(mfrow=c(1,1))


# 172      # there seems to be somekind of bug in this calculation...it's an order of magnitude smaller than the maxent solution
TDI = FMR %|*|% (Cfish %*% Pfish %/% (AEfish %|*|% GEfish) %+% Cinverts %*% Pinverts %/% (AEinverts %|*|% GEinverts))
plot(TDI, col='black')

# compare with results from slides 76-78
bw = normal( 608, 66.9) 
a = uniform(0.354,0.47)
b = uniform(0.836,0.888) 
fmt = a * bw ^ b
 sawinconradalpha01 <- function(mu) {
   if (abs(mu-0.5)<0.000001) return(0)
   f = function(alpha) 1/(1-1/exp(alpha)) - 1/alpha - mu
   uniroot(f,c(-500,500))$root
   }
qsawinconrad <- function(p, min, mu, max){
   alpha = sawinconradalpha01((mu-min)/(max-min))
   if (abs(alpha)<0.000001) return(min+(max-min)*p) else
   min+(max-min)*((log(1+p*(exp(alpha)-1)))/alpha)
   }
sawinconrad <- function(min, mu, max, r=runif(MC$many)){
   a <- left(min);   b <- right(max)
   c <- left(mu);    d <- right(mu)
   if (c<a) c <- a   # implicit constraints
   if (b<d) d <- b
   pbox(qsawinconrad(iii(), min, mu, max))
   }
aefish = sawinconrad(0.77, 0.91, 0.98)   
aeinverts = sawinconrad(0.72, 0.87, 0.96)   
gefish = normal(1200, 240) 
geinverts = normal(1050, 225) 
cfish = 0.3   # uniform(0.1,0.3)
cinverts = 0.06   # uniform(0.02, 0.06) 
pfish = 0.9  
pinverts = 0.1 
TDI_ME = fmr * (cfish * pfish / (aefish * gefish) + cinverts * pinverts / (aeinverts * geinverts))
lines(TDI_ME, col='red')     # from earlier slide 78


TDI_ME = fmr * (cfish * pfish / (aefish * gefish) + cinverts * pinverts / (aeinverts * geinverts))
TDI = FMR %|*|% (Cfish %*% Pfish %/% (AEfish %|*|% GEfish) %+% Cinverts %*% Pinverts %/% (AEinverts %|*|% GEinverts))



# 174  # pba.r gives a puffy answer
EF = mmms(1, 52, 5.4, 10) 		# meals per year	// exposure frequency, censored data, n = 23
IR = mmms(1.5, 675, 188, 113) 	# grams per meal	// poultry ingestion rate from EPA's EFH
C = interval(7.1, 9.73) 			# mg per kg	// exposure point (mean) concentration
LOSS = 0						# [dimensionaless] // loss due to cooking
AT = 365.25 					# days per year	// averaging time (not just units conversion)
BWmale = lognormal(171, 30) 		# pounds	// adult male n = 9,983
BWfemale = lognormal(145, 30) 		# pounds	// adult female n = 10,339
BW = mixture(BWfemale, BWmale)	# // body mass (Brainard and Burmaster 1992)
RfD = 0.00002 					# mg per kg per day	// reference dose (EPA considers tolerable)
# pba.r assumes the variables are independent
HQ = (EF * IR * C * (1 - LOSS)) / (AT * BW * RfD)   # pba.r seems to give a puffy answer


# 175  
par(mfrow=c(2,3))
plot(EF, col='blue', cumulative=FALSE)
plot(IR, col='blue', cumulative=FALSE)
plot(HQ, col='blue', cumulative=FALSE)
plot(BW, col='blue', cumulative=FALSE)
plot(pbox(C), col='blue', cumulative=FALSE)   # note pbox(C)

# pba.r gives a puffy answer, presumably because it's not performing the moment contractions
# pba.r is also NOT accounting for UNIT CONVERSIONS...those have to be done by the analyst

plot(HQ, col='blue', cumulative=FALSE)
par(mfrow=c(1,1))
mean(HQ)
sd(HQ)
median(HQ)
percentile(HQ, 0.95)
range(HQ)
###################################
## the analogous code for Risk Calc looks like this:
#EF = mmms(1, 52, 5.4, 10) * 1 meals per year	// exposure frequency, censored data, n = 23
#IR = mmms(1.5, 675, 188, 113) * 1grams per meal	// poultry ingestion rate from EPA's EFH
#C = [7.1, 9.73] * 1 mg per kg	// exposure point (mean) concentration
#LOSS = 0	// loss due to cooking
#AT = 365.25 days per year	// averaging time (not just units conversion)
#BWmale = lognormal(171, 30) *1 pounds	// adult male n = 9,983
#BWfemale = lognormal(145, 30) * 1 pounds	// adult female n = 10,339
#BW = mixture(BWfemale, BWmale)	// body mass (Brainard and Burmaster 1992)
#RfD = 0.00002 mg per kg per day	// reference dose (EPA considers tolerable)
#HQ = (EF |*| IR |*| C |*| (1 - LOSS)) |/| (AT |*| BW |*| RfD) + 0 
#mean(HQ)      #    [ 14.25714, 20.07042] 
#sd(HQ)           #    [ 32.71976, 46.91305] 
#median(HQ)    #    [ 1.008474, 35.47708] 
#cut(HQ, 0.95)  #    [ 5.655222, 224.5597] 
#range(HQ)       #    [ 0.01218646, 1229.972] 





A = 5
B = 3 
a = N(A,1)
b = U(B-1,B+1)
A+B; a + b    
A-B; a - b    
A*B; a * b
A/B;  a / b
min(A,B);  pmin(a,b)
max(A,B);  pmax(a,b)
A^B;  a ^ b

many = 10000
a = rnorm(many, A, 1)
b = runif(many,B-1,B+1)
e = a ^ b
edf(e)
mean(e)
var(e)

# nolint end
