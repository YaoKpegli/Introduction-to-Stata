
cd "C:\Users\Yao Thibaut Kpegli\Desktop\ENS Paris Saclay\Stata2023_2024"

sca a = 2
sca b=3
sca c=-4

di a
di b
di c

di a+b
di a*b
di a/b
di a-b

di 2^3 
di a^b
di log(a)
di sign(c)
di round(4.56789,0.01)
di ceil(4.56789)

clear all

matrix def A = (1,2 \ 3,4)
matrix list A

di A[2,1]

mat I= I(2)
mat li I

mat V1 = J(2,1,1) /* matrix with two lines and 1 column containing 1 */
mat li V1

 mat def B = A'
 mat li B
 

 mat C= A*B
 mat li C
 
 mat D = A+C
 mat li D
 
 mat E = A-C
 mat li E
 
 mat F = A/2
 mat li F
 
 
 mat A = (0.5 \ 2)
 mat B = (1,0 \ 1,1)
 mat li A
 mat li B
 
 mat C = A#B
 mat li C


mat A =(1,2 \ 3,4 )
mat B = I(2)

mat C = (A,B)
mat li C

mat D = (A\B)
mat li D


mat inv = inv(A)
mat li inv

mat diag = vecdiag(A)
mat li diag

di trace(A)

di det(A)


****************************

mat X = (1 \ 4 \ 2 \ 5) 
mat X = J(4,1,1),X
mat li X

mat B = inv(X'*X)
mat li B


mat diag = vecdiag(B)
mat li diag

mat Y = (3 \ 6 \ 4 \ 7) 
mat li Y

mat beta = B*X'*Y

mat li beta


****************************
clear all
sysuse auto
browse 



clear all
*edit
*type your data
*rename (var1 var2 var3)(one two three)


clear all
set obs 10
gen one = _n 
gen two = _n+10
gen three = _n +20



summarize  one
summarize  one, detail
gen ratio = one/two
rename ratio new
replace one = 3 if one ==  5

gen ratio = one/two
gen dumy_ch  = ""
replace dumy_ch="Low" if one <= 5
replace dumy_ch="High" if one > 5
gen dumy_num  = (one <= 5)

lab define lab_name 1 "Low" 0 "High"
label value dumy_num lab_name  


keep if two <=16
drop if two >=14
drop new
keep one three


*** exportation en format .dta de stata
 
save example , replace

*** exportation excel
 export excel using example.xlsx , firstrow(variables) replace
 
*** exportation csv
export delimited using example.csv, replace

*** exportation txt
export delimited using example.txt,  replace
 
 
*** importation en format .dta de stata
 
use example , clear
 
*** importation excel
 import excel example.xlsx , firstrow clear
 
*** importation csv
import delimited example.csv, clear

*** importation txt
import delimited example.txt,  clear




********append

sysuse auto, clear
br
keep if _n <=37
save first37 , replace



sysuse auto, clear
br
keep if _n>37
save after37 , replace


use first37 , clear
append using after37


***** merge

sysuse auto, clear
br
keep if _n <=37
keep make price
rename (make price)(make_1 price_1)
gen identifier = _n
save first37_merge , replace


sysuse auto, clear
br
keep if _n>37
keep make price
gen identifier = _n
save after37 , replace

use first37_merge , clear
merge 1:1 identifier using after37


***** 
clear all
sysuse auto, clear
describe
codebook
sum rep78 price weight foreign trunk length, det
tab rep78
tab rep78, mis 

tab foreign
tab foreign, nolabel

tab foreign rep78 , m
gen domestic = 1- foreign 

sum trunk, det
gen trunk_small = (trunk<10)
gen trunk_standard = (trunk>=10)*(trunk<20) 
gen trunk_huge=(trunk>=20)
gen trunk_categ= trunk_small + 2*trunk_standard+ 3*trunk_huge
br
lab def catego 1 "Small" 2 "Standard" 3 "Huge"
lab value trunk_categ catego

keep foreign price trunk* weight length 
gen lprice = ln(price)
save autoME , replace
export excel using autoME.xlxs , replace firstrow(variables)














**** egen
sysuse auto, clear
egen rowmean= rowmean( price - turn ) 
edit
sum rowmean
egen  mean_price =  mean(price) , by(rep78)
edit
ta mean_price rep78, m


*** collapse
sysuse auto , clear
collapse (mean) price  weight (min) trunk , by(rep78)
edit


**** preserve-restore
sysuse auto , clear
preserve
collapse (mean) price  weight (min) trunk , by(rep78)
save data_manipulate , replace
restore
edit


***** plot

sysuse auto, clear
gen lprice=log(price)

scatter price weight
scatter price weight , graphregion(color(white))
corr price trunk

graph twoway (scatter price weight, msize(small)) ///
(lfit price weight), graphregion(color(white)) ///
 title( "Scatterplot and OLS fitted line")

 
hist lprice , graphregion(color(white)) name(gh)
cumul lprice, gen(cum_lprice)
sort cum_lprice
line cum_lprice lprice, graphregion(color(white)) title("Cumulative of median family income") name(gc) 
kdensity lprice , graphregion(color(white)) name(gk)
graph box lprice , graphregion(color(white)) name(gb)
graph combine gh gc gk gb , graphregion(color(white))


tw function y = x^2 , range(-2 2) graphregion(color(white)) ytitle("f(x)")

sum lprice
sca m=r(mean)
sca sd= r(sd)


tw (kdensity lprice) (function y = (1/(scalar(sd)*sqrt(2*_pi)))*exp(-0.5*((x-scalar(m))/scalar(sd))^2), range(7 10)), graphregion(color(white)) ytitle(density) legend(position(11) col(1) ring(0) label(1 "Kernel empirical pdf") label(2 "Normal pdf"))



		
sysuse auto , clear
gen lprice=log(price)
program drop _all

program define ols_ml
args lnf a b
tempvar y x  
qui {

generate double `y' = $ML_y1
generate double `x'  = $ML_y2

replace `lnf' = -(`y'- `a'*`x'-`b')^2 

}

end			
ml model lf ols_ml (a: lprice weight = ) (b: ) ,  maximize 
ml di
		
reg lprice weight


gen rep78small =(rep78<=2)


program define bin_ml
args lnf p
tempvar y x  
qui {

generate double `y' = $ML_y1

replace `lnf' = `y'*log(`p')+(1-`y')*log(1-`p') 

}

end	

ml model lf bin_ml (p: rep78small = ) , maximize 
ml di
		
sum rep78small

******

************




forval i=1/74 {
qui sum weight if _n <= `i'
di "mean of lprice of the `i' first observations is: " r(mean)
}


forval i=1/100 {
if mod(`i',3) == 0 {
di "`i' is a multiple of 3"
}
}

global VAR weight length
foreach x in $VAR {
scatter lprice `x' , graphregion(color(white)) name(`x')
}

graph combine $VAR , graphregion(color(white))


sum lprice 
sca min = r(min)
sca j = 1
local i = 1
while (lprice[`i']!= r(min)) {
local i = `i' + 1
sca j = `i'
}

di "the min of lprice is at the row: " j
 

 
******
 gen sdnorm=rnormal(0,1)
 gen sdunif=runiform(0,1)
	
		
*********
clear all

set obs 10000000	
gen x=runiform(-10,10)	
	
gen f= (1/sqrt(2*_pi))*exp(-0.5*x^2)

sum f

di "the approximated value is:" 20*r(mean)

****
		
clear all

set seed 123456789
set obs 1000

gen X = runiform()
gen Y= (X<=0.4)

gen Ybar=.

qui forvalue i=1/1000 {
sum Y if _n<=`i'
replace Ybar = r(mean) in `i'
}

gen i = _n
line Ybar i, graphregion(color(white)) yline(0.4)
	
*********
clear all

set obs 100000
gen x=runiform(-1,1)	
gen y=runiform(-1,1)
	
gen circle = (x^2 + y^2 <=1)
	
sum  circle
di "the approximated value is:" 4*r(mean)

tw (scatter y x)(scatter y x if  circle==1), graphregion(color(white)) legend(label(1 "Square") label(2 "Disque"))
