/**********************************************************************/
/*  SECTION 1: Marriage	
    Notes: */
/**********************************************************************/
use "${SSM_DIR}/Edited/2015.dta",clear
rename wstartage age
keep pid workch marage age emp female bage1 redu byear
sort pid age
by pid: gen n=_n
keep if n == 1
gen year = 2015.5
gen survey="SSM"
save "${SSM_DIR}/Edited/SSM.dta",replace

use "${SSM_DIR}/Edited/2015.dta",clear

gen mar=1 if marage == wstartage
replace mar=0 if mar != 1
replace mar=. if marage == bage1 & wstartage >= marage
keep if mar !=.
by pid: gen marsum = sum(sum(mar))
drop if marsum > 1

rename wstartage age
keep pid workch mar age emp female redu byear

recode age 15/19=1 20/24=2 25/29=3 30/34=4 35/39=5 40/44=6 45/49=7

save "${Data_DIR}/2015mar.dta",replace

/**********************************************************************/
/*  SECTION 2: Fertility	
    Notes: */
/**********************************************************************/
use "${SSM_DIR}/Edited/2015.dta",clear
keep if marage !=.
gen bir=1 if bage1 == wstartage
replace bir=0 if bir != 1
by pid: gen birsum = sum(sum(bir))
drop if birsum > 1
keep if marage <= bage1

recode wstartage 15/19=1 20/24=2 25/29=3 30/34=4 35/39=5 40/44=6 45/49=7,gen(age)

keep pid workch bir age emp female redu byear

save "${Data_DIR}/2015birth.dta",replace

/**********************************************************************/
/*  SECTION 3: Bridal pregnancy	
    Notes: */
/**********************************************************************/
use "${SSM_DIR}/Edited/2015.dta",clear

gen bri=1 if marage == bage1 & marage == wstartage
replace bri=0 if bri != 1
replace bri=. if marage != bage1 & marage == wstartage
keep if bri !=.
by pid: gen brisum = sum(sum(bri))
drop if brisum > 1
keep if marage <= bage1

recode wstartage 15/19=1 20/24=2 25/29=3 30/34=4 35/39=5 40/44=6 45/49=7,gen(age)

keep pid workch bri age emp female redu byear
 
save "${Data_DIR}/2015bridal.dta",replace



