*Things to construct: pid, female, edu, byear, age, marage, bage, emp

use "${JGSS_DIR}/ICPSR_03593/DS0001/03593-0001-Data.dta",clear
rename *, lower
rename iduse pid
drop duration
gen year = 2000
replace pid = pid + 2000*10000
save "${JGSS_DIR}/Edited/2000.dta",replace

use "${JGSS_DIR}/ICPSR_04213/DS0001/04213-0001-Data.dta",clear
rename *, lower
rename iduse pid
drop duration
gen year = 2001
replace pid = pid + 2001*10000
save "${JGSS_DIR}/Edited/2001.dta",replace

use "${JGSS_DIR}/ICPSR_04214/DS0001/04214-0001-Data.dta",clear
rename *, lower
rename iduse pid
drop duration
gen year = 2002
replace pid = pid + 2002*10000
save "${JGSS_DIR}/Edited/2002.dta",replace

use "${JGSS_DIR}/ICPSR_25181/DS0001/25181-0001-Data.dta", clear
rename *, lower
rename caseid pid
drop duration
gen year = 2006
replace pid = pid + 2006*10000
save "${JGSS_DIR}/Edited/2006.dta",replace

use "${JGSS_DIR}/ICPSR_36577/DS0001/36577-0001-Data.dta", clear
rename *, lower
rename iduse pid
drop duration
gen year = 2012
replace pid = pid + 2012*10000
save "${JGSS_DIR}/Edited/2012.dta",replace

use "${JGSS_DIR}/ICPSR_37874/DS0001/37874-0001-Data.dta", clear
rename *, lower
rename iduse pid
drop duration
gen year = 2015
replace pid = pid + 2015*10000
save "${JGSS_DIR}/Edited/2015.dta",replace

import spss using "${JGSS_DIR}/JGSS-2016_v1.1/JGSS-2016_japanese/2)JGSS-2016_v1.1_j.sav", clear
rename iduse pid
drop duration
gen year = 2016
replace pid = pid + 2016*10000
save "${JGSS_DIR}/Edited/2016.dta",replace

import spss using "${JGSS_DIR}/JGSS-20172018_v1.0/JGSS-20172018_japanese/2)JGSS-20172018_v1.0_j.sav", clear
rename iduse pid
drop duration
gen year = 2017
replace pid = pid + 2017*10000
save "${JGSS_DIR}/Edited/2017.dta",replace

/*----------------------------------------------------*/
   /* [>   1.  JGSS 2000-2002 recode  <] */ 
/*----------------------------------------------------*/
foreach x of numlist 2000 2001 2002 {
	use "${JGSS_DIR}/Edited/`x'.dta",clear
	gen duration = ageb - 14

recode cc01age 888/999=.
gen byear1 = year - cc01age
gen bage1 = byear - dobyear
keep if bage1 > 16

* female 
recode sexa 1=0 2=1,gen(female)
* age at marriage
recode age1mg 888/999=.,gen(marage)
drop if marage > bage1 & marage != . 

* age1mg age2mg age3mg age4mg

* education 
* 1 JHS 2 HS 3 JC 4 BA 5 GS
drop if xxlstsch < 8
recode xxlstsch 8=1 9=2 10=3 11=4 12=5 else=.,gen(redu)
recode redu 1/2=1 3/5=2
keep if dolstsch == 1 | dolstsch == 2 // include dropout

* wstartyear
cap keep if xgetjob <= 2 // those who ever worked
gen wstartyear = .
replace wstartyear = dobyear + 16 if redu == 1
replace wstartyear = dobyear + 19 if redu == 2
replace wstartyear = dobyear + 21 if redu == 3
replace wstartyear = dobyear + 23 if redu == 4
replace wstartyear = dobyear + 25 if redu == 5
recode wstartyear 1970/1989=1 1990/2015=2 else=. ,gen(workch)
keep if workch != .

*first job
* https://jgss.daishodai.ac.jp/surveys/table/TP12FSTJ.html
* recode to 
*1 standard
*2 standard part time
*3 standard dispached

recode tp12fstj 1/7=1 8=2 9=3 10/12=. else=.,gen(emp)
rename ageb age
gen byear = year - age

keep pid year duration bage1 marage redu female workch emp redu byear

save "${JGSS_DIR}/Edited/`x'_ed.dta",replace
}

/*----------------------------------------------------*/
   /* [>   2.  JGSS 2006 recode  <] */ 
/*----------------------------------------------------*/
foreach x of numlist 2006 {
	use "${JGSS_DIR}/Edited/`x'.dta",clear
	gen duration = ageb - 14

recode cc01age 888/999=.
gen byear1 = year - cc01age
gen bage1 = byear - dobyear
keep if bage1 > 16

* female 
recode sexa 1=0 2=1,gen(female)
* age at marriage
recode age1mg 888/999=.,gen(marage)
drop if marage > bage1 & marage != . 

* age1mg age2mg age3mg age4mg

* education 
* 1 JHS 2 HS 3 JC 4 BA 5 GS
drop if xxlstsch < 8
recode xxlstsch 8=1 9=2 10/11=3 12=4 13=5 else=.,gen(redu)
recode redu 1/2=1 3/5=2
keep if dolstsch == 1 | dolstsch == 2 // include dropout

* wstartyear
cap keep if xgetjob <= 2 // those who ever worked
gen wstartyear = .
replace wstartyear = dobyear + 16 if redu == 1
replace wstartyear = dobyear + 19 if redu == 2
replace wstartyear = dobyear + 21 if redu == 3
replace wstartyear = dobyear + 23 if redu == 4
replace wstartyear = dobyear + 25 if redu == 5
recode wstartyear 1970/1989=1 1990/2015=2 else=. ,gen(workch)
keep if workch != .

*first job
* https://jgss.daishodai.ac.jp/surveys/table/TPFSTJB.html
* recode to 
*1 standard
*2 standard part time (note: includes 内職)
*3 standard dispached

recode tpfstjb 1/2=1 3=2 4=3 else=.,gen(emp)
rename ageb age
gen byear = year - age

keep pid year duration bage1 marage redu female workch emp redu byear

save "${JGSS_DIR}/Edited/`x'_ed.dta",replace
}

/*----------------------------------------------------*/
   /* [>   3.  JGSS 2012-2017 recode  <] */ 
/*----------------------------------------------------*/
foreach x of numlist 2012 2015 2016 2017 {
	use "${JGSS_DIR}/Edited/Data/`x'.dta",clear
	gen duration = ageb - 14

recode cc01age 888/999=.
gen byear1 = year - cc01age
gen bage1 = byear - dobyear
keep if bage1 > 16

* female 
recode sexa 1=0 2=1,gen(female)
* age at marriage
recode age1mg 888/999=.,gen(marage)
drop if marage > bage1 & marage != . 

* age1mg age2mg age3mg age4mg

* education 
* 1 JHS 2 HS 3 JC 4 BA 5 GS
drop if xxlstsch < 8
recode xxlstsch 8=1 9=2 10/11=3 12=4 13=5 else=.,gen(redu)
recode redu 1/2=1 3/5=2
keep if dolstsch == 1 | dolstsch == 2 // include dropout

* wstartyear
keep if xgetjob <= 2 // those who ever worked
gen wstartyear = .
replace wstartyear = dobyear + 16 if redu == 1
replace wstartyear = dobyear + 19 if redu == 2
replace wstartyear = dobyear + 21 if redu == 3
replace wstartyear = dobyear + 23 if redu == 4
replace wstartyear = dobyear + 25 if redu == 5
recode wstartyear 1970/1989=1 1990/2015=2 else=. ,gen(workch)
keep if workch != .

*first job
* https://jgss.daishodai.ac.jp/surveys/table/TP1STJBS.html
* recode to 
*1 standard
*2 standard part time
*3 standard contract
recode tp1stjbs 1=1 2=2 3=3 4/5=2 else=.,gen(emp)
rename ageb age
gen byear = year - age

keep pid year duration bage1 marage redu female workch emp redu byear

save "${JGSS_DIR}/Edited/`x'_ed.dta",replace
}

/*----------------------------------------------------*/
   /* [>   4.  JGSS edited data merge  <] */ 
/*----------------------------------------------------*/
use "${JGSS_DIR}/Edited/2000_ed.dta",clear
append using "${JGSS_DIR}/Edited/2001_ed.dta"
append using "${JGSS_DIR}/Edited/2002_ed.dta"
append using "${JGSS_DIR}/Edited/2006_ed.dta"
append using "${JGSS_DIR}/Edited/2012_ed.dta"
append using "${JGSS_DIR}/Edited/2015_ed.dta"
append using "${JGSS_DIR}/Edited/2016_ed.dta"
append using "${JGSS_DIR}/Edited/2017_ed.dta"
keep if emp != .
*keep if female == 0

expand duration 
bysort pid: gen n = _n
sort pid n

/*----------------------------------------------------*/
   /* [>   3.  Sample restriction  <] */ 
/*----------------------------------------------------*/
gen wstartage=15+n-1
drop if wstartage > 49
save "${JGSS_DIR}/Edited/JGSS.dta",replace

/**********************************************************************/
/*  SECTION 1: Marriage	
    Notes: */
/**********************************************************************/
use "${JGSS_DIR}/Edited/JGSS.dta",clear
rename wstartage age
keep pid workch marage age emp female year bage1 redu byear
sort pid age
by pid: gen n=_n
keep if n == 1
gen survey="JGSS"
save "${JGSS_DIR}/Edited/JGSSed.dta",replace

use "${JGSS_DIR}/Edited/JGSS.dta",clear

gen mar=1 if marage == wstartage
replace mar=0 if mar != 1
replace mar=. if marage == bage1 & wstartage >= marage
keep if mar !=.
by pid: gen marsum = sum(sum(mar))
drop if marsum > 1

rename wstartage age
keep pid workch mar age emp female redu byear

recode age 15/19=1 20/24=2 25/29=3 30/34=4 35/39=5 40/44=6 45/49=7

save "${Data_DIR}/JGSSmar.dta",replace

/**********************************************************************/
/*  SECTION 2: Fertility	
    Notes: */
/**********************************************************************/
use "${JGSS_DIR}/Edited/JGSS.dta",clear
keep if marage !=.
gen bir=1 if bage1 == wstartage
replace bir=0 if bir != 1
by pid: gen birsum = sum(sum(bir))
drop if birsum > 1
keep if marage <= bage1

recode wstartage 15/19=1 20/24=2 25/29=3 30/34=4 35/39=5 40/44=6 45/49=7,gen(age)

keep pid workch bir age emp female redu byear

save "${Data_DIR}/JGSSbirth.dta",replace

/**********************************************************************/
/*  SECTION 3: Bridal pregnancy	
    Notes: */
/**********************************************************************/
use "${JGSS_DIR}/Edited/JGSS.dta",clear

gen bri=1 if marage == bage1 & marage == wstartage
replace bri=0 if bri != 1
replace bri=. if marage != bage1 & marage == wstartage
keep if bri !=.
by pid: gen brisum = sum(sum(bri))
drop if brisum > 1
keep if marage <= bage1

recode wstartage 15/19=1 20/24=2 25/29=3 30/34=4 35/39=5 40/44=6 45/49=7,gen(age)

keep pid workch bri age emp female redu byear

save "${Data_DIR}/JGSSbridal.dta",replace
