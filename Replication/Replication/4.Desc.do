/**********************************************************************/
/*  SECTION 1: Desc stats	
    Notes: */
/**********************************************************************/
use "${JGSS_DIR}/Edited/JGSSed.dta",clear
append using "${SSM_DIR}/Edited/SSM.dta"

replace marage = . if marage > 49

gen empx=emp
recode emp 3=2
tabulate emp,gen(emp)

tabulate workch,gen(workch)

eststo male1: estpost tabstat emp1 emp2 marage bage1 if workch == 1 & female == 0, statistics(mean sd) columns(statistics) 
eststo male2: estpost tabstat emp1 emp2 marage bage1 if workch == 2 & female == 0, statistics(mean sd) columns(statistics) 

quietly esttab male1 male2 using "${Tables_DIR}/Desc_male.csv", replace cells("mean(fmt(3)) sd(fmt(3)) ") nonote label plain

eststo female1: estpost tabstat emp1 emp2 marage bage1 if workch == 1 & female == 1, statistics(mean sd) columns(statistics) 
eststo female2: estpost tabstat emp1 emp2 marage bage1 if workch == 2 & female == 1, statistics(mean sd) columns(statistics) 

quietly esttab female1 female2 using "${Tables_DIR}/Desc_female.csv", replace cells("mean(fmt(3)) sd(fmt(3)) ") nonote label plain

ta emp workch if female == 1
ta emp workch if female == 0
ta emp workch 

