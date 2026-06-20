/**********************************************************************/
/*  SECTION 1: Data const SSM 2015		
    Notes: */
/**********************************************************************/

use "${SSM_DIR}/SSM2015_v070_20170227.dta", clear

/*----------------------------------------------------*/
   /* [>   1.  Demographic traits   <] */ 
/*----------------------------------------------------*/
*gen pid = id
gen pid = _n + 40000
gen year = 2015
* id, sex, year/month of birth, age
recode q1_1 1=0 2=1,gen(female)
gen age = q1_2_5
recode q28 99/999=.,gen(spage)
gen birthy = .
replace birthy = meibo_2 + 1925 if meibo_1 == 1 
replace birthy = meibo_2 + 1988 if meibo_1 == 2 
gen birthm = meibo_3 if female == 1
replace birthm = 6 if female == 0
gen agem=age*12+birthm
* Education
* 4 "中学" 5 "高校" 7 "専門学校" 8 "短大" 9 "高専" 10 "大学" 11 "大学院" 88 "学歴なし "99 "不明"
recode edssmx 4=1 5=1 7=1 8/9=2 10/11=2 else=.,gen(redu)

/*----------------------------------------------------*/
   /* [>   2.  Age at marriage   <] */ 
/*----------------------------------------------------*/
* SSM2015 can distinguish the first marriage from a higher order one
gen marstat = q25
recode q26 (888/999=.),gen(marage) 
recode q34 888/999=.
replace marage = q34 if (marstat == 3 | marstat == 4) & female == 1
replace marage = spage - (age-marage) if female == 0

/*----------------------------------------------------*/
   /* [>   3.  Child birth   <] */ 
/*----------------------------------------------------*/
forvalues i=1/2{
gen cbirth`i'yr = . 
recode dq13_`i'_2b 88/99=.
replace cbirth`i'yr = dq13_`i'_2b + 1925 if dq13_`i'_2a == 1 
replace cbirth`i'yr = dq13_`i'_2b + 1988 if dq13_`i'_2a == 2 
gen childage`i' = 2015 - cbirth`i'yr
*First birth age
gen bage`i'=age-childage`i' 
gen spbage`i'=spage-childage`i' 
}
recode dq12 99=. ,gen(nchild)

/**********************************************************************/
/*  SECTION 2: Data const SSM 2015 - Employment -
    Notes: */
/**********************************************************************/

/*----------------------------------------------------*/
   /* [>   1.  starting and ending age for each job   <] */ 
/*----------------------------------------------------*/
* work start age
gen wstartage1 = q8_h_1
forvalue i=2/22 {
gen wstartage`i'=q9_`i'_c_7
recode wstartage`i' 98/max=.
}

//start and end age
forvalues i=1/21{
gen wendage`i'=.
forvalues j=2/22{
replace wendage`i'= wstartage`j' if `i'+1==`j'
}
}
gen wendage22=.

forvalue i=1/22 {
replace wendage`i'=wstartage`i' if wstartage`i'>wendage`i'
}

//ever worked
recode dansu (0=0 "No")(1/max=1 "Yes"),into(everw)

replace wstartage1=16 if everw==0
replace wendage1 = age if everw==0

/*----------------------------------------------------*/
   /* [>   2.  Employment status and firm size   <] */ 
/*----------------------------------------------------*/
rename q8_a q08d1
rename q8_c q08c1
rename q8_f q08e1
forvalue i=2/22 {
rename q9_`i'_c_3 q08c`i' //firm size
rename q9_`i'_c_4 q08d`i' //emp status
rename q9_`i'_c_5 q08e`i' //occupation
}
********************************
**** Recoding to 2005 value ****
********************************
*2015 coding 
*q8_a	
*   1	 経営者、役員
*	2	 常時雇用されている一般従業者
*	3	 パート・アルバイト
*	4	 派遣社員
*	5	 契約社員、嘱託
*	6	 臨時雇用
*	7	 自営業主、自由業者
*	8	 家族従業者
*	9	 内職
*	77	 複数コード
*	99	 わからない
*	888	 非該当
*	999	 不明
	
*2005 coding 1:manager, 2:regular 3:non regular (part time), 4: non regular (dispatched), 
* 5: non regular (contract), 6: self emp, 7: family business, 8: side job at home, 9 student, 10 non employed
forvalue i=1/22 {
recode q08d`i' (1/2=2)(3=3)(4=4)(5/6=3)(7=6)(8=7)(9=8)(10/12=10)(888=10)(99/999=.) 
recode q08c`i' (888=98)(99/999=99) 
}

/*----------------------------------------------------*/
   /* [>   3.  Survival time   <] */ 
/*----------------------------------------------------*/
gen fbirth=1 if bage1>0 
replace fbirth=0 if bage1==.

* Work cohort
gen wstartyear = wstartage1+birthy
recode wstartyear 1970/1989=1 1990/2015=2 else=. ,gen(workch)

ta female everw if wstartyear > 1969 & wstartage1 < 50,row
gen mar_job = .
replace mar_job = 1 if marage < wstartage1
replace mar_job = 0 if marage >= wstartage1

ta mar_job if female == 1 & workch == 1
ta mar_job if female == 1 & workch == 2

keep pid female marstat marage birthy fbirth redu year age everw ///
bage* q08c* q08d* q08e* wstartage* wendage* nchild workch wstartyear

reshape long q08c q08d q08e wstartage wendage,i(pid) j(num)

duplicates drop //0 duplicates
keep if everw == 1
drop if everw==0 & num>1 // Omit never worked women
drop if everw==1 & q08c==. & q08d==. & wstartage==.
drop if wstartage == . 
drop if bage1 < 18

/*----------------------------------------------------*/
   /* [>   4.  Expand  <] */ 
/*----------------------------------------------------*/
replace wendage = age if wendage ==. 
recode wstartage wendage (99/999=.)
gen duration=wendage-wstartage
expand duration
bysort pid num: gen obs=_n

replace wstartage=wstartage+obs-1
replace wendage=wstartage+1

/*----------------------------------------------------*/
   /* [>   5.  Sample restriction  <] */ 
/*----------------------------------------------------*/
keep if wstartyear > 1969 
drop duration age
recode q08d 2=1 3=2 4=3 6/7=4 8=. 10=5,gen(emp)
keep if wstartage < 50

sort pid wstartage
by pid: gen duration = _n

gen emp1 = emp if duration == 1
by pid: egen emp1x = max(emp1)
keep if emp1x == 1 | emp1x == 2 | emp1x == 3

drop emp
rename emp1x emp
rename birthy byear

save "${SSM_DIR}/Edited/2015.dta",replace
