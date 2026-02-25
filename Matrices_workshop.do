****************************************************
*TITLE: Matrices in Stata - Tables & Graphs
*PROJECT: Coding and Data Committee - Workshop
*AUTHOR: Renzo Fernandez Escobar
*DATE CREATED: April 20, 2025
*DATE LAST UPDATE: April 28, 2025
************************************************

********************************************************************************
**#1. Creating a matrix from scratch
********************************************************************************
*Notice where is each value in the output of our matrix "A"
matrix A = (1,2 \ 3,4)
mat list A
mat dir 

********************************************************************************
**#2. Identifying what is the logic behind matrices 
********************************************************************************
sysuse auto, clear
sum price
return list //Check which values are being stored! This will vary depending on the command you are running.

tabstat price, stats(mean sd) save 
mat B = r(StatTotal)
mat list B

********************************************************************************
**#3. Work with made up date that is closer to our interests 
********************************************************************************
clear all
set obs 1500   // Create 1500 fake observations
* Randomly generate sex: 0 = Male, 1 = Female
gen sex = runiform() > 0.5
label define sexlbl 0 "Male" 1 "Female"
label values sex sexlbl

* Randomly generate race: 1 = White, 2 = Black, 3 = Hispanic
gen race = ceil(runiform()*3)
label define racelbl 1 "White" 2 "Black" 3 "Hispanic"
label values race racelbl

* Randomly generate maternal incarceration: 1 = Never, 2 = CLS contact, 3 = Incarceration
gen maternal_incarceration = .
replace maternal_incarceration = 1 if runiform() <= 0.75    // 75% never
replace maternal_incarceration = 2 if runiform() <= 0.15    // 15% CLS contact
replace maternal_incarceration = 3 if missing(maternal_incarceration)  // rest incarceration
label define matincarlbl 1 "Never" 2 "CLS contact" 3 "Incarceration"
label values maternal_incarceration matincarlbl

* Randomly generate parents' education: 1 = Less than HS, 2 = High School, 3 = Some College, 4 = College+
gen parent_education = ceil(runiform()*4)
label define edulbl 1 "Less than HS" 2 "High School" 3 "Some College" 4 "College+"
label values parent_education edulbl

* Randomly generate disability: 0 = No, 1 = Yes
gen disability = runiform() > 0.85   // 15% have a disability
label define dislbl 0 "No" 1 "Yes"
label values disability dislbl

* Randomly generate poverty: 0 = No, 1 = Yes
gen poverty = runiform() > 0.7   // 30% in poverty
label define povlbl 0 "No" 1 "Yes"
label values poverty povlbl

* Randomly generate child test scores (0 to 100)
gen test_score = round(runiform()*100, 0.1)

*Randomly generate time studying
gen time_studying=.
gen base_study = runiform()*10   // random between 0 and 10 hours

* Adjust based on logic
replace time_studying = base_study + 0.2*test_score/10    // those with higher test scores study more
replace time_studying = time_studying - 1 if poverty == 1 // if in poverty, study about 1 hour less
replace time_studying = time_studying - 0.5 if disability == 1 // if disability, 0.5 hour less

* Make sure time_studying is not negative
replace time_studying = 0 if time_studying < 0

* Round to 0.1 hour
replace time_studying = round(time_studying, 0.1)

* Drop helper variable
drop base_study

numlabel, ad //See numeric values
********************************************************************************
**#4. Creating a descriptive table with matrices
********************************************************************************
*Generate table structure
gen lastob=1
la def lastob 1 "N"
la val lastob lastob

estpost tab lastob maternal_incarceration, nototal
est sto mat_incar
ereturn list
esttab mat_incar using "Appendix1_Descr.csv", replace nonum nonote noobs ///
	cell("b(fmt(0)) rowpct(par fmt(2))") unstack collabels("N" "Pct.") ///
	nomtitle coeflabel("N") title("Appendix 1. Descriptives") ///
	eqlabels("No contact" "CLS contact" "Incarceration")

*Before appending the variables of interest, since we are going to loop, we want all our variables in a similar format!
tab parent_education, gen(par_ed_) //This generates dummy vars for each value of parental education
tab race, gen(race_)

*Assign label to relevant vars and append variables of interest
cap estimates drop *
local stub "sex disability poverty par_ed_1 par_ed_2 par_ed_3 par_ed_4 race_1 race_2 race_3"
local name `""Female" "Has disability" "Condition of poverty" "Less than HS" "High School" "Some college" "College +" "White" "Black" "Hispanic" "'
local n : word count `stub'
forval x=1/`n' {
	local s : word `x' of `stub'
	local t : word `x' of `name'
	la var `s' "`t'"
}

*Create matrixes for each variable of interest and append it to our table previously created.
foreach x of local stub {
	local t: var label `x'
	cap matrix drop mat2
	cap matrix drop mat3
	cap matrix drop mat4
	qui estpost tab `x' maternal_incarceration, nototal
	mat mat2= e(b)
	mat mat3= e(colpct) 
	mat mat4 = mat2[1,2], mat3[1,2], mat2[1,4], mat3[1,4], mat2[1,6], mat3[1,6]
	mat rownames mat4 ="`t'"
	esttab matrix(mat4, fmt(2)) using "Appendix1_Descr.csv", append nomtitle plain nonum nonote collabels(none) nomtitles eqlabels(none) noobs
}

*Let's see what we did back there. Note: Notice the importance of dropping prior matrix before generating a new one. 
estpost tab race_3 maternal_incarceration, nototal
mat list mat2
mat list mat3 
mat list mat4 

*We are still missing our main variable of interest: test score. This is because continuous vars have a different logic.
	*Continuous vars
cap estimates drop *
local stub "time_studying test_score"
local name `""Mean study time" "Mean test score""'
local n : word count `stub'
forval x=1/`n' {
	local s : word `x' of `stub'
	local t : word `x' of `name'
	la var `s' "`t'"
}

foreach x of local stub {
	local t: var label `x'
	cap matrix drop mat2
	cap matrix drop mat3
	qui estpost tabstat `x', by(maternal_incarceration) statistics(mean sd)
	mat mat2 =e(mean)
	mat mat3 =e(sd)
	mat mat4 = mat2[1,1], mat3[1,1], mat2[1,2], mat3[1,2], mat2[1,3], mat3[1,3]
	mat rownames mat4 ="`t'"
esttab matrix(mat4, fmt(2 2 2 2 2 2)) using "Appendix1_Descr.csv", append collabels(none) nomtitles eqlabels(none) nonum nonote noobs
}

********************************************************************************
**#5. Descriptive table with testing differences across groups
********************************************************************************
*Generate table structure
cap matrix drop mat2
cap matrix drop mat3
estpost tab lastob maternal_incarceration, nototal
mat mat2=e(b)
mat mat3 = mat2[1,1], mat2[1,2], mat2[1,3],  ., ., . 
mat colnames mat3="None" "Noncustodial" "Incarcerated" "None=Noncustodial" "None=Incarcerated" "Noncustodial=Incarcerated"
mat rownames mat3 ="N"
esttab matrix(mat3, fmt(0)) using "Appendix2_Descr.csv", replace nomtitles eqlabels(none) plain nonum nonote noobs title("Appendix 2. Sample Description by CLS contact")

*Repeat process for labels and just categoricals or dummy
cap estimates drop *
local stub "sex disability poverty par_ed_1 par_ed_2 par_ed_3 par_ed_4 race_1 race_2 race_3"
local name `""Female" "Has disability" "Condition of poverty" "Less than HS" "High School" "Some college" "College +" "White" "Black" "Hispanic" "'
local n : word count `stub'
forval x=1/`n' {
	local s : word `x' of `stub'
	local t : word `x' of `name'
	la var `s' "`t'"
}

*Create matrixes for each variable of interest and append it to our table previously created.
foreach x of local stub {
	local t: var label `x'
	cap matrix drop mat2
	cap matrix drop mat3
	qui estpost tab `x' maternal_incarceration, nototal
	mat mat2 =e(colpct)
	mat mat3 = mat2[1, 2], mat2[1,4], mat2[1, 6]
	*** get p values for comparison tests ***
qui proportion `x', over(maternal_incarceration) coeflegend
test _b[1.`x'@1.maternal_incarceration]=_b[1.`x'@2.maternal_incarceration]
local `x'p1 = r(p)
disp ``x'p1'
	test _b[1.`x'@1.maternal_incarceration]=_b[1.`x'@3.maternal_incarceration]
local `x'p2 = r(p)
	test _b[1.`x'@2.maternal_incarceration]=_b[1.`x'@3.maternal_incarceration]
local `x'p3 = r(p)
mat mat3 = (mat3, ``x'p1', ``x'p2', ``x'p3')
mat rownames mat3 ="`t'"
esttab matrix(mat3, fmt(2 2 2 3 3 3)) using "Appendix2_Descr.csv", append collabels(none) nomtitles eqlabels(none) nonum nonote noobs
}

*Repeat process now with continuous variables. 
*Continuous vars
cap estimates drop *
local stub "time_studying test_score"
local name `""Mean study time" "Mean test score""'
local n : word count `stub'
forval x=1/`n' {
	local s : word `x' of `stub'
	local t : word `x' of `name'
	la var `s' "`t'"
}

foreach x of local stub {
	local t: var label `x'
	cap matrix drop mat2
	cap matrix drop mat3
	qui estpost tabstat `x', by(maternal_incarceration) statistics(mean)
	mat mat2 =e(mean)
	mat mat3 = mat2[1,1], mat2[1,2], mat2[1,3]
	qui mean `x', over(maternal_incarceration) coeflegend
	test `x'@1.maternal_incarceration = `x'@2.maternal_incarceration 
	local `x'p1 = r(p)
	disp ``x'p1'
	test `x'@1.maternal_incarceration = `x'@3.maternal_incarceration 
	local `x'p2 = r(p)
	test `x'@2.maternal_incarceration = `x'@3.maternal_incarceration 
	local `x'p3 = r(p)
	matrix mat3=(mat3, ``x'p1', ``x'p2', ``x'p3')
	mat rownames mat3 ="`t'"
esttab matrix(mat3, fmt(2 2 2 3 3 3)) using "Appendix2_Descr.csv", append collabels(none) nomtitles eqlabels(none) nonum nonote noobs
}

********************************************************************************
**#6. You can do the same for regression outputs
********************************************************************************
reg test_score i.maternal_incarceration i.parent_education i.race sex poverty disability
return list
mat list r(table)   //Check out everything that is being stored! 


********************************************************************************
**#7. Graphs!
********************************************************************************

* Set number of groups
levelsof maternal_incarceration, local(groups)

* Gen empty matrices
matrix b = J(`=wordcount("`groups'")', 3, .)   // Coefficients: rows = groups, columns = education levels
matrix ll = J(`=wordcount("`groups'")', 3, .) // Lower limit of CI
matrix ul = J(`=wordcount("`groups'")', 3, .) // Upper limit of CI

* Start group counter
local i = 1

*Generate matrixes with coefficients and confidence intervals
foreach g of local groups {
    
    regress test_score i.parent_education if maternal_incarceration == `g'
    
    * Store coefficients
    matrix b[`i', 1] = _b[2.parent_education]
    matrix b[`i', 2] = _b[3.parent_education]
    matrix b[`i', 3] = _b[4.parent_education]
    
    * Store confidence intervals
    matrix ll[`i', 1] = _b[2.parent_education] - 1.96*_se[2.parent_education]
    matrix ll[`i', 2] = _b[3.parent_education] - 1.96*_se[3.parent_education]
    matrix ll[`i', 3] = _b[4.parent_education] - 1.96*_se[4.parent_education]
    
    matrix ul[`i', 1] = _b[2.parent_education] + 1.96*_se[2.parent_education]
    matrix ul[`i', 2] = _b[3.parent_education] + 1.96*_se[3.parent_education]
    matrix ul[`i', 3] = _b[4.parent_education] + 1.96*_se[4.parent_education]
    
    local ++i
}

mat dir
mat list b


***We can even use a combination of locals to store values in matrixes and generate a coefplot
* Run by group: maternal_incarceration == 1 (Never)
regress test_score i.parent_education if maternal_incarceration == 1

local b_never_1 = _b[2.parent_education]
local b_never_2 = _b[3.parent_education]
local b_never_3 = _b[4.parent_education]

local se_never_1 = _se[2.parent_education]
local se_never_2 = _se[3.parent_education]
local se_never_3 = _se[4.parent_education]

local ci_low_never_1 = `b_never_1' - 1.96*`se_never_1'
local ci_low_never_2 = `b_never_2' - 1.96*`se_never_2'
local ci_low_never_3 = `b_never_3' - 1.96*`se_never_3'

local ci_high_never_1 = `b_never_1' + 1.96*`se_never_1'
local ci_high_never_2 = `b_never_2' + 1.96*`se_never_2'
local ci_high_never_3 = `b_never_3' + 1.96*`se_never_3'

* Run by group: maternal_incarceration == 2 (CLS Contact)
regress test_score i.parent_education if maternal_incarceration == 2

local b_cls_1 = _b[2.parent_education]
local b_cls_2 = _b[3.parent_education]
local b_cls_3 = _b[4.parent_education]

local se_cls_1 = _se[2.parent_education]
local se_cls_2 = _se[3.parent_education]
local se_cls_3 = _se[4.parent_education]

local ci_low_cls_1 = `b_cls_1' - 1.96*`se_cls_1'
local ci_low_cls_2 = `b_cls_2' - 1.96*`se_cls_2'
local ci_low_cls_3 = `b_cls_3' - 1.96*`se_cls_3'

local ci_high_cls_1 = `b_cls_1' + 1.96*`se_cls_1'
local ci_high_cls_2 = `b_cls_2' + 1.96*`se_cls_2'
local ci_high_cls_3 = `b_cls_3' + 1.96*`se_cls_3'

* Run by group: maternal_incarceration == 3 (Incarceration)
regress test_score i.parent_education if maternal_incarceration == 3

local b_incar_1 = _b[2.parent_education]
local b_incar_2 = _b[3.parent_education]
local b_incar_3 = _b[4.parent_education]

local se_incar_1 = _se[2.parent_education]
local se_incar_2 = _se[3.parent_education]
local se_incar_3 = _se[4.parent_education]

local ci_low_incar_1 = `b_incar_1' - 1.96*`se_incar_1'
local ci_low_incar_2 = `b_incar_2' - 1.96*`se_incar_2'
local ci_low_incar_3 = `b_incar_3' - 1.96*`se_incar_3'

local ci_high_incar_1 = `b_incar_1' + 1.96*`se_incar_1'
local ci_high_incar_2 = `b_incar_2' + 1.96*`se_incar_2'
local ci_high_incar_3 = `b_incar_3' + 1.96*`se_incar_3'

*Generate matrixes by calling the locals stored
* Coefficient matrix
matrix b = ( ///
    `b_never_1' , `b_never_2' , `b_never_3' , ///
    `b_cls_1' , `b_cls_2' , `b_cls_3' , ///
    `b_incar_1' , `b_incar_2' , `b_incar_3' ///
)

* Lower confidence limit matrix
matrix ci_low = ( ///
    `ci_low_never_1' , `ci_low_never_2' , `ci_low_never_3' , ///
    `ci_low_cls_1' , `ci_low_cls_2' , `ci_low_cls_3' , ///
    `ci_low_incar_1' , `ci_low_incar_2' , `ci_low_incar_3' ///
)

* Upper confidence limit matrix
matrix ci_high = ( ///
    `ci_high_never_1' , `ci_high_never_2' , `ci_high_never_3' , ///
    `ci_high_cls_1' , `ci_high_cls_2' , `ci_high_cls_3' , ///
    `ci_high_incar_1' , `ci_high_incar_2' , `ci_high_incar_3' ///
)

matrix colnames b = Never_HS Never_SomeCollege Never_CollegePlus CLS_HS CLS_SomeCollege CLS_CollegePlus Incar_HS Incar_SomeCollege Incar_CollegePlus
matrix colnames ci_low = Never_HS Never_SomeCollege Never_CollegePlus CLS_HS CLS_SomeCollege CLS_CollegePlus Incar_HS Incar_SomeCollege Incar_CollegePlus
matrix colnames ci_high = Never_HS Never_SomeCollege Never_CollegePlus CLS_HS CLS_SomeCollege CLS_CollegePlus Incar_HS Incar_SomeCollege Incar_CollegePlus

*Gen coefplot	
coefplot (matrix(b), ci((ci_low ci_high))), ///
    horizontal ///
    yscale(reverse) ///
    mcolor("blue") ///
    ciopts(lcolor(gs8) lwidth(medthin)) ///
    msize(medium) ///
	heading(Never_HS ="{bf: No CLS contact (ref:Less than HS)}" CLS_HS ="{bf: CLS contacted mother (ref:Less than HS)}" Incar_HS ="{bf: Incarcerated mother (ref:Less than HS)}", labsize(small)) ///
	coeflabels(Never_HS = "High school" Never_SomeCollege = "Some College" Never_CollegePlus = "College +" CLS_HS = "High school" CLS_SomeCollege = "Some College" CLS_CollegePlus = "College +" Incar_HS = "High school" Incar_SomeCollege = "Some College" Incar_CollegePlus = "College +", labsize(small)) ///
    xlabel(, labsize(medsmall)) ///
    graphregion(color(white) margin(large)) ///
    ylabel(, labsize(medsmall) angle(0)) ///
    xline(0, lpattern(dash) lcolor(red)) ///
    title("Effect of Parent Education on Test Scores by Maternal Incarceration", size(small) color(black)) ///
    legend(off) ///
    graphregion(color(white)) ///
	plotregion(margin(medium)) ///
	scheme(white_jet) ///
    plotregion(margin(medsmall))



