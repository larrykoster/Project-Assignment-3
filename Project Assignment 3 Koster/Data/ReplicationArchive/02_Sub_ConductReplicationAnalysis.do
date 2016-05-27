clear
set more 1
use "`localpath'sourcedata\YALECCAP2008_SecretBallotItems.dta", clear

**********************************
*
* OUTCOME VARIABLE
*
**********************************
* Reported Vote Choices
recode pcap600 (1=1) (2=-1) (3 4=0) (*=.), gen(preschoice)
label var preschoice "Presidential Vote Choice (-1=McCain; 0=Other/Didn't Vote; +1=Obama)"

**********************************
*
* Recode DEMOGRAPHICS 
*
**********************************
* Party ID
recode bcap8 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (8=.)
egen pid7=rmean(bcap8)
label var pid7 "Party ID (-3=Str. Rep.; 3=Str. Dem.)"
tab pid7, gen(d_pidC_)
label var d_pidC_1 "Party: Strong Republican"
label var d_pidC_2 "Party: Weak Republican"
label var d_pidC_3 "Party: Lean Republican"
label var d_pidC_5 "Party: Lean Democrat"
label var d_pidC_6 "Party: Weak Democrat"
label var d_pidC_7 "Party: Strong Democrat"

* Ideology
***CODING ERROR. ORIGINAL ANALYSIS USED egen COMMAND TO IMPROPERLY COMBINE TWO IDEOLOGY MEASURES*** 
***CORRECTED MEASURE USING SINGLE ITEM FROM BASELINE REDUCES N from 655 to 626, BUT DOES NOT ALTER SUBSTANTIVE FINDINGS--ALL TRUST***
***IN GOVERNMENT INTERACTIONS FALL SHORT OF STATISTICAL SIGNIFICANCE***
/* ERRANT CODE HERE
recode bcap700s (1=2) (2=1) (3=0) (4=-1) (5=-2) (*=.)
egen ideol5=rmean(*cap700s)
*/
**CORRECTED CODE
recode bcap700s (1=2) (2=1) (3=0) (4=-1) (5=-2) (*=.), gen(ideol5)
label var ideol5 "Ideology (-2=very cons.; 0=moderate/not sure; +2=very lib.)"

* Zip Code
*rename profile50 zip

* State
recode profile66 (.=0)
tab profile66, gen(state)
gen statemis=0
replace statemis=1 if profile66==0

* Region
gen regwest=0
gen regmidwest=0
gen regsouth=0
gen regnortheast=0
gen regpacific=0
replace regwest=1 if profile66==4 | profile66==6 | profile66==8 | profile66==16 | profile66==30 | profile66==32 | profile66==35 | profile66==38 | profile66==41 | profile66==46 | profile66==49 | profile66==53 | profile66==56
replace regmidwest=1 if profile66==17 | profile66==18 | profile66==19 | profile66==20 | profile66==26 | profile66==27 | profile66==29 | profile66==31 | profile66==39 | profile66==55
replace regsouth=1 if profile66==1 | profile66==5 | profile66==12 | profile66==13 | profile66==21 | profile66==22 | profile66==28 | profile66==37 | profile66==40 | profile66==45 | profile66==47 | profile66==48 | profile66==51 | profile66==54 
replace regnortheast=1 if profile66==9 | profile66==10 | profile66==11 | profile66==23 | profile66==24 | profile66==25 | profile66==33 | profile66==34 | profile66==36 | profile66==42 | profile66==44 | profile66==50 
replace regpacific=1 if profile66==2 | profile66==15
label var regnortheast "Northeast"
label var regmidwest "Midwest"
label var regsouth "South"

* Battleground State
gen bgstatemis = 0
replace bgstatemis = 1 if profile67 ==.
recode profile67 (1=1) (2 .=0), gen(bgstate)
label var bgstate "Battleground state (Yes=1)"

* Gender
recode profile54 (1=0) (2=1), gen(gender)
label var gender "Female (1=yes)"

* Race
rename profile55 race
gen white = 0 if race ~=.
replace white = 1 if race==1
gen black = 0 if race ~=.
replace black = 1 if race==2
gen hispanic = 0 if race ~=.
replace hispanic = 1 if race==3
gen otherrace = 0 if race ~=.
replace otherrace = 1 if race==4 | race==5 | race==6 | race==7 | race==8
label var white "Race: White (1=yes)"
label var black "Race: Black (1=yes)"
label var hispanic "Race: Hispanic (1=yes)"
label var otherrace "Race: Other Race (1=yes)"

* Education
rename profile57 educ
label var educ "Education (1=No HS; 6=Post-grad)"
tab educ, gen(educD)
drop educD2
label var educD1 "Education: No HS"
label var educD3 "Education: Some college"
label var educD4 "Education: 2-year college"
label var educD5 "Education: 4-year college"
label var educD6 "Education: Post-graduate"

* Married
gen married=(profile58==1)|(profile58==6)
label var married "Married/Domestic Partnership (1=yes)"

* Family Income
gen income=profile59
gen incomemis = 0
replace incomemis = 1 if income==15 | income==.
label var incomemis "Income Missing"
recode income (.=15)
label var income "Income (1=<10k; 14=>150k; 15=RF/Missing)"

tab income, gen(incomeD)

* Religious Services Frequency
recode profile30 (9=1) (8=2) (6 7=3) (3 4 5=4) (2=5) (1=6) (*=.), gen(religatt)
label var religatt "Religious Attendance (1-6)"
tab religatt, gen(religattD)

* Age
rename profile51 birthyr
label var birthyr "birth year"
gen age=2009-birthyr 
label var age "Age (Years)"
forvalues i=1/4{
gen agecat`i'=0
}
replace agecat1=1 if age<31
label var agecat1 "Age (<31)"
replace agecat2=1 if age>30 & age<51
label var agecat2 "Age (31-50)"
replace agecat3=1 if age>50 & age<66
label var agecat3 "Age (51-65)"
replace agecat4=1 if age>65
label var agecat4 "Age (66+)"

gen age2=(age^2)/100
label var age2 "Age-squared/100"

* Union 
recode pcap12_2 (1=1) (2=0) (*=.), gen(union)
label var union "Union HH (1=yes)"

recode bcap813 (8=.)
egen intSTD=std(bcap813*-1)
label var intSTD "Interest in Politics (standardized)"

recode oct_yal024 (8 9=.)
label var oct_yal024 "Trust in Government (1=almost never;4=just about always)"
egen trustgov=std(oct_yal024)
label var trustgov "Trust in Government (Standardized, Mean=0, SD=1)"

foreach i in d_pidC_1 d_pidC_2 d_pidC_3 d_pidC_5 d_pidC_6 d_pidC_7 union {
local label: variable label `i'
gen trustX`i'=`i'*trustgov
label var trustX`i' "Trust in Government x `label'"
}

replace religatt=. if religatt==0

*** TABLE
reg preschoice trustgov trustX* d_pidC_1 d_pidC_2 d_pidC_3 d_pidC_5 d_pidC_6 d_pidC_7 union black hispanic otherrace gender age age2 married religatt educD* income intSTD ideol5 incomemis state* [aw=weight], r 
outreg trustgov trustX* d_pidC_1 d_pidC_2 d_pidC_3 d_pidC_5 d_pidC_6 d_pidC_7 union black hispanic otherrace gender age age2 married religatt educD* income intSTD ideol5 incomemis using "logs\Table03_ccap", 3aster se bracket replace tdec(3) 

