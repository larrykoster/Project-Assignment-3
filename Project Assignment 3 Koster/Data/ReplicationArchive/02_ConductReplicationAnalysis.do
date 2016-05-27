clear
set more off

*******************************************
*
* This .do file creates the analysis reported in "Is There A Secret Ballot: Ballot Secrecy Perceptions and Their Implications for Voting Behaviour"
* British Journal of Political Science
* Alan Gerber, Gregory Huber, David Doherty, Conor Dowling
*
* This version: January 23, 2012
*
*******************************************

local localpath=""

set mem 500m
use "`localpath'sourcedata\YaleCCES2008_SecretBallotItems.dta", clear

****************************
*
* RECODE DEMOGRAPHIC VARIABLES
*
****************************

*Race
tab  v211, gen(race)
gen white=race1
gen black=race2
gen hispanic=race3
gen otherrace=race4+race5+race6+race7+race8
label var white "Race: White (1=yes)"
label var black "Race: Black (1=yes)"
label var hispanic "Race: Hispanic (1=yes)"
label var otherrace "Race: Other Race (1=yes)"

*Gender
gen gender=v208-1
label var gender "Female (1=yes)"

*Age
gen age=2009-v207
gen age2=(age^2)/100
label var age "Age (Years)"
label var age2 "Age-squared/100"

*Region
tab v252, gen(regiond)
label var regiond1 "Region: Northeast"
label var regiond2 "Region: Midwest"
label var regiond3 "Region: South"
label var regiond4 "Region: West"
drop regiond4 
*State dummies
set more 1
tab v206, gen(state)

*Family Income
gen income=v246
replace income =. if income>15
gen incomemis = 0
replace incomemis = 1 if income==15 
label var incomemis "Income Missing"
label var income "Income (1=<10k; 14=>150k; 15=RF/Missing)"

*PID
recode cc307a (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (8=0) (*=.), gen(pid7)
label var pid7 "Party ID (-3=Str. Rep.; 3=Str. Dem.)"

recode cc307a (1 2=2) (3=1) (4=0) (5=-1) (6 7=-2) (8=0) (*=.), gen(pid5A)
label var pid5A "Party ID (Rep.-Dem. (collapse lean/weak))"

recode cc307a (1=2) (2=1) (3 4 5=0) (6=-1) (7=-2) (8=0) (*=.), gen(pid5B)
label var pid5B "Party ID (Rep.-Dem. (collapse lean/ind))"

tab pid5A, gen(d_pidA_)
tab pid5B, gen(d_pidB_)
tab pid7, gen(d_pidC_)
drop d_pidA_3 d_pidB_3 d_pidC_4
label var d_pidA_1 "Party: Strong Republican"
label var d_pidA_2 "Party: Weak Republican"
label var d_pidA_4 "Party: Weak Democrat"
label var d_pidA_5 "Party: Strong Democrat"

label var d_pidB_1 "Party: Strong Republican"
label var d_pidB_2 "Party: Weak Republican"
label var d_pidB_4 "Party: Weak Democrat"
label var d_pidB_5 "Party: Strong Democrat"

label var d_pidC_1 "Party: Strong Republican"
label var d_pidC_2 "Party: Weak Republican"
label var d_pidC_3 "Party: Lean Republican"
label var d_pidC_5 "Party: Lean Democrat"
label var d_pidC_6 "Party: Weak Democrat"
label var d_pidC_7 "Party: Strong Democrat"

*Strength of PID
gen pidstr7=abs(pid7)
label var pidstr7 "Strength PID (0=Indep/not sure, 3=Str. ID)"

*Education
gen educ=v213
label var educ "Education (1=No HS; 6=Post-grad)"
tab educ, gen(educD)
drop educD2
label var educD1 "Education: No HS"
label var educD3 "Education: Some college"
label var educD4 "Education: 2-year college"
label var educD5 "Education: 4-year college"
label var educD6 "Education: Post-graduate"

* Marital Status
gen married=(v214==1)|(v214==6)
label var married "Married/Domestic Partnership (1=yes)"
*Religious Attendance
recode v217 (1=6) (2=5) (3=4) (4=3) (5=2) (6=1) (*=.), gen(religatt)
label var religatt "Religious Attendance (1-6)"

*Ideology
recode v243 (5=-2) (4=-1) (3=0) (2=1) (1=2) (6=0) (*=.), gen(ideol5)
label var ideol5 "Ideology (-2=very cons.; 0=moderate/not sure; +2=very lib.)"

*Str. Ideology
gen ideolstr5=abs(ideol5)
label var ideolstr5 "Str. Ideology (0=moderate/not sure; +2=str. ideol)"

*Interest in Politics
recode v244 (1=4) (2=3) (3=2) (4=1) (*=.), gen(painterest)
label var painterest "Interest in News & Public Affairs (1-4)"
egen intSTDpa=std(painterest)

recode v245 (1=3) (2=2) (3=1) (*=.), gen(polinterest)
label var polinterest "Interest in Politics & Current Events (1-3)"
egen intSTDpol=std(polinterest)

egen intSTD=rmean(intSTDpa intSTDpol)
label var intSTD "Interest in Politics (standardized)"

alpha intSTDpa intSTDpol

recode cc329 (1=0) (2 3 4=1) (*=.), gen(union)
label var union "Union HH (1=yes)"

*Reported turnout in 2008
recode cc403 (1=0) (2=0) (3=0) (4=0) (5=1) (*=.), gen(repvote08)
label var repvote08 "Voted in 2008 (1=yes)"

*Mode of voting in 2008
recode cc405 (1=1) (2=2) (3=3) (*=.)
tab cc405, gen(modevote)
gen inpersoned = modevote1
label var inpersoned "Voted in person on election day"
gen inpersonearly = modevote2
label var inpersonearly "Voted in person early"
gen bymail = modevote3
replace bymail=0 if bymail==.
label var bymail "Voted by mail"

*Voter registration status
recode v203 (1=1) (2 3=0) (*=.), gen(regvote)
label var regvote "Registered to vote? (0=no/don't know, 1=yes)"

*** Issue attitudes
recode cc310 (1=1) (2=2) (3=3) (4=4) (*=.), gen(iss_abortion)
recode cc311 (1=5) (2=4) (3=3) (4=2) (5=1) (*=.), gen(iss_environ)
recode cc312 (1=1) (2=2) (3=3) (4=4) (*=.), gen(iss_privss)
recode cc313 (1=4) (2=3) (3=2) (4=1) (*=.), gen(iss_affact)
recode cc316* (1=3) (2=1) (3=2) (*=.)
label define issues 1 "oppose" 2 "don't know" 3 "support"
label values cc316* issues

label var iss_abortion "Oppose Abortion (1-4)"
label var iss_environ "Support Environ. Protection (1-5)"
label var iss_privss "Oppose Privatizing SS (1-4)"
label var iss_affact "Support Affirmative Action (1-4)"

****************************
*
* OUTCOME VARIABLE: 2008 PRESIDENTIAL VOTE CHOICE
*
****************************

*Vote Choice in 2008 (President)
recode cc410 (1=-1) (2=1) (3 5 6 7 99=0) (*=.), gen(presvote_all)
label var presvote_all "Presidential Vote Choice (-1=McCain; 0=Other/Didn't Vote; +1=Obama)"
recode cc410 (1=-1) (2=1) (3 5 6 7=0) (*=.), gen(presvote_noabstain)
label var presvote_noabstain "Presidential Vote Choice (-1=McCain; 0=Other; +1=Obama)"
recode cc410 (1=-1) (2=1) (99=0) (*=.), gen(presvote_nothird)
label var presvote_nothird "Presidential Vote Choice (-1=McCain; 0=Didn't Vote; +1=Obama)"
recode cc410 (1=0) (2=1) (*=.), gen(presvote2pty_logit)
recode cc410 (1=-1) (2=1) (*=.), gen(presvote2pty)
label var presvote2pty_logit "Presidential Vote Choice (0=McCain; 1=Obama)"
label var presvote2pty "Presidential Vote Choice (-1=McCain; 1=Obama)"

gen obamavote=(presvote2pty+1)/2
label var obamavote "Presidential Vote Choice (0=McCain; 1=Obama [else missing])"

****************************
*
* SECRET BALLOT (OR KEEP VOTE/PREFERENCE SECRET)
*
****************************

**All scored with idea that ballot is truly secret scored high (i.e., more skeptical view that ballot is NOT secret low)

*Psychological
recode yal361 (1=0) (2=1) (*=.), gen(sb_chktrnout)
label var sb_chktrnout "Someone will check turnout? (1=No)"
recode yal362 (1=1) (2=0) (*=.), gen(sb_choisec)
label var sb_choisec "Choices kept secret? (1=Yes)"
recode yal363 (1=5) (2=4) (3=3) (4=2) (5=1) (*=.), gen (sb_difffind)
label var sb_difffind "Difficult to find out? (1=not difficult, 5=impossible)"
recode yal364 (1=0) (2=1) (*=.), gen(sb_findout)
label var sb_findout "Ever find out? (1=No)"
recode yal365 (1=1) (2=2) (3=3) (4=4) (5=5) (*=.), gen(sb_findoutoft)
label var sb_findoutoft "Find out often? (1=always, 5=never)"

*Social
recode yal366 (1=1) (2=2) (3=3) (4=4) (5=5) (*=.), gen(sb_mentvote)
label var sb_mentvote "Mention vote? (1=almost all of the time, 5=never or almost never)"
recode yal367 (1=1) (2=2) (3=3) (4=4) (5=5) (*=.), gen(sb_askvote)
label var sb_askvote "People ask about vote? (1=almost all of the time, 5=never or almost never)"
recode yal368 (1=1) (2=2) (3=3) (4=4) (5=5) (*=.), gen(sb_namecand)
label var sb_namecand "Name a candidate? (1=almost all of the time, 5=never or almost never)"
recode yal369 (1=1) (2=2) (3=3) (4=4) (5=5) (*=.), gen(sb_telltru)
label var sb_telltru "Tell truth about preference? (1=almost all of the time, 5=never or almost never)"
recode yal370 (1=-1) (2=1) (3=0) (*=.), gen(sb_trudis)
label var sb_trudis "Truth when disagree? (-1=yes, 0=change subject, 1=no)"
recode yal371 (1=1) (2=0) (*=.), gen(sb_keepsec)
label var sb_keepsec "Could keep secret? (1=yes)"

*PSYCH AND SOCIAL SECRECY
gen sb_both=(sb_keepsec*sb_choisec==1)
label var sb_both "Psychological and Social Secrecy (1=yes)"

*** CROSS TAB CATEGORIES
gen intCOL=(intSTD!=.)
replace intCOL=0 if intSTD<.5

gen religattCOL=.
replace religattCOL=1 if religatt==1 | religatt==2
replace religattCOL=2 if religatt==3 | religatt==4
replace religattCOL=3 if religatt==5 | religatt==6

sum age, det
gen ageCOL=. if age==.
replace ageCOL=1 if age<40
replace ageCOL=2 if age>=40 & age<61
replace ageCOL=3 if age>=61 

sum educ, det
gen educCOL=. if educ==.
replace educCOL=1 if educ<3
replace educCOL=2 if educ>=3 & educ<5
replace educCOL=3 if educ>=5

sum income if incomemis!=1, det
gen incomeCOL=. if incomemis==1
replace incomeCOL=1 if income<=6
replace incomeCOL=2 if income>6 & income<11
replace incomeCOL=3 if income>=11 & income<15

tab sb_choi sb_ment [aw=v201], cel

****************************
*
* GENERATED INSAMPLE INDICATOR
*
****************************

gen insample=1
foreach i in sb_choisec sb_difffind sb_findout sb_mentvote sb_askvote sb_namecand sb_telltru sb_trudis sb_keepsec {
reg `i' presvote_all white gender age age2 religatt educ intSTD pid7 ideol5 union income incomemis [aw=v201]
replace insample=0 if e(sample)==0
}
drop if insample==0

****************************
*
* CREATE INDICES
*
****************************

* Issue Index
alpha iss_*, std gen(ideol_index)
label var ideol_index "Index of issue attitudes (conservative-liberal)"

*SB Indexes
*** TABLE A2 (PRINCIPLE COMPONENTS ANALYSIS)
factor sb_choisec sb_difffind sb_findout sb_mentvote sb_askvote sb_namecand sb_telltru sb_trudis sb_keepsec if insample==1 [aw=v201], pcf
rotate, factor(2)
predict socsecrecy psychsecrecy
label var socsecrecy "Social Secrecy (PC)"
label var psychsecrecy "Psychological Secrecy (PC)"

****************************
*
* POST ELECTION QUESTIONS
*
****************************

recode yal441 (1=1) (2=2) (3=3) (4=4) (5=5) (*=.), gen(sb_told_num)
recode yal442 (1=1) (2=0) (9=9) (*=.), gen(sb_bossask) 

tab sb_told_num [aw=v201] 
tab sb_bossask [aw=v201] 
tab union sb_bossask [aw=v201], row

reg sb_told_num psychsecrecy socsecrecy
outreg using "logs\robustness_postregressions.out", 3aster se bracket tdec(3) replace 
reg sb_told_num psychsecrecy socsecrecy union pid7 white black hispanic otherrace gender age religatt educ income incomemis ideol5 intSTD 
outreg using "logs\robustness_postregressions.out", 3aster se bracket tdec(3) append 
reg sb_bossask psychsecrecy socsecrecy if sb_bossask!=9
outreg using "logs\robustness_postregressions.out", 3aster se bracket tdec(3) append 
reg sb_bossask psychsecrecy socsecrecy union pid7 white black hispanic otherrace gender age religatt educ income incomemis ideol5 intSTD if sb_bossask!=9
outreg using "logs\robustness_postregressions.out", 3aster se bracket tdec(3) append 

****************************
*
* ANALYSIS
*
****************************

*** TABLE A1 (SUMMARY STATS)
sum presvote_all obamavote psychsecrecy socsecrecy union pid7 white black hispanic otherrace gender age religatt educ income incomemis ideol5 ideol_index intSTD [aw=v201] if insample==1, sep(0)

*** Table 1 (MARGINALS)
matrix marginals=.
foreach i in sb_choisec sb_difffind sb_findout sb_findoutoft sb_mentvote sb_askvote sb_namecand sb_telltru sb_trudis sb_keepsec {
tab `i' [aw=v201] if insample==1, gen(D_`i'_)
local rows=r(r)
forvalues r=1/`rows'{
qui sum D_`i'_`r' [aw=v201] if insample==1
matrix marginals=marginals\r(mean)
}
}

*Vector of values for Table 1.
matrix list marginals

*** TABLE A3 (LOW SECRECY MARGINALS [BY GROUP])
foreach i in sb_choisec sb_difffind sb_findout sb_findoutoft sb_mentvote sb_askvote sb_namecand sb_telltru sb_trudis sb_keepsec {
qui tab `i' [aw=v201] if insample==1
local rows=r(r)
if `rows'>3{
qui gen LS_`i'=D_`i'_1+D_`i'_2
}
else{
qui gen LS_`i'=D_`i'_1
}
qui sum LS_`i' [aw=v201] if insample==1 & white==1
matrix item_`i'=r(mean)
qui sum LS_`i' [aw=v201] if insample==1 & black==1
matrix item_`i'=item_`i',r(mean)
qui sum LS_`i' [aw=v201] if insample==1 & hispanic==1
matrix item_`i'=item_`i',r(mean)
forvalues p=0/1{
qui sum LS_`i' [aw=v201] if insample==1 & gender==`p'
matrix item_`i'=item_`i',r(mean)
}
forvalues p=1/3{
qui sum LS_`i' [aw=v201] if insample==1 & ageCOL==`p'
matrix item_`i'=item_`i',r(mean)
}
forvalues p=1/3{
qui sum LS_`i' [aw=v201] if insample==1 & educCOL==`p'
matrix item_`i'=item_`i',r(mean)
}
forvalues p=1/3{
qui sum LS_`i' [aw=v201] if insample==1 & incomeCOL==`p'
matrix item_`i'=item_`i',r(mean)
}
forvalues p=0/1{
qui sum LS_`i' [aw=v201] if insample==1 & intCOL==`p'
matrix item_`i'=item_`i',r(mean)
}

qui sum LS_`i' [aw=v201] if insample==1 & inpersoned==1
matrix item_aux_`i'=r(mean)
qui sum LS_`i' [aw=v201] if insample==1 & inpersonearly==1
matrix item_aux_`i'=item_aux_`i',r(mean)
qui sum LS_`i' [aw=v201] if insample==1 & bymail==1
matrix item_aux_`i'=item_aux_`i',r(mean)

qui sum LS_`i' [aw=v201] if insample==1 & married==0
matrix item_aux_`i'=item_aux_`i',r(mean)
qui sum LS_`i' [aw=v201] if insample==1 & married==1
matrix item_aux_`i'=item_aux_`i',r(mean)

if "`i'"=="sb_choisec" {
matrix full=item_`i'
matrix full_aux=item_aux_`i'
}
else{
matrix full=full\item_`i'
matrix full_aux=full_aux\item_aux_`i'
}
}
svmat full, names(VARS_full)
outsheet VARS_full* using logs\TableA03_marg_by_group.csv,comma replace
drop D_* LS_* VARS_*
svmat full_aux, names(VARS_full_aux)
outsheet VARS_full_aux* using logs\Unreported_marg_by_group_aux.csv,comma replace
drop VARS_*

*** TABLE 2 (VOTE CHOICE MODEL)
foreach i in black hispanic otherrace gender age age2 married religatt educ intSTD income incomemis{
local label: variable label `i'
gen `i'U=`i'*union
gen `i'PID=`i'*pid7
label var `i'U "`label' x Union"
label var `i'PID "`label' x PID"
}

foreach i in ideol_index ideol5 {
local label: variable label `i'
gen `i'U=`i'*union
label var `i'U "`label' x Union"
}

foreach i in black hispanic otherrace gender age age2 married religatt educ intSTD income incomemis d_pidC_1 d_pidC_2 d_pidC_3 d_pidC_5 d_pidC_6 d_pidC_7 ideol_index ideol5 union {
local label: variable label `i'
gen socX`i'=`i'*socsecrecy
gen psychX`i'=`i'*psychsecrecy
label var socX`i' "Social Secrecy x `label'"
label var psychX`i' "Psychological Secrecy x `label'"
}

* SET CONTROL VARIABLES
local controls="d_pidC* union black hispanic otherrace gender age age2 married religatt educD* income incomemis intSTD ideol5 ideol_index"

*FULL MODEL
reg presvote_all psychsecrecy socsecrecy socXunion psychXunion psychXd_pidC* socXd_pidC* *U *PID `controls' state* if insample==1 [aw=v201], r
outreg psychsecrecy socsecrecy socXunion psychXunion psychXd_pidC* socXd_pidC* *U *PID `controls' using "logs\TableA04_predictvote_fullmodel.out", 3aster se bracket replace tdec(3) 
reg presvote_all psychsecrecy socsecrecy socX* psychX* *U *PID `controls' state* if insample==1 [aw=v201], r
outreg psychsecrecy socsecrecy socX* psychX* *U *PID `controls' using "logs\TableA04_predictvote_fullmodel.out", 3aster se bracket append tdec(3) 

*BASIC (NAIVE) MODEL
reg presvote_all psychsecrecy socsecrecy d_pidC* `controls' state* if insample==1 [aw=v201], r
outreg psychsecrecy socsecrecy `controls' using "logs\Table02_predictvote.out", 3aster se bracket replace tdec(3) 

*INTERACTIONS OF INTEREST ONLY
reg presvote_all psychsecrecy socsecrecy socXunion psychXunion psychXd_pidC* socXd_pidC* `controls' state* if insample==1 [aw=v201], r
lincom psychsec+psychXunion	
lincom 2*(psychsec+psychXunion)
lincom socsec+socXunion 
lincom (socsecrecy+socXd_pidC_2)
lincom (socsecrecy+socXd_pidC_6)
test socXd_pidC_1 socXd_pidC_2 socXd_pidC_3 socXd_pidC_5 socXd_pidC_6 socXd_pidC_7 
local pidXsoc=r(p)
test psychXd_pidC_1 psychXd_pidC_2 psychXd_pidC_3 psychXd_pidC_5 psychXd_pidC_6 psychXd_pidC_7 
local pidXpsych=r(p)
outreg psychsecrecy socsecrecy psychXunion socXunion psychXd_pidC* socXd_pidC* `controls' using "logs\Table02_predictvote.out", 3aster se bracket append tdec(3) addstat("Social Secrecy x PID (Prob>F)", `pidXsoc', "Psychological Secrecy x PID (Prob>F)", `pidXpsych') 

*ROBUSTNESS 1
reg presvote_all psychsecrecy socsecrecy socXunion psychXunion psychXd_pidC* socXd_pidC* *U *PID `controls' state* if insample==1 [aw=v201], r
test blackU blackPID hispanicU hispanicPID otherraceU otherracePID genderU genderPID ageU agePID age2U age2PID marriedU marriedPID religattU religattPID educU educPID intSTDU intSTDPID incomeU incomePID incomemisU incomemisPID 
local pidunion=r(p)
test socXd_pidC_1 socXd_pidC_2 socXd_pidC_3 socXd_pidC_5 socXd_pidC_6 socXd_pidC_7 
local pidXsoc=r(p)
test psychXd_pidC_1 psychXd_pidC_2 psychXd_pidC_3 psychXd_pidC_5 psychXd_pidC_6 psychXd_pidC_7 
local pidXpsych=r(p)
outreg psychsecrecy socsecrecy psychXunion socXunion psychXd_pidC* socXd_pidC* `controls' using "logs\Table02_predictvote.out", 3aster se bracket append tdec(3) addstat("Union and PID interactions (Prob>F)", `pidunion', "Social Secrecy x PID (Prob>F)", `pidXsoc', "Psychological Secrecy x PID (Prob>F)", `pidXpsych') 

*FULL MODEL
reg presvote_all psychsecrecy socsecrecy socX* psychX* *U *PID `controls' state* if insample==1 [aw=v201], r
test blackU blackPID hispanicU hispanicPID otherraceU otherracePID genderU genderPID ageU agePID age2U age2PID marriedU marriedPID religattU religattPID educU educPID intSTDU intSTDPID incomeU incomePID incomemisU incomemisPID 
local pidunion=r(p)
test socXd_pidC_1 socXd_pidC_2 socXd_pidC_3 socXd_pidC_5 socXd_pidC_6 socXd_pidC_7 
local pidXsoc=r(p)
test psychXd_pidC_1 psychXd_pidC_2 psychXd_pidC_3 psychXd_pidC_5 psychXd_pidC_6 psychXd_pidC_7 
local pidXpsych=r(p)
test socXblack psychXblack socXhispanic psychXhispanic socXotherrace psychXotherrace socXgender psychXgender socXage psychXage socXage2 psychXage2 socXmarried psychXmarried socXreligatt psychXreligatt socXeduc psychXeduc socXintSTD psychXintSTD socXincome psychXincome socXincomemis psychXincomemis
outreg psychsecrecy socsecrecy psychXunion socXunion psychXd_pidC* socXd_pidC* `controls' using "logs\Table02_predictvote.out", 3aster se bracket append tdec(3) adec(7) addstat("Union and PID interactions (Prob>F)", `pidunion', "Secrecy interactions (Prob>F)", r(p), "Social Secrecy x PID (Prob>F)", `pidXsoc', "Psychological Secrecy x PID (Prob>F)", `pidXpsych') 

*INTERACTIONS OF INTEREST ONLY (OLOGIT)
local controls="d_pidC* union black hispanic otherrace gender age age2 married religatt educD* income incomemis intSTD ideol5 ideol_index"
ologit presvote_all psychsecrecy socsecrecy socXunion psychXunion psychXd_pidC* socXd_pidC* `controls' if insample==1 [aw=v201], r
test socXd_pidC_1 socXd_pidC_2 socXd_pidC_3 socXd_pidC_5 socXd_pidC_6 socXd_pidC_7 
local pidXsoc=r(p)
test psychXd_pidC_1 psychXd_pidC_2 psychXd_pidC_3 psychXd_pidC_5 psychXd_pidC_6 psychXd_pidC_7 
local pidXpsych=r(p)
outreg psychsecrecy socsecrecy psychXunion socXunion psychXd_pidC* socXd_pidC* `controls' using "logs\Table02_predictvote.out", 3aster se bracket append tdec(3) addstat("Social Secrecy x PID (Prob>F)", `pidXsoc', "Psychological Secrecy x PID (Prob>F)", `pidXpsych') 

* EXCLUDE 3rd PTY
reg presvote_nothird psychsecrecy socsecrecy socXunion psychXunion psychXd_pidC* socXd_pidC* `controls' state* if insample==1 [aw=v201], r
test socXd_pidC_1 socXd_pidC_2 socXd_pidC_3 socXd_pidC_5 socXd_pidC_6 socXd_pidC_7 
local pidXsoc=r(p)
test psychXd_pidC_1 psychXd_pidC_2 psychXd_pidC_3 psychXd_pidC_5 psychXd_pidC_6 psychXd_pidC_7 
local pidXpsych=r(p)
outreg psychsecrecy socsecrecy psychXunion socXunion psychXd_pidC* socXd_pidC* `controls' using "logs\Table02_predictvote.out", 3aster se bracket append tdec(3) addstat("Social Secrecy x PID (Prob>F)", `pidXsoc', "Psychological Secrecy x PID (Prob>F)", `pidXpsych') 

* 2 Party Voters Only 
reg presvote2pty psychsecrecy socsecrecy socXunion psychXunion psychXd_pidC* socXd_pidC* `controls' state* if insample==1 [aw=v201], r
test socXd_pidC_1 socXd_pidC_2 socXd_pidC_3 socXd_pidC_5 socXd_pidC_6 socXd_pidC_7 
local pidXsoc=r(p)
test psychXd_pidC_1 psychXd_pidC_2 psychXd_pidC_3 psychXd_pidC_5 psychXd_pidC_6 psychXd_pidC_7 
local pidXpsych=r(p)
outreg psychsecrecy socsecrecy psychXunion socXunion psychXd_pidC* socXd_pidC* `controls' using "logs\Table02_predictvote.out", 3aster se bracket append tdec(3) addstat("Social Secrecy x PID (Prob>F)", `pidXsoc', "Psychological Secrecy x PID (Prob>F)", `pidXpsych') 

****************************
*
* SIDE NOTES IN TEXT
*
****************************

*restrict to registered voters
tab sb_choisec [aw=v201] if insample==1 & v203==1
tab sb_difffind [aw=v201] if insample==1 & v203==1

*cross secrecy types
tab sb_mentvote sb_choisec [aw=v201] if insample==1, col
pwcorr sb_mentvote sb_choisec [aw=v201] if insample==1, sig
pwcorr sb_mentvote sb_choisec [aw=v201] if insample==1, sig

*** OLS PREDICTING SECRECY INDICES
replace incomeCOL=4 if incomemis==1
tab ageCOL, gen(ageD)
tab educCOL, gen(educP)
tab incomeCOL, gen(incomeD)
tab intCOL, gen(intD)
drop ageD3 educP3 incomeD3 intD2 

reg psychsecrecy black hispanic otherrace gender ageD* educP* incomeD* intD* state*  [aw=v201] if insample==1, r
reg socsecrecy black hispanic otherrace gender ageD* educP* incomeD* intD* state*  [aw=v201] if insample==1, r

********************************************************
***** REGRESSIONS USING ADDITIVE SCALES ****************
********************************************************
drop psychsecrecy socsecrecy socX* psychX* 

alpha sb_choisec sb_difffind sb_findout, i std gen(psychsecrecy)
alpha sb_mentvote sb_askvote sb_namecand sb_telltru sb_trudis sb_keepsec, i std gen(socsecrecy)

label var socsecrecy "Social Secrecy (PC)"
label var psychsecrecy "Psychological Secrecy (PC)"

foreach i in black hispanic otherrace gender age age2 married religatt educ intSTD income incomemis d_pidC_1 d_pidC_2 d_pidC_3 d_pidC_5 d_pidC_6 d_pidC_7 ideol5 union {
local label: variable label `i'
gen socX`i'=`i'*socsecrecy
gen psychX`i'=`i'*psychsecrecy
label var socX`i' "Social Secrecy x `label'"
label var psychX`i' "Psychological Secrecy x `label'"
}

*BASIC (NAIVE) MODEL
reg presvote_all psychsecrecy socsecrecy d_pidC* union black hispanic otherrace gender age age2 married religatt educD* income intSTD ideol5 incomemis state* if insample==1 [aw=v201], r
outreg psychsecrecy socsecrecy d_pidC* union black hispanic otherrace gender age age2 married religatt educD* income intSTD ideol5 incomemis using "logs\robustness_predictvote_add.out", 3aster se bracket replace tdec(3) 

*INTERACTIONS OF INTEREST ONLY
reg presvote_all psychsecrecy socsecrecy socXunion psychXunion psychXd_pidC* socXd_pidC* d_pidC* union black hispanic otherrace gender age age2 married religatt educD* income intSTD ideol5 incomemis state* if insample==1 [aw=v201], r
lincom psychsec+psychXunion
lincom 2*(psychsec+psychXunion)
lincom socsec+socXunion 
lincom (socsecrecy+socXd_pidC_2)
lincom (socsecrecy+socXd_pidC_6)
test socXd_pidC_1 socXd_pidC_2 socXd_pidC_3 socXd_pidC_5 socXd_pidC_6 socXd_pidC_7 
local pidXsoc=r(p)
test psychXd_pidC_1 psychXd_pidC_2 psychXd_pidC_3 psychXd_pidC_5 psychXd_pidC_6 psychXd_pidC_7 
local pidXpsych=r(p)
outreg psychsecrecy socsecrecy psychXunion socXunion psychXd_pidC* socXd_pidC* d_pidC* union black hispanic otherrace gender age age2 married religatt educD* income intSTD ideol5 incomemis using "logs\robustness_predictvote_add.out", 3aster se bracket append tdec(3) addstat("Social Secrecy x PID (Prob>F)", `pidXsoc', "Psychological Secrecy x PID (Prob>F)", `pidXpsych') 

*ROBUSTNESS 1
reg presvote_all psychsecrecy socsecrecy socXunion psychXunion psychXd_pidC* socXd_pidC* *U *PID d_pidC* union black hispanic otherrace gender age age2 married religatt educD* income intSTD ideol5 incomemis state* if insample==1 [aw=v201], r
test blackU blackPID hispanicU hispanicPID otherraceU otherracePID genderU genderPID ageU agePID age2U age2PID marriedU marriedPID religattU religattPID educU educPID intSTDU intSTDPID incomeU incomePID incomemisU incomemisPID ideol5U
local pidunion=r(p)
test socXd_pidC_1 socXd_pidC_2 socXd_pidC_3 socXd_pidC_5 socXd_pidC_6 socXd_pidC_7 
local pidXsoc=r(p)
test psychXd_pidC_1 psychXd_pidC_2 psychXd_pidC_3 psychXd_pidC_5 psychXd_pidC_6 psychXd_pidC_7 
local pidXpsych=r(p)
outreg psychsecrecy socsecrecy psychXunion socXunion psychXd_pidC* socXd_pidC* d_pidC* union black hispanic otherrace gender age age2 married religatt educD* income intSTD ideol5 incomemis using "logs\robustness_predictvote_add.out", 3aster se bracket append tdec(3) addstat("Union and PID interactions (Prob>F)", `pidunion', "Social Secrecy x PID (Prob>F)", `pidXsoc', "Psychological Secrecy x PID (Prob>F)", `pidXpsych') 

*FULL MODEL
reg presvote_all psychsecrecy socsecrecy socX* psychX* *U *PID d_pidC* union black hispanic otherrace gender age age2 married religatt educD* income intSTD ideol5 incomemis state* if insample==1 [aw=v201], r
test blackU blackPID hispanicU hispanicPID otherraceU otherracePID genderU genderPID ageU agePID age2U age2PID marriedU marriedPID religattU religattPID educU educPID intSTDU intSTDPID incomeU incomePID incomemisU incomemisPID ideol5U
local pidunion=r(p)
test socXd_pidC_1 socXd_pidC_2 socXd_pidC_3 socXd_pidC_5 socXd_pidC_6 socXd_pidC_7 
local pidXsoc=r(p)
test psychXd_pidC_1 psychXd_pidC_2 psychXd_pidC_3 psychXd_pidC_5 psychXd_pidC_6 psychXd_pidC_7 
local pidXpsych=r(p)
test socXblack psychXblack socXhispanic psychXhispanic socXotherrace psychXotherrace socXgender psychXgender socXage psychXage socXage2 psychXage2 socXmarried psychXmarried socXreligatt psychXreligatt socXeduc psychXeduc socXintSTD psychXintSTD socXideol5 psychXideol5 socXincome psychXincome socXincomemis psychXincomemis
outreg psychsecrecy socsecrecy psychXunion socXunion psychXd_pidC* socXd_pidC* d_pidC* union black hispanic otherrace gender age age2 married religatt educD* income intSTD ideol5 incomemis using "logs\robustness_predictvote_add.out", 3aster se bracket append tdec(3) adec(7) addstat("Union and PID interactions (Prob>F)", `pidunion', "Secrecy interactions (Prob>F)", r(p), "Social Secrecy x PID (Prob>F)", `pidXsoc', "Psychological Secrecy x PID (Prob>F)", `pidXpsych') 

****************************
*
* TRUST GOV ANALYSIS USING CCAP 2008 DATA
*
****************************

include "02_Sub_ConductReplicationAnalysis.do"

