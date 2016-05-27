*******************************************
*
*Trim CCES 2008 Data to exclude other (non secret ballot) content
*
*******************************************

clear
set mem 500m
use "..\SourcesFilesNotInArchive\cces08_yal_output.dta", clear

* Drop other private content
drop yal301-yal384
drop yal3131-yal3204
drop yal321-yal322
drop yal391-yal3152
drop group seeyal372 block yal3151rev nameaint namebint namecint namedint nameeint partyanum partybnum partycnum partydnum partyenum posanum posbnum poscnum posdnum posenum
drop v202 v204 v205 v253 v257 v258

* Secret ballot questions only on Form B
keep if yal_form_pre == 2

* Save data
save "sourcedata\YaleCCES2008_SecretBallotItems.dta", replace

*******************************************
*
*Trim CCAP 2008 Data to exclude other (non "secret ballot") content
*
*******************************************

clear
use "..\SourcesFilesNotInArchive\ccap0001_yal_output_r2_1.dta", clear

drop profile50

* Drop other private content
drop oct_yal001-oct_yal023
drop oct_yal025-oct_randblurb
drop post_yal501-post_yal204

* Save data
save "sourcedata\YALECCAP2008_SecretBallotItems.dta", replace
