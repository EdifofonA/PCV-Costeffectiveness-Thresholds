 * 20 March 2023
* Stata/SE version 16.1


*For NC:
// cd  "/Users/ncarvalho/OneDrive - The University of Melbourne/PCV Cost-effectiveness Thresholds/Analysis/"


***PCV CEA analysis
import excel "data\publications.xlsx", sheet("data") firstrow case(lower) clear
//drop if strpos(firstauthor,"Shafie") > 0
*remove two evaluations (Edi - see mistake - see in excel file for reason)
//drop if uniqueid == "5103" | uniqueid == "5104"

*clean up
destring vaxpriceinterventionusd vaxpricecomparatorusd icerusd ochalekhigh ochaleklow woodshigh woodslow switch*, replace

foreach var of varlist herdeffects herdeffectolder seroreplace asiapacific efficacypneumonia efficacyipd efficacyotitis budgetimpact {
	replace `var' = "1" if `var' == "Yes"
	replace `var' = "0" if `var' == "No"
	destring `var', replace
}

label define YESNO 1 "Yes" 0 "No"
label values herdeffects herdeffectolder seroreplace asiapacific efficacyipd efficacyotitis budgetimpact efficacypneumonia YESNO

replace income = "1" if income == "L"
replace income = "2" if income == "LM"
replace income = "3" if income == "UM"
destring income, replace
label define INCOME 1 "Low income" 2 "Lower-middle income" 3 "Upper-middle income"
label values income INCOME

//gen perspectiveCat = .
//replace perspectiveCat = 2 if perspective == "Societal"
//replace perspectiveCat = 1 if (perspective == "Healthcare" | perspective == "Payer" | perspective == "Healthcare/Payer")
//label define PERSPECTIVE 1 "Healthcare/Payer" 2 "Societal"
//label values perspectiveCat PERSPECTIVE


replace perspective = "0" if perspective == "Societal"
replace perspective = "1" if (perspective == "Healthcare" | perspective == "Payer" | perspective == "Healthcare/Payer")
label define PERSPECTIVE  0 "Societal" 1 "Healthcare/Payer"
destring perspective, replace
label values perspective PERSPECTIVE




egen comparison = concat(intervention comparator), punct(" vs ")
gen comparisonCat = .
replace comparisonCat = 1 if comparison == "PCV7 vs NoVax"
replace comparisonCat = 2 if comparison == "PCV10 vs NoVax"
replace comparisonCat = 3 if comparison == "PCV13 vs NoVax"
replace comparisonCat = 4 if comparison == "PCV13 vs PCV10"
replace comparisonCat = 5 if (comparison == "PCV10 vs PCV7" | comparison == "PCV10 vs PCV7" | comparison ==  "PCV13 vs PCV7" | comparison == "PCV9 vs NoVax")

label define COMPARISON 1 "PCV7 vs NoVax" 2 "PCV10 vs NoVax" 3 "PCV13 vs NoVax" 4 "PCV13 vs PCV10" 5 "Others"
label values comparisonCat COMPARISON

gen dosenumCat =.
replace dosenumCat = 1 if dosenum == "3"
replace dosenumCat = 2 if dosenum == "4"
replace dosenumCat = 3 if (dosenum == "0" | dosenum == "1" | dosenum == "2" | dosenum == ".")

label define DOSE 1 "Three doses" 2 "Four doses" 3 "Others/unknown"
label values dosenumCat DOSE


foreach var of varlist onegdppcdecision threegdppcdecision ochalekhighdecision ochaleklowdecision woodshighdecision woodslowdecision studythresholddecision {
	replace `var' = "1" if `var' == "Cost-Saving"
	replace `var' = "2" if `var' == "Cost-Effective"
	replace `var' = "3" if `var' == "Not Cost-Effective"
	destring `var', replace
}

label define DECISIONCat 1 "Cost-Saving" 2 "Cost-Effective" 3 "Not Cost-Effective"
label values onegdppcdecision threegdppcdecision ochalekhighdecision ochaleklowdecision woodshighdecision woodslowdecision studythresholddecision DECISIONCat

drop interventionphrase dosephrase dosenum comparison icer icerphrase efficacyassumptions efficacyothers seroreplacemethod vaxpriceintervention vaxpricecomparator

*create cost saving variable
gen costsav = 1 if icerusd < 0
replace costsav = 0 if icerusd >=0 & icerusd !=.

*Create sum of # published articles per year
sort pubyear studyid

gen studydecisionBin = studythresholddecision
recode studydecisionBin 3=0 2=1

gen ochalekhighdecisionBin = ochalekhighdecision
recode ochalekhighdecisionBin 3=0 2=1

gen ochaleklowdecisionBin = ochaleklowdecision
recode ochaleklowdecisionBin 3=0 2=1

gen woodshighdecisionBin = woodshighdecision
recode woodshighdecisionBin 3=0 2=1

gen woodslowdecisionBin = woodslowdecision
recode woodslowdecisionBin 3=0 2=1

label define DECISIONBin 0 "Not Cost-Effective" 1 "Cost-Effective"
label values studydecisionBin ochalekhighdecisionBin ochaleklowdecisionBin woodshighdecisionBin woodslowdecisionBin DECISIONBin


lab var studyid "Study Identification"
lab var icerid "ICER Identification"
lab var uniqueid "Unique Identification"
lab var firstauthor "First Author"
lab var pubyear "Publication Year"
lab var title "Publication Title"
lab var country "Country"
lab var income "Income Group"
lab var asiapacific "Asia-Pacific"
lab var intervention "Intervention"
lab var comparator "Comparator"
lab var basecase "Base Case"
lab var currencyname "Currency Name"
lab var currencycountry "Currency Country"
lab var currencyyear "Currency Year"
lab var vaxpriceinterventionusd "Intervention Vaccine Price"
lab var vaxpriceinterventionsource "Intervention Vaccine Price Source"
lab var vaxpricecomparatorusd "Comparator Vaccine Price"
lab var vaxpricecomparatorsource "Comparator Vaccine Price Source"
lab var herdeffects "Herd Effects Inclusion"
lab var herdeffectsmethod "Herd Effects Method"
lab var herdeffectolder "Herd Effects Inclusion 5+"
lab var seroreplace "Serotype Replacement Inclusion"
lab var efficacypneumonia "Efficacy Pneumonia"
lab var efficacyipd "Efficacy IPD"
lab var efficacyotitis "Efficacy Otitis"
lab var icermetric "ICER Metric"
lab var icerusd "ICER USD"
lab var gdppc "GDP per capita"
lab var onegdppcdecision "One GDP Per Capita Decision"
lab var threegdppcdecision "Three GDP Per Capita Decision"
lab var percentgdppc "Percent GDP Per Capita"
lab var ochalekhigh "Ochalek High Threshold"
lab var ochalekhighdecision "Ochalek High Decision"
lab var ochaleklow "Ochalek Low Threshold"
lab var ochaleklowdecision "Ochalek Low Decision"
lab var woodshigh "Woods High Threshold"
lab var woodshighdecision "Woods High Decision"
lab var woodslow "Woods Low Threshold"
lab var woodslowdecision "Woods Low Decision"
lab var studythreshold "Study Threshold"
lab var studythresholddecision "Study Decision"
lab var switchochaleklow "Switch Ochalek Low"
lab var switchochalekhigh "Switch Ochalek High"
lab var switchwoodslow "Switch Woods Low"
lab var switchwoodshigh "Switch Woods High"
lab var switches "Switch Total"
lab var budgetimpact "Budget Impact Included"
lab var budgetimpactresults "Budget Impact Results"
lab var notescomments "Notes and Comments"
lab var perspective "Perspective"
lab var comparisonCat "Comparison"
lab var dosenumCat "Number of Doses Categorical"
lab var costsav "Cost Saving Binary"
lab var studydecisionBin "Study Decision Binary"
lab var ochalekhighdecisionBin "Ochalek High Decision Binary"
lab var ochaleklowdecisionBin "Ochalek Low Decision Binary"
lab var woodshighdecisionBin "Woods High Decision Binary"
lab var woodslowdecisionBin "Woods Low Decision Binary"



*ICER 999999 means dominated (there are 4 such evals) 
*careful with negative ICERs (cost-saving)



********************** Table 1 *****************************
//net install table1_mc.pkg
table1_mc, vars(comparisonCat cat %5.0f\ dosenumCat cat %5.0f \ perspective bin %5.0f \ income cat %5.0f \ herdeffects cat %5.0f \ seroreplace cat %5.0f \ efficacyotitis cat %5.0f \ budgetimpact cat %5.0f)  onecol slashN


// unique articles by study characteristics
//ssc inst unique
unique studyid, gen(totstud) by(icerid)
drop totstud
unique studyid, gen(totstud) by(pubyear)
drop totstud
unique studyid, gen(totstud) by(comparisonCat)
drop totstud
unique studyid, gen(totstud) by(dosenumCat)
drop totstud
unique studyid, gen(totstud) by(perspective)
drop totstud
unique studyid, gen(totstud) by(income)
drop totstud
unique studyid, gen(totstud) by(country)
drop totstud
unique studyid, gen(totstud) by(herdeffects)
drop totstud
unique studyid, gen(totstud) by(seroreplace)
drop totstud
unique studyid, gen(totstud) by(efficacyotitis)
drop totstud
unique studyid, gen(totstud) by(budgetimpact)
drop totstud
unique studyid, gen(totstud) by(studythreshold)
drop totstud


********************** Table 2 *****************************
*Summarise findings - compare study decision with Ochalek / Woods decisions

tab comparisonCat studydecisionBin if ochalekhigh !=., row freq
tab comparisonCat ochalekhighdecisionBin, row nofreq
tab comparisonCat ochaleklowdecisionBin,  row nofreq


tab comparisonCat studydecisionBin if woodshigh !=., row freq
tab comparisonCat woodshighdecisionBin,   row nofreq
tab comparisonCat woodslowdecisionBin,    row nofreq


tab comparisonCat studythresholddecision, row freq



********************* Descriptives ****************************

*make "new_switches" variable that is only relevant to evaluations that switch from being cost-effective (not cost-saving or "not cost-effective")
gen new_switches = switches
replace new_switches = . if switches == 0 & studythresholddecision == 1 // cost-saving
replace new_switches = . if switches == 0 & studythresholddecision == 3 // not cost-effective


gen any_switch_new = new_switches
replace any_switch_new = 1 if new_switches > 1


* Which comparisons are most likely to switch?
*by type of comparison
tab comparisonCat switchochaleklow , row  freq all exact  //PCV10vs Novax followed by PCV7 vs No Vax and PCV13 vs No Vax
tab comparisonCat switchochalekhigh , row  freq  all exact   //PCV7 vs Novax followed by PCV10 vs No Vax and PCV13 vs No Vax
tab comparisonCat switchwoodslow  , row  freq  all exact   // PCV10vs Novax followed by PCV13 vs No Vax and PCV7 vs No Vax
tab comparisonCat switchwoodshigh , row  freq  all exact   // PCV10vs Novax followed by PCV13 vs No Vax and PCV7 vs No Vax
tab comparisonCat switches , row  freq  all    // Comparisons betweem PCV 7 vs np vax and PCV10vs Novax were most likely to switch (p<0.05 Chi sq)
tab comparisonCat new_switches, row  freq  all    // Comparisons betweem PCV 7 vs np vax and PCV10vs Novax were most likely to switch (p>0.05 Chi sq)
tab comparisonCat any_switch_new, row  freq  all    // p>0.05 pearson Chi sq)
*Summary: more often, PCV10 vs no vax comparison more likely to switch, followed by bothPCV7 and PCV13 vs no vax

*by income group
tab income switchochaleklow , row  freq  all exact // L followed by LM followed by UM
tab income switchochalekhigh , row  freq  all exact  // L followed by LM followed by UM
tab income switchwoodslow , row  freq  all exact  // LM followed by L followed by UM
tab income switchwoodshigh , row  freq  all exact  // L followed by LM followed by UM
tab income switches , row  freq  all    // L most likely, then LM (p<0.05)
tab income new_switches , row  freq  all    // L most likely, then LM (p<0.05)
tab income any_switch_new , row  freq  all    // P> 0.05 Pearson Chi sq
*summary: Low income more likely to switch than LM and than UM

*by herdeffects
tab herdeffects switchochaleklow , row  freq  all exact  // No (42% switch) Y (38% switch) 
tab herdeffects switchochalekhigh , row  freq  all exact   // No (32% switch) Y (26% switch) 
tab herdeffects switchwoodslow , row  freq  all exact   // No (74% switch) Y (59% switch) 
tab herdeffects switchwoodshigh , row  freq   all exact   // No (32% switch) Y (31% switch) 
tab herdeffects switches , row  freq  all    // less likely to consider herd effects (p<0.05)
tab herdeffects new_switches , row  freq  all    // less likely to consider herd effects (p>0.05)
tab herdeffects any_switch_new , row  freq  all    // P> 0.05 Pearson Chi sq
*Summary: evaluations that did not consider herd effects were more likely to switch than those that did 

*by seroreplace
tab seroreplace switchochaleklow , row  freq  all exact  // No (40% switch) Y (53% switch) 
tab seroreplace switchochalekhigh , row  freq  all exact  // No (28% switch) Y (53% switch) 
tab seroreplace switchwoodslow , row  freq  all exact  // No (68% switch) Y (82% switch) 
tab seroreplace switchwoodshigh , row  freq  all exact  // No (30% switch) Y (11% switch) 
tab seroreplace switches , row  freq  all   // Less likely to switch if Yes - consider serotype replacement; (p>0.05) 
tab seroreplace new_switches , row  freq  all   // more likely to consider serotype replacement; (p>0.05) 
tab seroreplace any_switch_new , row  freq  all   // P> 0.05 Pearson Chi sq
*Summary: evaluations that considered serotype replacement were more likely to switch than those that did not

*mean vax price
tab switchochaleklow , sum(vaxpriceinterventionusd)   //all exact  // Switch mean price 31; no switch mean price 21.7
tab switchochalekhigh , sum(vaxpriceinterventionusd)  //all exact   // Switch mean price 35; no switch mean price 21
tab switchwoodslow , sum(vaxpriceinterventionusd)  //all exact   // Switch mean price 29; no switch mean price 17
tab switchwoodshigh , sum(vaxpriceinterventionusd)  //all exact   // Switch mean price 37; no switch mean price 20
tab  new_switches , sum(vaxpriceinterventionusd)     // higher vaccine price - more likely to switch
tab income new_switches , sum(vaxpriceinterventionusd)      // higher vaccine price - more likely to switch
*Summary: evaluations that switched had a higher mean vaccine price than ones that didn't switch

*mean vax price by income group
tab income , sum(vaxpriceinterventionusd)    // price increases by income group



tab perspective new_switches, row  freq  all    // Comparisons betweem PCV 7 vs np vax and PCV10vs Novax were most likely to switch (p>0.05 Chi sq)


probit switchochaleklow i.comparisonCat perspective herdeffects seroreplace  i.income  vaxpriceinterventionusd   // 
probit switchochalekhigh i.comparisonCat perspective herdeffects seroreplace  i.income  vaxpriceinterventionusd   // 
probit switchwoodslow i.comparisonCat perspective herdeffects seroreplace  i.income  vaxpriceinterventionusd   // 
probit switchwoodshigh i.comparisonCat perspective herdeffects seroreplace  i.income  vaxpriceinterventionusd   // 


probit switchochaleklow i.comparisonCat perspective herdeffects seroreplace    vaxpriceinterventionusd  if income == 3   // 
probit switchochalekhigh i.comparisonCat perspective herdeffects seroreplace    vaxpriceinterventionusd  if income == 3   // 
probit switchwoodslow i.comparisonCat perspective herdeffects seroreplace    vaxpriceinterventionusd  if income == 3   // 
probit switchwoodshigh i.comparisonCat perspective herdeffects seroreplace    vaxpriceinterventionusd  if income == 3   // 
probit any_switch_new i.comparisonCat perspective herdeffects seroreplace    vaxpriceinterventionusd  if income == 3   // 




prtest herdeffects, by (any_switch_new)  //no statistically significant
prtest perspective, by (any_switch_new)  //trecode perspective to be 0/1 var (0 = societal
prtest seroreplace, by (any_switch_new)  //no statistically significant


ttest vaxpriceinterventionusd, by (any_switch_new) uneq

by (group)

********************* Statistical associations ****************************



*Graphs to show
graph box vaxpriceinterventionusd, over(income)  // edit labels manually
*graph save Graph "/Users/ncarvalho/Desktop/Graph-PCV dose price by country income level.gph


*See 1-way associations with cost saving
probit costsav vaxpriceinterventionusd  i.income   // shows no significance, no association with vaccine price and cost saving


*See 1-way associations with cost effective/costsaving
probit studydecisionBin vaxpriceinterventionusd i.herdeffects i.seroreplace,  vce(robust)     // shows significant association with vaccine price and prob(cost-effective)  - as vax price increases, less likely to be cost-effective
probit studydecisionBin vaxpriceinterventionusd if income == 3,  vce(robust)     // shows significant association with vaccine price and prob(cost-effective)  - as vax price increases, less likely to be cost-effective



*See 1-way association of price with ICER value  (for ICER > 0 because can't use negative ICERs)
reg icerusd vaxpriceinterventionusd if costsav == 0, r
// on average each dollar increase in vaccine price is associated with a $453 increase in ICER (not significant at p<0.05 using robust Standard errors)



*See 1-way association of herd effects with ICER value  (for ICER > 0)
reg icerusd i.herdeffects if costsav == 0
reg icerusd i.herdeffects

reg icerusd vaxpriceinterventionusd if costsav == 0, r
reg icerusd vaxpriceinterventionusd, r					
// on average each dollar increase in vaccine price is associated with a $500 increase in ICER (significant at p<0.05 but at p<0.1 using robust Standard errors)


probit studydecisionBin i.herdeffects,  vce(robust)     // shows significant association with vaccine price and prob(cost-effective)  - as vax price increases, less likely to be cost-effective





*gen var to identify when decisions switched
gen switchOH = 1 if (studythresholddecision == 2 | studythresholddecision == 1) & ochalekhighdecision == 3
replace switchOH = 0 if (studythresholddecision == 2 | studythresholddecision == 1)  & ochalekhighdecision == 2
replace switchOH = 0 if studythresholddecision == 3  & ochalekhighdecision == 3

gen switchOL = 1 if (studythresholddecision == 2 | studythresholddecision == 1) & ochaleklowdecision == 3
replace switchOL = 0 if (studythresholddecision == 2 | studythresholddecision == 1)  & ochaleklowdecision == 2
replace switchOL = 0 if studythresholddecision == 3  & ochaleklowdecision == 3

gen switchWH = 1 if (studythresholddecision == 2 | studythresholddecision == 1) & woodshighdecision == 3
replace switchWH = 0 if (studythresholddecision == 2 | studythresholddecision == 1)  & woodshighdecision == 2
replace switchWH = 0 if studythresholddecision == 3  & woodshighdecision == 3

gen switchWL = 1 if (studythresholddecision == 2 | studythresholddecision == 1) & woodslowdecision == 3
replace switchWL = 0 if (studythresholddecision == 2 | studythresholddecision == 1)  & woodslowdecision == 2
replace switchWL = 0 if studythresholddecision == 3  & woodslowdecision == 3



*income and switch?
//table switchOH, c(%) by(income)

*graphs
// twoway scatter vaxpriceinterventionusd icerusd, by(switchOH)
// twoway scatter vaxpriceinterventionusd icerusd, by(switchOL)
// twoway scatter vaxpriceinterventionusd icerusd, by(switchWH)
// twoway scatter vaxpriceinterventionusd icerusd, by(switchWL)


// twoway scatter vaxpriceinterventionusd icerusd, by(switchOH)
// graph box vaxpriceinterventionusd, over(income) by(switchOH)  // edit labels manually
// graph box vaxpriceinterventionusd, over(income) by(switchOL)  // edit labels manually


tab switchOH 
tab switchOL 
tab switchWH 
tab switchWH 

tab vaxpriceinterventionusd if studythresholddecision == 2
tab vaxpriceinterventionusd if switchOL == 1



codebook vaxpriceinterventionusd if switchOH == 1 & income == 3   //mean price higher, but no real difference based on hisograms
codebook vaxpriceinterventionusd if switchOH == 0 & income == 3

// histogram vaxpriceinterventionusd if income == 3, by(switchOH)
// histogram vaxpriceinterventionusd if income == 0, by(switchOH)


*two sample t test

*redoing below across all income groups together - no associations

*FOR INCOME GROUP 3
ttest vaxpriceinterventionusd if income == 3, by (switchOL) uneq
ttest vaxpriceinterventionusd if income == 3, by (switchOH) uneq  //t-test shows 'switch' had higher mean dose price
ttest vaxpriceinterventionusd if income == 3, by (switchWL) uneq
ttest vaxpriceinterventionusd if income == 3, by (switchWH) uneq  //t-test shows 'switch' had higher mean dose price

*For other income groups


*two sample test of proportions using groups

prtest herdeffects, by (switchOH)  //test of proportions shows 'switch' had lower proportion herd effects considered
prtest herdeffects, by (switchOL) 
prtest herdeffects, by (switchWH) 
prtest herdeffects, by (switchWL) //test of proportions shows 'switch' had lower proportion herd effects considered


prtest seroreplace, by (switchOH)  
prtest seroreplace, by (switchOL) 
prtest seroreplace, by (switchWH) 
prtest seroreplace, by (switchWL) 



graph box vaxpriceinterventionusd, over(income)  // edit labels manually


