
/*
Algorithm	Freq.	Percent	Cum.
			
1: Raw	129	14.29	14.29
2: Janney 2016	129	14.29	28.57
3: Littman 2012	129	14.29	42.86
4: Maciejewski 2016
5: Breland 2017	129	14.29	57.14
6: Maguen 2013	129	14.29	71.43
7: Goodrich 2016	129	14.29	85.71
8: Chan & Raffa 2017	129	14.29	100.00
9: Jackson 2015 |        128        7.64       69.21
10: Buta 2018 |        129        7.70       76.91
11: Kazerooni & Lim 2016 |        129        7.70       84.61
12: Noel 2012 |        129        7.70       92.30
13: Rosenberger 2011 |        129        7.70      100.00
			
Total	903	100.00
*/

cd "X:\Damschroder-NCP PEI\6. Aim 2 Weight Algorithms\CDW Data\data"

use Site_Level_wt5, clear
des
tab Algorithm
tab Algorithm, nolab

bys Algorithm: sum prop
bys Algorithm: sum prop, det
gen group=1 if Algorithm==9 | Algorithm==7 | Algorithm==2 | Algorithm==11 | Algorithm==12 | Algorithm==13
replace group=2 if Algorithm==8 | Algorithm==6 | Algorithm==5 | Algorithm==4 | Algorithm==3 | Algorithm==10

bys group Algorithm: sum prop
bys group Algorithm: sum prop, det

sum prop if Algorithm>1

sort prop

forv i=1/13 {
preserve
keep if Algorithm==`i'
gsort prop
gen rank`i'=_n
gen prop`i'=prop
keep Sta3n rank`i' prop`i'
save rank`i', replace
restore
}

use rank1, clear
merge 1:1 Sta3n using rank2 
drop _merge
merge 1:1 Sta3n using rank3
drop _merge
merge 1:1 Sta3n using rank4
drop _merge
merge 1:1 Sta3n using rank5 
drop _merge
merge 1:1 Sta3n using rank6
drop _merge
merge 1:1 Sta3n using rank7
drop _merge
merge 1:1 Sta3n using rank8
drop _merge
merge 1:1 Sta3n using rank9
drop _merge
merge 1:1 Sta3n using rank10
drop _merge
merge 1:1 Sta3n using rank11
drop _merge
merge 1:1 Sta3n using rank12
drop _merge
merge 1:1 Sta3n using rank13
drop _merge

egen minrank=rowmin(rank2 rank3 rank4 rank5 rank6 rank7 rank8 rank9 rank10 rank11 rank12 rank13)
egen maxrank=rowmax(rank2 rank3 rank4 rank5 rank6 rank7 rank8 rank9 rank10 rank11 rank12 rank13)
egen minprop=rowmin(prop2 prop3 prop4 prop5 prop6 prop7 prop8 prop9 prop10 prop11 prop12 prop13)
egen maxprop=rowmax(prop2 prop3 prop4 prop5 prop6 prop7 prop8 prop9 prop10 prop11 prop12 prop13)

egen minpropg1=rowmin(prop2 prop7 prop9 prop11 prop12 prop13)
egen maxpropg1=rowmax(prop2 prop7 prop9 prop11 prop12 prop13)
egen minpropg2=rowmin(prop3 prop4 prop5 prop6 prop8 prop10)
egen maxpropg2=rowmax(prop3 prop4 prop5 prop6 prop8 prop10)

sum minprop, det
sum maxprop, det

sort rank1
*scatter prop1 rank1, mcolor(gs9) msize(vsmall)  
line prop1 rank1, lcolor(gs0) lwidth(medium) ///
	|| scatter prop2 rank1 , msymbol(diamond)  msize(vsmall) /// 
	|| scatter prop3 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop4 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop5 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop6 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop7 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop8 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop9 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop10 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop11 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop12 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop13 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| rcap minprop maxprop rank1 , lcolor(black) msize(vsmall) lwidth(vthin) 
	
line prop1 rank1, lcolor(black) lwidth(medium) ///
	|| scatter prop2 rank1 , msymbol(diamond)  msize(vsmall) /// 
	|| scatter prop7 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop9 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop11 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop12 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop13 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| rcap minpropg1 maxpropg1 rank1 , lcolor(gs13) msize(zero) lwidth(thin) 
*graph save site_5p_g1.gph, replace	
graph save site_5p_g1_2.gph, replace	

line prop1 rank1, lcolor(black) lwidth(medium) ///
	|| scatter prop3 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop4 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop5 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop6 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop8 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter prop10 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| rcap minpropg2 maxpropg2 rank1 , lcolor(gs13) msize(zero) lwidth(thin) 
*graph save site_5p_g2.gph, replace	
graph save site_5p_g2_2.gph, replace	

graph combine  site_5p_g1.gph  ///
		       site_5p_g2.gph ///
				  , ycommon graphregion(color(white)) iscale(.5)
graph export "X:\Damschroder-NCP PEI\6. Aim 2 Weight Algorithms\CDW Data\Figures\panel_plot_site5p_102119.pdf", replace

graph combine  site_5p_g1_2.gph  ///
		       site_5p_g2_2.gph ///
				  , ycommon graphregion(color(white)) iscale(.5)
graph export "X:\Damschroder-NCP PEI\6. Aim 2 Weight Algorithms\CDW Data\Figures\panel_plot_site5p_010720.pdf", replace


gen diffrank=abs(minrank-maxrank)
sum diffrank, det
gen diffprop=abs(minprop-maxprop)
sum diffprop, det
sort Sta3n
hist diffrank

gen rank2_3=abs(rank2-rank3)
gen rank2_4

forvalues i=2/13 {
	ttest prop1==prop`i'
	}

forvalues i=1/13 {
	gen diff1_`i'=prop1-prop`i'
	}

browse Sta3n rank1 prop1 prop4 prop6
	
sort rank1
line diff1_1 rank1, lcolor(black) lwidth(medium) ///
	|| scatter diff1_2 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter diff1_7 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter diff1_9 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter diff1_11 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter diff1_12 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter diff1_13 rank1 , msymbol(diamond)  msize(vsmall) 
graph save site_5p_g3.gph, replace	


sort rank1
line diff1_1 rank1, lcolor(black) lwidth(medium) ///
	|| scatter diff1_3 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter diff1_4 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter diff1_5 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter diff1_6 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter diff1_8 rank1 , msymbol(diamond)  msize(vsmall) ///
	|| scatter diff1_10 rank1 , msymbol(diamond)  msize(vsmall) 
graph save site_5p_g4.gph, replace	


graph combine  site_5p_g3.gph  ///
		       site_5p_g4.gph ///
				  , ycommon graphregion(color(white)) iscale(.5)
graph export "X:\Damschroder-NCP PEI\6. Aim 2 Weight Algorithms\CDW Data\Figures\panel_plot_site5p_propdiff_010720.pdf", replace










line sbpmed0 popyr0, lcolor(gs9) lwidth(medium) ///
	|| rcap sbploq0 sbpupq0 popyr0 , lcolor(gs9) lwidth(medthin) msize(vsmall) ///
	|| scatter sbpmed0 popyr0 , msymbol(diamond) mcolor(gs9) ///
	|| line sbpmed1 popyr1, lcolor(black) lwidth(medium) ///
	|| rcap sbploq1 sbpupq1 popyr1 , lcolor(black) msize(vsmall) lwidth(medthin) ///
	|| scatter sbpmed1 popyr1 , msymbol(square) mcolor(black) ///
	,ylabel(110(10)150) yscale(r(.,150)) yscale(r(.,110)) ///
	xline(8, lcolor(gray)) xtitle("Years since Start of Study") ///
	xlabel(3(2)17) legend(label(1 "Standard therapy" 2 "Intensive therapy")) legend(off) ///
	graphregion(color(white)) ytitle("SBP") 
	

