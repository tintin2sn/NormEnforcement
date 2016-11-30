* Analyses for: 
* Norm envorcement in small-scale societies depends on coordinated third party responses and pre-existing relationships
* Moya et. al. 2016 

clear
set more off
cap log close

*------------------------------
* 1. Main Article Analyses 
*------------------------------

*---- Load Wide Data ------------------------
clear
use "NormEnforcementDataSupplement.dta"

** Table 1 Sample Demographics

tab site if story_theft, sum(female)	
bysort site: sum age if story_theft

** Table 2 Coding Schemes
	* for community responses
des tcq_a_not tcq_b_sco tcq_c_cmp tcq_d_vio tcq_f_neu tcq_g_hvi tcq_e_negtell tcq_e_negact tcq_e_negother
des com_benefits_vic com_costs_violator com_costs_com com_costs_com_hi
	* for specific protagonist responses
		*neutral neighbor
des tn_a_not tn_b_sco tn_c_cmp tn_d_vio tn_f_neu tn_g_hvi tn_e_negtell tn_e_negsolo tn_h_hab tn_i_hde tn_j_hel
des n_benefits_vic n_costs_violator n_costs_pro n_benefits_violator  n_costs_pro_hi
		*sibling
des ts_a_not ts_b_sco ts_c_cmp ts_d_vio ts_f_neu ts_g_hvi ts_e_negtell ts_e_negsolo ts_h_hab ts_i_hde ts_j_hel
des s_benefits_vic s_costs_violator s_costs_pro s_benefits_violator  s_costs_pro_hi
		*friend
des tf_a_not tf_b_sco tf_c_cmp tf_d_vio tf_f_neu tf_g_hvi tf_e_negtell tf_e_negsolo tf_h_hab tf_i_hde tf_j_hel
des f_benefits_vic f_costs_violator f_costs_pro f_benefits_violator  f_costs_pro_hi
		*rival
des tr_a_not tr_b_sco tr_c_cmp tr_d_vio tr_f_neu tr_g_hvi tr_e_negtell tr_e_negsolo tr_h_hab tr_i_hde tr_j_hel
des r_benefits_vic r_costs_violator r_costs_pro r_benefits_violator  r_costs_pro_hi

** Figure 1 Community responses descriptive codes by site
	*sort sites from fewest to greatest proportion of active responses
gen num_tcq=tcq_a_not+ tcq_b_sco +tcq_c_cmp +tcq_d_vio +tcq_e_neg+ tcq_f_neu +tcq_g_hvi //# of community outcomes coded per person
egen tcq_totalbysite= total(num_tcq), by(site) 											//# of community outcomes coded per site
gen mild_tcq=tcq_a_not+ tcq_g_hvi+tcq_f_neu+tcq_e_negoth  								//# of mild community outcomes coded per person
egen mild_tcq_bysite=total(mild_tcq), by(site) 											//# of mild community outcomes coded per site
	*plot
graph bar (mean) tcq_a_not (mean) tcq_g_hvi (mean) tcq_f_neu (mean) tcq_e_negoth ///
(mean) tcq_e_negact (mean) tcq_e_negtell (mean) tcq_b_sco  (mean) tcq_c_cmp (mean) tcq_d_vio, ///
over(site, sort(sort_mild) descending label(angle(vertical)) ) ///
graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
percentages stack ytitle(percent of responses) ///
legend(order( 9 "violence" 8 "compensation"  7 "chastise"  6 "tell others"  ///
5 "joint discussion" 4 "negative emotion" 3 "neutral" 2 "help victim" 1 "nothing" ) ///
cols(1) position(3)) ///
bar(1, color("215 215 215")) bar(2, color("24 150 160")) bar(3, color("138 202 182")) ///
bar(4, color("252 243 44")) bar(5, color("240 183 25")) bar(6, color("228 123 07")) ///
bar(7, color("248 180 183")) bar(8, color("250 85 85")) bar(9, color("132 5 5"))  

** Section 3.1 Do Los Angeles participants mention authorities and victims more than other societies do?
foreach a of varlist aut vic {
	xtlogit `a' site_LA, i(id_participant) or
	* when controlling for question level wordcount (Section S3)
	xtlogit `a' site_LA wordcount, i(id_participant) or
	* when controlling for site-level wordcount 	(Section S3)
	xtlogit `a' site_LA sm_wordcount, i(id_participant) or
}

** Section 3.2 Qualitative analysis of shame terms
foreach sh in  "shame" { //don't exist: "loss face" "loss of face" "humil" "honor" "embarass"  "guilt" (in the right context)
	gen quals_`sh' = strpos(tsaenglish , "`sh'")>0 
	gen qualn_`sh' = strpos(tnaenglish , "`sh'")>0 
	gen qualf_`sh' = strpos(tfaenglish , "`sh'")>0 
	gen qualr_`sh' = strpos(traenglish , "`sh'")>0 
	tab quals_`sh' 
	tab qualf_`sh' 
	tab qualn_`sh' 
	tab qualr_`sh'
	drop qual*
}

** Figure 5 & S8 Proportion of responses that mention an actor engaging in punishment by site-level authority reliance	

tab site if story_theft, sum(sm_aut)						//authority reliance by site
foreach a in "n" "s" "f" "r" {				
	tab site if story_theft, sum(sm_costs_notben_vio_`a')	// tabulate protagonist only punishes, by site
}

*---- Load Long Data ------------------------
clear		
use "NormEnforcementDataLongSupplement.dta"
		
** Figure 2 Predicted probability of each payoff outcome by protagonist (Equation 1)
foreach out of varlist benefits_vic benefits_violator costs_violator costs_pro costs_pro_hi{
	quietly: xtmelogit `out' i.protagonist || site: || id_participant: , or 	
	di "for outcome: `out'"
	margins, over(protagonist)  predict(mu fixed)
}	
	
** Figure 4 Proportion of responses that ostracizer would suffer negative reputational consequences
	*descriptively
bysort site:tab protagon, sum(ostrabad)			
	* mean effects of protagonist relationship on consequence of ostracism? 
xtmelogit ostrabad_v_goodneutral i.protagonist || site: || id_participant: , or 
	* per site paired-comparisons of protagonists 
foreach s of numlist  1/4 6/8   {					//except Storozhnitsa since 0s don't allow modelling
quietly: xtmelogit ostrabad_v_goodneutral pro_2 pro_5 pro_4 i.story || id_participant: if site==`s'		
		di `s' "," ///	//site, log odds, se, p
		_b[pro_5] "," _se[pro_5] "," 2 * ttail( (e(N) - e(df_m)), abs(_b[pro_5]/_se[pro_5])) "," ///	//sibling v neutral
		_b[pro_2] "," _se[pro_2] "," 2 * ttail( (e(N) - e(df_m)), abs(_b[pro_2]/_se[pro_2])) "," ///	//friend v neutral
		_b[pro_4] "," _se[pro_4] "," 2 * ttail( (e(N) - e(df_m)), abs(_b[pro_4]/_se[pro_4]))			//rival v neutral
}

*---- Load Wide Data ------------------------
clear		
use "NormEnforcementDataSupplement.dta"
** Figure 6 Variance partition at the site and individual levels
** Figure S11 Proportion of variance explained at the site level, by protagonist and outcome
** Figure S12 Absolute variance explained at the site-level, by protagonist and outcome

log using VariancePartition.log, replace

foreach costben of varlist com_efficient_bi n_efficient_bi s_efficient_bi f_efficient_bi r_efficient_bi ///
com_costs_violator com_benefits_vic  com_costs_com com_costs_com_hi /// 
n_benefits_vic n_benefits_violator n_costs_violator n_costs_pro n_costs_pro_hi n_ostrabad_v_goodneutral ///
s_benefits_vic s_benefits_violator s_costs_violator s_costs_pro s_costs_pro_hi  s_ostrabad_v_goodneutral ///
f_benefits_vic f_benefits_violator f_costs_violator f_costs_pro f_costs_pro_hi  f_ostrabad_v_goodneutral ///
r_benefits_vic r_benefits_violator r_costs_violator r_costs_pro r_costs_pro_hi  r_ostrabad_v_goodneutral {

	di "Model for: `costben'"					// 3-level models for payoffs 
	xtmelogit `costben' || site: || id_participant:, mle variance intpoints(7) 
	predict u0 u1, reffects
	predict u0se u1se, reses

	di "Site Residuals for:`costben'"			//Site residuals
	egen pickone=tag(site)
	sort u0 u1
	gen u0rank = sum(pickone)
	list site u0 if pickone==1 		
	list site u0se if pickone==1

	di "Random effect SDs for:`costben'"		//http://www.ats.ucla.edu/stat/stata/faq/diparm.htm
	_diparm lns1_1_1, f(exp(@)^2) d(exp(@)^2) 	
	local temp=r(est)							
	_diparm lns2_1_1, f(exp(@)^2) d(2*exp(@)^2)

	di "Variance Partition:`costben'"			// variance partitioning after boskers snijders / lemma
	di "question, individual, site"
	di 3.29/(`temp' + r(est) + 3.29) "," r(est)/(`temp'+ r(est) +3.29) "," `temp'/(`temp' + r(est) + 3.29)

	drop u0 u0rank u0se u1 u1se pickone			//reset vars
}

log close 										//VariancePartition.log


*---------------------------------------
* 2. Supplementary Materials  Analyses 
*---------------------------------------

*---- Load Wide Data ------------------------
clear		
use "NormEnforcementDataSupplement.dta"

** Figure S1 Mean severity of each norm violation by site
	*make necessary variables
gen s_theft:theft=s if severity_story=="Theft"
gen s_battery:battery=s if severity_story=="Battery"
gen s_negligence:negligence=s if severity_story=="Negligence"
egen s_z = std(s) if severity_story!="."						//standardized s
encode severity_story, generate(severity_story_string) 			//make a numeric severity story
replace severity_story_string=. if severity_story_string==1		//. meant missing rather than category. replace.
quietly tabulate severity_story, generate(severity_story_) 		//make indicator variable
	*plot
graph hbar (mean) s_theft (mean) s_battery (mean) s_negligence, ///
over(site) ytitle("Severity") ///
graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
legend(order(1 "theft" 2 "battery" 3 "negligent arson") rows(1)) ///
bar(1, color("143 188 143")) bar(2, color("178 34 34")) bar(3, color("255 140 0"))
	*regress: Are there differences in how severe norm violations are considered?
xtmixed s_z b3.severity_story_str if story_battery ==1 || site: // (if battery==1 ensures one obs per participant)
	*regress: Do these differences vary by site?
xtmixed s_z b3.severity_story_str if story_battery ==1 || site: severity_story_2 severity_story_4

** Figure S2 Other actors' involvement in punishment by site 
	* a) Actual mentions (across all Story conditions) 
graph bar (mean) aut (mean) vic, stack ///
legend(order(1 "authority" 2 "victim") cols(1) position(3)) ///
graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
bar(1, color("50 100 50")) bar(2, color("50 185 106")) ///
over(site, sort(sort_mild) label(angle(vertical)) ) ///
ytitle("proportion mentioning actor")
	* not shown) Actual mentions by Story condition
graph hbar (mean) aut (mean) vic,  ///
legend(order(1 "aut" 2 "vic") rows(1)) scheme(s1manual) ///
by(, graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))) ///
by(story, rows(1)) over(site)
	* b) Predicted authority mentions controlling for wordcount
xtlogit aut b3.site wordcount, i(id_participant)
margins, over(site) predict(pu0) at(wordcount=9)
marginsplot, recast(dot) ytitle("predicted prob. authority") xtitle("") title("") ///
graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
plot1opts(lcolor(gs9) mcolor(red)) ci1opts(lcolor(red))	xlabel(,angle(vertical))
	* c) Predicted victim mentions controlling for wordcount
xtlogit vic b3.site wordcount, i(id_participant)
margins, over(site) predict(pu0) at(wordcount=9)
marginsplot, recast(dot) ytitle("predicted prob. victim") xtitle("") title("") ///
graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
plot1opts(lcolor(gs9) mcolor(red)) ci1opts(lcolor(red))	xlabel(,angle(vertical))


** Table S2 Actor involvement by a) Site and b) Norm Violation
	*actor mentions
foreach actor of varlist com aut vic {
	tab site `actor', row
	tab story `actor', row
}
	*word counts
tabstat sm_wordcount, by(site)
tabstat smed_wordcount, by(site)

** Figure S3 Histograms of word counts on community questions, by site
gen mean=.4																	// y-values for plotting vert lines
replace mean=0 if story==3
gen median=mean
histogram wordcount, fraction fcolor(bluishgray) lcolor(bluishgray) ///		//wordcount histogram
addplot((line mean sm_wordcount, lcolor(maroon) ) ///
(line median smed_wordcount, lcolor(orange) )) ///
ytitle(Proportion) xtitle(# words) /// 
by(site, cols(2) legend(position(3)) ///
graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)))
drop mean median 


** Figure S5 Specific protagonists' responses descriptive codes by site
	*rivals
graph bar (mean) tr_i_hde (mean) tr_h_hab  (mean) tr_j_hel ///
(mean) tr_a_not (mean) tr_g_hvi (mean) tr_f_neu ///
(mean) tr_e_negsolo (mean) tr_e_negtell   ///
(mean) tr_b_sco (mean) tr_c_cmp (mean) tr_d_vio, ///
over(site, label(angle(vertical)) ) ///
percentages yscale(range(0 100)) ytick(0(50)100) ylabel(0(50)100) ///
graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) stack ///
bar(1, color("50 100 50")) bar(2, color("50 185 106")) bar(3, color("50 250 106")) ///
bar(4, color("253 253 205")) bar(5, color("138 202 182")) bar(6, color("24 150 160")) ///
bar(7, color("252 243 44"))  bar(8, color("228 123 07")) ///
bar(9, color("248 180 183")) bar(10, color("221 38 36")) bar(11, color("132 5 5"))  ///
legend(order(1 "defend violator" 2 "abet" 3 "help violator (vague)" ///
4 "nothing" 5 "help victim" 6 "neutral" 7 "negative emotion" ///
8 "tell others" 9 "chastise" 10 "compensation" 11 "violence" ) ///
cols(2) position(1) colfirst) ///
legend(on)
	*neutral neighbor
graph bar (mean) tn_i_hde (mean) tn_h_hab  (mean) tn_j_hel ///
(mean) tn_a_not (mean) tn_g_hvi (mean) tn_f_neu ///
(mean) tn_e_negsolo (mean) tn_e_negtell   ///
(mean) tn_b_sco (mean) tn_c_cmp (mean) tn_d_vio, ///
over(site, label(angle(vertical)) ) ///
percentages yscale(range(0 100)) ytick(0(50)100) ylabel(0(50)100) ///
graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) stack ///
bar(1, color("50 100 50")) bar(2, color("50 185 106")) bar(3, color("50 250 106")) ///
bar(4, color("253 253 205")) bar(5, color("138 202 182")) bar(6, color("24 150 160")) ///
bar(7, color("252 243 44"))  bar(8, color("228 123 07")) ///
bar(9, color("248 180 183")) bar(10, color("221 38 36")) bar(11, color("132 5 5"))  ///
legend(off)
	*sibling
graph bar (mean) ts_i_hde (mean) ts_h_hab  (mean) ts_j_hel ///
(mean) ts_a_not (mean) ts_g_hvi (mean) ts_f_neu ///
(mean) ts_e_negsolo (mean) ts_e_negtell   ///
(mean) ts_b_sco (mean) ts_c_cmp (mean) ts_d_vio, ///
over(site, label(angle(vertical)) ) ///
percentages yscale(range(0 100)) ytick(0(50)100) ylabel(0(50)100) ///
graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) stack ///
bar(1, color("50 100 50")) bar(2, color("50 185 106")) bar(3, color("50 250 106")) ///
bar(4, color("253 253 205")) bar(5, color("138 202 182")) bar(6, color("24 150 160")) ///
bar(7, color("252 243 44"))  bar(8, color("228 123 07")) ///
bar(9, color("248 180 183")) bar(10, color("221 38 36")) bar(11, color("132 5 5"))  ///
legend(off)
		//friends	
graph bar (mean) tf_i_hde (mean) tf_h_hab  (mean) tf_j_hel ///
(mean) tf_a_not (mean) tf_g_hvi (mean) tf_f_neu ///
(mean) tf_e_negsolo (mean) tf_e_negtell   ///
(mean) tf_b_sco (mean) tf_c_cmp (mean) tf_d_vio, ///
over(site, label(angle(vertical)) ) ///
percentages yscale(range(0 100)) ytick(0(50)100) ylabel(0(50)100) ///
graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) stack ///
bar(1, color("50 100 50")) bar(2, color("50 185 106")) bar(3, color("50 250 106")) ///
bar(4, color("253 253 205")) bar(5, color("138 202 182")) bar(6, color("24 150 160")) ///
bar(7, color("252 243 44"))  bar(8, color("228 123 07")) ///
bar(9, color("248 180 183")) bar(10, color("221 38 36")) bar(11, color("132 5 5"))  ///
legend(off)

*---- Load Long Data ------------------------
clear		
use "NormEnforcementDataLongSupplement.dta"
	
** Figure S7 Payoffs to sibling's versus friend's actions by site

	*a) Costs protagonist (as Fixed Effect of site)
xtmelogit costs_pro pro_5##site || id_participant: if pro_5 | pro_2, or 
margins pro_5, over(site)  predict(mu fixed)
marginsplot, scheme(s1manual)  xtitle("sibling vs. friend", size(large)) ///
ytitle("prob. action costs protagonist", size(medlarge)) title("") ///
legend(size(large)) xlabel(,labsize(large)) ylabel(,labsize(large)) ///
plot1opts(lcolor(pink) mcolor(pink)) ci1opts(lcolor(pink))	///
plot2opts(lcolor("180 180 180") mcolor("180 180 180")) ci2opts(lcolor("180 180 180")) ///
plot3opts(lcolor("49 183 204") mcolor("49 183 204")) ci3opts(lcolor("49 183 204")) ///
plot4opts(lcolor(maroon) mcolor(maroon)) ci4opts(lcolor(maroon)) ///
plot5opts(lcolor(blue) mcolor(blue)) ci5opts(lcolor(blue)) ///
plot6opts(lcolor(orange) mcolor(orange)) ci6opts(lcolor(orange)) ///
plot7opts(lcolor(green) mcolor(green)) ci7opts(lcolor(green)) ///
plot8opts(lcolor(purple) mcolor(purple)) ci8opts(lcolor(purple))

	*a) Costs protagonist (as Random Effect of site)
xtmelogit costs_pro pro_5 || site: pro_5 || id_participant: if pro_5 | pro_2, or 
matrix temp= r(table) 				//save results in matrix
predict u0 u1 u2, reffects
predict u0se u1se u2se, reses
egen pickone=tag(site)
gen u0rank = sum(pickone) 			//random effects for site: pro_5
local u0_mean=ln(temp[1,1])
gen u0_withmean = u0+`u0_mean'
sort u0 u1 u2 

serrbar u0_withmean u0se site if pickone==1, ///				//graph
xlabel(1(1)8, angle(forty_five) valuelabel labsize(large)) 	///		//graph label sites
scheme(s1manual) xtitle("site", size(large)) ylabel(,labsize(large)) ///
ytitle("Effect of protagonist=sibling", size(large)) ///			//graph parameters
yline(`u0_mean', lcolor(red) lstyle(dash) )

list site u0 if pickone==1 		//list results residual
list site u0se if pickone==1	//list results residuals SE
drop pickone u*

	*b) Costs violator (as Fixed Effect of site)
xtmelogit costs_violator pro_5##site || id_participant: if pro_5 | pro_2, or 
margins pro_5, over(site)  predict(mu fixed)
marginsplot, scheme(s1manual)  xtitle("sibling vs. friend", size(large)) ///
ytitle("prob. action costs violator", size(large)) title("") ///
legend(size(large)) xlabel(,labsize(large)) ylabel(,labsize(large)) ///
plot1opts(lcolor(pink) mcolor(pink)) ci1opts(lcolor(pink))	///
plot2opts(lcolor("180 180 180") mcolor("180 180 180")) ci2opts(lcolor("180 180 180")) ///
plot3opts(lcolor("49 183 204") mcolor("49 183 204")) ci3opts(lcolor("49 183 204")) ///
plot4opts(lcolor(maroon) mcolor(maroon)) ci4opts(lcolor(maroon)) ///
plot5opts(lcolor(blue) mcolor(blue)) ci5opts(lcolor(blue)) ///
plot6opts(lcolor(orange) mcolor(orange)) ci6opts(lcolor(orange)) ///
plot7opts(lcolor(green) mcolor(green)) ci7opts(lcolor(green)) ///
plot8opts(lcolor(purple) mcolor(purple)) ci8opts(lcolor(purple))

	*b) Costs violator (as Random Effect of site)
xtmelogit costs_violator pro_5 || site: pro_5 || id_participant: if pro_5 | pro_2, or 
matrix temp= r(table) 			//save results in matrix
predict u0 u1 u2, reffects
predict u0se u1se u2se, reses
egen pickone=tag(site)
gen u0rank = sum(pickone) 		//random effects for site_num: pro_5
local u0_mean=ln(temp[1,1])
gen u0_withmean = u0+`u0_mean'
sort u0 u1 u2 

serrbar u0_withmean u0se site if pickone==1, ///			//graph
xlabel(1(1)8, angle(forty_five) valuelabel labsize(large)) 	///	//graph label sites
scheme(s1manual) xtitle("site", size(large)) ylabel(,labsize(large)) ///
ytitle("Effect of protagonist=sibling", size(large)) ///		//graph parameters
yline(`u0_mean', lcolor(red) lpattern(dash))

list site u0 if pickone==1 		//list results residual
list site u0se if pickone==1	//list results residuals SE
drop pickone u*

	*c) Benefits violator (as Fixed Effect of site)
xtmelogit benefits_violator pro_5##site || id_participant: if pro_5 | pro_2, or 
margins pro_5, over(site)  predict(mu fixed)
marginsplot, scheme(s1manual)  xtitle("sibling vs. friend", size(large)) ///
ytitle("prob. action benefits violator", size(medlarge)) title("") ///
legend(size(large)) xlabel(,labsize(large)) ylabel(,labsize(large)) ///
plot1opts(lcolor(pink) mcolor(pink)) ci1opts(lcolor(pink))	///
plot2opts(lcolor("180 180 180") mcolor("180 180 180")) ci2opts(lcolor("180 180 180")) ///
plot3opts(lcolor("49 183 204") mcolor("49 183 204")) ci3opts(lcolor("49 183 204")) ///
plot4opts(lcolor(maroon) mcolor(maroon)) ci4opts(lcolor(maroon)) ///
plot5opts(lcolor(blue) mcolor(blue)) ci5opts(lcolor(blue)) ///
plot6opts(lcolor(orange) mcolor(orange)) ci6opts(lcolor(orange)) ///
plot7opts(lcolor(green) mcolor(green)) ci7opts(lcolor(green)) ///
plot8opts(lcolor(purple) mcolor(purple)) ci8opts(lcolor(purple))
		
** Figure S9 Predicted probability of protagonist punishing transgressor as function of site's authority reliance
	* Table S3a - baseline model
xtmelogit costs_notben_vio b4.protagonist##c.sm_aut_z   || site_num: || id_participant: , or  			//(& Section 3.2)
margins, at(sm_aut_z=(-1(1)1)) over(protagonist)  predict(mu fixed)
marginsplot, ytitle("Predicted prob. punishment") xtitle("Site authority-reliance z-score") ///
title("") scheme(s1manual) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
plot1opts(lcolor("237 205 194") mcolor("237 205 194")) ci1opts(lcolor("237 205 194"))	///
plot2opts(lcolor("180 180 180") mcolor("180 180 180")) ci2opts(lcolor("180 180 180")) ///
plot3opts(lcolor("49 183 204") mcolor("49 183 204")) ci3opts(lcolor("49 183 204")) ///
plot4opts(lcolor(maroon) mcolor(maroon)) ci4opts(lcolor(maroon))
legend(order(3 "rival" 2 "neighbor" 1 "friend" 4 "sibling" ) rows(1)) /// 						//lines in legend
legend(order(7 "rival" 6 "neighbor" 5 "friend" 8 "sibling" ) rows(1)) 							//markers in legend
	* Table S3b - comparing to wordcount control (group-level mean wordcount)
xtmelogit costs_notben_vio b4.protagonist##c.sm_aut_z sm_wordcount || site_num: || id_participant: , or

*---- Load Wide Data ------------------------
clear		
use "NormEnforcementDataSupplement.dta"
** Figure S10 Effect of ostracising on ostracizer's reputation
foreach a in "n" "s" "f" "r"{ 					// for neighbor, sibling, friend, rival (only 1st pictured)
	quietly: graph bar (mean) `a'_ostra_1 (mean) `a'_ostra_2 (mean) `a'_ostra_3 (mean) `a'_ostra_4, ///
	over(site, label(angle(vertical))) ///
	graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
	bar(1, fcolor("239 69 31") lcolor("239 69 31")) bar(2, fcolor("255 253 209") lcolor("255 253 209")) ///
	bar(3, fcolor("178 247 226") lcolor("178 247 226")) bar(4, fcolor("237 239 238") lcolor("237 239 238")) ///
	legend(order(1 "bad" 2 "no effect" 3 "good" 4 "split vote")) stack  ///
	title(Actor: `a') ytitle(Proportion of responses) name(`a')
	graph export "Ostracism_`a'.pdf", replace
}
graph drop _all

** Table S5 Proportion of variance at each level, (Robusticity Check)
foreach costben of varlist com_efficient_bi n_efficient_bi s_efficient_bi f_efficient_bi r_efficient_bi ///
com_costs_violator com_benefits_vic  com_costs_com com_costs_com_hi /// 
n_benefits_vic n_benefits_violator n_costs_violator n_costs_pro n_costs_pro_hi n_ostrabad_v_goodneutral ///
s_benefits_vic s_benefits_violator s_costs_violator s_costs_pro s_costs_pro_hi  s_ostrabad_v_goodneutral ///
f_benefits_vic f_benefits_violator f_costs_violator f_costs_pro f_costs_pro_hi  f_ostrabad_v_goodneutral ///
r_benefits_vic r_costs_violator r_costs_pro r_costs_pro_hi  r_ostrabad_v_goodneutral {
	*3-level model
	quietly:xtmelogit `costben' || site: || id_participant:, mle variance intpoints(7)
	quietly:_diparm lns1_1_1, f(exp(@)^2) d(exp(@)^2) //http://www.ats.ucla.edu/stat/stata/faq/diparm.htm
	local temp=r(est)
	quietly:_diparm lns2_1_1, f(exp(@)^2) d(2*exp(@)^2)
	local 3la= `temp'/(`temp' + r(est) + 3.29)
	*2-level model
	quietly:xtmelogit `costben'  || site: , mle variance intpoints(7)
	quietly:_diparm lns1_1_1, f(exp(@)^2) d(exp(@)^2) 
	local 2la= r(est)/(r(est)+3.29)
	*2-level model (battery story only)
	quietly:xtmelogit `costben'  || site: if story_bat==1 , mle variance intpoints(7)
	quietly:_diparm lns1_1_1, f(exp(@)^2) d(exp(@)^2) 
	local bat= r(est)/(r(est)+3.29)
	*2-level model (negligence story only)
	quietly:xtmelogit `costben'  || site: if story_neg==1 , mle variance intpoints(7)
	quietly:_diparm lns1_1_1, f(exp(@)^2) d(exp(@)^2) 
	local neg= r(est)/(r(est)+3.29)
	*2-level model (theft story only)
	quietly:xtmelogit `costben'  || site: if story_the==1 , mle variance intpoints(7)
	quietly:_diparm lns1_1_1, f(exp(@)^2) d(exp(@)^2) 

di "`costben'"
di "3la, " `3la' 
di "2la, " `2la' 
di "bat, " `bat' 
di "neg, " `neg' 
di "theft, " r(est)/(r(est)+3.29)
}	//end costben loop

*---- Load Long Data ------------------------
clear		
use "NormEnforcementDataLongSupplement.dta"
** Figure S13 Economic game behavior and responses to norm violation vignettes
	* Minimal acceptable offer (MAO) in third party punishment game at each site from Henrich et. al. 2006
gen MAO=.
replace MAO=19.33 if site==4
replace MAO=5 if site==8
replace MAO=10.31 if site==6
replace MAO=3.91 if site==7
replace MAO=16 if site==3
	* vignette outcomes in theft story
tabstat costs_pro if protagonist==2 & story_theft, by(site) 	//neutral neighbor, costs protagonist
tabstat costs_vio if protagonist==2 & story_theft, by(site) 	//neutral neighbor, costs violator
tabstat costs_pro if protagonist==0, by(site) 				//community, costs protagonist
tabstat costs_vio if protagonist==0, by(site) 				//community, costs violator
	* site-level MAO effect on payoff outcomes in vignette
xtmelogit costs_pro MAO if protagonist==2 & story_theft || site:  , or 
xtmelogit costs_vio  MAO if protagonist==2 & story_theft || site:  , or 
xtmelogit costs_pro MAO if protagonist==0 & story_theft || site:  , or 
xtmelogit costs_vio  MAO if protagonist==0 & story_theft || site:  , or 

*---------------------------------------
* 3. Unincorporated Questions
*---------------------------------------

*---- Load Wide Data ------------------------
clear		
use "NormEnforcementDataSupplement.dta"	

** 1. What is the effect of story condition 
		* on community,victim, authority involvement?	
foreach a in "com" "aut" "vic" {		// 
	xtmelogit `a' story_theft story_battery || site: || id_participant: , variance intpoints(7) or	
}
		* on how often victim is helped (more common in negligence condition because of arson?)
xtmelogit tcq_g_hvi story_negligence || site: || id_participant: , variance intpoints(7) or 
		* on how often victim is compensated (less common in battery since no property loss?)
xtmelogit tcq_c_cmp story_battery || site: || id_participant: , variance intpoints(7) or 

** 2. What are the reputational consequences of other behaviors for a neutral neighbor?
foreach a in "a_" "b_" "c_" "d_" "e_neg" "f_" "g_" "h_" "i_" "j_" {
	tab tnrep_`a' if (tnrep_`a'>=-1 | tnrep_`a'<=1) & tn_`a'==1
}

/** 3. Multi-level fit statistics with DIC weights (Requires runmlwin in Stata & MLWIN)

*---- Load Long Data ------------------------
clear		
use "NormEnforcementDataLongSupplement.dta"	

gen cons=1 
global MLwiN_path C:\Program Files (x86)\MLwiN v2.30\i386\mlwin.exe // !!! Pathway must be specified in own computer !!! 

foreach a in "n" "s" "f" "r" "com" {
foreach cb of varlist efficient benefits_vic costs_violator costs_pro costs_pro_hi ///
benefits_vio ostrabad_v_goodneutral {
	sort site_num id_participant q_num

	*2-level only model			
					
	quietly: runmlwin `cb' cons if protagon=="_`a'" , ///			// get initial values
	level1(id_participant:  ) ///
	discrete(distribution(binomial) link(logit) denominator(cons) pql2) nopause
			
	quietly: runmlwin `cb' cons if protagon=="_`a'" , ///			// MCMC
	level1(id_participant:  ) ///
	discrete(distribution(binomial) link(logit) denominator(cons)) ///
	mcmc(on) initsprevious nopause
	local ri= e(dic)
	
	*3-level using MLWin	
					 
	quietly: runmlwin `cb' cons if protagon=="_`a'" , ///			// get initial values
	level2(site_num: cons) ///
	level1(id_participant:  ) ///
	discrete(distribution(binomial) link(logit) denominator(cons) pql2) nopause

	quietly: runmlwin `cb' cons if protagon=="_`a'"  , ///			// MCMC
	level2(site_num: cons) ///
	level1(id_participant:  ) ///
	discrete(distribution(binomial) link(logit) denominator(cons)) ///
	mcmc(on) initsprevious nopause		

di "`cb', `a'," `ri' "," e(dic) 
}	//end cb loop
}	//end a loop





