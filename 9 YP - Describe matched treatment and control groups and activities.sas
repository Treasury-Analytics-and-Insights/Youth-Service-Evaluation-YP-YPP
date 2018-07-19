/* 9. YP - Describe matched treatment and control groups and activities */ 
/* Keith March 2016 */
/* Based on code by Sylvia March 2016 */

** Key data inputs:
*		- ypiyb.matchrate_YP / ypiyb.matchrate_IYB (match rates)
*		- ypiyb.allmodel_YP / ypiyb.allmodel_IYB (dataset of descriptive characteristics for participants and matched control)
*		- ypypp.monthly_ext (extended monthly outcomes data to 2015)
*		- ypiyb.rep0_yp_1 / ypiyb.rep0_iyb_1 (participant/historical comparison and matched comparison data)

** Key data outputs:
*		- ypiyb.graphs_yp_1 / ypiyb.graphs_iyb_1 (participant/historical comparison and matched comparison monthly outcomes data for graphing)
*		- ypiyb.graphs_ypiyb_1 (participant/historical comparison and matched comparison monthly outcomes data for graphing - YP/IYB combined and timeframe standardised)
;

proc datasets lib=work kill;
run;

**********************************************************************;
**PART A: Comparison of YP and IYB participants and matched comparison;
**********************************************************************;

proc freq data=ypiyb.matchrate_yp;
	tables strata*flag / nocol nopercent norow;
	tables strata*flag / nocol nopercent nofreq;
	title "Match rate for all strata - YP with contemperaneous matching"; 
run;
proc freq data=ypiyb.matchrate_iyb;
	tables strata*flag / nocol nopercent norow;
	tables strata*flag / nocol nopercent nofreq;
	title "Match rate for all strata - IYB with contemperaneous matching"; 
run;

data all_model;
	set ypiyb.allmodel_YP(in=a) ypiyb.allmodel_IYB(in=b);
	if a then part_type='YP ';
	else if b then part_type='IYB';
	startyear=year(startdate);
    if susp_da_cat in ('b1-2','c3-9') then susp_da_cat='b1-9';
    if stand_da_cat in ('b1-2','c3-9') then stand_da_cat='b1-9';
run;

**Tables for publication;
proc format;
value agelsch
.='NA'
1-15='LT16'
16='16'
17='17'
18='18'
19-high='19+';

value nzdep
1-2='1-2'
3-4='3-4'
5-6='5-6'
7-8='7-8'
9-10='9-10'
99='NA';

value nschool
.,0,1='0-1'
2='2'
3='3'
4-high='4+';

value months
0,.='aNone'
1-6='b1-6mths'
7-12='c7-12mths'
13-18='d13-18mths';

value earnings
.,0='aNone'
.0001-<500='bLT$500'
500-<1000='cLT$1000'
1000-<2500='dLT$2500'
2500-high='dGT$2500';

value credits
.,0='None'
1-<40='LT40'
40-<60='40-59'
60-<80='60-67'
80-high='80+';

value creds
.,0='None'
1-<40='LT40'
40-high='40+';

value $ prevbenyn
''='No'
'NONE'='No'
other='Yes';

value $ bentyp
'Youth Payment'='Youth Payment'
other='Other';

value region
1='Northland'
2='Auckland' 
3='Waikato' 
4='Bay of Plenty'
5='Gisborne' 
6='Hawkes Bay' 
7='Taranaki' 
8=	'Manawatu-Wanganui' 
9=	'Wellington' 
12=	'West Coast' 
13=	'Canterbury' 
14=	'Otago' 
15=	'Southland' 
16=	'Tasman' 
17=	'Nelson' 
18=	'Marlborough' 
99=	'NA';
run;

ods html body= 
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output ypiyb\Pubvar profile comparison YP IYB.xls";
%let prof_vars=startyear age_det female european maori pacific asian ethoth nzdep 
			    region age_when_left_sch decile nschools hqual hterqual ncea_cr_l1-ncea_cr_l3 prop_cr_ext_cat
			    sedu_da trua_da stand_da_cat susp_da_cat prop_os_aschild_cat ever_onben_aschild prop_onben_aschild_cat
			    mother_unqual cg_cust cg_comm child_not_cat child_any_fdgs_abuse child_cyf_place child_yj_referral_cat all_act time_since_school
			    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 mean_earn_last6
			    occskills_only genskills_only gen_n_occ_skills num_child prev_ben partner benefittype yts_ysneet schtype schauth;

proc tabulate data=all_model missing format=9.;
var weight;
class &prof_vars. part_type treat;
tables (&prof_vars. ALL),part_type*treat*weight*(SUM)/NOCELLMERGE;
format nzdep nzdep. region region.  age_when_left_sch agelsch. decile  nzdep.
    nschools  nschool. ncea_cr_l1-ncea_cr_l2   credits. ncea_cr_l3 creds.  
    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 months.
    mean_earn_last6 earnings. prev_ben $prevbenyn. benefittype $bentyp.;
title "Comparison of characterstics of matched samples - YP and IYB";
run;
ods html close;

**********************************************************************;
**PART B: Comparison of activity pre and post-participation           ;
**********************************************************************;
%macro graphdata(lib,strata,part_type,suff);
proc sort data=&lib..rep0_&part_type.&suff.&strata.; 
	by snz_uid refmth strata; 
run;
data &lib..graphs_&part_type.&suff.&strata.(drop=nemp: i);
merge &lib..rep0_&part_type.&suff.&strata.(in=a keep=snz_uid refmth strata treat _snz_uid _refmth pscore weight)
	 ypypp.monthly_ext;  
by snz_uid refmth;
if a;
	array emp_mw{*} emp_mw1-emp_mw79;
	array nemp_mw{*} nemp_mw1-nemp_mw79;
	array emp{*} emp1-emp79;
	array nemp{*} nemp1-nemp79;
	array ben{*} ben1-ben79;
	array emp_noben{*} emp_noben1-emp_noben79;
	** Create a new employment and off benefit outcome;
	do i=1 to 79;
		emp{i}=nemp{i};
		emp_mw{i}=nemp_mw{i};
		if ben{i}=0 and emp{i}=1 then emp_noben{i}=1;
		else if ben{i}=. or emp{i}=. then emp_noben{i}=.;
		else emp_noben{i}=0;
	end;
if treat=1 then treat2='Treatment ';
else if treat=0 then treat2='Comparison';
run;
%mend graphdata;

%graphdata(ypiyb,1,YP,);
%graphdata(ypiyb,1,IYB,);

%macro convert(out,run);
array &out.&run.{*} &out.1-&out.79;
do i=1 to 79;
	time{i}=&out.&run.{i};
	outcome="&out.       ";
end;
output;
%mend convert;

%macro graphyp(strata);
data ypiyb.graphs_YPIYB&strata.(drop=i sch: emp: study: ben: neet:);
	set ypiyb.graphs_YP&strata.(in=a keep=snz_uid refmth pscore weight _snz_uid _refmth treat strata sch: emp: study: ben: neet: nsa:) 
		ypiyb.graphs_IYB&strata.(in=b keep=snz_uid refmth pscore weight _snz_uid _refmth treat strata sch: emp: study: ben: neet: nsa:);
	by snz_uid refmth;
	array time{*} t1-t91;
	if a then do;
		part_type='YP ';
		%convert(sch,A);
		%convert(study,A);
		%convert(neet,A);
		%convert(ben,A);
		%convert(emp,A);
		%convert(emp_noben,A);
		%convert(emp_mw,A);
	end;
	else if b then do;
		part_type='IYB';
		%convert(sch,B);
		%convert(study,B);
		%convert(neet,B);
		%convert(ben,B);
		%convert(emp,B);
		%convert(emp_noben,B);
		%convert(emp_mw,B);
	end;
run;
%mend graphyp;

%graphyp(1);


ods html body=
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output ypiyb\Matched_activity_graphs_YPIYB.xls";

proc tabulate data=ypiyb.graphs_YPIYB1 missing format=9.;
	class part_type treat outcome;
	var t1-t91;
	weight weight;
	tables (outcome*part_type*treat),((t1-t91)*(SUM) (t1-t91)*(SUMWGT)) /NOCELLMERGE;
	title;
run;
ods html close;









