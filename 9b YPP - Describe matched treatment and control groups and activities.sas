/* 9b. YPP - Describe matched treatment and control groups and activities */ 
/* Keith March 2016 */
/* Based on code by Sylvia March 2016 */

** Key data inputs:
*		- yppdpb.matchrate_YPP / yppdpb.matchrate_DPBEMA / yppdpb.matchrate_YPPb / yppdpb.matchrate_DPBEMAb (match rates)
*		- yppdpb.allmodel_YPP / yppdpb.allmodel_DPBEMA / yppdpb.allmodel_YPPb / yppdpb.allmodel_DPBEMAb (dataset of descriptive characteristics for participants and matched control)
*		- ypypp.monthly_ext (extended monthly outcomes data to 2015)
*		- yppdpb.rep0_YPP_1 / yppdpb.rep0_DPBEMA_1 / yppdpb.rep0_YPPb_1 / yppdpb.rep0_DPBEMAb_1(participant/historical comparison and matched comparison data)

** Key data outputs:
*		- yppdpb.graphs_YPP_1 / yppdpb.graphs_DPBEMA_1 / yppdpb.graphs_YPPb_1 / yppdpb.graphs_DPBEMAb_1(participant/historical comparison and matched comparison monthly outcomes data for graphing)
*		- yppdpb.graphs_YPPDPB_1 / yppdpb.graphs_YPPDPB1_b (participant/historical comparison and matched comparison monthly outcomes data for graphing - YPP/DPBEMA combined and timeframe standardised)
;

proc datasets lib=work kill;
run;

**********************************************************************;
**PART A: Comparison of YP and IYB participants and matched comparison;
**********************************************************************;

data matches_a;
	set yppdpb.matchrate_ypp(in=a keep=flag) yppdpb.matchrate_dpbema(in=b keep=flag);
	if a then part_type='YPP    ';
	else if b then part_type='DPBEMA';
run;

ods html body=
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output yppdpb\Match rates YPPDPB.xls";
proc freq data=matches_a;
	tables part_type*flag / nocol nopercent norow;
	title "Match rate for all strata - YPP DPB EMA with matching on 6 month+"; 
run;
ods html close;

data matches_b;
	set yppdpb.matchrate_yppb(in=a keep=flag) yppdpb.matchrate_dpbemab(in=b keep=flag);
	if a then part_type='YPP    ';
	else if b then part_type='DPBEMA';
run;

ods html body=
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output yppdpb\Match rates YPPDPB b.xls";
proc freq data=matches_b ;
	tables part_type*flag / nocol nopercent norow;
	title "Match rate for all strata - YPP DPB EMA with matching to month 0"; 
run;
ods html close;

data all_model;
	length part_type $ 8;
	set yppdpb.allmodel_YPP(in=a) yppdpb.allmodel_DPBEMA(in=b drop=year_started);
	if a then part_type='YPP    ';
	else if b then part_type='DPBEMA';
	age_year=floor(age);
	startyear=year(startdate);
	child_days_old_start=startdate-child_birthdt;
    if susp_da_cat in ('b1-2','c3-9') then susp_da_cat='b1-9';
    if stand_da_cat in ('b1-2','c3-9') then stand_da_cat='b1-9';
run;

data all_model_b;
	length part_type $ 6;
	set yppdpb.allmodel_YPPb(in=a) yppdpb.allmodel_DPBEMAb(in=b drop=year_started);
	if a then part_type='YPP    ';
	else if b then part_type='DPBEMA';
	age_year=floor(age);
	startyear=year(startdate);
	child_days_old_start=startdate-child_birthdt;
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

value num_chd
.='None'
2-high='2+';

value chd_age
.='None'
3-high='3+';

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
13=	'Canterbury' 
14=	'Otago' 
15=	'Southland' 
12,16,17,18= 'Tasman/Nelson/Marlborough/West Coast' 
99=	'NA';

value $ prevbenyn
''='No'
'NONE'='No'
other='Yes';

value $ prevben
'DPB Sole Parent'='Other'
'DPB Caring for Sick'='Other'
'Emergency Benefit'='Other'
'Emergency Maintenanc'='Other'
'Independent Youth Be'='Other'
"Invalid's Benefit"='Other'
'Job Seeker Health Co'='Sickness Benefit or equiv'
'Job Seeker Work Read'='Unemployment Benefit or equiv'
'NONE'='Never on benefit'
'Sickness Benefit'='Sickness Benefit or equiv'
'Supported Living Pay'='Other'
'Unemployment Benefit'='Unemployment Benefit or equiv'
'Youth Payment'='Other';

value $ bentyp
'Emergency Maintenance'='Emergency Maintenance'
'Job Seeker'='Job Seeker'
'Sickness'='Sickness'
'Sole Parent'='Sole Parent'
'Young Parent Payment'='Young Parent Payment'
other='Other';

value days_old
low-<31='<1 month'
31-<93='1-3 months'
93-<366='3-12 months'
366-<731='1-2 years'
731-high='2+ years';
run;

%let prof_vars=startyear age_det female european maori pacific asian ethoth nzdep 
			    region age_when_left_sch decile nschools hqual hterqual ncea_cr_l1-ncea_cr_l3 prop_cr_ext_cat
			    sedu_da trua_da stand_da_cat susp_da_cat prop_os_aschild_cat ever_onben_aschild prop_onben_aschild_cat
			    mother_unqual cg_cust cg_comm child_not_cat child_any_fdgs_abuse child_cyf_place child_yj_referral_cat all_act time_since_school
			    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 mean_earn_last6
			    occskills_only genskills_only gen_n_occ_skills num_child child_days_old_start prev_ben partner benefittype yts_ysneet schtype schauth;

ods html body= 
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output yppdpb\Pubvar profile comparison YPP DPBEMA.xls";
proc tabulate data=all_model missing format=9.;
var weight;
class &prof_vars. part_type treat;
tables (&prof_vars. ALL),part_type*treat*weight*(SUM)/NOCELLMERGE;
format nzdep nzdep. region region.  age_when_left_sch agelsch. decile  nzdep.
    nschools  nschool. ncea_cr_l1-ncea_cr_l2   credits. ncea_cr_l3 creds.  
    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 months.
    mean_earn_last6 earnings. age_young_chd chd_age. num_child num_chd. child_days_old_start days_old. prev_ben $prevben. benefittype $bentyp.;
title "Comparison of characterstics of matched samples - YPP and DPBEMA";
run;
ods html close;

ods html body= 
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output yppdpb\Pubvar profile comparison YPP DPBEMA b.xls";
proc tabulate data=all_model_b missing format=9.;
var weight;
class &prof_vars. part_type treat;
tables (&prof_vars. ALL),part_type*treat*weight*(SUM)/NOCELLMERGE;
format nzdep nzdep. region region.  age_when_left_sch agelsch. decile  nzdep.
    nschools  nschool. ncea_cr_l1-ncea_cr_l2   credits. ncea_cr_l3 creds.  
    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 months.
    mean_earn_last6 earnings. age_young_chd chd_age. num_child num_chd. child_days_old_start days_old. prev_ben $prevben. benefittype $bentyp.;
title "Comparison of characterstics of matched samples - YPP and DPBEMA";
run;
ods html close;

data prevben(keep=snz_uid refmth);
	set all_model;
	where prev_ben not in ('','NONE');
run;

data noprevben(keep=snz_uid refmth);
	set all_model;
	where prev_ben='NONE';
run;

data partner(keep=snz_uid refmth);
	set all_model;
	where partner=1;
run;

data nopartner(keep=snz_uid refmth);
	set all_model;
	where partner=0;
run;

proc sql;
	create table allmod_prevben as select a.*
	from all_model a inner join prevben b
	on (a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid and a._refmth=b.refmth);
quit;

proc sql;
	create table allmod_noprevben as select a.*
	from all_model a inner join noprevben b
	on (a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid and a._refmth=b.refmth);
quit;

proc sql;
	create table allmod_partner as select a.*
	from all_model a inner join partner b
	on (a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid and a._refmth=b.refmth);
quit;

proc sql;
	create table allmod_nopartner as select a.*
	from all_model a inner join nopartner b
	on (a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid and a._refmth=b.refmth);
quit;


ods html body= 
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output yppdpb\Pubvar profile comparison YPP DPBEMA strat.xls";
proc tabulate data=allmod_prevben missing format=9.;
var weight;
class &prof_vars. part_type treat;
tables (&prof_vars. ALL),part_type*treat*weight*(SUM)/NOCELLMERGE;
format nzdep nzdep. region region.  age_when_left_sch agelsch. decile  nzdep.
    nschools  nschool. ncea_cr_l1-ncea_cr_l2   credits. ncea_cr_l3 creds.  
    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 months.
    mean_earn_last6 earnings. age_young_chd chd_age. num_child num_chd. child_days_old_start days_old. prev_ben $prevben. benefittype $bentyp.;
title "Comparison of characterstics of matched samples - YPP and DPBEMA Previous ben";
run;
proc tabulate data=allmod_noprevben missing format=9.;
var weight;
class &prof_vars. part_type treat;
tables (&prof_vars. ALL),part_type*treat*weight*(SUM)/NOCELLMERGE;
format nzdep nzdep. region region.  age_when_left_sch agelsch. decile  nzdep.
    nschools  nschool. ncea_cr_l1-ncea_cr_l2   credits. ncea_cr_l3 creds.  
    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 months.
    mean_earn_last6 earnings. age_young_chd chd_age. num_child num_chd. child_days_old_start days_old. prev_ben $prevben. benefittype $bentyp.;
title "Comparison of characterstics of matched samples - YPP and DPBEMA No previous ben";
run;
ods html close;


ods html body= 
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output yppdpb\Pubvar profile comparison YPP DPBEMA strat 2.xls";
proc tabulate data=allmod_partner missing format=9.;
var weight;
class &prof_vars. part_type treat;
tables (&prof_vars. ALL),part_type*treat*weight*(SUM)/NOCELLMERGE;
format nzdep nzdep. region region.  age_when_left_sch agelsch. decile  nzdep.
    nschools  nschool. ncea_cr_l1-ncea_cr_l2   credits. ncea_cr_l3 creds.  
    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 months.
    mean_earn_last6 earnings. age_young_chd chd_age. num_child num_chd. child_days_old_start days_old. prev_ben $prevben. benefittype $bentyp.;
title "Comparison of characterstics of matched samples - YPP and DPBEMA Partner";
run;
proc tabulate data=allmod_nopartner missing format=9.;
var weight;
class &prof_vars. part_type treat;
tables (&prof_vars. ALL),part_type*treat*weight*(SUM)/NOCELLMERGE;
format nzdep nzdep. region region.  age_when_left_sch agelsch. decile  nzdep.
    nschools  nschool. ncea_cr_l1-ncea_cr_l2   credits. ncea_cr_l3 creds.  
    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 months.
    mean_earn_last6 earnings. age_young_chd chd_age. num_child num_chd. child_days_old_start days_old. prev_ben $prevben. benefittype $bentyp.;
title "Comparison of characterstics of matched samples - YPP and DPBEMA No partner";
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

%graphdata(yppdpb,1,YPP,);
%graphdata(yppdpb,1,YPP,b);
%graphdata(yppdpb,1,DPBEMA,);
%graphdata(yppdpb,1,DPBEMA,b);

%macro convert(out,run);
array &out.&run.{*} &out.1-&out.79;
do i=1 to 79;
	time{i}=&out.&run.{i};
	outcome="&out.       ";
end;
output;
%mend convert;

%macro graphypp(strata);
data yppdpb.graphs_YPPDPB&strata.(drop=i sch: emp: study: ben: neet:);
	length part_type $ 6;
	set yppdpb.graphs_YPP&strata.(in=a keep=snz_uid refmth pscore weight _snz_uid _refmth treat strata sch: emp: study: ben: neet:) 
		yppdpb.graphs_DPBEMA&strata.(in=b keep=snz_uid refmth pscore weight _snz_uid _refmth treat strata sch: emp: study: ben: neet:);  
	by snz_uid refmth;
	array time{*} t1-t91;
	if a then do;
		part_type='YPP ';
		%convert(sch,A);
		%convert(study,A);
		%convert(neet,A);
		%convert(ben,A);
		%convert(emp,A);
		%convert(emp_noben,A);
		%convert(emp_mw,A);
	end;
	else if b then do;
		part_type='DPBEMA';
		%convert(sch,B);
		%convert(study,B);
		%convert(neet,B);
		%convert(ben,B);
		%convert(emp,B);
		%convert(emp_noben,B);
		%convert(emp_mw,B);
	end;
run;
%mend graphypp;

%graphypp(1);

ods html body=
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output yppdpb\Matched_activity_graphs_YPPDPB.xls";

proc tabulate data=yppdpb.graphs_YPPDPB1 missing format=9.;
	class part_type treat strata outcome;
	var t1-t91;
	weight weight;
	tables (strata*outcome*part_type*treat),((t1-t91)*(SUM) (t1-t91)*(SUMWGT)) /NOCELLMERGE;
	title "Matched activity counts - YPP DPBEMA";
run;
ods html close;

%macro convert(out,run);
array &out.&run.{*} &out.1-&out.79;
do i=1 to 79;
	time{i}=&out.&run.{i};
	outcome="&out.       ";
end;
output;
%mend convert;

%macro graphyppb(strata);
data yppdpb.graphs_YPPDPB&strata._b(drop=i sch: emp: study: ben: neet:);
	length part_type $ 6;
	set yppdpb.graphs_YPPb&strata.(in=a keep=snz_uid refmth pscore weight _snz_uid _refmth treat strata sch: emp: study: ben: neet: nsa:) 
		yppdpb.graphs_DPBEMAb&strata.(in=b keep=snz_uid refmth pscore weight _snz_uid _refmth treat strata sch: emp: study: ben: neet: nsa:);  
	by snz_uid refmth;
	array time{*} t1-t91;
	if a then do;
		part_type='YPP ';
		%convert(sch,A);
		%convert(study,A);
		%convert(neet,A);
		%convert(ben,A);
		%convert(emp,A);
		%convert(emp_noben,A);
		%convert(emp_mw,A);
	end;
	else if b then do;
		part_type='DPBEMA';
		%convert(sch,B);
		%convert(study,B);
		%convert(neet,B);
		%convert(ben,B);
		%convert(emp,B);
		%convert(emp_noben,B);
		%convert(emp_mw,B);
	end;
run;
%mend graphyppb;

%graphyppb(1);

ods html body=
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output yppdpb\Matched_activity_graphs_YPPDPB_b.xls";
proc tabulate data=yppdpb.graphs_YPPDPB1_b missing format=9.;
	class part_type treat strata outcome;
	var t1-t91;
	weight weight;
	tables (strata*outcome*part_type*treat),((t1-t91)*(SUM) (t1-t91)*(SUMWGT)) /NOCELLMERGE;
run;
ods html close;

** Now split the YPP DPB population according to whether they have a benefit history prior to YPP DPB or not;
proc sql;
	create table graph_prevben as select a.*
	from yppdpb.graphs_YPPDPB1 a inner join prevben b
	on (a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid and a._refmth=b.refmth);
quit;

proc sql;
	create table graph_noprevben as select a.*
	from yppdpb.graphs_YPPDPB1 a inner join noprevben b
	on (a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid and a._refmth=b.refmth);
quit;

ods html body=
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output yppdpb\Matched_activity_graphs_YPPDPB strat.xls";
proc tabulate data=graph_prevben missing format=9.;
	class part_type treat strata outcome;
	var t1-t91;
	weight weight;
	tables (strata*outcome*part_type*treat),((t1-t91)*(SUM) (t1-t91)*(SUMWGT)) /NOCELLMERGE;
run;
proc tabulate data=graph_noprevben missing format=9.;
	class part_type treat strata outcome;
	var t1-t91;
	weight weight;
	tables (strata*outcome*part_type*treat),((t1-t91)*(SUM) (t1-t91)*(SUMWGT)) /NOCELLMERGE;
run;
ods html close;

** And finally split the YPP DPB population according to whether they have a partner or not;
proc sql;
	create table graph_partner as select a.*
	from yppdpb.graphs_YPPDPB1 a inner join partner b
	on (a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid and a._refmth=b.refmth);
quit;

proc sql;
	create table graph_nopartner as select a.*
	from yppdpb.graphs_YPPDPB1 a inner join nopartner b
	on (a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid and a._refmth=b.refmth);
quit;

ods html body=
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output yppdpb\Matched_activity_graphs_YPPDPB strat 2.xls";
proc tabulate data=graph_partner missing format=9.;
	class part_type treat strata outcome;
	var t1-t91;
	weight weight;
	tables (strata*outcome*part_type*treat),((t1-t91)*(SUM) (t1-t91)*(SUMWGT)) /NOCELLMERGE;
run;
proc tabulate data=graph_nopartner missing format=9.;
	class part_type treat strata outcome;
	var t1-t91;
	weight weight;
	tables (strata*outcome*part_type*treat),((t1-t91)*(SUM) (t1-t91)*(SUMWGT)) /NOCELLMERGE;
run;
ods html close;
