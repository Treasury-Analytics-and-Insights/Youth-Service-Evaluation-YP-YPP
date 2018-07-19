/* 7. All groups - Calculate period-adjusted impacts and standard errors*/
/*	Also output information participants and comparison groups, all potential matched comparisons, match rates, matching variables etc;
/* Keith March 2016 */
/* Based on code by Sylvia March 2016 */

** Key data inputs:
*		- ypiyb.matchrate_YP / ypiyb.matchrate_IYB / yppdpb.matchrate_YPP / yppdpb.matchrate_DPBEMA / yppdpb.matchrate_YPPb / yppdpb.matchrate_DPBEMAb (match rates)
*		- ypypp.all_participants / ypypp.all_participants1 / ypypp.all_participants2 / ypypp.model_final_restricted (participant/historical comparison datasets at varying stages of exclusions being applied)
*		- ypiyb.allcases_yp_1 / ypiyb.allcases_IYB_1 / yppdpb.allcases_ypp_1 / yppdpb.allcases_dpbema_1 (all participants and potential controls)
*		- ys5.monthly_revised_2015update / yts.monthly_revised / ys5.depvars_revised / yts.depvars (monthly outcomes and outcomes variables for potential matched comparison groups)
*		- ypiyb.pe_yp_1 / ypiyb.pe_iyb_1 / yppdpb.pe_ypp_1 / yppdpb.pe_dpbema_1 / yppdpb.pe_yppb_1 / yppdpb.pe_dpbemab_1 (parameter estimates)
*		- byp.allreps_1 / biyb.allreps_1 / bypp.allreps_1 / bdpbema.allreps_1 / byppb.allreps_1 / bdpbemab.allreps_1 (all participants/historical comparisons and matched comparisons - all bootstrap replicates)

** Key data outputs:
* 		- ypypp.depvars_temp_all (datasets containing all outcomes variables of interest for standard error estimation - including updated data to 2015)
*		- byp.sedata_1 / bypp.sedata_1 / byppb.sedata_1 / biyb.sedata_1 / bdpbema.sedata_1 / bdpbemab.sedata_1 (standard errors data)
*		- byp.yp_sedata_1 / bypp.ypp_sedata_1 / byppb.yppb_sedata_1 / biyb.iyb_sedata_1 / bdpbema.dpbema_sedata_1 / bdpbemab.bdpbemab_sedata_1 (standard errors data with prevben and partner info added)
*		- ypiyb.combinedyp / ypiyb.combinedyp / ypiyb.combinedypp (bring together data for both participants and historical comparison groups, and their respective matched comparisons;
* 		- ypypp.results_YPall / ypypp.results_YPPall / ypypp.results_YPPball (brings together estimated outcomes, standard errors, and confidence limits for all participants)
* 		- ypypp.results_YPprevben / ypypp.results_YPPprevben / ypypp.results_YPPbprevben  (brings together estimated impact, standard errors, and confidence limits for participants with and without previous time on benefit)
* 		- ypypp.results_YPpartner / ypypp.results_YPPpartner / ypypp.results_YPPbpartner  (brings together estimated impact, standard errors, and confidence limits for participants with and without a partner)

** Bring together all of the information on match rates from the various models - YP, IYB (YP historical comparison), YPP, 
** 	DPBEMA (YPP historical comparison), and alternative specifications of matching for YPP and DPBEMA;
data all_matchrate;
length part_type $ 7;
set ypiyb.matchrate_YP      (in=a keep=flag)
	ypiyb.matchrate_IYB     (in=b keep=flag)
	yppdpb.matchrate_YPP    (in=c keep=flag)
	yppdpb.matchrate_DPBEMA (in=d keep=flag)
	yppdpb.matchrate_YPPb   (in=e keep=flag)
	yppdpb.matchrate_DPBEMAb (in=f keep=flag);
	if a then part_type='YP ';
	else if b then part_type='IYB';
	else if c then part_type='YPP';
	else if d then part_type='DPBEMA';
	else if e then part_type='YPPb';
	else if f then part_type='DPBEMAb';
run;

** Track the population selection process - summarising how many people are excluded at each stage of the process;
** 	until we get the final study population and then the matched population;
proc freq data=ypypp.all_participants;
	tables part_type /missing nopercent nocol norow out=out1(drop=percent rename=(count=counta));
	where '01jan2009'd <= startdate < '01jan2015'd and participant in (0,1) and (startdate<'01jul2012'd or startdate>'31jul2012'd);
run;
proc freq data=ypypp.all_participants1;
	tables part_type /missing nopercent nocol norow out=out2(drop=percent rename=(count=countb));
	where participant in (0,1) and (startdate<'01jul2012'd or startdate>'31jul2012'd);
run;
proc freq data=ypypp.all_participants2;
	tables part_type /missing nopercent nocol norow out=out3(drop=percent rename=(count=countc));
	where participant in (0,1) and (startdate<'01jul2012'd or startdate>'31jul2012'd);
run;
proc freq data=ypypp.model_final_restricted;
	tables part_type /missing nopercent nocol norow out=out4(drop=percent rename=(count=countd));
	where participant in (0,1) and (startdate<'01jul2012'd or startdate>'31jul2012'd);
run;
proc freq data=all_matchrate;
	tables part_type /missing nopercent nocol norow out=out5(drop=percent rename=(count=counte));
run;
proc freq data=all_matchrate;
	tables part_type /missing nopercent nocol norow out=out6(drop=percent rename=(count=countf));
	where flag='Matched';
run;
data selection;
	merge out1 out2 out3 out4 out5 out6;
	by part_type;
run;

** Output population selection counts;
ods html body=
"\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output\Popn selection all.xls";
proc print data=selection;
	title "Population selection for all YP YPP models"; 
run;
ods html close;

** Set variables we want to use to break down and describe our potential matched control populations;
%let prof_vars=startyear age_det female european maori pacific asian ethoth nzdep 
			    region age_when_left_sch decile nschools hqual hterqual ncea_cr_l1-ncea_cr_l3 prop_cr_ext_cat
			    sedu_da trua_da stand_da_cat susp_da_cat prop_os_aschild_cat ever_onben_aschild prop_onben_aschild_cat
			    mother_unqual cg_cust cg_comm child_not_cat child_any_fdgs_abuse child_cyf_place child_yj_referral_cat all_act time_since_school
			    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 mean_earn_last6
			    occskills_only genskills_only gen_n_occ_skills num_child prev_ben partner benefittype yts_ysneet schtype schauth;

** Bring together all potential matched control observations;
data all_control;
	length part_type $ 8;
	set ypiyb.allcases_yp_1(in=a keep=snz_uid refmth &prof_vars. startdate treat) 
		ypiyb.allcases_IYB_1(in=b keep=snz_uid refmth &prof_vars. startdate treat)
		yppdpb.allcases_ypp_1(in=c keep=snz_uid refmth &prof_vars. startdate treat) 
		yppdpb.allcases_dpbema_1(in=d keep=snz_uid refmth &prof_vars. startdate treat);
	if a and floor(age_det)<18 then part_type='B YP ';
	else if b and floor(age_det)<18 then part_type='A IYB';
	else if c then part_type='D YPP ';
	else if d then part_type='C DPBEMA';
	else delete;
	startyear=year(startdate);
    if susp_da_cat in ('b1-2','c3-9') then susp_da_cat='b1-9';
    if stand_da_cat in ('b1-2','c3-9') then stand_da_cat='b1-9';
	where treat=0;
run;

** Identify each matched control UID/reference month combination; 
proc sort data=all_control out=control_uids(keep=snz_uid refmth) nodupkey;
	by snz_uid refmth;
run;

**Output control population descriptions;
** Set formats;
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

** Describe potential matched comparison groups using descriptive variables;
ods html body= 
"\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output\Pubvar controls.xls";
proc tabulate data=all_control missing format=9.;
class &prof_vars. part_type treat;
tables (&prof_vars. ALL),part_type*treat*(N)/NOCELLMERGE;
format nzdep nzdep. region region.  age_when_left_sch agelsch. decile  nzdep.
    nschools  nschool. ncea_cr_l1-ncea_cr_l2 credits. ncea_cr_l3 creds.  
    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 months.
    mean_earn_last6 earnings. prev_ben $prevbenyn. benefittype $bentyp.;
title "Comparison of characterstics of matched samples";
run;
ods html close;

** Create outcomes variables for our potential matched comparison groups - study and employment pre-participation and 6, 12, and 18 months post-participation;
**	Separately look at potential controls for the historical comparison groups (pre_post=pre) and the YP/YPP participants (pre_post=post);
data control_out(keep=snz_uid refmth pre_post emp_: study_:);
	merge control_uids(in=a) 
			ys5.monthly_revised_2015update(in=b keep=snz_uid refmth study19 study25 study31 study37 study43 study49 emp19 emp25 emp31 emp37 emp43 emp49)
			yts.monthly_revised(in=c keep=snz_uid refmth study43 study49 study55 study61 study67 study73 emp43 emp49 emp55 emp61 emp67 emp73 emp79);
	by snz_uid refmth;
	if a and (b or c);
	if c then do;
		pre_post='Pre ';
		study_pre6=study43; study_zero=study49; study_post6=study55; study_post12=study61; study_post18=study67; study_post24=study73;
		emp_pre6=emp43; emp_zero=emp49; emp_post6=emp55; emp_post12=emp61; emp_post18=emp67; emp_post24=emp73;
	end;
	else if b then do;
		pre_post='Post';
		study_pre6=study19; study_zero=study25; study_post6=study31; study_post12=study37; study_post18=study43; study_post24=study49;
		emp_pre6=emp19; emp_zero=emp25; emp_post6=emp31; emp_post12=emp37; emp_post18=emp43; emp_post24=emp49;
	end;
run;

** Create outcomes variables for our potential matched comparison group - level 1, 2 and 3 qualifications in the reference year and up to three calendar years later;
data control_out2;
	merge control_out(in=a) 
		ys5.depvars_revised(in=b keep=snz_uid refmth i1hqual_y0 i1hqual_y1 i1hqual_y2 i1hqual_y3 i2hqual_y0 i2hqual_y1 i2hqual_y2 i2hqual_y3 i3hqual_y0 i3hqual_y1 i3hqual_y2 i3hqual_y3)
		yts.depvars(in=b keep=snz_uid refmth i1hqual_y0 i1hqual_y1 i1hqual_y2 i1hqual_y3 i2hqual_y0 i2hqual_y1 i2hqual_y2 i2hqual_y3 i3hqual_y0 i3hqual_y1 i3hqual_y2 i3hqual_y3);
	by snz_uid refmth;
	if a and b;
run;

** Describe the potential control groups in the historical and YP/YPP periods;
proc tabulate data=control_out2 missing format=9.;
class pre_post study_pre6 study_zero study_post6 study_post12 study_post18 study_post24 emp_pre6 emp_zero emp_post6 emp_post12 emp_post18 emp_post24
		i1hqual_y0 i1hqual_y1 i1hqual_y2 i1hqual_y3 i2hqual_y0 i2hqual_y1 i2hqual_y2 i2hqual_y3 i3hqual_y0 i3hqual_y1 i3hqual_y2 i3hqual_y3;
tables (study_pre6 study_zero study_post6 study_post12 study_post18 study_post24 emp_pre6 emp_zero emp_post6 emp_post12 emp_post18 emp_post24
		i1hqual_y0 i1hqual_y1 i1hqual_y2 i1hqual_y3 i2hqual_y0 i2hqual_y1 i2hqual_y2 i2hqual_y3 i3hqual_y0 i3hqual_y1 i3hqual_y2 i3hqual_y3 ALL),pre_post*(N)/NOCELLMERGE;
title "Comparison of pre-post for the potential control population";
run;

** Create monthly study, employment and benefit outcomes for the potential matched comparisons;
data control_mths(keep=snz_uid refmth pre_post emp: study: ben:);
	merge control_uids(in=a) 
			ys5.monthly_revised_2015update(in=b keep=snz_uid refmth study: emp: ben:)
			yts.monthly_revised(in=c keep=snz_uid refmth study: emp: ben:);
	by snz_uid refmth;
	if a and (b or c);
	if c then do;
		pre_post='Pre ';
	end;
	else if b then do;
		pre_post='Post';
	end;
run;

** Macro to convert historical and YP/YPP period data to the same 79 month pre-post participation periods;
%macro convert(out,run);
array &out.&run.{*} &out.1-&out.79;
do i=1 to 79;
	time{i}=&out.&run.{i};
	outcome="&out.       ";
end;
output;
%mend convert;

** Standardise pre and post outcomes data to the same 79 month pre-post participation periods - because of different lengths of pre-post;
** 	data available we end up with 91 time points;
%macro graph;
data control_mths2(drop=i emp: study: ben:);
	set control_mths;
	by snz_uid refmth;
	array time{*} t1-t91;
	if pre_post="Pre" then do;
		%convert(study,A);
		%convert(ben,A);
		%convert(emp,A);
	end;
	else if pre_post="Post" then do;
		%convert(study,B);
		%convert(ben,B);
		%convert(emp,B);
	end;
run;
%mend graph;

%graph;

proc tabulate data=control_mths2 missing format=9.;
	class pre_post outcome;
	var t1-t91;
	tables (outcome*pre_post),((t1-t91)*(SUM)) /NOCELLMERGE;
	title;
run;

** Output match rates;
ods html body=
"\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output\Match rates all.xls";
proc freq data=all_matchrate;
	tables part_type*flag / nocol nopercent norow;
	title "Match rate for all YP YPP models"; 
run;
ods html close;

** Output matching variables;
proc sort data=ypiyb.pe_yp_1(keep=variable) out=pe_yp_1 nodupkey; by variable;run;
proc sort data=ypiyb.pe_iyb_1(keep=variable) out=pe_iyb_1 nodupkey; by variable;run;
proc sort data=yppdpb.pe_ypp_1(keep=variable) out=pe_ypp_1 nodupkey; by variable;run;
proc sort data=yppdpb.pe_dpbema_1(keep=variable) out=pe_dpbema_1 nodupkey; by variable;run;
proc sort data=yppdpb.pe_yppb_1(keep=variable) out=pe_yppb_1 nodupkey; by variable;run;
proc sort data=yppdpb.pe_dpbemab_1(keep=variable) out=pe_dpbemab_1 nodupkey; by variable;run;

data variable_models;
	merge pe_yp_1(in=a) pe_iyb_1(in=b) pe_ypp_1(in=c) pe_dpbema_1(in=d) pe_yppb_1(in=e) pe_dpbemab_1(in=f);
	by variable;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	if a then YP=1;
	if b then iyb=1;   
	if c then ypp=1;    
	if d then dpbema=1;   
	if e then yppb=1;  
	if f then dpbemab=1; 
run;

data variable_exact;
	length variable $ 20;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='refmth';
	YP=1;
	iyb=1;   
	ypp=1;    
	dpbema=1;  
	yppb=1;  
	dpbemab=1; 
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='hqual';
	YP=1;
	iyb=1;   
	ypp=1;    
	dpbema=1;  
	yppb=1;  
	dpbemab=1; 
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='age_det';
	YP=1;
	iyb=1;   
	ypp=1;    
	dpbema=1;  
	yppb=1;  
	dpbemab=1; 
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='female';
	ypp=1;    
	dpbema=1;  
	yppb=1;  
	dpbemab=1; 
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='ben_last6';
	YP=1;
	iyb=1;   
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='sch_prior6';
	YP=1;
	iyb=1;   
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='tert_prior6';
	YP=1;
	iyb=1;   
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='sch_first12b';
	ypp=1;    
	dpbema=1;  
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='tert_first12b';
	ypp=1;    
	dpbema=1;  
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='ben_first12b';
	ypp=1;    
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='ben_first12';
	dpbema=1;    
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='sch_last6';
	yppb=1;    
	dpbemab=1;  
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='tert_last6';
	yppb=1;    
	dpbemab=1;  
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='ben_prior6b';
	yppb=1;    
	output;
	yp=0; iyb=0; ypp=0; dpbema=0; yppb=0; dpbemab=0; 
	variable='ben_prior6';
	dpbemab=1;    
	output;
run;

proc sort data=variable_exact;
	by variable;
run;

ods html body=
"\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output\Matching variables all.xls";
proc print data=variable_models;
title 'Variables used in matching for all models';
run;

proc print data=variable_exact;
title 'Variables matched exactly for all models';
run;
ods html close;

** Bring together all participants, historical comparison groups, and matched comparison groups;
data all_matches;
length part_type $ 7;
set byp.allreps_1     (in=a keep=snz_uid refmth _snz_uid _refmth treat weight)
	biyb.allreps_1     (in=b keep=snz_uid refmth _snz_uid _refmth treat weight)
	bypp.allreps_1    (in=c keep=snz_uid refmth _snz_uid _refmth treat weight)
	bdpbema.allreps_1    (in=d keep=snz_uid refmth _snz_uid _refmth treat weight)
	byppb.allreps_1    (in=e keep=snz_uid refmth _snz_uid _refmth treat weight)
	bdpbemab.allreps_1    (in=f keep=snz_uid refmth _snz_uid _refmth treat weight);
	if a then part_type='YP ';
	else if b then part_type='IYB';
	else if c then part_type='YPP';
	else if d then part_type='DPBEMA';
	else if e then part_type='YPPb';
	else if f then part_type='DPBEMAb';
	*where weight ne .;
run;

proc sort data=all_matches;
	by snz_uid refmth part_type;
run;

** Add on indicators of previous time on benefit and whether participant has a partner on benefit - sub-groups for separate impact estimation;
data all_matches;
	merge all_matches(in=a) ypypp.model_final_restricted(in=b keep=snz_uid refmth partner prev_ben);
	by snz_uid refmth;
	if a;
run;

** create subset of those with a benefit history;
data prevben(keep=snz_uid refmth part_type);
	set all_matches;
	where prev_ben not in ('','NONE');
run;

/*data noprevben(keep=snz_uid refmth part_type);
	set all_matches;
	where prev_ben='NONE';
run;*/

** create subset of those with a partner on benefit;
data partner(keep=snz_uid refmth part_type);
	set all_matches;
	where partner=1;
run;

/*data nopartner(keep=snz_uid refmth part_type);
	set all_matches;
	where partner=0;
run;*/

** Create datasets of both participant/historial comparison and matched comparisons for each subset of interest;
**	Matched comparisons aren't identified as belonging to the subset but instead are identified as having been matched to someone
**  in a subset population;
proc sql;
	create table allmat_prevben as select a.snz_uid, a.refmth, a.part_type, a._snz_uid
	from all_matches a inner join prevben b
	on ((a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid)) and a.part_type=b.part_type
	order by snz_uid, refmth, part_type, _snz_uid;
quit;

/*proc sql;
	create table allmat_noprevben as select a.snz_uid, a.refmth, a.part_type, a._snz_uid
	from all_matches a inner join noprevben b
	on ((a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid)) and a.part_type=b.part_type
	order by snz_uid, refmth, part_type, _snz_uid;
quit;*/

proc sql;
	create table allmat_partner as select a.snz_uid, a.refmth, a.part_type, a._snz_uid
	from all_matches a inner join partner b
	on ((a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid)) and a.part_type=b.part_type
	order by snz_uid, refmth, part_type, _snz_uid;
quit;

/*proc sql;
	create table allmat_nopartner as select a.snz_uid, a.refmth, a.part_type, a._snz_uid
	from all_matches a inner join nopartner b
	on ((a.snz_uid=b.snz_uid and a.refmth=b.refmth) or (a._snz_uid=b.snz_uid)) and a.part_type=b.part_type
	order by snz_uid, refmth, part_type, _snz_uid;
quit;*/

** Add on subset tags for previous benefit status and partner status to full dataset of participants and controls;
data all_matches2;
	merge all_matches(in=z) allmat_prevben(in=a) /*allmat_noprevben(in=b)*/ allmat_partner(in=c) /*allmat_nopartner(in=d)*/;
	by snz_uid refmth part_type _snz_uid;
	if z;
	if a then prevben=1;
	else prevben=0;
	if c then partner=1;
	else partner=0;
run;

proc freq data=all_matches2;
	tables prevben partner/nocol norow nopercent;
run;

** Now add on outcomes variables of interest from the extended data of outcomes to 2015;
** Also calculate new outcomes of interest - time in employment and off benefit, time in tertiary study without school,
**	and being in study but not on benefit;
data ypypp.depvars_temp_all;
merge all_matches2(in=a) 
      ypypp.depvars_ext;
	by snz_uid refmth;
	if a;
	all=1;

	if emp_post6=. or ben_post6=. then emp_noben_post6=.;
	else if emp_post6=1 and ben_post6=0 then emp_noben_post6=1;
	else emp_noben_post6=0;
	if emp_post12=. or ben_post12=. then emp_noben_post12=.;
	else if emp_post12=1 and ben_post12=0 then emp_noben_post12=1;
	else emp_noben_post12=0;
	if emp_post18=. or ben_post18=. then emp_noben_post18=.;
	else if emp_post18=1 and ben_post18=0 then emp_noben_post18=1;
	else emp_noben_post18=0;
	if emp_post24=. or ben_post24=. then emp_noben_post24=.;
	else if emp_post24=1 and ben_post24=0 then emp_noben_post24=1;
	else emp_noben_post24=0;
	if emp_post30=. or ben_post30=. then emp_noben_post30=.;
	else if emp_post30=1 and ben_post30=0 then emp_noben_post30=1;
	else emp_noben_post30=0;

	if tert_post6=. or sch_post6=. then tert_nosch_post6=.;
	else if tert_post6=1 and sch_post6=0 then tert_nosch_post6=1;
	else tert_nosch_post6=0;
	if tert_post12=. or sch_post12=. then tert_nosch_post12=.;
	else if tert_post12=1 and sch_post12=0 then tert_nosch_post12=1;
	else tert_nosch_post12=0;
	if tert_post18=. or sch_post18=. then tert_nosch_post18=.;
	else if tert_post18=1 and sch_post18=0 then tert_nosch_post18=1;
	else tert_nosch_post18=0;
	if tert_post24=. or sch_post24=. then tert_nosch_post24=.;
	else if tert_post24=1 and sch_post24=0 then tert_nosch_post24=1;
	else tert_nosch_post24=0;
	if tert_post30=. or sch_post30=. then tert_nosch_post30=.;
	else if tert_post30=1 and sch_post30=0 then tert_nosch_post30=1;
	else tert_nosch_post30=0;

	if study_post6=. or ben_post6=. then study_noben_post6=.;
	else if study_post6=1 and ben_post6=0 then study_noben_post6=1;
	else study_noben_post6=0;
	if study_post12=. or ben_post12=. then study_noben_post12=.;
	else if study_post12=1 and ben_post12=0 then study_noben_post12=1;
	else study_noben_post12=0;
	if study_post18=. or ben_post18=. then study_noben_post18=.;
	else if study_post18=1 and ben_post18=0 then study_noben_post18=1;
	else study_noben_post18=0;
	if study_post24=. or ben_post24=. then study_noben_post24=.;
	else if study_post24=1 and ben_post24=0 then study_noben_post24=1;
	else study_noben_post24=0;
	if study_post30=. or ben_post30=. then study_noben_post30=.;
	else if study_post30=1 and ben_post30=0 then study_noben_post30=1;
	else study_noben_post30=0;

	** Assign each participant, historical comparison and matched comparison group to a different group code gp;
	if part_type='YP' then do;
		if treat=1 then gp=1;
		else if treat=0 then gp=2;
	end;
	if part_type='IYB' then do;
		if treat=1 then gp=3;
		else if treat=0 then gp=4;
	end;
	if part_type='YPP' then do;
		if treat=1 then gp=5;
		else if treat=0 then gp=6;
	end;
	if part_type='DPBEMA' then do;
		if treat=1 then gp=7;
		else if treat=0 then gp=8;
	end;
	if part_type='YPPb' then do;
		if treat=1 then gp=9;
		else if treat=0 then gp=10;
	end;
	if part_type='DPBEMAb' then do;
		if treat=1 then gp=11;
		else if treat=0 then gp=12;
	end;
run;

** Define the list of outcomes were are interested in;
%let vlist= i1hqual_y0 i1hqual_y1 i1hqual_y2 i1hqual_y3  
		   	i2hqual_y0 i2hqual_y1 i2hqual_y2 i2hqual_y3 
           	i3hqual_y0 i3hqual_y1 i3hqual_y2 i3hqual_y3     
			i1hncea_y0 i1hncea_y1 i1hncea_y2 i1hncea_y3 
			i2hncea_y0 i2hncea_y1 i2hncea_y2 i2hncea_y3 
			i3hncea_y0 i3hncea_y1 i3hncea_y2 i3hncea_y3 
			i1hnonncea_y0 i1hnonncea_y1 i1hnonncea_y2 i1hnonncea_y3
			i2hnonncea_y0 i2hnonncea_y1 i2hnonncea_y2 i2hnonncea_y3
			i3hnonncea_y0 i3hnonncea_y1 i3hnonncea_y2 i3hnonncea_y3
	        i1hterqual_y0 i1hterqual_y1 i1hterqual_y2 i1hterqual_y3
	        i2hterqual_y0 i2hterqual_y1 i2hterqual_y2 i2hterqual_y3
	        i3hterqual_y0 i3hterqual_y1 i3hterqual_y2 i3hterqual_y3
         	study_post6 study_post12 study_post18  study_post24  study_post30
         	study_noben_post6 study_noben_post12 study_noben_post18  study_noben_post24  study_noben_post30
           	sch_post6 sch_post12 sch_post18 sch_post24 sch_post30
           	tert_post6 tert_post12 tert_post18 tert_post24 tert_post30
           	tert_nosch_post6 tert_nosch_post12 tert_nosch_post18 tert_nosch_post24 tert_nosch_post30
           	neet_post6 neet_post12 neet_post18 neet_post24 neet_post30
			ben_post6 ben_post12 ben_post18 ben_post24 ben_post30
           	emp_post6 emp_post12 emp_post18 emp_post24 emp_post30
			emp_noben_post6 emp_noben_post12 emp_noben_post18 emp_noben_post24 emp_noben_post30
           	comm_post6 comm_post12 comm_post18 comm_post24 comm_in_followup
           	cust_post6 cust_post12 cust_post18 cust_post24 cust_in_followup 
           	sa_post6 sa_post12 sa_post18 sa_post24 sa_post30
           	os_post6 os_post12 os_post18 os_post24 os_post30;

* Count how many variables we are looking at estimate impacts for;
%let numvars=%sysfunc(countw(&vlist.));

* Set the names of the various variables we want to output for each outcome of interest;
%let classvar = all;
** Number of people in the treatment group;
%let ntreat   = n1-n&numvars.;
** Mean outcomes in the treatment group;
%let vtreat   = t1-t&numvars.;
** Mean outcomes in the matched comparison group;
%let vcontrol = c1-c&numvars.;
** Sum of outcomes in the treatment group;
%let streat   = st1-st&numvars.;
** Sum of outcomes in the matched comparison group;
%let scontrol = sc1-sc&numvars.;
** Number of people in the historical comparison group;
%let ntreatb   = nb1-nb&numvars.;
** Mean outcomes in the historical comparison group;
%let vtreatb  = tb1-tb&numvars.;
** Mean outcomes in the historical matched comparison group;
%let vcontrolb= cb1-cb&numvars.;
** Sum of outcomes in the historical comparison group;
%let streatb   = stb1-stb&numvars.;
** Sum of outcomes in the historical matched comparison group;
%let scontrolb = scb1-scb&numvars.;
** Estimated impact of the programme = (vtreat-vcontrol)- (vtreatb-vcontrolb);
%let vdiff    = diff1-diff&numvars.;
** Standard errors on impact estimate;
%let vse      = sd1-sd&numvars.;
** Lower confidence limit on impact estimate;
%let diff_lcl = dl1-dl&numvars.;
** Upper confidence limit on impact estimate;
%let diff_ucl = du1-du&numvars.;
%let diff_lcld = dl1d-dl&numvars.d;
%let diff_ucld = du1d-du&numvars.d;
%let format   = ;

%macro impacts(data, part_type, part_type2, vlist, classvar, ntreat, vcontrol, vtreat, vcontrolb, vtreatb, vdiff); 
proc summary data=&data nway;
var   &vlist;
class &classvar;
where treat=1 and part_type="&part_type." and weight ne .;
weight weight;
format &classvar &format.;
output out=samples(drop=_type_ ) n=&ntreat. mean=&vtreat. sum=&streat.;
run;

proc summary data=&data nway;
var   &vlist;
class &classvar;
where treat=0 and part_type="&part_type." and weight ne .;
weight weight;
format &classvar &format.;
output out=results0(drop=_type_ ) mean=&vcontrol. sum=&scontrol.;
run;

proc summary data=&data nway;
var   &vlist;
class &classvar;
where treat=1 and part_type="&part_type2." and weight ne .;
weight weight;
format &classvar &format.;
output out=samplesb(drop=_type_ ) n=&ntreatb.  mean=&vtreatb. sum=&streatb.;
run;

proc summary data=&data nway;
var   &vlist;
class &classvar;
where treat=0 and part_type="&part_type2." and weight ne .;
weight weight;
format &classvar &format.;
output out=results0b(drop=_type_ ) mean=&vcontrolb. sum=&scontrolb.;
run;

data diff;
merge samples(rename=_FREQ_=ntreat)   results0(rename=_FREQ_=ncontrol)   /*results1(rename=_FREQ_=ntreat)*/
	  samplesb(rename=_FREQ_=ntreatb) results0b(rename=_FREQ_=ncontrolb) /*results1b(rename=_FREQ_=ntreatb)*/;
by &classvar;
array treat (*) &vtreat;
array control (*) &vcontrol;
array treatb (*) &vtreatb;
array controlb (*) &vcontrolb;
array diff (*) &vdiff;
do i=1 to &numvars.;
   treat(i)= treat(i)*100;
   control(i)=control(i)*100;
   treatb(i)= treatb(i)*100;
   controlb(i)=controlb(i)*100;
   diff(i)=(treat(i)-treatb(i))-(control(i)-controlb(i));
  end;
run;

data results_main_&part_type.&classvar(keep=i &classvar varlabel ntreat treat_mean control_mean treat_sum control_sum ntreatb treat_meanb control_meanb treat_sumb control_sumb diff ); 
set diff;
array sample (*) &ntreat;
array treat (*) &vtreat;
array control (*) &vcontrol;
array sumtreat (*) &streat;
array sumcontrol (*) &scontrol;
array sampleb (*) &ntreatb;
array treatb (*) &vtreatb;
array controlb (*) &vcontrolb;
array sumtreatb (*) &streatb;
array sumcontrolb (*) &scontrolb;
array differ(*) &vdiff;
do i= 1 to dim(treat); 
 ntreat=sample(i);
 treat_mean=treat[i];
 control_mean=control[i];
 treat_sum=sumtreat[i];
 control_sum=sumcontrol[i];
 ntreatb=sampleb(i);
 treat_meanb=treatb[i];
 control_meanb=controlb[i];
 treat_sumb=sumtreatb[i];
 control_sumb=sumcontrolb[i];
 diff=differ[i];
 varlabel=scan("&vlist.", i);
 output;
 end;
run ;

proc print; 
var &classvar varlabel ntreat treat_mean control_mean treat_sum control_sum ntreatb treat_meanb control_meanb treat_sumb control_sumb diff ;
title "Main estimates &part_type. &classvar.";
format treat_mean control_mean treat_meanb control_meanb diff 6.1;
run;
%mend impacts;

%impacts(ypypp.depvars_temp_all, YP   , IYB    ,&&vlist, all,  &&ntreat, &&vcontrol, &&vtreat, &&vcontrolb, &&vtreatb, &&vdiff);
%impacts(ypypp.depvars_temp_all, YPP  , DPBEMA ,&&vlist, all,  &&ntreat, &&vcontrol, &&vtreat, &&vcontrolb, &&vtreatb, &&vdiff);
%impacts(ypypp.depvars_temp_all, YPPb , DPBEMAb,&&vlist, all,  &&ntreat, &&vcontrol, &&vtreat, &&vcontrolb, &&vtreatb, &&vdiff);
%impacts(ypypp.depvars_temp_all, YP   , IYB    ,&&vlist, prevben,  &&ntreat, &&vcontrol, &&vtreat, &&vcontrolb, &&vtreatb, &&vdiff);
%impacts(ypypp.depvars_temp_all, YPP  , DPBEMA ,&&vlist, prevben,  &&ntreat, &&vcontrol, &&vtreat, &&vcontrolb, &&vtreatb, &&vdiff);
%impacts(ypypp.depvars_temp_all, YPPb , DPBEMAb,&&vlist, prevben,  &&ntreat, &&vcontrol, &&vtreat, &&vcontrolb, &&vtreatb, &&vdiff);
%impacts(ypypp.depvars_temp_all, YP   , IYB    ,&&vlist, partner,  &&ntreat, &&vcontrol, &&vtreat, &&vcontrolb, &&vtreatb, &&vdiff);
%impacts(ypypp.depvars_temp_all, YPP  , DPBEMA ,&&vlist, partner,  &&ntreat, &&vcontrol, &&vtreat, &&vcontrolb, &&vtreatb, &&vdiff);
%impacts(ypypp.depvars_temp_all, YPPb , DPBEMAb,&&vlist, partner,  &&ntreat, &&vcontrol, &&vtreat, &&vcontrolb, &&vtreatb, &&vdiff);

**STANDARD ERRORS FOR MAIN IMPACT ESTIMATES ;
***THE REPLICATE WEIGHTS ARE CREATED IN THE STANDARD ERROR PROG;

** Compile the necessary data;
%macro sedata(part_type,strata);
data long(keep=nsnz_uid n_snz_uid ntreat nrefmth i newweight strata rename=(  
    nsnz_uid=snz_uid n_snz_uid=_snz_uid  ntreat=treat nrefmth=refmth i=rep newweight=weight));
set b&part_type..allreps_&strata;
strata=&strata.;
nsnz_uid=snz_uid;
n_snz_uid=_snz_uid;
ntreat=treat;
nrefmth=refmth; 	
array wgts[300] wgt1-wgt300;
do i= 1 to 300;
   newweight=wgts[i];
   if newweight^=. then output;
   end;
drop snz_uid _snz_uid treat refmth weight;
run;

proc sort data=long;
	by snz_uid refmth; 
run;

data b&part_type..sedata_&strata;
	merge long(in=a)
    		ypypp.depvars_temp_all(where=(part_type="&part_type."));
	by snz_uid refmth; 
	if a;
	if emp_post6=. or ben_post6=. then emp_noben_post6=.;
	else if emp_post6=1 and ben_post6=0 then emp_noben_post6=1;
	else emp_noben_post6=0;
	if emp_post12=. or ben_post12=. then emp_noben_post12=.;
	else if emp_post12=1 and ben_post12=0 then emp_noben_post12=1;
	else emp_noben_post12=0;
	if emp_post18=. or ben_post18=. then emp_noben_post18=.;
	else if emp_post18=1 and ben_post18=0 then emp_noben_post18=1;
	else emp_noben_post18=0;
	if emp_post24=. or ben_post24=. then emp_noben_post24=.;
	else if emp_post24=1 and ben_post24=0 then emp_noben_post24=1;
	else emp_noben_post24=0;
	if emp_post30=. or ben_post30=. then emp_noben_post30=.;
	else if emp_post30=1 and ben_post30=0 then emp_noben_post30=1;
	else emp_noben_post30=0;

	if tert_post6=. or sch_post6=. then tert_nosch_post6=.;
	else if tert_post6=1 and sch_post6=0 then tert_nosch_post6=1;
	else tert_nosch_post6=0;
	if tert_post12=. or sch_post12=. then tert_nosch_post12=.;
	else if tert_post12=1 and sch_post12=0 then tert_nosch_post12=1;
	else tert_nosch_post12=0;
	if tert_post18=. or sch_post18=. then tert_nosch_post18=.;
	else if tert_post18=1 and sch_post18=0 then tert_nosch_post18=1;
	else tert_nosch_post18=0;
	if tert_post24=. or sch_post24=. then tert_nosch_post24=.;
	else if tert_post24=1 and sch_post24=0 then tert_nosch_post24=1;
	else tert_nosch_post24=0;
	if tert_post30=. or sch_post30=. then tert_nosch_post30=.;
	else if tert_post30=1 and sch_post30=0 then tert_nosch_post30=1;
	else tert_nosch_post30=0;
	all=1;
run;

%mend sedata;

%sedata(YP     ,1);
%sedata(IYB    ,1);
%sedata(YPP    ,1);
%sedata(DPBEMA ,1);
%sedata(YPPb   ,1);
%sedata(DPBEMAb,1);
*%sedata(YPold  ,1);
*%sedata(IYBold ,1);

%macro add_detls(part_type,lib,suff);
data &part_type.&suff._treat(drop=treat prev_ben);
	set &lib..allcases_&part_type._1(keep=snz_uid partner prev_ben treat);
	where treat;
	if prev_ben not in ('','NONE') then prevben=1;
	else prevben=0;
run;
proc sort data=&part_type._treat nodupkey;
	by snz_uid;
run;

proc sql;
	create table b&part_type.&suff..&part_type.&suff._sedata_1 as select a.*, b.partner, b.prevben
	from b&part_type.&suff..sedata_1 a inner join &part_type.&suff._treat b
	on a.snz_uid=b.snz_uid or a._snz_uid=b.snz_uid;
quit;
/*proc sql;
	create table b&part_type..&part_type._sedata_1b as select a.*, b.partner, b.prevben
	from b&part_type..sedata_1 a inner join &part_type._treat b
	on a._snz_uid=b.snz_uid;
quit;*/
%mend add_detls;

%add_detls(YP,ypiyb,);
%add_detls(IYB,ypiyb,);
%add_detls(YPP,yppdpb,);
%add_detls(DPBEMA,yppdpb);
%add_detls(YPP,yppdpb,b);
%add_detls(DPBEMA,yppdpb,b);

data ypiyb.combinedyp;
length part_type $ 7;
set byp.yp_sedata_1(in=a) biyb.iyb_sedata_1(in=b);
if a then part_type='YP';
else if b then part_type='IYB';
run;

/*data combinedypold;
length part_type $ 7;
set ypold_sedata_1a(in=a) ypold_sedata_1b(in=a) iybold_sedata_1a(in=b) iybold_sedata_1b(in=b);
if a then part_type='YP';
else if b then part_type='IYB';
run;*/

data yppdpb.combinedypp;
length part_type $ 7;
set bypp.ypp_sedata_1(in=a) bdpbema.dpbema_sedata_1(in=b);
if a then part_type='YPP';
else if b then part_type='DPBEMA';
run;

data yppdpb.combinedyppb;
length part_type $ 7;
set byppb.yppb_sedata_1(in=a) bdpbemab.dpbemab_sedata_1(in=b);
if a then part_type='YPPb';
else if b then part_type='DPBEMAb';
run;


***CALCULATE STANDARD ERRORS;
**CLASSVAR IS THE SUBGROUP VARIABLE(S). IF SET TO 'ALL' THEN CODE GENERATES AGGREGATE RESULTS;
%macro fastboot(data, part_type, part_typeb, vlist, classvar, format, vtreat, vcontrol, vtreatb, vcontrolb, vdiff, vse);
proc sort data=&data;
by rep;
run;

proc means data=&data noprint nway;
by rep;
weight weight;
class &classvar. part_type treat;
var &vlist;
output out=outall mean=;
run;

* split the participants from non-participants and then merge back together into one wide record
for each category of the SPLIT variable;

data treatgp(drop= &vlist);
set outall;
if treat=1 and part_type="&part_type.";
array oldname (*) &vlist;
array newname (*) &vtreat;
do i=1 to dim(oldname);
  newname(i)=oldname(i);
  end;
run;

data controlgp(drop=&vlist);
set outall;
if treat=0 and part_type="&part_type.";
array oldname (*) &vlist;
array newname (*) &vcontrol;
do i=1 to dim(oldname);
  newname(i)=oldname(i);
  end;
run;

data treatgpb(drop= &vlist);
set outall;
if treat=1 and part_type="&part_typeb.";
array oldname (*) &vlist;
array newname (*) &vtreatb;
do i=1 to dim(oldname);
  newname(i)=oldname(i);
  end;
run;

data controlgpb(drop=&vlist);
set outall;
if treat=0 and part_type="&part_typeb.";
array oldname (*) &vlist;
array newname (*) &vcontrolb;
do i=1 to dim(oldname);
  newname(i)=oldname(i);
  end;
run;

data diff;
merge treatgp(drop=treat _type_ _freq_ part_type) controlgp(drop=treat _type_ _freq_ part_type)
	  treatgpb(drop=treat _type_ _freq_ part_type) controlgpb(drop=treat _type_ _freq_ part_type);
by rep &classvar;
array treat (*) &vtreat;
array control (*) &vcontrol;
array treatb (*) &vtreatb;
array controlb (*) &vcontrolb;
array diff (*) &vdiff;
do i=1 to dim(treat);
   diff(i)=(treat(i)-treatb(i))-(control(i)-controlb(i));
   end;
run;

proc means data=diff noprint nway ;
var &vdiff &vtreat &vcontrol &vtreatb &vcontrolb;
class &classvar;
output out=se(keep=&classvar &vdiff &vtreat &vcontrol &vtreatb &vcontrolb &vse)
   mean=&vdiff &vtreat &vcontrol &vtreatb &vcontrolb  
   stddev(&vdiff) = &vse;
run;

proc univariate data=diff noprint;
	class &classvar.;
	var &vdiff.;
	output out=lcl pctlpts=2.5 pctlpre=&diff_lcl.
			pctlname=d;
run;
proc univariate data=diff noprint;
	class &classvar.;
	var &vdiff.;
	output out=ucl pctlpts=97.5 pctlpre=&diff_ucl.
			pctlname=d;
run;

data results_se_&part_type.&classvar(keep=i &classvar varlabel treat_mean control_mean treat_meanb control_meanb diff std_error u_ci l_ci t sig u_ci2 l_ci2); 
merge se lcl ucl;
by &classvar.;
array treat (*) &vtreat;
array control (*) &vcontrol;
array treatb (*) &vtreatb;
array controlb (*) &vcontrolb;
array difference (*) &vdiff;
array lcl (*) dl:;
array ucl (*) du:;
array se (*) &vse;
do i= 1 to dim(treat);
 std_error=se[i];
 treat_mean=treat[i];
 control_mean=control[i];
 treat_meanb=treatb[i];
 control_meanb=controlb[i];
 U_CI = difference[i] + 1.96*se[i] ;
 L_CI = difference[i] - 1.96*se[i] ;
 u_ci2=ucl[i];
 l_ci2=lcl[i];
 t = difference[i]/se[i]  ;
 if t > 1.96 or t < -1.96 then sig = '*' ; else sig =' ' ;
 diff=difference[i];
 varlabel=scan("&vlist.", i);
 output;
 end;
run;

proc print; 
var &classvar varlabel treat_mean control_mean treat_meanb control_meanb diff std_error u_ci l_ci t sig ;
title "Standard Errors Benefit= &part_type. Group= &classvar.";
run;

%mend fastboot;

***Calculate  SE  - full sample and by strata;
%fastboot(ypiyb.combinedyp,  YP,   IYB,      &&vlist, all,      ,&&vtreat, &&vcontrol, &&vtreatb, &&vcontrolb, &&vdiff, &&vse);
%fastboot(ypiyb.combinedyp,  YP,   IYB,      &&vlist, prevben,  ,&&vtreat, &&vcontrol, &&vtreatb, &&vcontrolb, &&vdiff, &&vse);
%fastboot(ypiyb.combinedyp,  YP,   IYB,      &&vlist, partner,  ,&&vtreat, &&vcontrol, &&vtreatb, &&vcontrolb, &&vdiff, &&vse);
%fastboot(yppdpb.combinedypp, YPP,  DPBEMA,   &&vlist, all,      ,&&vtreat, &&vcontrol, &&vtreatb, &&vcontrolb, &&vdiff, &&vse);
%fastboot(yppdpb.combinedypp, YPP,  DPBEMA,   &&vlist, prevben,  ,&&vtreat, &&vcontrol, &&vtreatb, &&vcontrolb, &&vdiff, &&vse);
%fastboot(yppdpb.combinedypp, YPP,  DPBEMA,   &&vlist, partner,  ,&&vtreat, &&vcontrol, &&vtreatb, &&vcontrolb, &&vdiff, &&vse);
%fastboot(yppdpb.combinedyppb,YPPb, DPBEMAb,  &&vlist, all,      ,&&vtreat, &&vcontrol, &&vtreatb, &&vcontrolb, &&vdiff, &&vse);
%fastboot(yppdpb.combinedyppb,YPPb, DPBEMAb,  &&vlist, prevben,  ,&&vtreat, &&vcontrol, &&vtreatb, &&vcontrolb, &&vdiff, &&vse);
%fastboot(yppdpb.combinedyppb, YPPb, DPBEMAb,  &&vlist, partner,  ,&&vtreat, &&vcontrol, &&vtreatb, &&vcontrolb, &&vdiff, &&vse);

*%fastboot(combinedypold,  YP,   IYB,      &&vlist, all,      ,&&vtreat, &&vcontrol, &&vtreatb, &&vcontrolb, &&vdiff, &&vse);


***JOIN THE IMPACT AND STANDARD ERROR DATASETS;
***Only keep the standard error variable from the SE dataset and recalculate the measures of significance
   using the base estimate (and not the mean of the replicate estimates);
****Print the FINAL OUTPUT TABLES;

%macro results(classvar,part_type);
data ypypp.results_&part_type.&classvar;
merge results_main_&part_type.&classvar(keep=&classvar i varlabel ntreat treat_sum control_sum ntreatb treat_sumb control_sumb diff)       
      results_se_&part_type.&classvar(keep=&classvar i std_error treat_mean control_mean treat_meanb control_meanb diff u_ci2 l_ci2
      rename=(treat_mean=treat_mean_SE control_mean=control_mean_SE  treat_meanb=treat_meanb_SE control_meanb=control_meanb_SE  diff=diff2));
	by &classvar i;
   treat_SE=treat_mean_SE*100;
   control_SE=control_mean_SE*100;
   treatb_SE=treat_meanb_SE*100;
   controlb_SE=control_meanb_SE*100;
   impact_SE=diff2*100;
   SE=std_error*100;
   ** Confidence interval based on estimated standard errors;
	U_CI = diff + 1.96*SE ;
	L_CI = diff - 1.96*SE ;
   ** Alternative confidence interval based on 2.5th and 97.5th percentile of replicate estimated impacts;
	u_ci2=u_ci2*100;
	l_ci2=l_ci2*100;
	t = diff/SE ;
	if t > 1.96 or t < -1.96 then sig = '*' ; else sig =' ' ;
	if (u_ci2<0 or l_ci2>0) then sig2 = '*' ; else sig2 =' ' ;
run;

proc print data=ypypp.results_&part_type.&classvar;
var &classvar varlabel ntreat treat_sum control_sum ntreatb treat_sumb control_sumb SE t l_ci u_ci sig l_ci2 u_ci2 sig2 treat_SE control_SE treatb_SE controlb_SE impact_SE;
format control_sum control_sumb 9.;
title "Classvar = &classvar., part_type=&part_type.";
run;

%mend results;

ods html body= 
"\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output\Main_impacts_SE_newcode ALL.xls";

%results(all,YP);
%results(partner,YP);
%results(prevben,YP);
%results(all,YPP);
%results(partner,YPP);
%results(prevben,YPP);
%results(all,YPPb);
%results(partner,YPPb);
%results(prevben,YPPb);
ods html close;

** Look at significant results;
proc print data=ypiyb.PE_rerun_YP1;
	where ProbChiSq<=0.05;
	title "YP1";
run;
proc print data=ypiyb.PE_rerun_iyb1;
	where ProbChiSq<=0.05;
	title "IYB1";
run;
proc print data=yppdpb.PE_rerun_ypp1;
	where ProbChiSq<=0.05;
	title "YPP1";
run;
proc print data=yppdpb.PE_rerun_dpbema1;
	where ProbChiSq<=0.05;
	title "DPB1";
run;
proc print data=yppdpb.PE_rerun_yppb1;
	where ProbChiSq<=0.05;
	title "YPPb1";
run;
proc print data=yppdpb.PE_rerun_dpbemab1;
	where ProbChiSq<=0.05;
	title "DPBb1";
run;
