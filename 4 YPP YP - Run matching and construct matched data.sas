/* 4 YPP YP - Run matching and construct matched data */

******************************************************;
**  Construct final datset for matching YP and YPP participant populations;
**  Run matching code, check balance and run descriptive analysis;
** 	We run matching for YPP based on both a preferred specifiction with no matching in the months before participation
**  and an alternative specification with matching right up to participation (Specification B)
******************************************************;

** Key data inputs:
*		- yst.yst_spells			(YTS and YS:NEET participation spell information)
*		- ys5.model_final_addrecs_revised (model data from the YS:NEET study)
*		- ys5.popn_restricted		(population of potential controls)
*		- ys.mth_os					(monthly time overseas records)
*		- ys5.last_sch_rec			(last school records to create an exclusion dataset for those with imputed school enddate)
*		- ys5.kids_nonnqf			(non-nqf school enrolment records for exclusion)
* 		- ys5.models				(source of strata variable for matching)
*		- ypypp.pre_ys5_monthly		(pre YS participation data)
*		- ypypp.model_final			(final YP YPP model data)
*		- ypiyb.YP_newparts			(YP participant data)
*		- yppdpb.YPP_newparts		(YPP participant data)

** Key data outputs:
* 		- ypiyb.allcases_YP_1		(Dataset of participants and potential matched controls YP)
* 		- yppdpb.allcases_YPP_1		(Dataset of participants and potential matched controls YPP)
*		- ypiyb.PE_YP_1				(Dataset of parameter estimates from YP logistic regression)
* 		- ypiyb.OR_YP_1				(Dataset of Odds Ratios from YP logistic regression)
* 		- ypiyb.rep0_YP1			(YP study and matched comparison data)
*		- ypiyb.matchrate_YP		(Match rate data for YP)
*		- yppdpb.PE_YPP_1			(Dataset of parameter estimates from YPP logistic regression)
* 		- yppdpb.OR_YPP_1			(Dataset of Odds Ratios from YPP logistic regression)
* 		- yppdpb.rep0_YPP1			(YP study and matched comparison data)
*		- yppdpb.matchrate_YPP		(Match rate data for YPP)
*		- yppdpb.PE_YPPb_1			(Dataset of parameter estimates from YPP model spec B logistic regression)
* 		- yppdpb.OR_YPPb_1			(Dataset of Odds Ratios from YPP model spec B logistic regression)
* 		- yppdpb.rep0_YPPb1			(YPP model spec B study and matched comparison data)
*		- yppdpb.matchrate_YPPb		(Match rate data for YPP model spec B)
* 		- ypiyb.matched_modelrerun_YP1 (Rerun model on YP participants and matched comparison to check balance)
* 		- yppdpb.matched_modelrerun_YPP1 (Rerun model on YPP participants and matched comparison to check balance)
* 		- yppdpb.matched_modelrerun_YPPb1 (Rerun model on YPP participants and matched comparison to check balance for specification B)
*		- ypiyb.PE_rerun_YP_1		(Dataset of parameter estimates from YP logistic regression model rerun)
* 		- ypiyb.OR_rerun_YPP_1		(Dataset of Odds Ratios from YP logistic regression model rerun)
*		- yppdpb.PE_rerun_YPP_1		(Dataset of parameter estimates from YPP logistic regression model rerun)
* 		- yppdpb.OR_rerun_YPP_1		(Dataset of Odds Ratios from YPP logistic regression model rerun)
*		- yppdpb.PE_rerun_YPPb_1	(Dataset of parameter estimates from YPP logistic regression model rerun specification B)
* 		- yppdpb.OR_rerun_YPPb_1	(Dataset of Odds Ratios from YPP logistic regression model rerun specification B)
*		- ypiyb.allmodel_YP			(Dataset of descriptive characteristics for YP participants and matched control)
*		- ypiyb.allmodel_YPP		(Dataset of descriptive characteristics for YPP participants and matched control)
*		- ypiyb.allmodel_YPPb		(Dataset of descriptive characteristics for YPP participants and matched control - specification B)
;

** Firstly create a dataset of YTS and YSNEET participation;
data YSneet(keep=snz_uid startdate enddate refmth);
  set yst.yst_spells;
  startdate  =input(compress(yst_spl_participation_start_date,"-"),yymmdd8.) ;
  enddate    =input(compress(yst_spl_participation_end_date,"-"),yymmdd8.) ;
  where yst_spl_programme_name_text in ('NOT IN EDUCATION, EMPLOYMENT OR TRAINING','YOUTH TRANSITION SERVICE');
  startyr=year(startdate);
  format startdate enddate date9.;
  refmth=(12*(year(startdate) - 1999) + (month(startdate) -3));
run;

**Get one record per person, first ever enrolment;
proc sort data=YSneet;
	by snz_uid refmth startdate;
run;

data YSneet_first(drop=refmth);
	set YSneet;
	by snz_uid refmth startdate;
	if first.snz_uid ;
	prog_refmth=refmth;
run;

**Drop records of people who were already participants in  YS-NEET;
**But keep their records for reference months before the first YS-NEET enrolment;
** First create indicators of prior participation in YS-NEET or YTS;
data yst_parts;
	set yst.yst_spells;
	ys_startdate=input(compress(yst_spl_participation_start_date,"-"),yymmdd10.);
	ys_enddate=input(compress(yst_spl_participation_end_date,"-"),yymmdd10.);
	format ys_startdate ys_enddate date9.;
run;

** Add on YST and YSNEET participation data onto the population data from the YSNEET study for the matched control;
proc sql;
	create table prior_parts as 
	select a.snz_uid, a.refmth, a.ysstartdate as startdate, b.ys_startdate, b.ys_enddate, b.yst_spl_programme_code as prog_code
	from ys5.model_final_addrecs_revised a inner join yst_parts b
	on a.snz_uid=b.snz_uid
	order by snz_uid, refmth, ys_startdate;
quit;

** Create indicators of previous participation;
data prior_parts_con(keep=snz_uid refmth prior_yts prior_neet);
	set prior_parts;
	retain prior_neet prior_yts;
	by snz_uid refmth;
	if first.refmth then do;
		prior_neet=0;
		prior_yts=0;
	end;
	** Only include those participants who had a participation that started more than 3 months before the benefit spell start
		and lasted longer than 3 months;
	prior_mth=intck('month',ys_startdate,startdate);
	part_mths=intck('month',ys_startdate,ys_enddate);
	if prior_mth>3 and part_mths>3 then do;
		if prog_code='ART_NEET' then prior_neet=1;
		if prog_code='YTS' then prior_yts=1;
	end;
	if (prior_neet or prior_yts) and last.refmth then output;
run;

** Create an exclusion dataset for those with imputed school enddate for potential matched control;
data drop(keep=snz_uid refmth);
	set ys5.last_sch_rec;
	if sch_enddate_imputed=1 and (enddate-startdate)>(365*5);
run;

** Create an exclusion dataset for those with > 6 months overseas for potential matched control;
%let n=79;
data not_os(keep=snz_uid refmth)
     overseas_6mthsplus(keep=snz_uid refmth);
    merge ys5.popn_restricted(in=a keep=snz_uid refmth) ys.mth_os(keep=snz_uid os_da_58-os_da_195); 
	by snz_uid;
	if a;
	windowstart_leed=refmth-48;
	array os(*) os_da_58-os_da_195;
	array nos(*) nos1-nos&n;
    array nosb(*) nosb1-nosb&n;
    avail_mths=&n;
    do i=1 to 12;
	  if refmth=(i+177) then avail_mths=(&n-i);
      end;
	do i=1 to avail_mths;
		if 1 <= i+(windowstart_leed-57-1) <= 138 then do; 
			nos(i)=os(i+(windowstart_leed-57)-1);
	        if nos(i)>=7 then nos(i)=1;
	        else nos(i)=0;	
		end;
	end;
    do i=1 to avail_mths;
		if 1 <= i+(windowstart_leed-58-1) <= 138 then do; 
			nosb(i)=os(i+(windowstart_leed-58)-1);
			if nosb(i)=. then nosb(i)=0;
		end;
	end;
	if sum(of nosb1-nosb&n)<=180 then output not_os;
	  else output overseas_6mthsplus;
run;

** Exclude overseas and imputed school end dates from potential matched control;
data first;
merge ys5.model_final_revised(in=a) overseas_6mthsplus(in=b) drop(in=c);
by snz_uid refmth;
if a and not b and not c;
run;

** Exclude those who last attended non-NQF school from potential matched control;
data model_final_addrecs_revised(compress=y);
merge first(in=a ) ys5.kids_nonnqf(in=b) ; 
by snz_uid;
if a and not b;
run;

proc sort data=model_final_addrecs_revised;
	by snz_uid refmth;
run;

** Add other fields for potential matched controls from datasets created for YSNEET analysis;
data model_final_control(compress=y);
	merge model_final_addrecs_revised(in=a drop=strata tert_: ben_: sch_: neet_: emp_:) ys5.models(keep=snz_uid refmth strata) ypypp.pre_ys5_monthly;
	by snz_uid refmth;
	if a;
run;

** Create a list of all people who have ever been on YP, YPP or equivalents so these can be excluded from potential matched controls;
proc sort data=ypypp.model_final(keep=snz_uid) out=model_final nodupkey;
	by snz_uid;
run;

** Exclude those who have ever been on a youth benefit from matched control data and restrict to our period of interest;
data control ypypp_excl;
	merge model_final_control(in=a) 
	      model_final(in=c); /*exclude people who have been on YPYPPDPBEMA as controls*/
	by snz_uid;
	if a and not c and 160<=refmth<=177 then output control; 
	else if a and c and 160<=refmth<=177 then output ypypp_excl; 
run;

proc freq data=control;
	tables refmth;
run;

** Add on the indicator of prior YSNEET or YTS participation - we include these in our data;
data control;
	merge control(in=a) prior_parts_con(in=b);
	by snz_uid refmth;
	if a;
	if b then yts_ysneet=1;
	else yts_ysneet=0;
run;

%macro model_data(lib,part_type);
** Construct dataset of participants and potential matched controls - re-classify variables if necessary to remove categories that are too small;
** Exclude July 2017 starts (refmth 160) as they werent included in the YSNEET potential control data;
data &lib..allcases_&part_type._1(drop=participant part_type_part mths_ben_to_ys);
	set &lib..&part_type._newparts(in=a) control(in=b rename=(ysstartdate=startdate age=age_det));	
	by snz_uid;
	if a then treat=1;
	else treat=0;
	part_type="&part_type.";
	if cohort=1993 then cohort=1994;
	age=floor(age_det);
	if nsch_cat > 4 then nsch_cat=4;
	** Combine Nelson, Tasman, Marlborough and West Coast;
	if region in (16,17,18) then region=12;
	if decile=10 then decile=9;
	if schtype="Standard" then schtype="Other";
	if schauth="StateInt" then schauth="Other";
	if prop_os_aschild_cat ='d75%+ ' then prop_os_aschild_cat='c50-<75%';
	strata_old=strata;
	strata=1;
	if age in (16,17,18) and refmth>160;
run;

** Check categories to make sure they are large enough for matching;
proc freq data=&lib..allcases_&part_type._1;
	tables (yts_ysneet refmth cohort age_det female maori pacific asian ethoth
	  nzdep region decile schtype schauth hqual hothqual hterqual 
	  ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nsch_cat time_since_school
	  genskills_only gen_n_occ_skills
	  sch_last6 emp_last6 tert_last6 neet_last6 ben_last6
	  sch_prior6 emp_prior6 tert_prior6 neet_prior6 ben_prior6
	  sch_first12 emp_first12 tert_first12 neet_first12 ben_first12
	  prop_onben_aschild_cat ever_onben_aschild child_bentype
	  prop_os_aschild_cat mother_unqual cg_cust cg_comm 
	  child_not_cat child_yj_referral_cat child_cyf_place child_yj_place child_any_fdgs_abuse all_act
	  sedu_da trua_da susp_da_cat stand_da_cat)*treat/norow nopercent missing;
run;
	
%macro select(strata);
	data controls;
		set &lib..allcases_&part_type._&strata;
		if treat=0;
	run;

	data study;
		set &lib..allcases_&part_type._&strata;
		if treat=1;
	run;

	proc surveyselect data=controls method=srs reps=1
		sampsize=10000 seed=12345 out=random1;
	run;

	data &lib..model_&part_type._&strata(compress=y);
		set study random1;
	run;

%mend select;

%select(1);
%mend model_data;

%model_data(ypiyb,YP);
%model_data(yppdpb,YPP);

**************************************************;
**PART A: PS MODELS AND MATCHING - YPP;
***********************************************;

%macro main(lib,classvars,xvars,strata,part_type,suff);
ods output ParameterEstimates=&lib..PE_&part_type.&suff._&strata
           oddsratios=&lib..OR_&part_type.&suff._&strata;
proc logistic data=&lib..model_&part_type._&strata.;
class &classvars.;
model treat(event='1') = &xvars.
	  / maxiter=200 firth rsquare; roc;
	  score data=&lib..allcases_&part_type._&strata. out=score1;
	  title "Propensity model &part_type. Strata &strata.";
run;
%run_match_&part_type.&suff._1;

data matches_final;
	set matchesa;
	match_run=1;
run;

****CALCULATING THE MATCH RATE; 
proc freq data=matches_final noprint ; 
	tables _snz_uid*_refmth*match_run / out=out ;
run ;

data matchrate_&part_type.&suff.&strata. ;
	merge study_m (in=a )
    out (in=b keep=_snz_uid _refmth count match_run) ;
  	by _snz_uid _refmth;
  	if a and b  then flag='Matched  ' ;
  	if a and not b then flag='Unmatched  ' ;
  	if a and not b then count=0 ;
run;

proc freq data=matchrate_&part_type.&suff.&strata. ;
	tables flag match_run / nocol norow;
	title "Match rate for &part_type. &strata"; 
run;

***SELECTING UP TO 10 BEST MATCHES TO KEEP AND CALCULATING WEIGHTS FOR THE MATCHED CONTROL GROUP;
data matches2 ;  
set matches_final;
 distance=abs(_pscore-pscore);
 rn=rand('uniform')  ;
run ;

proc sort data=matches2 ; 
by _snz_uid _refmth distance rn ; 
run; * take closest matches ;

data matches3 ; 
	set matches2;
 	by _snz_uid _refmth distance rn ;
 	if first._snz_uid then n=0 ;
 	n+1;
	if n<=10 then output ; * keep max of 10 matches ;
run ;

proc freq data=matches3 noprint ;
  tables _snz_uid*_refmth / out=out2 ;
run;

 * Weight per each matched non participant;
data out3 (keep=_snz_uid _refmth weight) ;  
  set out2 ;
  weight=1/count ;
run;

*One record per matched control person;
data comparison_gp(keep=snz_uid _snz_uid refmth _refmth weight pscore startdate);   
  merge matches3 out3;
  by _snz_uid _refmth;
run ;

**One record per matched study grp mbr;
data study_gp (rename=(_snz_uid=snz_uid _refmth=refmth _pscore=pscore count=nbr_matched _startdate=startdate)) ;
   set matchrate_&part_type.&suff.&strata.(keep=_snz_uid _refmth count _pscore _startdate);
   if count=0 then weight=0 ; 
   else weight=1 ;
   if weight>0;
 run ;

 **Combine these study group and control group records;
 **The persons own id is stored in snz_uid but if they are in the control group, _snz_uid gives the person they were matched to;

data matched ;
	set study_gp comparison_gp (in=a) ;
	if a then treat=0 ; 
	else treat=1 ;  
run ;

proc sort data=matched ; 
	by snz_uid;  
run ;

data &lib..rep0_&part_type.&suff.&strata.;
	set matched;
	strata=&strata.;
run;

proc freq data=&lib..rep0_&part_type.&suff.&strata.;
weight weight; tables treat;
title "&part_type.&strata.";
run;

data &lib..matchrate_&part_type.&suff.;
	set matchrate_&part_type.&suff.1(in=a);
		if a then strata="&part_type.1 ";
run;

proc freq data=&lib..matchrate_&part_type.&suff.;
	tables strata*flag / nocol nopercent norow;
	tables strata*flag / nocol nopercent nofreq;
	title "Match rate for all strata - &part_type. with contemperaneous matching"; 
run;
%mend main;

%main(ypiyb,&classvars_yp_1.,&xvars_yp_1.,1,YP,);
%main(yppdpb,&classvars_ypp_1.,&xvars_ypp_1.,1,YPP,);
%main(yppdpb,&classvars_yppb_1.,&xvars_yppb_1.,1,YPP,b);

******CHECK MODELS ARE BALANCED ;
** Calculate the standardised bias before and after;

******Take the matched records, merge on the full set of model variables 
    and then re-run the model using the control group weights, to check that nothing is statistically significant;

%macro matches(lib,strata,part_type,suff);
proc sort data=&lib..rep0_&part_type.&suff.&strata.; 
	by snz_uid refmth; 
run;

proc sort data=&lib..allcases_&part_type._&strata.; 
	by snz_uid refmth; 
run;

data &lib..matched_modelrerun_&part_type.&suff.&strata.;
	merge &lib..rep0_&part_type.&suff.&strata.(in=a keep=snz_uid refmth treat pscore weight)
 		  &lib..allcases_&part_type._&strata.;
	by snz_uid refmth;
	if a;
run;
%mend;
%matches(ypiyb,1,YP,);
%matches(yppdpb,1,YPP,);
%matches(yppdpb,1,YPP,b);

%macro rerun(lib,classvars,xvars,strata,part_type,suff);
ods output ParameterEstimates=&lib..PE_rerun_&part_type.&suff.&strata.
           oddsratios=&lib..OR_rerun_&part_type.&suff.&strata.;
proc logistic data=&lib..matched_modelrerun_&part_type.&suff.&strata.;
class &classvars.;
model treat(event='1') = &xvars
	  / maxiter=100 firth rsquare; roc;	
	  weight weight;
	  where region ne 99;
title "Model re-run for &part_type. &strata.";
run;

** Just print out any results that are still significant to check how many left;
proc print data=&lib..PE_rerun_&part_type.&suff.&strata.;
	where ProbChiSq<=0.05;
	title "Significant vars model re-run &part_type. &strata.";
run;
%mend;

%rerun(ypiyb,&classvars_yp_1.,&xvars_yp_1.,1,YP,);
%rerun(yppdpb,&classvars_ypp_1.,&xvars_ypp_1.,1,YPP,);
%rerun(yppdpb,&classvars_yppb_1.,&xvars_yppb_1.,1,YPP,b);

****************************************************;
**PART B: DESCRIPTIVE PROFILE OF THE MATCHED GROUP AND THEIR COMPARISIONS;
**Takes individual characteristics from the earliest model dataset, before any aggregation of categories is done;
**************************************************;
%macro describe(lib,part_type,suff);
data matches;
set &lib..rep0_&part_type.&suff.1 (in=a keep=snz_uid refmth strata treat pscore weight _snz_uid _refmth);
run;

proc sort data=matches;
	by snz_uid refmth strata;
run;
data model_final;
	set &lib..allcases_&part_type._1;
run;
proc sort data=model_final;
	by snz_uid refmth strata;
run;

proc format;
value gp
1="&part_type. Strata 1 Treatment"
2="&part_type. Strata 1 Control"
;
run;

data &lib..allmodel_&part_type.&suff.;
	merge matches(in=a) model_final;
	by snz_uid refmth strata;
	if a;
		if strata=1 and treat=1 then gp=1;
		else if strata=1 and treat=0 then gp=2;
	year_started=year(startdate);
	format gp gp.;
run;

ods html body="\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output &lib.\Matched_sample_profile_modelvars &part_type.&suff..xls";
%macro profile(vars);
proc freq data=&lib..allmodel_&part_type.&suff.;
	tables (&vars)*gp/ missing norow nocol nopercent;
	weight weight;
	title "Matched sample means ";
run;
%mend;

%profile(refmth cohort age_det female maori pacific asian ethoth
  nzdep region decile schtype schauth hqual hothqual hterqual 
  ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nsch_cat time_since_school
  genskills_only gen_n_occ_skills
  sch_last6 emp_last6 tert_last6 neet_last6 ben_last6
  sch_prior6 emp_prior6 tert_prior6 neet_prior6 ben_prior6
  sch_first12 emp_first12 tert_first12 neet_first12 ben_first12
  prop_onben_aschild_cat ever_onben_aschild child_bentype
  prop_os_aschild_cat mother_unqual cg_cust cg_comm 
  child_not_cat child_yj_referral_cat child_cyf_place child_yj_place child_any_fdgs_abuse all_act
  sedu_da trua_da susp_da_cat stand_da_cat);
ods html close;

%mend describe;

%describe(ypiyb,YP,);
%describe(yppdpb,YPP,);
%describe(yppdpb,YPP,b);