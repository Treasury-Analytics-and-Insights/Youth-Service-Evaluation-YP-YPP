/* 0 Libnames and macros */

** This code assigns libnames used elsewhere in the analysis (plus some others) and constructs some macros used to undertake the propensity score matching;

** Set the version that most of the analysis runs off - December 2016 update / outcomes were updated in the June 2016 update though;
%let date=24022016;
%let VERSION=20160224;

** These are the libnames for the March 2016 update linking in new outcomes data;
%let new_VERSION=20160418;
libname moenew ODBC dsn=idi_clean_&new_VERSION._srvprd schema=moe_clean;
libname datanew ODBC dsn=idi_clean_&new_VERSION._srvprd schema=data;
libname msdnew ODBC dsn=idi_clean_&new_VERSION._srvprd schema=msd_clean;
libname cornew ODBC dsn=idi_clean_&new_VERSION._srvprd schema=cor_clean;
libname ystnew ODBC dsn=idi_clean_&new_VERSION._srvprd schema=yst_clean;
libname secnew ODBC dsn=idi_clean_&new_VERSION._srvprd schema=security;

** Set standard libnames for the December 2015 update;
libname cen ODBC dsn=idi_clean_&VERSION._srvprd schema=cen_clean;
libname dol ODBC dsn=idi_clean_&VERSION._srvprd schema=dol_clean;
libname hlfs ODBC dsn=idi_clean_&VERSION._srvprd schema=hlfs_clean;
libname leed ODBC dsn=idi_clean_&VERSION._srvprd schema=from_leed_clean;
libname moe ODBC dsn=idi_clean_&VERSION._srvprd schema=moe_clean;
libname msd_leed ODBC dsn=idi_clean_&VERSION._srvprd schema=from_leed_clean;
libname msd ODBC dsn=idi_clean_&VERSION._srvprd schema=msd_clean;
libname sla ODBC dsn=idi_clean_&VERSION._srvprd schema=sla_clean;
libname moe ODBC dsn=idi_clean_&VERSION._srvprd schema=moe_clean;
libname cor ODBC dsn=idi_clean_&VERSION._srvprd schema=cor_clean;
libname moj ODBC dsn=idi_clean_&VERSION._srvprd schema=moj_clean;
libname acc ODBC dsn=idi_clean_&VERSION._srvprd schema=acc_clean;
libname cus ODBC dsn=idi_clean_&VERSION._srvprd schema=cus_clean;
libname lisnz ODBC dsn=idi_clean_&VERSION._srvprd schema=lisnz_clean;
libname ms ODBC dsn=idi_clean_&VERSION._srvprd schema=ms_clean;
libname sofie ODBC dsn=idi_clean_&VERSION._srvprd schema=sofie_clean;
libname dbh ODBC dsn=idi_clean_&VERSION._srvprd schema=dbh_clean;
libname br ODBC dsn=idi_clean_&VERSION._srvprd schema=br_clean;
libname cyf ODBC dsn=idi_clean_&VERSION._srvprd schema=cyf_clean;
libname dia ODBC dsn=idi_clean_&VERSION._srvprd schema=dia_clean;
libname pol ODBC dsn=idi_clean_&VERSION._srvprd schema=pol_clean;
libname moh ODBC dsn=idi_clean_&VERSION._srvprd schema=moh_clean;
libname data ODBC dsn=idi_clean_&VERSION._srvprd schema=data;
libname wff ODBC dsn=idi_clean_&VERSION._srvprd schema=wff_clean;
libname yst ODBC dsn=idi_clean_&VERSION._srvprd schema=yst_clean;
libname ird ODBC dsn=idi_clean_&VERSION._srvprd schema=ir_clean;
libname security ODBC dsn=idi_clean_&VERSION._srvprd schema=security;

libname sanddol ODBC dsn=idi_sandpit_srvprd schema="clean_read_dol";
libname sandmoe ODBC dsn=idi_sandpit_srvprd schema="clean_read_moe";
libname sandmsd ODBC dsn=idi_sandpit_srvprd schema="clean_read_msd";
libname sandcen ODBC dsn=idi_sandpit_srvprd schema="clean_read_cen";
libname sandhes ODBC dsn=idi_sandpit_srvprd schema="clean_read_hes";

** Set libnames for the project - 
** 		- sharedro/project/chris/age_0_14 are source data from previous work on population datasets 
**		- ys/ys5/yts are from Sylvia's YS:NEET analysis - I use the population-level activity, outcome etc. datasets from Sylvia;
**		- ypypp/yppdpb/ypiyb are for this project - the first one is for data across the project, the second just for the YPP analysis, the third just YP;
libname SharedRO "\\wprdsas10\RODatalab\MAA2013-16  Citizen pathways through human services\B16";	
libname age_0_14 "\\wprdsas10\TreasuryData\MAA2013-16c Youth Funding Review 0-5 years\version 20150805\SASFiles\ages 6-14";
libname Project "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\Social Investment_2016\1_Indicator_at_age_datasets\dataset_rerun_24022016";
libname Chris "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\Chris\Working Directory\Family Relationship";	
libname ys "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data March refresh\Base datasets";
libname ys5 "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data March refresh\Impact evaluation";
libname yts "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\Youth Service\Data March refresh\YTS evaluation";
libname ypypp "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\data\";
libname yppdpb "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\data ypp2\";
libname ypiyb "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\data yp2\";
libname schools "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\Sylvia\School deciles\Data";	

** Set libnames for the folders where bootstrap replicate datasets are to be stored for each propensity score model;
libname bYP "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\data YP2\Replicate weights YP\Strata1YP";
libname bIYB "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\data YP2\Replicate weights YP\Strata1IYB";
libname bYPold "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\data YP2\Replicate weights YP\Strata1YPold";
libname bIYBold "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\data YP2\Replicate weights YP\Strata1IYBold";
libname bYPP "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\data YPP2\Replicate weights YPP\Strata1YPP";
libname bDPBEMA "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\data YPP2\Replicate weights YPP\Strata1DPBEMA";
libname bYPPb "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\data YPP2\Replicate weights YPP\Strata1YPP_b";
libname bDPBEMAb "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\data YPP2\Replicate weights YPP\Strata1DPBEMA_b";

** Include the standard macros developed by AandI;
%include "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\Common Code\Std_macros_and_libs\Std_macros.txt";


** Macros and code below specifies how each model will be run - which variables to be used in the matching, 
**	and criteria for matching (pscore band and variables to use for blocking - exact matching);

*** YP MODEL SPEC;
%let classvars_yp_1= refmth(ref='167') cohort(ref='1996') age_det(ref='17') female maori pacific asian ethoth /*time_since_school(ref='a<2mths')*/
  nzdep(ref='5') region(ref='2') decile(ref='5') schtype(ref='Other') schauth(ref='Other') hqual(ref='1') hothqual(ref='0') hterqual(ref='0') 
  ncea_cr_l1_cat(ref='4') ncea_cr_l2_cat(ref='2') ncea_cr_l3_cat(ref='1') nsch_cat(ref='2') 
  sch_last6(ref=first) /*emp_last6(ref=first)*/ tert_last6(ref=first) /*neet_last6(ref=first)*/ ben_last6(ref=first)
  sch_prior6(ref=first) emp_prior6(ref=first) tert_prior6(ref=first) neet_prior6(ref=first) ben_prior6(ref=first)
  sch_first12(ref=first) emp_first12(ref=first) tert_first12(ref=first) neet_first12(ref=first) ben_first12(ref=first)
  prop_onben_aschild_cat(ref=first) 
  prop_os_aschild_cat(ref=first) mother_unqual cg_cust cg_comm 
  child_cyf_place child_yj_place child_any_fdgs_abuse all_act sedu_da trua_da 
  child_not_cat(ref=first)  child_yj_referral_cat(ref=first) susp_da_cat(ref=first) stand_da_cat(ref=first);
%let xvars_yp_1= yts_ysneet /*time_since_school*/
  refmth cohort age_det female maori pacific asian ethoth
  nzdep region decile schtype schauth hqual hothqual hterqual 
  ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nsch_cat 
  occskills_only genskills_only gen_n_occ_skills
  sch_last6 /*emp_last6*/ tert_last6 /*neet_last6*/ ben_last6
  sch_prior6 emp_prior6 tert_prior6 neet_prior6 ben_prior6
  sch_first12 emp_first12 tert_first12 neet_first12 ben_first12
  prop_onben_aschild_cat
  prop_os_aschild_cat mother_unqual cg_cust cg_comm 
  child_not_cat child_yj_referral_cat child_cyf_place child_yj_place child_any_fdgs_abuse all_act
  sedu_da trua_da susp_da_cat stand_da_cat;

%macro run_match_yp_1;
data score1;
	set score1;
	if unique_id=. then unique_id=0;
run;

********MATCHING******;
**Get the variables needed for the matching;
data study_m(drop=treat rename=(snz_uid=_snz_uid refmth=_refmth female=_female age_det =_age_det age2 =_age2 ben_last6=_ben_last6 unique_id=_unique_id
        hqual=_hqual hqual2=_hqual2 region=_region region2=_region2 tert_prior6=_tert_prior6 sch_prior6=_sch_prior6 p_1=_pscore startdate=_startdate)) 
      comparison_m(drop=treat rename=(p_1=pscore )) ;
	set score1(keep=snz_uid refmth treat female age_det age2  hqual region region2 tert_prior6 sch_prior6 startdate p_1 ben_last6 unique_id);
	if p_1~=.;
	if hqual in (3,6) then hqual2=2;
	else hqual2=hqual;
	if treat=1 then output study_m ; 
	else output comparison_m ;
run;

proc sql ;
  create table matchesa as
  select *
  from study_m a, comparison_m b
  where	a._refmth=b.refmth and
   		a._hqual=b.hqual and
   		a._age_det=b.age_det and
		a._ben_last6=ben_last6 and
		a._sch_prior6=sch_prior6 and
		a._tert_prior6=tert_prior6 and
  		a._pscore+0.03 >= b.pscore  and    
        a._pscore-0.03 <= b.pscore
  order by _snz_uid, _unique_id, snz_uid;
quit;
%mend run_match_yp_1;

*** IYB MODEL SPEC;
%let classvars_IYB_1=
  refmth(ref='130') cohort(ref='1993') age_det(ref='17') /*time_since_school(ref='a<2mths')*/
  nzdep(ref='5') region(ref='2') decile(ref='5') schtype(ref='Other') schauth(ref='State') hqual(ref='1')
    ncea_cr_l1_cat(ref='4') ncea_cr_l2_cat(ref='2') ncea_cr_l3_cat(ref='1') nsch_cat(ref='2')
  sch_last6(ref=first) /*emp_last6(ref=first)*/ tert_last6(ref=first) /*neet_last6(ref=first)*/ ben_last6(ref=first)
  sch_prior6(ref=first) emp_prior6(ref=first) tert_prior6(ref=first) neet_prior6(ref=first) ben_prior6(ref=first)
  sch_first12(ref=first) emp_first12(ref=first) tert_first12(ref=first) neet_first12(ref=first) ben_first12(ref=first)
  prop_onben_aschild_cat(ref=first) 
  prop_os_aschild_cat(ref=first) 
  child_not_cat(ref=first)  child_yj_referral_cat(ref=first)  susp_da_cat(ref=first) stand_da_cat(ref=first);
%let xvars_IYB_1= /*time_since_school*/
  refmth cohort yts_ysneet age_det female maori pacific asian ethoth
  nzdep region decile schtype schauth hqual hothqual hterqual ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nsch_cat
  occskills_only genskills_only gen_n_occ_skills
  sch_last6 /*emp_last6*/ tert_last6 /*neet_last6*/ ben_last6
  sch_prior6 emp_prior6 tert_prior6 neet_prior6 ben_prior6
  sch_first12 emp_first12 tert_first12 neet_first12 ben_first12
  prop_onben_aschild_cat 
  prop_os_aschild_cat mother_unqual cg_cust cg_comm
  child_not_cat child_any_fdgs_abuse child_cyf_place child_yj_referral_cat child_yj_place all_act
  sedu_da trua_da susp_da_cat stand_da_cat;

%macro run_match_iyb_1;
data score1;
	set score1;
	if unique_id=. then unique_id=0;
run;

********MATCHING******;
**Get the variables needed for the matching;
data study_m(drop=treat rename=(snz_uid=_snz_uid refmth=_refmth female=_female age_det =_age_det tert_prior6=_tert_prior6 sch_prior6=_sch_prior6 startdate=_startdate
        hqual=_hqual hqual2=_hqual2 region=_region region2=_region2 p_1=_pscore startdate=_startdate ben_last6=_ben_last6 unique_id=_unique_id)) 
      comparison_m(drop=treat rename=(p_1=pscore )) ;
	set score1(keep=snz_uid refmth treat female age_det hqual region region2 p_1 startdate sch_prior6 tert_prior6 ben_last6 unique_id);
	if p_1~=.;
	if hqual in (3,6) then hqual2=2;
	else hqual2=hqual;
	if treat=1 then output study_m ; 
	else output comparison_m ;
run;

proc sql ;
  create table matchesa as
  select *
  from study_m a, comparison_m b
  where	a._refmth=b.refmth and
   		a._hqual=b.hqual and
   		a._age_det=b.age_det and
		a._ben_last6=ben_last6 and
		a._sch_prior6=sch_prior6 and
		a._tert_prior6=tert_prior6 and
  		a._pscore+0.01 >= b.pscore  and    
        a._pscore-0.01 <= b.pscore
  order by _snz_uid, _unique_id, snz_uid;
quit;
%mend run_match_iyb_1;


** YPP MODEL SPEC;
%let classvars_YPP_1= 
  refmth(ref='167') cohort(ref='1996') age_det(ref='17') female maori pacific asian ethoth /*time_since_school(ref='a<2mths')*/
  nzdep(ref='5') region(ref='2') decile(ref='5') schtype(ref='Other') schauth(ref='Other') hqual(ref='1') hothqual(ref='0') hterqual(ref='0') 
  ncea_cr_l1_cat(ref='4') ncea_cr_l2_cat(ref='2') ncea_cr_l3_cat(ref='1') nsch_cat(ref='2') 
  /*sch_last6(ref=first) emp_last6(ref=first) tert_last6(ref=first) neet_last6(ref=first) ben_last6(ref=first)*/
  sch_prior6(ref=first) emp_prior6(ref=first) tert_prior6(ref=first) neet_prior6(ref=first) /*ben_prior6(ref=first)*/
  sch_first12(ref=first) emp_first12(ref=first) tert_first12(ref=first) neet_first12(ref=first) ben_first12(ref=first)
  prop_onben_aschild_cat(ref=first) 
  prop_os_aschild_cat(ref=first) mother_unqual cg_cust cg_comm 
  child_cyf_place child_yj_place child_any_fdgs_abuse all_act
  sedu_da trua_da 
  child_not_cat(ref=first)  child_yj_referral_cat(ref=first)  susp_da_cat(ref=first) stand_da_cat(ref=first);
%let xvars_YPP_1= yts_ysneet /*time_since_school*/
  refmth cohort age_det female maori pacific asian ethoth
  nzdep region decile schtype schauth hqual hothqual hterqual 
  ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nsch_cat 
  occskills_only genskills_only gen_n_occ_skills
  /*sch_last6 emp_last6 tert_last6 neet_last6 ben_last6*/
  sch_prior6 emp_prior6 tert_prior6 neet_prior6 /*ben_prior6*/
  sch_first12 emp_first12 tert_first12 neet_first12 ben_first12
  prop_onben_aschild_cat 
  prop_os_aschild_cat mother_unqual cg_cust cg_comm 
  child_not_cat child_yj_referral_cat child_cyf_place child_yj_place child_any_fdgs_abuse all_act
  sedu_da trua_da susp_da_cat stand_da_cat;

%macro run_match_ypp_1;
data score1;
	set score1;
	if unique_id=. then unique_id=0;
run;

********MATCHING******;
**Get the variables needed for the matching;
data study_m(drop=treat rename=(snz_uid=_snz_uid refmth=_refmth female=_female age_det =_age_det age2 =_age2 ben_first12=_ben_first12 unique_id=_unique_id
								ben_first12b=_ben_first12b tert_first12b=_tert_first12b sch_first12b=_sch_first12b ben_last6b=_ben_last6b ben_prior6b=_ben_prior6b 
        						hqual=_hqual hqual2=_hqual2 region=_region region2=_region2 tert_first12=_tert_first12 sch_first12=_sch_first12 p_1=_pscore 
								 startdate=_startdate ben_last6=_ben_last6 ben_prior6=_ben_prior6)) 
      comparison_m(drop=treat rename=(p_1=pscore )) ;
	set score1(keep=snz_uid refmth startdate treat female age_det age2  hqual region region2 tert_first12 sch_first12 ben_first12 ben_last6 ben_prior6 unique_id p_1 );
	if p_1~=.;
	if hqual in (3,6) then hqual2=2;
	else hqual2=hqual;
	if tert_first12>0 then tert_first12b=1;
	else tert_first12b=0;
	if sch_first12>0 then sch_first12b=1;
	else sch_first12b=0;
	if ben_first12>0 then ben_first12b=1;
	else ben_first12b=0;
	if ben_prior6>0 then ben_prior6b=1;
	else ben_prior6b=0;
	if ben_last6>0 then ben_last6b=1;
	else ben_last6b=0;
	if treat=1 then output study_m ; 
	else output comparison_m ;
run;

proc sql ;
  create table matchesa as
  select *
  from study_m a, comparison_m b
  where	a._refmth=b.refmth and
   		a._hqual=b.hqual and
   		a._age_det=b.age_det and
   		a._female=b.female and
		a._sch_first12=sch_first12 and
		a._tert_first12b=tert_first12b and
		a._ben_first12b=ben_first12b and
		/*a._ben_prior6b=ben_prior6b and
		a._ben_last6b=ben_last6b and*/
  		a._pscore+0.03 >= b.pscore  and    
        a._pscore-0.03 <= b.pscore
  order by _snz_uid, _unique_id, snz_uid;
quit;
%mend run_match_ypp_1;

** DPBEMA MODEL SPEC;
%let classvars_DPBEMA_1=
  refmth(ref='130') cohort(ref='1993') age_det(ref='17') /*time_since_school(ref='a<2mths')*/
  nzdep(ref='5') region(ref='2') decile(ref='5') schtype(ref='Other') schauth(ref='Other') hqual(ref='1')
    ncea_cr_l1_cat(ref='4') ncea_cr_l2_cat(ref='2') ncea_cr_l3_cat(ref='1') nsch_cat(ref='2')
  /*sch_last6(ref=first) emp_last6(ref=first) tert_last6(ref=first) neet_last6(ref=first) ben_last6(ref=first)*/ 
  sch_prior6(ref=first) emp_prior6(ref=first) tert_prior6(ref=first) neet_prior6(ref=first) /*ben_prior6(ref=first)*/
  sch_first12(ref=first) emp_first12(ref=first) tert_first12(ref=first) neet_first12(ref=first) ben_first12(ref=first)
  prop_onben_aschild_cat(ref=first) prop_os_aschild_cat(ref=first) 
  child_not_cat(ref=first)  child_yj_referral_cat(ref=first)  susp_da_cat(ref=first) stand_da_cat(ref=first);
%let xvars_DPBEMA_1= /*time_since_school*/
  refmth cohort yts_ysneet age_det female maori pacific asian ethoth
  nzdep region decile schtype schauth hqual hothqual hterqual ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nsch_cat
  occskills_only genskills_only gen_n_occ_skills
  /*sch_last6 emp_last6 tert_last6 neet_last6 ben_last6*/ 
  sch_prior6 emp_prior6 tert_prior6 neet_prior6 /*ben_prior6*/
  sch_first12 emp_first12 tert_first12 neet_first12 ben_first12
  prop_onben_aschild_cat prop_os_aschild_cat mother_unqual cg_cust cg_comm
  child_not_cat child_any_fdgs_abuse child_cyf_place child_yj_referral_cat child_yj_place all_act
  sedu_da trua_da susp_da_cat stand_da_cat;

%macro run_match_dpbema_1;
data score1;
	set score1;
	if unique_id=. then unique_id=0;
run;

********MATCHING******;
**Get the variables needed for the matching;
data study_m(drop=treat rename=(snz_uid=_snz_uid refmth=_refmth female=_female age_det =_age_det ben_first12=_ben_first12 unique_id=_unique_id
								ben_first12b=_ben_first12b tert_first12b=_tert_first12b sch_first12b=_sch_first12b
        		hqual=_hqual hqual2=_hqual2 region=_region region2=_region2 tert_first12=_tert_first12 sch_first12=_sch_first12 p_1=_pscore startdate=_startdate)) 
      comparison_m(drop=treat rename=(p_1=pscore )) ;
	set score1(keep=snz_uid refmth startdate treat female age_det hqual region region2 tert_first12 sch_first12 p_1 ben_first12 unique_id);
	if p_1~=.;
	if hqual in (3,6) then hqual2=2;
	else hqual2=hqual;
	if tert_first12>0 then tert_first12b=1;
	else tert_first12b=0;
	if sch_first12>0 then sch_first12b=1;
	else sch_first12b=0;
	if ben_first12>0 then ben_first12b=1;
	else ben_first12b=0;
	if treat=1 then output study_m ; 
	else output comparison_m ;
run;

proc sql ;
  create table matchesa as
  select *
  from study_m a, comparison_m b
  where	a._refmth=b.refmth and
   		a._hqual=b.hqual and
   		a._age_det=b.age_det and
   		a._female=b.female and
		a._sch_first12=b.sch_first12 and
		a._tert_first12b=b.tert_first12b and
		a._ben_first12=b.ben_first12 and
  		a._pscore+0.03 >= b.pscore  and    
        a._pscore-0.03 <= b.pscore
  order by _snz_uid, _unique_id, snz_uid;
quit;
%mend run_match_dpbema_1;

** YPP B MODEL SPEC;
%let classvars_YPPb_1= 
  refmth(ref='167') cohort(ref='1996') age_det(ref='17') female maori pacific asian ethoth /*time_since_school(ref='a<2mths')*/
  nzdep(ref='5') region(ref='2') decile(ref='5') schtype(ref='Other') schauth(ref='Other') hqual(ref='1') hothqual(ref='0') hterqual(ref='0') 
  ncea_cr_l1_cat(ref='4') ncea_cr_l2_cat(ref='2') ncea_cr_l3_cat(ref='1') nsch_cat(ref='2') 
  sch_last6(ref=first) /*emp_last6(ref=first)*/ tert_last6(ref=first) /*neet_last6(ref=first) ben_last6(ref=first)*/
  sch_prior6(ref=first) emp_prior6(ref=first) tert_prior6(ref=first) neet_prior6(ref=first) ben_prior6(ref=first)
  sch_first12(ref=first) emp_first12(ref=first) tert_first12(ref=first) neet_first12(ref=first) ben_first12(ref=first)
  prop_onben_aschild_cat(ref=first) 
  prop_os_aschild_cat(ref=first) mother_unqual cg_cust cg_comm 
  child_cyf_place child_yj_place child_any_fdgs_abuse all_act sedu_da trua_da 
  child_not_cat(ref=first)  child_yj_referral_cat(ref=first)  susp_da_cat(ref=first) stand_da_cat(ref=first);
%let xvars_YPPb_1= yts_ysneet /*time_since_school*/
  refmth cohort age_det female maori pacific asian ethoth
  nzdep region decile schtype schauth hqual hothqual hterqual 
  ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nsch_cat 
  occskills_only genskills_only gen_n_occ_skills
  sch_last6 /*emp_last6*/ tert_last6 /*neet_last6 ben_last6*/
  sch_prior6 emp_prior6 tert_prior6 neet_prior6 ben_prior6
  sch_first12 emp_first12 tert_first12 neet_first12 ben_first12
  prop_onben_aschild_cat 
  prop_os_aschild_cat mother_unqual cg_cust cg_comm 
  child_not_cat child_yj_referral_cat child_cyf_place child_yj_place child_any_fdgs_abuse all_act
  sedu_da trua_da susp_da_cat stand_da_cat;

%macro run_match_yppb_1;
data score1;
	set score1;
	if unique_id=. then unique_id=0;
run;

********MATCHING******;
**Get the variables needed for the matching;
data study_m(drop=treat rename=(snz_uid=_snz_uid refmth=_refmth female=_female age_det =_age_det age2 =_age2 ben_prior6=_ben_prior6 ben_prior6b=_ben_prior6b unique_id=_unique_id
        hqual=_hqual hqual2=_hqual2 region=_region region2=_region2 tert_last6=_tert_last6 sch_last6=_sch_last6 p_1=_pscore startdate=_startdate))
      comparison_m(drop=treat rename=(p_1=pscore )) ;
	set score1(keep=snz_uid refmth startdate treat female age_det age2  hqual region region2 tert_last6 sch_last6 p_1 ben_prior6 unique_id);
	if p_1~=.;
	if hqual in (3,6) then hqual2=2;
	else hqual2=hqual;
	if ben_prior6>0 then ben_prior6b=1;
	else ben_prior6b=0;
	if treat=1 then output study_m ; 
	else output comparison_m ;
run;

proc sql ;
  create table matchesa as
  select *
  from study_m a, comparison_m b
  where	a._refmth=b.refmth and
   		a._hqual=b.hqual and
   		a._age_det=b.age_det and
   		a._female=b.female and
		a._sch_last6=b.sch_last6 and
		a._tert_last6=b.tert_last6 and
		a._ben_prior6b=b.ben_prior6b and
  		a._pscore+0.03 >= b.pscore and    
        a._pscore-0.03 <= b.pscore
  order by _snz_uid, _unique_id, snz_uid;
quit;
%mend run_match_yppb_1;


** DPBEMA B MODEL SPEC;
%let classvars_DPBEMAb_1=
  refmth(ref='130') cohort(ref='1993') age_det(ref='17') /*time_since_school(ref='a<2mths')*/
  nzdep(ref='5') region(ref='2') decile(ref='5') schtype(ref='Other') schauth(ref='Other') hqual(ref='1')
    ncea_cr_l1_cat(ref='4') ncea_cr_l2_cat(ref='2') ncea_cr_l3_cat(ref='1') nsch_cat(ref='2')
  sch_last6(ref=first) /*emp_last6(ref=first)*/ tert_last6(ref=first) /*neet_last6(ref=first) ben_last6(ref=first)*/
  sch_prior6(ref=first) emp_prior6(ref=first) tert_prior6(ref=first) neet_prior6(ref=first) ben_prior6(ref=first)
  sch_first12(ref=first) emp_first12(ref=first) tert_first12(ref=first) neet_first12(ref=first) ben_first12(ref=first)
  prop_onben_aschild_cat(ref=first) /*ever_onben_aschild(ref=first) child_bentype(ref=first)*/
  prop_os_aschild_cat(ref=first) 
  child_not_cat(ref=first)  child_yj_referral_cat(ref=first)  susp_da_cat(ref=first) stand_da_cat(ref=first);
%let xvars_DPBEMAb_1= /*time_since_school*/
  refmth cohort yts_ysneet age_det female maori pacific asian ethoth
  nzdep region decile schtype schauth hqual hothqual hterqual ncea_cr_l1_cat ncea_cr_l2_cat ncea_cr_l3_cat nsch_cat
  occskills_only genskills_only gen_n_occ_skills
  sch_last6 /*emp_last6*/ tert_last6 /*neet_last6 ben_last6*/
  sch_prior6 emp_prior6 tert_prior6 neet_prior6 ben_prior6
  sch_first12 emp_first12 tert_first12 neet_first12 ben_first12
  prop_onben_aschild_cat /*ever_onben_aschild child_bentype */
  prop_os_aschild_cat mother_unqual cg_cust cg_comm
  child_not_cat child_any_fdgs_abuse child_cyf_place child_yj_referral_cat child_yj_place all_act
  sedu_da trua_da susp_da_cat stand_da_cat;

%macro run_match_dpbemab_1;
data score1;
	set score1;
	if unique_id=. then unique_id=0;
run;

********MATCHING******;
**Get the variables needed for the matching;
data study_m(drop=treat rename=(snz_uid=_snz_uid refmth=_refmth female=_female age_det =_age_det ben_prior6=_ben_prior6 unique_id=_unique_id
        hqual=_hqual hqual2=_hqual2 region=_region region2=_region2 tert_last6=_tert_last6 sch_last6=_sch_last6 p_1=_pscore startdate=_startdate)) 
      comparison_m(drop=treat rename=(p_1=pscore )) ;
	set score1(keep=snz_uid startdate refmth treat female age_det hqual region region2 tert_last6 sch_last6 ben_prior6 p_1 unique_id);
	if p_1~=.;
	if hqual in (3,6) then hqual2=2;
	else hqual2=hqual;
	if treat=1 then output study_m ; 
	else output comparison_m ;
run;

proc sql ;
  create table matchesa as
  select *
  from study_m a, comparison_m b
  where	a._refmth=b.refmth and
   		a._hqual=b.hqual and
   		a._age_det=b.age_det and
   		a._female=b.female and
		a._sch_last6=b.sch_last6 and
		a._tert_last6=b.tert_last6 and
		a._ben_prior6=b.ben_prior6 and
  		a._pscore+0.03 >= b.pscore  and    
        a._pscore-0.03 <= b.pscore
  order by _snz_uid, _unique_id, snz_uid;
quit;
%mend run_match_dpbemab_1;


/**Standard errors  */
/** This macro creates replicate weights for the calculation of SEs using bootstrap sampling */

%macro SEweightb(part_type, strata, boot, lib, suff);
%put &part_type.&suff. &strata.;

data treatment control;
set &lib..allcases_&part_type._&strata;
if treat=1 then output treatment;
else output control;
run;

data _null_;
	set treatment;
	call symput('sampsize_s',_n_);
run;

proc surveyselect data=treatment  noprint
	method=urs reps=1 outhits sampsize=&sampsize_s out=treatment2;
run;

proc sort data=treatment2;
by snz_uid ;
run;

data samplea;
	set treatment2;
	by snz_uid ;
	if first.snz_uid then unique_id = 0 ;
	  unique_id + 1;
run ;

proc sort data=control(keep=snz_uid) out=control_uids nodupkey;
	by snz_uid;
run;

data _null_;
	set control_uids;
	call symput('sampsize_c',_n_);
run;

proc surveyselect data=control_uids noprint 
	method=urs reps=1 outhits sampsize=&sampsize_c out=control_uids2;
run;

proc sort data=control;
by snz_uid refmth;
run;

data control2;
	merge control control_uids2(in=a);
	by snz_uid;
	if a;
run;

data sampleb;
	set control2;
	by snz_uid refmth;
	if first.refmth then unique_id = 0 ;
    unique_id + 1 ;
run ;

data allcases;
	set samplea sampleb;
run;

**Subsample 10000 controls to use in the regression; 
proc surveyselect data=sampleb(drop=replicate) method=srs reps=1 
   sampsize=10000 out=control3;

data regression;
set samplea control3;
run;

proc logistic data=regression noprint;
class &&classvars_&part_type.&suff._&strata.;
model treat(event='1') = &&xvars_&part_type.&suff._&strata.
	  / maxiter=200 firth rsquare; roc;
	  score data=allcases out=score1;
run;

%run_match_&part_type.&suff._&strata.;

data matches_final;
	set matchesa;
	match_run=1;
run;

****CALCULATING THE MATCH RATE; 
proc freq data=matches_final noprint ; 
	tables _snz_uid*_unique_id / out=out ;
run ;

proc sort data=study_m ;  
	by _snz_uid _unique_id;
run;

data matchrate_&part_type.&suff.&strata ;
	merge study_m (in=a )
    out (in=b keep=_snz_uid _unique_id count) ;
  	by _snz_uid _unique_id;
  	if a and b  then flag='Matched  ' ;
  	if a and not b then flag='Unmatched  ' ;
  	if a and not b then count=0 ;
run ;

***SELECTING UP TO 20 BEST MATCHES TO KEEP AND CALCULATING WEIGHTS FOR THE MATCHED CONTROL GROUP;
data matches2 ;  
set matches_final;
 distance=abs(_pscore-pscore);
 rn=rand('uniform')  ;
run ;

proc sort data=matches2 ; 
by _snz_uid _unique_id distance rn ; 
run; * take closest matches ;

data matches3 ; 
	set matches2;
 	by _snz_uid _unique_id  distance rn ;
 	if first._unique_id then n=0 ;
 	n+1 ;
	if n<=20 then output ; * keep max of 20 matches ;
run ;

proc freq data=matches3 noprint ;
  tables _snz_uid*_unique_id / out=out2 ;
run ;

 * Weight per each matched non participant;
data out3 (keep=_snz_uid _unique_id weight) ;  
  set out2 ;
  weight=1/count ;
run ;

*One record per matched control person;

data comparison_gp(keep=snz_uid unique_id _snz_uid _unique_id refmth _refmth weight pscore );   
  merge matches3 out3;
  by _snz_uid _unique_id;
run ;

**One record per matched study grp mbr;
data study_gp (rename=(_snz_uid=snz_uid _refmth=refmth _unique_id=unique_id _pscore=pscore count=nbr_matched /*flag=match_flag*/ )) ;
   set matchrate_&part_type.&suff.&strata.(keep=_snz_uid _unique_id _refmth count _pscore);
   if count=0 then weight=0 ; 
   else weight=1 ;
   if weight>0;
 run ;

 **Combine these study group and control group records;
 **The persons own id is stored in snz_uid but if they are in the control group, _snz_uid gives the person they were matched to;
data matched(keep=snz_uid refmth unique_id _snz_uid _unique_id treat pscore weight nbr_matched rename=(weight=wgt&i));
  set study_gp comparison_gp(in=a) ;
  if a then treat=0 ; 
  else treat=1 ;  
run ;

**Sum up records and weights for people who have been selected more than once into the sample for this iteration
    and matched more than once to the same study sample person;
proc summary data = matched nway MISSING ;    * need to specify missing or the records of the treatment group wont remain intact; 
  class snz_uid _snz_uid treat refmth ;    
  var wgt&i ;
  output out=&boot..rep&i(drop=_type_ _freq_ ) sum= ;
 run ;
%mend SEweightb;
