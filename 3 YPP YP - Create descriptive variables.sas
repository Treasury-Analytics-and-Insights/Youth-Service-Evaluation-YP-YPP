/* 3 YPP YP - Create descriptive variables */

*****************************************************;
**  Take YP-YPP participant population and equivalent pre-benefit reform youth benefit population;
**  Restrict to people who are on IDI spine, have an IRD number, and a domestic NZ school enrolment before July 2012;
** 	Create descriptive and activity variables necessary for matching, outcomes measurement, and describing groups;
******************************************************;

** Key data inputs:
*		- ypypp.all_participants2	(cleaned up YP YPP participant and control data)
*		- ys.mth_sch_enrol			(monthly school enrolment records from YSNEET analysis)
*		- ys.mth_ter_enrol			(monthly tertiary enrolment records from YSNEET analysis)
*		- ys.mth_emp				(monthly employment records from YSNEET analysis)
*		- ys.mth_it_enrol			(monthly industry training records from YSNEET analysis)
*		- ys.mth_ben				(monthly benefit records from YSNEET analysis)
*		- ys.mth_os					(monthly overseas spells from YSNEET analysis)
*		- ys.mth_studall			(monthly study records from YSNEET analysis)
*		- ys.mth_corrections		(monthly corrections records from YSNEET analysis)
* 		- ys5.monthly_revised		(monthly ys pre-participation activities)
*		- yts.pre_YS_monthly_revised(monthly yts pre-participation activities)
;

** Key data outputs:
* 		- ypypp.mth_ypypp			(monthly participation info)
*		- ypypp.popn 				(populations of SNZ_UIDS)
*		- ypypp.lastsch_ypiyb 		(last school info)
*		- ypypp.nschools_ypypp 		(number of schools)
*		- ypypp.tert_prog_preYPYPP 	(tertiary programme type)
*		- ypypp.lastmth_ypypp 		(location in last month before participating)
* 		- ypypp.pre_YPYPP_quals 	(quals prior to participation)
*		- ypypp.enddate_imputed		(records with an imputed school end date)
*		- ypypp.monthly 			(monthly indicators)
*		- ypypp.monthly_corrections (with corrections data)
*		- ypypp.pre_ypypp_monthly 	(preparticipation measures for matching)
*		- ypypp.annual 				(annual measures)
*		- ypypp.model_final 		(final dataset for matching)
*		- ypypp.depvars 			(outcome variables)
*		- ypypp.depvars_additional 	(additional outcomes variables)
* 		- ypypp.not_os				(population of participants and controls not overseas for 6 months or more)
*		- ypypp.overseas_6mthsplus	(population of participants and controls overseas for 6 months or more)
* 		- ypypp.kids_nonnqf			(population who attending a non national qualification framework school)
* 		- ypypp.prior_parts 		(records of prior participation in youth transition service or yp/ypp as control variables)
*		- ypypp.model_final_restricted (final data with exclusions applied for time in NZ, non-NQF school attendance or an imputed school end date)
*		- ypiyb.yp_newparts			(YP participant and matched control records)
*		- ypiyb.iyb_newparts		(YP historical control and matched controld records - IYB)
*		- yppdpb.ypp_newparts		(YPP participant and matched control records)
*		- yppdpb.DPBEMA_newparts	(YPP historical control and matched controld records - DPB/EMA)
* 		- ypypp.pre_ys5_monthly		(pre-YS summarised activity information)
*		- ypypp.pre_yts_monthly 	(pre-YTS summarised activity information)
;

** Start a month earlier so we can later check if participation started at that month or not;
%let start='01Dec2007'd;
%let first=106; *Jan2008;
%let last= 195; *Jun2015;

** Create monthly participation dataset for participants - note this isn't actually used in the analysis - instead we track benefit receipt more broadly;
data ypypp_month_temp(drop=ind i days start_window end_window);
	set ypypp.all_participants2;
	format start_window end_window date9.;
	array ypypp_id_(*) ypypp_id_%eval(&first-1)-ypypp_id_&last;
	array ypypp_da_(*) ypypp_da_%eval(&first-1)-ypypp_da_&last;

	do ind=&first-1 to &last;
		i=ind-&first+2;
		ypypp_id_(i)=0;
		ypypp_da_(i)=0;

		* overwriting start and end window as interval equal to one month;
		start_window=intnx("month",&start.,i-1,"beginning");

		* start is beg of the month;
		end_window=intnx("month",&start.,i-1,"end");

		* end is end of the month;
		if not((startdate > end_window) or (enddate < start_window)) then do;
				ypypp_id_(i)=1;

				if (startdate <= start_window) and  (enddate > end_window) then
					days=(end_window-start_window)+1;
				else if (startdate <= start_window) and  (enddate <= end_window) then
					days=(enddate-start_window)+1;
				else if (startdate > start_window) and  (enddate <= end_window) then
					days=(enddate-startdate)+1;
				else if (startdate > start_window) and  (enddate > end_window) then
					days=(end_window-startdate)+1;
				ypypp_da_[i]=days*ypypp_id_(i);
			end;
	end;
run;

proc summary data=ypypp_month_temp nway idmin;
	class snz_uid startdate;
	id additional_service_data;
	var ypypp_id_%eval(&first-1)-ypypp_id_&last ypypp_da_%eval(&first-1)-ypypp_da_&last;
	output out=ypyppTEMP(drop=_:) sum=;
run;

**Find first and last months of ypypp participation using LEED month seq;
data mth_ypypp(drop=ind i) zz;
	set ypypptemp;
	by snz_uid startdate;

	tot_ypypp_days=sum(of ypypp_da_&first-ypypp_da_&last);
	array ypypp_id_(*) ypypp_id_%eval(&first-1)-ypypp_id_&last;
	array ypypp_da_(*) ypypp_da_%eval(&first-1)-ypypp_da_&last;
	first_ypypp_refmth=.;
	last_ypypp_refmth=.;

	do ind=&first.-1 to &last;
		i=ind-&first+2;

		if ypypp_id_[i]>1 then
			ypypp_id_[i]=1;

		if ypypp_id_[i]>=1 and first_ypypp_refmth=. then
			first_ypypp_refmth=ind;

		if ypypp_id_[i]>=1 then
			last_ypypp_refmth=ind;
	end;
	if first_ypypp_refmth>=&first. then output mth_ypypp;
	else output zz;
run;

**Create std monthly vectors showing months with yp and ypp participation and days per month in the final 30 months;
**One rec per person*;

** First 48 months should be zeros;
data ypypp.mth_ypypp(drop=i windowstart_leed ypypp_id_%eval(&first-1)-ypypp_id_&last ypypp_da_%eval(&first-1)-ypypp_da_&last rename=(first_ypypp_refmth=refmth));
	merge ypypp.all_participants2 mth_ypypp(in=a);
	by snz_uid startdate;
	if a;
	windowstart_leed=first_ypypp_refmth-48;
	array ypypp_id_(*) 	ypypp_id_&first-ypypp_id_&last;
	array ypypp_da_(*) 	ypypp_da_&first-ypypp_da_&last;
	array nypypp(*) 	ypypp1-ypypp79;
	array nypypp2(*) 	ypypp_da1-ypypp_da79;

	** Construct 79 months worth of participation information - 48 months pre-participation, month of participation start, and 30 months post-participation;
	do i=1 to 79;
		if 0<i+(windowstart_leed-57)-1<90 then do;
			nypypp(i)=ypypp_id_(i+(windowstart_leed-57)-1);
			nypypp2(i)=ypypp_da_(i+(windowstart_leed-57)-1);
		end;
		else if i+(windowstart_leed-57)-1<1 then do; 
			nypypp(i)=0;
			nypypp2(i)=0;
		end;
		else do; 
			nypypp(i)=.;
			nypypp2(i)=.;
		end;
	end;
	** Sum up days in the first year post-participation;
	ypypp_days_y1=sum(of ypypp_da49-ypypp_da60);
run;

** Now assign all participants and potential controls to one of three strata - Still at school, enrolled in tertiary, all the rest;
** Note: These aren't used in the final analysis - the YP YPP numbers are too small to really do a stratified analysis;

** Set the reference months for which we have school and tertiary participation data;
%let m=82;
%let z=195;
%let n=106;
%let p=189;

data ypypp.mth_ypypp(drop=windowstart_leed i sch1-sch3 tert1-tert3 sch_enr_id_&m-sch_enr_id_&z ter_enr_id_&m-ter_enr_id_&p) 
	 strata(keep=snz_uid refmth participant part_type strata study_start);
	merge ypypp.mth_ypypp(in=a) 
		ys.mth_sch_enrol(in=b keep=snz_uid sch_enr_id_&m-sch_enr_id_&z)
	    ys.mth_ter_enrol(keep=snz_uid ter_enr_id_&m-ter_enr_id_&p) ;
    by snz_uid;
	if a;
    array sch(*) sch_enr_id_&m-sch_enr_id_&z;
	array tert(*) ter_enr_id_&m-ter_enr_id_&p;
	array nsch(*) sch1-sch3;
	array ntert(*) tert1-tert3;
    windowstart_leed=refmth-1;
	if refmth<=188 then do;
        do i=1 to 3;
			nsch(i)=sch(i+(windowstart_leed-81)-1);
			if nsch(i)=. then nsch(i)=0;
			ntert(i)=tert(i+(windowstart_leed-81)-1);
			if ntert(i)=. then ntert(i)=0;
		end;
		if sch2 then study_start=1; 		/* at school at t=0 */
 		else if tert2 then study_start=2; 	/*in tertiary but not school at t=0*/
		else study_start=3; 				/* not studying at t=0*/
		if mths_ben_to_ys=0 then do;
			if sch1=1 and sch2=1 and sch3=1 then strata=1;
			else if tert1=1 and tert2=1 and tert3=1 then strata=2.1;
			else strata=2.2;
			output;
		end;
	end;
run;

/*proc freq data=ypypp.mth_ypypp;
	tables part_type*participant/missing;
run;*/

** Create a dataset that just holds the uids of the participant and historical comparison populations;
data ypypp.popn;
	set ypypp.mth_ypypp(keep=snz_uid);
run;

%let population=ypypp.popn;

****LAST SCHOOL enrolment before refernece date - Needed here purely to exclude people with missing school enrolment enddate***********;
***Get characteristics of the LAST SCHOOL enrolment spell before the refmth and find out whether the enddate of this spell was imputed;
**Also look for attendance at any non-NQF school - these kids also to be dropped;
proc sql;
	create table enrol
		as select 
			snz_uid
			,input(compress(moe_esi_start_date,"-"),yymmdd10.) format date9. as startdate
			,case when moe_esi_end_date is not null then input(compress(moe_esi_end_date,"-"),yymmdd10.) 
			      else input(compress(moe_esi_extrtn_date,"-"),yymmdd10.) 
		end 
		format date9. as enddate
		,input(moe_esi_provider_code, 10.) as schoolnbr
		,moe_esi_domestic_status_code as domestic_status
		,case when moe_esi_end_date='  ' then 1 
		    else 0 end as sch_enddate_imputed
	from moe.student_enrol
		where snz_uid in (select distinct snz_uid from &population.) and moe_esi_start_date is not null 
			order by snz_uid, startdate, enddate;
quit;

**Removing any overlaps in enrolment;
%OVERLAP (enrol);

**Find non-NQF schools so chn who attended them can be identified;
data nonnqf;
	set enrol_or;
	if schoolnbr in 
		(29,41,52,54,62,78,81,89,130,141,278,281,436,439,440,441,456,459,460,484,571,620,1132,1139,1605,1626,1655,2085,4152,
		37,60,67,333,387,617,1606,1640);
run;

proc sql;
	create table kids_nonnqf
		as select distinct snz_uid
			from nonnqf
				order by snz_uid;
quit;

**Select last school before startdate, then select those whose enrolment spell enddate was imputed;
proc sql;
	create table keep
		as select a.snz_uid, a.refmth, a.dob, a.startdate as YSstartdate,
			b.*
		from ypypp.mth_ypypp a 
			left join enrol_or b
				on a.snz_uid=b.snz_uid
			    where b.startdate<a.startdate
				order by a.snz_uid,a.refmth, a.startdate, b.startdate;
quit;

proc sort data=keep;
	by snz_uid refmth YSstartdate startdate;
run;

data last_sch_rec;
	set keep;
	by snz_uid refmth YSstartdate startdate;
	if last.refmth;
run;


*********************************************************;
*Characterisitics of the last school attended before the refdate, 
**Time gap between last school attended and the refdate,
**Information about any tertiary study that was done before the reference date;
**************************************************************;
proc sql;
	create table profile
		as select 
			schoolnumber as schoolnbr
			,school_authority
			,schooltype
			,decile
		from project.schoolprofile_open_&version.
			where schoolnumber in (select distinct schoolnbr from last_sch_rec)
				order by schoolnbr;
quit;

**Link these additional school characteristics to the enrolment records;
proc sort data=last_sch_rec;
	by schoolnbr;
run;

data lastsch(keep=snz_uid refmth decile schtype school_authority nonnqf 
    last_sch_enddate age_when_left_sch sch_enddate_imputed);
	merge last_sch_rec(in=a) profile(rename=(decile=olddec));
	by schoolnbr;
	if a;
	last_sch_enddate=enddate;
	age_when_left_sch= floor((intck('month',dob,enddate)- (day(enddate) < day(dob))) / 12);
    
	if olddec='DecileNA' or olddec=' ' then
		decile=.;
	else decile=substr(olddec,7,2)*1;

	if school_authority='StateIntegrated' then
		school_authority='StateInt';

	if school_authority=' ' then
		school_authority='Other';

	if schoolnbr in 
		(29,41,52,54,62,78,81,89,130,141,278,281,436,439,440,441,456,459,460,484,571,620,1132,1139,1605,1626,1655,2085,4152,
		37,60,67,333,387,617,1606,1640) then
		nonnqf=1;
	else nonnqf=0;

	if school_authority in ('Private', 'Other' ) then
		decile=.;
	format schtype $12.;

	if schooltype in ('Special School', 'Special Unit', 'Special Unit Funded') then
		schtype='Special';
	else if schooltype ='Correspondence School' then
		schtype='Corresp';
	else if schooltype in ('   ', 'Miscellaneous') then
		schtype='Other';
	else schtype='Standard';
run;

proc sort data=lastsch ;
	by snz_uid refmth;
run;

**	Time gap between last school enrolment and YS enrolment;
data timegap(keep=snz_uid refmth YSstartdate enddate months_since_last_school);
set last_sch_rec;
if enddate<YSstartdate and sch_enddate_imputed~=1;
months_since_last_school=intck('month',enddate,YSstartdate,'D');
run;

proc sort data=timegap;
	by snz_uid refmth;
run;

data lastsch2;
	merge lastsch(in=a) timegap(keep=snz_uid refmth months_since_last_school);
	by snz_uid refmth;
	if a;
run;

data ypypp.lastsch_ypiyb(drop=startdate strata);
	merge ypypp.mth_ypypp(in=a keep=snz_uid refmth startdate strata) lastsch2(in=b);
	by snz_uid refmth;
	if a;
	**Set to missing if person has not yet left school ;
	if last_sch_enddate>=startdate or strata=1 then do;
 	   	age_when_left_sch=.;
  	  	months_since_last_school=.;
    end; 
run;


*******************************************************;
****NUMBER OF SCHOOLS ATTENDED BEFORE THE REFERENCE DATE;
**keep school enrolment spells that started before the YS startdate;
****************************************;
proc sql;
create table schenrol
as select 
snz_uid
,input(compress(moe_esi_start_date,"-"),yymmdd10.) format date9. as startdate
,case when moe_esi_end_date is not null then input(compress(moe_esi_end_date,"-"),yymmdd10.) 
   else input(compress(moe_esi_extrtn_date,"-"),yymmdd10.) end format date9. as enddate
,input(moe_esi_provider_code, 10.) as schoolnbr
,moe_esi_domestic_status_code as domestic_status
,case when moe_esi_end_date='  ' then 1 else 0 end as sch_enddate_imputed
from moe.student_enrol
where snz_uid in (select distinct snz_uid from ypypp.mth_ypypp) and moe_esi_start_date is not null 
order by snz_uid, startdate, enddate;
quit;

**Removing any overlaps in enrolment;
%OVERLAP (schenrol);

/*proc freq data=schenrol;
	tables startdate;
	format startdate year4.;
run;*/

** Sort and check for duplicates but there aren't any;
proc sort data=ypypp.mth_ypypp out=mth_ypypp nodupkey;
	by snz_uid refmth;
run;

proc sql;
	create table schenrol_1 as
		select a.snz_uid,
			a.refmth,
			a.startdate as ysstartdate,
			b.*
		from mth_ypypp a inner join schenrol_or b
			on a.snz_uid=b.snz_uid
			order by snz_uid, refmth;
quit;

** Only take school enrolments that started before the benefit spell starts;
data before;
set schenrol_1;
if startdate<YSstartdate;
count=1;
run;

**First summarise records to school level - we only want one record per person id, refmth and school id;
proc summary data=before nway;
class snz_uid refmth schoolnbr;
var count;
output out=stats(keep=snz_uid refmth schoolnbr count) sum=count;
run;

**Count number of distinct schools attended;
proc summary data=stats nway;
class snz_uid refmth;
var count;
output out=ypypp.nschools_ypypp(keep=snz_uid refmth nschools) n=nschools;
run;

** Set reference months for which we have school and tertiary data;
%let m=82;
%let z=195;
%let n=106;
%let p=189;

*******************************************************;
**NATURE OF ANY TERTIARY STUDY THAT WAS DONE BEFORE THE REFERENCE DATE;
******************************************************;

**Get all tertiary enrolment records;
%let first_anal_yr=2006;
%let last_anal_yr=2015;

proc sql;
	create table enrolt as
		SELECT distinct 
			snz_uid
			,moe_enr_year_nbr as year
			,input(moe_enr_prog_start_date,yymmdd10.) format date9.  as startdate
			,input(moe_enr_prog_end_date,yymmdd10.) format date9.  as enddate  
			,sum(moe_enr_efts_consumed_nbr) as EFTS_consumed
			,moe_enr_efts_prog_years_nbr as EFTS_prog_yrs
			,moe_enr_qacc_code as qacc
			,moe_enr_qual_code as Qual
			,moe_enr_prog_nzsced_code as NZSCED
			,moe_enr_funding_srce_code as fund_source
			,moe_enr_subsector_code as subsector format $subsector.
			,moe_enr_qual_level_code as level
			,moe_enr_qual_type_code as qual_type
		FROM moe.enrolment 
			WHERE snz_uid IN 
				(SELECT DISTINCT snz_uid FROM ypypp.mth_ypypp) and moe_enr_year_nbr>=&first_anal_yr and moe_enr_year_nbr<=&last_anal_yr 
					group by snz_uid, moe_enr_prog_start_date, moe_enr_prog_end_date, qual, NZSCED
						order by snz_uid;
quit;

data enrol_clean_formal;
	set enrolt;
	if EFTS_consumed>0;
	dur=enddate-startdate;
	if dur>0;
	start_year=year(startdate);
	if start_year>=&first_anal_yr and start_year<=&last_anal_yr;
	if qual_type="D" then Formal=1;
	if formal=1 then output;
run;

proc sql;
	create table enrol_1 as
		select 
			a.refmth,
			a.startdate as ysstartdate,
			b.*
		from ypypp.mth_ypypp a inner join enrol_clean_formal b
			on a.snz_uid=b.snz_uid
			where b.startdate<a.startdate
			order by snz_uid, refmth, startdate;
quit;

* Formating dates and creating clean enrolment file;
* Defining formal and informal enrolments;
data enrol_clean_formal2;
	set enrol_1;
	if NZSCED >'120000' then
		genskills_prog=1;
	else genskills_prog=0;
	if NZSCED <'120000' then
		occskills_prog=1;
	else occskills_prog=0;
run;

proc summary data=enrol_clean_formal2 nway;
	class snz_uid refmth;
	var genskills_prog occskills_prog;
	output out=stats sum=;
run;

** Create variables indicating the types of tertiary programmes undertaken - general (mixed) skills or occupational skills or both;
data ypypp.tert_prog_preYPYPP(compress=y keep=snz_uid refmth genskills_only occskills_only gen_n_occ_skills);
	set stats;
	if genskills_prog>=1 then
		genskills_prog=1;
	else genskills_prog=0;
	if occskills_prog>=1 then
		occskills_prog=1;
	else occskills_prog=0;
	if genskills_prog=1 and occskills_prog=0 then
		genskills_only=1;
	else genskills_only=0;
	if genskills_prog=1 and occskills_prog=1 then
		gen_n_occ_skills=1;
	else gen_n_occ_skills=0;
	if genskills_prog=0 and occskills_prog=1 then
		occskills_only=1;
	else occskills_only=0;
run;

**Place of residence at end of the month immediately before the reference date;
**For those with a reference month of 161=Aug 2012, we will get their address as at the end of July 2012, for example;
data lastmth(keep=snz_uid refmth nzdep tla reg);
	merge ypypp.mth_ypypp(in=a keep=snz_uid refmth startdate) ys.residence /* Residence dataset from YSNEET work */;
	by snz_uid;
	if a;
	array nzdepV(*) nzdep105-nzdep195;
	array tlaV(*) ta105-ta195;
	array regV(*) reg105-reg195;
	nzdep=nzdepV(refmth-104);
	tla=tlaV(refmth-104);
	reg=regV(refmth-104);
run;

/*proc freq data=lastmth;
	tables nzdep tla reg  /list missing;
run;*/

proc sort data=lastmth out=ypypp.lastmth_ypypp;
	by snz_uid refmth;
run;

***********************************************;
**NCEA credits and NCEA qualifications awarded;
**As at the end of the year before the reference date;
**Any tertiary qualifications that were awarded before the reference date;
***********************************************;
proc sort data=ys.ncea_credit_sum;
	by snz_uid;
run;

data quals(keep=snz_uid refmth hqual hothqual hterqual ncea_cr_l1-ncea_cr_l3 prop_cr_ext_cat );
	merge ypypp.mth_ypypp(in=a keep=snz_uid refmth startdate startyear) ys.highest_quals_achieved /* Highest qual dataset from YSNEET work */
		ys.ncea_credit_sum /* NCEA credits data from YSNEET work */;
	by snz_uid;
	if a;

	** Split qualifications by NCEA / non-NCEA school / tertiary / industry training;
	array hncea(*) hncea_2006-hncea_2014;
	array hnonncea(*) hnonncea_2006-hnonncea_2014;
    array hterqual_(*) hterqual_2006-hterqual_2014;
	array hitqual(*) hitqual_2006-hitqual_2014;
	array tot_ncea_cr_L1(*) tot_ncea_cr_L1_by_2007-tot_ncea_cr_L1_by_2014;
	array tot_ncea_cr_L2(*) tot_ncea_cr_L2_by_2007-tot_ncea_cr_L2_by_2014;
	array tot_ncea_cr_L3(*) tot_ncea_cr_L3_by_2007-tot_ncea_cr_L3_by_2014;
	array prop_cr_ext(*) prop_cr_ext_by_2007_cat prop_cr_ext_by_2008_cat prop_cr_ext_by_2009_cat prop_cr_ext_by_2010_cat prop_cr_ext_by_2011_cat prop_cr_ext_by_2012_cat prop_cr_ext_by_2013_cat prop_cr_ext_by_2014_cat;

	if startyear>2006 then do;
		hqual=hncea(startyear-2006);
		hothqual=hnonncea(startyear-2006);
		hterqual=max(of hterqual_(startyear-2006), hitqual(startyear-2006));
	end;
	if startyear>2007 then do;
		ncea_cr_l1=tot_ncea_cr_L1(startyear-2007);
		ncea_cr_l2=tot_ncea_cr_L2(startyear-2007);
		ncea_cr_l3=tot_ncea_cr_L3(startyear-2007);
		prop_cr_ext_cat=prop_cr_ext(startyear-2007);
	end;

	array quals(*) hqual hothqual hterqual ncea_cr_l1-ncea_cr_l3;

	do i=1 to dim(quals);
		if quals(i)=. then
			quals(i)=0;
	end;
run;

proc sort data=quals out=ypypp.pre_YPYPP_quals;
	by snz_uid refmth;
run;

****************************************;
**Monthly employment, enrolment history, benefit receipt history and inactivity history prior to the refmth;
****************************************;

* Set reference months for school, tertiary, employment, benefit, overseas, industry, student allowances;
%let k=58;
%let m=82;
%let p=189;
%let z=195;

data ypypp.monthly(drop=sch_enr_id_&k-sch_enr_id_&z rearn&k-rearn&z ter_enr_id_&k-ter_enr_id_&z emp&k-emp&z ben_id_&k-ben_id_&z os_da_&k-os_da_&z
						sa&k-sa&z itl_id_&k-itl_id_&z);
	merge ypypp.mth_ypypp(in=a keep=snz_uid refmth startdate startyear part_type participant strata rate_code) 
		ys.mth_sch_enrol(in=b keep=snz_uid sch_enr_id_&k-sch_enr_id_&z)			/*58 to 195 */ /*monthly activity datasets from the YSNEET work */
		ys.mth_ter_enrol(in=c keep=snz_uid ter_enr_id_&m-ter_enr_id_&p)        	/*82 to 189 */
		ys.mth_emp(in=d keep=snz_uid emp&m-emp&z rearn&m-rearn&z)				/*82 to 195 */
		ys.mth_it_enrol(in=e keep=snz_uid itl_id_&m-itl_id_&z)					/*82 to 195 */
		ys.mth_ben(in=f keep=snz_uid ben_id_&m-ben_id_&z)						/*82 to 195 */
        ys.mth_os(in=g keep=snz_uid os_da_&k-os_da_&z)							/*58 to 195*/
        ys.mth_studall(in=h keep=snz_uid sa&m.-sa&z.)							/*82 to 195*/
		;
	by snz_uid;
	if a;
	
	** Monthly arrays for variables by reference month;
	array sch(*) sch_enr_id_&k-sch_enr_id_&z;
	array emp(*) emp&k-emp&z;
	array tert(*) ter_enr_id_&k-ter_enr_id_&z;
	array it(*)  itl_id_&k-itl_id_&z;
	array ben(*) ben_id_&k-ben_id_&z;
	array earn(*) rearn&k-rearn&z;
	array os(*) os_da_&k-os_da_&z;
	array sa(*) sa&k-sa&z;

	** Set the indicators to zero to start with;
	if not b then do i=&k. to &z.;
		sch{i-57}=0;
	end;
	if not c then do i=&m. to &p.;
		tert{i-57}=0;
	end;
	if not d then do i=&m. to &z.;
		emp{i-57}=0;
	end;
	if not e then do i=&m. to &z.;
		it{i-57}=0;
	end;
	if not f then do i=&m. to &z.;
		ben{i-57}=0;
	end;
	if not g then do i=&k. to &z.;
		os{i-57}=0;
	end;
	if not h then do i=&m. to &z.;
		sa{i-57}=0;
	end;

	** Monthly arrays for variables by participation period - 1-48 are 48 pre-participation month, 49 is the month of participation start
	** months 50-79 are the 30 months post-participantion;
	array nsch(*) sch1-sch79;
	array nemp(*) nemp1-nemp79;
	array ntert(*) tert1-tert79;
	array nit(*) it1-it79;
	array nben(*) ben1-ben79;
	array nearn(*) earn1-earn79;
    array nos(*) nos1-nos79;
	array nsa(*) nsa1-nsa79;

	array study(*) study1-study79;
	array neet(*) neet1-neet79;
	array eet(*) eet1-eet79;

	**Select the data for the 48 months before the refmth, the refmth and the next 30 months;
	windowstart_leed=refmth-48;

	do i=1 to 79;
		if 1 <= i+(windowstart_leed-57)-1 <= 138 then do;
			nemp(i)=emp(i+(windowstart_leed-57)-1); 
			nearn(i)=earn(i+(windowstart_leed-57)-1); 
			nsch(i)=sch(i+(windowstart_leed-57)-1); 
			nben(i)=ben(i+(windowstart_leed-57)-1); 
			nos(i)=os(i+(windowstart_leed-57)-1); 
			if nos(i)>=7 then nos(i)=1; * longer than a week overseas that month; 
				else nos(i)=0;
			ntert(i)=tert(i+(windowstart_leed-57)-1); 
			nit(i)=it(i+(windowstart_leed-57)-1);
			nsa(i)=sa(i+(windowstart_leed-57)-1);
			if (nsch(i)=. or ntert(i)=.) then study(i)=.;
			else if (nsch(i)>0 or ntert(i)>0) then study(i)=1;
			    else study(i)=0;
			if (nsch(i)=. or ntert(i)=. or nemp(i)=.) then eet(i)=.;
			else if (nsch(i)>0 or ntert(i)>0 or nemp(i)>0) then eet(i)=1;
			  	else eet(i)=0;
			if (nsch(i)=. or ntert(i)=. or nemp(i)=. or nos(i)=.) then neet(i)=.;
			else if (nsch(i)=0 and ntert(i)=0 and nemp(i)=0 and nos(i)=0) then neet(i)=1;
				else neet(i)=0;
		end;
	end;
run;

**Vectors showing whether any custodial or community sentence was served in each calendar month;
data ypypp.monthly_corrections(compress=y drop= i j windowstart_leed avail_mths
 								cust_id_&m-cust_id_&z comm_id_&m-comm_id_&z);
	merge ypypp.mth_ypypp(in=a keep=snz_uid refmth strata startdate) 
 	      ys.mth_corrections(keep=snz_uid cust_id_&m-cust_id_&z comm_id_&m-comm_id_&z) /* monthly corrections data from YSNEET analysis */;
	by snz_uid ;
	if a;
	array corr [2,114] cust_id_&m-cust_id_&z  comm_id_&m-comm_id_&z;  
	array ncorr [2,79] cust1-cust79 comm1-comm79;
	windowstart_leed=refmth-48;
	avail_mths=79;
   	do i=1 to 12;
		if refmth=(i+177) then avail_mths=(avail_mths-i);
   	end;
	do j=1 to 2;
		do i=1 to avail_mths;
			if 1 <= i+(windowstart_leed-81-1) <= &z.-&m.+1 then do; 
    			ncorr(j,i)=corr(j,i+(windowstart_leed-81)-1);
				if ncorr(j,i)=. then ncorr(j,i)=0;
			end;
  		end;
  	end;
run;

** Construct summary measures of activities over the last 6 months pre-participation, the prior 6 months, and the 12 months before that (for matching)
** and the 18 months before participation (for descriptive results - consistent with YSNEET work);
data ypypp.pre_ypypp_monthly(keep=snz_uid refmth startdate startyear 
								mths_sch_last6 mths_sch_prior6 mths_sch_first12 mths_sch_last18
								mths_emp_last6 mths_emp_prior6 mths_emp_first12 mths_emp_last18
								mths_tert_last6 mths_tert_prior6 mths_tert_first12 mths_tert_last18
								mths_neet_last6 mths_neet_prior6 mths_neet_first12 mths_neet_last18
								mths_ben_last6 mths_ben_prior6 mths_ben_first12 mths_ben_last18
							    mean_earn_last6);
	set ypypp.monthly(rename=(nemp25-nemp79=emp25-emp79));
	mths_sch_last6=sum(of sch43-sch48);
	mths_emp_last6=sum(of emp43-emp48);
	mths_tert_last6=sum(of tert43-tert48);
	mths_neet_last6=sum(of neet43-neet48);
	mths_ben_last6=sum(of ben43-ben48);
	mths_sch_prior6=sum(of sch37-sch42);
	mths_emp_prior6=sum(of emp37-emp42);
	mths_tert_prior6=sum(of tert37-tert42);
	mths_neet_prior6=sum(of neet37-neet42);
	mths_ben_prior6=sum(of ben37-ben42);
	mths_sch_first12=sum(of sch25-sch36);
	mths_emp_first12=sum(of emp25-emp36);
	mths_tert_first12=sum(of tert25-tert36);
	mths_neet_first12=sum(of neet25-neet36);
	mths_ben_first12=sum(of ben25-ben36);
    mths_sch_last18=sum(of sch31-sch48);
	mths_emp_last18=sum(of emp31-emp48);
	mths_tert_last18=sum(of tert31-tert48);
	mths_neet_last18=sum(of neet31-neet48);
	mths_ben_last18=sum(of ben31-ben48);

	mean_earn_last6=mean(of earn43-earn48);
run;

*********************************************************;
**COMPILE OTHER EXPLANATORY VARIABLES - annual in construction;
**USE VARIABLES COVERING EACH YEAR OF AGE UP TO THE BIRTHDAY BEFORE THE REFERENCE DATE;
*********************************************************;
***Compile data on life histories by year of age;
**Divide sample into 4 subgroups - according to whether I want to summarise the data for ages 0-14, 0-15, 0-16 or 0-17;

%let firstage=0;
%let lastage=17;

data annual;
	merge ypypp.mth_ypypp(in=a keep=snz_uid refmth startdate age age_at_start
		dob female european maori pacific  asian ethoth ethmiss cohort) 
		project._ind_cyf_at_age_&version(keep=snz_uid 
											child_any_fdgs_abuse_at_age_&firstage-Child_any_fdgs_abuse_at_age_&lastage
											child_not_at_age_&firstage-child_not_at_age_&lastage
											child_Pol_FV_not_at_age_&firstage-child_Pol_FV_not_at_age_&lastage
											child_YJ_referral_at_age_&firstage-child_YJ_referral_at_age_&lastage
											child_CYF_place_at_age_&firstage-child_CYF_place_at_age_&lastage
											child_YJ_place_at_age_&firstage-child_YJ_place_at_age_&lastage)
		project._ind_cg_corr_at_age_&version(keep=snz_uid cg_comm_at_age_&firstage-cg_comm_at_age_&lastage 
											cg_cust_at_age_&firstage-cg_cust_at_age_&lastage)
		project._ind_courts_at_age_&version(keep=snz_uid YC_appearances_at_age_0-YC_appearances_at_age_&lastage
											proven_charges_at_age_0-proven_charges_at_age_&lastage)
		project._ind_bdd_child_at_age_&version(keep=snz_uid ch_total_da_onben_at_age_0-ch_total_da_onben_at_age_&lastage ch_da_SPSR: )
		project._ind_primhd_at_age_&version(keep=snz_uid all_act_at_age_0-all_act_at_age_&lastage)	
		project._ind_os_spells_at_age_&version(keep=snz_uid os_da:)
		project._ind_interven_at_age_&version(keep=snz_uid trua_da_at_age_0-trua_da_at_age_&lastage   
											susp_da_at_age_0-susp_da_at_age_&lastage sedu_da_at_age_0-sedu_da_at_age_&lastage
											stand_da_at_age_0-stand_da_at_age_&lastage)
		project.mat_educ_comb_&version(keep=snz_uid maternal_edu_at_age_0-maternal_edu_at_age_19);
	by snz_uid;

	if a;
	array mother1(*) maternal_edu_at_age_0-maternal_edu_at_age_19;
	array mother2(*) mother_unqual_at_age_0-mother_unqual_at_age_19;

	do i=1 to dim(mother1);
		if mother1(i)=0 then
			mother2(i)=1;
		else mother2(i)=0;
	end;

	array comm(*) cg_comm_at_age_&firstage-cg_comm_at_age_&lastage;
	array cust(*) cg_cust_at_age_&firstage-cg_cust_at_age_&lastage;
	array corr(*) cg_corr_at_age_&firstage-cg_corr_at_age_&lastage;

	do i=1 to dim(comm);
		if comm(i)=1 or cust(i)=1 then
			corr(i)=1;
		else corr(i)=0;
	end;

	model_age=age_at_start-1;
run;

%macro sumbyage (prefix,lower_age1,to_age,ind);
	%if &ind=1 %then
		X_&prefix.=sum(of &prefix._at_age_&lower_age1.-&prefix._at_age_&to_age.)>0;
	%else X_&prefix._sum=sum(of &prefix._at_age_&lower_age1.-&prefix._at_age_&to_age.);;
%mend;

proc format;
	value bendur
		.,low-0='anone  '
		0<-.10='b1-9%'
		.10<-.25='c10-24%'
		.25<-.50='d25-49%'
		.50<-.75='e50-74%'
		.75<-high='f75+%';

proc format;
	value osdur
		.,low-<.10='a<10%  '
		.10-<.50='b10-<50%'
		.50-<.74999='c50-<75%'
		.74999<-high='d75%+ ';
run;

proc format;
   value nbrnot
       .,low-0='aNone '
       1-2='b1-2'
	   3-9='c3-9'
	   10-high='d10+';
run;

%macro compile2(lower_age1,to_age,curr_age);
	data grp&curr_age(keep=snz_uid
		dob
		refmth
		startdate
		x_age
		x_female
		x_european
		x_maori
		x_pacific
		x_asian
		x_ethoth
		x_ethmiss
		x_cohort

		x_ch_total_da_onben_sum
		X_prop_onben_aschild_cat
		X_onben50_aschild
		X_onben75_aschild
		X_ever_onben_aschild
		x_ch_da_SPSR
		x_child_bentype
		x_years_onben_as_child

		x_os_da_sum
		x_prop_os_aschild_cat
		x_years_os_as_child

		x_mother_unqual
		X_cg_cust
		X_cg_comm
		x_cg_corr

		X_child_not
		X_child_not_cat
		X_child_yj_referral
		X_child_yj_referral_cat
		X_child_cyf_place
		X_child_yj_place
		X_child_any_fdgs_abuse
		x_all_act

		x_sedu_da
		x_trua_da
		x_susp_da_cat
		x_stand_da_cat);
		set annual;

		if model_age=&curr_age;
		x_age=age;
		x_female=female;
		x_european=european;
		x_maori=maori;
		x_pacific=pacific;
		x_asian=asian;
		x_ethoth=ethoth;
		x_ethmiss=ethmiss;
		x_cohort=cohort;
		x_mother_unqual=mother_unqual_at_age_&to_age;

		%sumbyage(child_not,&lower_age1,&to_age,1);
	        %sumbyage(child_not,&lower_age1,&to_age,0);
	        X_child_not_cat = put(X_child_not_sum,nbrnot.); 
		%sumbyage(child_any_fdgs_abuse,&lower_age1,&to_age,1);
		%sumbyage(child_CYF_place,&lower_age1,&to_age,1);
		%sumbyage(child_Pol_FV_not,&lower_age1,&to_age,1);
		%sumbyage(cg_comm,&lower_age1,&to_age,1);
		%sumbyage(cg_cust,&lower_age1,&to_age,1);
		%sumbyage(cg_corr,&lower_age1,&to_age,1);
		%sumbyage(ch_total_da_onben,&lower_age1,&to_age,0);
		currentdate=intnx('YEAR',dob,&to_age+1,'S')-1;
		days_since_birth=currentdate-dob;
		X_prop_onben_aschild = (X_ch_total_da_onben_sum) /days_since_birth;
		X_prop_onben_aschild_cat=put(X_prop_onben_aschild,bendur.);
		X_onben50_aschild = (X_ch_total_da_onben_sum /days_since_birth) >.50;
		X_onben75_aschild = (X_ch_total_da_onben_sum /days_since_birth) >.75;
		X_years_onben_as_child =round((X_ch_total_da_onben_sum/365),0.1);
		X_ever_onben_aschild=X_ch_total_da_onben_sum>0;

		%sumbyage(ch_da_SPSR,&lower_age1,&to_age,1);

		if X_ch_da_SPSR then
			X_child_bentype='SPSR ';
		else if X_ch_total_da_onben_sum>0 then
			X_child_bentype='Other';
		else  X_child_bentype='None ';
		%sumbyage(sedu_da,&lower_age1,&to_age,1);
		%sumbyage(trua_da,&lower_age1,&to_age,1);
		%sumbyage(stand_da,&lower_age1,&to_age,1);
		    %sumbyage(stand_da,&lower_age1,&to_age,0);
	        X_stand_da_cat = put(X_stand_da_sum,nbrnot.);
		%sumbyage(susp_da,&lower_age1,&to_age,1);
            %sumbyage(susp_da,&lower_age1,&to_age,0);
            X_susp_da_cat = put(X_susp_da_sum,nbrnot.);
		%sumbyage(child_YJ_referral,&lower_age1,&to_age,1);
	        %sumbyage(child_YJ_referral,&lower_age1,&to_age,0);
            X_child_YJ_referral_cat = put(X_child_YJ_referral_sum,nbrnot.);
		%sumbyage(YC_appearances,&lower_age1,&to_age,1);
		%sumbyage(child_YJ_place,&lower_age1,&to_age,1);

		%sumbyage(all_act,&lower_age1,&to_age,1);
		%sumbyage(os_da,&lower_age1,&to_age,0);
		X_prop_os_aschild=(X_os_da_sum) /days_since_birth;
		X_prop_os_aschild_cat=put(X_prop_os_aschild, osdur.);
		X_years_os_as_child =round((X_os_da_sum/365),0.1);
	run;

%mend compile2;

%compile2(0,14,14);
%compile2(0,15,15);
%compile2(0,16,16);
%compile2(0,17,17);

data annual2;
	set grp14 grp15 grp16 grp17;
run;

proc sort data=annual2 out=ypypp.annual;
	by snz_uid refmth;
run;

**********************************************************************************;
** COMPILE SUMMARY DATASETS TO USE IN PS MODELS or descriptive statistics;
*********************************************************************;

proc format;
	value mth6m
		0=0
		1-3=1
		4-6=2;

	value mth12m
		0=0
		1-3=1
		4-6=2
		7-9=3
		10-12=4;
	value nceacred
		.,0-4=1
		5-49=2
		50-59=3
		60-79=4
		80-high=5;
 value schgap
   .='hNA'
   0,1='a<2mths'
   2-3='b2-3mths'
   4-6='c4-6mths'
   7-12='d7-12mths'
   13-24='e1-2yrs'
   25-36='f2-3yrs'
   37-high='g>3yrs';

value nschool
1,2=2
3=3
4=4
5=5
6-high=6;
run;

**Start combining the source data for the model variables and then convert some continuous variables into categorial;
**Also collapse some variable categories that are rare for people in the study population;

data modelb(keep=snz_uid refmth startdate startyear partner age_young_chd num_child benefittype benefitname prev_ben child_birthdt rate_code part_type participant strata study_start mths_ben_to_ys
	x_cohort age age_det x_female x_european x_maori x_pacific x_asian x_ethoth x_ethmiss x_nzdep x_tla x_region x_decile x_schtype x_schauth
	ncea_cr_l1-ncea_cr_l3 prop_cr_ext_cat
	x_hqual x_hothqual x_hterqual x_ncea_cr_l1_cat x_ncea_cr_l2_cat x_ncea_cr_l3_cat nschools x_nsch_cat months_since_last_school 
     time_since_school age_when_left_sch
	x_sch_last6 x_emp_last6 x_tert_last6 x_neet_last6 x_ben_last6
	x_sch_prior6 x_emp_prior6 x_tert_prior6 x_neet_prior6 x_ben_prior6
	x_sch_first12 x_emp_first12 x_tert_first12 x_neet_first12 x_ben_first12
	X_prop_onben_aschild_cat  X_ever_onben_aschild  x_child_bentype 
	x_prop_os_aschild_cat
	x_mother_unqual X_cg_cust X_cg_comm x_cg_corr
	X_child_not X_child_yj_referral X_child_cyf_place X_child_yj_place X_child_any_fdgs_abuse x_all_act
	x_sedu_da x_trua_da x_susp_da x_stand_da
    genskills_only occskills_only gen_n_occ_skills
	X_child_not_cat X_child_yj_referral_cat X_susp_da_cat X_stand_da_cat
    mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 mean_earn_last6);
	merge ypypp.mth_ypypp(in=a) 
		ypypp.lastsch_ypiyb
		ypypp.nschools_ypypp
		ypypp.lastmth_ypypp
		ypypp.pre_ypypp_quals
		ypypp.pre_ypypp_monthly
		ypypp.annual
        ypypp.tert_prog_preYPYPP;
	by snz_uid refmth;
	if a;
	** Convert historical activity variables into categories as defined in the formats created earlier;
	x_sch_last6=(put(mths_sch_last6,mth6m.))*1;
	x_emp_last6=(put(mths_emp_last6,mth6m.))*1;
	x_tert_last6=(put(mths_tert_last6,mth6m.))*1;
	x_neet_last6=(put(mths_neet_last6,mth6m.))*1;
	x_ben_last6=(put(mths_ben_last6,mth6m.))*1;
	x_sch_prior6=(put(mths_sch_prior6,mth6m.))*1;
	x_emp_prior6=(put(mths_emp_prior6,mth6m.))*1;
	x_tert_prior6=(put(mths_tert_prior6,mth6m.))*1;
	x_neet_prior6=(put(mths_neet_prior6,mth6m.))*1;
	x_ben_prior6=(put(mths_ben_prior6,mth6m.))*1;
	x_sch_first12=(put(mths_sch_first12,mth12m.))*1;
	x_emp_first12=(put(mths_emp_first12,mth12m.))*1;
	x_tert_first12=(put(mths_tert_first12,mth12m.))*1;
	x_neet_first12=(put(mths_neet_first12,mth12m.))*1;
	x_ben_first12=(put(mths_ben_first12,mth12m.))*1;
	x_hqual=hqual;
    if hqual=. then x_hqual=0;
	**create a new category of the highest qual variable for people whose highest
	   qual is not an ncea qualification;
	if hothqual>hqual or hterqual>hqual then x_hqual=6; 
    if hothqual>0 then x_hothqual=1; else x_hothqual=0; 
	if hterqual>0 then x_hterqual=1; else x_hterqual=0; 
	array credits(*) ncea_cr_l1-ncea_cr_l3;
	do i=1 to 3;
		if credits(i)=. then
			credits(i)=0;
	end;
	x_ncea_cr_l1_cat=(put(ncea_cr_l1,nceacred.))*1;
	x_ncea_cr_l2_cat=(put(ncea_cr_l2,nceacred.))*1;
	x_ncea_cr_l3_cat=(put(ncea_cr_l3,nceacred.))*1;
	x_decile=decile;

	if decile in (., 99) then
		x_decile=99;
	x_schtype=schtype;
	x_schauth=school_authority;
    if months_since_last_school~=. then do;
      time_since_school=put(months_since_last_school,schgap.);
      end;
    x_nsch_cat=(put(nschools,nschool.))*1;
	x_nzdep=nzdep;

	if nzdep in (., 99) then
		x_nzdep=99;
	x_region=reg;

	if reg=. then
		x_region=99;
	x_tla=tla;

	if tla=. then
		x_tla=99;

	if x_schtype='Special' then
		x_schtype='Other';

	if x_schauth='Private' then
		x_schauth='Other';

	if x_ethmiss=1 then
		x_ethoth=1;
  if genskills_only=. then genskills_only=0;
  if occskills_only=. then occskills_only=0;
  if gen_n_occ_skills=. then gen_n_occ_skills=0;
  if nschools=. then nschools=0;
run;

/*proc freq data=ypypp.mth_ypypp;
	tables participant*part_type/missing;
run;

proc freq data=modelb;
	tables participant*part_type/missing;
run;*/

**Rename vars - get rid of Xs;
**Create more aggregated versions of the age and region variables;

data ypypp.model_final(compress=y keep=snz_uid refmth startdate startyear benefittype benefitname prev_ben child_birthdt rate_code  partner age_young_chd num_child part_type participant part_type_part strata study_start mths_ben_to_ys
	cohort age age2 age_det female european maori pacific asian ethoth ethmiss nzdep tla region region2 region3 decile x_schtype x_schauth
	hqual hothqual hterqual ncea_cr_l1-ncea_cr_l3 prop_cr_ext_cat
    x_ncea_cr_l1_cat x_ncea_cr_l2_cat x_ncea_cr_l3_cat nschools nsch_cat time_since_school age_when_left_sch
	sch_last6 emp_last6 tert_last6 neet_last6 ben_last6
	sch_prior6 emp_prior6 tert_prior6 neet_prior6 ben_prior6
	sch_first12 emp_first12 tert_first12 neet_first12 ben_first12
	genskills_only occskills_only gen_n_occ_skills 
	x_prop_onben_aschild_cat  ever_onben_aschild  x_child_bentype 
	x_prop_os_aschild_cat
	mother_unqual cg_cust cg_comm cg_corr
	child_not child_yj_referral child_cyf_place child_yj_place child_any_fdgs_abuse all_act
	sedu_da trua_da susp_da stand_da
	mths_sch_last18 mths_emp_last18 mths_tert_last18 mths_neet_last18 mths_ben_last18 mean_earn_last6
	X_child_not_cat X_child_yj_referral_cat X_susp_da_cat X_stand_da_cat
	rename=(x_schtype=schtype x_schauth=schauth X_prop_onben_aschild_cat=prop_onben_aschild_cat   
	    x_child_bentype=child_bentype x_prop_os_aschild_cat=prop_os_aschild_cat
	    x_ncea_cr_l1_cat=ncea_cr_l1_cat   x_ncea_cr_l2_cat=ncea_cr_l2_cat   x_ncea_cr_l3_cat=ncea_cr_l3_cat  
		X_child_not_cat=child_not_cat  X_child_yj_referral_cat=child_yj_referral_cat
        X_susp_da_cat=susp_da_cat X_stand_da_cat=stand_da_cat
    ));
	length part_type_part $ 8;
	set modelb;
	if participant=1 then part_type_part=part_type||" Post";
	else if participant=0 then part_type_part=part_type||" Pre";
	array old(*) x_cohort x_female x_european x_maori x_pacific x_asian x_ethoth x_ethmiss x_nzdep x_tla x_region x_decile /*x_schtype x_schauth*/
	    x_hqual x_hothqual x_hterqual x_nsch_cat
		x_sch_last6 x_emp_last6 x_tert_last6 x_neet_last6 x_ben_last6
		x_sch_prior6 x_emp_prior6 x_tert_prior6 x_neet_prior6 x_ben_prior6
		x_sch_first12 x_emp_first12 x_tert_first12 x_neet_first12 x_ben_first12
	    X_ever_onben_aschild 
	    x_mother_unqual X_cg_cust X_cg_comm x_cg_corr
		X_child_not X_child_yj_referral X_child_cyf_place X_child_yj_place X_child_any_fdgs_abuse x_all_act
		x_sedu_da x_trua_da x_susp_da x_stand_da;
	array new(*) cohort female european maori pacific asian ethoth ethmiss nzdep tla region decile /*schtype schauth*/
	    hqual hothqual hterqual nsch_cat
		sch_last6 emp_last6 tert_last6 neet_last6 ben_last6
		sch_prior6 emp_prior6 tert_prior6 neet_prior6 ben_prior6
		sch_first12 emp_first12 tert_first12 neet_first12 ben_first12
	    ever_onben_aschild  
	    mother_unqual cg_cust cg_comm cg_corr
		child_not child_yj_referral child_cyf_place child_yj_place child_any_fdgs_abuse all_act
		sedu_da trua_da susp_da stand_da;
	do i=1 to dim(old);
		new(i)=old(i);
	end;
	**Aggregating some region and age categories to assist with better exact matching;
	region2=region;
	if region in (5,6,7,8) then
		region2=10;
	*rest of north island;
	if region in (12,13,14,15,16,17,18,99) then
		region2=19;
	*rest of south island;
	region3=region;
	if region in (7,12,14,15,16,17,18,99) then
		region3=19;
	*Taranaki and rest of south island;
	age2=age;
	if age=15 then
		age2=16;
	if age=18 then
		age2=17;
    if time_since_school='   ' then time_since_school='hNA';
run;

/*proc freq data=ypypp.model_final;
	tables part_type_part*age_when_left_sch/nopercent nocol;
run;*/

****************************************************;
** Construct post-YS OUTCOME VARIABLES for everyone in study and potential controls;
** Using monthly data on enrolment status, employment, benefit receipt and NEET status
** and annual data on educational achievement;
*******************************************************;

** Set months of first outcome period (6 months post-particiption), next outcome period (12 months), etc. up to 30 months post-participation;
%let m=55;
%let n=61;
%let o=67;
%let p=73;
%let q=79;

data depvars(compress=y keep=snz_uid refmth strata startdate startyear additional_service_data part_type participant prev_ben child_birthdt rate_code 
				dob emp_post6 emp_post12 emp_post18 emp_post24 emp_post30 study_post6 study_post12 study_post18 study_post24 neet_post6 neet_post12 neet_post18 neet_post24  
			   	eet_post6 eet_post12 eet_post18 eet_post24 ben_post6 ben_post12 ben_post18 ben_post24 ben_post30 tert_post6 tert_post12 tert_post18 tert_post24 
				cust_post6 cust_post12 cust_post18 cust_post24 cust_post30 comm_post6 comm_post12 comm_post18 comm_post24 comm_post30 cust_in_followup comm_in_followup);
	merge ypypp.monthly(in=a) 
		  ypypp.mth_ypypp(keep=snz_uid refmth dob additional_service_data part_type participant strata prev_ben child_birthdt rate_code )      
		  ypypp.monthly_corrections(keep=snz_uid refmth cust49-cust79 comm49-comm79);
 	by snz_uid refmth;
  	if a;
  	array base(*) nemp&m nemp&n nemp&o nemp&p nemp&q study&m study&n study&o study&p
   	        	neet&m neet&n neet&o neet&p eet&m eet&n eet&o eet&p ben&m ben&n ben&o ben&p ben&q tert&m tert&n tert&o tert&p
        		cust&m cust&n cust&o cust&p cust&q comm&m comm&n comm&o comm&p comm&q;
 	array outcome(*) emp_post6 emp_post12 emp_post18 emp_post24 emp_post30 study_post6 study_post12 study_post18 study_post24
               	neet_post6 neet_post12 neet_post18 neet_post24 eet_post6 eet_post12 eet_post18 eet_post24 ben_post6 ben_post12 ben_post18 ben_post24 ben_post30
               	tert_post6 tert_post12 tert_post18 tert_post24 cust_post6 cust_post12 cust_post18 cust_post24 cust_post30 comm_post6 comm_post12 comm_post18 comm_post24 comm_post30;
  	do i=1 to dim(base);
   	  outcome(i)=base(i);
  	end;
	** Create indicators of any community or custodial sentence in the outcome period; 
	if sum(of cust50-cust79)>0 then cust_in_followup=1; else cust_in_followup=0;
	if sum(of comm50-comm79)>0 then comm_in_followup=1; else comm_in_followup=0;
run;

** Bring together all of the outcomes variables of interest - note many of these were used in the YSNEET analysis but arent being used for YP YPP - especially those related to outcomes at 18th birthday;
data ypypp.depvars(compress=y drop=i sch_enr_id_106-sch_enr_id_189 ter_enr_id_106-ter_enr_id_189 
									emp106-emp189 ben_id_106-ben_id_195 hqual_2006-hqual_2014 hncea_2006-hncea_2014
									hnonncea_2006-hnonncea_2014 hterqual_2006-hterqual_2014 
								    emp_3mths_post sch_3mths_post tert_3mths_post);
	merge depvars(in=a)
		ys.mth_sch_enrol(keep=snz_uid sch_enr_id_106-sch_enr_id_189)
		ys.mth_ter_enrol(keep=snz_uid ter_enr_id_106-ter_enr_id_189)
		ys.mth_emp(keep=snz_uid emp106-emp189)  
		ys.mth_ben(keep=snz_uid ben_id_106-ben_id_195)
		ys.highest_quals_achieved(keep=snz_uid hncea_2006-hncea_2014 hnonncea_2006-hnonncea_2014 hterqual_2006-hterqual_2014 hqual_2006-hqual_2014)
        ys.mth_corrections(keep=snz_uid cust_id_106-cust_id_195 comm_id_106-comm_id_195);
	by snz_uid;
	if a;
	array highqual(*) hqual_2006-hqual_2014;
	do i=1 to dim(highqual);
		if highqual(i)=. then
			highqual(i)=0;
	end;
	bday_18=intnx('YEAR',dob,18,'S');
	format bday_18 date9.;
	year_18=year(bday_18);
	refmth_18=(12*(year(bday_18)-1999) + (month(bday_18) -3));
	refmth_17_75=refmth_18-3;

	**Highest qualifications completed, ordered by reference year where 0 =year before YS participation
		  and 1= year of YS participation;
	array old(4,8) hncea_2007-hncea_2014 hnonncea_2007-hnonncea_2014
	             hterqual_2007-hterqual_2014 hqual_2007-hqual_2014;
	array new(4,4) hncea_y0-hncea_y3 hnonncea_y0-hnonncea_y3
	             hterqual_y0-hterqual_y3 hqual_y0-hqual_y3;
	if 106 <= refmth < 118 then do j=1 to 4;
	             do i=1 to 4;
	           new(j,i)=old(j,i);
	           end;
			   end;
	else if 118 <= refmth < 130 then do j=1 to 4;
	             do i=1 to 4;
	           new(j,i)=old(j,i+1);
	           end;
			   end;
	else if 130 <= refmth < 142 then do j=1 to 4;
	             do i=1 to 4;
	           new(j,i)=old(j,i+2);
	           end;
			   end;
	else if 142 <= refmth < 154 then do j=1 to 4;
	             do i=1 to 4;
	           new(j,i)=old(j,i+3);
	           end;
			   end;
	else if 154 <= refmth < 166 then do j=1 to 4;
	             do i=1 to 4;
	           new(j,i)=old(j,i+4);
	           end;
			   end;
	else if 166 <= refmth < 178 then do j=1 to 4;
	             do i=1 to 3;
	           new(j,i)=old(j,i+5);
	           end;
			   end;
	**Indicators capturing whether person has a level 1 qualification at each time point, based on the above;
	array newind(4,4) i1hncea_y0-i1hncea_y3 i1hnonncea_y0-i1hnonncea_y3
	             i1hterqual_y0-i1hterqual_y3 i1hqual_y0-i1hqual_y3;
	    do j=1 to 4;
		do i=1 to 4;
		   if new(j,i)>=1 then newind(j,i)=1; 
	       else if new(j,i)~=. then newind(j,i)=0;
	     end;
		 end;
	**Indicators capturing whether person has a level 2 qualification at each time point, based on the above;
	array newindb(4,4) i2hncea_y0-i2hncea_y3 i2hnonncea_y0-i2hnonncea_y3
	             i2hterqual_y0-i2hterqual_y3 i2hqual_y0-i2hqual_y3;
	    do j=1 to 4;
		do i=1 to 4;
		   if new(j,i)>=2 then newindb(j,i)=1; 
	       else if new(j,i)~=. then newindb(j,i)=0;
	     end;
		 end;
	**Indicators capturing whether person has a level 3 or higher qualification at each time point, based on the above;
	array newindc(4,4) i3hncea_y0-i3hncea_y3 i3hnonncea_y0-i3hnonncea_y3
	             i3hterqual_y0-i3hterqual_y3 i3hqual_y0-i3hqual_y3;
    do j=1 to 4;
	do i=1 to 4;
	   if new(j,i)>=3 then newindc(j,i)=1; 
       else if new(j,i)~=. then newindc(j,i)=0;
     end;
	 end;

	**Create a marker variable to identify people who turned 18 at least 3 months after
		they enrolled in YS for the first time and have educ outcome data available;
	if refmth<refmth_17_75 and year_18<=2014 then
		lev2_yr18_avail=1;
	    else lev2_yr18_avail=0;

	*Highest qualification attained by end of year person turned 18;
	**Attainment data ends in 2014 so we can't observe this outcome for everyone yet;
	if lev2_yr18_avail=1 then
		do;
			lev2_yr18=0;
		end;

	do i=1 to dim(highqual);
		if i+2005=year_18 and highqual(i)>=2 and lev2_yr18_avail=1 then
			lev2_yr18=1;
	end;

	**Create a BEN variable Providing an indicator for receiving a benefit in any of the first 3 months after 18th birthday;

	**for people enrolled in YS at least 3 months before turning 18
	   and who also have benefit data for at least some of the 3 months after they
	    turned 18;
	if refmth<refmth_17_75 and (refmth_18+1<=195) then
		do;
			ben_3mths_aft_18=0;
			cust_3mths_aft_18=0;
			comm_3mths_aft_18=0;
		end;

	array ben(*) ben_id_106-ben_id_195;
	array cust(*) cust_id_106-cust_id_195;
    array comm(*) comm_id_106-comm_id_195;

	do i=1 to dim(ben);
		if refmth>refmth_17_75 and (refmth_18+1<=195)  
			and (i+105=(refmth_18+1) or i+105=(refmth_18+2) or i+105=(refmth_18+3)) 
			and ben(i)=1 then ben_3mths_aft_18=1;
        if refmth<refmth_17_75 and (refmth_18+1<=195)  
			and (i+105=(refmth_18+1) or i+105=(refmth_18+2) or i+105=(refmth_18+3)) 
			and cust(i)=1 then cust_3mths_aft_18=1;
        if refmth<refmth_17_75 and (refmth_18+1<=195)  
			and (i+105=(refmth_18+1) or i+105=(refmth_18+2) or i+105=(refmth_18+3)) 
			and comm(i)=1 then comm_3mths_aft_18=1;

	end;
	**Create a NEET variable to capture being NEET for all of the first 3 months after the month of turning 18,
	 for people who enrolled in YS at least 3 months before turning 18, and also have enrolment data for the 3 months after they
	    turned 18;
	if refmth<refmth_17_75 and (refmth_18+3<=189) then
		do;
			neet_3mths_aft_18=0;
		end;

	emp_3mths_post=0;
	sch_3mths_post=0;
	tert_3mths_post=0;
	array ter(*) ter_enr_id_106-ter_enr_id_189;
	array sch(*) sch_enr_id_106-sch_enr_id_189;
	array emp(*) emp106-emp189;

	do i=1 to dim(emp);
		if (i+105=(refmth_18+1) or i+105=(refmth_18+2) or i+105=(refmth_18+3)) 
			and (refmth_18+3<=189) then
			do;
				if ter(i)=1 then
					tert_3mths_post+1;

				if sch(i)=1 then
					sch_3mths_post+1;

				if emp(i)=1 then
					emp_3mths_post+1;
			end;
	end;
	if emp_3mths_post=0 and sch_3mths_post=0 and tert_3mths_post=0 and neet_3mths_aft_18~=. then
		do;
			neet_3mths_aft_18=1;
		end;
run;

** Additional outcomes measures re student allowances and earnings - for YP YPP we only use the student allowances ones;
%let m=55;
%let n=61;
%let o=67;
%let p=73;
%let q=79;

data ypypp.depvars_additional(keep=snz_uid refmth sa_post6 sa_post12 sa_post18 sa_post24 sa_post30 earn_post6 earn_post12 earn_post18 earn_post24 earn_post30);
	set ypypp.monthly(keep=snz_uid refmth nsa&m nsa&n nsa&o nsa&p nsa&q earn&m earn&n earn&o earn&p earn&q);
  array base(*) nsa&m nsa&n nsa&o nsa&p nsa&q earn&m earn&n earn&o earn&p earn&q;
  array outcome(*) sa_post6 sa_post12 sa_post18 sa_post24 sa_post30 earn_post6 earn_post12 earn_post18 earn_post24 earn_post30;
  do i=1 to dim(base);
     outcome(i)=base(i);
      end; 
run;

**************************************************************************************;
** APPLY FURTHER POPULATION RESTRICTIONS TO BE USED IN THE MAIN IMPACT ANALYSIS;
******************************************************************************;
**RESTRICTIONS FOR OVERSEAS TIME, non-NQF schools and missing school enrolment enddates - created here but not applied yet;

**Find records where person was overseas for a total of 6 months or longer during the 2 years immed before the reference month, 
  the reference month plus the main follow-up period of  24 months = 49 months;
**Note this follow-up window is censored for reference dates in 2014 and is a minimum of 6 months;

%let n=49;
data ypypp.not_os(keep=snz_uid refmth)
     ypypp.overseas_6mthsplus(keep=snz_uid refmth);
    merge ypypp.mth_ypypp(in=a keep=snz_uid refmth) ys.mth_os(keep=snz_uid os_da_58-os_da_195); 
	by snz_uid;
	if a;
	windowstart_leed=refmth-24;
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
	if sum(of nosb1-nosb&n)<=180 then output ypypp.not_os;
	  else output ypypp.overseas_6mthsplus;
run;

**LAST SCHOOL enrolment before reference date - Needed here purely to exclude people with missing school enrolment enddates***********;
**Also look for attendance at any non-NQF school - these kids also to be dropped;
proc sql;
	create table enrol
		as select 
			snz_uid
			,input(compress(moe_esi_start_date,"-"),yymmdd10.) format date9. as startdate
			,case when moe_esi_end_date is not null then input(compress(moe_esi_end_date,"-"),yymmdd10.) 
			      else input(compress(moe_esi_extrtn_date,"-"),yymmdd10.) 
		end 
		format date9. as enddate
		,input(moe_esi_provider_code, 10.) as schoolnbr
		,moe_esi_domestic_status_code as domestic_status
		,case when moe_esi_end_date='  ' then 1 
		    else 0 end as sch_enddate_imputed
	from moe.student_enrol
		where snz_uid in (select distinct snz_uid from ypypp.mth_ypypp) and moe_esi_start_date is not null 
			order by snz_uid, startdate, enddate;
quit;

**Removing any overlaps in enrolment;
%OVERLAP (enrol);

**Find non-NQF schools so children who attended them can be identified;
data nonnqf;
	set enrol_or;
	if schoolnbr in 
		(29,41,52,54,62,78,81,89,130,141,278,281,436,439,440,441,456,459,460,484,571,620,1132,1139,1605,1626,1655,2085,4152,
		37,60,67,333,387,617,1606,1640);
run;

proc sql;
	create table ypypp.kids_nonnqf
		as select distinct snz_uid
			from nonnqf
				order by snz_uid;
quit;

**Select the last school enrolment spell starting before the reference date, then select those with an imputed enddate;
proc sql;
	create table keep
		as select a.snz_uid, a.refmth, a.dob, a.startdate as ysstartdate,
			b.*
		from ypypp.mth_ypypp a 
			left join enrol_or b
				on a.snz_uid=b.snz_uid
			    where b.startdate<a.startdate
				order by a.snz_uid, a.refmth, a.startdate, b.startdate;
quit;

data ypypp.last_sch_rec;
	set keep;
	by snz_uid refmth YSstartdate startdate;
	if last.refmth;
run;

**Select if last school enrolment spell hasn't yet ended - these records to be dropped;
data enddate_imputed(keep=snz_uid refmth);
	set ypypp.last_sch_rec;
	if sch_enddate_imputed=1;
run;

proc sort data=enddate_imputed out=ypypp.enddate_imputed;
	by snz_uid refmth;
run;

** First create indicators of prior participation in YS-NEET or YTS;
data yst_parts;
	set yst.yst_spells;
	ys_startdate=input(compress(yst_spl_participation_start_date,"-"),yymmdd10.);
	ys_enddate=input(compress(yst_spl_participation_end_date,"-"),yymmdd10.);
	format ys_startdate ys_enddate date9.;
run;

proc sql;
	create table prior_parts as 
	select a.snz_uid, a.refmth, a.part_type, a.participant, a.startdate, b.ys_startdate, b.ys_enddate, b.yst_spl_programme_code as prog_code
	from ypypp.monthly a left join yst_parts b
	on a.snz_uid=b.snz_uid
	order by snz_uid, refmth, ys_startdate;
quit;

data ypypp.prior_parts(keep=snz_uid refmth prior_yts prior_neet);
	set prior_parts;
	retain prior_ypp prior_yp prior_neet prior_yts;
	by snz_uid refmth;
	if first.refmth then do;
		prior_ypp=0;
		prior_yp=0;
		prior_neet=0;
		prior_yts=0;
	end;
	** Only include those participants who had a participation that started more than 3 months before the benefit spell start
		and lasted longer than 3 months;
	prior_mth=intck('month',ys_startdate,startdate);
	part_mths=intck('month',ys_startdate,ys_enddate);
	if prior_mth>3 and part_mths>3 then do;
		if prog_code='ART_YPP' then prior_ypp=1;
		if prog_code='ART_YP' then prior_yp=1;
		if prog_code='ART_NEET' then prior_neet=1;
		if prog_code='YTS' then prior_yts=1;
	end;
	if (prior_neet or prior_yts) and last.refmth then output;
run;

proc sort data=ypypp.overseas_6mthsplus nodupkey;
	by snz_uid refmth;
run;

proc freq data=ypypp.model_final;
	tables part_type*participant/missing;
run;

** Apply exclusions - exclude those overseas for more than 6 months and those with an imputed school end date;
data first;
	merge ypypp.model_final(in=a) ypypp.overseas_6mthsplus(in=b) ypypp.enddate_imputed(in=c) ypypp.prior_parts(in=d);
	by snz_uid refmth;
	if a and not b and not c;
	** Identify those who have previously participated in the YSNEET stream of youth service or in its predecessor (YTS or Youth Transition Service);
	if d then yts_ysneet=1;
	else yts_ysneet=0;
run;

proc freq data=first;
	tables part_type;
	where participant in (0,1);
run;

** Apply additional exclusion of those kids who most recently attended a non nqf school;
data ypypp.model_final_restricted(compress=y);
	merge first(in=a) ypypp.kids_nonnqf(in=b); 
	by snz_uid;
	if a and not b;
run;

proc freq data=ypypp.model_final_restricted;
	tables part_type;
	where participant in (0,1);
run;

proc freq data=ypypp.model_final_restricted;
	tables participant*startyear/missing nopercent norow nocol;
run;

proc means data=ypypp.model_final_restricted;
run;

data ypiyb.yp_newparts;
	set ypypp.model_final_restricted;
	where part_type='YP' and participant in (0,1);
run;

data ypiyb.iyb_newparts;
	set ypypp.model_final_restricted;
	where part_type='IYB' and participant in (0,1);
run;

data yppdpb.ypp_newparts;
	set ypypp.model_final_restricted;
	where part_type='YPP' and participant in (0,1);
run;

data yppdpb.DPBEMA_newparts;
	set ypypp.model_final_restricted;
	where part_type='DPBEMA' and participant in (0,1);
run;

data ypypp.pre_ys5_monthly(keep=snz_uid refmth 
								sch_last6 sch_prior6 sch_first12 
								emp_last6 emp_prior6 emp_first12 
								tert_last6 tert_prior6 tert_first12 
								neet_last6 neet_prior6 neet_first12 
								ben_last6 ben_prior6 ben_first12 
							    mean_earn_last6);
	set ys5.monthly_revised;
	mths_sch_last6=sum(of sch43-sch48);
	mths_emp_last6=sum(of emp43-emp48);
	mths_tert_last6=sum(of tert43-tert48);
	mths_neet_last6=sum(of neet43-neet48);
	mths_ben_last6=sum(of ben43-ben48);
	mths_sch_prior6=sum(of sch37-sch42);
	mths_emp_prior6=sum(of emp37-emp42);
	mths_tert_prior6=sum(of tert37-tert42);
	mths_neet_prior6=sum(of neet37-neet42);
	mths_ben_prior6=sum(of ben37-ben42);
	mths_sch_first12=sum(of sch25-sch36);
	mths_emp_first12=sum(of emp25-emp36);
	mths_tert_first12=sum(of tert25-tert36);
	mths_neet_first12=sum(of neet25-neet36);
	mths_ben_first12=sum(of ben25-ben36);
    mths_sch_last18=sum(of sch31-sch48);
	mths_emp_last18=sum(of emp31-emp48);
	mths_tert_last18=sum(of tert31-tert48);
	mths_neet_last18=sum(of neet31-neet48);
	mths_ben_last18=sum(of ben31-ben48);
	sch_last6=(put(mths_sch_last6,mth6m.))*1;
	emp_last6=(put(mths_emp_last6,mth6m.))*1;
	tert_last6=(put(mths_tert_last6,mth6m.))*1;
	neet_last6=(put(mths_neet_last6,mth6m.))*1;
	ben_last6=(put(mths_ben_last6,mth6m.))*1;
	sch_prior6=(put(mths_sch_prior6,mth6m.))*1;
	emp_prior6=(put(mths_emp_prior6,mth6m.))*1;
	tert_prior6=(put(mths_tert_prior6,mth6m.))*1;
	neet_prior6=(put(mths_neet_prior6,mth6m.))*1;
	ben_prior6=(put(mths_ben_prior6,mth6m.))*1;
	sch_first12=(put(mths_sch_first12,mth12m.))*1;
	emp_first12=(put(mths_emp_first12,mth12m.))*1;
	tert_first12=(put(mths_tert_first12,mth12m.))*1;
	neet_first12=(put(mths_neet_first12,mth12m.))*1;
	ben_first12=(put(mths_ben_first12,mth12m.))*1;

	mean_earn_last6=mean(of earn43-earn48);
run;

data ypypp.pre_yts_monthly(keep=snz_uid refmth 
								sch_last6 sch_prior6 sch_first12 
								emp_last6 emp_prior6 emp_first12 
								tert_last6 tert_prior6 tert_first12 
								neet_last6 neet_prior6 neet_first12 
								ben_last6 ben_prior6 ben_first12 
							    );
	set yts.pre_YS_monthly_revised;
	sch_last6=(put(mths_sch_last6,mth6m.))*1;
	emp_last6=(put(mths_emp_last6,mth6m.))*1;
	tert_last6=(put(mths_tert_last6,mth6m.))*1;
	neet_last6=(put(mths_neet_last6,mth6m.))*1;
	ben_last6=(put(mths_ben_last6,mth6m.))*1;
	sch_prior6=(put(mths_sch_prior6,mth6m.))*1;
	emp_prior6=(put(mths_emp_prior6,mth6m.))*1;
	tert_prior6=(put(mths_tert_prior6,mth6m.))*1;
	neet_prior6=(put(mths_neet_prior6,mth6m.))*1;
	ben_prior6=(put(mths_ben_prior6,mth6m.))*1;
	sch_first12=(put(mths_sch_first12,mth12m.))*1;
	emp_first12=(put(mths_emp_first12,mth12m.))*1;
	tert_first12=(put(mths_tert_first12,mth12m.))*1;
	neet_first12=(put(mths_neet_first12,mth12m.))*1;
	ben_first12=(put(mths_ben_first12,mth12m.))*1;
run;