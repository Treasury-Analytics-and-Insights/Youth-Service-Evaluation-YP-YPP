/* 7a YPP YP - Extend outcomes to 2015 */

******************************************************;
**  Outcomes were previously only able to be observed up to 2014 - this takes advantage of the new data in the March 2016 update;
**  Need to use the agency uids instead of the snz_uids as these generally don't change over time - the exception is the Justice uid between December and March, so this has to be treated differently;
******************************************************;

** Key data inputs:
*		- byp.allreps_1 / biyb.allreps_1 / bypp.allreps_1 / bdpbema.allreps_1 / byppb.allreps_1 / bdpbemab.allreps_1 (all participants/historical comparisons and matched comparisons - all bootstrap replicates)
*		- security.concordance / secnew.concordance (concordance files between snz_uids and source agency uids)
*		- sandmoe.moe_student_enrol_info (school enrolment information)
*		- moenew.enrolment (tertiary enrolments)
*		- msdnew.msd_spell (benefit spells)
*		- datanew.income_cal_yr (income data for earnings and student allowances)
*		- moenew.student_standard (nqf standards)
*		- sandmoe.student_qual (qualifications)
*		- sandmoe.moe_standard_lookup (nqf standard lookup)
*		- sandmoe.tertiary_completions (tertiary qualification completions)
*		- project.schoolprofile_open_&version (open schools)
*		- project.schoolprofile_closed_&version (closed schools)
*		- moenew.tec_it_learner  (industry training data)
*		- datanew.person_overseas_spell (time overseas)
*		- CORnew.ov_major_mgmt_periods (corrections sentences)

** Key data outputs:
* 		- ypypp.all_popn (complete population of participants and controls we want outcomes data for)
*		- ypypp.popn_uidlink (file linking the UIDs from the two refreshes - allows us to see which UIDs from the March refressh match which ones in the December refresh;
*		- ypypp.mth_sch_enrol (monthly school enrolment data)
* 		- ypypp.tertiary_quals (tertiary quals data)
*		- ypypp.ncea_credit_sumb (credits gained)
*		- ypypp.mth_os (months overseas)
*		- ypypp.mth_corrections (months serving corrections sentences)
* 		- ypypp.mth_studall (months receiving student allowances)
*		- ypypp.mth_ter_enrol (months in tertiary enrolment)
*		- ypypp.IT_quals (industry training quals)
*		- ypypp.highest_quals_achieved (highest quals achieved)
* 		- ypypp.mth_ben (monthly benefit data)
*		- ypypp.mth_emp (monthly employment data)
*		- ypypp.monthly_ext (extended monthly outcomes - all outcomes)
*		- ypypp.depvars_ext (extended outcomes variables for impact and standard erroro estimation)
;

%let start='01Dec2007'd;
%let first=106; *Jan2008;
%let last= 201; *Dec2015;

%let population=ypypp.popn_uidlink;

proc datasets lib=work kill;
run;

** Select a population of SNZ_UIDs from all matched records in any of our models;
data ypypp.all_popn;
set byp.allreps_1  (in=a keep=snz_uid refmth)
	biyb.allreps_1  (in=b keep=snz_uid refmth)
	bypp.allreps_1 (in=c keep=snz_uid refmth)
	bdpbema.allreps_1 (in=d keep=snz_uid refmth)
	byppb.allreps_1 (in=e keep=snz_uid refmth)
	bdpbemab.allreps_1 (in=f keep=snz_uid refmth);
run;

proc sort data=ypypp.all_popn nodupkey;
	by snz_uid refmth;
run;

** Now we want other uids for this population;
proc sql;
    connect to sqlservr (server=WPRDSQL36\ileed database=IDI_Clean_20160224);
    create table ypypp.popn_uidlink as
    select * from connection to sqlservr
    (select snz_uid, snz_ird_uid, snz_moe_uid, snz_msd_uid, snz_jus_uid from security.concordance)
	 where snz_uid in (select distinct snz_uid from ypypp.all_popn)
	order by snz_uid;
quit;

** The SNZ_JUS_UID seems to have changed - we need to link the new justice UID - use the snz_ird_uid - they all have one;
proc sql;
	create table ypypp.popn_uidlink as
	select a.*,b.snz_jus_uid as jus_uid_new from
	ypypp.popn_uidlink a left join secnew.concordance b
	on a.snz_ird_uid = b.snz_ird_uid
	order by snz_uid;
quit;

/*proc sort data=ypypp.popn_uidlink nodupkey;
	by snz_uid;
run;

/*proc sql;
	create table ypypp.popn_uidlink as
	select * from
	ypypp.popn_uidlink a left join ypypp.all_popn b
	on a.snz_uid = b.snz_uid;
quit;

proc sort data=ypypp.popn_uidlink(drop=refmth) nodupkey;
	by snz_uid;
run;*/

** ys.mth_sch_enrol --- sandmoe.moe_student_enrol_info;

**********************************;
**School enrolment monthly vectors;
***********************************;

proc sql;
create table enrol
as select 
snz_moe_uid
,input(compress(start_date,"-"),yymmdd10.) format date9. as startdate
,case when end_date is not null then input(compress(end_date,"-"),yymmdd10.) 
   else input(compress(extrtn_date,"-"),date9.) end format date9. as enddate
,input(provider_code, 10.) as schoolnbr
,domestic_status_code as domestic_status
,case when end_date='' then 1 else 0 end as sch_enddate_imputed
from sandmoe.moe_student_enrol_info
where snz_moe_uid in (select distinct snz_moe_uid from &population) and start_date is not null 
order by snz_moe_uid, startdate, enddate;
quit;

data enrol;
set enrol;
if enddate=. then enddate='30aug2016'd;
run;

proc sql;
create table enrol as select
a.*, b.snz_uid
from enrol a left join &population. b
on a.snz_moe_uid = b.snz_moe_uid
order by snz_uid, startdate, enddate;
quit;

**Removing any overlaps in enrolment;
%OVERLAP (enrol); 

**CODE FOR SCHOOL ENROLMENT MONTHS;
%let start='01Jan2004'd;
%let first=58;  *Jan2006;
%let last=207;  *Jun2016;

data enrol_month_temp  ;
set enrol_OR;
format start_window end_window date9.;
array sch_enr_id_(*) sch_enr_id_&first-sch_enr_id_&last; * end of Dec2015;
array sch_enr_da_(*) sch_enr_da_&first-sch_enr_da_&last; * end of Dec2015;

do ind=&first to &last; i=ind-&first+1;
	sch_enr_id_(i)=0;
	sch_enr_da_(i)=0;
* overwriting start and end window as interval equal to one month;

start_window=intnx("month",&start.,i-1,"beginning"); * start is beg of the month;
end_window=intnx("month",&start.,i-1,"end");* end is end of the month;

if not((startdate > end_window) or (enddate < start_window)) then do;
	sch_enr_id_(i)=1; * creating inidcator of school enrolment;
	* measuring the days enrolled;
				if (startdate <= start_window) and  (enddate > end_window) then
					days=(end_window-start_window)+1;
				else if (startdate <= start_window) and  (enddate <= end_window) then
					days=(enddate-start_window)+1;
				else if (startdate > start_window) and  (enddate <= end_window) then
					days=(enddate-startdate)+1;
				else if (startdate > start_window) and  (enddate > end_window) then
					days=(end_window-startdate)+1;
				sch_enr_da_[i]=days*sch_enr_id_(i);
end;
end;
run;

proc summary data=enrol_month_temp nway;
class snz_uid ;
var sch_enr_id_&first-sch_enr_id_&last  sch_enr_da_&first-sch_enr_da_&last sch_enddate_imputed;
output out=TEMP(drop=_:) sum=;
run;

data ypypp.mth_sch_enrol(drop=ind i);
*retain snz_uid first_sch_enr_refmth  last_sch_enr_refmth;
set temp;
if sum(of sch_enr_id_&first-sch_enr_id_&last )>0;
array sch_enr_id_(*) sch_enr_id_&first-sch_enr_id_&last ; * end of dec2015;
array sch_enr_da_(*) sch_enr_da_&first-sch_enr_da_&last ; * end of dec2015;
first_sch_enr_refmth=.;
last_sch_enr_refmth=.;
do ind=&first to &last; i=ind-&first+1;
   if sch_enr_id_[i]>1 then sch_enr_id_[i]=1;
   if sch_enr_id_[i]>=1 and first_sch_enr_refmth=. then first_sch_enr_refmth=ind;
   if sch_enr_id_[i]>=1 then last_sch_enr_refmth=ind;
   end;
run;

** ys.mth_ter_enrol --- moenew.enrolment;
**********************************;
**Tertiary enrolment monthly vectors;
**Now using formal programmes only for this vector ;
**********************************;
proc freq data=moenew.enrolment;
	tables moe_enr_prog_start_date;
run;

* FORMATING, CLEANING AND SENSORING;

%let first_anal_yr=2006;
%let last_anal_yr=2015;

proc sql;
	create table enrol as
	SELECT distinct 
		snz_moe_uid
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
	FROM moenew.enrolment 
		WHERE snz_moe_uid IN 
		(SELECT DISTINCT snz_moe_uid FROM &population) and moe_enr_year_nbr>=&first_anal_yr and moe_enr_year_nbr<=&last_anal_yr 
		group by snz_uid, moe_enr_prog_start_date , moe_enr_prog_end_date, qual, NZSCED
			order by snz_moe_uid;
quit;

proc sql;
create table enrol_1 as select
a.*, b.snz_uid
from enrol a left join &population. b
on a.snz_moe_uid = b.snz_moe_uid
order by snz_uid, startdate, enddate;
quit;

* Formating dates and creating clean enrolment file;
* Defining formal and informal enrolments;

data enrol_clean_formal;
	set enrol_1;
	if EFTS_consumed>0;
	dur=enddate-startdate;
	if dur>0;
	start_year=year(startdate);
	if start_year>=&first_anal_yr and start_year<=&last_anal_yr;
if /*fund_source not in ('05','06','07','08','11','24') and*/ qual_type="D" then Formal=1; 
if formal=1 then output;
run;

**2% have programme durations of more than one year.  How do we know they remained enrolled;
**Might be best cut off enrolment at end of one year;
%overlap(enrol_clean_formal);

%let start='01Jan2006'd;
%let first=82;  *Jan2006;
%let last=201;  *Dec2015;  


data TER_ENROL_MON_temp; 
set enrol_clean_formal_OR ;
*start=max(13,intck("MONTH",&start.,startdate)+1); 
*end=intck("MONTH",&start.,enddate)+1;

format start_window end_window date9.;
array ter_enr_id_(*) ter_enr_id_&first-ter_enr_id_&last; 
array ter_enr_da_(*) ter_enr_da_&first-ter_enr_da_&last; 
/*
array f_ter_enr_id_(*) f_ter_enr_id_&first-f_ter_enr_id_&last; 
array f_ter_enr_da_(*) f_ter_enr_da_&first-f_ter_enr_da_&last; 
*/
do ind=&first to &last; i=ind-&first+1;
	ter_enr_id_(i)=0;
	ter_enr_da_(i)=0;
	*f_ter_enr_id_(i)=0;
	*f_ter_enr_da_(i)=0;
* overwriting start and end window as interval equal to one month;

start_window=intnx("month",&start.,i-1,"beginning"); * start is beg of the month;
end_window=intnx("month",&start.,i-1,"end");* end is end of the month;

if not((startdate > end_window) or (enddate < start_window)) then do;
	ter_enr_id_(i)=1; * creating inidcator of school enrolment;
	* measuring the days enrolled;
				if (startdate <= start_window) and  (enddate > end_window) then
					days=(end_window-start_window)+1;
				else if (startdate <= start_window) and  (enddate <= end_window) then
					days=(enddate-start_window)+1;
				else if (startdate > start_window) and  (enddate <= end_window) then
					days=(enddate-startdate)+1;
				else if (startdate > start_window) and  (enddate > end_window) then
					days=(end_window-startdate)+1;
				ter_enr_da_[i]=days*ter_enr_id_(i);
end;
end;
run;

proc summary data=TER_ENROL_MON_temp nway;
class snz_uid ;
var ter_enr_id_&first-ter_enr_id_&last  ter_enr_da_&first-ter_enr_da_&last 
   /*f_ter_enr_id_&first-f_ter_enr_id_&last f_ter_enr_da_&first-f_ter_enr_da_&last*/;
output out=mth_ter_enrol(drop=_:) sum=;
run;

data ypypp.mth_ter_enrol;
set mth_ter_enrol;
array ter_enr_id_(*) ter_enr_id_&first-ter_enr_id_&last; 
do ind=&first to &last; i=ind-&first+1;
   if ter_enr_id_[i]>1 then ter_enr_id_[i]=1;
   end;
run;



** ys.mth_emp - datanew.income_cal_yr;
*************************;
****Employment;
**I choose to select data from 2006 onwards;
**when these kids are about 11 or 12;
****************************;

%macro select(year);
proc sql;
create table earners&year as 
   select snz_ird_uid, 
    inc_cal_yr_year_nbr as year,
      sum(inc_cal_yr_mth_01_amt) as earn1,
	  sum(inc_cal_yr_mth_02_amt) as earn2,
      sum(inc_cal_yr_mth_03_amt) as earn3,
      sum(inc_cal_yr_mth_04_amt) as earn4,
      sum(inc_cal_yr_mth_05_amt) as earn5,
	  sum(inc_cal_yr_mth_06_amt) as earn6,
      sum(inc_cal_yr_mth_07_amt) as earn7,
      sum(inc_cal_yr_mth_08_amt) as earn8,
      sum(inc_cal_yr_mth_09_amt) as earn9,
      sum(inc_cal_yr_mth_10_amt) as earn10,
      sum(inc_cal_yr_mth_11_amt) as earn11,
      sum(inc_cal_yr_mth_12_amt) as earn12   
    from datanew.income_cal_yr
    where inc_cal_yr_income_source_code = 'W&S' and inc_cal_yr_year_nbr=&year and snz_ird_uid in (SELECT DISTINCT snz_ird_uid FROM &population) 
    group by snz_ird_uid, year
    order by snz_ird_uid;
quit;

%mend select;

%select(2006);
%select(2007);
%select(2008);
%select(2009);
%select(2010);
%select(2011);
%select(2012);
%select(2013);
%select(2014);
%select(2015);

**Convert earnings data to Dec 2015 $ values;
**Disregard months where monthly earnings are less than $10;
%let m=82;
%let n=201;
data ypypp.mth_emp(keep=snz_ird_uid emp&m-emp&n rearn&m-rearn&n);
merge 
  earners2006(keep=snz_ird_uid earn1-earn12 rename=(earn1-earn12=earn82-earn93))
  earners2007(keep=snz_ird_uid earn1-earn12 rename=(earn1-earn12=earn94-earn105))
  earners2008(keep=snz_ird_uid earn1-earn12 rename=(earn1-earn12=earn106-earn117)) 
  earners2009(keep=snz_ird_uid earn1-earn12 rename=(earn1-earn12=earn118-earn129))
  earners2010(keep=snz_ird_uid earn1-earn12 rename=(earn1-earn12=earn130-earn141))
  earners2011(keep=snz_ird_uid earn1-earn12 rename=(earn1-earn12=earn142-earn153))
  earners2012(keep=snz_ird_uid earn1-earn12 rename=(earn1-earn12=earn154-earn165)) 
  earners2013(keep=snz_ird_uid earn1-earn12 rename=(earn1-earn12=earn166-earn177))
  earners2014(keep=snz_ird_uid earn1-earn12 rename=(earn1-earn12=earn178-earn189))
  earners2015(keep=snz_ird_uid earn1-earn12 rename=(earn1-earn12=earn190-earn201));
by snz_ird_uid;
array earn(*) earn&m-earn&n;
array rearn(*) rearn&m-rearn&n;
array emp(*) emp&m-emp&n;
do i=&m to &n;
    if i<=48 then rearn(i-&m+1) = earn(i-&m+1)*1191/913;
    else if i<=51 then rearn(i-&m+1) = earn(i-&m+1)*1191/913;
    else if i<=54 then rearn(i-&m+1) = earn(i-&m+1)*1191/918;
    else if i<=57 then rearn(i-&m+1) = earn(i-&m+1)*1191/924;
    else if i<=60 then rearn(i-&m+1) = earn(i-&m+1)*1191/928;
    else if i<=63 then rearn(i-&m+1) = earn(i-&m+1)*1191/935;
    else if i<=66 then rearn(i-&m+1) = earn(i-&m+1)*1191/941;
    else if i<=69 then rearn(i-&m+1) = earn(i-&m+1)*1191/949;
    else if i<=72 then rearn(i-&m+1) = earn(i-&m+1)*1191/953;
    else if i<=75 then rearn(i-&m+1) = earn(i-&m+1)*1191/962;
    else if i<=78 then rearn(i-&m+1) = earn(i-&m+1)*1191/973;
    else if i<=81 then rearn(i-&m+1) = earn(i-&m+1)*1191/979;
    else if i<=84 then rearn(i-&m+1) = earn(i-&m+1)*1191/985;
    else if i<=87 then rearn(i-&m+1) = earn(i-&m+1)*1191/1000;
    else if i<=90 then rearn(i-&m+1) = earn(i-&m+1)*1191/1007;
    else if i<=93 then rearn(i-&m+1) = earn(i-&m+1)*1191/1005;
    else if i<=96 then rearn(i-&m+1) = earn(i-&m+1)*1191/1010;
    else if i<=99  then rearn(i-&m+1) = earn(i-&m+1)*1191/1020;
    else if i<=102 then rearn(i-&m+1) = earn(i-&m+1)*1191/1025;
    else if i<=105 then rearn(i-&m+1) = earn(i-&m+1)*1191/1037;
    else if i<=108 then rearn(i-&m+1) = earn(i-&m+1)*1191/1044;
    else if i<=111 then rearn(i-&m+1) = earn(i-&m+1)*1191/1061;
    else if i<=114 then rearn(i-&m+1) = earn(i-&m+1)*1191/1077;
    else if i<=117 then rearn(i-&m+1) = earn(i-&m+1)*1191/1072;
    else if i<=120 then rearn(i-&m+1) = earn(i-&m+1)*1191/1075;    
    else if i<=123 then rearn(i-&m+1) = earn(i-&m+1)*1191/1081;
    else if i<=126 then rearn(i-&m+1) = earn(i-&m+1)*1191/1095;
    else if i<=129 then rearn(i-&m+1) = earn(i-&m+1)*1191/1093;  
    else if i<=132 then rearn(i-&m+1) = earn(i-&m+1)*1191/1097;   
    else if i<=135 then rearn(i-&m+1) = earn(i-&m+1)*1191/1099;
    else if i<=138 then rearn(i-&m+1) = earn(i-&m+1)*1191/1111;
    else if i<=141 then rearn(i-&m+1) = earn(i-&m+1)*1191/1137;
    else if i<=144 then rearn(i-&m+1) = earn(i-&m+1)*1191/1146;   
    else if i<=147 then rearn(i-&m+1) = earn(i-&m+1)*1191/1157;   
    else if i<=150 then rearn(i-&m+1) = earn(i-&m+1)*1191/1162;
    else if i<=153 then rearn(i-&m+1) = earn(i-&m+1)*1191/1158;  * 1176 Dec 2011 ;  
    else if i<=156 then rearn(i-&m+1) = earn(i-&m+1)*1191/1164;
    else if i<=159 then rearn(i-&m+1) = earn(i-&m+1)*1191/1168;   
    else if i<=162 then rearn(i-&m+1) = earn(i-&m+1)*1191/1171;
    else if i<=165 then rearn(i-&m+1) = earn(i-&m+1)*1191/1169;  * Dec 2012;  
    else if i<=168 then rearn(i-&m+1) = earn(i-&m+1)*1191/1174;
    else if i<=171 then rearn(i-&m+1) = earn(i-&m+1)*1191/1176;  * June 2013; 
    else if i<=174 then rearn(i-&m+1) = earn(i-&m+1)*1191/1187;   
    else if i<=177 then rearn(i-&m+1) = earn(i-&m+1)*1191/1188;  *Dec 2013;
	else if i<=180 then rearn(i-&m+1) = earn(i-&m+1)*1191/1192; *Mar 2015;
    else if i<=183 then rearn(i-&m+1) = earn(i-&m+1)*1191/1195; *Jun 2015;
	else if i<=186 then rearn(i-&m+1) = earn(i-&m+1)*1191/1199; *Sep 2015;
	else if i<=189 then rearn(i-&m+1) = earn(i-&m+1)*1191/1197; *Dec 2015;
    else if i<=192 then rearn(i-&m+1) = earn(i-&m+1)*1191/1195; *Mar 2015;
    else if i<=195 then rearn(i-&m+1) = earn(i-&m+1)*1191/1200; *Jun 2015;
    else if i<=198 then rearn(i-&m+1) = earn(i-&m+1)*1191/1204; *Sep 2015;
    else if i<=201 then rearn(i-&m+1) = earn(i-&m+1)*1191/1198; *Dec 2015;
end;
do i=1 to dim(rearn);
if rearn(i)<10 then do; 
     emp(i)=0; 
	 rearn(i)=.; end;
else do; 
   emp(i)=1; 
   rearn(i)=round(rearn(i), 1); 
   if rearn(i)>500000 then rearn(i)=500000;
   end;
end;
run;

proc sql;
create table ypypp.mth_emp as select
a.*, b.snz_uid
from ypypp.mth_emp a left join &population. b
on a.snz_ird_uid = b.snz_ird_uid
order by snz_uid;
quit;

proc sort data=ypypp.mth_emp nodupkey;
	by snz_uid;
run;

** ys.mth_ben --- msdnew.msd_spell;
*****************************************************;
**Adult or youth benefit receipt -using Sarah's code;
**************************************************;
proc format ;
VALUE $bengp_pre2013wr                  /* Jane suggest to add the old format */
    '020','320' = "Invalid's Benefit"
    '030','330' = "Widow's Benefit"
    '040','044','340','344'
                = "Orphan's and Unsupported Child's benefits"
    '050','350','180','181'
    = "New Zealand Superannuation and Veteran's and Transitional Retirement Benefit"
    '115','604','605','610'
                = "Unemployment Benefit and Unemployment Benefit Hardship"
    '125','608' = "Unemployment Benefit (in Training) and Unemployment Benefit Hardship (in Training)"
    '313','613','365','665','366','666','367','667'
                = "Domestic Purposes related benefits"
    '600','601' = "Sickness Benefit and Sickness Benefit Hardship"
    '602','603' = "Job Search Allowance and Independant Youth Benefit"
    '607'       = "Unemployment Benefit Student Hardship"
    '609','611' = "Emergency Benefit"
    '839','275' = "Non Beneficiary"
    'YP ','YPP' = "Youth Payment and Young Parent Payment"
        ' '     = "No Benefit"
 ;

value $bennewgp 

'020'=	"Invalid's Benefit"
'320'=	"Invalid's Benefit"

'330'=	"Widow's Benefit"
'030'=	"Widow's Benefit"

'040'=	"Orphan's and Unsupported Child's benefits"
'044'=	"Orphan's and Unsupported Child's benefits"
'340'=	"Orphan's and Unsupported Child's benefits"
'344'=	"Orphan's and Unsupported Child's benefits"

'050'=	"New Zealand Superannuation and Veteran's and Transitional Retirement Benefit"
'180'=	"New Zealand Superannuation and Veteran's and Transitional Retirement Benefit"
'181'=	"New Zealand Superannuation and Veteran's and Transitional Retirement Benefit"
'350'=	"New Zealand Superannuation and Veteran's and Transitional Retirement Benefit"

'115'=	"Unemployment Benefit and Unemployment Benefit Hardship"
'604'=	"Unemployment Benefit and Unemployment Benefit Hardship"
'605'=	"Unemployment Benefit and Unemployment Benefit Hardship"
'610'=	"Unemployment Benefit and Unemployment Benefit Hardship"
'607'=	"Unemployment Benefit Student Hardship"
'608'=	"Unemployment Benefit (in Training) and Unemployment Benefit Hardship (in Training)"
'125'=	"Unemployment Benefit (in Training) and Unemployment Benefit Hardship (in Training)"


'313'=  "Domestic Purposes related benefits"
'365'=	"Sole Parent Support "					/* renamed */
'366'=	"Domestic Purposes related benefits"
'367'=	"Domestic Purposes related benefits"
'613'=	"Domestic Purposes related benefits"
'665'=	"Domestic Purposes related benefits"
'666'=	"Domestic Purposes related benefits"
'667'=	"Domestic Purposes related benefits"

'600'=	"Sickness Benefit and Sickness Benefit Hardship"
'601'=	"Sickness Benefit and Sickness Benefit Hardship"

'602'=	"Job Search Allowance and Independant Youth Benefit"
'603'=	"Job Search Allowance and Independant Youth Benefit"

'611'=	"Emergency Benefit"

'315'=	"Family Capitalisation"
'461'=	"Unknown"
'000'=	"No Benefit"
'839'=	"Non Beneficiary"

/* new codes */
'370'=  "Supported Living Payment related"
'675'=  "Job Seeker related"
'500'=  "Work Bonus"
;
run  ;

proc format;
value $ADDSERV
'YP'	='Youth Payment'
'YPP'	='Young Parent Payment'
'CARE'	='Carers'
'FTJS1'	='Job seeker Work Ready '
'FTJS2'	='Job seeker Work Ready Hardship'
'FTJS3'	='Job seeker Work Ready Training'
'FTJS4'	='Job seeker Work Ready Training Hardship'
'MED1'	='Job seeker Health Condition and Disability'
'MED2'	='Job seeker Health Condition and Disability Hardship'
'PSMED'	='Health Condition and Disability'
''		='.';
run;

%let sensor=30Nov2015;

data msd_spel; 
   set msdnew.msd_spell;
* Formating dates and sensoring;
	format startdate enddate spellfrom spellto date9.;
	spellfrom=input(compress(msd_spel_spell_start_date,"-"),yymmdd10.);
	spellto=input(compress(msd_spel_spell_end_date,"-"),yymmdd10.);
	if spellfrom<"&sensor"d;
	if spellto>"&sensor"d then spellto="&sensor"d;
	if spellto=. then spellto="&sensor"d;
	startdate=spellfrom;
	enddate=spellto;
* TRANSLATING POST REFORM SERVF INTO PRE REFORM FOR OLD TIME SERIES******;

	if msd_spel_prewr3_servf_code='' then prereform=put(msd_spel_servf_code, $bengp_pre2013wr.); 
	else prereform=put(msd_spel_prewr3_servf_code,$bengp_pre2013wr.);	

* applying wider groupings;
if prereform in ("Domestic Purposes related benefits", "Widow's Benefit","Sole Parent Support ") then ben='dpb';
else if prereform in ("Invalid's Benefit", "Supported Living Payment related") then ben='ib';
else if prereform in ("Unemployment Benefit and Unemployment Benefit Hardship",
   "Unemployment Benefit Student Hardship", "Unemployment Benefit (in Training) and Unemployment Benefit Hardship (in Training)") then ben='ub';
else if prereform in ("Job Search Allowance and Independant Youth Benefit") then ben='iyb';
else if prereform in ("Sickness Benefit and Sickness Benefit Hardship") then ben='sb';
else if prereform in ("Orphan's and Unsupported Child's benefits") then ben='ucb';
else ben='oth';

* TRANSLATING PREREFORM SERVF INTO POST REFORM SERVF FOR NEW TIME SEIRES*****;
length benefit_desc_new $50;
servf=msd_spel_servf_code;
additional_service_data=msd_spel_add_servf_code;
	if  servf in ('602', /* Job Search Allowance - a discontinued youth benefit */
				 '603') /* IYB then aft 2012 Youth/Young Parent Payment */	 
		and additional_service_data ne 'YPP' then benefit_desc_new='1: YP Youth Payment Related' ;/* in 2012 changes some young DPB-SP+EMA moved to YPP */

	else if servf in ('313')   /* EMA(many were young mums who moved to YPP aft 2012) */
		or additional_service_data='YPP' then benefit_desc_new='1: YPP Youth Payment Related' ;
  
	else if  (servf in (
				   '115', /* UB Hardship */
                   '610', /* UB */
                   '611', /* Emergency Benefit (UB for those that did not qualify)*/
				   '030', /* B4 2012 was MOST WB, now just WB paid overseas) */ 
				   '330', /* Widows Benefit (weekly, old payment system) */ 
				   '366', /* DPB Woman Alone (weekly, old payment system) */
				   '666'))/* DPB Woman Alone */
		or (servf in ('675') and additional_service_data in (
					'FTJS1', /* JS Work Ready */
					'FTJS2')) /* JS Work Ready Hardship */
			
		then benefit_desc_new='2: Job Seeker Work Ready Related'; 

	else if  (servf in ('607', /* UB Student Hardship (mostly over summer holidays)*/ 
				   '608')) /* UB Training */
        or (servf in ('675') and additional_service_data in (
					'FTJS3', /* JS Work Ready Training */
					'FTJS4'))/* JS Work Ready Training Hardship */
		then benefit_desc_new='2: Job Seeker Work Ready Training Related'; 


	else if (servf in('600', /* Sickness Benefit */
				  '601')) /* Sickness Benefit Hardship */ 
		or (servf in ('675') and additional_service_data in (
				'MED1',   /* JS HC&D */
				'MED2'))  /* JS HC&D Hardship */
		then benefit_desc_new='3: Job Seeker HC&D Related' ;

	else if servf in ('313',   /* Emergency Maintenance Allowance (weekly) */
				   
				   '365',   /* B4 2012 DPB-SP (weekly), now Sole Parent Support */
				   '665' )  /* DPB-SP (aft 2012 is just for those paid o'seas)*/
		then benefit_desc_new='4: Sole Parent Support Related' ;/*NB young parents in YPP since 2012*/

	else if (servf in ('370') and additional_service_data in (
						'PSMED', /* SLP */
						'')) /* SLP paid overseas(?)*/ 
		or (servf ='320')    /* Invalids Benefit */
		or (servf='020')     /* B4 2012 020 was ALL IB, now just old IB paid o'seas(?)*/
		then benefit_desc_new='5: Supported Living Payment HC&D Related' ;

	else if (servf in ('370') and additional_service_data in ('CARE')) 
		or (servf in ('367',  /* DPB - Care of Sick or Infirm */
					  '667')) /* DPB - Care of Sick or Infirm */
		then benefit_desc_new='6: Supported Living Payment Carer Related' ;

	else if servf in ('999') /* merged in later by Corrections... */
		then benefit_desc_new='7: Student Allowance';

	else if (servf = '050' ) /* Transitional Retirement Benefit - long since stopped! */
		then benefit_desc_new='Other' ;

	else if benefit_desc_new='Unknown'   /* hopefully none of these!! */;

* applying wider groupings;
if prereform in ("Domestic Purposes related benefits", "Widow's Benefit","Sole Parent Support ") then ben='DPB';
else if prereform in ("Invalid's Benefit", "Supported Living Payment related") then ben='IB';
else if prereform in ("Unemployment Benefit and Unemployment Benefit Hardship",
   "Unemployment Benefit Student Hardship", "Unemployment Benefit (in Training) and Unemployment Benefit Hardship (in Training)") then ben='UB';
else if prereform in ("Job Search Allowance and Independant Youth Benefit") then ben='IYB';
else if prereform in ("Sickness Benefit and Sickness Benefit Hardship") then ben='SB';
else if prereform in ("Orphan's and Unsupported Child's benefits") then ben='UCB';
else ben='OTH';

if benefit_desc_new='2: Job Seeker Work Ready Training Related' then ben_new='JSWR_TR';
else if benefit_desc_new='1: YP Youth Payment Related' then ben_new='YP';
else if benefit_desc_new='1: YPP Youth Payment Related' then ben_new='YPP';
else if benefit_desc_new='2: Job Seeker Work Ready Related' then ben_new='JSWR';

else if benefit_desc_new='3: Job Seeker HC&D Related' then ben_new='JSHCD';
else if benefit_desc_new='4: Sole Parent Support Related' then ben_new='SPSR';
else if benefit_desc_new='5: Supported Living Payment HC&D Related' then ben_new='SLP_HCD';
else if benefit_desc_new='6: Supported Living Payment Carer Related' then ben_new='SLP_C';
else if benefit_desc_new='7: Student Allowance' then ben_new='SA';

else if benefit_desc_new='Other' then ben_new='OTH';
if prereform='370' and ben_new='SLP_C' then ben='DPB';
if prereform='370' and ben_new='SLP_HCD' then ben='IB';

if prereform='675' and ben_new='JSHCD' then ben='SB';
if prereform='675' and (ben_new ='JSWR' or ben_new='JSWR_TR') then ben='UB';
	spell=msd_spel_spell_nbr;
	keep snz_msd_uid spell servf spellfrom spellto ben ben_new;
run;

proc sort data=msd_spel out=mainbenefits(rename=(spellfrom=startdate spellto=enddate));
	by snz_msd_uid spell spellfrom spellto;
run;

* BDD partner spell table;
data icd_bdd_ptnr;
	set msdnew.msd_partner;
	format ptnrfrom ptnrto date9.;
	spell=msd_ptnr_spell_nbr;
	ptnrfrom=input(compress(msd_ptnr_ptnr_from_date,"-"), yymmdd10.);
	ptnrto=input(compress(msd_ptnr_ptnr_to_date,"-"), yymmdd10.);

	* Sensoring;
	if ptnrfrom>"&sensor"d then
		delete;

	if ptnrto=. then
		ptnrto="&sensor"d;

	if ptnrto>"&sensor"d then
		ptnrto="&sensor"d;
	keep snz_msd_uid partner_snz_msd_uid spell ptnrfrom ptnrto;
run;

* EXTRACTING MAIN BENEFIT AS PRIMARY;
proc sql;
	create table prim_mainben_prim_data as
		select
			s.snz_msd_uid, s.spellfrom as startdate, s.spellto as enddate, s.ben, s.ben_new, s.spell
		from
			msd_spel s inner join &population t
			on t.snz_msd_uid= s.snz_msd_uid;
run;

* MAIN BENEFITS AS PARTNER (relationship);
proc sql;
	create table prim_mainben_part_data as
		select
			s.partner_snz_msd_uid, s.ptnrfrom as startdate, s.ptnrto as enddate,s.spell,
			s.snz_msd_uid as main_snz_msd_uid
		from  icd_bdd_ptnr  s inner join &population t
			on t.snz_msd_uid = s.partner_snz_msd_uid
		order by s.snz_msd_uid, s.spell;
quit;

	**ADD benefit type to the partner's dataset;

	**Note that snz_uid+spell does not uniquely identify benefit spells therefore the start
	and enddate of each spell is also used below to correctly match partner spells to those of the main beneficiary;

	**This is done in two steps - (1) spells with fully matching start and end dates
	(2) partner spells that fall within the matching main benefit spell but are not as long;
proc sort data=mainbenefits out=main nodupkey;
	by snz_msd_uid spell startdate enddate;
run;

proc sort data=prim_mainben_part_data out=partner(rename=(main_snz_msd_uid=snz_msd_uid)) nodupkey;
	by main_snz_msd_uid spell startdate enddate;
run;

data fullymatched  unmatched(drop=ben ben_new servf);
	merge partner (in = a)
		main (in = b);
	by snz_msd_uid spell startdate enddate;

	if a and b then
		output fullymatched;
	else if a and not b then
		output unmatched;
run;

proc sql;
	create table partlymatched as
		select a.partner_snz_msd_uid, a.snz_msd_uid, a.spell, a.startdate, a.enddate,
			b.ben, b.ben_new, b.servf
		from unmatched a left join main b
			on a.snz_msd_uid=b.snz_msd_uid and a.spell=b.spell and a.startdate>=b.startdate and (a.enddate<=b.enddate or b.enddate=.) ;
quit;
run;

data prim_mainben_part_data_2;
	set fullymatched partlymatched;
run;

proc freq data=prim_mainben_part_data_2;
	tables ben_new ben;
run;

* CONSOLIDATING BENEFIT SPELLS AS PRIMARY AND PARTNER;
data prim_bennzs_data_1;
	set prim_mainben_prim_data (in=a)
		prim_mainben_part_data_2 (in=b);
	if b then
		snz_msd_uid=partner_snz_msd_uid;
run;

* sorting before running ROGER'S OVERLAP CODE spells as spells should not overlap;
proc sort data = prim_bennzs_data_1;
	by snz_msd_uid startdate enddate;
run;

proc sql;
create table prim_bennzs_data_1 as select
a.*, b.snz_uid
from prim_bennzs_data_1 a left join &population. b
on a.snz_msd_uid = b.snz_msd_uid
order by snz_uid, startdate, enddate;
quit;

proc sort data=prim_bennzs_data_1 nodupkey;
	by snz_uid startdate enddate;
run;

%overlap(prim_bennzs_data_1,examine=F);

%let start='01Jan2006'd;
%let first=82;  *Jan2006;
%let last=200;  *Nov2015;  

data ben_TEMP; 
set PRIM_BENNZS_DATA_1_OR(keep=snz_uid startdate enddate);
format start_window end_window date9.;

array ben_id_(*) ben_id_&first-ben_id_&last; 
array ben_da_(*) ben_da_&first-ben_da_&last; 

do ind=&first to &last; i=ind-&first+1;
	ben_id_(i)=0;
* overwriting start and end window as interval equal to one month;
start_window=intnx("month",&start.,i-1,"beginning"); * start is beg of the month;
end_window=intnx("month",&start.,i-1,"end");* end is end of the month;
if not((startdate > end_window) or (enddate < start_window)) then do;
	ben_id_(i)=1; * creating inidcator of school enrolment;
	* measuring the days enrolled;
				if (startdate <= start_window) and  (enddate > end_window) then
					days=(end_window-start_window)+1;
				else if (startdate <= start_window) and  (enddate <= end_window) then
					days=(enddate-start_window)+1;
				else if (startdate > start_window) and  (enddate <= end_window) then
					days=(enddate-startdate)+1;
				else if (startdate > start_window) and  (enddate > end_window) then
					days=(end_window-startdate)+1;
				ben_da_[i]=days*ben_id_(i);
end;
end;
run;

proc summary data=ben_temp nway;
class snz_uid ;
var ben_id_&first-ben_id_&last ben_da_&first-ben_da_&last;
output out=ben_mon_enrol(drop=_:) sum=;
run;

data ypypp.mth_ben(drop=i);
set ben_mon_enrol;
array ben_id_(*) ben_id_&first-ben_id_&last; 
do i=1 to dim(ben_id_);
   if ben_id_(i)>=1 then ben_id_(i)=1;
   end;
run;

** ys.highest_quals_achieved - moenew.student_standard;
*****************************************************;
**Part B;
**NCEA Credits and all qualification attainment by year;
*****************************************************;

**School credit and qualification attainment;

%let first_anal_yr=2003;
%let last_anal_yr=2015;

proc sql;
	create table stand as select 
    snz_moe_uid	
	,moe_sst_attained_year_nbr as AttainedYear
    ,moe_sst_study_provider_code as school
	,moe_sst_standard_code as StandardTable
	,moe_sst_exam_result_code as ExamResultCode
	from moenew.student_standard
    where snz_moe_uid in (select snz_moe_uid from &population)
    and (AttainedYear>=&first_anal_yr and AttainedYear<=&last_anal_yr) 
    order by snz_moe_uid, AttainedYear; 
quit;

proc sql;
create table stand as select
a.*, b.snz_uid
from stand a left join &population. b
on a.snz_moe_uid = b.snz_moe_uid;
quit;

data standard;
set stand;
schoolnbr=school*1;
standardtableid=StandardTable*1;
run;

* count credits gained at school only;
* where schools are secondary , composite, restricted composite, correspondence or special schools;
proc sort data=standard;
	by schoolnbr;
run;
data stand1;
	merge standard(in=a) 
			project.schoolprofile_open_&version(keep=schoolnumber schooltypeid schooltype rename=(schoolnumber=schoolnbr schooltypeid=open_sch_typeid schooltype=open_sch_type))
			project.schoolprofile_closed_&version(keep=schoolnumber schooltype rename=(schoolnumber=schoolnbr schooltype=closed_sch_type));
	by schoolnbr;
	if a;
run;

data stand_lookup;
	set sandmoe.moe_standard_lookup;
	keep StandardTableID StandardLevel Credit standardtypecode InternalorExternal;
run;

proc sort data=stand_lookup;
	by standardTableid;
run;

proc sort data=stand1;
	by standardTableid;
run;

data stand2;
	merge stand1(in=a) stand_lookup;
	by standardtableid;
	if a;
	* limiting to NCEA standards only;
	if standardtypecode=1 or standardtypecode=2;
	* limiting to Unit, acheivement and excluding scholarship standards;
	CR_gained=credit*1;
	if ExamResultCode in ('D','F','N','NC','V','Y') then
		CR_gained=0;
	* mainly failed and not attended exam;
	if AttainedYear>=2003;
	*Level=cats("L",StandardLevel);
    level=StandardLevel;
	if open_sch_typeid in (10026,10039,10060,10028,10031,10029,10030,10033, 10032, 10023, 10034, 10048) then
		inst='sch';
	if open_sch_typeid in (., 10010,10011,10012,10013,10014,10015,10016,10017, 10018, 10021, 10036, 10051) then
		inst='other';
	* inlcudes industry training;
    if InternalorExternal ='  ' then InternalorExternal='UN';
run;

data stand3;
set stand2;
if inst="sch" and CR_gained>0;
run;
  
proc summary data=stand3 nway;
		class snz_moe_uid  attainedyear InternalorExternal;
		var CR_gained;
		output out=credit_type_ach_sch (drop=_type_ _freq_) sum=;
run;

**Code for summing internal and external credits separately - this section can be skipped;

data credit_type_ach_sch2;
set credit_type_ach_sch;
if InternalorExternal='IN' then int_cr=CR_gained;
if InternalorExternal='EX' then ext_cr=CR_gained;
*if InternalorExternal='UN' then unk_cr=CR_gained;
run;

proc summary data=credit_type_ach_sch2 nway;
class snz_moe_uid attainedyear;
var int_cr ext_cr;
		output out=credit_type_ach_sch3 (drop=_type_ _freq_) sum=;
run;

proc format;
value credits
0-<.1='aLT10%'
.1-<.35='bLT35%'
.35-high='cGT35%';
run;

%let m=2003;
%let n=2015;
data credits_external(keep=snz_moe_uid cr_ext_&m-cr_ext_&n cr_tot_&m-cr_tot_&n);
set credit_type_ach_sch3;
array extcr(*) cr_ext_&m-cr_ext_&n;
array totcr(*) cr_tot_&m-cr_tot_&n;
if ext_cr=. then ext_cr=0;
if int_cr=. then int_cr=0;
tot_cr=sum(of int_cr, ext_cr);
do ind=1 to dim(extcr);
       i=ind+(&m-1);
	   if attainedyear=i then do;
        extcr(ind)=ext_cr;
		totcr(ind)=tot_cr;
       end;
end;
run;

proc summary data=credits_external nway;
		class snz_moe_uid ;
		var cr_ext_&m-cr_ext_&n cr_tot_&m-cr_tot_&n;
		output out=credits_external2 (drop=_type_ _freq_) sum=;
run;

**Back to summing all credits regardless of type;
proc summary data=stand3 nway;
		class  snz_moe_uid attainedyear Level;
		var CR_gained;
		output out=EDU_NQF_ach_sch (drop=_type_ _freq_) sum=;
run;

**I am ignoring NCEA level 4 credits here as there are so few of them;
data EDU_NQF_ach_sch_2;
	set EDU_NQF_ach_sch;
	array credits (13,3) 
         ncea_cr_2003_L1-ncea_cr_2003_L3
         ncea_cr_2004_L1-ncea_cr_2004_L3  ncea_cr_2005_L1-ncea_cr_2005_L3 
         ncea_cr_2006_L1-ncea_cr_2006_L3  ncea_cr_2007_L1-ncea_cr_2007_L3 
         ncea_cr_2008_L1-ncea_cr_2008_L3  ncea_cr_2009_L1-ncea_cr_2009_L3
         ncea_cr_2010_L1-ncea_cr_2010_L3  ncea_cr_2011_L1-ncea_cr_2011_L3
         ncea_cr_2012_L1-ncea_cr_2012_L3  ncea_cr_2013_L1-ncea_cr_2013_L3
         ncea_cr_2015_L1-ncea_cr_2015_L3   ncea_cr_2015_L1-ncea_cr_2015_L3;
    do j=1 to 3;
    do ind=1 to 13; i=ind+2002;
	   if attainedyear=i and level=j then credits(ind,j)=CR_gained;
	   end;
	   end;
   run;

proc summary data=EDU_NQF_ach_sch_2 nway;
class snz_moe_uid ;
var ncea_cr: ;
output out=credit_sum (drop=_type_ _freq_) sum=;
run;

%macro creditsum;
data ypypp.ncea_credit_sumb(keep=snz_moe_uid prop_cr_ext: tot_ncea_cr:);
merge credit_sum(in=a) credits_external2;  
by snz_moe_uid;
if a;
**Sum up externally assessed and total credits;
%do i=2003 %to 2015;
tot_ext_cr_by_&i=sum(of cr_ext_2003-cr_ext_&i);
tot_cr_by_&i=sum(of cr_tot_2003-cr_tot_&i);
if tot_ext_cr_by_&i~=. and tot_cr_by_&i~=. then do;
  prop_cr_ext_by_&i=tot_ext_cr_by_&i/tot_cr_by_&i;
  prop_cr_ext_by_&i._cat=put(prop_cr_ext_by_&i,credits. );
  end;
%end;
**Rename variables so the following operation will work;
array L1(*) ncea_cr_2003_L1 ncea_cr_2004_L1  ncea_cr_2005_L1 ncea_cr_2006_L1 ncea_cr_2007_L1  ncea_cr_2008_L1
          ncea_cr_2009_L1 ncea_cr_2010_L1 ncea_cr_2011_L1 ncea_cr_2012_L1 ncea_cr_2013_L1 ncea_cr_2015_L1 ncea_cr_2015_L1;
array L1a(*) a2003-a2015;
array L2(*) ncea_cr_2003_L2 ncea_cr_2004_L2 ncea_cr_2005_L2 
          ncea_cr_2006_L2 ncea_cr_2007_L2 ncea_cr_2008_L2 ncea_cr_2009_L2 ncea_cr_2010_L2 ncea_cr_2011_L2 
          ncea_cr_2012_L2 ncea_cr_2013_L2 ncea_cr_2015_L2 ncea_cr_2015_L2;
array L2b(*) b2003-b2015;
array L3(*) ncea_cr_2003_L3 ncea_cr_2004_L3 ncea_cr_2005_L3
   ncea_cr_2006_L3 ncea_cr_2007_L3 ncea_cr_2008_L3
   ncea_cr_2009_L3 ncea_cr_2010_L3 ncea_cr_2011_L3 ncea_cr_2012_L3 ncea_cr_2013_L3 ncea_cr_2015_L3 ncea_cr_2015_L3;
array L3c(*) c2003-c2015;
do i=1 to dim(L1);
   L1a(i)=L1(i);
   L2b(i)=L2(i);
   L3c(i)=L3(i);
end;
**Sum NCEA credits obtained up to the reference date;
%do i=2003 %to 2015;
  tot_ncea_cr_L1_by_&i=sum(of a2003-a&i);
  tot_ncea_cr_L2_by_&i=sum(of b2003-b&i);
  tot_ncea_cr_L3_by_&i=sum(of c2003-c&i);
%end;
run;

%mend creditsum;

%creditsum;


**NCEA QUALIFICATIONS OBTAINED;
Proc format;
value HA 
42='National Diploma at level 4 or above'
41='National Certificate at level 4 or above'
40='New Zealand Scholarship award'
39='NCEA level 3 (with Excellence)'
38='NCEA level 3 (with Merit)'
37='NCEA level 3 (with Achieve)'
36='NCEA level 3 (No Endorsement)'
35='Other NQF Qualification at level 3'
29='NCEA level 2 (with Excellence)'
28='NCEA level 2 (with Merit)'
27='NCEA level 2 (with Achieve)'
26='NCEA level 2 (No Endorsement)'
25='Other NQF Qualification at level 2'
19='NCEA level 1 (with Excellence)'
18='NCEA level 1 (with Merit)'
17='NCEA level 1 (with Achievement)'
16='NCEA level 1 (No Endorsement)'
15='Other NQF Qualification at level 1';

value HA_grouped
42,41,40='Level 4 Qualification or above'
39,38,37,36='NCEA level 3 Qualification'
35='Other NQF Qualification at level 3'
29,28,27,26='NCEA level 2 Qualification'
25='Other NQF Qualification at level 2'
19,18,17,16='NCEA level 1 Qualification'
15='Other NQF Qualification at level 1';

Value ha_grp
42,41,40,39,38,37,36,35='NCEA level 3 or above'
29,28,27,26,25='Level 2 Qualification'
19,18,17,16,15='NCEA level 1 Qualification'
0,.='No Formal NCEA Attainment';
run;

* FORMATING DATES;
proc contents data=sandmoe.student_qual;run;

data student_qual; 
set sandmoe.student_qual;
	format nzqaloadeddate1 date9.;
	qual=qual_code;
	result=exam_result_code;
	awardingschool=award_provider_code;
	level=nqf_level_code;
	year=attained_year_nbr;
	end_year=endorsed_year_nbr;
	nzqaloadeddate1=input(compress(nzqa_load_date,"-"),yymmdd10.);
	load_year=year(nzqaloadeddate1);
run;

proc sql;
create table sec_qual as
select distinct
      a.*
from student_qual a 
   inner join &population b
on a.snz_moe_uid=b.snz_moe_uid
order by snz_moe_uid, year;
quit;

proc sql;
create table sec_qual as select
a.*, b.snz_uid
from sec_qual a left join &population. b
on a.snz_moe_uid = b.snz_moe_uid;
quit;

data qual_lookup;
	set sandmoe.moe_qualification_lookup;
	rename qualificationtableid=qual;
run;

proc sort data=qual_lookup;
	by qual;
run;

proc sort data=sec_qual;
	by qual;
run;


* DEFINE NCEA ATTAINMENT: 
* limiting to national certificates which is 99& percent of records;
* excluding qualifications gained prior to 2006, before NCEA time;

DATA QualsHA; 
merge sec_qual(in=a) qual_lookup(in=b); 
by qual; 
if a; 
HA=0;
* Allows 2 years for loading qualifications;
if year=load_year or load_year-year<=2 or load_year=.; 
if NQFlevel in (0,.) then delete;
if year < 2003 then delete; 
if year>=2003 and year<=2015;
if nqflevel >= 4 and QualificationType=21 then ha=41;
else if nqflevel >= 4 and QualificationType=10 then ha=40;
else if nqflevel >= 4 then ha=42;
else if qualificationcode='1039' and result='E' then HA=39;
else if qualificationcode='1039' and result='M' then HA=38;
else if qualificationcode='1039' and result='ZZ' then HA=37;
else if qualificationcode='1039' and result='N' then HA=36;
else if nqflevel=3 then HA=35;
else if (qualificationcode='0973' or qualificationcode='973') and result='E' then HA=29;
else if (qualificationcode='0973' or qualificationcode='973') and result='M' then HA=28;
else if (qualificationcode='0973' or qualificationcode='973') and result='ZZ' then HA=27;
else if (qualificationcode='0973' or qualificationcode='973') and result='N' then HA=26;
else if nqflevel=2 then HA=25;
else if (qualificationcode='0928' or qualificationcode='928') and result='E' then HA=19;
else if (qualificationcode='0928' or qualificationcode='928') and result='M' then HA=18;
else if (qualificationcode='0928' or qualificationcode='928') and result='ZZ' then HA=17;
else if (qualificationcode='0928' or qualificationcode='928') and result='N' then HA=16;
else if nqflevel=1 then HA=15;
NCEA_L1=0; NCEA_L2=0; NCEA_L3=0; non_NCEA_L1=0; non_NCEA_L2=0; non_NCEA_L3=0; non_NCEA_L4=0;
if HA in (19,18,17,16) then NCEA_L1=1;
else if HA=15 then non_NCEA_L1=1;

else if HA in (29,28,27,26) then NCEA_L2=1;
else if HA=25 then non_NCEA_L2=1;

else if HA in (39,38,37,36) then NCEA_L3=1;
else if HA=35 then non_NCEA_L3=1;
else if HA in (42,41,40) then non_NCEA_L4=1;
*keep snz_uid year HA NCEA_L1 NCEA_L2 NCEA_L3 non_NCEA_L1 non_NCEA_L2 non_NCEA_L3 non_NCEA_L4 ;
run;

**The codes for ncea with no endorsement (16, 26, 36) dont appear in the resulting data set in practive;

* Creating final dataset;
proc sql;
create table ind_NCEA_qual as select
snz_moe_uid,
snz_uid,
year,
max(NCEA_L1) as NCEA_L1,
max(NCEA_L2) as NCEA_L2,
max(NCEA_L3) as NCEA_L3,
max(non_NCEA_L1) as nonNCEA_L1,
max(non_NCEA_L2) as nonNCEA_L2,
max(non_NCEA_L3) as nonNCEA_L3,
max(non_NCEA_L4) as nonNCEA_L4
from QualsHA
group by snz_uid, year
order by snz_uid, year;
quit;

data ypypp.NCEA_qual;
set ind_NCEA_qual;
run;


**TERTIARY QUALS;
proc format;
	value $lv8id
		"40","41","46", "60", "96", "98"      ="1"
		"36"-"37","43"                        ="2"
		"30"-"35"                       ="3"
		"20","21","25"                       ="4"
		"12"-"14"                       ="6"
		"11"                            ="7"
		"01","10"                       ="8"
		"90", "97", "99"                ="9"
		Other                           ="E";
run;

proc contents data=sandmoe.tertiary_completions;
run;

proc sql;
	create table TER_compl as
		select  
			a.snz_moe_uid,
			a.year,
			put(a.qacc,$lv8id.) as att_TER_qual_type,
			a.NZQF_levelq as level_num,
			a.nzscedq			
		from sandmoe.tertiary_completions a inner join &population b
on a.snz_moe_uid=b.snz_moe_uid
where MDY(12,31,year)<='31Dec2015'd;
quit;

proc sql;
create table TER_compl as select
a.*, b.snz_uid
from TER_compl a left join &population. b
on a.snz_moe_uid = b.snz_moe_uid
order by b.snz_uid, a.year, a.level_num;
quit;

data TER_COMPL_; 
set TER_COMPL;
level=1*level_num;
if level=. then level=1*att_TER_qual_type;
field=substr(nzscedq,1,2);
run;

**Get dataset containing highest qualification completed per person per year;
proc sort data=TER_COMPL_; 
by snz_uid year descending level; 
run;

data ypypp.tertiary_quals(keep=snz_uid year level ); 
set TER_COMPL_; 
by snz_uid year descending level; 
if first.year;
run;

proc freq data=ypypp.tertiary_quals;
tables year /list missing;
run;


**Industry training qualifications;
data it deletes;
	set moenew.tec_it_learner;
if moe_itl_tot_credits_awarded_nbr>0 and moe_itl_sum_units_consumed_nbr>0;
if moe_itl_programme_type_code in ("NC","TC"); 
   **National certificate or Trade certificate - but nearly all are NC;
   *Limited credit programmes, Supplementary credit programmes, and records with missing
   prog type are not selected;
format startdate enddate  date9.;
	startdate=input(compress(moe_itl_start_date,"-"),yymmdd10.);
    startyr=year(startdate);
	if moe_itl_end_date ne '' then
		enddate=input(compress(moe_itl_end_date,"-"),yymmdd10.);
	if moe_itl_end_date=' ' then do;
        it_enddate_imputed=1;
        enddate=input((strip(31)||strip(12)||strip(startyr)),ddmmyy8.);
		end;
	if startdate>enddate then
		output deletes;
	else output it;
run;

proc freq data=it;
tables startyr /list missing;
run;

proc sql;
	create table itl_event as 
		SELECT distinct
snz_moe_uid,
moe_itl_fund_code,
startdate,
enddate,
it_enddate_imputed
,moe_itl_level1_qual_awarded_nbr as L1
			,moe_itl_level2_qual_awarded_nbr as L2
			,moe_itl_level3_qual_awarded_nbr as L3
			,moe_itl_level4_qual_awarded_nbr as L4
			,moe_itl_level5_qual_awarded_nbr as L5
			,moe_itl_level6_qual_awarded_nbr as L6
			,moe_itl_level7_qual_awarded_nbr as L7
			,moe_itl_level8_qual_awarded_nbr as L8
   FROM IT 
   WHERE snz_moe_uid IN (select distinct snz_moe_uid from &population)
   order by snz_moe_uid, startdate;
quit;

proc sql;
create table itl_event as select
a.*, b.snz_uid
from itl_event a left join &population. b
on a.snz_moe_uid = b.snz_moe_uid
order by b.snz_uid, a.startdate;
quit;

data IT_quals(keep=snz_uid /*startdate enddate it_enddate_imputed*/ year level); 
set itl_event;
level=0;
	if L1=1 then level=1;
	if L2=1 then level=2;
	if L3=1 then level=3;
	if L4=1 then level=4;
	if L5=1 then level=5;
	if L6=1 then level=6;
	if L7=1 then level=7;
	if L8=1 then level=8;
qual_type='ITL';
if level>0;
year=year(enddate);
run;

proc summary data=it_quals nway;
class snz_uid year;
var level;
output out=ypypp.IT_quals(drop= _:) max=;
run;

************************************************;
***Identify highest qualification level at each year end;
*************Combine school (NCEA and nonNCEA) and tertiary and industry training qualification attainment;
***********************************************;
%let y1=2006;  *Year 1;
%let yn=2015;    *Year n;
%let z=2005;  *y1-1;

data quals;
merge ypypp.NCEA_qual(keep=snz_uid year NCEA_L1 NCEA_L2 NCEA_L3 nonNCEA_L1 nonNCEA_L2  nonNCEA_L3 nonNCEA_L4) 
     ypypp.tertiary_quals(keep=snz_uid year level rename=(level=tert_level))
     ypypp.it_quals(keep=snz_uid year level rename=(level=it_level));
by snz_uid year;
**Highest per year;
array NCEA (*) ncea_&y1-ncea_&yn;
array nonNCEA (*) nonncea_&y1-nonncea_&yn;
array tert (*) terqual_&y1-terqual_&yn;
array it(*) itqual_&y1-itqual_&yn;
do i=1 to dim(NCEA);
   NCEA(i)=0;
   nonNCEA(i)=0;
   tert(i)=0;
   it(i)=0;
   end;
do i=1 to dim(NCEA);
   if year=(i+&z) and NCEA_L1 =1 then NCEA(i)=1;
   if year=(i+&z) and NCEA_L2 =1 then NCEA(i)=2;
   if year=(i+&z) and NCEA_L3 =1 then NCEA(i)=3;
   if year=(i+&z) and nonNCEA_L1 =1 then nonNCEA(i)=1;
   if year=(i+&z) and nonNCEA_L2 =1 then nonNCEA(i)=2;
   if year=(i+&z) and nonNCEA_L3 =1 then nonNCEA(i)=3;
   if year=(i+&z) and nonNCEA_L4 =1 then nonNCEA(i)=4;

   if year=(i+&z) and tert_level =1 then tert(i)=1;
   if year=(i+&z) and tert_level =2 then tert(i)=2;
   if year=(i+&z) and tert_level =3 then tert(i)=3;
   if year=(i+&z) and tert_level =4 then tert(i)=4;
   if year=(i+&z) and tert_level >=5 then tert(i)=5;
   if year=(i+&z) and it_level =1 then it(i)=1;
   if year=(i+&z) and it_level =2 then it(i)=2;
   if year=(i+&z) and it_level =3 then it(i)=3;
   if year=(i+&z) and it_level =4 then it(i)=4;
end;
run;

**Get max qualification level for each of the 3 types;
proc summary data=quals nway;
class snz_uid;
var ncea_&y1-ncea_&yn nonncea_&y1-nonncea_&yn  terqual_&y1-terqual_&yn  itqual_&y1-itqual_&yn;
output out=stats max=;
run;

/*
proc means data=stats;
var ncea_&y1-ncea_&yn nonncea_&y1-nonncea_&yn  terqual_&y1-terqual_&yn  itqual_&y1-itqual_&yn;
run;
*/

**Variables starting with an h show the highest level that had been achieved, before or during each reference year;

data ypypp.highest_quals_achieved(keep=snz_uid hqual_&y1-hqual_&yn 
         hncea_&y1-hncea_&yn  hnonncea_&y1-hnonncea_&yn
         hterqual_&y1-hterqual_&yn  hitqual_&y1-hitqual_&yn);
set stats;
array NCEA (*) ncea_&y1-ncea_&yn;
array hncea(*) hncea_&y1-hncea_&yn;
array nonNCEA (*) nonncea_&y1-nonncea_&yn;
array hnonNCEA (*) hnonncea_&y1-hnonncea_&yn;
array tert (*) terqual_&y1-terqual_&yn; 
array htert (*) hterqual_&y1-hterqual_&yn;
array it(*) itqual_&y1-itqual_&yn;
array hit(*) hitqual_&y1-hitqual_&yn;
array hqual(*) hqual_&y1-hqual_&yn;
do i=1 to dim(NCEA);   
    hqual(i)=max(of NCEA(i), nonNCEA(i), tert(i), it(i));
    hncea(i)=ncea(i);
	hnonncea(i)=nonncea(i);
	htert(i)=tert(i);
	hit(i)=it(i);
	end;
do i=2 to dim(NCEA);
  if hncea(i)<hncea(i-1) then hncea(i) = hncea(i-1);
  if hnonncea(i)<hnonncea(i-1) then hnonncea(i) = hnonncea(i-1);
  if htert(i)<htert(i-1) then htert(i) = htert(i-1);
  if hit(i)<hit(i-1) then hit(i) = hit(i-1); 
  if hqual(i)<hqual(i-1) then hqual(i) = hqual(i-1);
end;
run;


***OVERSEAS DAYS PER MONTH;
proc sql;
create table os_spells
as select snz_uid, 
   datepart(pos_applied_date) format date9.  as startdate, 
   datepart(pos_ceased_date) format date9. as enddate
from datanew.person_overseas_spell
where snz_uid IN 
             (SELECT DISTINCT snz_uid FROM &population ) 
order by snz_uid, startdate;
quit;

data os_spells2;
set os_spells;
	if year(enddate)=9999 then enddate='31Dec2015'd;
	if year(startdate)=1900 then startdate='30Jun1997'd;
run;

proc sort data=os_spells2;
by snz_uid startdate enddate;
run;

**Count all days spent overseas in each calendar month from Jan 2004 to Jun 2015;
**Use LEED dates to index each month;

%let m=58;
%let n=199;

data os_spells3(drop=i start_window end_window days);
set os_spells2;
start='01Jan2004'd; 
array osdays [*] os_da_&m-os_da_&n ; * days os;
do i=1 to dim(osdays);
   start_window=intnx('month',start,i-1,'S');
   end_window=(intnx('month',start,i,'S'))-1;
   format start_window end_window date9.;  
   if not((startdate > end_window) or (enddate < start_window)) then do;	              
		            if (startdate <= start_window) and  (enddate > end_window) then days=(end_window-start_window)+1;
		            else if (startdate <= start_window) and  (enddate <= end_window) then days=(enddate-start_window)+1;
		            else if (startdate > start_window) and  (enddate <= end_window) then days=(enddate-startdate)+1;
		            else if (startdate > start_window) and  (enddate > end_window) then days=(end_window-startdate)+1;     	     
		            osdays[i]=days;				   
		         end;
	end;	          
run;

proc summary data=os_spells3 nway;
class snz_uid;
var os_da_&m-os_da_&n;
output out=ypypp.mth_os(drop=_:)  sum=;
run;

proc means data=ypypp.mth_os;
var os_da_&m-os_da_&n;
run;

*****************************************************************;
***ANY CUSTODIAL OR COMMUNITY SENTENCES SERVED;
**by month - from Jan 2006 to June 2015;
***************************************************************;
proc sql;
	create table COR as
		SELECT distinct 
		 snz_jus_uid,
			input(cor_mmp_period_start_date,yymmdd10.) format date9. as startdate,
			input(cor_mmp_period_end_date, yymmdd10.) format date9. as enddate,
			cor_mmp_mmc_code,  
           /* Creating wider correction sentence groupings */
	    	(case when cor_mmp_mmc_code in ('PRISON','REMAND' ) then 'Custody'
			     /*when cor_mmp_mmc_code in ('HD_SENT','HD_SENT', 'HD_rel' ) then 'COR_HD'*/
                 /*when cor_mmp_mmc_code in ('ESO','PAROLE','ROC','PDC' ) then 'COR_Post_Re'*/
				 when cor_mmp_mmc_code in ('HD_SENT','HD_SENT', 'HD_REL' 'COM_DET','CW','COM_PROG',
                 'COM_SERV' ,'OTH_COM','INT_SUPER','SUPER','PERIODIC') then 'Comm'
                 else 'COR_OTHER' end) as sentence 
		FROM CORnew.ov_major_mgmt_periods 
		where snz_jus_uid in (SELECT DISTINCT jus_uid_new FROM &population) 
		AND cor_mmp_mmc_code IN ('PRISON','REMAND','HD_SENT','HD_REL','PERIODIC',
			'COM_DET','CW','COM_PROG','COM_SERV','OTH_COM','INT_SUPER','SUPER')        
		ORDER BY snz_jus_uid,startdate;
quit;

/*data cor_check;
	set CORnew.ov_major_mgmt_periods;
	where snz_jus_uid in (19622,39788,47515);
run;
data cor_checkold;
	set COR.ov_major_mgmt_periods;
	where snz_jus_uid in (19622,39788,47515);
run;


proc freq data=COR.ov_major_mgmt_periods;
	tables cor_mmp_mmc_code;
run;
proc freq data=CORnew.ov_major_mgmt_periods;
	tables cor_mmp_mmc_code;
run;*/

proc sql;
create table COR_1 as select
a.* ,
b.snz_uid
from COR a left join &population b
on a.snz_jus_uid=b.jus_uid_new
where a.snz_jus_uid IN 
             (SELECT DISTINCT jus_uid_new FROM &population ) 
order by snz_uid,startdate;
quit;

%OVERLAP (COR_1); 

%let m=82;  *Jan 2006;
%let n=201;   *Dec 2015;

data cor_spells(drop=i start_window end_window days);
set COR_1_OR;
start='01Jan2006'd; 
array custdays [*] cust_da_&m-cust_da_&n ; 
array commdays [*] comm_da_&m-comm_da_&n ; 
do i=1 to dim(custdays);
   start_window=intnx('month',start,i-1,'S');
   end_window=(intnx('month',start,i,'S'))-1;
   format start_window end_window date9.;  
   if not((startdate > end_window) or (enddate < start_window)) and sentence='Custody' then do;	              
		            if (startdate <= start_window) and  (enddate > end_window) then days=(end_window-start_window)+1;
		            else if (startdate <= start_window) and  (enddate <= end_window) then days=(enddate-start_window)+1;
		            else if (startdate > start_window) and  (enddate <= end_window) then days=(enddate-startdate)+1;
		            else if (startdate > start_window) and  (enddate > end_window) then days=(end_window-startdate)+1;     	     
		            custdays[i]=days;	                 
		         end;
   if not((startdate > end_window) or (enddate < start_window)) and sentence='Comm' then do;	              
		            if (startdate <= start_window) and  (enddate > end_window) then days=(end_window-start_window)+1;
		            else if (startdate <= start_window) and  (enddate <= end_window) then days=(enddate-start_window)+1;
		            else if (startdate > start_window) and  (enddate <= end_window) then days=(enddate-startdate)+1;
		            else if (startdate > start_window) and  (enddate > end_window) then days=(end_window-startdate)+1;     	     
		            commdays[i]=days;	                 
		         end;
	end;	          
run;

proc summary data=cor_spells nway;
class snz_uid;
var cust_da_&m-cust_da_&n comm_da_&m-comm_da_&n ; 
output out=corr(drop=_:)  sum=;
run;

data ypypp.mth_corrections(drop=i);
set corr;
array custdays [*] cust_da_&m-cust_da_&n ; 
array commdays [*] comm_da_&m-comm_da_&n ; 
array cust [*] cust_id_&m-cust_id_&n ; 
array comm [*] comm_id_&m-comm_id_&n ; 
do i=1 to dim(custdays);
   if custdays(i)>0 then cust(i)=1; else cust(i)=0;
   if commdays(i)>0 then comm(i)=1; else comm(i)=0;
   end;
run;

******************************************;
****STUDENT ALLOWANCE RECEIPT;
**I choose to select data from 2006 onwards;
**when kids are about 11 or 12;
******************************************;

%macro select(year);
proc sql;
create table studall&year as 
   select snz_ird_uid, 
    inc_cal_yr_year_nbr as year,
      sum(inc_cal_yr_mth_01_amt) as sa1,
	  sum(inc_cal_yr_mth_02_amt) as sa2,
      sum(inc_cal_yr_mth_03_amt) as sa3,
      sum(inc_cal_yr_mth_04_amt) as sa4,
      sum(inc_cal_yr_mth_05_amt) as sa5,
	  sum(inc_cal_yr_mth_06_amt) as sa6,
      sum(inc_cal_yr_mth_07_amt) as sa7,
      sum(inc_cal_yr_mth_08_amt) as sa8,
      sum(inc_cal_yr_mth_09_amt) as sa9,
      sum(inc_cal_yr_mth_10_amt) as sa10,
      sum(inc_cal_yr_mth_11_amt) as sa11,
      sum(inc_cal_yr_mth_12_amt) as sa12   
    from datanew.income_cal_yr
    where inc_cal_yr_income_source_code = 'STU' and inc_cal_yr_year_nbr=&year and snz_ird_uid in (SELECT DISTINCT snz_ird_uid FROM &population) 
    group by snz_ird_uid, year
    order by snz_ird_uid;
quit;

%mend select;

%select(2006);
%select(2007);
%select(2008);
%select(2009);
%select(2010);
%select(2011);
%select(2012);
%select(2013);
%select(2014);
%select(2015);

%let m=82;
%let n=201;
data mth_studall(keep=snz_ird_uid sa&m-sa&n );
merge 
  studall2006(keep=snz_ird_uid sa1-sa12 rename=(sa1-sa12=sa82-sa93))
  studall2007(keep=snz_ird_uid sa1-sa12 rename=(sa1-sa12=sa94-sa105))
  studall2008(keep=snz_ird_uid sa1-sa12 rename=(sa1-sa12=sa106-sa117)) 
  studall2009(keep=snz_ird_uid sa1-sa12 rename=(sa1-sa12=sa118-sa129))
  studall2010(keep=snz_ird_uid sa1-sa12 rename=(sa1-sa12=sa130-sa141))
  studall2011(keep=snz_ird_uid sa1-sa12 rename=(sa1-sa12=sa142-sa153))
  studall2012(keep=snz_ird_uid sa1-sa12 rename=(sa1-sa12=sa154-sa165)) 
  studall2013(keep=snz_ird_uid sa1-sa12 rename=(sa1-sa12=sa166-sa177))
  studall2014(keep=snz_ird_uid sa1-sa12 rename=(sa1-sa12=sa178-sa189))
  studall2015(keep=snz_ird_uid sa1-sa12 rename=(sa1-sa12=sa190-sa201));
  by snz_ird_uid;
run;

data mth_studall(drop=i);
set mth_studall;
array stud(*) sa&m-sa&n;
do i=1 to dim(stud);
   if stud(i)>0 then stud(i)=1;
   else stud(i)=0;
end;
run;

proc sql;
create table ypypp.mth_studall as select
a.*, b.snz_uid
from mth_studall a left join &population. b
on a.snz_ird_uid = b.snz_ird_uid
order by snz_uid;
quit;

proc sort data=ypypp.mth_studall nodupkey;
	by snz_uid;
run;


****************************************;
**Monthly employment, enrolment history, benefit receipt history and inactivity history prior to the refmth;
****************************************;
*195-58=197-60=137 +1=138+6=144;
%let k=58;
%let m=82;
%let p=189;
%let z=201;

data ypypp.monthly_ext(drop=sch_enr_id_&k-sch_enr_id_&z rearn&k-rearn&z ter_enr_id_&k-ter_enr_id_&z emp&k-emp&z ben_id_&k-ben_id_&z os_da_&k-os_da_&z cust_id_&k-cust_id_&z comm_id_&k-comm_id_&z sa&k-sa&z) zzz;
	merge ypypp.all_popn(in=a keep=snz_uid refmth)
		ypypp.mth_sch_enrol(in=b keep=snz_uid sch_enr_id_&k-sch_enr_id_&z)			/*58 to 201 */
		ypypp.mth_ter_enrol(in=c keep=snz_uid ter_enr_id_&m-ter_enr_id_&z)        	/*82 to 201 */
		ypypp.mth_emp(in=d keep=snz_uid emp&m-emp&z rearn&m-rearn&z)				/*82 to 201 */
		ypypp.mth_ben(in=f keep=snz_uid ben_id_&m-ben_id_%eval(&z-1))				/*82 to 200 */
        ypypp.mth_os(in=g keep=snz_uid os_da_&k-os_da_%eval(&z-2))					/*58 to 199*/
		ypypp.mth_corrections(in=h keep=snz_uid cust_id_&m-cust_id_&z comm_id_&m-comm_id_&z)
		ypypp.mth_studall(in=j keep=snz_uid sa&m.-sa&z.)
		;
	by snz_uid;
	
	array sch(*) sch_enr_id_&k-sch_enr_id_&z;
	array emp(*) emp&k-emp&z;
	array emp_mw(*) emp_mw&k-emp_mw&z;
	array tert(*) ter_enr_id_&k-ter_enr_id_&z;
	array ben(*) ben_id_&k-ben_id_&z;
	array os(*) os_da_&k-os_da_&z;
	array earn(*) rearn&k-rearn&z;
	array cust(*) cust_id_&k-cust_id_&z;  
	array comm(*) comm_id_&k-comm_id_&z;  
	array sa(*) sa&k-sa&z;  

	if not b then do i=&k. to &z.;
		sch{i-57}=0;
	end;
	if not c then do i=&m. to &z.;
		tert{i-57}=0;
	end;
	if not d then do i=&m. to &z.;
		emp{i-57}=0;
	end;
	if not f then do i=&m. to %eval(&z-1);
		ben{i-57}=0;
	end;
	if not g then do i=&k. to %eval(&z-2);
		os{i-57}=0;
	end;
	if not h then do i=&m. to &z.;
		cust{i-57}=0;
		comm{i-57}=0;
	end;
	if not j then do i=&m. to &z.;
		sa{i-57}=0;
	end;

	do i=&k. to &z.;
		if  82 <= i <=  93 then do; if earn(i-57)>1312 then emp_mw(i-57)=1; else emp_mw(i-57)=0; end;
		else if  94 <= i <=  105 then do; if earn(i-57)>1440 then emp_mw(i-57)=1; else emp_mw(i-57)=0; end;
		else if  106 <= i <= 117 then do; if earn(i-57)>1536 then emp_mw(i-57)=1; else emp_mw(i-57)=0; end;
		else if  118 <= i <= 129 then do; if earn(i-57)>1600 then emp_mw(i-57)=1; else emp_mw(i-57)=0; end;
		else if  130 <= i <= 141 then do; if earn(i-57)>1632 then emp_mw(i-57)=1; else emp_mw(i-57)=0; end;
		else if  142 <= i <= 153 then do; if earn(i-57)>1664 then emp_mw(i-57)=1; else emp_mw(i-57)=0; end;
		else if  154 <= i <= 165 then do; if earn(i-57)>1728 then emp_mw(i-57)=1; else emp_mw(i-57)=0; end;
		else if  166 <= i <= 177 then do; if earn(i-57)>1760 then emp_mw(i-57)=1; else emp_mw(i-57)=0; end;
		else if  178 <= i <= 189 then do; if earn(i-57)>1824 then emp_mw(i-57)=1; else emp_mw(i-57)=0; end;
		else if  190 <= i <= 201 then do; if earn(i-57)>1888 then emp_mw(i-57)=1; else emp_mw(i-57)=0; end;
	end;

	array nsch(*) sch1-sch79;
	array nemp(*) nemp1-nemp79;
	array nemp_mw(*) nemp_mw1-nemp_mw79;
	array ntert(*) tert1-tert79;
	array nben(*) ben1-ben79;
	array nearn(*) earn1-earn79;
    array nos(*) nos1-nos79;
	array ncust(*) cust1-cust79;
	array ncomm(*) comm1-comm79;
	array nsa(*) nsa1-nsa79;

	array study(*) study1-study79;
	array neet(*) neet1-neet79;
	array eet(*) eet1-eet79;

	**Select the data for the 48 months before the refmth, the refmth and the next 36 months;
	windowstart_leed=refmth-48;

	do i=1 to 79;
		if 1 <= i+(windowstart_leed-57)-1 <= 144 then do;
			nemp(i)=emp(i+(windowstart_leed-57)-1); 
			nemp_mw(i)=emp_mw(i+(windowstart_leed-57)-1); 
			nearn(i)=earn(i+(windowstart_leed-57)-1); 
			nsch(i)=sch(i+(windowstart_leed-57)-1); 
			nben(i)=ben(i+(windowstart_leed-57)-1); 
			ncust(i)=cust(i+(windowstart_leed-57)-1); 
			ncomm(i)=comm(i+(windowstart_leed-57)-1); 
			nsa(i)=sa(i+(windowstart_leed-57)-1); 
			nos(i)=os(i+(windowstart_leed-57)-1); 
			if nos(i)>=7 then nos(i)=1; * longer than a week overseas that month; 
				else nos(i)=0;
			ntert(i)=tert(i+(windowstart_leed-57)-1); 
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

	if a then output ypypp.monthly_ext;
	else output zzz;
run;

****************************************************;
**PART E: Construct post-YS OUTCOME VARIABLES for everyone in study and potential controls;
**Using monthly data on enrolment status, employment, benefit receipt and NEET status
 and annual data on educational achievement;
*******************************************************;
%let m=55;
%let n=61;
%let o=67;
%let p=73;
%let q=79;

data depvars_ext(compress=y keep=snz_uid refmth emp_post6 emp_post12 emp_post18 emp_post24 emp_post30 study_post6 study_post12 study_post18 study_post24 study_post30 sch_post6 sch_post12 sch_post18 sch_post24 sch_post30
               neet_post6 neet_post12 neet_post18 neet_post24 neet_post30 eet_post6 eet_post12 eet_post18 eet_post24 eet_post30 ben_post6 ben_post12 ben_post18 ben_post24 ben_post30
               tert_post6 tert_post12 tert_post18 tert_post24 tert_post30 cust_post6 cust_post12 cust_post18 cust_post24 cust_post30 comm_post6 comm_post12 comm_post18 comm_post24 comm_post30
			   sa_post6 sa_post12 sa_post18 sa_post24 sa_post30 os_post6 os_post12 os_post18 os_post24 os_post30 emp_mw_post12 emp_mw_post18 emp_mw_post24 emp_mw_post30
			   cust_in_followup comm_in_followup);
 	set ypypp.monthly_ext;
    array base(*) nemp&m nemp&n nemp&o nemp&p nemp&q study&m study&n study&o study&p study&q sch&m sch&n sch&o sch&p sch&q
                neet&m neet&n neet&o neet&p neet&q eet&m eet&n eet&o eet&p eet&q ben&m ben&n ben&o ben&p ben&q tert&m tert&n tert&o tert&p tert&q
                cust&m cust&n cust&o cust&p cust&q comm&m comm&n comm&o comm&p comm&q nsa&m nsa&n nsa&o nsa&p nsa&q  nos&m nos&n nos&o nos&p nos&q
				nemp_mw&m nemp_mw&n nemp_mw&o nemp_mw&p nemp_mw&q;
    array outcome(*) emp_post6 emp_post12 emp_post18 emp_post24 emp_post30 study_post6 study_post12 study_post18 study_post24 study_post30 sch_post6 sch_post12 sch_post18 sch_post24 sch_post30
               	neet_post6 neet_post12 neet_post18 neet_post24 neet_post30 eet_post6 eet_post12 eet_post18 eet_post24 eet_post30 ben_post6 ben_post12 ben_post18 ben_post24 ben_post30
               	tert_post6 tert_post12 tert_post18 tert_post24 tert_post30 cust_post6 cust_post12 cust_post18 cust_post24 cust_post30 comm_post6 comm_post12 comm_post18 comm_post24 comm_post30
			   	sa_post6 sa_post12 sa_post18 sa_post24 sa_post30 os_post6 os_post12 os_post18 os_post24 os_post30 
			 	emp_mw_post6 emp_mw_post12 emp_mw_post18 emp_mw_post24 emp_mw_post30;
  	do i=1 to dim(base);
   	  outcome(i)=base(i);
  	end;
	if sum(of cust50-cust73)>0 then cust_in_followup=1; else cust_in_followup=0;
	if sum(of comm50-comm73)>0 then comm_in_followup=1; else comm_in_followup=0;
run;

data ypypp.depvars_ext(compress=y drop=i j hqual_2006-hqual_2015 hncea_2006-hncea_2015
									hnonncea_2006-hnonncea_2015 hterqual_2006-hterqual_2015);
	merge depvars_ext(in=a)
		ypypp.highest_quals_achieved(keep=snz_uid hncea_2006-hncea_2015 hnonncea_2006-hnonncea_2015 hterqual_2006-hterqual_2015 hqual_2006-hqual_2015);
	by snz_uid;
	if a;
	array highqual(*) hncea_2006-hncea_2015 hnonncea_2006-hnonncea_2015 hterqual_2006-hterqual_2015  hqual_2006-hqual_2015;    
	do i=1 to dim(highqual);
		if highqual(i)=. then
			highqual(i)=0;
	end;

	**Highest qualifications completed, ordered by reference year where 0 =year before YS participation
		  and 1= year of YS participation;
	array old(4,9) hncea_2007-hncea_2015 hnonncea_2007-hnonncea_2015
	               hterqual_2007-hterqual_2015 hqual_2007-hqual_2015;
	array new(4,4) hncea_y0-hncea_y3 hnonncea_y0-hnonncea_y3
	               hterqual_y0-hterqual_y3 hqual_y0-hqual_y3;
	if 106 <= refmth < 118 then do j=1 to 4; /*2008*/
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
	else if 154 <= refmth < 166 then do j=1 to 4; /* 2012 */
	             do i=1 to 4;
	           new(j,i)=old(j,i+4);
	           end;
			   end;
	else if 166 <= refmth < 178 then do j=1 to 4; /*2013*/
	             do i=1 to 4;
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
run;