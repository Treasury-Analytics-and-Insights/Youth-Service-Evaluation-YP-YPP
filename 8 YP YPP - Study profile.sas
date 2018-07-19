/* 8. YP YPP - Study profile */
/* Describe TERTIARY enrolment and achievement after first YS enrolment*/
/* Construct other measures of YS enrolment duration and study patterns during YS enrolment*/;
/* Keith March 2016 */
/* Based on code by Sylvia March 2016 */

** Key data inputs:
*		- ypiyb.rep0_yp_1 / ypiyb.rep0_iyb_1 / yppdpb.rep0_ypp_1 / yppdpb.rep0_dpbema_1 (participant/historical comparison and matched comparison data)
*		- moe.enrolment (tertiary enrolment data)
*		- moe.completion (tertiary completion data)
*		- moe.student_enrol (school enrolment data)
*		- project.schoolprofile_open_&version. (school profile information)
*		- ys5.monthly_revised / yts.monthly_revised (monthly data to identify industry training)
*		- ys.highest_quals_achieved (highest qualifications)
;

proc datasets lib=work kill;
run;

proc format;
value efts
0-<0.5='LT0.5EFTS'
0.5-<1.0='0.5-<1.0EFTS'
1.0-<2.0='1.0-<2.0EFTS'
2.0-high='2.0+EFTS';

value $lv8id
		"40","41","46", "60", "96", "98"="1"
		"36"-"37","43"                  ="2"
		"30"-"35"                       ="3"
		"20","21","25"                  ="4"
		"12"-"14"                       ="6"
		"11"                            ="7"
		"01","10"                       ="8"
		"90", "97", "99"                ="9"
		Other                           ="E";

value $field
'  ','00'=	'No Qualification'	
'01'=	'Natural and Physical Sciences'	
'02'=	'Information Technology'
'03'=	'Engineering and Related Technologies'	
'04'=	'Architecture and Building'	
'05'=	'Agriculture, Environmental and Related Studies'	
'06'=	'Health'	
'07'=	'Education'	
'08'=	'Management and Commerce'	
'09'=	'Society and Culture'	
'10'=	'Creative Arts'	
'11'=	'Food, Hospitality and Personal Services'	
'12'=	'Mixed Field Programmes';

value level
.,0='None'
1='Level 1'
2='Level 2'
3='Level 3'
4-high='Level 4+';

value $subsector
		"1","3"="Universities"
		"2"="Polytechnics"
		"4"="Wananga"
		"5","6"="Private Training Establishments";

value $funding
'01', '25', '26'='Student component'
'22'='Youth Guarantee'
'02','03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '20', '21', '23', '24','27','28','29' ='Other funding';

value hqual
0=	'No Formal Attainment'
10=	'1–13 credits at Level 1'
13=	'Other Level 1 NQF Qualification'
14=	'NCEA Level 1  Not Further Defined'
15=	'NCEA Level 1  Achieved'
16=	'NCEA Level 1  with Merit'
17=	'NCEA Level 1  with Excellence'
20=	'1–13 credits at Level 2'
24=	'NCEA Level 2  Not Further Defined'
25=	'NCEA Level 2  Achieved'
26=	'NCEA Level 2  with Merit'
27=	'NCEA Level 2  with Excellence'
30=	'1–13 credits at Level 3'
33=	'Other Level 3 NQF Qualification'
34=	'NCEA Level 3  Not Further Defined'
35=	'NCEA Level 3  Achieved'
36=	'NCEA Level 3  with Merit'
37=	'NCEA Level 3  with Excellence'
4=	'Other Level 2 NQF Qualification'
40=	'3+ NZ Scholarship subjects'
43=	'National Certificate at Level 4'
51=	'14–39 credits at any level without Level 1 literacy and numeracy credits'
52=	'14–39 credits at any level including Level 1 literacy and numeracy credits'
53=	'40+ credits at any level without Level 1 literacy and numeracy credits'
54=	'40+ credits at any level including Level 1 literacy and numeracy credits'
55=	'30+ credits at Level 2 or above'
56=	'30+ credits at Level 3 or above'
60='International Baccalaureate Year 11'
61='International Baccalaureate Year 12'
62=	'International Baccalaureate Year 13'
70=	'Cambridge International Exams Year 11'
71=	'Cambridge International Exams Year 12'
72=	'Cambridge International Exams Year 13'
80=	'Accelerated Christian Education Year 11'
81=	'Accelerated Christian Education Year 12'
82=	'Accelerated Christian Education Year 13'
90=	'Other Overseas Awards Year 11'
91=	'Other Overseas Awards Year 12'
92=	'Other Overseas Awards Year 13';

value hqualb
0=	'No Formal Attainment'
4-56='NQF quals'
60-92='Overseas quals';
run;

data study_final;
length group $ 15;
set ypiyb.rep0_YP1(in=a keep=snz_uid _snz_uid refmth startdate treat pscore weight rename=(startdate=ysstartdate))
    ypiyb.rep0_iyb1(in=b keep=snz_uid _snz_uid refmth startdate treat pscore weight rename=(startdate=ysstartdate))
	yppdpb.rep0_YPP1(in=c keep=snz_uid _snz_uid refmth startdate treat pscore weight rename=(startdate=ysstartdate))
    yppdpb.rep0_dpbema1(in=d keep=snz_uid _snz_uid refmth startdate treat pscore weight rename=(startdate=ysstartdate));
	if a and treat then group='a YP';
	else if b and treat then group='b IYB';
	else if c and treat then group='c YPP 6+';
	else if d and treat then group='d DPBEMA 6+';
	else if a then group='e YP match';
	else if b then group='f IYB match';
	else if c then group='g YPP 6+ match';
	else if d then group='h DPBEMA 6+ match';
	*if treat=1 then output;
run;

proc sort data=study_final;
	by snz_uid group refmth;
run;

**TERTIARY PROGRAMMES ENROLLED IN DURING YS ENROLMENT;
**Get all tertiary enrolment records between first YS-NEET enrolment date and last enrolment date;
proc sql;
	create table enrolt as
		SELECT distinct 
			snz_uid
			,moe_enr_year_nbr as year
			,input(moe_enr_prog_start_date,yymmdd10.) format date9.  as startdate
			,input(moe_enr_prog_end_date,yymmdd10.) format date9.  as enddate  
			,sum(moe_enr_efts_consumed_nbr) as EFTS_consumed
			,moe_enr_efts_prog_years_nbr as EFTS_prog_yrs
			,moe_enr_efts_prog_nbr as EFTS_prog
			,moe_enr_qacc_code as qacc
			,moe_enr_qual_code as Qual
			,moe_enr_prog_nzsced_code as NZSCED
			,moe_enr_funding_srce_code as fund_source
			,moe_enr_subsector_code as subsector format $subsector.
			,moe_enr_qual_level_code as level_nbr
			,moe_enr_qual_type_code as qual_type
		FROM moe.enrolment 
			WHERE snz_uid IN (SELECT DISTINCT snz_uid FROM study_final)
					group by snz_uid, moe_enr_prog_start_date, moe_enr_prog_end_date, qual, NZSCED
						order by snz_uid;
quit;

**Select tertiary spells that were begun in the 24 months following YP/YPP enrolment;
proc sql;
	create table enrol_1 as
		select 
			a.snz_uid,
			a.ysstartdate as ys_start,
			a.refmth,
			a.group,
			b.*
		from study_final a inner join enrolt b
			on a.snz_uid=b.snz_uid 
			where (a.ysstartdate<=b.startdate<=(a.ysstartdate+730))
			order by a.snz_uid, a.group, b.startdate;
quit;

data enrol_2;
	set enrol_1;
	if EFTS_consumed>0 and qual_type="D" ; ** Formal quals; 
	level=1*level_nbr;
	dur=enddate-startdate;
	if dur>0;
	start_year=year(startdate);
	if NZSCED >'120000' then genskills_prog=1;
	else genskills_prog=0;
	if NZSCED <'120000' then occskills_prog=1;
	else occskills_prog=0;
    field=substr(NZSCED,1,2);
run;

**Select the first programme that was enrolled in after the YS startdate;
proc sort data=enrol_2;
by snz_uid group refmth startdate;
run;

data first;
set enrol_2;
by snz_uid group refmth startdate;
if first.refmth;
run;

data programmes;
merge study_final(in=a) first(keep=snz_uid group refmth level field EFTS_prog_yrs EFTS_prog subsector fund_source);
by snz_uid group refmth;
if a;
if level=. then level=0;
if level=0 then field='00';
run;

**Find QUALIFICATIONS obtained during the year of YP YPP start or in the following two years;
data tertiary2;
set study_final;
first_yr=year(ysstartdate);
sec_yr=first_yr+2;
run;

proc sql;
	create table TER_compl as
		select  
            b.snz_uid,
			b.group,
			b.refmth,
			b.ysstartdate,
			b.first_yr,
			b.sec_yr,
			a.moe_com_year_nbr as year,
			put(a.moe_com_qacc_code,$lv8id.) as att_TER_qual_type,
			a.moe_com_qual_level_code as level_num,
			a.moe_com_qual_nzsced_code			
		from moe.completion a inner join tertiary2 b
on a.snz_uid=b.snz_uid
where MDY(12,31,moe_com_year_nbr)<='31Dec2015'd
  and a.moe_com_year_nbr>=b.first_yr and a.moe_com_year_nbr<=b.sec_yr
order by b.snz_uid, b.group, a.moe_com_year_nbr, a.moe_com_qual_level_code;
quit;

data TER_COMPL_; 
set TER_COMPL;
level=1*level_num;
if level=. then level=1*att_TER_qual_type;
if level>4 then level=4;
field=substr(moe_com_qual_nzsced_code,1,2);
if field ='12' then genskills_qual=1;
	else genskills_qual=0;
	if '01'<=field<='11' then occskills_qual=1;
	else occskills_qual=0;
run;

proc sort data=TER_COMPL_; 
by snz_uid group refmth descending level;
run;

data highest;
set TER_COMPL_; 
by snz_uid group refmth;
if first.refmth;
run;

*** Merge onto the original YS participant dataset so we have a record for everyone who first enrolled in 2012 or 2013;
data quals1;
merge study_final(in=a) highest(keep=snz_uid group refmth field level);
by snz_uid group refmth;
if a;
if level=. then level=0;
run;


******************************************************;
**STATISTICS ABOUT YS ENROLMENT;
**Measures of the proportion who were enrolled at school and enrolled in tertiary during the YS enrolment
   - considering the first 24 months only;
******************************************************;

**Now find school enrolments between the YP YPP start date and 24 months later;
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
where snz_uid in (select distinct snz_uid from study_final) and moe_esi_start_date is not null 
order by snz_uid, startdate, enddate;
quit;

**Removing any overlaps in enrolment;
%OVERLAP (schenrol);

proc sql;
	create table enrol_1 as
		select 
			a.snz_uid,
			a.group,
			a.refmth,
			a.ysstartdate as ys_start,
			b.*
		from study_final a inner join schenrol_or b
			on a.snz_uid=b.snz_uid
			where 
			  (b.startdate<a.ysstartdate and b.enddate>a.ysstartdate+7) or
                (a.ysstartdate<=b.startdate<(a.ysstartdate+730)) 
			order by a.snz_uid, a.group, a.refmth, b.startdate;
quit;

data first;
set enrol_1;
by snz_uid group refmth;
if first.refmth;
run;

proc sort data=first;
	by schoolnbr;
run;

proc sql;
	create table profile
		as select 
			schoolnumber as schoolnbr
			,school_authority
			,schooltype
			,decile
		from project.schoolprofile_open_&version.
			where schoolnumber in (select distinct schoolnbr from first)
				order by schoolnbr;
quit;

data first;
	merge first(in=a) profile;
	by schoolnbr;
	if schoolnbr in (18,58,45,53,100,135,158,153,120,661,220,208,179,200,237,251,274,243,295,314,337,552) then schooltype='Teen Parent Unit';
	else if schooltype ne 'Correspondence School' then schooltype='Other';
run;

proc freq data=first;
	tables group*schooltype/nocol nopercent;
run;

proc sort data=first;
	by snz_uid group refmth;
run;

data participation_sch;
	merge study_final(in=a) first(in=b keep=snz_uid group refmth schooltype);
	by snz_uid group refmth ;
	if a;
	if b then school_enrolled=1; 
	else school_enrolled=0;
run;

**Any tertiary enrolment starting before YP YPP;
proc sql;
	create table enrol_t1 as
		select 
			a.snz_uid,
			a.group,
			a.refmth,
			a.ysstartdate as ys_start,
			b.*
		from study_final a inner join enrolt b
			on a.snz_uid=b.snz_uid
			where b.startdate<a.ysstartdate and b.enddate>(a.ysstartdate+7)               
			order by a.snz_uid, a.group, a.refmth, b.startdate;
quit;

**Limit to formal courses;
data enrol_t2;
	set enrol_t1;	
	if EFTS_consumed>0 and qual_type="D" ; ** Formal quals; 
	level=1*level_nbr;
	dur=enddate-startdate;
	if dur>0;	
run;

**Select first to get numbers;
data tert_before(keep=snz_uid group refmth);
set enrol_t2;
by snz_uid group refmth;
if first.refmth;
run;

**Any tertiary starting in the 24 months following YPP;
proc sql;
	create table enrol_t3 as
		select 
			a.snz_uid,
			a.group,
			a.refmth,
			a.ysstartdate as ys_start,
			b.*
		from study_final a inner join enrolt b
			on a.snz_uid=b.snz_uid
			where 
                a.ysstartdate<=b.startdate<(a.ysstartdate+730) 
			order by a.snz_uid, a.group, a.refmth, b.startdate;
quit;

data enrol_t4;
	set enrol_t3;	
	if EFTS_consumed>0 and qual_type="D" ; ** Formal quals; 
	level=1*level_nbr;
	dur=enddate-startdate;
	if dur>0;	
run;

**Select first to get numbers;
data tert_after(keep=snz_uid group refmth);
set enrol_t4;
by snz_uid group refmth;
if first.refmth;
run;

data tert_enrol;
merge study_final(in=a) tert_before(in=b) tert_after(in=c);
by snz_uid group refmth;
if a;
if b then tert_bf=1; else tert_bf=0;
if c then tert_af=1; else tert_af=0;
if b or c then tert=1; else tert=0;
run;

****Industry training - use monthly participation vector only;
**Dates of start and finish are not v exact;
proc sort data=study_final;
	by snz_uid group refmth;
run;

data it(drop=it:);
merge study_final(in=a keep=snz_uid group refmth) ys5.monthly_revised(in=b keep=snz_uid refmth it50-it67) yts.monthly_revised(in=c keep=snz_uid refmth it26-it43);
by snz_uid refmth;
if a;
if b and sum(of it50-it67)>0 then someit=1; 
else if c and sum(of it26-it43)>0 then someit=1; 
else someit=0;
run;

proc sort data=it;
	by snz_uid group refmth;
run;

data all;
merge participation_sch tert_enrol it;
by snz_uid group refmth;
if school_enrolled=1 or tert=1 or someit=1 then educ_or_train=1; else educ_or_train=0;
if school_enrolled=1 or tert=1 then study=1; else study=0;
if school_enrolled=0 and tert=0 then nostudy=1; else nostudy=0;
run;


****************************************************;
**Highest qualifications achieved in the first year of enrolment or the following 2 years;
***************************************************;
%let y1=2006;  *Year 1;
%let yn=2014;    *Year n;

data highest_quals(drop=hncea_2006-hncea_2014 hterqual_2006-hterqual_2014 hitqual_2006-hitqual_2014) nomatch;
	merge study_final(in=a) ys.highest_quals_achieved(in=b keep=snz_uid hncea_&y1-hncea_&yn  hterqual_&y1-hterqual_&yn  hitqual_&y1-hitqual_&yn);
	by snz_uid;
	if a;
	startyear=year(ysstartdate);
	if a and not b then output nomatch;
	**Highest qualifications completed, ordered by reference year where 0 =year before YS participation
		  and 1= year of YS participation;
	array old(4,8) hncea_2007-hncea_2014 hitqual_2007-hitqual_2014
	             hterqual_2007-hterqual_2014 hqual_2007-hqual_2014;
	array new(4,4) hncea_y0-hncea_y3 hitqual_y0-hitqual_y3
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

highest_ncea=max(hncea_y1,hncea_y2,hncea_y3);
highest_tert=max(hterqual_y1,hterqual_y2,hterqual_y3);
highest_it=max(hitqual_y1, hitqual_y2,hitqual_y3);
hqual_y1=max(hncea_y1,hterqual_y1,hitqual_y1);
hqual_y2=max(hncea_y2,hterqual_y2, hitqual_y2);
hqual_y3=max(hncea_y3,hterqual_y3, hitqual_y3);
highest_all=max(hqual_y1,hqual_y2,hqual_y3); 
output highest_quals;
run;


proc sort data=all;
	by snz_uid group refmth;
run;
proc sort data=programmes;
	by snz_uid group refmth;
run;
proc sort data=quals1;
	by snz_uid group refmth;
run;

data all_study;
	merge all programmes quals1(rename=(field=field_high level=level_high)) highest_quals;
	by snz_uid group refmth;
run;

**Results;
ods html body= 
"\\wprdfs08\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\ys code - 2016 update\output\Study profile all.xls";
**Whether enrolled at school, enrolled in tertiary, during the YS enrolment period;

proc tabulate data=all_study missing;
class group school_enrolled schooltype tert tert_bf tert_af educ_or_train study nostudy someit level field EFTS_prog_yrs EFTS_prog 
		subsector fund_source highest_ncea highest_tert highest_it  hqual_y1 hqual_y2 hqual_y3 highest_all level_high field_high;
tables (school_enrolled schooltype tert tert_bf tert_af educ_or_train study nostudy someit level field EFTS_prog_yrs EFTS_prog
		subsector fund_source highest_ncea highest_tert highest_it hqual_y1 hqual_y2 hqual_y3 highest_all level_high field_high ALL),group*weight*(SUM) /nocellmerge;
format  field field_high $field. EFTS_prog_yrs EFTS_prog efts. level level_high highest_ncea highest_tert highest_it hqual_y1 hqual_y2 hqual_y3 highest_all level. fund_source $funding.; 
var weight;
title "Study variables by participant group";
run;

ods html close;

