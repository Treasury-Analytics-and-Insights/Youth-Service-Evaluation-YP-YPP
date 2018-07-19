/* 1 YPP YP - Identify population */

** Define YPP and YP population and comparable pre-youth service participants;
**
** For YPP:
**  - Aged 15-18 at benefit start date.
**  - No partner or partner aged 16-18 also.
**  - Benefit type recorded as anything (apart from IB) prior to welfare reform and 603 with additional service data=YPP after.
**  - Child on benefit within +/- 90 days of benefit start.
**   
** For YP:
**  - Aged 15-17 at benefit start date (18 year olds are not eligible for YP).
**  - No child on benefit within 90 days of benefit start (these will be classified as YPP as above).
**  - No partners aged 18 or over (restriction had no impact for this group - assume due to benefit eligibility below).
**	- Benefit type recorded as service code in 602 (JSA) or 603 (IYB/YP/YPP) and additional service data not YPP.
**
** NOTE: Some people can move onto sickness benefit or equivalent before having a child and moving onto YPP. We therefore dont exclude people with an earlier 
**		benefit spell. If someone has less than 6 months on YP and then moves to YPP they are counted as a YPP participant with participation starting 
**  	at the time they start on YP;
**  ;

** Key data inputs:
		- msd.msd_swn - MSD clients
		- msd.msd_spell - spells on benefit
		- msd.msd_partner - partner spells on benefit
		- msd.msd_child - child on benefit records
;

** Key data outputs:
* 		- ypypp.yp_ypp_spell_child (all YP YPP participation information with child benefit spells merged on)
*		- ypypp.ypypp_parts (participants and historical comparison population);

** Set date for censoring;
%let censor=30Jun2015;

** Get the birthdates for all MSD clients so we can check age of eligibility - 15-17 for YP/IYB/JSA and 15-18 for Young parents (YPP) - most are aged 16 or above;
data msd_swn;
	set msd.msd_swn(keep=snz_uid msd_swn_birth_month_nbr msd_swn_birth_year_nbr);
	by snz_uid;
	if last.snz_uid;
run;

** Extract main benefit spells and add partner spells onto main ben spells;
** Include standard macro for assigning benefit types across time;
%include "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\code\BenefitNameTypeMacro.sas";

** Extract primary spells on benefits - Note YP and YPP are assigned benefits as individuals so all spells should be here. 
** As such we can't link them with their partners;
** But the rate code (srtet) allows us to at least identify whether they have a child... 
075	YP 16-17, no dependent children
076	YP 16-17, with partner and no dependent children
077	YPP 16-18, dependent children, 16-17 yrs not living at home
078	YPP 16-17, single, living at home or supported living away
079	YPP 16-18, 1 or more dependent children, with partner;

data msd_spel(drop=msd_spel_spell_start_date msd_spel_spell_end_date);
	set msd.msd_spell(keep=snz_uid msd_spel_spell_nbr msd_spel_servf_code msd_spel_add_servf_code msd_spel_spell_start_date msd_spel_spell_end_date 
							msd_spel_srtet_code);
	rename msd_spel_spell_nbr=spell msd_spel_srtet_code=rate_code;
	startdate=input(compress(msd_spel_spell_start_date,"-"),yymmdd10.);
	enddate=input(compress(msd_spel_spell_end_date,"-"),yymmdd10.);
	format startdate enddate date9.;
	%BNT_BenNmType( BNTserv = msd_spel_servf_code 
               ,BNTasd = msd_spel_add_servf_code
               ,BNTdate = startdate
               ,BNT_BenNm = BenefitName
               ,BNT_BenTyp = BenefitType
               ) ;
run;

** Extract partner spells on benefits;
data ptnr_spel(keep=main_snz_uid partner_snz_uid spell startdate enddate);
	set msd.msd_partner;
	format startdate enddate date9.;
	rename msd_ptnr_spell_nbr=spell snz_uid=main_snz_uid;
	startdate=input(compress(msd_ptnr_ptnr_from_date,"-"), yymmdd10.);
	enddate=input(compress(msd_ptnr_ptnr_to_date,"-"), yymmdd10.);

	* Deal with censoring;
	if startdate>"&censor"d then
		delete;

	if enddate=. then
		enddate="&censor"d;

	if enddate>"&censor"d then
		enddate="&censor"d;
run;

** Add benefit type to the partner's dataset - benefit type is not assigned to a partner's record so needs to be matched and carried across;

** Note that snz_uid+spell does not uniquely identify benefit spells therefore the start and enddate of each spell is also used below to correctly 
** match partner spells to those of the main beneficiary;

** This is done in two steps - 
**	(1) spells with fully matching start and end dates
**	(2) partner spells that fall within the matching main benefit spell but are not as long;

proc sort data=msd_spel out=main nodupkey;
	by snz_uid spell startdate enddate;
run;

proc sort data=ptnr_spel out=partner(rename=(main_snz_uid=snz_uid)) nodupkey;
	by main_snz_uid spell startdate enddate;
run;

data fullymatched unmatched;
	merge partner (in = a)
		main (in = b);
	by snz_uid spell startdate enddate;

	if a and b then
		output fullymatched;
	else if a and not b then
		output unmatched;
run;

proc sql;
	create table partlymatched as
		select a.partner_snz_uid, a.snz_uid, a.spell, a.enddate, a.startdate, b.msd_spel_servf_code, b.msd_spel_add_servf_code, b.benefittype, b.benefitname, b.rate_code
		from unmatched a inner join main b
			on a.snz_uid=b.snz_uid and a.spell=b.spell and a.startdate>=b.startdate and (a.enddate<=b.enddate or b.enddate=.)
	order by snz_uid, spell, startdate, enddate;
quit;

data prim_mainben_part_data_2;
	merge fullymatched partlymatched;
	by snz_uid spell startdate enddate;
run;
proc sort data=prim_mainben_part_data_2 out=part_spel(rename=(partner_snz_uid=snz_uid snz_uid=main_snz_uid spell=main_spell));
	by partner_snz_uid startdate enddate;
run;

** And do the reverse - add the partner details onto the main spell - note some main spells have more than one partner attached - just take one though;
proc sort data=prim_mainben_part_data_2 out=main_spel(keep=snz_uid spell partner_snz_uid) nodupkey;
	by snz_uid spell;
	where partner_snz_uid ne .;
run; 

* CONSOLIDATING BENEFIT SPELLS AS PRIMARY AND PARTNER;
data allben_spel;
	set main (in=a)
		part_spel (in=b);
	if b then do;
		ptnr_spell=spell;
		spell=.;
	end;
run;

proc sort data=allben_spel;
	by snz_uid spell;
run;

data allben_spel2;
	merge allben_spel(in=a) main_spel;
	by snz_uid spell;
	if a;
run;

proc sort data=allben_spel2;
	by snz_uid startdate;
run;

** Now work out the previous spell on benefit - what type of benefit was it, and how many days and months were there between the last benefit ending 
** and the current one starting;
data before_after(keep=snz_uid startdate prev_ben days_between mths_between);
	set allben_spel2;
	retain prev_ben prev_end;
	length prev_ben $ 20;
	by snz_uid;
	if first.snz_uid then do;
		prev_ben='NONE';
		days_between=.;
	end;
	else days_between=startdate-prev_end;
	mths_between=floor(days_between/30.5);
	output;
	prev_ben=benefitname;
	prev_end=enddate;
run;

data allben_spel2;
	merge allben_spel2 before_after;
	by snz_uid startdate;
run;

** Want to select anyone who commenced a benefit aged 17 or less (or 18 or less for those with children) - begin by selecting all spells since 2008
** where a person was aged under 19 - identify first spells on benefit also;
data yp_ypp_spell(drop=dobc);
	merge allben_spel2(in=a) msd_swn(in=b);
	by snz_uid;
	if a and b;
	* Formating dates and censoring;
	format startdate enddate dob date9.;
	if startdate<"&censor"d;
	if enddate>"&censor"d then enddate="&censor"d;
	if enddate=. then enddate="&censor"d;

	ys_period=enddate-startdate+1;

	if msd_swn_birth_month_nbr='' then delete;
	dobc="15"||put(msd_swn_birth_month_nbr,z2.)||left(compress(msd_swn_birth_year_nbr));
	dob=input(dobc,ddmmyy8.);

	** Construct an age variables in quarter years;
	age_at_start=floor((intck('month',dob,startdate)- (day(startdate) < day(dob))) / 12);
	age_at_start_ex=((intck('month',dob,startdate)- (day(startdate) < day(dob))) / 12);
	if (age_at_start_ex-age_at_start)<.25 then age=age_at_start;
	else if .25<=(age_at_start_ex-age_at_start)<.50 then age=age_at_start+.25;
    else if .50<=(age_at_start_ex-age_at_start)<.75 then age=age_at_start+.50;
    else if .75<=(age_at_start_ex-age_at_start)<1 then age=age_at_start+.75;

	** YPP can be 18, YP up to age 17;
	** Create variable indicating whether it is the persons first spell on benefit;
	if first.snz_uid then firstspell=1;
	else firstspell=0;
	if year(startdate) >= 2008 and age_at_start < 19 then output; 

	rename msd_spel_servf_code=servf msd_spel_add_servf_code=additional_service_data;
run;

** Now extract all child records;
data msd_child(keep=snz_uid child_from child_birthdt child_age);
	set msd.msd_child;
	child_from=input(msd_chld_child_from_date,yymmdd10.);
	child_birthdt=input('01'||put(msd_chld_child_birth_month_nbr,z2.)||put(msd_chld_child_birth_year_nbr,z4.),ddmmyy8.);
	child_age=floor((child_from-child_birthdt)/365.25);
	if child_age>18 then child_age=18;
	format child_from child_birthdt date9.;
run;

proc sort data=msd_child;
	by snz_uid child_from descending child_age;
run;

** Count up number of children associated with the benefit at the time the child spell starts - and output the age of the youngest child at that time;
data msd_child2(keep=snz_uid child_from num_child age_young_chd child_birthdt);
	set msd_child(rename=(child_age=age_young_chd));
	by snz_uid child_from;
	if first.child_from then num_child=0;
	num_child+1;
	if last.child_from then output;
run;

** Select those with children then check overlap with YPP population and with YP population - match on benefit start data within +/- 90 days;
** Firstly primary beneficiaries;
proc sql;
	create table yp_ypp_spell_child1 as
	select a.snz_uid, a.startdate, b.child_from, b.num_child, b.age_young_chd, b.child_birthdt from
	yp_ypp_spell as a inner join msd_child2 as b
	on a.snz_uid=b.snz_uid and a.startdate-90 <= b.child_from <= a.startdate+90 and b.child_from < a.enddate
	order by a.snz_uid, a.startdate;
quit;

** And then secondary beneficiaries;
proc sql;
	create table yp_ypp_spell_child2 as
	select a.snz_uid, a.startdate, b.child_from, b.num_child, b.age_young_chd, b.child_birthdt from
	yp_ypp_spell as a inner join msd_child2 as b
	on a.main_snz_uid=b.snz_uid and a.main_snz_uid ne . and a.startdate-90 <= b.child_from <= a.startdate+90 and b.child_from < a.enddate
	order by a.snz_uid, a.startdate;
quit;

** Match back together primary and secondary beneficiaries;
data yp_ypp_spell_child;
	merge yp_ypp_spell yp_ypp_spell_child1 yp_ypp_spell_child2;
	by snz_uid startdate;
run;

*** Check when people transition to YPP/YP, their most recent benefit end date, and the length of time since they first came onto benefit;
proc sql;
	create table ypypp_all as
	select snz_uid, spell, msd_spel_servf_code, msd_spel_add_servf_code, startdate, enddate
	from allben_spel2
	where msd_spel_add_servf_code in ('YP','YPP')
	order by snz_uid, startdate;
quit;

** Just take the first spell;
data ypypp_all;
	set ypypp_all;
	by snz_uid;
	if first.snz_uid;
run;

** Around 15k - 13k after removing subsequent spells;

** Now just keep those who had a previous benefit spell;
proc sql;
	create table ypypp_prev as
	select y.snz_uid, y.spell, y.msd_spel_servf_code as serv, y.msd_spel_add_servf_code, y.startdate format=date9., y.enddate format=date9.,
		   m.msd_spel_servf_code as prev_serv, m.msd_spel_add_servf_code as prev_add_serv,
		   m.startdate format=date9. as prev_start, m.enddate format=date9. as prev_end, m.spell as prev_spell
	from ypypp_all y inner join allben_spel2 m
	on y.snz_uid=m.snz_uid and m.spell < y.spell
	order by snz_uid, prev_spell;
quit;

** And calculate the start date of the first spell and the end date of the most recent spell;
data ypypp_prev2(keep=snz_uid spell serv msd_spel_add_servf_code prev_serv prev_add_serv startdate enddate first_start first_spell last_end last_spell);
	set ypypp_prev;
	retain first_start first_spell last_end last_spell;
	by snz_uid;
	if first.snz_uid then do;
		first_start=prev_start;
		first_spell=prev_spell;
	end;
	if last.snz_uid then do;
		last_end=prev_end;
		last_spell=prev_spell;
		output;
	end;
	format first_start last_end date9.;
run;

data ypypp_prev3;
	set ypypp_prev2;
	days_since_first_ben=startdate-first_start;
	days_since_last_ben=startdate-last_end;
	mths_since_first_ben=round(days_since_first_ben/30.5);
run;

** Restrict to direct transfers (days_since_last_ben=0) and look at the start dates;
/*
** Code to check when existing youth beneficiaries were transferred to the new benefits - we want to exclude these from our analysis;
proc freq data=ypypp_prev3;
	tables startdate/nopercent nocol norow;
	where days_since_last_ben=0;
run;

** Around 2.6k on 13 Aug 2012 - use this date plus 20 Aug as a number (<50) were also transferred then;
proc freq data=ypypp_prev3;
	tables mths_since_first_ben;
	where startdate='13Aug2012'd and days_since_last_ben=0;
run;
*/

data transfers_ypypp(keep=snz_uid);
	set ypypp_prev3;
	where startdate in ('13Aug2012'd,'20aug2012'd) and days_since_last_ben=0;
run;	

** Youth Payment are identified through being tagged as YP post benefit reform, or on JSA/IYB prior to benefit reform;
** Young parent payment are identified through being tagged as YPP post reform, or through being aged under 19 and with a child on benefit pre reform;
** Merge on direct transfers to YPYPP from other benefits at the time of the implementation of the reform;
data ypypp.yp_ypp_spell_child;
	merge yp_ypp_spell_child(in=a) transfers_ypypp(in=b);
	by snz_uid;
	if a;

	if  servf in ('602',  /* Job Search Allowance - a discontinued youth benefit */
				 '603')   /* IYB then after 2012 Youth/Young Parent Payment */	 
		and additional_service_data ne 'YPP' then YP=1;/* in 2012 changes some young DPB-SP+EMA moved to YPP */
	else yp=0;
	if additional_service_data='YPP' or servf in ('313','613','365','665') then ypp=1;
	else ypp=0;

	** Want a YPP identifier that allows for children added to the benefit between the start date and 90 days later - note these equiv variables not used in the end;
	if startdate <= child_from <= startdate+90 then ypp_equiv=1;
	else ypp_equiv=0;
	if ypp_equiv=0 and servf not in ('320','370') and age_at_start le 17 then yp_equiv=1;
	else yp_equiv=0;

	if partner_snz_uid ne . or main_snz_uid ne . then partner=1;
	else partner=0;
	days_parttochild=startdate-child_from;
	mths_parttochild=floor(days_parttochild/30);
	if startdate ge '01jul2012'd then prepost="Post";
	else prepost="Pre ";
	if b and startdate in ('13Aug2012'd,'20Aug2012'd) then transfer=1;
	else transfer=0;
	output;
run;

** Get birth dates for youth beneficiaries and partners - we need to check if they are over the eligibility age in the historical period;
proc sql;
	create table ypp_spell_check as 
	select a.*, b.msd_swn_birth_month_nbr as main_birth_month, b.msd_swn_birth_year_nbr as main_birth_year
	from ypypp.yp_ypp_spell_child as a left join msd_swn as b
	on a.main_snz_uid = b.snz_uid
	order by snz_uid;
quit;

proc sql;
	create table ypp_spell_check as 
	select a.*, b.msd_swn_birth_month_nbr as ptnr_birth_month, b.msd_swn_birth_year_nbr as ptnr_birth_year
	from ypp_spell_check as a left join msd_swn as b
	on a.partner_snz_uid = b.snz_uid
	order by snz_uid,startdate;
quit;

** Construct date of birth variables and calculate partner age at startdate;
data ypp_spell_check(drop=ptnr_dobc ptnr_dob main_dobc main_dob);
	set ypp_spell_check;
	** First the partner date of birth for primary beneficiaries;
	ptnr_dobc="15"||put(ptnr_birth_month,z2.)||left(compress(ptnr_birth_year));
	ptnr_dob=input(ptnr_dobc,ddmmyy8.);
	** And then the partner date of birth for partner beneficiaries (ie. the main recipient);
	main_dobc="15"||put(main_birth_month,z2.)||left(compress(main_birth_year));
	main_dob=input(main_dobc,ddmmyy8.);
	** Partner age calculated as the maximum of the two ie the one that is non-missing;
	partner_age=max(floor((intck('month',ptnr_dob,startdate)- (day(startdate) < day(ptnr_dob))) / 12),
					floor((intck('month',main_dob,startdate)- (day(startdate) < day(main_dob))) / 12));
run;	

** Split into YP and YPP participant and comparison populations;
data ypypp_parts;
	length part_type $ 7;
	set ypp_spell_check;
	startyear=year(startdate);
	** First those who are clearly identified as being of the right age and on the right benefit type (i.e. YP, YPP, DPB, EMA, IYB);
	if ypp and 15 <= age_at_start <= 18 then part_type='YPP';  
	else if yp and 15 <= age_at_start <= 17 then part_type='YP ';
	** Then also identify those who are in the right age range with a partner in the right age range and with a child on benefit (in the case of YPP);
	else if 15 <= age_at_start <= 18 and partner_age <=18 and partner and startdate-90 <= child_from <= startdate+90 then part_type='YPP 2';
	else if 15 <= age_at_start <= 17 and partner_age <=17 and partner then part_type='YP 2';
	else delete;
	** We're not including transfers in this study so exclude them here;
	if transfer then delete;
	else if startdate ge '01Jul2012'd then participant=1;
	else participant=0;
run;

** YPYPP started 01 July 2012 - very few starts before then - but we restrict later on to August onwards in line with NEET study;
** The control population constructed by Sylvia relates to Aug12 onwards;

** Commented out code below (no longer used) classifies participants starting from Jan11 to Jun12 according to the number of months between coming onto benefit 
** and July 2012 (Assuming they are still on benefit and move to YP/YPP in July/August 2012);
** Also constructs up to 36 records for 2008 to 2010 benefit starts depending on how long they stay on benefit and the year they are first on benefit
** - also with an indicator of how many months since benefit start Creates 36 months for 2008 starts, 30 months for 2009 starts, and 18 months for 2010 starts;

** This code no longer necessary as we exclude people transferring to YP or YPP in August 2012, and so don't need to construct historical comparison for them;

data ypypp.ypypp_parts/*(drop=z)*/;
	set ypypp_parts;
	retain first_strt;
	by snz_uid;
	** Set participant=1 if post-YS youth beneficiary and =0 if pre-YS youth beneficiary (only those coming onto benefit from 2008 to 2010);
	if first.snz_uid then first_strt=startdate;
	mths_ben_to_ys=0;
	days_ben_to_ys=0;
	ben_start=startdate;
	** Participant=0 if in our historical comparison group (2008-2010 start), participant=1 if in our YS participant group (july 2012 on), 
	** and participant=. if in-between these periods. We keep these as they may get re-assigned later on if e.g. they start on YP in this period
	** but transfer to YPP in the YS participant period;
	/*if startyear=2008 then num_mths=36;
	else if startyear=2009 then num_mths=30;
	else if startyear=2010 then num_mths=18;
	/*if transfer then do;
		mths_ben_to_ys=intck('month',first_strt,startdate);
		days_ben_to_ys=startdate-first_strt;
		*startdate=enddate;
		output;
	end;
	else*/ if participant=0 and 2008 le startyear le 2010 and first.snz_uid then /*do z=0 to num_mths;
		if z=0 and first.snz_uid then*/ output;
		/*if z>0 and ben_start < intnx('month',first_strt,z) < enddate then do; 
			mths_ben_to_ys=z;
			days_ben_to_ys=intnx('month',first_strt,z)-first_strt;
			startdate=intnx('month',first_strt,z);
			output;
		end;
	end;*/
	else if participant=0 and first.snz_uid and startdate < '01Jul2012'd then do;
		participant=.;
		output;
	end;
	else if first.snz_uid then output;
	format ben_start date9.;
run;
		
proc freq data=ypypp.ypypp_parts;
	tables startyear*part_type /nopercent nocol norow;
run;

proc freq data=ypypp.ypypp_parts;
	tables part_type*participant*transfer/missing;
run;












