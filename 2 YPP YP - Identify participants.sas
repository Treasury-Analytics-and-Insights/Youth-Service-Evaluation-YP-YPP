/* 2 YPP YP - Identify participants */

** This code tidies up the population and re-assigns YP or IYB participants as YPP or DPB EMA if they transfer within 6 months 
** participation start date is treated as the date of the transfer;

** Key data inputs:
*		- ypypp.ypypp_parts			(participants and historical comparison population)
*		- msd.msd_spells 			(benefit spells)
;

** Key data outputs:
* 		- ypypp.all_participants 	(first categorisation of participants)
*		- ypypp.all_participants1	(deal with transfers from YP to YPP and categorise as YPP - on YP while pregnant)
*		- ypypp.all_participants2	(matched demographic information and restrict to defined population - time in NZ, school enrolment etc.)
;

data ypypp.all_participants;
	set ypypp.ypypp_parts;
	** Create our two YS partcipant groups (YP and YPP) and our two pre-YS historical comparison groups (IYB and DPBEMA);
	if part_type='YP' and participant=1 then part_type='YP';
	else if participant in (0,.) and part_type in ('YP','YP 2') then part_type='IYB';
	else if part_type='YPP' and participant=1 and benefittype='Young Parent Payment' then part_type='YPP';
	else if participant in (0,.) and part_type in ('YPP','YPP 2') then part_type='DPBEMA';
	else delete;
	** Assign participants more recent than the study period to participant=2 - these will be excluded;
	if startyear>2013 then participant=2;
	** In the post-YS period we cant identify if someone has a partner from the spouse data as they each receive a benefit as a primary in their own right
	** - we have to use the payment rate to do this;
	if part_type in ('YP','YPP')and rate_code in ('076','079') then partner=1;
	** Set age 15 to 16 (few of these) - also set age as in exact years, and age_det in quarter years;
	age_det=age;
	if age_det<16 then age_det=16;
	age=floor(age_det);
run;

** Extract benefit spells;
%include "\\wprdsas10\TreasuryData\MAA2013-16 Citizen pathways through human services\keith\code\BenefitNameTypeMacro.sas";
data msd_spel(drop=msd_spel_spell_start_date msd_spel_spell_end_date);
	set msd.msd_spell(keep=snz_uid msd_spel_spell_nbr msd_spel_servf_code msd_spel_add_servf_code msd_spel_spell_start_date msd_spel_spell_end_date msd_spel_srtet_code);
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

** Look at transfers between benefits where the second benefit starts within 9 months of the first YPYPP benefit starting;
proc sql;
	create table transfers_to_spben as
	select a.snz_uid, a.startdate, a.benefittype, a.part_type, a.participant, b.startdate as trans_start, b.enddate as trans_end, b.BenefitName as trans_benname, b.benefittype as trans_bentype, b.rate_code as trans_rate
	from ypypp.all_participants a inner join msd_spel b
	on a.snz_uid=b.snz_uid and a.startdate<b.startdate<a.startdate+270 and a.benefittype ne b.benefittype
	order by a.snz_uid, b.startdate;
quit;

** Now restrict to transfers that are to a young parent benefit and from a youth benefit;
data transfers_to_spben;
	set transfers_to_spben;
	by snz_uid;
	if first.snz_uid;
	trans_refmth=12*(year(trans_start) -1999) + (month(trans_start) -3); 
	if (trans_bentype in ('Emergency Maintenance','Young Parent Payment') and part_type in ('YP','IYB')) and 2008 <= year(trans_start) <= 2014
		 then output;
run;

/*proc freq data=transfers_to_spben;
	tables part_type*participant*(trans_benname trans_bentype)/missing nopercent norow nocol;
run;*/

** Merge on transfer information for those being re-categorised from YP to YPP (or historical equivalents);
data ypypp.all_participants1;
	merge ypypp.all_participants transfers_to_spben(in=b);
	by snz_uid;
	if b then do;
		if trans_start >= '01jul2012'd then do;   
			if trans_start >= '01jan2014'd then participant=2; * These are those transferring to YPP after the study period;
			else participant=1;	** Now those transferring to YPP within the post-YS study period - re-classify as YPP;
			part_type='YPP';
			prev_ben='Youth Payment';
		end;
		else do;
			if trans_start >= '01jan2011'd then participant=.; ** Now those transferring to DPBEMA in the pre-YS study period - re-classify as DPBEMA;
			else participant=0;	
			part_type='DPBEMA';
			prev_ben='Independent Youth Be';
		end;
		benefitname=trans_benname;
		benefittype=trans_bentype;
		/*startdate=trans_start;
		enddate=trans_end;*/
		rate_code=trans_rate;
		if part_type in ('YP','YPP')and rate_code in ('076','079') then partner=1;
	end;
	startyear=year(startdate);
	if 2009 le startyear le 2014 then output;
run;

** Add on demographic variables and restrict to population with school enrolment, in NZ, with IRD indicator etc.;
** These requirements have already been applied to the ys5.demog datasets from the YSNEET analysis;
data ypypp.all_participants2;
	merge ypypp.all_participants1(in=a rename=(dob=dob_msd)) ys5.demog(in=b);
	by snz_uid;
	if a and b;
	startyear=year(startdate);
run;

proc sort data=ypypp.all_participants2 nodupkey;
	by snz_uid startdate;
run;
