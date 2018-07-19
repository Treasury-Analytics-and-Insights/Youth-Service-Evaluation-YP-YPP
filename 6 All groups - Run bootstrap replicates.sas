/* 6. All groups -  run bootstrap replicates */
/*	Runs bootstrap replicates */
/* Keith March 2016 */
/* Based on code by Sylvia March 2016 */

** Key data inputs:
*		- ypiyb.rep0_yp_1 / ypiyb.rep0_iyb_1 / yppdpb.rep0_ypp_1 / yppdpb.rep0_dpbema_1 / yppdpb.rep0_yppb_1 / yppdpb.rep0_dpbemab_1 (participant/historical comparison and matched comparison data)

** Key data outputs:
*		- byp.repXXX / bypp.repXXX / byppb.repXXX / biyb.repXXX / bdpbema.repXXX / bdpbemab.repXXX (data for each bootstrap replicate - XXX = 1 to &reps.)
*		- byp.allreps_1 / biyb.allreps_1 / bypp.allreps_1 / bdpbema.allreps_1 / byppb.allreps_1 / bdpbemab.allreps_1 (all participants/historical comparisons and matched comparisons - all bootstrap replicates)
;

**** CREATING REPLICATE WEIGHTS ;
%let reps=300;
options nonotes;

%macro replicate ; 
 %DO i =1 %TO &reps.;
	proc datasets lib=work kill;
	run;
  %put Iteration &i.;
  %SEweightb(YP,1,bYP,ypiyb,);
  %SEweightb(IYB,1,bIYB,ypiyb,);
  %SEweightb(YPP,1,bYPP,yppdpb,);
  %SEweightb(DPBEMA,1,bDPBEMA,yppdpb,);
  %SEweightb(YPP,1,bYPPb,yppdpb,b);
  %SEweightb(DPBEMA,1,bDPBEMAb,yppdpb,b);
  %END;
%mend replicate;
%replicate;
options notes;

**Append the base and replicate datasets to form a single  dataset of weights;
%macro append(lib,part_type,strata);
options nomprint;
data allreps ;
	set &lib..rep0_&part_type.&strata.(keep=snz_uid _snz_uid treat refmth _refmth weight);
run;

proc sort data=allreps;
by snz_uid _snz_uid treat refmth;
run;

%let numsteps=%eval(&reps./50);

%do z=1 %to &numsteps.;
data allreps_&strata.&z.(compress=Y);
  merge %DO i=%eval((&z.-1)*50+1) %TO %eval(&z.*50);
			b&part_type..rep&i.
		 %END ; ;
  by snz_uid _snz_uid treat refmth;
run;
%end;

data b&part_type..allreps_&strata;
	merge allreps 
		%do z=1 %to &numsteps.;
		allreps_&strata.&z.
		%end;
		;
  by snz_uid _snz_uid treat refmth;
run;
%mend append;

%append(ypiyb,YP,1);
%append(ypiyb,IYB,1);
%append(yppdpb,YPP,1);
%append(yppdpb,DPBEMA,1);
%append(yppdpb,YPPb,1);
%append(yppdpb,DPBEMAb,1);


/*
**Calculate again using the old approach;

****CREATING REPLICATE WEIGHTS ;
%let reps=400;
options nonotes;

%macro replicate ; 
 %DO i =1 %TO &reps.;
  %put Iteration &i.;
  %SEweight(YP,1,bYPold,&lib.);
  %SEweight(IYB,1,bIYBold,&lib.);
  %END;
%mend replicate;
%replicate;
options notes;

**Append the base and replicate datasets to form a single  dataset of weights;
%macro append(part_type,strata);
data allreps ;
	set &lib..rep0_&part_type.&strata.(keep=snz_uid _snz_uid treat refmth weight);
run;

proc sort data=allreps;
by snz_uid _snz_uid treat refmth;
run;

%macro merge ;
 %DO i =1 %TO &reps.;
 %put Iteration &i.;

  data allreps(compress=Y) ;
  merge allreps 
        b&part_type.old.rep&i;
  by snz_uid _snz_uid treat refmth ;
  run ;
 
%END ;

%mend merge ;
%merge;

data b&part_type.old.allreps_&strata;
set allreps;
run;

%mend append;

%append(YP,1);
%append(IYB,1);
*/
