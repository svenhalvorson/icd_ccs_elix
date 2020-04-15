/******************************************************************/
/* Title:       CCSR FOR ICD-10-CM DIAGNOSIS MAPPING PROGRAM      */
/*                                                                */
/* Program:     DXCCSR_Mapping_Program.SAS                        */
/*                                                                */
/* Description: This is the SAS mapping program to apply 		  */
/*			    the CCSR to the user’s ICD-10-CM-coded data. 	  */
/*				The mapping program includes two options 		  */
/*				for the file structure of CCSR output.            */
/*                                                                */
/*              There are two general sections to this program:   */
/*                                                                */
/*              1) The first section creates temporary SAS        */
/*                 informats using the ICD-10-CM tool file.       */
/*                 These informats are used in step 2 to create   */
/*                 the CCSR variables in vertical and horizontal  */
/*                 output files.                                  */
/*              2) The second section loops through the diagnosis */
/*                 array in your SAS dataset and assigns          */
/*                 CCSR categories in the output files.           */
/*                                                                */
/*              Starting the v2020-2 of the CCSR, the SAS program */
/*              and accompanying CSV file include the assignment  */
/*              of the default CCSR for the principal diagnosis.  */
/*              The v2020-2 SAS program cannot be used with the   */
/*              prior versions of the CSV file  The v2020-2 CSV   */
/*              file is compatible with ICD-10-CM diagnosis codes */
/*              from October 2015 through September 2020.         */
/*                                                                */
/* Output:      The program outputs a vertical file or user may   */
/*			    select to output an additional horizontal file    */
/*              + Vertical file's layout: RECID DXCCSR DX_POSITION*/
/*              + Horizontal file's layout:RECID DXCCSR_BBBNNN    */
/*              where BBB=3-letter body system abbreviation, 	  */
/*                     NNN=3-digit number                         */
/******************************************************************/
* Path & name for the ICD-10-CM DXCCSR tool ;
       
FILENAME INRAW1  'Location of CSV file, $DXCCSR_v2020-2.CSV'   lRECL=3000;
LIBNAME  IN1     'Location of input discharge data';
LIBNAME  OUT1    'Directory to store output file';    

TITLE1 'CREATE ICD-10-CM DXCCSR TOOL CATEGORIES';
TITLE2 'USE WITH DISCHARGE ADMINISTRATIVE DATA THAT HAS ICD-10-CM CODES';
                                            
/******************************************************************/
/*  Macro Variables that must be set to define the characteristics*/
/*  of your SAS discharge data. Change these values to match the  */
/*  number of diagnoses in your dataset. Change CORE to match the */
/*  name of your dataset. Set DXPREFIX to be the fixed part of    */
/*  the diagnosis variables. For example, if the diagnosis        */
/*  variables in your SAS dataset are I10_DX1, I10_DX2, ... then  */
/*  set DXPREFIX to I10_DX.                                       */
/******************************************************************/

* Build default DXCCSR file? 1=yes, 0=no;     %LET DFLT=1;
* Build horizontal file? 1=yes, 0=no;         %LET HORZ=1;
* Linking key on input SAS data;              %LET RECID=KEY;
* Prefix of diagnosis variables;              %LET DXPREFIX=I10_DX;
* Maximum number of DXs on any record;        %LET NUMDX=30;
* Record specific DX Count variable, if not available leave it blank; 
                                              %LET NDXVAR=I10_NDX; 
* Input SAS file member name;                 %LET CORE=INPUT_SAS_FILE;
* Output SAS file name, vertical;             %LET VERTFILE=OUTPUT_VERT_FILE_NAME;
* Output SAS file name, horizontal;           %LET HORZFILE=OUTPUT_HORZ_FILE_NAME;
* Output SAS file name, default DXCCSR;       %LET DFLTFILE=OUTPUT_DFLT_FILE_NAME;

/******************* SECTION 1: CREATE INFORMATS ****************************/
/*  SAS Load the ICD-10-CM CCSR tool & convert into temporary SAS informats */
/*  that will be used to assign the DXCCSR variables in the next step.       */
/****************************************************************************/
%LET CCSRN_ = 5;
DATA DXCCSR;
    LENGTH LABEL $1140;
    INFILE INRAW1 DSD DLM=',' END = EOF FIRSTOBS=2;
    INPUT
       START             : $CHAR7.
       I10Label          : $CHAR124.
       I10CCSRDeft       : $10.
       I10CCSRLabelDeft  : $CHAR228.
       I10CCSR1          : $10.
       I10CCSRLabel1     : $CHAR228.
       I10CCSR2          : $10.
       I10CCSRLabel2     : $CHAR228.
       I10CCSR3          : $10.
       I10CCSRLabel3     : $CHAR228.
       I10CCSR4          : $10.
       I10CCSRLabel4     : $CHAR228.
       I10CCSR5          : $10.
       I10CCSRLabel5     : $CHAR228.
    ;

   RETAIN HLO " " FMTNAME "$DXCCSR" TYPE  "J" ;
   
   LABEL = CATX("#", OF I10CCSR1-I10CCSR5, OF I10CCSRLabel1-I10CCSRLabel5) ;
   OUTPUT;

   IF EOF THEN DO ;
      START = " " ;
      LABEL = " " ;
      HLO   = "O";
      OUTPUT ;
   END ;
RUN;

PROC FORMAT LIB=WORK CNTLIN = DXCCSR;
RUN;

DATA DXCCSRL(KEEP=START LABEL FMTNAME TYPE HLO);
  SET DXCCSR(KEEP=I10CCSR:) END=EOF;

  RETAIN HLO " " FMTNAME "$DXCCSRL" TYPE  "J" ;

  ARRAY CCSRC(&CCSRN_) I10CCSR1-I10CCSR&CCSRN_;
  ARRAY CCSRL(&CCSRN_) I10CCSRLabel1-I10CCSRLabel&CCSRN_;  

  LENGTH START $6 LABEL $228;
  DO I=1 to &CCSRN_;
    IF NOT MISSING(CCSRC(I)) then do;
      START=CCSRC(I);
      LABEL=CCSRL(I);
      output;
    end;
  end;

  IF EOF THEN DO ;
     START = " " ;
     LABEL = " " ;
     HLO   = "O";
     OUTPUT;
  END;
run;

PROC SORT DATA=DXCCSRL NODUPKEY; 
  BY START; 
RUN;

PROC FORMAT LIB=WORK CNTLIN = DXCCSRL;
RUN;

DATA DXCCSRDEFT(KEEP=START LABEL FMTNAME TYPE HLO);
  SET DXCCSR(KEEP=START I10CCSRDeft rename=(I10CCSRDeft=LABEL)) END=EOF;

  RETAIN HLO " " FMTNAME "$DXCCSRD" TYPE  "J" ;
  output;
  IF EOF THEN DO ;
     START = " " ;
     LABEL = " " ;
     HLO   = "O";
     OUTPUT;
  END;
run;

PROC SORT DATA=DXCCSRDEFT NODUPKEY; BY START; RUN;

PROC FORMAT LIB=WORK CNTLIN = DXCCSRDEFT;
RUN;

/*********** SECTION 2: CREATE ICD-10-CM CCSR OUTPUT FILES ***********************/
/*  Create CCSR categories for ICD-10-CM using the SAS informats created above   */
/*  using the diagnosis codes in your SAS dataset                                */
/*  Two separate files are created                                               */
/*********************************************************************************/  

%Macro dxccsr_vt;
   DATA OUT1.&VERTFILE (KEEP=&RECID DXCCSR DX_POSITION DEFAULT_DXCCSR) ;
     RETAIN &RECID;
     LENGTH ICD10_Code $7 DXCCSR $6 DX_POSITION 3 DEFAULT_DXCCSR $1;
     LABEL DEFAULT_DXCCSR = "Indication of the default CCSR for principal/first-listed diagnosis";

     SET IN1.&CORE;
 
     array A_DX(&NUMDX)       &DXPREFIX.1-&DXPREFIX.&NUMDX;

     %if &NDXVAR ne %then %let MAXNDX = &NDXVAR;
     %else %let MAXNDX=&NUMDX;
 
     DO I=1 TO &MAXNDX ;
       ICD10_CODE=A_DX(I);
       DX_POSITION=I;
       Default_DCCSR_Val =input(ICD10_CODE, $DXCCSRD.);

       CCSRString=INPUT(A_DX(I), $DXCCSR.); 

       if not missing(CCSRString) then do;
          ccsrn=(COUNTC(CCSRString,'#')+1)/2;
	      if ccsrn=1 then do;
             next_delim = findc(CCSRString,"#"); 
	         DXCCSR=substr(CCSRString,1, next_delim-1);

             if I=1 then do;
    		    if Default_DCCSR_Val = DXCCSR then DEFAULT_DXCCSR = 'Y';
	    		else if Default_DCCSR_Val = 'XXX000' then DEFAULT_DXCCSR = 'X';
		    	else DEFAULT_DXCCSR = 'N';
			 end;
			 else DEFAULT_DXCCSR = '';
			 
	         output OUT1.&VERTFILE;
	      end;
	      else do;
	       do j=1 to ccsrn;
             next_delim = findc(CCSRString,"#"); 
             DXCCSR=substr(CCSRString,1, next_delim-1);
             CCSRString=substr(CCSRString,next_delim+1);

             if I=1 then do;
			    if Default_DCCSR_Val = DXCCSR then DEFAULT_DXCCSR = 'Y';
			    else if Default_DCCSR_Val = 'XXX000' then DEFAULT_DXCCSR = 'X';
			    else DEFAULT_DXCCSR = 'N';
			 end;
			 else DEFAULT_DXCCSR = '';
			 
	         output OUT1.&VERTFILE;
	       end; 
	       do j=1 to ccsrn-1;
             next_delim = findc(CCSRString,"#"); 
	         CCSRString=substr(CCSRString,next_delim+1);
	       end; /*do j*/
	      end; /*else do*/
       end; /*not missing CCSString*/
     end; /*loop i*/
     drop i j ccsrn next_delim CCSRString; 
run;

Title1 "Vertical file";
proc contents data=out1.&VERTFILE varnum;
run;
Title2 "Sample print of vertical file";
proc print data=OUT1.&VERTFILE(obs=10);
run;
%mend;

* =========================================================================== * 
* count max number of DXCCSR values for each body system, please do not change *
* =========================================================================== *;
%macro count_ccsr;
  DATA Body_sys;
    length body bnum $3 ;
    INFILE INRAW1 DSD DLM=',' END = EOF FIRSTOBS=2;
    INPUT
       START             : $CHAR7.
       I10Label          : $CHAR124.
       I10CCSR1          : $10.
       I10CCSRLabel1     : $CHAR228.
       I10CCSR2          : $10.
       I10CCSRLabel2     : $CHAR228.
       I10CCSR3          : $10.
       I10CCSRLabel3     : $CHAR228.
       I10CCSR4          : $10.
       I10CCSRLabel4     : $CHAR228.
       I10CCSR5          : $10.
       I10CCSRLabel5     : $CHAR228.
    ;

    array ccsrs I10CCSR1-I10CCSR5;
    do over ccsrs;
      body=substr(ccsrs, 1, 3);
      bnum=substr(ccsrs, 4, 3);
      if body ^= '' then output;
    end;
    keep body bnum;
   run;
   proc sort data=Body_sys; by body bnum ; run;
   data body_max;
     set body_sys;
     by body bnum;
     if last.body;
   run;
   %global mnbody;
   %global body_;
   proc sql noprint;
     select distinct body into :body_ separated by ' '
     from body_max
     ; 
   quit;
   data null;
     set body_max end=eof;
     if eof then call symput("mnbody", put(_N_, 2.)); 
   run; 

   %do i=1 %to &mnbody;
     %let b=%scan(&body_, &i);
     %global max&b. ;
   %end;  

   data null;
     set body_max end=eof;
     mbody="max" || body; 
     call symput(mbody, bnum); 
     if eof then call symput("mnbody", put(_N_, 2.)); 
   run; 

   %put verify macro definition:;
   %put mnbody=&mnbody;
   %do i=1 %to &mnbody;
     %let b=%scan(&body_, &i);
     %put max&b._ = &&&max&b;
   %end;  
%mend;

%macro dxccsr_hz;
* =========================================================================== * 
* Create horizontal file layout using vertical file                           *
* =========================================================================== *;
proc sort data = out1.&VERTFILE;
  by &RECID;
run;
Data DXCCSR_First(keep=&RECID DXCCSR) DXCCSR_second(keep=&RECID DXCCSR);
  set out1.&VERTFILE;
  by &RECID;
  if DX_Position = 1 then output DXCCSR_First;
  else output DXCCSR_Second;
run;

proc sort data=DXCCSR_second nodupkey;
  by &RECID DXCCSR;
run;
proc sort data=DXCCSR_First;
  by &RECID DXCCSR;
run;

data DXCCSR;
  length DX_Position 3;
  merge DXCCSR_First(in=inp) DXCCSR_Second(in=ins);
  by &RECID DXCCSR;
  if inp and not ins then DX_Position = 1;
  else if ins and not inp then DX_Position = 3;
  else DX_Position = 2;
run;

proc transpose data=DXCCSR out=DXCCSR_Transposed(drop=_NAME_) prefix=DXCCSR_; 
  by &RECID;
  ID DXCCSR;
  Var DX_Position;
run; 

***for any DXCCSR_* value, if no DX is found on the record, set DXCCSR_* value to 0;
data out1.&HORZFILE;
  RETAIN &RECID;
  LENGTH
    %do i=1 %to &mnbody; 
      %let b=%scan(&body_, &i);
      DXCCSR_&b.001-DXCCSR_&b.&&max&b. 
    %end;
    3 ;

  set DXCCSR_Transposed;
  array a _all_;
  do over a;
    if a = . then a=0;
  end;
  drop DXCCSR_XXX: DXCCSR_MBD015 DXCCSR_MBD016;
run;

Title1 "Horizontal file";
proc contents data=out1.&HORZFILE varnum;
run;
Title2 "Sample print of horizontal file";
proc print data=out1.&HORZFILE(obs=10);
run;
%mend;

%macro dxccsr_dflt;
* ================================================================================ * 
* create default CCSR file with RECID & Default DXCCSR value using vertical file   *
* ================================================================================ *;
Data OUT1.&DFLTFILE(keep=&RECID DEFAULT_DXCCSR_DX1);
  length DEFAULT_DXCCSR_DX1 $6;
  label DEFAULT_DXCCSR_DX1 = "Default CCSR for principal/first-listed diagnosis";
  
  set OUT1.&VERTFILE(keep=&RECID DX_POSITION DXCCSR DEFAULT_DXCCSR);
  by &RECID;
  
  if DX_POSITION = 1 and DEFAULT_DXCCSR in ('Y','X');
  DEFAULT_DXCCSR_DX1 = DXCCSR;
run;

Title1 "Default DXCCSR file";
proc contents data=OUT1.&DFLTFILE;
run;
Title2 "Sample print of default DXCCSR file";
proc print data=OUT1.&DFLTFILE(obs=10);
  var &RECID DEFAULT_DXCCSR_DX1;
run;

%mend;

%macro main;
   %count_ccsr;
   %dxccsr_vt;
   %if &horz = 1 %then %do; 
     %dxccsr_hz;
   %end;
   %if &dflt = 1 %then %do; 
     %dxccsr_dflt;
   %end;
%mend;
%main;

