***************************************************************************************
Script: hashing_base.sas
Date: May 2022
Updated: June 2022
Author: California Policy Lab
Purpose: This is a general-purpose hashing script that will hash the following identifying 
		 variables (and various permutations of those variables) in the source data. This
		 script should be modified for each dataset and project it is run on. 
			1. SSN
			2. DOB
				- Day
				- Month
				- Year
			3. Name
				- First
				- Last
		 Some common modifications include: 
			1. Changing the salt to whatever is applicable for the project
			2. Changing the latest year to the year corresponding to the latest year of data 
			3. Changing the variable names to correspond to the raw data 
			4. Modifying as necessary to hash additional PII, or PII across multiple files 

**************************************************************************************
	1. DEFINE LIST OF VARIABLES TO BE HASHED
	- If additional PII variables need to be hashed, they should be added to the list below
	- Raw variable names should not have the same name as the var_list below;
	
	options mprint;
	
	
	%let var_list = fn fn2l fn4l fn_sdx ln ln2l ln4l ln_sdx fn1l ln1l dob_y dob_m dob_d ssn ssn89 ssn78 ssn67 ssn56 ssn45 ssn34 ssn23 ssn12;
	
**************************************************************************************
	2. CREATE MACRO
	- Create macro that will run cleaning and hashing for each file;
				
	%macro hash(salt=,
				first_name=,
				last_name=,
				dob=,
				ssn=,
				filepath_import=,
				filepath_export=, 
				filepath_log=,
				latest_year=
				);

	* Print log to a file instead of the log window;	
	proc printto log = &filepath_log new;
	run;

**************************************************************************************
	3. IMPORT RAW DATA
		- This csv should contain, at minimum, all the variables defined above

	* Read in the non PII fields and let SAS guess their types
	* Can add eg (keep=&first_name &last_name &dob &ssn) after the out=raw_data below to save on disk space
	* if other non-PII fields in the file are not needed in the output;
	filename temp &filepath_import;

	proc import datafile=temp
	      out=raw_data 
	      dbms=csv
	      replace;
		  guessingrows=MAX;
		  delimiter=",";
	run;


*************************************************************************************
	4. CLEAN RAW DATA
		- Strip spaces and symbols from names
		- Ensure that DOBs are valid dates
		- Ensure that SSNs are strings;
		

		data work.clean_data;
				length fn2l $2.; 
				length ln2l $2.; 
				length fn4l $4.;
				length ln4l $4.;
				length fn1l $1.;
				length ln1l $1.;
				length fn_sdx $4.;
				length ln_sdx $4.;
			set work.raw_data;

		* Clean names;

			* Convert to lowercase and strip leading and trailing whitespace;
			&first_name = strip(lowcase(&first_name));
		    &last_name = strip(lowcase(&last_name));

			* Remove common prefixes/suffixes from first & last names;
			&first_name = prxchange('s/^(mr\ |mrs\ |dr\ |ms\ |mr\.|mrs\.|dr\.|ms\.)+//', 1, &first_name);
			&last_name = prxchange('s/(\ jr|\ sr|\ jr\.|\ sr\.)+\s*$//', 1, &last_name);

		    * Replace special alphabetic characters with standard alphabetic characters;
			u00e0=unicode('\u00E0'); *à;
			u00e1=unicode('\u00E1'); *á;
			u00e2=unicode('\u00E2'); *â;
			u00e3=unicode('\u00E3'); *ã;
			u00e4=unicode('\u00E4'); *ä;
			u00e5=unicode('\u00E5'); *å;
			u00e7=unicode('\u00E7'); *ç;
			u00e8=unicode('\u00E8'); *è;
			u00e9=unicode('\u00E9'); *é;
			u00ea=unicode('\u00EA'); *ê;
			u00eb=unicode('\u00EB'); *ë;
			u00ec=unicode('\u00EC'); *ì;
			u00ed=unicode('\u00ED'); *í;
			u00ee=unicode('\u00EE'); *î;
			u00ef=unicode('\u00EF'); *ï;
			u00f2=unicode('\u00F2'); *ò;
			u00f3=unicode('\u00F3'); *ó;
			u00f4=unicode('\u00F4'); *ô;
			u00f5=unicode('\u00F5'); *õ;
			u00f6=unicode('\u00F6'); *ö;
			u00f9=unicode('\u00F9'); *ù; 
			u00fa=unicode('\u00FA'); *ú;
			u00fb=unicode('\u00FB'); *û;
			u00fc=unicode('\u00FC'); *ü;
			u00fd=unicode('\u00FD'); *ý;
			u00f1=unicode('\u00F1'); *ñ; 
			u00c0=unicode('\u00C0'); *À;
			u00c1=unicode('\u00C1'); *Á;
			u00c2=unicode('\u00C2'); *Â;
			u00c3=unicode('\u00C3'); *Ã;
			u00c4=unicode('\u00C4'); *Ä;
			u00c5=unicode('\u00C5'); *Å;
			u00c7=unicode('\u00C7'); *Ç;
			u00c8=unicode('\u00C8'); *È;
			u00c9=unicode('\u00C9'); *É;
			u00ca=unicode('\u00CA'); *Ê;
			u00cb=unicode('\u00CB'); *Ë;
			u00cc=unicode('\u00CC'); *Ì;
			u00cd=unicode('\u00CD'); *Í;
			u00ce=unicode('\u00CE'); *Î;
			u00cf=unicode('\u00CF'); *Ï;
			u00d1=unicode('\u00D1'); *Ñ;
			u00d2=unicode('\u00D2'); *Ò;
			u00d3=unicode('\u00D3'); *Ó;
			u00d4=unicode('\u00D4'); *Ô;
			u00d5=unicode('\u00D5'); *Õ;
			u00d6=unicode('\u00D6'); *Ö;
			u00d9=unicode('\u00D9'); *Ù;
			u00da=unicode('\u00DA'); *Ú;
			u00db=unicode('\u00DB'); *Û;
			u00dc=unicode('\u00DC'); *Ü;
			u00dd=unicode('\u00DD'); *Ý;

			%let spec_char = 	u00e0	u00e1	u00e2	u00e3	u00e4	u00e5	u00e7	u00e8	u00e9	u00ea	u00eb	u00ec	u00ed	u00ee	u00ef	u00f2	u00f3	u00f4	u00f5	u00f6	u00f9	u00fa	u00fb	u00fc	u00fd	u00f1	u00c0	u00c1	u00c2	u00c3	u00c4	u00c5	u00c7	u00c8	u00c9	u00ca	u00cb	u00cc	u00cd	u00ce	u00cf	u00d1	u00d2	u00d3	u00d4	u00d5	u00d6	u00d9	u00da	u00db	u00dc	u00dd	 ;
			%let final_char = 	'a'		'a'		'a'		'a'		'a'		'a'		'c'		'e'		'e'		'e'		'e'		'i'		'i'		'i'		'i'		'o'		'o'		'o'		'o'		'o'		'u'		'u'		'u'		'u'		'y'		'n'		'A'		'A'		'A'		'A'		'A'		'A'		'C'		'E'		'E'		'E'		'E'		'I'		'I'		'I'		'I'		'N'		'O'		'O'		'O'		'O'		'O'		'U'		'U'		'U'		'U'		'Y'		 ;
	   						* 	 à		 á		 â		 ã		 ä		 å		 ç		 è		 é		 ê		 ë		 ì		 í		 î		 ï		 ò		 ó		 ô		 õ		 ö		 ù		 ú		 û		 ü		 ý		 ñ		 À		 Á		 Â		 Ã		 Ä		 Å		 Ç		 È		 É		 Ê		 Ë		 Ì		 Í		 Î		 Ï		 Ñ		 Ò		 Ó		 Ô		 Õ		 Ö		 Ù		 Ú		 Û		 Ü		 Ý		 ;

			%macro translate_spec_char;
				%local i temp_old temp_new;
			
				%do i = 1 %to %sysfunc(countw(&spec_char));
			  		%let temp_old=%scan(&spec_char,&i);
					%let temp_new=%scan(&final_char,&i);
					&first_name=translate(&first_name,&temp_new,&temp_old); 
					&last_name=translate(&last_name,&temp_new,&temp_old); 
				%end;

			%mend;
			%translate_spec_char;
			

			* Remove all non a to z characters;
			&first_name = prxchange('s/[^a-z]//', -1, &first_name);
			&last_name = prxchange('s/[^a-z]//', -1, &last_name);


			* Flag missing names;
				badFN = (&first_name="");
				badLN = (&last_name="");


			* Create shortened and Soundex versions of names (only for good FN/LNs);
				if badFN = 0 then do;
					fn2l = substr(&first_name,1,2);
					fn4l = substr(&first_name,1,4);
					* Adding 1l to compare to old verison of code;
					fn1l = substr(&first_name,1,1);
					fn_sdx = soundex(&first_name);
					fn_sdx = substr(fn_sdx,1,4);
					* Pad soundex values with trailing 0s if less than 4 characters;
					if length(fn_sdx)=3 then fn_sdx = cats(fn_sdx, "0");
					if length(fn_sdx)=2 then fn_sdx = cats(fn_sdx, "00");
					if length(fn_sdx)=1 then fn_sdx = cats(fn_sdx, "000");
				end;

				if badLN = 0 then do;
					ln2l = substr(&last_name,1,2);
					ln4l = substr(&last_name,1,4);
					* Adding 1l to compare to old verison of code;
					ln1l = substr(&last_name,1,1);
					ln_sdx = soundex(&last_name);
					ln_sdx = substr(ln_sdx,1,4);
					if length(ln_sdx)=3 then ln_sdx = cats(ln_sdx, "0");
					if length(ln_sdx)=2 then ln_sdx = cats(ln_sdx, "00");
					if length(ln_sdx)=1 then ln_sdx = cats(ln_sdx, "000");
				end;

		* Clean DOB;
				dob_y = year(&dob);
				dob_m = month(&dob); 
				dob_d = day(&dob);

			* Flag invalid days, months, and years;
				badDOB = 0 ;
		     	if dob_y ~in(1900:&latest_year) then badDOB = 1;
				if dob_y < 1678 or dob_y > 2261 then badDOB = 1;
				if dob_d = . or dob_m = . or dob_y = . then badDOB = 1;
				if badDOB = 1 then dob_d = .;
				if badDOB = 1 then dob_m = .;
				if badDOB = 1 then dob_y = .;

	
		* Clean SSNs;
		      	*Make ssn a string variable;
		      ssn_str = put(&ssn, 9.);		      

		      	* "aps" indicates other characters to remove - "a" is alphabetic, "p" is punctuation, "s" is whitespace;
		      ssn_str = compress(ssn_str, " ", "aps"); 
		      ssn_length = length(ssn_str);

			* Pad SSNs with leading zeroes to 9 characters;
		      if ssn_length=8 then ssn_str = cats("0", ssn_str);
		      if ssn_length=7 then ssn_str = cats("00", ssn_str);
		      if ssn_length=6 then ssn_str = cats("000", ssn_str);
		      if ssn_length=5 then ssn_str = cats("0000", ssn_str);
		      if ssn_length=4 then ssn_str = cats("00000", ssn_str);
		      if ssn_length=3 then ssn_str = cats("000000", ssn_str);
		      if ssn_length=2 then ssn_str = cats("0000000", ssn_str);
		      if ssn_length=1 then ssn_str = cats("00000000", ssn_str);

			* Make three variables to flag invalid SSNs;
		      ssnA = substr(ssn_str,1,3);
		      ssnB = substr(ssn_str,4,2);
		      ssnC = substr(ssn_str,6,4);

			* Make numeric versions of the same;
		      ssnA_num = input(ssnA, best3.);
		      ssnB_num = input(ssnB, best2.);
		      ssnC_num = input(ssnC, best4.);

		    * Flag invalid ssns;
		      badSSN = 0 ;

			* SSNs should only have 9 digits;			  
		      ssn_length = length(ssn_str);
			  if ssn_length>9 then badSSN = 1; 

			* These are values that are not assigned by the SSA;
		      if ssnA = "000" or ssnB = "00" or ssnC = "0000" then badSSN = 1;
		      if ssnA = "666" then badSSN = 1 ;
		      if ssnA_num in(900:999) then badSSN = 1;

			* These are common fake values of SSNs;
		      if ssn_str = "078051120" then badSSN = 1;
		      if ssn_str = "123456789" then badSSN = 1;

			* Make other ssn variables that will be hashed at the end;
		  	  * ssn89 = ssn excluding digits 8 and 9;
			  * ssn78 = ssn excluding digits 7 and 8, etc.;
			  * this will only allow matches to SSNs that are either 1 digit off, or have 2 consecutive digits off;
			  * Only create these features for good SSNS, otherwise fill the SSN itself with an empty string;
			  if badSSN = 0 then do;
			      ssn89 = substr(ssn_str,1,7);
			      ssn78 = cats(substr(ssn_str,1,6),substr(ssn_str,9,1));
			      ssn67 = cats(substr(ssn_str,1,5),substr(ssn_str,8,2));
			      ssn56 = cats(substr(ssn_str,1,4),substr(ssn_str,7,3));
			      ssn45 = cats(substr(ssn_str,1,3),substr(ssn_str,6,4));
			      ssn34 = cats(substr(ssn_str,1,2),substr(ssn_str,5,5));
			      ssn23 = cats(substr(ssn_str,1,1),substr(ssn_str,4,6));
			      ssn12 = substr(ssn_str,3,7);
			  end;
			  else do;
			  	  ssn_str = "";
			  end;
			  		

		* Drop intermediate vars and raw DOB, SSN;
		drop &ssn ssn_length ssnA ssnB ssnC ssnA_num ssnB_num ssnC_num &dob 
				u00e0	u00e1	u00e2	u00e3	u00e4	u00e5	u00e7	u00e8	u00e9	u00ea	u00eb	u00ec	u00ed	u00ee	u00ef 	
				u00f2	u00f3	u00f4	u00f5	u00f6	u00f9	u00fa	u00fb	u00fc	u00fd	u00f1	u00c0	u00c1	u00c2	u00c3	
				u00c4	u00c5	u00c7	u00c8	u00c9	u00ca	u00cb	u00cc	u00cd	u00ce	u00cf	u00d1	u00d2	u00d3	u00d4	
				u00d5	u00d6	u00d9	u00da	u00db	u00dc	u00dd	 ;

		rename &first_name = fn &last_name = ln ssn_str = ssn;


	run;
	
*************************************************************************************
	5. HASH CLEANED DATA USING AGREED UPON SALT;

	data work.hashed_data;

		length 	fn_hash fn2l_hash fn4l_hash fn_sdx_hash ln_hash ln2l_hash ln4l_hash ln_sdx_hash
				fn1l_hash ln1l_hash dob_y_hash dob_m_hash dob_d_hash ssn_hash ssn89_hash ssn78_hash ssn67_hash ssn56_hash ssn45_hash ssn34_hash ssn23_hash ssn12_hash $64;

		set work.clean_data;

		%local x i; 
		  %do x=1 %to %sysfunc(countw(&var_list));
		  	%let i=%scan(&var_list,&x);
				
				if missing(&i.) then &i._hash = "";
				else &i._hash = sha256hex(cats(&salt,&i));
			
		%end;

		drop &var_list;

	run;

*************************************************************************************
	6. EXPORT HASHED DATA TO A CSV;

	proc export data=hashed_data
	      outfile=&filepath_export
	      dbms=csv
		  replace;
	run;

	%mend hash;

*************************************************************************************
	7. CALL MACRO;

	* INSTRUCTIONS:
	* The macro should be called once for every file that needs to be hashed.
	* Inputs:
	- Salt should be the string of characters that was agreed upon by the data contributors
	- filepath_import should be the filepath of the raw csv data
	- filepath_export should be the filepath where the hashed data should be exported to
	- filepath_log should be the filepath where the log will be saved
	- latest_year should correspond to the latest year of data
	- all other inputs should be the name of the corresponding variable
	  in the raw data. For example, if the ssn variable is named social_security,
      the input should be changed to ssn=social_security.
	;


	%hash(		salt="",
				first_name=,
				last_name=,
				dob=,
				ssn=,
				filepath_import="" encoding="utf-8",
				filepath_export="",
				filepath_log="",
				latest_year=2021
			);

*************************************************************************************
