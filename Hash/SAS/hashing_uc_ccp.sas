***************************************************************************************
Script: hashing_uc_ccp.sas
Date: April 2022
Author: California Policy Lab / Alex Bell (alexbell@ucla.edu)
Purpose: This is a hashing script that will hash the following identifying variables 
		(and various permutations of those variables) in the source data. 
			1. SSN
			2. DOB
				- Day
				- Month
				- Year
			3. Name
				- First
				- Middle
				- Last
				- Generation code
			4. Address
				- House number
				- Pre-direction code
				- Street name
				- Street suffix
				- Unit type
				- Unit ID
				- City
		 This script must be modified before running in the following ways:
		 	1. Changing the salt to whatever is applicable for the project
			2. Changing the variable names and filepaths in the macro call at the bottom
				to correspond to the raw data
Input:
	Please produce a CSV dataset that has on it:
		CTK
		Archive Date (numeric date)
		SSN (string)
		DOB (numeric date)
		First Name
		Middle Name
		Last Name
		Generation Code
		City	
		Address fields
	Important notes when this is run at Credit Bureau: 
		- If the dataset has additional fields, those will be propagated to the final dataset 
		in an unhashed way.
		- We request the following fields be on the input file & passed through unmasked: state, CTK, zip code
		- There may be modifications to the code required to handle different archive dates (e.g.
		perhaps the credit bureau runs this on a different file for each archive date, or alternatively
		perhaps all archive dates are stored in one big file).
		- When exporting the dataset to CSV, please make sure that the date variables (Archive
		Date and DOB) are exported as numeric dates, i.e. without quotation marks, and that SSN is string.
		If dates come in with quotation marks, the SAS code will set them to missing!
		- When inputing the source file name to the macro call, be sure not to remove the UTF-8 encoding option.

Changelog:
	- Branched from hashing_base_linkagetoolkit.sas on April 6, 2020 (see github version) by Alex Bell
		- Adding:
			Unhashed ctk and archive date
			Hashed middle name, middle initial, generation code, and city
		- Fixing:
			Remove unhashed PII from final file
	- April 22, 2022
		- After conversation with Myla/Cathy, adding full-address fields so that code is more 
			comprehensive for subsequent CPL projects.
		- Also adding a few other variants of names (e.g., full name field)

**************************************************************************************
	1. DEFINE LIST OF VARIABLES TO BE HASHED
	- If additional PII variables need to be hashed, they should be added to the list below
	- Raw variable names should not have the same name as the var_list below;
	
	options mprint;
	

	* updated late April 2022;
	%let var_list = fn fn2l fn4l fn_sdx mn mi ln ln2l ln4l ln_sdx fn1l ln1l gc fml1 fml2 mln
		house_nb predirectn_cd street_name street_name1l street_name4l street_name_sdx street_suffix 
		unit_type unit_id city city1l city4l city_sdx
		dob_y dob_m dob_d ssn ssn89 ssn78 ssn67 ssn56 ssn45 ssn34 ssn23 ssn12;
	
**************************************************************************************
	2. CREATE MACRO
	- Create macro that will run cleaning and hashing for each file;
	
	* AB modified April 2022;		
	%macro hash(salt=,
				archive_date=,
				first_name=,
				middle_name=,
				last_name=,
				gen_code=,
				house_nb=,
				predirectn_cd=,
				street_name=,
				street_suffix=,
				unit_type=,
				unit_id=,
				city=,
				dob=,
				ssn=,
				filepath_import=,
				filepath_export=, 
				filepath_log=,
				latest_year=
				);

	* Uncomment below to print log to a file instead of the log window;	
	/*
	proc printto log = &filepath_log new;
	run;
	*/
	
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
		  delimiter="|";
	run;

proc contents data = work.raw_data; run;
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
				length mi $1.;
			set work.raw_data;

		* Clean names;

			* Convert to lowercase and strip leading and trailing whitespace;
			&first_name = strip(lowcase(&first_name));
			&middle_name = strip(lowcase(&middle_name)); * AB added April 2022;
		    &last_name = strip(lowcase(&last_name));
		    &gen_code = strip(lowcase(&gen_code)); * AB added April 2022;
		    &city = strip(lowcase(&city)); * AB added April 2022;

			* Remove common prefixes/suffixes from first & last names;
			&first_name = prxchange('s/^(mr\ |mrs\ |dr\ |ms\ |mr\.|mrs\.|dr\.|ms\.)+//', 1, &first_name);
			&last_name = prxchange('s/(\ jr|\ sr|\ jr\.|\ sr\.)+\s*$//', 1, &last_name);


			* Replace special characters with standard alphabetic characters;
				*note: SAS is unable to read some of the special characters and replaces them in code with a question mark - appears to affect lower and upper case instances of C, E, and Y;
			%let spec_char	 =	"à"	"á"	"â"	"ã"	"ä"	"å"	"c"	"c"	"c"	"c"	"ç"		"è"	"é"	"ê"		"ë"	"ì"	"í"	"î"	"i"	"ï"	"ò"	"ó"	"ô"	"õ"	"ö"	"ù"	"ú"	"û"	"u"	"ü"		"ý"	"y"	"ñ"	"À"	"Á"	"Â"	"Ã"	"Ä"	"Å"	"C"	"C"	"C"	"C"	"Ç"		"È"	"É"	"Ê"		"Ë"	"Ì"	"Í"	"Î"	"I"	"Ï"	"Ñ"	"Ò"	"Ó"	"Ô"	"Õ"	"Ö"	"Ù"	"Ú"	"Û"	"U"	"Ü"		"Ý"	"Y";
			%let final_char  = 	"a"	"a"	"a"	"a"	"a"	"a"	"c"	"c"	"c"	"c"	"c"		"e"	"e"	"e"		"e"	"i"	"i"	"i"	"i"	"i"	"o"	"o"	"o"	"o"	"o"	"u"	"u"	"u"	"u"	"u"		"y"	"y"	"n"	"A"	"A"	"A"	"A"	"A"	"A"	"C"	"C"	"C"	"C"	"C"		"E"	"E"	"E"		"E"	"I"	"I"	"I"	"I"	"I"	"N"	"O"	"O"	"O"	"O"	"O"	"U"	"U"	"U"	"U"	"U"		"Y"	"Y";

			%macro translate_spec_char;
				%local i temp_old temp_new;
			
				%do i = 1 %to %sysfunc(countw(&spec_char));
			  		%let temp_old=%scan(&spec_char,&i);
					%let temp_new=%scan(&final_char,&i);
					&first_name=translate(&first_name,&temp_new,&temp_old);
					&middle_name=translate(&middle_name,&temp_new,&temp_old); * AB added April 2022;
					&last_name=translate(&last_name,&temp_new,&temp_old); 
					&gen_code=translate(&gen_code,&temp_new,&temp_old); * AB added April 2022;
					&city=translate(&city,&temp_new,&temp_old); * AB added April 2022;
				%end;

			%mend;
			%translate_spec_char;
			******************;

			* Remove all non a to z characters;
			&first_name = prxchange('s/[^a-z]//', -1, &first_name); 
			&middle_name = prxchange('s/[^a-z]//', -1, &middle_name); * AB added April 2022;
			&last_name = prxchange('s/[^a-z]//', -1, &last_name);
			&city = prxchange('s/[^a-z]//', -1, &city); * AB added April 2022;
			&gen_code = prxchange('s/[^a-z]//', -1, &gen_code); * AB added April 2022;
			
			* name derivatives (AB expanding late April);
			mi = substr(&middle_name,1,1); * AB added 3/31/22;
			fml1 = cats(&first_name,&middle_name,&last_name,&gen_code);
			fml2 = cats(&first_name,&middle_name,&last_name);
			mln = cats(&middle_name,&last_name);
			
			* Flag missing names;
				badFN = (&first_name="");
				badLN = (&last_name="");
				badMN = (&middle_name="");
				badGC = (&gen_code="");
				badCity = (&city="");


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
			* AB note: handling of dates a bit tenuous. Relies on all dates being in same format, 
				otherwise I think potentially all dates get set to missing;
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
			* "aps" indicates other characters to remove - "a" is alphabetic, "p" is punctuation, "s" is whitespace;
		      &ssn = compress(&ssn, " ", "aps"); 
		      ssn_length = length(&ssn);

			* Pad SSNs with leading zeroes to 9 characters;
		      if ssn_length=8 then &ssn = cats("0", &ssn);
		      if ssn_length=7 then &ssn = cats("00", &ssn);
		      if ssn_length=6 then &ssn = cats("000", &ssn);
		      if ssn_length=5 then &ssn = cats("0000", &ssn);
		      if ssn_length=4 then &ssn = cats("00000", &ssn);
		      if ssn_length=3 then &ssn = cats("000000", &ssn);
		      if ssn_length=2 then &ssn = cats("0000000", &ssn);
		      if ssn_length=1 then &ssn = cats("00000000", &ssn);

			* Make three variables to flag invalid SSNs;
		      ssnA = substr(&ssn,1,3);
		      ssnB = substr(&ssn,4,2);
		      ssnC = substr(&ssn,6,4);

			* Make numeric versions of the same;
		      ssnA_num = input(ssnA, best3.);
		      ssnB_num = input(ssnB, best2.);
		      ssnC_num = input(ssnC, best4.);

		    * Flag invalid ssns;
		      badSSN = 0 ;

			* SSNs should only have 9 digits;			  
		      ssn_length = length(&ssn);
			  if ssn_length>9 then badSSN = 1; 

			* These are values that are not assigned by the SSA;
		      if ssnA = "000" or ssnB = "00" or ssnC = "0000" then badSSN = 1;
		      if ssnA = "666" then badSSN = 1 ;
		      if ssnA_num in(900:999) then badSSN = 1;

			* These are common fake values of SSNs;
		      if &ssn = "078051120" then badSSN = 1;
		      if &ssn = "123456789" then badSSN = 1;

			* Make other ssn variables that will be hashed at the end;
		  	  * ssn89 = ssn excluding digits 8 and 9;
			  * ssn78 = ssn excluding digits 7 and 8, etc.;
			  * this will only allow matches to SSNs that are either 1 digit off, or have 2 consecutive digits off;
			  * Only create these features for good SSNS, otherwise fill the SSN itself with an empty string;
			  if badSSN = 0 then do;
			      ssn89 = substr(&ssn,1,7);
			      ssn78 = cats(substr(&ssn,1,6),substr(&ssn,9,1));
			      ssn67 = cats(substr(&ssn,1,5),substr(&ssn,8,2));
			      ssn56 = cats(substr(&ssn,1,4),substr(&ssn,7,3));
			      ssn45 = cats(substr(&ssn,1,3),substr(&ssn,6,4));
			      ssn34 = cats(substr(&ssn,1,2),substr(&ssn,5,5));
			      ssn23 = cats(substr(&ssn,1,1),substr(&ssn,4,6));
			      ssn12 = substr(&ssn,3,7);
			  end;
			  else do;
			  	  &ssn = "";
			  end;
			  		
		
		* Clean addresses (added late April 2022);

			* Convert to lowercase, remove all spaces, punctuation & numbers from 
			  pre-direction code, street suffix, unit type, city;
				&predirectn_cd = compress(lowcase(&predirectn_cd), " ", "dp");
				&street_suffix = compress(lowcase(&street_suffix), " ", "dp");
				&unit_type = compress(lowcase(&unit_type), " ", "dp");
				&city = compress(lowcase(&city), " ", "dp");

			* Note: no longer transforming fractions into full integers for street number (AB mod late April 2022);

			* Convert to lowercase, remove all spaces & punctuation but keep letters and numbers for
			  house number, street name, unit ID;
				&house_nb = compress(lowcase(&house_nb), " ", "p");
				&street_name = compress(lowcase(&street_name), " ", "p");
				&unit_id = compress(lowcase(&unit_id), " ", "p");

			* Replace fully spelled out pre-direction codes, street suffixes, and unit types
				with USPS postal abbrevations;

				*pre-direction codes;
				
				&predirectn_cd = tranwrd(&predirectn_cd, "north", "n");
				&predirectn_cd = tranwrd(&predirectn_cd, "south", "s");
				&predirectn_cd = tranwrd(&predirectn_cd, "east", "e");	
				&predirectn_cd = tranwrd(&predirectn_cd, "west", "w");
				*street suffixes;
				&street_suffix = tranwrd(&street_suffix, "avenue", "ave");
				&street_suffix = tranwrd(&street_suffix, "boulevard", "blvd");
				&street_suffix = tranwrd(&street_suffix, "circle", "cir");
				&street_suffix = tranwrd(&street_suffix, "common", "cmn");
				&street_suffix = tranwrd(&street_suffix, "commons", "cmns");
				&street_suffix = tranwrd(&street_suffix, "court", "ct");
				&street_suffix = tranwrd(&street_suffix, "drive", "dr");
				&street_suffix = tranwrd(&street_suffix, "highway", "hwy");
				&street_suffix = tranwrd(&street_suffix, "lane", "ln");
				&street_suffix = tranwrd(&street_suffix, "mount", "mt");
				&street_suffix = tranwrd(&street_suffix, "parkway", "pkwy");
				&street_suffix = tranwrd(&street_suffix, "place", "pl");
				&street_suffix = tranwrd(&street_suffix, "plaza", "plz");
				&street_suffix = tranwrd(&street_suffix, "road", "rd");
				&street_suffix = tranwrd(&street_suffix, "route", "rte");
				&street_suffix = tranwrd(&street_suffix, "street", "st");
				&street_suffix = tranwrd(&street_suffix, "terrace", "ter");
				*unit types;
				&unit_type = tranwrd(&unit_type, "apartment", "apt");
				&unit_type = tranwrd(&unit_type, "floor", "fl");
				&unit_type = tranwrd(&unit_type, "room", "rm");
				&unit_type = tranwrd(&unit_type, "suite", "ste");

			*Convert spelled out numeric street names to numeric (e.g., first to 1st);
				&street_name = tranwrd(&street_name, "first", "1st");
				&street_name = tranwrd(&street_name, "second", "2nd");
				&street_name = tranwrd(&street_name, "third", "3rd");
				&street_name = tranwrd(&street_name, "fourth", "4th");
				&street_name = tranwrd(&street_name, "fifth", "5th");
				&street_name = tranwrd(&street_name, "sixth", "6th");
				&street_name = tranwrd(&street_name, "seventh", "7th");
				&street_name = tranwrd(&street_name, "eighth", "8th");
				&street_name = tranwrd(&street_name, "ninth", "9th");
				&street_name = tranwrd(&street_name, "tenth", "10th");

			* Create shortened and Soundex versions of street name;
				street_name1l = substr(&street_name,1,1);
				street_name4l = substr(&street_name,1,4);
				street_name_sdx = soundex(&street_name);
				street_name_sdx = substr(street_name_sdx,1,4);

					* Pad soundex values with trailing 0s if less than 4 characters;
					if length(street_name_sdx)=3 then street_name_sdx = cats(street_name_sdx, "0");
					if length(street_name_sdx)=2 then street_name_sdx = cats(street_name_sdx, "00");
					if length(street_name_sdx)=1 then street_name_sdx = cats(street_name_sdx, "000");

		*City-derived variables (added late April 2022);
			

			 * Create shortened and Soundex versions of city;
				city1l = substr(&city,1,1);
				city4l = substr(&city,1,4);
				city_sdx = soundex(&city);

					* Pad soundex values with trailing 0s if less than 4 characters;
					if length(city_sdx)=3 then city_sdx = cats(city_sdx, "0");
					if length(city_sdx)=2 then city_sdx = cats(city_sdx, "00");
					if length(city_sdx)=1 then city_sdx = cats(city_sdx, "000");

		
		* Drop intermediate vars and raw DOB;
		drop ssn_length ssnA ssnB ssnC ssnA_num ssnB_num ssnC_num &dob ;
		* AB modified below April 2022;
		rename &first_name = fn &last_name = ln  &middle_name=mn &gen_code=gc &city=city &ssn = ssn
			&house_nb=house_nb &predirectn_cd=predirectn_cd &street_name=street_name 
			&street_suffix=street_suffix &unit_type=unit_type &unit_id=unit_id;
		

	run;
proc contents data = work.clean_data; run;	
*************************************************************************************
	5. HASH CLEANED DATA USING AGREED UPON SALT;

	data work.hashed_data;
		set work.clean_data;
		%local x i; 
		  %do x=1 %to %sysfunc(countw(&var_list));
		  	%let i=%scan(&var_list,&x);
		  		* AB adding this line rather than listing each above , in late April 2022;
				length 	&i._hash $64;
				if missing(&i.) then &i._hash = "";
				else &i._hash = sha256hex(cats(&salt,&i));
		%end;
		* Drop unhashed PII;
		drop &var_list. ;
	run;
proc contents data = work.hashed_data; run;	
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
				archive_date=archive_date,
				first_name=inventor_name_first,
				middle_name=inventor_name_middle,
				last_name=inventor_name_last,
				gen_code=gc,
				house_nb=house_nb,
				predirectn_cd=predirectn_cd,
				street_name=street_name, 
				street_suffix=street_suffix,
				unit_type=unit_type,
				unit_id=unit_id,
				city=inventor_city_name,
				dob=dob,
				ssn=ssn,
				filepath_import="/home/your/file_path/here.csv" encoding="utf-8",
				filepath_export="/home/ambell0/sasuser.v94/out.csv",
				filepath_log="/home/ambell0/sasuser.v94/log.txt",
				latest_year=2022
			);

*************************************************************************************
