# CPL Hashed Linkage Toolkit

This is the repository of code described in the California Policy Lab's [Hashed Linkages for
Administrative Datasets]() guide. There are broadly two steps to a hashed linkage
(described in detail in the guide), hashing and
linkage. At present, this repository contains code for the hashing step; code for the linkage step
is in the process of being prepared for distribution. The files intended for use by an end user are
as follows:

##### Hashing
* `hashing_base_linkagetoolkit.sas` (SAS)
* `hashing_base_linkagetoolkit.ipynb` (python)

Either the SAS or python code can be used for the hashing process, according to the users's
preferred language. As indicated by the documentation in each, only the names of the PII fields
and the most recent acceptable year for DOB should need to be changed by the end user.

