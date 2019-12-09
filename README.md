# Code for calculation tax rates in Cook County

In Illinois, County Clerks are responsible for calculating the tax rates for taxing bodies. We have a system where 

1. the taxing bodies, like a city or school district, say how much money they want to raise: the levy
2. the county assessor values every property
3. the county clerk brings those two things together, along with lots of regulations, to calculate the tax rate necessary to produce the levy

This repository contains the COBOL source code for the programs that calculate the tax rate in Cook County, along with associated files. 

- The files, as originally provided by the Cook County Bureau of Technology, are in the [`raw`](/raw) directory.
- The Makefile transforms those files into a form of COBOL that can be run by [GnuCOBOL](https://en.wikipedia.org/wiki/GnuCOBOL), a free open-source COBOL compiler.
- The [Clerk's documentation for running the program](docs/Tax_Rate_Making_Programs_Index.pdf)

This material may be valuable to ensure that tax rates are being calculated correctly and to forecast how the consequences of the rapid changes in assessments brought by reform in the Cook County Assessor's office.

## FOIAs

These files have been produced in response to Freedom of Information Requests and subsequent lawsuits.

- [May, 2018 FOIA to the Bureau of Technology for source code](https://www.muckrock.com/foi/cook-county-365/property-mainframe-systems-53184/)
- [Case number 2019-CH-05947](https://courtlink.lexisnexis.com/cookcounty/FindDock.aspx?NCase=2019-CH-05947&SearchType=0&Database=3&case_no=&PLtype=1&sname=&CDate=)
- [November 2019 FOIA to the Cook County Clerk for manuals](https://www.muckrock.com/foi/cook-county-365/manuals-user-guides-and-other-dcoumentation-for-the-tax-extension-code-83492/#)

Many thanks to [Loevy and Loevy](https://loevy.com/) for helping me get these records.

## Other links and Resources

- [Illinois County Clerk Tax Manual](docs/County_Clerk_Tax_Manual__2017Sept6.pdf)
- [IDOR Tax Rate and Levy Manual link](http://www.revenue.state.il.us/Publications/LocalGovernment/PTAX-60.pdf)
- [IDOR Property Tax Extension Limitation Law Technical Manual link](http://www.revenue.state.il.us/publications/LocalGovernment/PTAX1080.pdf)
