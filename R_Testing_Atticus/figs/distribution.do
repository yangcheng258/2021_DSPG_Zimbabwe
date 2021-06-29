// MPI M_0, M_1, M_2 distribution

//import data
cd "G:\My Drive\PhD\Internship\Zimbabwe\03_Git\2021_DSPG_Zimbabwe\R_Testing_Atticus\figs"

use "G:\My Drive\PhD\Internship\Zimbabwe\03_Git\2021_DSPG_Zimbabwe\R_Testing_Atticus\MappingData.dta", clear


global numrep = 5000




	forvalues m = 0/2 {
		forvalues k=1/9{
			   
				global var =  m`m'_k`k'
				display "$var ="$var 
				histogram m`m'_k`k' , saving(fig`m'`k', replace) bin(40) xtitle(M`m'_K`k') kdensity					
		}
graph combine fig`m'1.gph fig`m'2.gph fig`m'3.gph fig`m'4.gph fig`m'5.gph fig`m'6.gph fig`m'7.gph fig`m'8.gph fig`m'9.gph,cols(3) saving(com`m', replace) 		
	}



	