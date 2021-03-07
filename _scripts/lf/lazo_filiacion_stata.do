

*** INDICADORES LAZO DE FILIACION ALC


keep if (age >= 75)

* Salvar bbdd 25-34 años
save "/Users/Marcelo/Documents/BBDD/IPUMS_International/ALC_edad(25-34)y(75ymas)/alc_75ymas.dta", replace

decode country, gen(pais)
egen pais_ano = concat(pais year), punct(_)

* Seleccionar variables básicas para calculo (lf1 y lf2)
keep country year perwt empstat age momloc poploc

* Seleccionar variables básicas para calculo (lf3 y lf4)
keep country year age perwt nchild persons 

* Personas que viven con padre o madre en el hogar
gen vivpar = .
replace vivpar = (momloc > 0 | poploc > 0) if momloc !=. & poploc !=.  

* Personas que viven con alguno de sus hijos
gen vivhij = .
replace vivhij = (nchild > 0) if nchild !=.  

* Personas que viven en hogares de más de 2 personas
gen npers2 = .
replace npers2 = (persons > 2) if persons !=.  


*** Resultados ***	

/*LF1: Personas de 25 a 34 años que viven con sus padres*/
table pais_ano [iw = perwt], c(mean vivpar) col row

/*LF2: Personas de 25 a 34 años desempleadas y que viven con sus padres*/
table pais_ano [iw = perwt] if empstat == 2, c(mean vivpar) col row

/*LF3: Personas de 75 años o más que viven con alguno de sus hijos*/
table pais_ano [iw = perwt], c(mean vivhij) col row

/*LF4: Personas de 75 años o más que viven con alguno de sus hijos*/
table pais_ano [iw = perwt], c(mean npers2) col row	
	
	

	
	
	
	
	
	
	
