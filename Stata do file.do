///--Dataset exploration
//Inspect the dataset
browse
describe
sort size
sort period

//Check integer Fields
codebook installs //no missing values
codebook apps //no missing values, min 1 & max 50 apps
codebook hhi_category //no missing values
codebook log_price //no missing values
codebook free_percent //no missing values
codebook dev_id //no missing values

sum users installs apps hhi_category log_price free_percent //no negative values

//Check Categorical Fields
tab size //Small, Medium & Large
codebook size //no missing values

tab period //min 1 & max 8 periods
codebook period //no missing values

//Introducing new variables
gen log_installs=log(installs)
gen log_users=log(users)

///--Descriptive Analytics
//Summary Statistics of all variables
summarize log_users log_installs  log_price installs apps hhi_category free_percent

//two-way summary statistics of all variables by size
tabstat log_users log_installs  log_price installs apps hhi_category free_percent, by(size) s(N median var mean) longstub column(s)

ssc install asdoc //To export the tables in word doc

asdoc summarize log_users log_installs  log_price installs apps hhi_category free_percent
asdoc tabstat log_users log_installs  log_price installs apps hhi_category free_percent, statistics(N median var mean) longstub by(size)


///Converting categorical variables to numerical
encode size, generate(size_dum)

//Significance level across years
oneway log_users size

//T3 significance test -spear-test Non Parametric significant test
spearman log_users size_dum , stats(rho p) star(0.05) pw

twoway (scatter log_users size_dum), xtitle(Size of a Firm) title(Significance bw Users & Size of a Firm)

 //Corelation Matrix of variables
eststo clear
estpost corr log_users log_installs log_price hhi_category apps period , matrix listwise
eststo T2
esttab T2 using corr_table_word.rtf, replace label unstack not compress

///--Exploratory Analysis
//1. Correlation graph matrix of the variables
graph matrix log_users log_installs apps hhi_category log_price period, diagonal(, size(medlarge) color(black)) mcolor(teal) title(Correlation Matrix - Baseline Model)

//log_users vs log_installs
twoway (scatter log_users log_installs, mcolor(maroon) msize(vsmall) msymbol(triangle)) (lfit log_users log_installs, lcolor(black) lwidth(medium)), ytitle(Log_users) xtitle(Log_installs)

//2. Summary Statistic graphs
graph hbar (mean) log_users (median) log_users (sd) log_users, over(size) ytitle(log_users) name(a)
graph hbar (mean) log_installs (median) log_installs (sd) log_installs, over(size) ytitle(Log_Installs) name(b)
graph hbar (mean) apps (median) apps (sd) apps, over(size) ytitle(apps) name(c)
graph hbar (mean) hhi_category (median) hhi_category (sd) hhi_category, over(size) ytitle(hhi_category) name(d)

graph combine a b c d  

//3. Distribution of main variables
histogram log_users, normal name(f)
histogram log_installs, normal name(g)
histogram apps, normal name(h)
histogram hhi_category, normal name(i)
histogram log_price, normal name(j)

graph combine f g h i j 

//4. Checking the Outliers
graph hbox hhi_category log_price free_percent
graph hbox log_users log_installs period

//5. Relation b/w dependent & independent variables
twoway (scatter log_users log_installs, msize(vsmall)) (lfit log_users log_installs), ytitle(Log_users) xtitle(Log_installs) name(r)
twoway (scatter log_users apps , msize(vsmall)) (lfit log_users apps ), ytitle(Log_users) xtitle( apps ) name(o)
twoway (scatter log_users hhi_category , msize(vsmall)) (lfit log_users hhi_category ), ytitle(Log_users) xtitle( hhi_category ) name(p)
twoway (scatter log_users log_price , msize(vsmall)) (lfit log_users log_price ), ytitle(Log_users) xtitle( log_price ) name(q)

graph combine r o p q

//6. Longitudinal trend of the main variables
twoway (lfit log_installs period), ytitle( log_installs ) name(s)
twoway (lfit log_users period), ytitle( log_users ) name(t)
twoway (lfit apps period), ytitle( apps ) name(u)
twoway (lfit log_price period), ytitle( log_price ) name(v)

graph combine s t u v


///--Main regression Analysis
//OLS regression baseline model 
eststo clear
reg log_users log_installs apps hhi_category log_price period 
eststo BL1

//Generating free_percent variable to categorical which distinguishes two types
recode free_percent 0.1/0.99 = 0 1/max = 1, generate(free_percent_categ)
//renaming 0 and 1 
recode free_percent_categ (0 = 0 "Only Free") (1 = 1 "Free & Paid"), generate(free_percent_cat)

//model2 Diff effect for "Only Free" developers apps
reg log_users log_installs apps hhi_category log_price period if free_percent_cat== 0 
eststo OF2

//model3 Diff effect for "Free & Paid" developers apps
reg log_users log_installs apps hhi_category log_price period if free_percent_cat== 1
eststo FP3

esttab BL1 OF2 FP3 using Main4_reg.rtf, replace ar2(3) b(3) se(3) r2(3) label compress title (Main Regression) mtitles("Baseline model" "Model2" "Model3") 

//marginplot 
reg log_users log_installs c.apps##free_percent_cat hhi_category log_price period i.free_percent_cat
margins free_percent_cat 
marginsplot
marginsplot,recastci(rarea)

///--Diagnostics and Robustness Analysis

hettest
imtest, white

//residuals vs fitted value
rvfplot, yline(0)

//baseline ols regression model 
eststo clear
reg log_users log_installs apps hhi_category log_price period i.free_percent_cat
eststo BL1

//robust regression model
reg log_users log_installs apps hhi_category log_price period i.free_percent_cat, vce(robust) 
eststo md2

//quadtraic effect on Total no. of apps on log_users
generate apps_sq=apps*apps

reg log_users log_installs apps_sq apps hhi_category log_price period i.free_percent_cat
eststo md3

twoway (scatter log_users apps) (lowess log_users apps)
twoway (qfit log_users apps)

//app developers fixed effect
egen dev_ID=group(dev_id)
xtset dev_ID period

xtreg log_users log_installs apps hhi_category log_price i.free_percent_cat, fe
eststo md4

esttab BL1 md2 md3 md4 using Main_reg.rtf, replace ar2(3) b(3) se(3) r2(3) label compress title(Model comparision)mtitles("Baseline model" "quadratic" "robust" "fixed")




