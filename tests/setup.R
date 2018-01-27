if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)
my_email_address <- Sys.getenv( "my_email_address" )
this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

ess_cat <-
	get_catalog( "ess" ,
		output_dir = file.path( getwd() ) , 
		your_email = my_email_address )

ess_cat <- ess_cat[ split( seq( nrow( ess_cat ) ) , sort( seq( nrow( ess_cat ) ) %% 3 ) )[[ this_sample_break ]] , ]

lodown( "ess" , ess_cat , 
	your_email = my_email_address )
if( any( ess_cat$year == 2014 ) ){
library(lodown)
# examine all available ESS microdata files
ess_cat <-
	get_catalog( "ess" ,
		output_dir = file.path( getwd() ) , 
		your_email = my_email_address )

# 2014 only
ess_cat <- subset( ess_cat , year == 2014 )
# download the microdata to your local computer



library(survey)

ess_be_df <- 
	readRDS( file.path( getwd() , "2014/ESS7BE.rds" ) )

ess_sddf_df <- 
	readRDS( file.path( getwd() , "2014/ESS7SDDFe01_1.rds" ) )

ess_df <-
	merge( 
		ess_be_df , 
		ess_sddf_df , 
		by = c( 'cntry' , 'idno' ) 
	)

stopifnot( nrow( ess_df ) == nrow( ess_be_df ) )

ess_design <- 
	svydesign(
		ids = ~psu ,
		strata = ~stratify ,
		probs = ~prob ,
		data = ess_df
	)
ess_design <- 
	update( 
		ess_design , 
		
		one = 1 ,
		
		non_european_immigrants =
			factor( impcntr ,
				labels = c( 'Allow many to come and live here' , 
				'Allow some' , 'Allow a few' , 'Allow none' )
			) ,
		
		sex = factor( icgndra , labels = c( 'male' , 'female' ) ) ,
			
		more_than_one_hour_tv_daily = as.numeric( tvtot >= 3 )
	)
sum( weights( ess_design , "sampling" ) != 0 )

svyby( ~ one , ~ non_european_immigrants , ess_design , unwtd.count )
svytotal( ~ one , ess_design )

svyby( ~ one , ~ non_european_immigrants , ess_design , svytotal )
svymean( ~ ppltrst , ess_design )

svyby( ~ ppltrst , ~ non_european_immigrants , ess_design , svymean )
svymean( ~ sex , ess_design , na.rm = TRUE )

svyby( ~ sex , ~ non_european_immigrants , ess_design , svymean , na.rm = TRUE )
svytotal( ~ ppltrst , ess_design )

svyby( ~ ppltrst , ~ non_european_immigrants , ess_design , svytotal )
svytotal( ~ sex , ess_design , na.rm = TRUE )

svyby( ~ sex , ~ non_european_immigrants , ess_design , svytotal , na.rm = TRUE )
svyquantile( ~ ppltrst , ess_design , 0.5 )

svyby( 
	~ ppltrst , 
	~ non_european_immigrants , 
	ess_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ ppltrst , 
	denominator = ~ pplfair , 
	ess_design 
)
sub_ess_design <- subset( ess_design , vote == 1 )
svymean( ~ ppltrst , sub_ess_design )
this_result <- svymean( ~ ppltrst , ess_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ ppltrst , 
		~ non_european_immigrants , 
		ess_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( ess_design )
svyvar( ~ ppltrst , ess_design )
# SRS without replacement
svymean( ~ ppltrst , ess_design , deff = TRUE )

# SRS with replacement
svymean( ~ ppltrst , ess_design , deff = "replace" )
svyciprop( ~ more_than_one_hour_tv_daily , ess_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( ppltrst ~ more_than_one_hour_tv_daily , ess_design )
svychisq( 
	~ more_than_one_hour_tv_daily + sex , 
	ess_design 
)
glm_result <- 
	svyglm( 
		ppltrst ~ more_than_one_hour_tv_daily + sex , 
		ess_design 
	)

summary( glm_result )
library(srvyr)
ess_srvyr_design <- as_survey( ess_design )
ess_srvyr_design %>%
	summarize( mean = survey_mean( ppltrst ) )

ess_srvyr_design %>%
	group_by( non_european_immigrants ) %>%
	summarize( mean = survey_mean( ppltrst ) )

}
