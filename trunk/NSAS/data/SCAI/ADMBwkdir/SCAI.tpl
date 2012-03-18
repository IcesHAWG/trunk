GLOBALS_SECTION 
 /*
 ###################################################################################################
 # Spawning Component Abundance Index (SCAI)
 #
 # Version 2.01 
 # $Rev$
 # $Date$
 #
 # Author: Mark Payne, mpa@aqua.dtu.dk
 # DTU-Aqua, Charlottenlund, DK 
 # 
 # Fits an ADMB random effects model to the LAI data set to estimate the spawning component
 # abundance index (SCAI), following the model described in Payne(2010).
 #
 # Developed with ADMB v 9.0
 #
 # Changes:
 #  v 2.01 - Added checksum feature to data reading
 #  v 2.00 - Tidied and annotated code more to fit with publication in HAWG repostiory.
 #  v 1.40 - Fixed problem with props not summing to 1
 #  v 1.30 - Estimates SCAI_hat, with std.dev internally within the model
 #  v 1.20 - Renamed from LAIre to SCAI. Variables renamed internally to match
 #           with text of manuscript. Added estimation of mean in random walk as well.
 #  v 1.10 - Treats the spampling units as estimating proportions of the total
 #           spawning abundance, allowing comparison between years
 #  v 1.00 - Initial version
 #
 # To be done:
 #
 # Notes:
 #   This work is described in more detail in the publication
 #   Payne, M.R. (2010) "Mind the gaps: a model robust to missing observations gives insight into the
 #   dynamics of the North Sea herring spawning components" ICES Journal of Marine Science. In press.
 #
 # ----------------------------------------------------------------------------
 # "THE BEER-WARE LICENSE" (Revision 42):
 # Mark Payne wrote this file. As long as you retain this notice, you can do
 # whatever you want with this stuff - just don't hold me responsible for it.
 # If we meet some day, and you think still this stuff is worth it, you
 # can buy me a beer in return. Mark Payne (with kudos to Poul-Henning Kamp)
 # ----------------------------------------------------------------------------
 ####################################################################################################*/

  //TRACE macro for use in debuging
  #define TRACE(object) cout<<"line "<<__LINE__<<", file "<<__FILE__<<", "<<#object" =\n"<<object<<endl<<endl; 
  #define DEBUG(stage) cout << stage;
  #define M_PI =3.1415926536

DATA_SECTION
  init_int n_years
  init_int start_yr
  init_int end_yr
  init_int n_surv
  init_int n_obs
  init_matrix obs(1,n_obs,1,3)

  init_ivector checksum_dat(1,2)  //Check that data has read correctly - should be 123456
  !!if(checksum_dat(1)!=checksum_dat(2)) { 
  !! cout << "CHECKSUM FAILURE: \t"<<checksum_dat << endl;
  !! exit(1);}


PARAMETER_SECTION
  // Parameters to fit
  init_number mulogSCAI                           //mean of the SCAI random walk
  init_number logSdSCAI                           //variance of the SCAI random walk
  init_number logSdObsErr                         //variance of the observation process ie of the LAIs about the true value
  init_vector prop_alpha(2,n_surv)                //transformed parameter used to estimate proportion 
                                                  //of total component biomass observed by each survey
                                                  //  (transformed using the eqn prop=exp(a)/(1+exp(a)))

  //Parameters to return
  sdreport_vector props(1,n_surv)                 //proportions of the component spawning biomass covered by each sampling unit
  sdreport_vector logSCAI_hat(1,n_obs)            //SCAIs estimated from observations
  sdreport_vector logobs_hat(1,n_obs)            //Observations estimated from modelled SCAIs

  //Other
  objective_function_value jnll;
  random_effects_vector logSCAI(start_yr,end_yr)   //Steps in the LAI random walk

PROCEDURE_SECTION
  //Initialiation and setup
  int yr;
  int surv;
  dvariable var;
  dvariable prop_alphas_norm;   //normalisation factor for the prop_alphas
  dvariable logSCAI_obs; 
  jnll=0.0;

  //Convert prop_alphas with range (-Inf, Inf) to props with range (0,1),
  //and with a constraint that they must sum to 1.00
  prop_alphas_norm=0;
  for(int i=2; i<=n_surv; i++) {
          prop_alphas_norm += exp(prop_alpha(i));
  }
  for(int i=2; i<=n_surv; i++) {
          props(i) = exp(prop_alpha(i))/(1+prop_alphas_norm);
  }
  props(1) = 1-prop_alphas_norm/(1+prop_alphas_norm);

  //JNLL for logSCAI random walk
  var=exp(2.0*logSdSCAI);
  for(int y=start_yr+1; y<=end_yr; ++y){
          jnll+=0.5*(log(2.0*M_PI*var) +square(logSCAI(y)-logSCAI(y-1)-mulogSCAI)/var);
  }

  //JNLL for observations
  var=exp(2.0*logSdObsErr);
  for(int i=1; i<=n_obs; ++i){
          surv=obs(i,2);
          yr=obs(i,3);
          logSCAI_hat(i)=log(obs(i,1)/props(surv));
          logobs_hat(i)=logSCAI(yr)+log(props(surv));
          jnll+=0.5*(log(2.0*M_PI*var) +square(log(obs(i,1))-logobs_hat(i))/var);

  }


TOP_OF_MAIN_SECTION
  gradient_structure::set_MAX_NVAR_OFFSET(3000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(3000);
  