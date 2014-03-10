
<!-- saved from url=(0056)https://raw.github.com/flr/FLSAM/master/inst/tpl/sam.tpl -->
<html><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"><style type="text/css"></style></head><body><pre style="word-wrap: break-word; white-space: pre-wrap;">// 
//  ----------------------------------------------------------------------------
//  "THE BEER-WARE LICENSE" (invented by Poul-Henning Kamp):
//  Anders Nielsen &lt;an@aqua.dtu.dk&gt; wrote this file. As long as you retain this 
//  notice you can do whatever you want with this stuff. If we meet some day, 
//  and you think this stuff is worth it, you can buy me a beer in return. 
//  ----------------------------------------------------------------------------
// 

//----------------------------------------------------------------
//SAM State Space Assessment Model
//--------------------------------
//$Rev: 17 $
//$LastChangedDate: 2011-11-10 17:52:12 +0100 (Thu, 10 Nov 2011) $
//----------------------------------------------------------------

GLOBALS_SECTION 
  #include &lt;time.h&gt;
  time_t StartTime;   //Time variables, for use in timing loop
  #include &lt;df1b2fun.h&gt;
  #include "nLogNormal.h"
  ofstream clogf("program.log");
  #define TRACE(object) clogf&lt;&lt;"line "&lt;&lt;__LINE__&lt;&lt;", file "&lt;&lt;__FILE__&lt;&lt;", "&lt;&lt;#object" =\n"&lt;&lt;object&lt;&lt;endl&lt;&lt;endl; 
  #define STRACE(object) cout&lt;&lt;"line "&lt;&lt;__LINE__&lt;&lt;", file "&lt;&lt;__FILE__&lt;&lt;", "&lt;&lt;#object" =\n"&lt;&lt;object&lt;&lt;endl&lt;&lt;endl; 
  bool usepin=false;    //Determines if model.init overrides pin file estimates or not

  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////
  #define assignInit(object){                           \
    adstring adStr;                                     \
    adstring name=#object;				\
    adstring colon=':';					\
    name=name+colon;                                    \
    int idx=0;                                          \
    adstring fname=__FILE__;                            \
    adstring lname="par";                               \
    adstring parname=fname(1,fname.size()-3)+lname;     \
    ifstream in(parname);                               \
    while (!in.eof()){                                  \
      in&gt;&gt;adStr;                                        \
      if(adStr==name){                                  \
        in&gt;&gt;object;					\
      }                                                 \
    }                                                   \
    in.close();                                         \
  }                                                                                
  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////

  dmatrix getSubMat(const dmatrix&amp; X, const int i1, const int i2){
    dmatrix ret(i1,i2,X.colmin(),X.colmax());
    for(int i=i1; i&lt;=i2; ++i){
      ret(i)=X(i);
    }    
    return ret;
  }
  
  dvector getSubVec(const dvector&amp; X, const int i1, const int i2){
    dvector ret(i1,i2);
    for(int i=i1; i&lt;=i2; ++i){
      ret(i)=X(i);
    }    
    return ret;
  }

  dvar_vector getSubVec(const dvar_vector&amp; X, const int i1, const int i2){
    dvar_vector ret(i1,i2);
    for(int i=i1; i&lt;=i2; ++i){
      ret(i)=X(i);
    }    
    return ret;
  }

  df1b2vector getSubVec(const df1b2vector&amp; X, const int i1, const int i2){
    df1b2vector ret(i1,i2);
    for(int i=i1; i&lt;=i2; ++i){
      ret(i)=X(i);
    }    
    return ret;
  }



  //Check that data has been read correctly
  bool checksums_ok(ivector checksum) {
    if(checksum(1)!=checksum(2)) {
      cout &lt;&lt; endl &lt;&lt; "CHECKSUM FAILURE!!!" &lt;&lt; endl;
      cout &lt;&lt; "Expected amount of input does not agree with amount read." &lt;&lt; endl;
      cout &lt;&lt; "Please check your data and configuration files." &lt;&lt; endl;
      cout &lt;&lt; "Checksum values : \t " &lt;&lt;checksum &lt;&lt; endl;
      exit(1);
    }
  return 1;
  };

DATA_SECTION
  !! cout&lt;&lt;"DATASECTION " &lt;&lt; endl; cout.flush(); 
  !! cout &lt;&lt; "sam.dat...";
  !! time(&amp;StartTime);  
  init_int noFleets
  !! TRACE(noFleets);
  init_ivector fleetTypes(1,noFleets)
  !! TRACE(fleetTypes);
  int ssbPhase
  int ssbPowPhase
  !! ssbPhase=-1;
  !! ssbPowPhase=-1;
  !! for(int i=1; i&lt;=noFleets; ++i){if(fleetTypes(i)==3){ssbPhase=1;}} 
  !! for(int i=1; i&lt;=noFleets; ++i){if(fleetTypes(i)==4){ssbPhase=1;ssbPowPhase=1;}} 
  !! TRACE(ssbPhase);
  !! TRACE(ssbPowPhase);
  init_vector fleetTimes(1,noFleets)
  !! TRACE(fleetTimes);
  init_int noYears
  !! TRACE(noYears);
  init_vector years(1,noYears)
  !! TRACE(years);
  vector times(0,noYears)
  !! times(0)=years(1)-1;
  !! times(1,noYears)=years;
  !! TRACE(times);
  init_int noObs
  !! TRACE(noObs);
  init_ivector idx1(1,noYears)
  !! TRACE(idx1);
  init_ivector idx2(1,noYears)
  !! TRACE(idx2);
  init_matrix data(1,noObs,1,4)
  !! TRACE(data);
  vector logObs(1,noObs)
  !! logObs=log(column(data,4));
  !! TRACE(logObs);
  int minAgeObs
  !! minAgeObs=(int)min(column(data,3));
  !! TRACE(minAgeObs);
  int maxAgeObs
  !! maxAgeObs=(int)max(column(data,3));
  !! TRACE(maxAgeObs);
  init_matrix propMature(1,noYears,minAgeObs,maxAgeObs)
  !! TRACE(propMature);
  init_matrix stockMeanWeight(1,noYears,minAgeObs,maxAgeObs)
  !! TRACE(stockMeanWeight);
  init_matrix catchMeanWeight(1,noYears,minAgeObs,maxAgeObs)
  !! TRACE(catchMeanWeight);
  init_matrix natMor(1,noYears,minAgeObs,maxAgeObs)
  !! TRACE(natMor);
  init_matrix landFrac(1,noYears,minAgeObs,maxAgeObs)
  !! TRACE(landFrac);
  init_matrix catchMeanWeightD(1,noYears,minAgeObs,maxAgeObs)
  !! TRACE(catchMeanWeightD);  
  init_matrix catchMeanWeightL(1,noYears,minAgeObs,maxAgeObs)
  !! TRACE(catchMeanWeightL);
  init_matrix Fprop(1,noYears,minAgeObs,maxAgeObs)
  !! TRACE(Fprop);
  init_matrix Mprop(1,noYears,minAgeObs,maxAgeObs)
  !! TRACE(Mprop);
  init_ivector dat_checksums(1,2)
  !! if(checksums_ok(dat_checksums)) cout &lt;&lt; "OK. " ;

  !! ad_comm::change_datafile_name("model.cfg");
  !! cout &lt;&lt; "model.cfg...";
  init_int minAge  
  !! TRACE(minAge);
  init_int maxAge 
  !! TRACE(maxAge);
  init_int maxAgePlusGroup;
  !! TRACE(maxAgePlusGroup);
  !! if((minAge&gt;minAgeObs)||(maxAge&lt;maxAgeObs)){cout&lt;&lt;"Error: ages in model.cfg mismatch."&lt;&lt;endl;} 
  !! //init_ivector likelihoodConf(1,2);
  !! //  TRACE(likelihoodConf);
  !! //  if(likelihoodConf(1)!=0){cout&lt;&lt;"Warning: Trying to use unimplimented likelihood, so quitting!"&lt;&lt;endl; ad_exit(1);}
  init_imatrix keyLogFsta(1,noFleets,minAge,maxAge)
  !! TRACE(keyLogFsta);
  int noLogFsta
  !! noLogFsta=max(keyLogFsta); 
  !! TRACE(noLogFsta);
  init_int corFlag
  !! TRACE(corFlag);
  init_imatrix keyLogFpar(1,noFleets,minAge,maxAge)
  !! TRACE(keyLogFpar);
  int noLogFpar
  !! noLogFpar=max(keyLogFpar); 
  !! TRACE(noLogFpar);

  init_imatrix keyQpow(1,noFleets,minAge,maxAge)
  !! TRACE(keyQpow);
  int noQpow
  !! noQpow=max(keyQpow); 
  !! TRACE(noQpow);

  init_imatrix keyVarF(1,noFleets,minAge,maxAge)
  !! TRACE(keyVarF);
  int noVarF
  !! noVarF=max(keyVarF);
  !! TRACE(noVarF); 
  init_ivector keyVarLogN(minAge,maxAge)
  !! TRACE(keyVarLogN); 
  int noVarLogN
  !! noVarLogN=max(keyVarLogN);
  !! TRACE(noVarLogN);  
  init_imatrix keyVarObs(1,noFleets,minAge,maxAge)
  !! TRACE(keyVarObs); 
  int noVarObs
  !! noVarObs=max(keyVarObs);
  !! TRACE(noVarObs);  
  init_int stockRecruitmentModelCode 
  !! TRACE(stockRecruitmentModelCode); 
  init_int noScaledYears
  !! TRACE(noScaledYears); 
  init_ivector keyScaledYears(1,noScaledYears)
  !! //TRACE(keyScaledYears); 
  init_imatrix keyParScaledYA(1,noScaledYears,minAge,maxAge)
  !! //TRACE(keyParScaledYA); 
  int noScaledPar  
  !! if(noScaledYears&gt;1){noScaledPar=max(keyParScaledYA);}else{noScaledPar=0;}
  !! TRACE(noScaledPar);
 
  int stateDim 
  !! stateDim=maxAge-minAge+1+noLogFsta; 
  !! TRACE(stateDim); 
  init_ivector fbarRange(1,2)  
  !! TRACE(fbarRange); 
  init_int timeout  
  !! TRACE(timeout); 
  init_ivector cfg_checksums(1,2)
  !! if(checksums_ok(cfg_checksums)) cout &lt;&lt; "OK. " ;


  //Read model initial guesses. These can either come from model.init
  //or a pin file. The -usepin commandline argument selects a pinfile
  //otherwise model.init is used
  !!if(!usepin) { 
     !! ad_comm::change_datafile_name("model.init");
     !! cout &lt;&lt; "model.init...";
     init_number varLogFstaInit;
     init_number varLogNInit;
     init_number varLogObsInit;
     init_number logFparInit;
     init_number rec_logaInit;
     init_number rec_logbInit;
     init_ivector ini_checksums(1,2)
     !! if(checksums_ok(ini_checksums)) cout &lt;&lt; "OK. " ;
  !!}


  !! ad_comm::change_datafile_name("reduced.cfg");
  !! cout &lt;&lt; "reduced.cfg...";
  init_ivector retro(1,noFleets);
  int reducedRun;
  !! if(sum(square(retro))&gt;0){reducedRun=1;}else{reducedRun=0;} 
  init_ivector red_checksums(1,2)
  !! if(checksums_ok(red_checksums)) cout &lt;&lt; "OK. " ;


  ivector lastYearData(1,noFleets);
  !!  lastYearData=0;
  !!  for(int i=1; i&lt;=noObs; ++i){
  !!    for(int f=1; f&lt;=noFleets; ++f){
  !!      if((lastYearData(f)&lt;(int)data(i,1))&amp;&amp;((int)data(i,2)==f))lastYearData(f)=(int)data(i,1);
  !!    }    
  !!  }   
  !!  for(int f=1; f&lt;=noFleets; ++f){
  !!    if(retro(f)&gt;=1)lastYearData(f)-=retro(f);
  !!  }
  !!  noYears=noYears-((int)years(noYears)-(int)max(lastYearData));  
  matrix residuals(1,noObs,1,6)
  !! cout&lt;&lt;"  ---  Done."&lt;&lt;endl; cout.flush(); 

PARAMETER_SECTION
  !! cout&lt;&lt;"PARAMETERSECTION"; cout.flush(); 
  init_vector logFpar(1,noLogFpar); 
  init_vector logQpow(1,noQpow); 
  init_bounded_vector logSdLogFsta(1,noVarF,-3,3); 
  init_bounded_vector logSdLogN(1,noVarLogN,-3,3); 
  init_bounded_vector logSdLogObs(1,noVarObs,-3,3); 
  !! int rec_phase=1; 
  !! if(stockRecruitmentModelCode==0){rec_phase=-1;}
  init_number rec_loga(rec_phase);
  init_number rec_logb(rec_phase);
  !! int cor_phase=1;
  !! if(corFlag==0){cor_phase=-1;}
  init_bounded_number rho(-0.99,0.99,cor_phase);
  init_vector logScale(1,noScaledPar); 
  init_number logScaleSSB(ssbPhase);
  init_number logPowSSB(ssbPowPhase);
  init_number logSdSSB(ssbPhase);  
  vector scaledLogObs(1,noObs);
  matrix X(1,noYears,1,stateDim);  

  random_effects_vector U(1,stateDim*noYears);

  objective_function_value jnll;
  
  sdreport_vector ssb(1,noYears);
  sdreport_vector logssb(1,noYears);
  sdreport_vector logCatch(1,noYears);
  sdreport_vector fbar(1,noYears);
  sdreport_vector logfbar(1,noYears);
  sdreport_vector tsb(1,noYears);
  sdreport_vector logtsb(1,noYears);
  !! cout&lt;&lt;"  ---  Done."&lt;&lt;endl; cout.flush(); 
PRELIMINARY_CALCS_SECTION
  cout&lt;&lt;"PRELIMINARYSECTION"; cout.flush(); 
  //Initialise model initial guesses. These can either come from model.init
  //or a pin file. The -usepin commandline argument selects a pinfile
  //otherwise model.init is used
  if(!usepin) {  //Then initialise from values read from model.init
     logSdLogFsta=log(sqrt(varLogFstaInit)); 
     logSdLogN=log(sqrt(varLogNInit)); 
     logSdLogObs=log(sqrt(varLogObsInit)); 
   
     logFpar=logFparInit;
     rec_loga=rec_logaInit;
     rec_logb=rec_logbInit;
  }

  if(noQpow&gt;0){ 
    logQpow=0.0;
  }
  
  if(reducedRun==1){
    assignInit(logFpar); 
    assignInit(logQpow); 
    assignInit(logSdLogFsta); 
    assignInit(logSdLogN); 
    assignInit(logSdLogObs); 
    assignInit(rec_loga);
    assignInit(rec_logb);
    assignInit(logScale); 
    assignInit(U); 

    int redIdx;
    for(int i=retro.indexmin(); i&lt;=retro.indexmax(); ++i){
      if(retro(i)==-1){
        for(int a=minAge; a&lt;=maxAge; ++a){
          redIdx=keyLogFpar(i,a);
          if(redIdx!=0){
            logFpar(redIdx)=1.0;
          }
          redIdx=keyVarObs(i,a);
          if(redIdx!=0){
            logSdLogObs(redIdx)=1.0;
          }
        }
      }
      if((retro(i)&gt;0)&amp;&amp;(fleetTypes(i)==0)){
        for(int y=1; y&lt;=noScaledYears; ++y){
          if(keyScaledYears(y)&gt;lastYearData(i)){
            for(int a=minAge; a&lt;=maxAge; ++a){
              logScale(keyParScaledYA(y,a))=0;
            }
          }
        }
      }
    }  
  }
  cout&lt;&lt;"  ---  Done."&lt;&lt;endl; cout.flush(); 
PROCEDURE_SECTION
  time_t currentTime;
  time(&amp; currentTime);
  if(difftime(currentTime,StartTime)&gt;timeout){ // Terminate after user defined time 
    cout&lt;&lt;endl;
    cout&lt;&lt;"############################################################"&lt;&lt;endl; 
    cout&lt;&lt;"############################################################"&lt;&lt;endl; 
    cout&lt;&lt;"############################################################"&lt;&lt;endl; 
    cout&lt;&lt;"     MAX TIME ALLOWED EXCEEDED - MODEL DID NOT FINISH"&lt;&lt;endl;  
    cout&lt;&lt;"############################################################"&lt;&lt;endl; 
    cout&lt;&lt;"############################################################"&lt;&lt;endl; 
    cout&lt;&lt;"############################################################"&lt;&lt;endl; 
    cout&lt;&lt;endl;
    ad_exit(1);
  } 

  jnll=0.0;
  if(reducedRun==1){
    int redIdx;
    for(int i=retro.indexmin(); i&lt;=retro.indexmax(); ++i){
      if(retro(i)==-1){
        for(int a=minAge; a&lt;=maxAge; ++a){
          redIdx=keyLogFpar(i,a);
          if(redIdx!=0){
            jnll+=square(logFpar(redIdx)-1.0);
          }
          redIdx=keyVarObs(i,a);
          if(redIdx!=0){
            jnll+=square(logSdLogObs(redIdx)-1.0);
          }
          redIdx=keyQpow(i,a);
          if(redIdx!=0){
            jnll+=square(logQpow(redIdx)-0.0)*1.0e-6;
          }

        }
      }
      if((retro(i)&gt;0)&amp;&amp;(fleetTypes(i)==0)){
        for(int y=1; y&lt;=noScaledYears; ++y){
          if(keyScaledYears(y)&gt;lastYearData(i)){
            for(int a=minAge; a&lt;=maxAge; ++a){
              jnll+=square(logScale(keyParScaledYA(y,a))-0.0);
            }
          }
        }
      }
    }  
  }

  // First the scale parameter is applied where requested;
  scaledLogObs=logObs;
  if(noScaledYears&gt;=1){
    int y, a;
    for(int yi=1; yi&lt;=noScaledYears; ++yi){
      y=keyScaledYears(yi);
      for(int i=1; i&lt;=noObs; ++i){
        a=(int)data(i,3);
        if((data(i,1)==y)&amp;&amp;(fleetTypes((int)data(i,2))&lt;2)&amp;&amp;(keyParScaledYA(yi,a)&gt;0)){
          scaledLogObs(i)+=logScale(keyParScaledYA(yi,a));
        }
      }
    }
  }

  for(int y=1; y&lt;=noYears-1; ++y){
    step(y,U((y-1)*stateDim+1,y*stateDim),U(y*stateDim+1,(y+1)*stateDim),logFpar,rec_loga,rec_logb,logSdLogN,logSdLogFsta,rho);
  }

  for(int y=1; y&lt;=noYears; ++y){    
    int idxlow=idx1(y);
    int idxhigh=idx2(y);
    dmatrix subData=getSubMat(data,idxlow,idxhigh);     
    dvar_vector subObs=getSubVec(scaledLogObs,idxlow,idxhigh);  
    if(noQpow&gt;0){
      obs(U((y-1)*stateDim+1,y*stateDim),subData,subObs,logFpar,logSdLogObs,logQpow,logScaleSSB,logPowSSB,logSdSSB);
    }else{
      dvar_vector fakeQpow(1,1);
      obs(U((y-1)*stateDim+1,y*stateDim),subData,subObs,logFpar,logSdLogObs,fakeQpow,logScaleSSB,logPowSSB,logSdSSB);
    }
  }

  if(sd_phase()){
    for(int y=1; y&lt;=noYears; ++y){
      for(int i=1; i&lt;=stateDim; ++i){
        X(y,i)=U((y-1)*stateDim+i);
      }
      ssb(y)=SSB(X(y),propMature(y),stockMeanWeight(y),Fprop(y),Mprop(y),natMor(y));
      logssb(y)=log(ssb(y));
      tsb(y)=TSB(X(y),stockMeanWeight(y));
      logtsb(y)=log(tsb(y));
      fbar(y)=FBAR(X(y),fbarRange(1),fbarRange(2));
      logfbar(y)=log(fbar(y));
      int idxlow=idx1(y);
      int idxhigh=idx2(y);
      dmatrix subData=getSubMat(data,idxlow,idxhigh);     
      dvar_vector subObs=getSubVec(scaledLogObs,idxlow,idxhigh);
      logCatch(y)=log(CATCH(X(y),natMor(y),catchMeanWeight(y)));
    }    

  }

SEPARABLE_FUNCTION void step(const int y, const dvar_vector&amp; u1,const dvar_vector&amp; u2, const dvar_vector&amp; logFpar, const dvariable&amp; rec_loga, const dvariable&amp; rec_logb, const dvar_vector&amp; logSdLogN, const dvar_vector&amp; logSdLogFsta, const dvariable&amp; rho)
  dvar_vector x1(1,stateDim);   
  int n1=u1.indexmin();
  int n2=u1.indexmax();
  for(int i=n1; i&lt;=n2; ++i){
    x1(i-n1+1)=u1(i);
  }

  dvar_vector x2(1,stateDim);   
  n1=u2.indexmin();
  n2=u2.indexmax();
  for(int i=n1; i&lt;=n2; ++i){
    x2(i-n1+1)=u2(i);
  }

  dvar_vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f&lt;=noFleets; ++f){
    if(fleetTypes(f)&lt;2){ // means that it is not a survey
      for(int a=minAge; a&lt;=maxAge; ++a){
        if(keyLogFsta(f,a)&gt;0){
          Ftot(a)+=exp(x1(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)&gt;0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }

  dvar_vector pred(1,stateDim);   
  pred=x1;

  dvariable ssb=0.0; 
  for(int a=minAge; a&lt;=maxAge; ++a){
    ssb+=exp(x1(a-minAge+1))*exp(-Ftot(a)*Fprop(y,a)-natMor(y,a)*Mprop(y,a))*propMature(y,a)*stockMeanWeight(y,a);
  }

  if(stockRecruitmentModelCode==0){
    // do nothing 
  }else{
    if(stockRecruitmentModelCode==1){//ricker
      pred(minAge-minAge+1)=rec_loga+log(ssb)-exp(rec_logb)*ssb; 
    }else{
      if(stockRecruitmentModelCode==2){//BH
        pred(minAge-minAge+1)=rec_loga+log(ssb)-log(1+exp(rec_logb)*ssb); 
      }else{
        cout&lt;&lt;"SR model code not recognized"&lt;&lt;endl;
      }
    }
  }
  for(int a=minAge+1; a&lt;=maxAge; ++a){
    pred(a-minAge+1)=x1(a-1-minAge+1)-Ftot(a-1)-natMor(y,a-1); 
  }
  
  if(maxAgePlusGroup==1){
    pred(maxAge-minAge+1)=log(exp(x1(maxAge-1-minAge+1)-Ftot(maxAge-1)-natMor(y,maxAge-1))+
                              exp(x1(maxAge-minAge+1)-Ftot(maxAge)-natMor(y,maxAge))); 
  }
  
  dvar_vector var(1,stateDim);
  for(int a=minAge; a&lt;=maxAge; ++a){
    var(a-minAge+1)=exp(2.0*logSdLogN(keyVarLogN(a))); 
  }
  int idx; 
  for(int f=1; f&lt;=noFleets; ++f){
    for(int a=minAge; a&lt;=maxAge; ++a){
      idx=keyLogFsta(f,a); 
      if(idx&gt;0){
        idx+=maxAge-minAge+1; 
        var(idx)=exp(2.0*logSdLogFsta(keyVarF(f,a))); 
      } 
    }
  }

  if(corFlag==0){ // not correlated 
    for(int i=1; i&lt;=stateDim; ++i){
      jnll+=0.5*(log(2.0*M_PI*var(i))+square(x2(i)-pred(i))/var(i));
    } 
  }else{          // correlated
    for(int i=1; i&lt;=(maxAge-minAge+1); ++i){
      jnll+=0.5*(log(2.0*M_PI*var(i))+square(x2(i)-pred(i))/var(i));
    } 

    int Fdim=stateDim-(maxAge-minAge+1);
    dvar_matrix fvar(maxAge-minAge+2,stateDim,maxAge-minAge+2,stateDim);
    dvar_matrix fcor(maxAge-minAge+2,stateDim,maxAge-minAge+2,stateDim);
    dvar_vector fsd(maxAge-minAge+2,stateDim);

    fvar.initialize();
    for(int i=(maxAge-minAge+2); i&lt;=stateDim; ++i){
      for(int j=(maxAge-minAge+2); j&lt;=stateDim; ++j){
        if(i!=j){fcor(i,j)=rho;}else{fcor(i,j)=1.0;}
      }
    }
    for(int i=(maxAge-minAge+2); i&lt;=stateDim; ++i){
      //fvar(i,i)=var(i);
      fsd(i)=sqrt(var(i));
      //fcor(i,i)=1.0;
    }
    fvar=elem_prod(outer_prod(fsd,fsd),fcor);
    //cout&lt;&lt;fcor&lt;&lt;endl&lt;&lt;endl;
    jnll+=nLogNormal(pred((maxAge-minAge+2),stateDim),x2(maxAge-minAge+2,stateDim),fvar);
  } 


SEPARABLE_FUNCTION void obs(const dvar_vector&amp; u, const dmatrix&amp; data, const dvar_vector&amp; obs, const dvar_vector&amp; logFpar, const dvar_vector&amp; logSdLogObs, const dvar_vector&amp; logQpow, const dvariable&amp; logScaleSSB, const dvariable&amp; logPowSSB, const dvariable&amp; logSdSSB)
  int n1=data.rowmin();
  int n2=data.rowmax();
  dvar_vector pred(n1,n2);

  dvar_vector x(1,stateDim);   
  int xn1=u.indexmin();
  int xn2=u.indexmax();
  for(int i=xn1; i&lt;=xn2; ++i){
    x(i-xn1+1)=u(i);
  }
  
  dvar_vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f&lt;=noFleets; ++f){
    if(fleetTypes(f)&lt;2){ // means that it is not a survey
      for(int a=minAge; a&lt;=maxAge; ++a){
        if(keyLogFsta(f,a)&gt;0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)&gt;0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }

  dvar_vector Z(Ftot.indexmin(),Ftot.indexmax());
  Z=Ftot; // missing M here
  int isMadded=0; 
  int f;
  int ft;
  int a; 
  int y, yIdx; 

  for(int i=n1; i&lt;=n2; ++i){
    f=(int)data(i,2);
    ft=fleetTypes(f);
    a=(int)data(i,3);
    y=(int)data(i,1);
    if(isMadded==0){
      for(int aa=minAge; aa&lt;=maxAge; ++aa){Z(aa)+=natMor((int)(y-years(1)+1),aa);} // added M here
      isMadded=1; 
    }
    if(ft==0){// residual fleet
      pred(i)=x(a-minAge+1)-log(Z(a))+log(1-exp(-Z(a)));
      if(keyLogFsta(f,a)&gt;0){
        pred(i)+=x(keyLogFsta(f,a)+maxAge-minAge+1);
      }
      if(keyLogFpar(f,a)&gt;0){
        pred(i)+=logFpar(keyLogFpar(f,a));
      }
    }else{
      if(ft==1){// comm fleet
        cout&lt;&lt;"Not implemented yet!!!"&lt;&lt;endl;  
        ad_exit(1);
      }else{
        if(ft==2){// survey
          pred(i)=x(a-minAge+1)-Z(a)*fleetTimes(f);
          if(keyQpow(f,a)&gt;0){
            pred(i)*=exp(logQpow(keyQpow(f,a))); 
          }
          if(keyLogFsta(f,a)&gt;0){
            pred(i)+=x(keyLogFsta(f,a)+maxAge-minAge+1);
          }
          if(keyLogFpar(f,a)&gt;0){
            pred(i)+=logFpar(keyLogFpar(f,a));
          }
        }else{
          if(ft==3){// SSB survey
            dvariable ssb=0.0; 
            for(int a=minAge; a&lt;=maxAge; ++a){
              yIdx=(int)(y-years(1)+1); 
              ssb+=exp(x(a-minAge+1))*exp(-Ftot(a)*Fprop(yIdx,a)-natMor(yIdx,a)*Mprop(yIdx,a))*propMature(yIdx,a)*stockMeanWeight(yIdx,a);
            }
            pred(i)=log(ssb)+logScaleSSB;
          }else{
            if(ft==4){
              dvariable ssb=0.0; 
              for(int a=minAge; a&lt;=maxAge; ++a){
                yIdx=(int)(y-years(1)+1); 
                ssb+=exp(x(a-minAge+1))*exp(-Ftot(a)*Fprop(yIdx,a)-natMor(yIdx,a)*Mprop(yIdx,a))*propMature(yIdx,a)*stockMeanWeight(yIdx,a);
              }
              pred(i)=exp(logPowSSB)*log(ssb)+logScaleSSB;
            }
          }
        } 
      }
    }      
  } 

  dvariable var=0.0;
  for(int i=n1; i&lt;=n2; ++i){
    f=(int)data(i,2);
    a=(int)data(i,3);
    if((retro(f)==-1)||((int)data(i,1)&gt;lastYearData(f))){
      residuals(i,1)=data(i,1);
      residuals(i,2)=f;
      residuals(i,3)=a;
      residuals(i,4)=0;     
      residuals(i,5)=0;     
      residuals(i,6)=0;     
    }else{
      if((fleetTypes(f)==3)||(fleetTypes(f)==4)){
        var=exp(2.0*logSdSSB); 
      }else{ 
        var=exp(2.0*logSdLogObs(keyVarObs(f,a)));
      } 
      jnll+=0.5*(log(2.0*M_PI*var)+square(obs(i)-pred(i))/var);
      residuals(i,1)=data(i,1);
      residuals(i,2)=f;
      residuals(i,3)=a;
      residuals(i,4)=value(obs(i));     
      residuals(i,5)=value(pred(i));     
      residuals(i,6)=value((obs(i)-pred(i))/sqrt(var));     
    }
  } 
  
FUNCTION dvariable SSB(dvar_vector x, dvector p, dvector w, dvector fprop, dvector mprop, dvector M)
  dvar_vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f&lt;=noFleets; ++f){
    if(fleetTypes(f)&lt;2){ // means that it is not a survey
      for(int a=minAge; a&lt;=maxAge; ++a){
        if(keyLogFsta(f,a)&gt;0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)&gt;0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }

  dvariable ret;
  ret=0; 
  for(int a=minAge; a&lt;=maxAge; ++a){
    ret+=exp(x(a-minAge+1))*exp(-Ftot(a)*fprop(a)-M(a)*mprop(a))*p(a)*w(a);
  }
  return ret;

FUNCTION dvariable CATCH(dvar_vector x, dvector M, dvector w)
  dvariable ret; 
  ret=0;
  dvar_vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f&lt;=noFleets; ++f){
    if(fleetTypes(f)&lt;2){ // means that it is not a survey
      for(int a=minAge; a&lt;=maxAge; ++a){
        if(keyLogFsta(f,a)&gt;0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)&gt;0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  dvar_vector Z(minAge,maxAge);
  Z=Ftot+M; 

  dvar_vector logCatch(minAge,maxAge); 
  for(int a=minAge; a&lt;=maxAge; ++a){
    logCatch(a)=x(a-minAge+1)-log(Z(a))+log(1-exp(-Z(a)))+log(Ftot(a));
  }
  ret=sum(elem_prod(exp(logCatch),w));
  return ret;

  

FUNCTION dvariable TSB(dvar_vector x, dvector w)
  dvariable ret;
  ret=0; 
  for(int a=minAge; a&lt;=maxAge; ++a){
    ret+=exp(x(a-minAge+1))*w(a);
  }
  return ret;

FUNCTION dvariable FBAR(dvar_vector x, int from, int to)
  dvariable ret;
  dvar_vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f&lt;=noFleets; ++f){
    if(fleetTypes(f)&lt;2){ // means that it is not a survey
      for(int a=minAge; a&lt;=maxAge; ++a){
        if(keyLogFsta(f,a)&gt;0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)&gt;0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  ret=mean(Ftot(from,to));
  return ret;


REPORT_SECTION
  report&lt;&lt;stateDim&lt;&lt;endl;
  report&lt;&lt;years&lt;&lt;endl; 

  ofstream resout("sam.res");
  resout&lt;&lt;residuals&lt;&lt;endl;


TOP_OF_MAIN_SECTION
  cout &lt;&lt; "SAM State-space Assessment Model" &lt;&lt; endl;
  cout &lt;&lt; "More info at: http://www.stockassessment.org" &lt;&lt; endl;
  cout &lt;&lt; "--------------------------------" &lt;&lt; endl;
  cout &lt;&lt; "$Rev: 17 $" &lt;&lt; endl &lt;&lt; "$LastChangedDate: 2011-11-10 17:52:12 +0100 (Thu, 10 Nov 2011) $"  &lt;&lt;endl &lt;&lt; endl;

  arrmblsize=2000000;
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(150000);
  gradient_structure::set_CMPDIF_BUFFER_SIZE(800000);
  gradient_structure::set_MAX_NVAR_OFFSET(100000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);

  //Check for usepin command line argument using ADMB built-in option_match arg
  if(option_match(argc,argv,"-usepin")&gt;-1 || option_match(argc,argv,"--usepin")&gt;-1) {usepin=true;}
  if(usepin) {  cout &lt;&lt; "Using pin file in preference to model.init" &lt;&lt; endl;
   } else {cout &lt;&lt; "Using model.init" &lt;&lt; endl;}
</pre></body></html>