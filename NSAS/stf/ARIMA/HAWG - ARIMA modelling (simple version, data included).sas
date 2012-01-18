data RAWDATA;
     input Year R_ICA WinterNAO WinterAMO;
     datalines;
     1960     12087837     -0.1475      0.16950
     1961    109000000      2.0150      0.12950
     1962     46275907     -0.5725      0.19250
     1963     47657598     -0.9650      0.18475
     1964     62784953     -0.7650      0.00425
     1965     34894783     -1.0075     -0.12075
     1966     27858148      0.2250     -0.02600
     1967     40255855      1.6050      0.07775
     1968     38698462     -0.0175     -0.17800
     1969     21581503     -2.0850      0.05475
     1970     41072449     -0.5250      0.00750
     1971     32306362     -0.6400     -0.23950
     1972     20858534      0.0750     -0.33550
     1973     10102036      1.4350     -0.33875
     1974     21688511      0.4925     -0.27250
     1975      2814491      1.1575     -0.29300
     1976      2720374      0.5850     -0.38225
     1977      4326038     -1.0950     -0.30525
     1978      4594665      0.3300     -0.12475
     1979     10600186     -1.3450     -0.15800
     1980     16716729      0.0725     -0.03800
     1981     37860685      0.9000     -0.11725
     1982     64740217      0.2475     -0.02750
     1983     61794951      2.0000     -0.14050
     1984     53439842      0.7425     -0.04175
     1985     80893853     -0.3800     -0.30125
     1986     97583821     -0.0325     -0.27075
     1987     86180225      0.3375     -0.18900
     1988     42262187      0.0975      0.00775
     1989     39173461      2.8600     -0.16500
     1990     35871987      2.3725     -0.15925
     1991     33634923      0.2050     -0.06275
     1992     62138096      1.6800     -0.11525
     1993     50250988      1.4250     -0.20700
     1994     34500565      1.8000     -0.25525
     1995     41602863      2.4375     -0.00550
     1996     49747245     -2.3225      0.02000
     1997     28730419      0.1750     -0.02850
     1998     27373602      0.8000      0.25725
     1999     67697220      0.9825      0.15475
     2000     40678164      1.8475      0.04025
     2001     90678376     -0.5000     -0.01875
     2002     30444152      0.7900      0.21900
     2003     19069558      0.3975      0.08050
     2004     23729100     -0.2025      0.24375
     2005     16141706     -0.1125      0.22275
     2006     27136239     -0.8175      0.16025
     2007     17358063      1.8250      0.21475
     2008     20044858      1.3675      0.15150
     2009     32832169     -0.3125     -0.04275
     ;


%macro ARIMA;

     title;
     data zeit;
          Start = time();
          Datum = date();
     
     options cleanup nonumber nocenter replace mprint mtrace source nodate pagesize=10000 linesize=240;
     goptions reset=goptions ftext="arial" htext=1.5 csymbol=black cback=white;
     
     %let hsymbol=1.5;

     data _null_;
          file print notitle;
          datum=date();
          zeit=time();
          put "------------------------------------------------------";
          put " ";
          put @5"Erstellungsdatum  : " Datum date. " - " Zeit time.;
          put " ";
          put "------------------------------------------------------";
          put " ";
     run;

     data _NUL_;
          set RAWDATA nobs=nobs end=eof;
          if eof then call symput ("nobs",nobs);

     proc print data=RAWDATA;
          id Year;
          var R_ICA WinterNAO WinterAMO;
     run;

     proc arima data=RAWDATA;
          i var=R_ICA(1) crosscorr=(WinterNAO(1) WinterAMO(1)) noprint;
          e input=(5$ / (1 2 4) WinterNAO 3$ / (1 2) WinterAMO) method=ml outmodel=OUTMODEL outstat=OUTSTAT; * denominator model;
          f lead=0 id=Year out=ARIMA_OUT;
     run;

     /* P L O T S */
     
     data ARIMA_OUT;
          merge RAWDATA (keep=Year R_ICA) ARIMA_OUT (drop=R_ICA);
          by Year;
          label WinterAMO="AMO index" WinterNAO="NAO index" R_ICA="R observed" Forecast="R predicted" Residual="residual R";

     goptions colors=(orange yellow gray black red blue green) ctext=black;
     symbol1 c=gray  v=dot    i=join    l=3 h=&hsymbol pointlabel=none w=1;
     symbol2 c=red   v=none   i=join    l=1 h=&hsymbol pointlabel=none w=2;
     symbol3 c=blue  v=none   i=join    l=1 h=&hsymbol pointlabel=none w=1;
     symbol4 c=black v=dot    i=RLCLI95 l=1 h=&hsymbol pointlabel=("#Year" C=blue) ci=red w=2;
     symbol5 c=black v=none   i=needle  l=1 h=1        pointlabel=none w=3;
     symbol6 c=red   v=none   i=spline  l=1 h=1        pointlabel=none w=1;
     axis1 label=(angle=90 h=2 "Recruitment");
     axis2 label=(h=2);
     axis3 order=(0 to 5 by 1) label=(angle=90 h=2 "SSB E+06 [kg]");
     proc gplot data=ARIMA_OUT;
          plot R_ICA*Year=1 ForeCast*Year=2 L95*Year=3 U95*Year=3 / overlay vaxis=axis1 haxis=axis2 caxis=black;
     run;
     quit;

     proc sort data=ARIMA_OUT;
          by ForeCast R_ICA;
     run;

     axis1 label=(angle=90 h=2 "R observed");
     axis2 label=(h=2 "R predicted");
     proc gplot data=ARIMA_OUT;
          plot R_ICA*ForeCast=4 / overlay vaxis=axis1 haxis=axis2 caxis=black;
     run;
     quit;

     axis1 label=(angle=90 h=2 "Residuals");
     axis2 label=(h=2 "Year");
     proc gplot data=ARIMA_OUT;
          plot Residual*Year=5 / overlay vaxis=axis1 haxis=axis2 caxis=black;
     run;
     quit;

     /* D I A G N O S T I S C H E  K E N N Z A H L E N */

     proc reg data=ARIMA_OUT;
          model R_ICA = ForeCast;
     run;

     proc corr data=ARIMA_OUT;
          var ForeCast R_ICA;
     run;

     /* AICC estimation */

     data OUTSTAT;
          set OUTSTAT;
          if compress(_STAT_)="NUMRESID" then call symput ("NResids",left(_value_));

     proc sort data=ARIMA_OUT;
          by Year;

     data EXTRA;
          set ARIMA_OUT;
          if forecast;

     data EXTRA;
          set EXTRA;
          n_resids=&NResids;
          n_obs=&NObs;
          p=n_obs-n_resids;
          u=R_ICA-Forecast;
          SSE+u**2;
          MSE=SSE/n_obs;
          AICC=log(MSE)+2*p/(n_obs-p-2);

     data EXTRA;
          set EXTRA nobs=nobs;
          if not (_n_ = nobs) then delete;

     title "AICC extra calculated";
     proc print data=EXTRA;
          var n_obs n_resids p SSE MSE AICC;
     run;

     /* NV-Test */
     
     title;
     goptions reset=goptions ftext="arial" ctext=black htext=2.0 csymbol=black cback=white;
     proc univariate data=ARIMA_OUT;
          var Residual;
          histogram / normal(mu=est sigma=est);
          qqplot / normal(mu=est sigma=est);
     run;
     quit;
     
/*---------------------------------------------------------------------------------------------*/

     data zeit;
          file print notitle;
          set zeit;
          Ende     = time();
          Sek_Diff = (Ende - Start);
          Min_Diff = Sek_Diff/60;
          Std_Diff = Sek_Diff/3600;
          put 80*'_';
          put " ";
          put @10 "AusfÅhrungsbeginn : " Start time.;
          put @10 "AusfÅhrungsende   : " Ende time.;
          if (Sek_Diff < 60) then put @10 "AusfÅhrungsdauer  : " Sek_Diff "Sek.";
             else if (Sek_Diff < 3600) then
                          put @10 "AusfÅhrungsdauer  : " Min_Diff "Min.";
                     else put @10 "AusfÅhrungsdauer  : " Std_Diff "Std.";
          put 80*'_';

     data zeit;

     run;
%mend;

%ARIMA;
