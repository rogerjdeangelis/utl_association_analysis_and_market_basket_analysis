MarketBasket analysis in SAS/WPS/R

 We want to answer this question.

   What are customers likely to buy if they purchase whole milk?

         LHS         Ahat else in basket    SUPPORT    CONFIDENCE      LIFT

     {whole milk}    {root vegetables}     0.048907      0.19140     1.75603   * almost 2 times as likely to purchase vegetables
     {whole milk}    {tropical fruit}      0.042298      0.16554     1.57759
     {whole milk}    {yogurt}              0.056024      0.21926     1.57174
     {whole milk}    {other vegetables}    0.074835      0.29288     1.51363
     {whole milk}    {rolls/buns}          0.056634      0.22165     1.20503


WORKING CODE

        * from SAS/WPS;
        have<-read_sas("d:/sd1/have.sas7bdat");

        * create market basket data structure;
        formatdata <- split(have$PRODUCTS, have$ID);
        foodlist <- as(formatdata,"transactions");

        * get available rules and we will pick the first 5;
        rules <- apriori(foodlist, parameter = list(supp = 0.001, conf = 0.8));
        rules<-sort(rules, decreasing=TRUE,by="confidence");
        res<-inspect(rules[1:5]);

        * column two is a special R construct an "=>" and is not needed;
        wantwps<-as.data.frame(res[,-2]);

        * send to SAS/WPS;
        import r=wantwps data=wrk.wantwps;


 see
 http://www.salemmarafi.com/code/market-basket-analysis-with-r/

HAVE  MARKET BASKETS
====================

  csv, xlsx, and sas7bdat  (I am using groceries csv file which can be downloaded from many sites)
                           ( also in the arules package)

  You can pick one of these or surf net for graceries.

  CSV
  https://www.dropbox.com/s/auqnh93u5f8887j/groceries.csv?dl=0

  XLSX
  https://www.dropbox.com/s/ug97hxkr0wuuvp3/groceries.xlsx?dl=0

  have,sas7bdat
  https://www.dropbox.com/s/0egnm9rz14eqhhs/have.zip?dl=0


Up to 40 obs SD1.HAVE total obs=43,367
      Customer
  Obs    ID    PRODUCTS

    1     1    citrus fruit
    2     1    semi-finished bread     Georges basket
    3     1    margarine
    4     1    ready soups

    5     2    tropical fruit          Maries basket
    6     2    yogurt
    7     2    coffee

    8     3    whole milk              Mikes basket

    9     4    pip fruit               Teds basket
   10     4    yogurt
   11     4    cream cheese
   12     4    meat spreads


WANT
===

Customers who purchase whole milk are 75% more likely to purchase root vegetables
then the overall likelihood.

Up to 40 obs from wantwps total obs=5

Obs        LHS         RHS                    SUPPORT    CONFIDENCE      LIFT

 1     {whole milk}    {root vegetables}     0.048907      0.19140     1.75603
 2     {whole milk}    {tropical fruit}      0.042298      0.16554     1.57759
 3     {whole milk}    {yogurt}              0.056024      0.21926     1.57174
 4     {whole milk}    {other vegetables}    0.074835      0.29288     1.51363
 5     {whole milk}    {rolls/buns}          0.056634      0.22165     1.20503


*                _              _       _
 _ __ ___   __ _| | _____    __| | __ _| |_ __ _
| '_ ` _ \ / _` | |/ / _ \  / _` |/ _` | __/ _` |
| | | | | | (_| |   <  __/ | (_| | (_| | || (_| |
|_| |_| |_|\__,_|_|\_\___|  \__,_|\__,_|\__\__,_|

;

options validvarname=upcase;
libname sd1 "d:/sd1";
libname xel "d:/xls/groceries.xlsx" scan_text=no header=no; * get from web;
data sd1.have;
  retain id products;
  length products $25;
  set xel.'groceries$'n;
  array fs[32] $25 f1-f32;
  format _all_;
  informat _all_;
  id=_n_;
  do i=1 to 32;
    if fs[i] ne '' then do;
       products=fs[i];
       output;
    end;
  end;
  drop i f1-f32;
run;quit;
libname xel clear;

*          _       _   _
 ___  ___ | |_   _| |_(_) ___  _ __
/ __|/ _ \| | | | | __| |/ _ \| '_ \
\__ \ (_) | | |_| | |_| | (_) | | | |
|___/\___/|_|\__,_|\__|_|\___/|_| |_|

;

%utl_submit_wps64('
options set=R_HOME "C:/Program Files/R/R-3.3.2";
libname sd1 "d:/sd1";
libname wrk sas7bdat "%sysfunc(pathname(work))";
proc r;
submit;
source("c:/Program Files/R/R-3.3.2/etc/Rprofile.site",echo=T);
library(arules);
library(haven);
have<-read_sas("d:/sd1/have.sas7bdat");
formatdata <- split(have$PRODUCTS, have$ID);
foodlist <- as(formatdata,"transactions");
rules <- apriori(foodlist, parameter = list(supp = 0.001, conf = 0.8));
head(rules);
rules <- apriori(foodlist, parameter = list(supp = 0.001, conf = 0.8,maxlen=3));
subset.matrix <- is.subset(rules, rules);
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA;
redundant <- colSums(subset.matrix, na.rm=T) >= 1;
rules.pruned <- rules[!redundant];
rules<-rules.pruned;
rules<-apriori(data=foodlist, parameter=list(supp=0.001,conf = 0.15,minlen=2),
  appearance = list(default="rhs",lhs="whole milk"),
  control = list(verbose=F));
rules<-sort(rules, decreasing=TRUE,by="confidence");
wantwps<-inspect(rules[1:5])[,-2];
endsubmit;
import r=wantwps data=wrk.wantwps;
run;quit;
');

/*
The WPS System

Absolute minimum support count: 9
set item appearances ...[0 item(s)] done [0.00s].
set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
sorting and recoding items ... [157 item(s)] done [0.00s].
creating transaction tree ... done [0.02s].
checking subsets of size 1 2 3 done [0.00s].
writing ... [29 rule(s)] done [0.00s].
creating S4 object  ... done [0.00s].
    lhs             rhs                support    confidence lift
[1] {whole milk}    {root vegetables}     0.048907      0.19140     1.75603
[2] {whole milk}    {tropical fruit}      0.042298      0.16554     1.57759
[3] {whole milk}    {yogurt}              0.056024      0.21926     1.57174
[4] {whole milk}    {other vegetables}    0.074835      0.29288     1.51363
[5] {whole milk}    {rolls/buns}          0.056634      0.22165     1.20503
NOTE: 39 records were read from the infile "wps_pgm.lst".
*/




