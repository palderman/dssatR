#include <Rcpp.h>

extern "C"{
  void for_inp_spe_(char *filecc, int *filecc_len, double *fnpgl, double *fnpgn, 
                    double *lmxref, double *lnref, double *qeref, double *slwref, 
                    char *typpgl, char *typpgn, double *xlmaxt, double *ylmaxt,
                    double *ccneff, double *cicad, double *cmxsf, double *cqesf,
                    char *pgpath);
}

// [[Rcpp::export]]
Rcpp::List read_forage_spe(Rcpp::StringVector file_name) {

  Rcpp::List spe;
  
  std::string fname=Rcpp::as<std::string>(file_name[0]);
  int flen=fname.length();
  Rcpp::NumericVector FNPGL(4);
  Rcpp::NumericVector FNPGN(4);
  Rcpp::NumericVector LMXREF(1);
  Rcpp::NumericVector LNREF(1);
  Rcpp::NumericVector QEREF(1);
  Rcpp::NumericVector SLWREF(1);
  std::string typpgl(3,' ');
  std::string typpgn(3,' ');
  Rcpp::CharacterVector TYPPGL(1);
  Rcpp::CharacterVector TYPPGN(1);
  Rcpp::NumericVector XLMAXT(6);
  Rcpp::NumericVector YLMAXT(6);
  Rcpp::NumericVector CCNEFF(1);
  Rcpp::NumericVector CICAD(1);
  Rcpp::NumericVector CMXSF(1);
  Rcpp::NumericVector CQESF(1);
  std::string pgpath(2,' ');
  Rcpp::CharacterVector PGPATH(1);
  
  for_inp_spe_(&(fname[0]),&flen,&(FNPGL[0]),&(FNPGN[0]),&(LMXREF[0]),&(LNREF[0]),
               &(QEREF[0]),&(SLWREF[0]),&(typpgl[0]),&(typpgn[0]),
               &(XLMAXT[0]),&(YLMAXT[0]),
               &(CCNEFF[0]),&(CICAD[0]),&(CMXSF[0]),&(CQESF[0]),
               &(pgpath[0]));
  
  TYPPGL[0] = typpgl;
  TYPPGN[0] = typpgn;
  PGPATH[0] = pgpath;
  
  spe = Rcpp::List::create(Rcpp::_["FNPGL"]=FNPGL,
                           Rcpp::_["FNPGN"]=FNPGN,
                           Rcpp::_["LMXREF"]=LMXREF,
                           Rcpp::_["LNREF"]=LNREF,
                           Rcpp::_["QEREF"]=QEREF,
                           Rcpp::_["SLWREF"]=SLWREF,
                           Rcpp::_["TYPPGL"]=TYPPGL,
                           Rcpp::_["TYPPGN"]=TYPPGN,
                           Rcpp::_["XLMAXT"]=XLMAXT,
                           Rcpp::_["YLMAXT"]=YLMAXT,
                           Rcpp::_["CCNEFF"]=CCNEFF,
                           Rcpp::_["CICAD"]=CICAD,
                           Rcpp::_["CMXSF"]=CMXSF,
                           Rcpp::_["CQESF"]=CQESF,
                           Rcpp::_["PGPATH"]=PGPATH);
    
  return spe;
}
