#include <Rcpp.h>

extern "C"{
  void lmatch_(int* NLAYRI,double* DSI,double* VI,int* NLAYRO,
	       double* DSO,double* VS);
}

// [[Rcpp::export]]
Rcpp::NumericVector lmatch(Rcpp::NumericVector soil_data,
			   Rcpp::NumericVector depths_in,
			   Rcpp::NumericVector depths_out) {

  int nlayri=depths_in.size();
  int nlayro=depths_out.size();
  Rcpp::NumericVector data_out(nlayro);

  lmatch_(&nlayri,&(depths_in[0]),&(soil_data[0]),
	  &nlayro,&(depths_out[0]),&(data_out[0]));

  return data_out;
}
