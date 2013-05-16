#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <cstdlib>

using namespace Rcpp;

// [[Rcpp::export]]
void index_gi_taxid(const std::string& nucfile,
  					const std::string& protfile,
					const std::string& outfile)
{
    std::ifstream NUCL( nucfile.c_str() );
    std::ifstream PROT( protfile.c_str() );
    std::ofstream OUT( outfile.c_str(), std::ios::app);
	if (! NUCL) {
		Rcerr << "cannot open input file \"" << nucfile << "\"" << std::endl;
		exit(EXIT_FAILURE);
	}
	if (! PROT) {
		Rcerr << "cannot open input file \"" << protfile << "\"" << std::endl;
		exit(EXIT_FAILURE);
	}
	if (! PROT) {
		Rcerr << "cannot open output file \"" << outfile << "\"" << std::endl;
		exit(EXIT_FAILURE);
	}
    unsigned long last_geneid = 0;
    unsigned long geneid;
    unsigned long taxid;
    typedef std::istream_iterator<unsigned long> input_iterator;
    input_iterator end_of_stream = input_iterator();
    for (input_iterator n(NUCL), p(PROT); p != end_of_stream || n != end_of_stream; )
    {
    	if ( p == end_of_stream || ( *n < *p && n != end_of_stream ) ) {
    		geneid = *n;
    		n++;
    		taxid = *n;
    		n++;
    	} else {
    		geneid = *p;
    		p++;
    		taxid = *p;
    		p++;
    	}
    	for (unsigned long i = last_geneid + 1; i < geneid; i++) {
    		OUT << 0 << std::endl;
        }
        OUT << taxid << std::endl;
        last_geneid = geneid;
    }
}
