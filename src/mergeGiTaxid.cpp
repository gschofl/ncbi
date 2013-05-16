#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <iterator>
#include <cstdlib>

using namespace Rcpp;

// [[Rcpp::export]]
void merge_gi_taxid(const std::string& nucfile,
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
		Rcerr << "cannott open input file \"" << protfile << "\"" << std::endl;
		exit(EXIT_FAILURE);
	}

	if (! PROT) {
		Rcerr << "cannot open output file \"" << outfile << "\"" << std::endl;
		exit(EXIT_FAILURE);
	}

	typedef std::istream_iterator<unsigned long> input_iterator;
	input_iterator end_of_stream = input_iterator();
	for (input_iterator n(NUCL), p(PROT); p != end_of_stream || n != end_of_stream; )
	{
		if ( p == end_of_stream || ( *n < *p && n != end_of_stream ) )
		{
			OUT << *n << ",";
			n++;
			OUT << *n << std::endl;
			n++;
		}
		else
		{
			OUT << *p << ",";
			p++;
			OUT << *p << std::endl;
			p++;
		}
	}
}
