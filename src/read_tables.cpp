#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <boost/algorithm/string.hpp>

using namespace Rcpp;

bool getNextLine( std::istream& str, std::vector<std::string>& vec )
{
    std::vector<std::string>    result;
    std::string                 line;
    bool success = std::getline( str, line );
    //Rcout << ">" << line <<  "<" << std::endl;
    std::stringstream           lineStream(line);
    std::string                 cell;
    while (std::getline( lineStream, cell, '|' )) {
        boost::trim( cell );
        vec.push_back( cell );
        //Rcout << ">" << cell <<  "<" << std::endl;
    }
    return success;
}

// [[Rcpp::export]]
Rcpp::List readNodes( const std::string& filename )
{
    std::ifstream file(filename.c_str());
    std::vector<std::string>    tax_id;
    std::vector<std::string>    parent_id;
    std::vector<std::string>    rank;
    std::vector<std::string>    embl_code;
    std::vector<std::string>    division_id;
    std::vector<std::string>    linevec;
    
    while (getNextLine( file, linevec )) {
        tax_id.push_back( linevec.at(0) );
        parent_id.push_back( linevec.at(1) );
        rank.push_back( linevec.at(2) );
        embl_code.push_back( linevec.at(3) );
        division_id.push_back( linevec.at(4) );
        linevec.clear();
    }
    
    return Rcpp::List::create(_["tax_id"] = tax_id,
                              _["parent_id"] = parent_id,
                              _["rank"] = rank,
                              _["embl_code"] = embl_code,
                              _["division_id"] = division_id);
}

// [[Rcpp::export]]
Rcpp::List readNames( const std::string& filename )
{
    std::ifstream file(filename.c_str());
    std::vector<std::string>    tax_id;
    std::vector<std::string>    tax_name;
    std::vector<std::string>    unique_name;
    std::vector<std::string>    tax_class;
    std::vector<std::string>    linevec;
    
    while (getNextLine( file, linevec )) {
        tax_id.push_back( linevec.at(0) );
        tax_name.push_back( linevec.at(1) );
        unique_name.push_back( linevec.at(2) );
        tax_class.push_back( linevec.at(3) );
        linevec.clear();
    }
    
    return Rcpp::List::create(_["tax_id"] = tax_id,
                              _["tax_name"] = tax_name,
                              _["unique_name"] = unique_name,
                              _["class"] = tax_class);
}

// [[Rcpp::export]]
Rcpp::List readMerged( const std::string& filename )
{
    std::ifstream file(filename.c_str());
    std::vector<std::string>    old_taxid;
    std::vector<std::string>    new_taxid;
    std::vector<std::string>    linevec;

    while (getNextLine( file, linevec )) {
        old_taxid.push_back( linevec.at(0) );
        new_taxid.push_back( linevec.at(1) );
        linevec.clear();
    }
    
    return Rcpp::List::create(_["old_taxid"] = old_taxid,
                              _["new_taxid"] = new_taxid);
}


