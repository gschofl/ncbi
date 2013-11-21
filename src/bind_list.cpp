#include <Rcpp.h>

// Enable C++11 (requires Rcpp >= 0.10.3)
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

inline std::vector<std::string> bind_character(List& L, int column)
{
  std::vector<std::string> res;
  for (auto& l : L)
  {
    std::vector<std::string> vec = as<List>(l)[column];
    res.insert(end(res), begin(vec), end(vec));
  }
  return res;
}

inline std::vector<double> bind_numeric(List& L, int column)
{
  std::vector<double> res;
  for (auto& l : L)
  {
    std::vector<double> vec = as<List>(l)[column];
    res.insert(end(res), begin(vec), end(vec));
  }
  return res;
}

inline std::vector<int> bind_integer(List& L, int column)
{
  std::vector<int> res;
  for (auto& l : L)
  {
    std::vector<int> vec = as<List>(l)[column];
    res.insert(end(res), begin(vec), end(vec));
  }
  return res;
}

inline std::vector<bool> bind_logical(List& L, int column)
{
  std::vector<bool> res;
  for (auto& l : L)
  {
    std::vector<bool> vec = as<List>(l)[column];
    res.insert(end(res), begin(vec), end(vec));
  }
  return res;
}


// [[Rcpp::export]]
List bind_list( List& L, int n_col, std::vector<std::string> col_classes)
{
    // Hold output
    List res{n_col};
    // take column names from the first data.frame in the list
    std::vector<std::string> col_names = as<List>(L[0]).attr("names");
    // loop over columns
    for (int column = 0; column < n_col; ++column)
    {
        if (col_classes[column] == "character")
        {
            CharacterVector tmp = wrap(bind_character(L, column));
            res[column] = tmp;
        }
        else if (col_classes[column] == "numeric")
        {
            NumericVector tmp = wrap(bind_numeric(L, column));
            res[column] = tmp; 
        }
        else if (col_classes[column] == "integer" || col_classes[column] == "factor")
        {
            IntegerVector tmp = wrap(bind_integer(L, column));
            res[column] = tmp;          
        }
        else if (col_classes[column] == "logical")
        {
            LogicalVector tmp = wrap(bind_logical(L, column));
            res[column] = tmp;         
        }
        else
        {
            stop("One of the vectors is of an incompatible type");
        }
    }
    res.attr("names") = col_names;
    res.attr("class") = "data.frame";
    return res;
}

