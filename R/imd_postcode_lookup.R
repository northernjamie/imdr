#' Lookup IMD Using Postcode
#'
#' Function for looking up a column of postcodes and returning information about the
#' indices of deprivation about that area.
#'
#' Uses the SPARQL endpoint at Open Data Communities - The Ministry of Housing, Communities and Local Government in the UK.
#' Coverage - England only.
#' @title Gets English indices of deprivation ranks by postcode
#'
#' @description This package takes a dataframe containing a column of postcodes (and other information) and uses it to look up IMD ranks for that area
#'
#'
#' @param postcodes This is the dataframe containing all data
#' @param pcdcolumn This is the index of the column that contains the postcode data (1 being the 1st column in the daataframe)
#'
#' @import httr
#' @import reshape2
#'
#' @return returns a dataframe with the original data with extra columns containing deprivation ranks
#'
#' @examples new_dataframe <- imd_lookup(dataToBeMatched,3) [3 is the index of the column with the postcode in]
#'
#' @export imd_lookup
#'
imd_lookup <- function(postcodes,pcdcolumn) {

  # Set the variables for the encoded sparql endpoint and the two halves of the encoded SPARQL query

  sparql_endpoint <- "http://opendatacommunities.org/sparql?"

  sparql_part1 <- "query=PREFIX%20dcterms%3A%20%3Chttp%3A%2F%2Fpurl.org%2Fdc%2Fterms%2F%3E%0APREFIX%20owl%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2002%2F07%2Fowl%23%3E%0APREFIX%20qb%3A%20%3Chttp%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23%3E%0APREFIX%20rdf%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F1999%2F02%2F22-rdf-syntax-ns%23%3E%0APREFIX%20rdfs%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%3E%0APREFIX%20skos%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2004%2F02%2Fskos%2Fcore%23%3E%0APREFIX%20xsd%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2001%2FXMLSchema%23%3E%0A%0ASELECT%20%3Fpostcodeuri%20%3Fpostcode%20%3Fpostcodestatus%20%3Flsoa%20%3Flsoaname%20%3Fdomain%20%3Frank%20%3Fdecile%20%3Fscore%20WHERE%20%7B%0A%20%20%7B%0A%20%20%20%20SELECT%20(%3Flsoa_uri_inner%20as%20%3Flsoa_uri)%20%3Fpostcode%20%3Fpostcodeuri%20%3Fpostcodestatus%20%3Flsoa%20%3Flsoaname%20%3Fposition%20WHERE%20%7B%0A%20%20%20%20%20%20VALUES%20(%3Fposition%20%3Fpostcodeuri)%20%7B"
  sparql_part2 <- "%7DGRAPH%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fgraph%2Fgeography%2Fuk-postcodes%3E%20%7B%0A%20%20%20%20%20%20%20%20%3Fpostcodeuri%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fdef%2Fontology%2Fadmingeo%2Flsoa%3E%20%3Flsoa_uri_inner%20%3B%0A%20%20%20%20%20%20%20%20%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23label%3E%20%3Fpostcode%20%3B%0A%20%20%20%20%20%20%20%20%3Chttp%3A%2F%2Fwww.w3.org%2F2004%2F02%2Fskos%2Fcore%23note%3E%20%3Fpostcodestatus%20.%0A%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20GRAPH%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fgraph%2Fons-geography-administration%3E%20%7B%0A%20%20%20%20%20%20%20%20%3Flsoa_uri_inner%20%3Chttp%3A%2F%2Fwww.w3.org%2F2004%2F02%2Fskos%2Fcore%23notation%3E%20%3Flsoa%20%3B%0A%20%20%20%20%20%20%20%20rdfs%3Alabel%20%3Flsoaname%20.%0A%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%7D%0A%20%20%7D%0A%20%20GRAPH%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fgraph%2Fsocietal-wellbeing%2Fimd%2Findices%3E%20%7B%0A%20%20%20%20%3Frank_obs%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fdef%2Fontology%2Fgeography%2FrefArea%3E%20%3Flsoa_uri%20%3B%0A%20%20%20%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fdef%2Fontology%2Fcommunities%2Fsocietal_wellbeing%2Fimd%2Findices%3E%20%3Fdomain_uri%20%3B%0A%20%20%20%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fdef%2Fontology%2Fcommunities%2Fsocietal_wellbeing%2Fimd%2FrankObs%3E%20%3Frank%20.%0A%20%20%20%20%3Fdecile_obs%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fdef%2Fontology%2Fgeography%2FrefArea%3E%20%3Flsoa_uri%20%3B%0A%20%20%20%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fdef%2Fontology%2Fcommunities%2Fsocietal_wellbeing%2Fimd%2Findices%3E%20%3Fdomain_uri%20%3B%0A%20%20%20%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fdef%2Fontology%2Fcommunities%2Fsocietal_wellbeing%2Fimd%2FdecObs%3E%20%3Fdecile%20.%0A%20%20%20%20%3Fscore_obs%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fdef%2Fontology%2Fgeography%2FrefArea%3E%20%3Flsoa_uri%20%3B%0A%20%20%20%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fdef%2Fontology%2Fcommunities%2Fsocietal_wellbeing%2Fimd%2Findices%3E%20%3Fdomain_uri%20%3B%0A%20%20%20%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fdef%2Fontology%2Fcommunities%2Fsocietal_wellbeing%2Fimd%2FscoreObs%3E%20%3Fscore%20.%0A%20%20%7D%0A%20%20%3Fdomain_uri%20rdfs%3Alabel%20%3Fdomain%20.%0A%20%20%7D%20ORDER%20BY(%3Fposition)"

  whole_values_statement <- ""
  postcodes$strippedpcd <- gsub('\\s+', '', postcodes[[pcdcolumn]])
  for (row in 1:nrow(postcodes)) {
    cur_values_statement <- paste(whole_values_statement,"(",row,"%20%3Chttp%3A%2F%2Fopendatacommunities.org%2Fid%2Fgeography%2Fpostcode%2Fpostcodeunit%2F",as.character(postcodes$strippedpcd)[row],"%3E)%0A",sep="")
    whole_values_statement <- cur_values_statement

  }
  query <- paste(sparql_part1,whole_values_statement,sparql_part2,sep="")

  results <- POST(url = sparql_endpoint,
             add_headers(Accept = "text/csv"),
             body = query)

  results <- content(results,"parsed")

  results_dcasted <- reshape2::dcast(results, postcode + lsoa ~ domain, value.var = "rank", fun.aggregate = first, na.rm = TRUE)
  results_dcasted$postcode <- gsub('\\s+', '', results_dcasted$postcode)
  merged <- merge(x=postcodes, y = results_dcasted, by.x="strippedpcd", by.y = "postcode", all.x = TRUE)
  return(merged)
}

# End -------------------------------------------------------------------------------------------------------
