#' Lookup IMD Using Postcode
#'
#' Function for looking up a column of postcodes and returning information about the
#' indices of deprivation about that area
#'
#' @param postcodes This is the dataframe containing all data
#' @param pcdcolumn This isd the name of the column that contains the postcode data
#'
#' @import SPARQL
#' @import reshape2
#'
imd_lookup <- function(postcodes,pcdcolumn) {

  # Set the variables for the sparql endpoint and the two halves of the SPARQL query

  sparql_endpoint <- "http://opendatacommunities.org/sparql"

  sparql_part1 <- "PREFIX dcterms: <http://purl.org/dc/terms/>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX qb: <http://purl.org/linked-data/cube#>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

  SELECT ?postcodeuri ?postcode ?postcodestatus ?lsoa ?lsoaname ?domain ?rank ?decile ?score WHERE {
  {
  SELECT (?lsoa_uri_inner as ?lsoa_uri) ?postcode ?postcodeuri ?postcodestatus ?lsoa ?lsoaname ?position WHERE {
  VALUES (?position ?postcodeuri) {"

  sparql_part2 <- "}
  GRAPH <http://opendatacommunities.org/graph/geography/uk-postcodes> {
  ?postcodeuri <http://opendatacommunities.org/def/ontology/admingeo/lsoa> ?lsoa_uri_inner ;
  <http://www.w3.org/2000/01/rdf-schema#label> ?postcode ;
  <http://www.w3.org/2004/02/skos/core#note> ?postcodestatus .
  }
  GRAPH <http://opendatacommunities.org/graph/ons-geography-administration> {
  ?lsoa_uri_inner <http://www.w3.org/2004/02/skos/core#notation> ?lsoa ;
  rdfs:label ?lsoaname .
  }
  }
  }
  GRAPH <http://opendatacommunities.org/graph/societal-wellbeing/imd/indices> {
  ?rank_obs <http://opendatacommunities.org/def/ontology/geography/refArea> ?lsoa_uri ;
  <http://opendatacommunities.org/def/ontology/communities/societal_wellbeing/imd/indices> ?domain_uri ;
  <http://opendatacommunities.org/def/ontology/communities/societal_wellbeing/imd/rankObs> ?rank .
  ?decile_obs <http://opendatacommunities.org/def/ontology/geography/refArea> ?lsoa_uri ;
  <http://opendatacommunities.org/def/ontology/communities/societal_wellbeing/imd/indices> ?domain_uri ;
  <http://opendatacommunities.org/def/ontology/communities/societal_wellbeing/imd/decObs> ?decile .
  ?score_obs <http://opendatacommunities.org/def/ontology/geography/refArea> ?lsoa_uri ;
  <http://opendatacommunities.org/def/ontology/communities/societal_wellbeing/imd/indices> ?domain_uri ;
  <http://opendatacommunities.org/def/ontology/communities/societal_wellbeing/imd/scoreObs> ?score .
  }
  ?domain_uri rdfs:label ?domain .
} ORDER BY(?position)"


  whole_values_statement <- ""
  postcodes$strippedpcd <- gsub('\\s+', '', postcodes[[pcdcolumn]])
  for (row in 1:nrow(postcodes)) {
    cur_values_statement <- paste(whole_values_statement,"( ",row," <http://opendatacommunities.org/id/geography/postcode/postcodeunit/",as.character(postcodes$strippedpcd)[row],"> )\n",sep="")
    whole_values_statement <- cur_values_statement

  }
  query <- paste(sparql_part1,whole_values_statement,sparql_part2,sep="")
  df <- SPARQL::SPARQL(sparql_endpoint,query)
  results <- df$results
  results_dcasted <- reshape2::dcast(results, postcode + lsoa ~ domain, value.var = "rank")
  results_dcasted$postcode <- gsub('\\s+', '', results_dcasted$postcode)
  merged <- merge(x=postcodes, y = results_dcasted, by.x="strippedpcd", by.y = "postcode", all.x = TRUE)
}

# End -------------------------------------------------------------------------------------------------------
