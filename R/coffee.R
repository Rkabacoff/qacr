#' @title Coffee Quality Institute Database
#'
#' @description
#' Data scraped from the Coffee Quality Institute Database of
#' 1312 arabica coffee beans. Contains both data about the
#' production and taste of the beans. Scores are aggregated based
#' on multiple cups of coffee with many reviewers.
#'
#' @docType data
#' @keywords datasets
#' @name coffee
#' @usage coffee
#'
#' @format A data frame with 1311 rows and 28 variables.
#' The variables are as follows:
#' \describe{
#'    \item{country_of_origin}{Country of Origin of Beans}
#'    \item{company}{Company which made the coffee}
#'    \item{number_of_bags}{Number of bags harvested}
#'    \item{in_country_partner}{Partner of the Company in the Country of Origin}
#'    \item{grading_date}{Date the Coffee was reviewed}
#'    \item{owner_1}{Owner of the Company or the Company Name}
#'    \item{variety}{Variety of Coffee}
#'    \item{processing_method}{Method by Which the Beans were Processed}
#'    \item{aroma}{Reviewer's Score of Aroma on a 1-10 scale}
#'    \item{flavor}{Reviewer's Score of Flavor on a 1-10 scale}
#'    \item{aftertaste}{Reviewer's Score of Aftertaste on a 1-10 scale}
#'    \item{acidity}{Reviewer's Score of Acidity on a 1-10 scale}
#'    \item{body}{Reviewer's Score of Body on a 1-10 scale}
#'    \item{balance}{Reviewer's Score of Balance on a 1-10 scale}
#'    \item{uniformity}{Reviewer's Score of Uniformity on a 1-10 scale}
#'    \item{clean_cup}{Reviewer's Score of "Transparency of a cup" on a 1-10 scale}
#'    \item{sweetness}{Reviewer's Score of Sweetness on a 1-10 scale}
#'    \item{cupper_points}{Reviewer's Holistic Score of the cup on a 1-10 scale}
#'    \item{total_cup_points}{Sum of the Reviewer's Scores}
#'    \item{moisture}{Moisture percentage from Green Analysis}
#'    \item{category_one_defects}{Major defects in the beans}
#'    \item{color}{Color of the Greens}
#'    \item{category_two_defects}{Minor defects in the beans}
#'    \item{expiration}{Expiration of the certification of the bean}
#'    \item{unit_of_measurement}{Unit of Measurement for the Altitude of the farm}
#'    \item{altitude_mean_meters}{Altitude of the farm}
#' }
#'
#'@source The data is a subset scraped from the Coffee Quality Institute website
#' using the scraper developed by *jldbc*. Full data can be
#' found at \href{https://github.com/jldbc/coffee-quality-database}{https://github.com/jldbc/coffee-quality-database}.
#'
#'@examples
#'summary(coffee)
NULL
