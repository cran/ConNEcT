#' Affective family interaction dataset
#'
#' These data was collected during a nine-minutes problem-solving family
#' interaction between two parents and their adolescent son or daughter
#' During this interaction, the presence and absence of expressions of
#' ‘anger’, ‘dysphoric’ feelings and ‘happiness‘ were coded for
#' each family member in an event-basis way (i.e., noting when a certain
#' behavior starts and when it stops). The codes were subsequently restructured
#' into second-to-second interval data, resulting in a 540 seconds by nine variables
#' binary dataset.
#'
#'
#' @name FamilyData
#' @author Lisa Sheeber \email{lsheeber@@ori.org}
#' @format A data frame with 540 rows and 9 variables:
#' \describe{
#'   \item{moanger}{mother expressing anger}
#'   \item{faanger}{father expressing anger}
#'   \item{adanger}{adolescent expressing anger}
#'   \item{modysph}{mother expressing dysphoric feelings}
#'   \item{fadysph}{father expressing dysphoric feelings}
#'   \item{addysph}{adolescent expressing dysphoric feelings}
#'   \item{mohappy}{mother expressing happy feelings}
#'   \item{fahappy}{father expressing happy feelings}
#'   \item{adhappy}{adolescent expressing happy feelings}
#'
#' }
#' @references  {Sheeber, L. B., Kuppens, P., Shortt, J. W., Katz, L. F., Davis, B., & Allen, N. B. (2012). Depression is associated with the escalation of adolescents’ dysphoric behavior during interactions with parents. Emotion, 12(5), 913–918. https://doi.org/10.1037/a0025784}
#' @references  {Allen, N. B., Kuppens, P., & Sheeber, L. B. (2012). Heart rate responses to parental behavior in depressed adolescents. Biological Psychology, 90(1), 80–87. https://doi.org/10.1016/j.biopsycho.2012.02.013}
#' @references  {Bodner, N., Kuppens, P., Allen, N. B., Sheeber, L. B., & Ceulemans, E. (2018). Affective family interactions and their associations with adolescent depression: A dynamic network approach. Development and Psychopathology, 30(4), 1459–1473. https://doi.org/10.1017/S0954579417001699}

"FamilyData"
