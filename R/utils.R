##' Create rotation matrix for an angle. Not needed I think. Took from Create_BC_Partition_New.Rmd
##'  <desc>
##'
##' @param a Angle of rotation
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @ontrun{
##' @
##' @}
rot = function(a = 318.5){
  matrix(c(cos(a),
           sin(a),
           -sin(a),
           cos(a)),
         2, 2)
}

##' Create a transition calculation, but not now needed I think. Took from Create_BC_Partition_New.Rmd
##'  <desc>
##'
##' @param geo
##' @param ang
##' @param center
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @ontrun{
##' @
##' @}
tran = function(geo, ang, center){
  (geo - center) * rot(ang * pi / 180) + center
}
