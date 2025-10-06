##' Create rotation matrix for an angle. Not needed I think. Took from Create_BC_Partition_New.Rmd
##'  <desc>
##'
##' @param a Angle of rotation
##' @return not-sure
##' @export
##' @author Andrew Edwards
rot = function(a = 318.5){
  matrix(c(cos(a),
           sin(a),
           -sin(a),
           cos(a)),
         2, 2)
}



##' Create a transition calculation, used in make_grid (could just put on there
##'  as I think only used once). Took from Create_BC_Partition_New.Rmd
##'  <desc>
##'
##' @param geo TODO
##' @param ang TODO
##' @param center TODO
##' @return TODO
##' @export
##' @author Andrew Edwards
tran = function(geo, ang, center){
  (geo - center) * rot(ang * pi / 180) + center
}
