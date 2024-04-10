#nest.grid.R

#' Function to downscale a regular grid in vector format (i.e. square polygons
#' as sf) into a new smaller resolution grid (square polygons sf). 
#'
#' @param ingrid input polygon sf representing a grid of regularly
#' spaced square polygons (eg. EEE ref grid)
#' @param res target resolution of nested grid in m. Must be a divisor of ingrid's
#' resolution (length of square's sides)
#'
#' @return a sf representing a grid of square polygons of res side length
#' @export
#'
#' @examples See nest.grid.lux.R
nest.grid<-function(ingrid,res=250,idcol=1){
  
  #get the 5 coords pairs for each input grid cell
  dfcoords<-data.frame(sf::st_coordinates(ingrid))
  #derive base resolution from Y difference of first square:
  input_r<-sf::st_bbox(ingrid[1,])$ymax-sf::st_bbox(ingrid[1,])$ymin
  if ((input_r %% res) >0){stop("Remainder of dividing input resolution by target resolution not null")}
  # split input into list so child output cell will know parent output cell
  splitted<-split(dfcoords,dfcoords$L2) #dfcoords$L2 identifies polygons
  #reuse input cells names with IN first to avoid numbers to start names and indicate it is input name
  names(splitted)<-paste0("IN",sf::st_drop_geometry(ingrid)[,idcol])
  
  #sq.polyg function gives the 5 coordinates pairs of the new (smaller) square polygons
  #given the requested resolution r and list of base polygons origin x and origin y
  sq.polyg<-function(x,y,base_r=input_r,r=res){
    f<-base_r/r #division factor (must be integer)
    xs<-seq(from=x, to=x+base_r-r, by=r)
    ys<-seq(from=y, to=y+base_r-r, by=r)
    xsys<-cbind(rep(xs,f),rep(ys,each=f))
    p<-cbind(xsys, #starting point
             xsys[,1],xsys[,2]+r,   #2nd point
             xsys[,1]+r,xsys[,2]+r, #3rd point
             xsys[,1]+r,xsys[,2],   #4th point
             xsys)                  #end point is start point
  }
  #Applies sq.polyg() to each element of list
  newsquares<-lapply(splitted,function(s){
    sq.polyg(x=s$X[1],y=s$Y[1])
    })
  
  #row2poly transforms a set of 5 coordinates in a row matrix into a
  # polygon geometry (crs will be added later)
  row2poly <- function(x) {
    m <- matrix(x, ncol = 2, byrow = TRUE)
    poly <- sf::st_polygon(list(m))
    return(poly)
  }
  
  #applies row2poly to newsquares and transform each row into
  # an sf object (st_sfc then st_sf) per previous feature (list of sf)
  sflist<-lapply(newsquares,function(a){
    polya<-apply(a, 1, function(x) {row2poly(x)})
    sf::st_sf(sf::st_sfc(polya))
  })
  
  #Bring up polygons from all list elements:
  fullsf<- do.call(rbind, sflist)
  sf::st_crs(fullsf)<-sf::st_crs(ingrid)
  
  return(fullsf)
}
