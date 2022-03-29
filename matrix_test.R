library(ggplot2)
library(sp)

punkt<-function(px,py,qx,qy,rx,ry)
{
  return(sign(qx*ry+rx*py+px*qy-py*qx-qy*rx-ry*px))
}

punkt(0,0,0,1,1,0)

###################################################################################


matrix_test<-function(matrix,px,py,qx,qy)
{
  result=qx*matrix[,2] +matrix[,1]*py + px*qy -qx*py -qy*matrix[,1] -matrix[,2]*px
  
  return(sign(result))
}

##########################################################################
#WERSJA Z DATA FRAME
dane<-read.csv("result2.csv",sep=',')

sign_result<-matrix_test(dane[,1:2],15,38.5,15.60208,38.78542)
sign_result


result_plot <- data.frame(dane[,1], dane[,2], sign_result)

ggplot(result_plot, aes(result_plot[,1], result_plot[,2],color=factor(result_plot[,3])))+
  geom_point()+xlab("x")+ylab("y")
##############################################################################
#WERSJA Z OBIEKTEM SPATIAL POINTS
matrix_test_sp<-function(matrix,px,py,qx,qy)
{
  result=qx*matrix[,2] +matrix[,1]*py + px*qy -qx*py -qy*matrix[,1] -matrix[,2]*px
  
  return(sign(result))
}

sp_points=SpatialPoints(coords=dane[,1:2])

sp_sign_result=matrix_test_sp(sp_points@coords[,1:2],15,38.5,15.60208,38.78542)

sp_sign_result

##################################
#WERSJA Z OBIEKTEM SPATIAL POINTS DATA FRAME
sp_sign_result_df=SpatialPointsDataFrame(coords=dane[,1:2],data=data.frame(dane[,3]))










