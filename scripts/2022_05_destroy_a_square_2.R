library(data.table)
library(ggplot2)

create_square<-function(x0,y0,a,id)
{
    data.table(
        x=c(x0,x0+a,x0+a,x0),
        y=c(y0,y0,y0+a,y0+a),
        id=id
    )
}
create_circle<-function(x0,y0,r,id)
{
    t<-seq(from=0,to=2*pi,length.out = 100)
    data.table(
        x=r*cos(t)+x0,
        y=r*sin(t)+y0,
        id=id
    )
}
dataC<-create_circle(x0 = 0,y0 = 0,r = 1,id = )
dataS<-rbindlist(
    l=lapply(
        X=1,
        FUN=function(i)
        {
            create_square(x0 = 0,y0 = 0,a = 1,id = i)
        }
    )
)

ggplot()+
    geom_polygon(data = dataS,mapping = aes(x=x,y=y,group=id))+
    theme_void()+
    theme(aspect.ratio = 1)
