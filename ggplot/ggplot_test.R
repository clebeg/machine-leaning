library(ggplot2)
######ggplot2的基本概念########
#1、数据（Data）和映射（Mapping）
#2、标度（Scale）
#3、几何对象（Geometric）
#4、统计变换（Statistics）
#5、坐标系统（Coordinate）
#6、图层（Layer）
#7、分面（Facet）

####### sample1 #######
#"Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"   
#数据必须是数据框，color参数必须是因子行
p <- ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species))
p + geom_point()

####### sample2 #######
p <- ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species))
#加上图形显示，即上述每个点用什么显示， 再加上统计变化，统计上的平滑
p + geom_point() + stat_smooth()
