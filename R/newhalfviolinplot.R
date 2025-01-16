#' new half violin plot with 2 conditions
#'
#' @param df your own imported data frame
#' @param x_var one of column names and it should be a character
#' @param y_var one of column names and it should be a character
#' @param fill_var one of column names containing only 2 values and it should be a character
#' @param condition1 value 1 in fill_var and it should be a character
#' @param condition2 value 2 in fill_var and it should be a character
#' @param color1 the color you like and it should be a character
#' @param color2 the color you like and it should be a character
#' @param topic the main content of your plot and it should be a character
#' @param number adjust the position
#'
#' @returns
#' @export
#'
#' @examples df<-read.csv("testdata.csv")
#' myplot(df,"celltype_final","ATP5MC3","condition","CTRL","EXP","pink","blue","myplot",0.05)


myplot<-function(df,x_var,y_var,fill_var,condition1,condition2,color1,color2,topic,number){
  library(ggplot2)
  library(gghalves)
  library(ggsignif)
p<-ggplot(df,aes_string(x=x_var,y=y_var,fill=fill_var))+
    geom_half_violin(side = "l", data = subset(df, get(fill_var) == condition1), alpha = 0.5,position=position_nudge(x=-number),color=NA)+
    geom_half_violin(side = "r", data = subset(df, get(fill_var) == condition2), alpha = 0.5,position=position_nudge(x=number),color=NA)+
    scale_fill_manual(values=setNames(c(color1, color2), c(condition1, condition2)))+
    theme_minimal()+
    labs(title=topic,x=x_var,y=y_var)+
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),  # 移除主网格线
          panel.grid.minor = element_blank()) + # 移除次网格线
    stat_summary(data = subset(df, get(fill_var) == condition1),
                 fun.data = mean_cl_normal,
                 geom = "errorbar",
                 width = 0.1,
                 position = position_nudge(x = -number),
                 linewidth = 0.7) +
    stat_summary(data = subset(df, get(fill_var) == condition1),
                 fun = mean,
                 geom = "point",
                 shape = 16,
                 size = 2,
                 color = "black",
                 position = position_nudge(x = -number)) +

    # 右侧均值和误差线
    stat_summary(data = subset(df, get(fill_var) == condition2),
                 fun.data = mean_cl_normal,
                 geom = "errorbar",
                 width = 0.1,
                 position = position_nudge(x = number),
                 linewidth = 0.7) +
    stat_summary(data = subset(df, get(fill_var) == condition2),
                 fun = mean,
                 geom = "point",
                 shape = 16,
                 size = 2,
                 color = "black",
                 position = position_nudge(x = number))
    return(p)
}
