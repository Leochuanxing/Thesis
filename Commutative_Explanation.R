library(ggplot2)

Shifted_norm <- function(x){
  dnorm(x-0.6)+0.6
}
shade1 <- function(x){
  if (x >=-0.5){
    y = Shifted_norm(x)
  }
  else{
   y = NA 
  }
  return(y)
}

shade2 <- function(x){
  if (x< -1.2 | x > -0.5){
    y = NA
  }
  else{
    y = Shifted_norm(x)
  }
  return(y)
}

clear_shade <- function(x){
  y = 0.6
}

shade3 <- function(x){
  if (x < -0.5){
    y = NA
  }
  else{
    y = dnorm(x)
  }
}

shade4 <- function(x){
  if (x< -1.2|x > -0.5){
    y = NA
  }
  else{
    y = dnorm(x)
  }
  return(y)
}

x=seq(-3, 4, by = 0.01)
y_shade1 <- sapply(x, shade1)
y_shade2 <- sapply(x, shade2)
y_shade3 <- sapply(x, shade3)
y_shade4 <- sapply(x, shade4)

graph <- ggplot(data.frame(x=seq(-3, 4, by = 0.01)), aes(x = x)) +
          stat_function(fun = Shifted_norm, size = 1)+
          stat_function(fun = dnorm, args = list(0, 1), size = 1)+
          geom_segment(aes(x=-1.2, y=-0.1, xend = -1.2, yend = 1), color = 'red')+
          geom_segment(aes(x=-0.5, y=-0.1, xend = -0.5, yend = 1), color = 'blue')+
          geom_hline(yintercept = 0.6, size = 1)+
          geom_hline(yintercept = 0, size = 1)+

          geom_ribbon(aes(ymin=0.6, ymax=y_shade1), fill = 'blue', alpha = 0.5)+
          geom_ribbon(aes(ymin=0.6, ymax=y_shade2), fill = 'gray0', alpha = 0.5)+
          geom_ribbon(aes(ymin=0, ymax=y_shade3), fill = 'brown', alpha = 0.5)+
          geom_ribbon(aes(ymin=0, ymax=y_shade4), fill = 'darkorange', alpha = 0.5)+

          theme(axis.title = element_blank(), axis.ticks = element_blank(),
               axis.text = element_blank(), panel.grid.major = element_blank(),
               panel.background = element_blank())+
  
          annotate('text', x= c(-1.2, -0.5), y=c(1.05, 1.05), label=c('R', 'I'))+
          annotate('text', x= 2, y= 0.9, label=' T and N')+
          annotate('text', x= 1.4, y= 0.3, label=' T and TR')+
          annotate("rect", xmin=-2.5, xmax=-2, ymin=1.2, ymax=1.25, fill = 'blue', alpha = 0.5)+
          annotate('text', x = -1.2, y = 1.225, label = 'AUC(I,T,N)')+
          annotate("rect", xmin=0, xmax=.5, ymin=1.2, ymax=1.25, fill = 'gray0', alpha = 0.5)+
          annotate('text', x = 2, y = 1.225, label = 'AUC(R,T,N) - AUC(I,T,N)')+
  
          annotate("rect", xmin=-2.5, xmax=-2, ymin=-0.2, ymax=-0.15, fill = 'brown', alpha = 0.5)+
          annotate('text', x = -1.2, y = -0.175, label = 'AUC(I,T,TR)')+
          annotate("rect", xmin=0, xmax=.5, ymin=-0.2, ymax=-0.15, fill = 'darkorange', alpha = 0.5)+
          annotate('text', x = 2, y = -0.175, label = 'AUC(R,T,TR) - AUC(I,T,TR)')+
  
          ylim(-0.3, 1.3)

graph

















