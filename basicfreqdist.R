#' Creates a frequency distribution table and automated ogives plot
#' @param lower_Class_limit,upper_Class_limit,freq numeric vector
#' @examples
#' seq(0,40,by=10)
#' seq(10,50,by=10)
#' c(34,21,54,23,12)
#' @importFrom graphics lines points
#' @export
basicfreqdist<-function(lower_Class_limit,upper_Class_limit,freq){
  classwidth<- function(lower_Class_limit,upper_Class_limit){
    h=upper_Class_limit[1]-lower_Class_limit[1]
    return(h)
  }
  classmarks<- function(lower_Class_limit,upper_Class_limit){
    x=(lower_Class_limit+upper_Class_limit)/2
    return(x)
  }
  MorethanCF <- function(freq) {MoreCF={}
  for (i in 1:length(freq)){MoreCF[i]<-sum(freq[i:length(freq)]);MoreCF}
  return(MoreCF)
  }
  LessthanCF <- function(freq) {LessCF={}
  for (j in 1:length(freq)){LessCF[j]<-sum(freq[1:j]);LessCF}
  return(LessCF)
  }
  h=classwidth(lower_Class_limit,upper_Class_limit)
  x=classmarks(lower_Class_limit,upper_Class_limit)
  MTCF=MorethanCF(freq)
  LTCF=LessthanCF(freq)
  table=data.frame(LL=lower_Class_limit,UL=upper_Class_limit,classmark=x,freq=freq,morecf=MTCF,lesscf=LTCF)
  print(table)
  plot(main = "Less than and More Than Ogive Plots",upper_Class_limit,LTCF,col=4,type='l',pch=2,
       xlab='Class intervals',ylab='Cummulative sum',xlim=range(min(lower_Class_limit),max(upper_Class_limit)),
       ylim=range(0,max(LTCF)))
  points(upper_Class_limit,LTCF,col=4,lwd=5,pch=2)
  lines(lower_Class_limit,MTCF,pch=3,lwd=3,col=2)
  points(lower_Class_limit,MTCF,pch=3,lwd=5,col=2)
  sprintf("CLASS WIDTH= %d",h)
}
