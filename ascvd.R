
ascvdf= function(age,numTC,hdl,numBP,smokc,diab,rxbp){
  est = ifelse(rxbp==0,1.957,2.019)
  cal=-29.799*log(age)+4.884*log(age)*log(age)+13.540*log(numTC)-3.114*log(age)*log(numTC)-13.578*log(hdl)+3.149*log(age)*log(hdl)+
    est*log(numBP)+7.574*smokc-1.665*log(age)*smokc+0.661*diab
  
  
  ASCVD<-round(100*(1-(0.9665^exp(cal+29.18))),2)
  ASCVD}


ascvdm = function(age,numTC,hdl,numBP,smokc,diab,rxbp){
  est = ifelse(rxbp==0,1.764,1.797)
  cal=12.344*log(age)+11.853*log(numTC)-2.664*log(age)*log(numTC)-7.990*log(hdl)+1.769*log(age)*log(hdl)+
    est*log(numBP)+7.837*smokc-1.795*log(age)*smokc+0.658*diab
  
  ASCVD<-round(100*(1-(0.9144^exp(cal-61.18))),2)
  ASCVD
}


