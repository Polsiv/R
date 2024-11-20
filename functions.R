f.test2 = function(sigma1, sigma2, n1, n2, alpha, cola){
  
  f.est = sigma1 / sigma2
  
  if(cola == "izq"){
    F.alpha = qf(alpha, n1 - 1, n2 - 1)
    p.value = pf(f.estm, n1 - 1,  n2 - 1)
    
    if (f.est <= F.alpha){ 
      cat ("Rechaza h0, con un p-value de:", p-value, "y un ratio de varianza de", f.est)
    } else {
      cat ("No rechaza h0, con un p-value de:", p-value, "y un ratio de varianza de", f.est) 
    }
    
  } else if(cola == "der") {
    
    F.alpha = qf(1 - alpha, n1 - 1, n2 - 1)
    p.value = 1 -pf(f.estm, n1 - 1,  n2 - 1)
    
    if (f.est >= F.alpha){ 
      cat ("Rechaza h0, con un p-value de:", p-value, "y un ratio de varianza de", f.est)
    } else {
      cat ("No rechaza h0, con un p-value de:", p-value, "y un ratio de varianza de", f.est) 
    }
    
  } else if (cola == "bilateral"){
    
    F.alpha1  = qf(alpha / 2, n1 - 1, n2 - 1)
    F.alpha2  = qf(1 - alpha / 2, n1 - 1, n2 - 1)
    p.value = 2 * min(pf(f.est, n1 - 1, n2 - 1), 1 - pf(f.est, n1 - 1, n2 - 1))
    if (f.est <= F.alpha1 || f.est >= F.alpha2 ){ 
      cat ("Rechaza h0, con un p-value de:", p.value, "y un ratio de varianza de", f.est)
    } else {
      cat ("No rechaza h0, con un p-value de:", p.value, "y un ratio de varianza de", f.est) 
    }
  }
}


two.sample.t.test = function(n1, n2, media1, media2, s1, s2, correccion, cola, alpha ){
  
  t0 = 0
  parametro = 0 
  
  #correcion de Welch
  
  if (correccion == TRUE){
    
    t0 = (media1 - media2) / sqrt((s1^2 / n1) + (s2^2 / n2))
    v = ((s1^2 / n1) + (s2^2 / n2))^2 / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
    parametro = v
    
  } else {
    
    varianza.ponderada = sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
    t0 = (media1 - media2) / (varianza.ponderada * sqrt((1 / n1) + (1 / n2)))
    parametro = n1 + n2 - 2
    
  }
  
  #decision
  
  if(cola == "izquierda"){
    
    p.value = pt(t0, parametro)
    
    if(t0 <= qt(alpha, parametro)){
      cat("Rechaza H0, con un P-value de:", p.value, "<", alpha)
    } else {
      cat("No rechaza H0, con un P-value de:", p.value, ">", alpha)
    }
    
  } else if (cola == "derecha"){
    
    p.value = 1 - pt(t0, df)
    
    if(t0 >= qt(1 - alpha, parametro)){
      cat("Rechaza H0, con un P-value de:", p.value, "<", alpha)
    } else {
      cat("No rechaza H0, con un P-value de:", p.value, ">", alpha)
    }
    
  } else if (cola == "bilateral"){
    
    p.value = 2 * (1 - pt(abs(t0), parametro))
    
    if(t0 >= qt(1 - alpha / 2, parametro)){
      cat("Rechaza H0, con un P-value de:", p.value, "<", alpha)
    } else {
      cat("No rechaza H0, con un P-value de:", p.value, ">", alpha)
    }
  }
}


