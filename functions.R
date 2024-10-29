f.test2 = function(sigma1, sigma2, n1, n2, alpha, cola){
  
  
  f.est = sigma1 / sigma2
  
  if(cola == "izq"){
    F.alpha = qf(alpha, n1 - 1, n2 - 1)
    p.value = pf(f.estm, n1 - 1,  n2 - 1)
    
    if (f.est <= F.alpha){ 
      cat ("Rechaza h0, con un p-value de:", p-value, "y un ratio de varianza de", f.ast)
    } else {
      cat ("No serechaza h0, con un p-value de:", p-value, "y un ratio de varianza de", f.ast) 
    }
    
  } else if(cola == "der") {
    
    F.alpha = qf(1 - alpha, n1 - 1, n2 - 1)
    p.value = 1 -pf(f.estm, n1 - 1,  n2 - 1)
    
    if (f.est >= F.alpha){ 
      cat ("Rechaza h0, con un p-value de:", p-value, "y un ratio de varianza de", f.ast)
    } else {
      cat ("No rechaza h0, con un p-value de:", p-value, "y un ratio de varianza de", f.ast) 
    }
    
  } else if (cola == "bilateral"){
    
    F.alpha1  = qf(alpha / 2, n1 - 1, n2 - 1)
    F.alpha2  = qf(1 - alpha / 2, n1 - 1, n2 - 1)
    p.value = 2 * min(pf(f.est, n1 - 1, n2 - 1), 1 - pf(f.est, n1 - 1, n2 - 1))
    if (f.est <= F.alpha1 || f.est >= F.alpha2 ){ 
      cat ("Rechaza h0, con un p-value de:", p.value, "y un ratio de varianza de", f.ast)
    } else {
      cat ("No serechaza h0, con un p-value de:", p.value, "y un ratio de varianza de", f.ast) 
    }
    
  }
  
}