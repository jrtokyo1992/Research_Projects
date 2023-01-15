// two-step model with edogeneous choice
functions {
   // cdf pr(err1<z1, err2<z2) when err1 and err2 are normal distribution with rho.
   real binormal_cdf(real z1, real z2, real rho) {
    if (z1 != 0 || z2 != 0) {
      real denom = fabs(rho) < 1.0 ? sqrt((1 + rho) * (1 - rho)) : not_a_number();
      real a1 = (z2 / z1 - rho) / denom;
      real a2 = (z1 / z2 - rho) / denom;
      real product = z1 * z2;
      real delta = product < 0 || (product == 0 && (z1 + z2) < 0);
      return 0.5 * (Phi(z1) + Phi(z2) - delta) - owens_t(z1, a1) - owens_t(z2, a2);
    }
    return 0.25 + asin(rho) / (2 * pi());
  }
  
  real biprobit_lpdf(row_vector Y, real xb1, real xb2, real rho) {
    // compute the L(y1, y2 | xb1, xb2)
    // we know that y1 = 1(xb1+err1>0) and y2 = 1(xb2+err2>0)
    // err1 and err2 are jointly normal distribution with rho.
    real q1;
    real q2;
    real rho1;
    
    // from Greene's econometrics book
    q1 = 2*Y[1] - 1.0;
    q2 = 2*Y[2] - 1.0;
    
    rho1 = q1*q2*rho;
    return log(binormal_cdf(q1*xb1, q2*xb2, rho1));
  }
}

data {
  int<lower=0> N;
  int k_1;
  int k_2;
  int k_3;
  matrix[N,3] y;
  matrix[N,k_1] x_1; //y[1]= 1(x_1*beta_1 + err1>0)
  matrix[N,k_2] x_2; // y[2] = 1(x_2*beta_2 + err2>0)
  matrix[N,k_3] x_3; // y[3] = x_3*beta_3 + err3>0
  
}


parameters {
  vector[k_1] beta_1;
  vector[k_2] beta_2;
  vector[k_3] beta_3;
  real rho_1_2; 
  real rho_1_3;
  real rho_2_3;
 // real<lower = 0> sigma_1;
 // real<lower = 0> sigma_2;
 // real<lower = 0> sigma_3;
}



model {
  // y1 = 1(x_1* beta_1 + err1>0)
  //  y2=1(x_2*beta_2+ err2>0)
  //  y3= x_3*beta_3 + err3
  // if y2 = 1, the liklihood is l(y1,y2,y3) = pr(y1,y2|y3)pr(y3) = pr(1(x_1*beta_1+err1>0), 1(x_2*beta_2 + err2>0)|err3 = y-x_3*beta_3)
  // in this case, need to know the joint distribution of err1 and err2 given err3 = y - x_3*beta_3
  // if y2 = 0, the likelihood is l(y1,y2) = pr(y1,y2) = pr(1(x_1*beta_1+err1>0), 1(x_2*beta_2 + err2>0))
  for (i in 1:N){
    if (y[i,2] ==0){
    target+= biprobit_lpdf(y[i,1:2] |x_1[i] * beta_1, x_2[i] * beta_2, rho_1_2);
    }
    else {
    target+= biprobit_lpdf(y[i,1:2] |x_1[i] * beta_1 + rho_1_3*(y[i,3] -x_3[i] * beta_3 - 0 ) , x_2[i] * beta_2 + rho_2_3*(y[i,3] -x_3[i] * beta_3 - 0 ), rho_1_2) + normal_lpdf(y[i,3] | x_3[i] * beta_3, 1);
    }
  }
}


