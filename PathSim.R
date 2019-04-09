PathSim <- function(P) {
  r <- dim(P)[1]
  c <- dim(P)[2]
  output <- matrix(nrow = r, ncol = c)
  for (i in 1:r) {
    for (j in 1:c) {
      if (P[i,i]+P[j,j] != 0) {
        output[i,j] <- 2*P[i,j]/(P[i,i]+P[j,j])
      }
      else {
        output[i,j] <- 0
      }
    }
  } 
  return (output)
}


PathCal <- function(mat1, mat2, mat3, mat4, mat5, m_p) {
  drug_num <- dim(mat1)[1]
  output <- diag(drug_num)
  so <- nchar(m_p)
  for (i in 1:so) {
    if (substr(m_p,i,i)=='1') {
      output <- output %*% mat1
    }
    else if (substr(m_p,i,i)=='2') {
      output <- output %*% mat2
    }
    else if (substr(m_p,i,i)=='3') {
      if (dim(output)[2]==dim(mat3)[1]) {
        output <- output %*% mat3
      }
      else {
        output <- output %*% t(mat3)
      } 
    }
    else if (substr(m_p,i,i)=='4') {
      output <- output %*% mat4
    }
    else if (substr(m_p,i,i)=='5') {
      output <- output %*% mat5
    }
  }
  return (output)
}

Sim_Pro <- function(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, P) {
  DC <- PathCal(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, P)
  SDC <- PathSim(DC)
  #NSDC <- L_Norm(SDC)
  return (SDC)
}