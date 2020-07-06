

### Variational Bayes Functions ###

######################################################################
#' 
#' Each function returns the ELBO maximizing value given the inputs. 
#' 
#' User-specific functions. The interpretation of the parameters is listed
#' with parentheses to note dependencies:
#'    lambdaU - user-specific cluster membership probabilities (gammaU, xi, alpha)
#'    phiUD - user-document-sepcific topic probabilities (W_ud,gammaU,beta)
#'    gammaU - users-specific marginal topic probabilities 
#'    
#' 
#' Model-level functions. 
#'    xi - corpus-level cluster probabilities/proportions (Gx1)
#'    alpha - cluster-specific dirichlet parameters (GxT)
#'    beta - topic distributions (TxV)
#' 

### Log Dirichelt Expectation
#many functions require computing the expected value of a logged Dirichlet random variable
expt_ldir <- function(i,param){
  digamma(param[i]) - digamma(sum(param))
}


#############################
### User-Specific Updates ###
#############################

### Lambda
update_labmdaU <- function(clusterP,gammaU,alpha_mat){
  if(nrow(alpha_mat)==1) return(1)
  
  sum_gamma <- sum(gammaU)
  sum_alpha <- rowSums(alpha_mat)
  
  expts <- sapply(1:dim(alpha_mat)[1],
                  function(g) lgamma(sum_alpha[g]) - sum(lgamma(alpha_mat[g,])) + sum((alpha_mat[g,]-1)*expt_ldir(1:dim(alpha_mat)[2],gammaU)))
  
  log_lambdaU <- log(clusterP) + expts
  lambdaU <- exp(log_lambdaU-max(log_lambdaU))
  lambdaU <- lambdaU/sum(lambdaU)
  lambdaU
}

### phiUD
update_phiUD <- function(W_ud,gammaU,beta_mat){
  nT <- length(gammaU)
  
  expts <- expt_ldir(1:nT,gammaU)
  
  log_likelihoods <- sapply(1:nT,function(t){
    sum(W_ud*if_else(beta_mat[t,]>0,log(beta_mat[t,]),0))
  })
  
  log_phiUD <- expts + log_likelihoods
  phiUD <- exp(log_phiUD-max(log_phiUD))
  phiUD <- phiUD/sum(phiUD)
  phiUD
}

### gammaU
update_gammaU <- function(lambdaU,phiU,alpha){
  colSums(lambdaU*alpha) + colSums(phiU)
}
  
###############################
### Corpus-Level Parameters ###
###############################

#xi
update_xi <- function(lambda_mat){
  colSums(lambda_mat) %>% {./sum(.)}
}

#beta
# will store phi as a large matrix, DxT
# will pre-compute c_ud, multinomial normalizer for each document
# for each t, take W, DxV word occurance matrix, multiply each column by phi[,t], rowsum, normalize

update_beta <- function(phi_mat,W_mat,beta_min=NULL){
  if(is.null(beta_min)) beta_min <- 0.0000001#1/dim(W_mat)[2]/100
  
  beta_unnormalized <- sapply(1:ncol(phi_mat),function(t){
    colSums(phi_mat[,t]*W_mat)
  }) %>% t
  
  beta_unnormalized %>% {./rowSums(.)} %>% {pmax(.,beta_min)} %>% {./rowSums(.)}
}


update_alpha <- function(lambda_mat,gamma_mat,alpha_old,lb=.00000001){
  #precompute M_g and E_q[log theta_t]
  nT <- dim(alpha_old)[2]
  lambda_sums <- colSums(lambda_mat)
  expts <- t(apply(gamma_mat,1,function(r) expt_ldir(1:nT,r)))

  #for g in 1:G
  sapply(1:dim(alpha_old)[1],function(g){
    gradient <- lambda_sums[g]*(digamma(sum(alpha_old[g,])) - digamma(alpha_old[g,])) + 
      colSums(expts*lambda_mat[,g])
    
    #hessian statistics
    hess_diag <- -trigamma(alpha_old[g,])*lambda_sums[g]
    hess_const <- trigamma(sum(alpha_old))*lambda_sums[g]
      
    #brute force hessian
    # hess <- diag(hess_diag) + matrix(hess_const,nrow = nT,ncol = nT)
    # alphag_new <- alpha_old[g,] - solve(hess) %*% matrix(gradient)
    
    #use linear-time trick
    c <- sum(gradient/hess_diag)/(1/hess_const + sum(1/hess_diag))
    hess_delta <- (gradient-c)/hess_diag
    alphag_new <- alpha_old[g,] - hess_delta
    
    
    pmax(alphag_new,lb)
  }) %>% t
}


#ELBO bound
elbo <- function(alpha_mat,beta_mat,xi,lambda_mat,phi_mat,gamma_mat,W_mat,users_list=users,n.cores = parallel::detectCores()){
  
  nT <- dim(alpha_mat)[2]
  nC <- dim(alpha_mat)[1]
  nU <- dim(lambda_mat)[1]
  nD <- dim(phi_mat)[1]
  
  expts <- t(apply(gamma_mat,1,function(r) expt_ldir(1:nT,r)))
  #log_beta_mat <- log(beta_mat)
  log_beta_mat_t <- t(log(beta_mat))
  
  #mapping from overleaf terms indicated in comments
  sum(t(lambda_mat[1:3,]) * log(xi)) + #1
    sum((lgamma(rowSums(alpha_mat))-rowSums(lgamma(alpha_mat)))*colSums(lambda_mat)) + #2 partially
    sum(unlist(parallel::mclapply(1:nU,mc.cores=n.cores,function(u){
      sum(unlist(lapply(1:nC,function(c){
        sum((alpha_mat[c,]-1)*expts[u,]*lambda_mat[u,c])
      })))
    }))) + #rest of 2
    sum(phi_mat*expts[users_list,]) + #3
    #sum(W_mat[rep(1:nD,rep(nT,nD)),]*log_beta_mat[rep(1:nT,nD),]*phi_mat[rep(1:nD,rep(nT,nD)),]) #breaks vector memory limitt
    sum(unlist(parallel::mclapply(1:nD,mc.cores=n.cores,mc.preschedule=T,function(d){
      sum(t(W_mat[d,]*log_beta_mat_t)*phi_mat[d,])
      #sum(matrix(W_mat[d,],nrow=nT,ncol=dim(W_mat)[2],byrow=T)*log_beta_mat*matrix(phi_mat[d,],ncol = dim(W_mat)[2],nrow = nT))
    }))) + #4, norm constant is constant so ignored
    -sum(lambda_mat*ifelse(lambda_mat==0,0,log(lambda_mat))) + #5
    sum(apply(gamma_mat,1,function(gU){
      -lgamma(sum(gU)) + sum(lgamma(gU)) - sum((gU-1)*expt_ldir(1:nT,gU))
    })) + #6
    -sum(phi_mat*ifelse(phi_mat==0,0,log(phi_mat))) #7
}


