


stldac_vb <- function(alpha_start=1,#beta_start=.1,xi_start = 1,
                      users,dw,nT,nC,
                      maxiter=10,tol=.01,seed=1.96,
                      n.cores = parallel::detectCores()){
  
  #drop columns without words
  #dw <- dw[,colSums(dw)>0]

  preschedule <- F #parallel option
  
  #extract dimensions
  nU <- users %>% unique %>% length
  nD <- dim(dw)[1]
  nW <- dim(dw)[2]
  
  #turn users into integers and store list of doc ids
  users <- users %>% match(unique(users))

  
  #compute multinomial constant for each document
  #mnc <- apply(dw,1,function(w) gamma(sum(w)+1)/prod(gamma(w+1)))
  
  #initialize model parameters
  set.seed(seed)
  alpha_mat <- matrix(alpha_start + rnorm(nC*nT,0,10),nrow = nC,ncol = nT) %>% abs #matrix(alpha_start,nrow = nC,ncol = nT) #alpha_true+e #
  beta_mat <- rdir(nT,alpha=rep(1,nW)) #matrix(1/nW,nrow=nT,ncol = nW) #tw_true+e #
  xi <- rep(1/nC,nC)
  
  #initialize user parameters
  gamma_mat <- matrix(alpha_start + rnorm(nU*nT,0,10),nrow=nU,ncol=nT) %>% abs #ut_true_counts+e #
  phi_mat <- rdir(nD,alpha = rep(1,nT)) #matrix(1/nT,nD,nT) #diag(nT)[ta_true,]+e #
  lambda_mat <-  rdir(nU,alpha = rep(1,nC)) #matrix(1/nC,nrow=nU,ncol=nC) #lambda_true

  converged <- FALSE
  loglike_new <- -Inf
  i <- 1
  
  #just fixed number of itterations for now
  start <- Sys.time()
  while(!converged & i<=maxiter){
    loglike_old <- loglike_new
    
    #update user paramters with parallel option
    userUpdates <- parallel::mclapply(1:nU,mc.cores = n.cores,mc.preschedule=preschedule,function(u){
    #for(u in 1:nU){
      convergedU <- F 
      j <- 1
      while(!convergedU & j<20){ 
        #save old values to check convergence
        phiU_old <- phi_mat[(1:nD)[users==u],]
        gammaU_old <- gamma_mat[u,]
        lambdaU_old <- lambda_mat[u,]
        
        #phi_mat[(1:nD)[users==u],] 
        phiU_new <- sapply((1:nD)[users==u],function(d){
            update_phiUD(dw[d,],gamma = gamma_mat[u,],beta = beta_mat)
          })
        
        #gamma_mat[u,] <- 
        gammaU_new <- update_gammaU(lambdaU = lambda_mat[u,],phiU = phi_mat[users == u,],alpha = alpha_mat)
        #lambda_mat[u,] <- 
        lambdaU_new <- update_labmdaU(xi,gamma_mat[u,],alpha = alpha_mat)
        
        if(max(abs(phiU_old-phi_mat[(1:nD)[users==u],]))<.001 & 
           max(abs(gammaU_old-gamma_mat[u,]))<.001 & 
           max(abs(lambdaU_old-lambda_mat[u,]))<.001) convergedU <- TRUE
        j <- j+1
      }
      #print(lambdaU_new)
      #print(u)
     return(list(phiU_new,gammaU_new,lambdaU_new))
    })

    phi_mat <- lapply(1:nU,function(u) t(userUpdates[[c(u,1)]])) %>% do.call(what="rbind")
    gamma_mat <- sapply(1:nU,function(u) userUpdates[[c(u,2)]]) %>% t
    lambda_mat <- sapply(1:nU,function(u) userUpdates[[c(u,3)]]) %>% t
    if(nC==1) lambda_mat <- matrix(1,nrow=nU,ncol=1)

    #update model prameters
    xi <- update_xi(lambda_mat) #only one step required here
    beta_mat <- update_beta(phi_mat,dw)
    #beta_mat[1:10,1:5] #%>% print
    
    converged_alpha <- FALSE
    q <- 0
    while(!converged_alpha & q<10){#should become while not converged
      alpha_mat_old <- alpha_mat

      for(j in 1:10){ #should become while not converged
        alpha_mat <- update_alpha(lambda_mat,gamma_mat,alpha_mat)
      }
      #print(alpha_mat[,1])
      
      #check convergence
      if(max(abs(alpha_mat_old-alpha_mat))<.001) converged_alpha <- TRUE
      q <- q+1
    }
    
    #xi %>% print
    
    
    
    
    
    if(i %% 4 == 0 | i<=maxiter){ #update likelihood every 5 itterattions and on the final itteration
      #NEW: do every round
      loglike_new <- elbo(alpha_mat = alpha_mat,beta_mat = beta_mat,xi = xi,
                          lambda_mat = lambda_mat,phi_mat = phi_mat,gamma_mat = gamma_mat,
                          W_mat = dw,users_list = users,n.cores = n.cores)
      print(str_c("ll: ",loglike_new,", delta: ",exp(loglike_new-loglike_old)))
      if(i>5&(exp(loglike_new-loglike_old)<1+tol)) converged <- TRUE
    }
    
    i <- i+1
    if(i %% 5==0) print(i)
    end <- Sys.time()
  }
  (end-start)/(i-1)
  print(str_c("Time elapsed: ",round(end-start,2),", iterations completed: ",i-1))
  #phi_mat %>% apply(1,which.max) %>% table(ta_true)
  #lambda_mat %>% apply(1,which.max) %>% table(ca_true)
  
  return(list("lambda_mat"=lambda_mat,"phi_mat"=phi_mat,"gamma_mat"=gamma_mat,
              "alpha_mat"=alpha_mat,"beta_mat"=beta_mat,"xi"=xi,
              log_likelihood = loglike_new,lr_delta = exp(loglike_new-loglike_old)))
}

#rm(alpha_mat,alpha_mat_old,beta_mat,gamma_mat,lambda_mat,phi_mat,userUpdates)

