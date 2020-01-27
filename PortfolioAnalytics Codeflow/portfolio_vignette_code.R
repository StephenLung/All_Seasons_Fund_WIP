R <- edhec[, 1:4]
 # set up simple portfolio with leverage and box constraints
   pspec <- portfolio.spec(assets=colnames(R))
   pspec <- add.constraint(portfolio=pspec, type="leverage",
                            min_sum=0.99, max_sum=1.01)
   pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1)
   # generate random portfolios using the 3 methods
     rp1 <- random_portfolios(portfolio=pspec, permutations=5000,
                                rp_method='sample')
     rp2 <- random_portfolios(portfolio=pspec, permutations=5000,
                                rp_method='simplex')
     rp3 <- random_portfolios(portfolio=pspec, permutations=5000,
                                rp_method='grid')
     # show feasible portfolios in mean-StdDev space
       tmp1.mean <- apply(rp1, 1, function(x) mean(R %*% x))
       tmp1.StdDev <- apply(rp1, 1, function(x) StdDev(R=R, weights=x))
       tmp2.mean <- apply(rp2, 1, function(x) mean(R %*% x))
       tmp2.StdDev <- apply(rp2, 1, function(x) StdDev(R=R, weights=x))
       tmp3.mean <- apply(rp3, 1, function(x) mean(R %*% x))
       tmp3.StdDev <- apply(rp3, 1, function(x) StdDev(R=R, weights=x))
       # plot feasible portfolios
         plot(x=tmp1.StdDev, y=tmp1.mean, col="gray", main="Random Portfolio Methods",
                ylab="mean", xlab="StdDev")
       points(x=tmp2.StdDev, y=tmp2.mean, col="red", pch=2)
       points(x=tmp3.StdDev, y=tmp3.mean, col="lightgreen", pch=5)
       legend("bottomright", legend=c("sample", "simplex", "grid"),
                col=c("gray", "red", "lightgreen"),
                pch=c(1, 2, 5), bty="n")
       
       
       
        fev <- 0:5
        par(mfrow=c(2, 3))
        for(i in 1:length(fev)){
          rp <- rp_simplex(portfolio=pspec, permutations=2000, fev=fev[i])
          tmp.mean <- apply(rp, 1, function(x) mean(R %*% x))
          tmp.StdDev <- apply(rp, 1, function(x) StdDev(R=R, weights=x))
          plot(x=tmp.StdDev, y=tmp.mean, main=paste("FEV =", fev[i]),
                 ylab="mean", xlab="StdDev", col=rgb(0, 0, 100, 50, maxColorValue=255))
          }
        par(mfrow=c(1,1))
       
  
        data(edhec)
         R <- edhec[, 1:6]
         colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQMN", "ED")
         funds <- colnames(R)
         # Create an initial portfolio object with leverage and box constraints
           init <- portfolio.spec(assets=funds)
         init <- add.constraint(portfolio=init, type="leverage",
                                  min_sum=0.99, max_sum=1.01)
         init <- add.constraint(portfolio=init, type="box", min=0.05, max=0.65)
        
        
        
        meanETL <- add.objective(portfolio=init, type="return", name="mean")
         meanETL <- add.objective(portfolio=meanETL, type="risk", name="ETL",
                                    arguments=list(p=0.95))
        opt_meanETL <- optimize.portfolio(R=R, portfolio=meanETL,
                                           optimize_method="random",
                                           trace=TRUE, search_size=2000)
        
        
         # change the box constraints to long only
           init$constraints[[2]]$min <- rep(0, 6)
         init$constraints[[2]]$max <- rep(1, 6)
         rb_meanETL <- add.objective(portfolio=init, type="return", name="mean")
         rb_meanETL <- add.objective(portfolio=rb_meanETL, type="risk", name="ETL",
                                       arguments=list(p=0.95))
         rb_meanETL <- add.objective(portfolio=rb_meanETL, type="risk_budget",
                                       name="ETL", max_prisk=0.4, arguments=list(p=0.95))
        opt_rb_meanETL <- optimize.portfolio(R=R, portfolio=rb_meanETL,
                                              optimize_method="DEoptim",
                                              search_size=2000,
                                              trace=TRUE, traceDE=5)
        chart.RiskBudget(opt_rb_meanETL, risk.type="percentage", neighbors=25)
        
        eq_meanETL <- add.objective(portfolio=init, type="return", name="mean")
         eq_meanETL <- add.objective(portfolio=eq_meanETL, type="risk", name="ETL",
                                       arguments=list(p=0.95))
         eq_meanETL <- add.objective(portfolio=eq_meanETL, type="risk_budget",
                                       name="ETL", min_concentration=TRUE,
                                       arguments=list(p=0.95))
        opt_eq_meanETL <- optimize.portfolio(R=R, portfolio=eq_meanETL,
                                              optimize_method="DEoptim",
                                            
                                              search_size=2000,
                                              trace=TRUE, traceDE=5)
        
        
        
        
        opt_combine <- combine.optimizations(list(meanETL=opt_meanETL,
                                                   rbmeanETL=opt_rb_meanETL,
                                                   eqmeanETL=opt_eq_meanETL))
        extractObjectiveMeasures(opt_combine)
        
         # View the weights and objective measures of each optimization
           extractWeights(opt_combine)
           
           chart.RiskReward(opt_combine, risk.col="ETL", return.col="mean",
                             main="ETL Optimization Comparison", xlim=c(0.018, 0.024),
                             ylim=c(0.005, 0.008))
        