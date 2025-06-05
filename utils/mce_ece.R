ece_mce = function(y, prob, g, stat_type, yhat = NULL) {
  mtx = data.frame(y = y, prob = prob)
  if (!is.null(yhat)) {
    if (is.factor(yhat)) {
      # suppose que les niveaux sont dans l'ordre c("zero", "un")
      yhat <- as.numeric(yhat) - 1
    } else {
      yhat <- as.numeric(yhat)
      if (all(unique(yhat) %in% c(1, 2))) {
        yhat <- yhat - 1
      }
    }
    mtx$yhat <- yhat
  }
  
  
  # Tri croissant sur les probabilités
  mtx = mtx[order(mtx$prob),]
  n <- length(prob)/g
  nr <- nrow(mtx)
  
  if (stat_type == 'C') {
    split_mtx = split(mtx, rep(1:ceiling(nr/n), each=n, length.out=nr))
    taille = n
  } else {
    split_mtx = split(mtx, cut(mtx$prob, seq(0, 1, 1/g), include.lowest = TRUE))
    split_mtx = split_mtx[sapply(split_mtx, nrow) > 0]
  }
  
  H_stat = c()
  ece_acc = c()
  taux = c()
  estimee = c()
  
  for (i in 1:length(split_mtx)) {
    group = split_mtx[[i]]
    obs = mean(group$y == 1)
    exp = mean(group$prob)
    
    H_stat = c(H_stat, abs(obs - exp))
    taux = c(taux, obs)
    estimee = c(estimee, exp)
    
    if (!is.null(group$yhat)) {
      acc = mean(group$y == group$yhat)
      ece_acc = c(ece_acc, abs(acc - exp))
    }
  }
  
  return(list(
    taille = taille,
    ece = mean(H_stat),
    mce = max(H_stat),
    ece_acc = if (length(ece_acc) > 0) mean(ece_acc) else NA,
    taux = taux,
    estimee = estimee,
    split = split_mtx
  ))
}
