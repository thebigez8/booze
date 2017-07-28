
coclustering <- function(mat, ncenter)
{
  out <- matrix(0, nrow = nrow(mat), ncol = nrow(mat), dimnames = list(rownames(mat), rownames(mat)))

  membership <- fitted(kmeans(mat, centers = ncenter), method = "classes")
  for(i in 1:ncenter)
  {
    idx <- names(membership[membership == i])
    out[idx, idx] <- 1 + out[idx, idx]
  }
  out
}

evaluate_coclustering <- function(mat, max.k = min(20, nrow(mat)), seed = NULL)
{
  set.seed(seed)
  output <- list()

  for(ncenter in 2:max.k)
  {

    tmp <- Reduce("+", replicate(100, coclustering(mat, ncenter), simplify = FALSE))

    ord <- order(pcs <- prcomp(tmp)$x[, 1])

    output[[ncenter]] <- structure(tmp[ord, ord], k = ncenter, pcs = pcs[ord], class = c("coclustered", "matrix"))
  }

  invisible(output)
}

plot.coclustered <- function(cc)
{
  p <- cc %>%
    reshape2::melt() %>%
    mutate(Var1 = factor(Var1, levels = rownames(cc)), Var2 = factor(Var2, levels = rownames(cc))) %>%
    ggplot(aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      ggtitle(paste0("Co-clustering matrix for k = ", attr(cc, "k"))) +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  print(p)
}

