#--- Normalize to reference
norm2ref <- function(data, meta, id_col, key_col, states_col, ref_value) {
  meta = meta[,c(id_col,key_col, states_col)]
  colnames(meta) = c('id','key','states')
  meta.s = split(meta, meta$key)
  key.hasREF =sapply(meta.s, function(df) ref_value %in% df$states)
  key.hasREF = key.hasREF[which(key.hasREF == T)]
  meta = subset(meta, key %in% names(key.hasREF))
  data = data.frame(t(data[,meta$id]))
  
  meta.s = split(meta, meta$states)
  data.s = split(data, meta$states)
  
  rownames(data.s[[ref_value]]) <- rownames(meta.s[[ref_value]]) <- meta.s[[ref_value]]$key
  states = setdiff(names(meta.s), ref_value)#[1]
  states.norm = lapply(states, function(s) { # s = states
    rownames(data.s[[s]]) <- rownames(meta.s[[s]]) <- meta.s[[s]]$key
    coherent_dataset = make_coherent(t(data.s[[ref_value]]), data.s[[s]])
    state.norm = log2(t(coherent_dataset[[2]])) - log2(coherent_dataset[[1]])
    colnames(state.norm) <- meta.s[[s]][rownames(coherent_dataset[[2]]),'id']
    return(state.norm)
  })
  return(Reduce(cbind, states.norm))
}

make_coherent = function(spread.df, long.df) {
  common.samples = intersect(rownames(long.df), colnames(spread.df))
  long.df = long.df[common.samples,]
  spread.df = spread.df[,rownames(long.df)]
  if(identical(colnames(spread.df),rownames(long.df))) {
    return(list(spread.df, long.df))
  } else { 
    message('not coherent...')
    }
}
