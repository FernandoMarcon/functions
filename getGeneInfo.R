library("biomaRt")
ensembl = useMart("ensembl",dataset="hsapiens_gene_ensembl")
gene.dic = getBM(attributes=c('ensembl_gene_id','hgnc_symbol','entrezgene_id','chromosome_name','start_position','end_position','description'), 
      filters = 'ensembl_gene_id', values = ensemblsIDS, mart = ensembl)
