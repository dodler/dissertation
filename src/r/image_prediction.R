es_num <- 50
es_low_bound <- 0.001
es_high_bound <- 0.999
es_pred_num <- 5

# es stands for exponential smoothing

predict_image <- function(series){
  
  es_step <- (es_high_bound - es_low_bound) / es_num
  
  cur_alpha <- es_low_bound
  
  result <- fitted(ses(series, alpha=cur_alpha, h=es_pred_num))
  
  for(i in (1:es_num)){
    
    es <- fitted(ses(series, alpha=cur_alpha, h=es_pred_num))
    
    result <- rbind(result, es)
    
    cur_alpha <- cur_alpha + es_step
  }
  
  #max_res_val <- max(result)
  #result <- result/max_res_val
  
  return (blur(as.im(result), sigma=10)$v)
}