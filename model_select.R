#' Function for model selection based on DIC and WAIC criteria
#' 
#' runs all the possible combinations of models 
#' (fixed and random effects) and returns the DIC and WAIC
#'
#' @param resp Response variable
#' @param variables Name of the variables to be included in the models
#' @param data Dataframe
#' @param n Number of output models
#' @param family Family of the response variable (default "binomial")
#' @param ... Arguments of the inla function
#'
#' @return
#' @export
#'
#' @examples
model_select<-function(resp, variables, data, n, family="binomial",...)
{  
  # terms to be used
  sel.terms <- switch('terms',terms=variables)
  
  # all combinations
  comb.terms <- function(m, v=sel.terms) {
    if(m==0) return('resp ~ 1')
    else {
      combis <- apply(combn(v, m), 2, paste, collapse=' + ')
      return(paste('resp ~ 1', combis, sep=' + '))
    }
  }
  
  # list with all possible models
  f.list <- unlist(sapply(0:length(sel.terms), comb.terms))
  
  # run each model and save DIC and WAIC
  dic<-numeric()
  waic<-numeric()
  for(i in 1:length(f.list)){
    print(c(i, f.list[i]))
    res <- try(inla(formula = eval(parse(text=f.list[i])), family=family, data=data, ...))
    if(class(res)!= "try-error"){
      dic[i] <- res$dic$dic
      waic[i]<-res$waic$waic
    }else{ # if the model fails it returns NA for DIC and WAIC and continues with the next model
      dic[i] <- waic[i] <- NA
      print("error_run")
    }
  }
  
  # models sorted by DIC
  models_DIC<-data.frame(f.list[order(dic)[1:n]], 
                          dic[order(dic)[1:n]], 
                          waic[order(dic)[1:n]])
  colnames(models_DIC)<-c("Model", "DIC", "WAIC")
  
  # models sorted by WAIC
  models_WAIC<-data.frame(f.list[order(waic)[1:n]], 
                           dic[order(waic)[1:n]], 
                           waic[order(waic)[1:n]])
  colnames(models_WAIC)<-c("Model", "DIC", "WAIC")

  models<-list(models_DIC, models_WAIC)
  names(models)<-c("sortby_DIC", "sortby_WAIC")
  models
}
