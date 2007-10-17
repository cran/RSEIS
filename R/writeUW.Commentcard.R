`writeUW.Commentcard` <-
function(comments)
  {
    v = vector()
    for(i in 1:length(comments))
      {
        v[i] = paste(sep=" ", "C", comments[i])
      }
    return(v)

  }

