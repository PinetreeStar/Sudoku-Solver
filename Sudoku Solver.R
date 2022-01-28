Sudoku_Solver<-function(nums){
  #make all existing numbers prime
  for (i in (1:81)){
    if (nums[i]==1){
      nums[i]<-2
    }else if (nums[i]==2){
      nums[i]<-3
    }else if (nums[i]==3){
      nums[i]<-5
    }else if (nums[i]==4){
      nums[i]<-7
    }else if (nums[i]==5){
      nums[i]<-11
    }else if (nums[i]==6){
      nums[i]<-13
    }else if (nums[i]==7){
      nums[i]<-17
    }else if (nums[i]==8){
      nums[i]<-19
    }else if (nums[i]==9){
      nums[i]<-23
    }else{
      nums[i]<-223092870
    }
  }
  puz<-matrix(nums,nrow=9,byrow=TRUE)
  primes<-c(2,3,5,7,11,13,17,19,23)
  #Checking inputs
  for (a in (1:9)){
    for (b in primes){
      guard<-0
      for (c in (1:9)){
        if (b==puz[a,c]){
          if (guard==0){
            guard<-1
          }else{
            guard<--1
          }
        }
      }
      if (guard==-1){
        return("Unsolvable, check input values")
      }
    }
  }
  for (a in (1:9)){
    for (b in primes){
      guard<-0
      for (c in (1:9)){
        if (b==puz[c,a]){
          if (guard==0){
            guard<-1
          }else{
            guard<--1
          }
        }
      }
      if (guard==-1){
        return("Unsolvable, check input values")
      }
    }
  }
  for (a in c(1,4,7)){
    for (b in c(1,4,7)){
      subpuz<-matrix(puz[a:(a+2),b:(b+2)],nrow=3)
      for (c in primes){
        guard<-0
        for (d in (1:3)){
          for (e in (1:3)){
            if (c==subpuz[d,e]){
              if (guard==0){
                guard<-1
              }else{
                guard<--1
              }
            }
          }
        }
        if (guard==-1){
          return("Unsolvable, check input values")
        }
      }
    }
  }
  guesses<-0
  while (TRUE){
    puzprime<-puz
    #Simple Solver
    z<-SimpleSolver(puz)
    if (FALSE %in% (z==puz)){
      puz<-z
    }
    if ((FALSE %in% (puzprime==puz))==FALSE){
      #Row Solver
      for (a in primes){
        for (b in (1:9)){
          d<-0
          guard<-0
          for (c in (1:9)){
            if (a %in% FindFactors(puz[b,c])){
              if (guard==0){
                guard<-1
                d<-c
              }else{
                guard<--1
              }
            }
          }
          if (guard==1){
            puz[b,d]<-a
            z<-SimpleSolver(puz)
            if (FALSE %in% (z==puz)){
              puz<-z
              changes<-1
            }
          }
        }
      }
      #Column Solver
      for (a in primes){
        for (b in (1:9)){
          d<-0
          guard<-0
          for (c in (1:9)){
            if (a %in% FindFactors(puz[c,b])){
              if (guard==0){
                guard<-1
                d<-c
              }else{
                guard<--1
              }
            }
          }
          if (guard==1){
            puz[d,b]<-a
            z<-SimpleSolver(puz)
            if (FALSE %in% (z==puz)){
              puz<-z
              changes<-1
            }
          }
        }
      }
      #Box Solver
      for (a in c(1,4,7)){
        for (b in c(1,4,7)){
          subpuz<-matrix(puz[a:(a+2),b:(b+2)],nrow=3,byrow=FALSE)
          for (c in primes){
            guard<-0
            indexa<-0
            indexb<-0
            for (d in (1:3)){
              for (e in (1:3)){
                if (c %in% FindFactors(subpuz[d,e])){
                  if (guard==0){
                    guard<-1
                    indexa<-d
                    indexb<-e
                  }else{
                    guard<--1
                  }
                }
              }
            }
            if (guard==1){
              puz[(a+indexa-1),(b+indexb-1)]<-c
              z<-SimpleSolver(puz)
              if (FALSE %in% (z==puz)){
                puz<-z
              }
            }
          }
        }
      }
    }
    if ((FALSE %in% (puzprime==puz))==FALSE){
      #Plain-Hint Row Solver
      for (a in (1:9)){
        for (b in (1:9)){
          ctr<-0
          index<-c()
          hints<-c()
          for (c in (1:9)){
            if ((FALSE %in% (FindFactors(puz[a,c]) %in% FindFactors(puz[a,b])))==FALSE){
              ctr<-(ctr+1)
              index<-c(index,c)
              hints<-c(hints,puz[a,c])
              
            }
          }
          if (ctr>1){
            if (ctr==length(FindFactors(puz[a,b]))){
              for (d in FindFactors(puz[a,b])){
                puz[a,]<-DivideRow(d,puz[a,])
              }
              for (e in (1:length(index))){
                puz[a,(index[e])]<-(hints[e])
              }
              z<-SimpleSolver(puz)
              if (FALSE %in% (z==puz)){
                puz<-z
                changes<-1
              }
            }
          }
        }
      }
      #Plain-Hint Column Solver
      for (a in (1:9)){
        for (b in (1:9)){
          ctr<-0
          index<-c()
          hints<-c()
          for (c in (1:9)){
            if ((FALSE %in% (FindFactors(puz[c,a]) %in% FindFactors(puz[b,a])))==FALSE){
              ctr<-(ctr+1)
              index<-c(index,c)
              hints<-c(hints,puz[c,a])
            }
          }
          if (ctr>1){
            if (ctr==length(FindFactors(puz[b,a]))){
              for (d in FindFactors(puz[b,a])){
                puz[,a]<-DivideRow(d,puz[,a])
              }
              for (e in (1:length(index))){
                puz[(index[e]),a]<-(hints[e])
              }
              z<-SimpleSolver(puz)
              if (FALSE %in% (z==puz)){
                puz<-z
                changes<-1
              }
            }
          }
        }
      }
      #Plain-Hint Box Solver
      for (a in c(1,4,7)){
        for (b in c(1,4,7)){
          subpuz<-matrix(puz[a:(a+2),b:(b+2)],nrow=3,byrow=FALSE)
          for (c in (1:3)){
            for (d in (1:3)){
              ctr<-0
              indexa<-c()
              indexb<-c()
              hints<-c()
              for (e in (1:3)){
                for (f in (1:3)){
                  if ((FALSE %in% (FindFactors(subpuz[e,f]) %in% FindFactors(subpuz[c,d])))==FALSE){
                    ctr<-(ctr+1)
                    indexa<-c(indexa,e)
                    indexb<-c(indexb,f)
                    hints<-c(hints,subpuz[e,f])
                  }
                }
              }
              if (ctr>1){
                if (ctr==length(FindFactors(subpuz[c,d]))){
                  for (g in FindFactors(subpuz[c,d])){
                    for (h in (1:3)){
                      for (i in (1:3)){
                        if ((subpuz[h,i]%%g)==0){
                          subpuz[h,i]<-(subpuz[h,i]/g)
                        }
                      }
                    }
                    for (j in (1:ctr)){
                      subpuz[indexa[j],indexb[j]]<-(hints[j])
                    }
                  }
                  puz[a:(a+2),b:(b+2)]<-subpuz
                  z<-SimpleSolver(puz)
                  if (FALSE %in% (z==puz)){
                    puz<-z
                    changes<-1
                  }
                }
              }
            }
          }
        }
      }
    }
    if ((FALSE %in% (puzprime==puz))==FALSE){
      #Pointer-Hint Row Solver
      for (a in (1:9)){
        for (b in primes){
          index<-c()
          for (c in (1:9)){
            if (b %in% FindFactors(puz[a,c])){
              index<-c(index,c)
            }
          }
          if (is.null(index)){
            next
          }else if ((FALSE %in% (index %in% (1:3)))==FALSE){
            d<-DivideBox(b,a,1,puz)
            puz<-d
            for (e in index){
              puz[a,e]<-(puz[a,e]*b)
            }
          }else if ((FALSE %in% (index %in% (4:6)))==FALSE){
            d<-DivideBox(b,a,4,puz)
            puz<-d
            for (e in index){
              puz[a,e]<-(puz[a,e]*b)
            }
          }else if ((FALSE %in% (index %in% (7:9)))==FALSE){
            d<-DivideBox(b,a,7,puz)
            puz<-d
            for (e in index){
              puz[a,e]<-(puz[a,e]*b)
            }
          }
          z<-SimpleSolver(puz)
          if (FALSE %in% (z==puz)){
            puz<-z
          }
        }
      }
      #Pointer-Hint Column Solver
      for (a in (1:9)){
        for (b in primes){
          index<-c()
          for (c in (1:9)){
            if (b %in% FindFactors(puz[c,a])){
              index<-c(index,c)
            }
          }
          if (is.null(index)){
            next
          }else if ((FALSE %in% (index %in% (1:3)))==FALSE){
            d<-DivideBox(b,1,a,puz)
            puz<-d
            for (e in index){
              puz[e,a]<-(puz[e,a]*b)
            }
          }else if ((FALSE %in% (index %in% (4:6)))==FALSE){
            d<-DivideBox(b,4,a,puz)
            puz<-d
            for (e in index){
              puz[e,a]<-(puz[e,a]*b)
            }
          }else if ((FALSE %in% (index %in% (7:9)))==FALSE){
            d<-DivideBox(b,7,a,puz)
            puz<-d
            for (e in index){
              puz[e,a]<-(puz[e,a]*b)
            }
          }
          z<-SimpleSolver(puz)
          if (FALSE %in% (z==puz)){
            puz<-z
          }
        }
      }
      #Pointer-Hint Box Solver
      for (a in c(1,4,7)){
        for (b in c(1,4,7)){
          subpuz<-matrix(puz[a:(a+2),b:(b+2)],nrow=3,byrow=FALSE)
          for (c in primes){
            indexa<-c()
            indexb<-c()
            for (d in (1:3)){
              for (e in (1:3)){
                if (c %in% FindFactors(subpuz[d,e])){
                  indexa<-c(indexa,d)
                  indexb<-c(indexb,e)
                }
              }
            }
            if (is.null(indexa)){
              next
            }else if ((FALSE %in% (indexa==1))==FALSE){
              f<-DivideRow(c,puz[a,])
              puz[a,]<-f
              for (g in indexb){
                puz[a,(b+g-1)]<-(puz[a,(b+g-1)]*c)
              }
            }else if ((FALSE %in% (indexa==2))==FALSE){
              f<-DivideRow(c,puz[(a+1),])
              puz[(a+1),]<-f
              for (g in indexb){
                puz[(a+1),(b+g-1)]<-(puz[(a+1),(b+g-1)]*c)
              }
            }else if ((FALSE %in% (indexa==3))==FALSE){
              f<-DivideRow(c,puz[(a+2),])
              puz[(a+2),]<-f
              for (g in indexb){
                puz[(a+2),(b+g-1)]<-(puz[(a+2),(b+g-1)]*c)
              }
            }
            if (is.null(indexb)){
              next
            }else if ((FALSE %in% (indexb==1))==FALSE){
              f<-DivideCol(c,puz[,b])
              puz[,b]<-f
              for (g in indexa){
                puz[(a+g-1),b]<-(puz[(a+g-1),b]*c)
              }
            }else if ((FALSE %in% (indexb==2))==FALSE){
              f<-DivideCol(c,puz[,(b+1)])
              puz[,(b+1)]<-f
              for (g in indexa){
                puz[(a+g-1),(b+1)]<-(puz[(a+g-1),(b+1)]*c)
              }
            }else if ((FALSE %in% (indexb==3))==FALSE){
              f<-DivideCol(c,puz[,(b+2)])
              puz[,(b+2)]<-f
              for (g in indexa){
                puz[(a+g-1),(b+2)]<-(puz[(a+g-1),(b+2)]*c)
              }
            }
            z<-SimpleSolver(puz)
            if (FALSE %in% (z==puz)){
              puz<-z
            }
          }
        }
      }
    }
    if ((FALSE %in% (puzprime==puz))==FALSE){
      #Hidden-Hint Row Solver
      for (a in (1:9)){
        primenums<-c()
        indexes<-c()
        for (b in primes){
          for (c in (1:9)){
            if ((b %in% FindFactors(puz[a,c]))){
              primenums<-c(primenums,b)
              indexes<-c(indexes,c)
            }
          }
        }
        primenumsctr<-c()
        for (d in primes){
          primenumsctr<-c(primenumsctr,length(primenums[primenums==d]))
        }
        for (e in primenumsctr){
          whatprimes<-c()
          if (e==0){
            next
          }else if (e<=length(primenumsctr[primenumsctr==e])){
            whatprimes<-c(whatprimes,primes[primenumsctr==e])
            for (f in whatprimes){
              ctr<-0
              for (g in whatprimes){
                if ((FALSE  %in% (indexes[primenums==f]==indexes[primenums==g]))==FALSE){
                  ctr=(ctr+1)
                }
              }
              if (ctr>=e){
                ctr1<-e
                keycells<-indexes[primenums==f]
                keyhints<-c()
                for (h in whatprimes){
                  if ((FALSE %in% (indexes[primenums==f]==indexes[primenums==h]))==FALSE){
                    keyhints<-c(keyhints,h)
                    ctr1<-(ctr1-1)
                    if (ctr==0){
                      keyhints1<-1
                      for (y in keyhints){
                        keyhints1<-(keyhints1*y)
                      }
                      keyhints<-keyhints1
                      for (i in keycells){
                        puz[a,i]<-keyhints
                      }
                      z<-SimpleSolver(puz)
                      if (FALSE %in% (z==puz)){
                        puz<-z
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      #Hidden-Hint Column Solver
      for (a in (1:9)){
        primenums<-c()
        indexes<-c()
        for (b in primes){
          for (c in (1:9)){
            if ((b %in% FindFactors(puz[c,a]))){
              primenums<-c(primenums,b)
              indexes<-c(indexes,c)
            }
          }
        }
        primenumsctr<-c()
        for (d in primes){
          primenumsctr<-c(primenumsctr,length(primenums[primenums==d]))
        }
        for (e in primenumsctr){
          whatprimes<-c()
          if (e==0){
            next
          }else if (e<=length(primenumsctr[primenumsctr==e])){
            whatprimes<-c(whatprimes,primes[primenumsctr==e])
            for (f in whatprimes){
              ctr<-0
              for (g in whatprimes){
                if ((FALSE  %in% (indexes[primenums==f]==indexes[primenums==g]))==FALSE){
                  ctr<-(ctr+1)
                }
              }
              if (ctr>=e){
                ctr1<-e
                keycells<-indexes[primenums==f]
                keyhints<-c()
                for (h in whatprimes){
                  if ((FALSE %in% (indexes[primenums==f]==indexes[primenums==h]))==FALSE){
                    keyhints<-c(keyhints,h)
                    ctr1<-(ctr1-1)
                    if (ctr==0){
                      keyhints1<-1
                      for (y in keyhints){
                        keyhints1<-(keyhints1*y)
                      }
                      keyhints<-keyhints1
                      for (i in keycells){
                        puz[i,a]<-keyhints
                      }
                      z<-SimpleSolver(puz)
                      if (FALSE %in% (z==puz)){
                        puz<-z
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      #Hidden-Hint Box Solver
      for (a in c(1,4,7)){
        for (b in c(1,4,7)){
          subpuz<-matrix(puz[a:(a+2),b:(b+2)],nrow=3,byrow=FALSE)
          primenums<-c()
          indexesa<-c()
          indexesb<-c()
          for (c in primes){
            for (d in (1:3)){
              for (e in (1:3)){
                if (c %in% FindFactors(subpuz[d,e])){
                  primenums<-c(primenums,c)
                  indexesa<-c(indexesa,d)
                  indexesb<-c(indexesb,e)
                }
              }
            }
          }
          primenumsctr<-c()
          for (f in primes){
            primenumsctr<-c(primenumsctr,length(primenums[primenums==f]))
          }
          for (g in primenumsctr){
            if (g==0){
              next
            }else if (g<=length(primenumsctr[primenumsctr==g])){
              whatprimes<-primes[primenumsctr==g]
              for (h in whatprimes){
                ctr<-0
                for (i in whatprimes){
                  if ((((FALSE  %in% (indexesa[primenums==h]==indexesa[primenums==i]))==FALSE)&((FALSE %in% indexesb[primenums==h]==indexesb[primenums==i])==FALSE))){
                    ctr<-(ctr+1)
                  }
                }
                if (ctr>=g){
                  ctr1<-g
                  keycellsa<-indexesa[primenums==h]
                  keycellsb<-indexesb[primenums==h]
                  keyhints<-c()
                  for (j in whatprimes){
                    if ((((FALSE  %in% (indexesa[primenums==h]==indexesa[primenums==j]))==FALSE)&((FALSE %in% indexesb[primenums==h]==indexesb[primenums==j])==FALSE))){
                      keyhints<-c(keyhints,j)
                      ctr1<-(ctr1-1)
                      if (ctr1==0){
                        keyhints1<-1
                        for (y in keyhints){
                          keyhints1<-(keyhints1*y)
                        }
                        keyhints<-keyhints1
                        for (k in (1:length(keycellsa))){
                          subpuz[keycellsa[k],keycellsb[k]]<-keyhints
                        }
                        puz[a:(a+2),b:(b+2)]<-subpuz
                        z<-SimpleSolver(puz)
                        if (FALSE %in% (z==puz)){
                          puz<-z
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      #Checker/Guesser/Exit
      if ((FALSE %in% (puzprime==puz))==FALSE){
        Stopper<-FALSE
        if ((FALSE %in% (puzprime==puz))==FALSE){
          checks<-0
          for (a in (1:9)){
            for (b in primes){
              if (b %in% puz[a,]){
                checks<-(checks+1)
              }
            }
          }
          for (a in (1:9)){
            for (b in primes){
              if (b %in% puz[,a]){
                checks<-(checks+1)
              }
            }
          }
          rows<-c(1:3)
          for (a in (1:3)){
            cols<-c(1:3)
            for (b in (1:3)){
              for (c in primes){
                if ((c %in% puz[rows,cols])){
                  checks<-(checks+1)
                }
              }
              cols<-(cols+3)
            }
            rows<-(rows+3)
          }
          if (checks==243){
            break
          }else{
            if (guesses==0){
              guesses<-1
              for (a in (1:9)){
                for (b in (1:9)){
                  if ((0 %in% FindFactors(puz[a,b]))==FALSE){
                    keyboxa<-a
                    keyboxb<-b
                    keyfacts<-FindFactors(puz[a,b])
                    puza<-puz
                    puz[a,b]<-FindFactors(puz[a,b])[1]
                    Stopper<-TRUE
                    if (Stopper){
                      break
                    }
                  }
                  if (Stopper){
                    break
                  }
                }
                if (Stopper){
                  break
                }
              }
            }else if (guesses<length(FindFactors(puza[keyboxa,keyboxb]))){
              guesses<-(guesses+1)
              puz<-puza
              puz[keyboxa,keyboxb]<-(FindFactors(puza[keyboxa,keyboxb])[guesses])
            }else if (guesses==length(FindFactors(puza[keyboxa,keyboxb]))){
              guesses<-1
              puz<-puza
              changed<-TRUE
              for (a in (1:9)){
                for (b in (1:9)){
                  if ((a>keyboxa)&(b>keyboxb)){
                    if ((0 %in% FindFactors(puz[a,b]))==FALSE){
                      keyboxa<-a
                      keyboxb<-b
                      keyfacts<-FindFactors(puz[a,b])
                      puza<-puz
                      puz[a,b]<-FindFactors(puz[a,b])[1]
                      Stopper<-TRUE
                      if (Stopper){
                        break
                      }
                    }
                  }
                  if (Stopper){
                    break
                  }
                }
                if (Stopper){
                  break
                }else{
                  return("Currently unsolvable by this program")
                }
              }
            }
          }
        }
      }
    }
  }
  #Reverting primes to their intended digits
  for (i in (1:9)){
    for (j in (1:9)){
      if (puz[i,j]==2){
        puz[i,j]<-1
      }else if (puz[i,j]==3){
        puz[i,j]<-2
      }else if (puz[i,j]==5){
        puz[i,j]<-3
      }else if (puz[i,j]==7){
        puz[i,j]<-4
      }else if (puz[i,j]==11){
        puz[i,j]<-5
      }else if (puz[i,j]==13){
        puz[i,j]<-6
      }else if (puz[i,j]==17){
        puz[i,j]<-7
      }else if (puz[i,j]==19){
        puz[i,j]<-8
      }else if (puz[i,j]==23){
        puz[i,j]<-9
      }else if (puz[i,j]==6){
        puz[i,j]<-0
      }
    }
  }
  return(puz)
}

FindFactors<-function(num){
  facts<-c()
  while (TRUE){
    changes<-0
    if ((num==1)){
      if (length(facts)<2){
        return(0)
      }else{
        return(facts)
      }
    }
    for (i in (2:num)){
      if ((num%%i)==0){
        facts<-c(facts,i)
        num<-(num/i)
        changes<-1
        break
      }
    }
    if (changes==1){
    }else if (length(facts)==0){
      return(0)
    }else{
      facts<-c(facts,num)
      return(facts)
    }
  }
}

SimpleSolver<-function(puz){
  changes<-1
  while (changes>0){
    changes<-0
    for (a in (1:9)){
      for (b in (1:9)){
        d<-FindFactors(puz[a,b])
        if ((0 %in% d)){
          e<-DivideRow(puz[a,b],c(puz[a,]))
          if (FALSE %in% (e==puz[a,])){
            changes<-1
          }
          puz[a,]<-e
          f<-DivideCol(puz[a,b],c(puz[,b]))
          if (FALSE %in% (f==puz[,b])){
            changes<-1
          }
          puz[,b]<-f
          g<-DivideBox(puz[a,b],a,b,puz)
          if (FALSE %in% (g==puz)){
            changes<-1
          }
          puz<-g
        }
      }
    }
  }
  return(puz)
}

DivideRow<-function(a,roww){
  for (b in (1:9)){
    if ((a %in% FindFactors(roww[b]))){
      roww[b]<-(roww[b]/a)
    }
  }
  return(roww)
}

DivideCol<-function(a,coll){
  for (b in (1:9)){
    if ((a %in% FindFactors(coll[b]))){
      coll[b]<-(coll[b]/a)
    }
  }
  return(coll)
}

DivideBox<-function(z,a,b,puz){
  if (a %in% c(1,2,3)){
    rows<-(1:3)
  }else{
    if (a %in% c(4,5,6)){
      rows<-(4:6)
    }else{
      rows<-(7:9)
    }
  }
  if (b %in% c(1,2,3)){
    cols<-(1:3)
  }else{
    if (b %in% c(4,5,6)){
      cols<-(4:6)
    }else{
      cols<-(7:9)
    }
  }
  for (c in rows){
    for (d in cols){
      if ((z %in% FindFactors(puz[c,d]))){
        puz[c,d]<-(puz[c,d]/z)
      }
    }
  }
  return(puz)
}

Sudoku_Solver(Sample1)

Sample1<-c(0,0,0,0,0,0,1,0,0,0,0,0,0,2,3,9,6,8,0,0,0,0,6,0,0,3,2,0,7,0,6,3,0,8,0,0,8,0,0,0,5,0,0,0,3,0,0,3,0,9,2,0,4,0,2,3,0,0,4,0,0,0,0,9,8,4,3,1,0,0,0,0,0,0,6,0,0,0,0,0,0)
Sample2<-c(0,0,1,0,0,4,0,0,3,6,0,0,2,0,0,4,0,0,4,7,0,8,3,0,5,1,0,0,0,7,0,0,8,0,0,4,5,0,0,9,0,0,8,0,0,9,8,0,7,5,0,2,6,0,0,0,3,0,0,9,0,0,8,7,0,0,1,0,0,3,0,0,2,9,0,3,7,0,1,4,0)
Sample3<-c(7,0,0,4,0,2,0,0,5,0,1,9,0,6,0,4,8,0,0,2,0,0,1,0,0,6,0,6,0,0,1,0,5,0,0,9,0,9,1,0,0,0,5,3,0,8,0,0,3,0,6,0,0,4,0,7,0,0,5,0,0,4,0,0,4,3,0,7,0,6,9,0,1,0,0,6,0,9,0,0,7)
Sample4<-c(0,8,3,0,0,0,0,0,0,0,5,0,6,0,0,0,1,9,0,0,9,2,0,0,7,0,3,0,0,0,0,0,0,5,2,0,0,0,0,0,9,0,0,0,0,0,3,7,0,0,0,0,0,0,1,0,2,0,0,4,8,0,0,8,4,0,0,0,7,0,9,0,0,0,0,0,0,0,1,6,0)
Sample5<-c(0,0,7,0,0,0,0,0,0,0,0,6,0,0,3,7,9,0,8,3,0,0,0,9,0,5,0,0,0,0,0,0,7,1,3,0,0,0,0,0,0,0,0,0,0,0,9,2,4,0,0,0,0,0,0,1,0,9,0,0,0,2,4,0,2,4,6,0,0,5,0,0,0,0,0,0,0,0,8,0,0)
Sample6<-c(0,0,3,0,0,0,9,0,0,0,0,0,8,3,4,0,0,0,5,0,0,0,0,0,0,0,7,0,7,0,5,0,8,0,3,0,0,4,0,0,1,0,0,9,0,0,3,0,9,0,6,0,2,0,7,0,0,0,0,0,0,0,6,0,0,0,6,2,1,0,0,0,0,0,1,0,0,0,5,0,0)
Sample7<-c(1,2,0,0,8,0,0,0,9,5,0,0,9,0,0,0,1,0,0,0,8,0,0,0,2,0,0,0,8,0,0,0,6,0,0,0,7,0,0,0,1,0,0,8,0,0,0,0,4,0,0,0,0,3,0,0,4,0,0,0,0,0,6,0,1,0,0,5,0,0,9,0,2,0,0,0,0,4,3,0,0)
Sample8<-c(2,9,0,0,0,0,0,0,0,0,0,6,0,0,8,5,0,0,0,0,0,4,0,0,0,7,0,0,0,0,8,0,0,0,0,2,8,0,0,0,0,0,0,0,9,3,0,0,0,0,9,0,0,0,0,7,0,0,0,6,0,0,0,0,0,5,2,0,0,4,0,0,0,0,0,0,0,0,0,2,3)
Sample9<-c(7,0,4,2,0,0,0,0,0,0,0,0,0,0,3,1,0,0,0,6,0,0,0,0,0,0,8,0,8,0,5,0,0,0,0,9,0,0,0,0,1,0,0,0,0,3,0,0,0,0,8,0,7,0,2,0,0,0,0,0,0,4,0,0,0,9,7,0,0,0,0,0,0,0,0,0,0,5,3,0,1)
Sample10<-c(8,6,0,0,2,0,0,0,0,0,0,0,7,0,0,0,5,9,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,8,0,0,0,4,0,0,0,0,0,0,0,0,0,5,3,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,6,0,0,0,0,7,5,0,9,0,0,0)
Sample11<-c(9,0,0,0,0,0,0,0,4,0,0,1,2,0,0,0,5,0,0,3,4,5,0,0,6,0,0,0,6,7,8,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,0,0,0,9,8,7,0,0,0,9,0,0,6,5,4,0,0,8,0,0,0,3,2,0,0,7,0,0,0,0,0,0,0,8)
Sample12<-c(0,0,5,1,3,0,0,2,0,0,0,0,0,0,0,7,0,0,4,7,0,6,0,0,0,0,1,0,0,0,2,0,0,0,1,0,0,0,6,7,0,9,2,0,0,0,8,0,0,0,3,0,0,0,2,0,0,0,0,6,0,5,7,0,0,8,0,0,0,0,0,0,0,4,0,0,5,2,9,0,0)
Sample13<-c(0,1,2,9,0,0,0,7,0,0,0,0,4,0,0,0,9,0,0,0,0,0,1,0,0,8,0,0,0,0,0,3,7,0,0,1,0,0,3,0,0,0,5,0,0,8,0,0,5,2,0,0,0,0,0,2,0,0,9,0,0,0,0,0,6,0,0,0,3,0,0,0,0,3,0,0,0,6,7,4,0)
Sample14<-c(1,4,0,8,0,0,0,0,0,7,0,2,0,0,0,0,0,0,0,0,5,0,3,9,0,0,0,0,0,0,0,2,0,7,0,1,2,0,0,1,0,7,0,0,8,3,0,7,0,9,0,0,0,0,0,0,0,3,6,0,8,0,0,0,0,0,0,0,0,3,0,4,0,0,0,0,0,2,0,9,5)
Sample15<-c(0,2,6,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,7,0,0,0,4,8,0,6,0,0,7,0,0,0,1,0,0,0,0,4,0,2,0,5,0,0,9,2,0,0,0,8,0,0,5,7,5,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,1,0,5,4,0)
Sample16<-c(0,1,0,7,0,6,0,0,0,7,0,3,0,0,0,0,0,0,0,2,0,5,4,0,0,0,0,3,4,0,0,0,0,2,5,0,5,0,0,0,0,0,0,0,1,0,8,1,0,0,0,0,7,4,0,0,0,0,5,7,0,8,0,0,0,0,0,0,0,3,0,9,0,0,0,8,0,1,0,4,0)
Sample17<-c(8,0,0,0,9,3,0,1,0,0,4,0,0,6,0,0,0,0,0,0,6,0,0,8,7,0,9,0,0,4,0,7,0,8,0,0,0,6,0,0,0,2,0,0,0,1,2,0,6,0,0,4,0,0,0,8,9,0,0,0,5,0,0,0,0,0,0,0,0,0,3,0,0,3,0,1,0,0,0,0,7)
Sample18<-c(0,0,5,4,0,0,0,0,0,1,0,0,0,8,0,0,7,0,0,0,0,9,0,0,1,0,0,6,8,0,0,5,0,0,9,3,0,9,0,0,0,0,0,1,0,7,3,0,0,9,0,0,6,5,0,0,3,0,0,7,0,0,0,0,1,0,0,3,0,0,0,6,0,0,0,0,0,6,2,0,0)
Sample19<-c(6,5,9,0,0,4,3,0,0,0,0,4,0,7,0,5,0,0,0,2,0,0,0,0,0,9,0,0,0,0,0,1,3,6,0,0,0,0,0,8,0,2,0,3,0,0,0,0,5,0,0,8,0,4,0,0,0,6,0,1,0,5,0,0,9,0,0,0,0,0,0,0,5,8,0,3,0,0,0,4,7)
Sample20<-c(0,0,0,0,0,0,0,7,0,9,5,0,0,0,4,0,0,1,8,4,0,0,9,0,0,0,0,2,0,0,0,0,0,0,8,0,0,0,0,3,0,0,9,0,0,0,0,5,7,4,0,0,0,0,0,7,0,6,0,0,0,0,0,5,0,8,0,0,0,7,3,0,0,1,0,0,0,3,6,9,0)