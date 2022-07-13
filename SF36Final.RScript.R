getwd()
library(dplyr)
SF363=read.csv("SF36F.csv", header=TRUE)
View(SF363)
SF36=select(SF363,-38:-69)
SF363main= select(SF363,-38:-69)
View(SF36)
View(SF363main)
c1 = c(1,2,3)
c2 = c(1,2,3,4,5)
c3 = c(1,2,3,4,5,6)
c1Col = c("Q3a","Q3b","Q3c","Q3d","Q3e","Q3f","Q3g","Q3h","Q3i","Q3j")
c2Col = c("Q1", "Q2", "Q4a", "Q4b", "Q4c", "Q4d",
          "Q5a","Q5b", "Q5c", "Q6","Q8",
          "Q9a","Q9b","Q9c","Q9d","Q9e","Q9f","Q9g","Q9h","Q9i","Q10",
          "Q11a","Q11b","Q11c","Q11d")
for(col in c1Col){
  for(index in 1:nrow(SF36)) {
    if(!(SF36[[col]][index] %in% c1)) {
      SF36[[col]][index] = NA
    }
  }
}

for(col in c2Col){
  for(index in 1:nrow(SF36)) {
    if(!(SF36[[col]][index] %in% c2)) {
      SF36[[col]][index] = NA
    }
  }
}

for(i in 1:nrow(SF36)){
  if(!(SF36$Q7[i] %in% c3)) {
    SF36$Q7[i] = NA
  }
} 

#Q1 recalibrate/reverse
for(i in 1:nrow(SF36)) {
  if(is.na(SF36$Q1[i])) {
    # keep it as NA
  } else if(SF36$Q1[i] == 1) {
    SF36$Q1[i] = 5
  } else if(SF36$Q1[i] == 2) {
    SF36$Q1[i] = 4.4
  } else if(SF36$Q1[i] == 3) {
    SF36$Q1[i] = 3.4
  } else if(SF36$Q1[i] == 4) {
    SF36$Q1[i] = 2
  } else if(SF36$Q1[i] == 5) {
    SF36$Q1[i] = 1
  }
}

#Q11b,Q11d,Q9a,Q9e ,Q9d,Q9h,Q6 reverse
for(i in 1:nrow(SF36)) {
  SF36$Q11b[i] = 6 - SF36$Q11b[i]
}
for(i in 1:nrow(SF36)) {
  SF36$Q11d[i] = 6 - SF36$Q11d[i]
}
for(i in 1:nrow(SF36)) {
  SF36$Q9a[i] = 6 - SF36$Q9a[i]
}
for(i in 1:nrow(SF36)) {
  SF36$Q9d[i] = 6 - SF36$Q9d[i]
}
for(i in 1:nrow(SF36)) {
  SF36$Q9h[i] = 6 - SF36$Q9h[i]
}
for(i in 1:nrow(SF36)) {
  SF36$Q9e[i] = 6 - SF36$Q9e[i]
}
for(i in 1:nrow(SF36)) {
  SF36$Q6[i] = 6 - SF36$Q6[i]
}

# Q8 recalibrate/based on Q7
for(i in 1:nrow(SF36)) {
  if(!is.na(SF36$Q8[i]) & !is.na(SF36$Q7[i])) {
    if(SF36$Q8[i] == 1 & SF36$Q7[i] == 1){
      SF36$Q8[i] = 6
    } else if(SF36$Q8[i] == 1 & between(SF36$Q7[i], 2, 6)) {
      SF36$Q8[i] = 5
    } else if(between(SF36$Q7[i], 1, 6)){
      SF36$Q8[i] =  6 - SF36$Q8[i]
    }
  } else if(is.na(SF36$Q7[i]) & !is.na(SF36$Q8[i])) {
    if(SF36$Q8[i] == 1) {
      SF36$Q8[i] = 6
    } else if(SF36$Q8[i] == 2) {
      SF36$Q8[i] = 4.75
    } else if(SF36$Q8[i] == 3) {
      SF36$Q8[i] = 3.5
    } else if(SF36$Q8[i] == 4) {
      SF36$Q8[i] = 2.25
    } else if(SF36$Q8[i] == 5) {
      SF36$Q8[i] = 1
    }
  }
}

# Q7 recalibrate
for(i in 1:nrow(SF36)) {
  if(is.na(SF36$Q7[i])) {
    #keep na 
  } else if(SF36$Q7[i] == 1) {
    SF36$Q7[i] = 6
  } else if(SF36$Q7[i] == 2) {
    SF36$Q7[i] = 5.4
  } else if(SF36$Q7[i] == 3) {
    SF36$Q7[i] = 4.2
  } else if(SF36$Q7[i] == 4) {
    SF36$Q7[i] = 3.1
  } else if(SF36$Q7[i] == 5) {
    SF36$Q7[i] = 2.2
  } else if(SF36$Q7[i] == 6) {
    SF36$Q7[i] = 1
  }
}

# Missing data computation for 3a:3j
for(row in 1:nrow(SF36[,c1Col])) {
  if(rowSums(is.na(SF36[row, c1Col])) > (length(c1Col) / 2)) {
    # keep the NA's as is
  } else {
    for(col in c1Col) {
      if(is.na(SF36[[col]][row])) {
        SF36[[col]][row] = rowMeans(SF36[row, c1Col], na.rm = TRUE)
      }
    }
  }
}

# Missing data computation for 4a:4d
Q4Col = c("Q4a","Q4b","Q4c","Q4d")

for(row in 1:nrow(SF36[,Q4Col])) {
  if(rowSums(is.na(SF36[row, Q4Col])) > (length(Q4Col) / 2)) {
    # keep the NA's as is
  } else {
    for(col in Q4Col) {
      if(is.na(SF36[[col]][row])) {
        SF36[[col]][row] = rowMeans(SF36[row, Q4Col], na.rm = TRUE)
      }
    }
  }
}

# Missing data computation for 1,11a,11b,11c,11d
Q11Col = c("Q1","Q11a","Q11b","Q11c","Q11d")

for(row in 1:nrow(SF36[,Q11Col])) {
  if(rowSums(is.na(SF36[row, Q11Col])) > (length(Q11Col) / 2)) {
    # keep the NA's as is
  } else {
    for(col in Q11Col) {
      if(is.na(SF36[[col]][row])) {
        SF36[[col]][row] = rowMeans(SF36[row, Q11Col], na.rm = TRUE)
      }
    }
  }
}

# Missing data computation for 9a,9b,9g,9i
Q9Col = c("Q9a","Q9e","Q9g","Q9i")

for(row in 1:nrow(SF36[Q9Col])) {
  if(rowSums(is.na(SF36[row, Q9Col])) > (length(Q9Col) / 2)) {
    # keep the NA's as is
  } else {
    for(col in Q9Col) {
      if(is.na(SF36[[col]][row])) {
        SF36[[col]][row] = rowMeans(SF36[row, Q9Col], na.rm = TRUE)
      }
    }
  }
}

# Missing data computation for 9b,9c,9d,9f,9h
Q9Col1 = c("Q9b","Q9c","Q9d","Q9f","Q9h")

for(row in 1:nrow(SF36[,Q9Col1])) {
  if(rowSums(is.na(SF36[row, Q9Col1])) > (length(Q9Col1) / 2)) {
    # keep the NA's as is
  } else {
    for(col in Q9Col1) {
      if(is.na(SF36[[col]][row])) {
        SF36[[col]][row] = rowMeans(SF36[row, Q9Col1], na.rm = TRUE)
      }
    }
  }
}

# Missing data computation for 5a,5b,5c
Q5Col = c("Q5a","Q5b","Q5c")

for(row in 1:nrow(SF36[,Q5Col])) {
  if(rowSums(is.na(SF36[row, Q5Col])) > (length(Q5Col) / 2)) {
    # keep the NA's as is
  } else {
    for(col in Q5Col) {
      if(is.na(SF36[[col]][row])) {
        SF36[[col]][row] = rowMeans(SF36[row, Q5Col], na.rm = TRUE)
      }
    }
  }
}

# Missing data computation for 7,8
for(i in 1:nrow(SF36)) {
  if(is.na(SF36$Q7[i])){
    SF36$Q7[i] = SF36$Q8[i]
  } else if(is.na(SF36$Q8[i])) {
    SF36$Q8[i] = SF36$Q7[i]
  }
}

# Missing data computation for 6,10
for(i in 1:nrow(SF36)) {
  if(is.na(SF36$Q6[i])){
    SF36$Q6[i] = SF36$Q10[i]
  } else if(is.na(SF36$Q10[i])) {
    SF36$Q10[i] = SF36$Q6[i]
  }
}

# Raw/Transformed/Standardized/Norm/AGG_PHYS/AGG_MENT/PCS/MCS Scores
SF36=SF36 %>%
  mutate(RAW_PF=Q3a+Q3b+Q3c+Q3d+Q3e+Q3f+Q3g+Q3h+Q3i+Q3j) %>%
  mutate(RAW_RP=Q4a+Q4b+Q4c+Q4d) %>%
  mutate(RAW_BP=Q7+Q8) %>%
  mutate(RAW_GH=Q1+Q11a+Q11b+Q11c+Q11d) %>%
  mutate(RAW_VT=Q9a+Q9e+Q9g+Q9i) %>%
  mutate(RAW_SF=Q6+Q10) %>%
  mutate(RAW_RE=Q5a+Q5b+Q5c) %>%
  mutate(RAW_MH=Q9b+Q9c+Q9d+Q9f+Q9h) %>%
  
  mutate(T_PF=(RAW_PF-10)*100/20) %>%
  mutate(T_RP=(RAW_RP-4)*100/16) %>%
  mutate(T_BP=(RAW_BP-2)*100/10) %>%
  mutate(T_GH=(RAW_GH-5)*100/20) %>%
  mutate(T_VT=(RAW_VT-4)*100/16) %>%
  mutate(T_SF=(RAW_SF-2)*100/8) %>%
  mutate(T_RE=(RAW_RE-3)*100/12) %>%
  mutate(T_MH=(RAW_MH-5)*100/20) %>%
  
  mutate(Std_PF=(T_PF-83.29094)/23.75883) %>%
  mutate(Std_RP=(T_RP-82.50964)/25.52028) %>%
  mutate(Std_BP=(T_BP-71.32527)/23.66224) %>%
  mutate(Std_GH=(T_GH-70.84570)/20.97821) %>%
  mutate(Std_VT=(T_VT-58.31411)/20.01923) %>%
  mutate(Std_SF=(T_SF-84.30250)/22.91921) %>%
  mutate(Std_RE=(T_RE-87.39733)/21.43778) %>%
  mutate(Std_MH=(T_MH-74.98685)/17.75604) %>%
  
  mutate(NB_PF= 50 +(Std_PF*10)) %>%
  mutate(NB_RP= 50 +(Std_RP*10)) %>%
  mutate(NB_BP= 50 +(Std_BP*10)) %>%
  mutate(NB_GH= 50 +(Std_GH*10)) %>%
  mutate(NB_VT= 50 +(Std_VT*10)) %>%
  mutate(NB_SF= 50 +(Std_SF*10)) %>%
  mutate(NB_RE= 50 +(Std_RE*10)) %>%
  mutate(NB_MH= 50 +(Std_MH*10)) %>%
  
  mutate(AGG_PHYS=(Std_PF*0.42402) 
         + (Std_RP*0.35119) 
         + (Std_BP*0.31754) 
         + (Std_GH*0.24954) 
         + (Std_VT*0.02877) 
         + (Std_SF*-0.00753) 
         + (Std_RE*-0.19206) 
         + (Std_MH*-0.22069)) %>%
  
  mutate(AGG_MENT=(Std_PF*-0.22999) 
         + (Std_RP*-0.12329) 
         + (Std_BP*-0.09731) 
         + (Std_GH*-0.01571) 
         + (Std_VT*0.23534) 
         + (Std_SF*0.26876) 
         + (Std_RE*0.43407) 
         + (Std_MH*0.48581)) %>%
  
  mutate(PCS=50 + (AGG_PHYS*10)) %>%
  mutate(MCS=50 + (AGG_MENT*10))

# Round
SF36=SF36 %>% 
  mutate(across(where(is.numeric), round, 2))

#Join
View(SF363main)
SF36F_combined=SF363main %>%
  left_join(SF36, by="Identifier", suffix=c('','_SF36'))
SF36F_combined.score=select(SF36F_combined, -38:-73)
View(SF36F_combined.score)

#Output
SF36F_scores=data.frame(SF36F_combined.score)
write.csv(SF36F_scores,"~/Desktop/Consltnt/SF36F_scores.csv",row.names=FALSE , na="")

